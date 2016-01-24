#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Collect C++ class definitions in a .hpp file and generate codes.
# + Decorator class which contains MOCK_*METHOD* methods
# + Forwarder variable (class instance) to delegate methods its
#   original variable or a mock instance
#
# Step
# 1. Read a given header file and parse its each line
#  Parse the file that clang preprocessed and wrote as a pretty AST.
#  The AST contains
#  - Blocks which begin with a "...{" line and ends with a "... }" line
#  - Non-Blocks which contains only one line ended with ";"
#  and
#  - Nesting depth of a block and its indent spaces do not match.
#  - Implicit namespaces for some symbols are solved.
#
#  Filter out data structures that this script do not handle.
#  - User-defined type except simple (not template) class and struct
#  - Standard C++, Boost C++, Google Test/Mock headers.
#  - Compiler internal symbol that contains double underline (__)
#
#  Abandon data structures unused later steps in this step because
#  standard C++ and Boost C++ headers are large.
#
# 2. Construct class hierarchy
#  - Search public base classes
#  - Determine namespaces for symbols in namespace blocks
#
#  Note that struct is a syntactic sugar that means class with
#  default public access and inheritance.
#
# 3. Collect class member functions
#  - Public and non-pure virtual
#  - Distinguish const and non-const. & and && are not supported.
#  - Treat same no arguments f() and void argument f(void)
#  - VA_arg, perfect forwarding and move semantics are not supported
#
# 4. Format and generate codes
#  - Decorator class : delegate member function to its original (base)
#    class or a registered mock instance with its original arguments.
#  - Forwarder class : delegate member function to its original
#    variable or a registered mock instance with its original arguments.
#  - Mock : attach and detach a decorator
#  - Declare and define forwarder class instances
#  - Macros to swap class and variable names
#
# 5. Write the generated codes to a specified output file

require 'tempfile'
require_relative './mockgenConst.rb'
require_relative './mockgenCommon.rb'

module Mockgen
  # Generic Block Structure
  class BaseBlock
    attr_reader :parent, :children

    # line : Head of block line (leading and trailing spaces must be removed)
    def initialize(line)
      @line = line    # Headline of a block
      @parent = nil   # nil for the root block
      @children = []  # Children order by addition
    end

    # Connect parent-child blocks
    def connect(child)
      child.setParent(self)
      addChild(child)
    end

    # Disconnect parent-child blocks
    def disconnect(child)
      # Ignore unrelated blocks
      child.setParent(nil) if child.parent == self
      removeChild(child)
    end

    ## Derived classes override methods below
    # Return if need to traverse this block
    def canTraverse
      false
    end

    def isClass?
      false
    end

    def isClassVariable?
      false
    end

    # Class and struct names are treated as namespaces
    def getNamespace
      ""
    end

    # Create a block instance and return it if can
    def parseChildren(line)
      nil
    end

    def getStringToClassFile
      nil
    end

    def getStringToSwapperFile
      nil
    end

    # Append :: prefix to the arg name in a namespace
    # Be consistent for clang output
    def addTopNamespace(name)
      pos = name.index("::")
      ((pos.nil? || pos == 0) ? "" : "::") + name
    end

    def isConstructor(line)
      line.match(/^\s*#{@name}\s*\(.*\)/) ? true : false
    end

    # Concatenate this block's namespaces with the arg name
    # template <> is excluded
    def getNonTypedFullname(name)
      nameChain = [name]
      parent = @parent

      while(parent)
        addedName = parent.getNamespace
        nameChain << addedName unless addedName.empty?
        parent = parent.parent
      end

      fullname = nameChain.reverse.join("::")
      addTopNamespace(fullname)
    end

    protected
    def setParent(block)
      @parent = block
    end

    private
    def addChild(block)
      @children << block unless @children.any? { |child| child.equal?(block) }
    end

    def removeChild(block)
      @children = @children.reject { |child| child.equal?(block) }
    end
  end

  # Root block as the top-level namespace
  class RootBlock < BaseBlock
    def initialize(line)
      super
    end

    def canTraverse
      true
    end
  end

  # C++ namespace
  class NamespaceBlock < BaseBlock
    def initialize(line)
      super

      @name = ""
      if md = line.tr("{","").match(/^namespace\s+(\S+)/)
        @name = md[1]
      end
    end

    def canTraverse
      return false if @name.include?(Mockgen::Constants::NAMESPACE_COMPILER_INTERNAL)
      !(Mockgen::Constants::NAMESPACE_SKIPPED_SET.any? { |name| @name =~ /^#{name}/ })
    end

    def getNamespace
      @name
    end
  end

  # Extern Variable (not extern typename)
  class ExternVariableStatement < BaseBlock
    # the className is CV removed and dereferenced
    attr_reader :varName, :className

    def initialize(line)
      super
      @varName = ""
      @className = ""
      @canTraverse = false
      parse(line.gsub(/\s*[{;].*$/,""))
    end

    def canTraverse
      @canTraverse
    end

    def isClassVariable?
      @canTraverse
    end

    ## Implementation detail (public for testing)
    def parse(line)
      return if line.empty? || line[-1] == ")"
      wordSet = line.split(/[\s\*&;]+/).reject do |word|
        ["extern", "class", "struct", "static", "const"].any? { |key| key == word }
      end

      return if wordSet.size < 2
      @varName = wordSet[-1]
      @className = wordSet[-2]
      @canTraverse = true
    end

    def getFullname
      getNonTypedFullname(@varName)
    end
  end

  # Extract argument variables from a typed argument list
  class ArgVariableSet
    attr_reader :str

    def initialize(typedArgPhase)
      md = typedArgPhase.match(/^\s*(.*)\s*$/)
      phrase = md[1]

      @str = ""
      unless (phrase.empty? || phrase == "void")
        @str = phrase.split(/\s*,/).map do |arg|
          arg.split(/[\s\*&]+/)[-1]
        end.join(", ")
      end
    end
  end

  # Constructor with arbitrary number of arguments
  # Templates are not supported yet
  class ConstructorBlock < BaseBlock
    def initialize(line, className)
      super(line)
      @valid, @typedArgSet, @callBase = parse(line, className)
    end

    def parse(line, className)
      return [false, "", ""] unless md = line.match(/^\s*#{className}\s*\(\s*(.*)\s*\)/)
      typedArgSet = md[1]
      argSet = ArgVariableSet.new(typedArgSet).str

      # Treat void argument as empty
      typedArgSet = (argSet.empty?) ? "" : typedArgSet
      # Add a comma to cascade other initializer arguments
      callBase = (argSet.empty?) ? "" : "#{className}(#{argSet}), "
      [true, typedArgSet, callBase]
    end

    def canTraverse
      @valid
    end

    # Empty if call a constructor without arguments
    # Add a comma to cascade other arguments
    def getTypedArgsForBaseClass
      # Add a comma to cascade other initializer arguments
      @typedArgSet.empty? ? "" : ", #{@typedArgSet}"
    end

    def getCallForBaseClassInitializer
      @callBase
    end

    def makeDecoratorDef(decoratorName)
      "    #{decoratorName}(#{@typedArgSet}) : " +
      "#{@callBase}#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    end

    def makeDefaultConstructor(decoratorName)
      "    #{decoratorName}(void) : " +
      "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    end
  end

  # Class member function
  # Templates are not supported yet
  class MemberFunctionBlock < BaseBlock
    attr_reader :valid, :argSignature

    def initialize(line)
      super
      @valid = false
      @constMemfunc = false
      @staticMemfunc = false
      @returnType == ""
      @returnVoid = false
      @decl = ""
      @argSet = ""
      @funcName = ""
      @typedArgSet = ""
      @argSignature = ""
      @postFunc = ""

      # Remove trailing ; and {
      body = line
      if md = line.match(/^([^{;]*)[{;]/)
        body = md[1]
      end

      # Remove trailing spaces
      return unless md = body.match(/^(.*\S)\s*$/)
      parse(md[1])
    end

    def canTraverse
      @valid
    end

    ## Public methods added on the base class
    def override?(block)
      @argSignature == block.argSignature
    end

    def makeMockDef(className)
      constStr = (@constMemfunc) ? "_CONST" : ""

      numberOfArgs = @typedArgSet.split(/,/).size
      # Treat void-only arg empty
      numberOfArgs = 0 if @argSet.empty?

      str = "    MOCK#{constStr}_METHOD#{numberOfArgs}"
      str += "(#{@funcName},#{@returnType}(#{@typedArgSet}));\n"
      str
    end

    def makeDecoratorDef(className)
      mockVarname = @staticMemfunc ? Mockgen::Constants::VARNAME_CLASS_MOCK : Mockgen::Constants::VARNAME_INSTANCE_MOCK
      decl = @staticMemfunc ? "static #{@decl}" : @decl
      str = "    #{decl} { if (#{mockVarname}) { "

      if (@returnVoid)
        str += "#{mockVarname}->#{@funcName}(#{@argSet}); return; } "
        str += "#{className}::#{@funcName}(#{@argSet});"
      else
        str += "return #{mockVarname}->#{@funcName}(#{@argSet}); } "
        str += "return #{className}::#{@funcName}(#{@argSet});"
      end

      str += " }\n"
      str
    end

    def makeForwarderDef(className)
      decl = @decl.dup
      if (@decl =~ /\s+override$/) || (@decl =~ /\s+override\s/)
        decl = @decl.gsub(/\s+override(\s?)/, '\1')
      end
      str = "    #{decl} { if (#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}) { "

      if (@returnVoid)
        str += "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}->#{@funcName}(#{@argSet}); return; } "
        str += "static_cast<#{className}*>(pActual_)->#{@funcName}(#{@argSet});"
      else
        str += "return #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}->#{@funcName}(#{@argSet}); } "
        str += "return static_cast<#{className}*>(pActual_)->#{@funcName}(#{@argSet});"
      end

      str += " }\n"
      str
    end

    ## Implementation detail (public for testing)
    def parse(line)
      elements = splitLine(removeAttribute(line))
      return unless elements
      # Include * and & ahead function names in preFunc
      preFunc, funcName, typedArgSet, postFunc = elements

      # Skip pure virtual functions
      return if isPureVirtual(postFunc)

      @constMemfunc = isConstMemberFunction(postFunc)
      @staticMemfunc, @returnType, @returnVoid = extractReturnType(preFunc)
      @decl = @returnType + " #{funcName}(" + typedArgSet + ")"
      @decl = @decl+ " " + postFunc unless postFunc.empty?

      # Split by , and collect variable names ahead ,
      # Note : assuming variable names are not omitted
      @argSet = ArgVariableSet.new(typedArgSet).str
      @argSignature = extractArgSignature(funcName, typedArgSet, @constMemfunc)

      @funcName = funcName
      @typedArgSet = typedArgSet
      @postFunc = postFunc
      @valid = true
    end

    # Remove attribute
    def removeAttribute(phrase)
      keyword = Mockgen::Constants::KEYWORD_ATTRIBUTE
      # Recursive regular expression
      phrase.gsub(/#{keyword}\s*(?<p>\(\s*[^(]*(\g<p>|([^()]*)\s*)\s*\)|)\s*/,"")
    end

    def splitLine(line)
      preFunc = nil
      funcName = nil
      typedArgSet = nil
      postFunc = nil

      # Exclude constructors
      return nil unless md = line.match(/^(.*\S)\s+([\*&]*)(\S+)\((.*)\)/)
      preFunc  = md[1] + md[2]  # Return type words
      funcName = md[3]    # Function name
      typedArgSet = md[4] # Arguments

      # Split before and after "(...)" and leave after ")" such as const and override
      declWords = line.split(/[\(\)]\s*/)
      postFunc = (declWords.size > 2) ? declWords[2..-1].join(" ") : ""

      # Destructors and operators not supported
      return nil if funcName.include?("~")
      return nil if funcName.match(/^operator\W/)

      [preFunc, funcName, typedArgSet, postFunc]
    end

    def isPureVirtual(phrase)
      phrase.gsub(/\s+/,"").include?("=0")
    end

    def isConstMemberFunction(phrase)
      return false if phrase.empty?
      phrase.split(/\s+/).any? { |word| word == "const" }
    end

    # Remove non-type related keywords
    def extractReturnType(phrase)
      wordSet = splitByReferenceMarks(phrase)
      staticMemfunc = wordSet.include?("static")

      returnType = wordSet.reject do |word|
        Mockgen::Constants::MEMFUNC_WORD_SKIPPED_SET.any? { |key| key == word }
      end.join(" ")

      # Distinguish void and void*
      returnVoid = (returnType == "void")

      [staticMemfunc, returnType, returnVoid]
    end

    # List each type of arguments and omit variables.
    # Distinguish const and non-const member function.
    # Discard return type because it may be covariant and
    # be determined unique by function name and argument types
    def extractArgSignature(funcName, phrase, constMemfunc)
      args = ""
      unless phrase == "void"
        args = phrase.split(/\s*,/).map do |arg|
          # split * and &
          argWords = splitByReferenceMarks(arg)
          argWords.size > 1 ? argWords[0..-2].join(" ") : argWords[0]
        end.join(",")
      end

      constStr = constMemfunc ? "const" : ""
      "#{funcName}(#{args})#{constStr}"
    end

    def splitByReferenceMarks(phrase)
      phrase.gsub(/([\*&])/, ' \1 ').split(/\s+/).reject { |w| w =~ /^\s*$/ }
    end
  end

  # Class and struct
  # Template is not supported yet
  class ClassBlock < BaseBlock
    attr_reader :mockName, :decoratorName, :forwarderName

    def initialize(line)
      super
      @templateHeader = "" # Template <T...>
      @name = ""           # Class name
      @mockName = ""
      @decoratorName = ""
      @forwarderName = ""
      @typename = "class"  # class or struct
      @pub = false         # Now parsing publicmembers

      # Remove trailing ; and {
      body = line
      if md = line.match(/^([^{;]*)[{;]/)
        body = md[1]
      end

      # Determine whether this block can be handled after parsed
      @valid = parseClassName(body)

      # One or more constructors
      @constructorSet = []
      @memberFunctionSet = []

      # Base classes
      @baseClassNameSet = parseInheritance(body)
      @baseClassBlockSet = []
    end

    def canTraverse
      @valid
    end

    def isClass?
      @valid
    end

    # Class and struct names can be treated as namespaces
    def getNamespace
      @name
    end

    # Parse a class member described in the arg line and return its block
    def parseChildren(line)
      return nil if parseAccess(line)
      block = nil

      # Disregard private members
      if @pub
        newBlock = nil
        if isConstructor(line)
          newBlock = ConstructorBlock.new(line, @name)
          @constructorSet << newBlock if newBlock.canTraverse
        else
          newBlock = MemberFunctionBlock.new(line)
          @memberFunctionSet << newBlock if newBlock.canTraverse
        end
        block = newBlock if newBlock.canTraverse
      end

      block
    end

    def parseAccess(line)
      md = line.match(/^(p\S+)\s*:/)
      if (md)
        case(md[1])
        when "public"
          @pub = true
          return true
        when "protected"
          @pub = false
          return true
        when "private"
          @pub = false
          return true
        end
      end

      false
    end

    def getStringToClassFile
      @mockClassDef + @decoratorClassDef
    end

    def getStringToSourceFile
      @staticMockDef + @mockClassFunc
    end

    ## Public methods added on the base class
    # Get class name with its namespaces
    def getFullname
      fullname = getNonTypedFullname(@name)
      @templateHeader.empty? ? fullname : "#{@templateHeader} #{fullname}"
    end

    # Set base classes
    # fullNameToBlock : { fullname => block }
    def setBaseClass(fullNameToBlock)
      @baseClassNameSet.each do |name|
        block = fullNameToBlock[name]
        @baseClassBlockSet << block if block
      end
    end

    # Generate class definition texts
    def makeClassSet
      fullname = getTypedFullname
      @mockClassDef, @mockClassFunc = formatMockClass(@mockName, @decoratorName, @forwarderName, fullname)
      @decoratorClassDef = formatDecoratorClass(@decoratorName, @mockName, fullname)
      @decoratorClassDef += formatForwarderClass(@forwarderName, @mockName, fullname)
      @staticMockDef = "#{@mockName}* #{@decoratorName}::#{Mockgen::Constants::VARNAME_CLASS_MOCK};\n"
    end

    ## Implementation detail (public for testing)
    def parseClassName(line)
      ["class", "struct"].each do |typenameStr|
        return true if parseClassHeader(line, typenameStr)
      end

      false
    end

    def parseClassHeader(line, typenameStr)
      if md = line.match(/^(template.*\S)\s+#{typenameStr}\s+(\S+)/)
        @templateHeader = md[1]  # テンプレート
        @name = md[2]   # クラス名
      elsif md = line.match(/^#{typenameStr}\s+(\S+)/)
        @name = md[1]   # クラス名
      else
        return false
      end

      @mockName = @name + Mockgen::Constants::CLASS_POSTFIX_MOCK
      @decoratorName = @name + Mockgen::Constants::CLASS_POSTFIX_DECORATOR
      @forwarderName = @name + Mockgen::Constants::CLASS_POSTFIX_FORWARDER

      @typename = typenameStr
      @pub = (typenameStr == "struct")
      true
    end

    def parseInheritance(line)
      nameSet = []
      return nameSet unless line.include?(":") && (md = line.match(/:\s*(.*)$/))

      md[1].split(/,\s*/).each do |sentence|
        wordSet = sentence.split(/\s+/)
        next if wordSet.empty?
        className = parseInheritancePhrase(wordSet)
        nameSet << addTopNamespace(className) if className
      end

      nameSet
    end

    def parseInheritancePhrase(argWordSet)
      wordSet = argWordSet.dup
      found = false

      # Skip protected and private inheritance
      if @typename == "struct"
        if (wordSet[0] == "public")
          wordSet.shift
          found = true
        elsif (wordSet[0] != "protected") && (wordSet[0] != "private")
          found = true
        end
      elsif wordSet[0] == "public"
        wordSet.shift
        found = true
      end

      found ? wordSet.join(" ") : nil
    end

    # Get class name with namespaces
    def getTypedFullname
      fullname = getNonTypedFullname(@name)
      md = @templateHeader.match(/template\s+<([^>]+)>/)
      return fullname if md.nil?

      typeSet = md[1].split(/,/).map{ |phrase| phrase.split(/\s+/)[-1] }.join(", ")
      "#{fullname}<#{typeSet}>"
    end

    # Cannot handle templates
    def formatMockClass(className, decoratorName, forwarderName, baseName)
      str =  "class #{decoratorName};\n"
      str += "class #{forwarderName};\n"
      str += "#{@templateHeader}class #{className} : public #{baseName} {\n"
      str += "public:\n"

      typedArgsArray = @constructorSet.empty? ? [""] : @constructorSet.map(&:getTypedArgsForBaseClass)
      callBaseArray = @constructorSet.empty? ? [""] : @constructorSet.map(&:getCallForBaseClassInitializer)
      ctorSet = typedArgsArray.zip(callBaseArray)

      typedArgsArray.each do |argSet|
        str += "    #{className}(#{decoratorName}* pDecorator#{argSet});\n"
        str += "    #{className}(#{decoratorName}& decorator#{argSet});\n"
        str += "    #{className}(#{forwarderName}* pForwarder#{argSet});\n"
        str += "    #{className}(#{forwarderName}& forwarder#{argSet});\n"
      end

      str += "    ~#{className}(void);\n"
      str += collectMockDef([])
      str += "    #{decoratorName}* pDecorator_;\n"
      str += "    #{forwarderName}* pForwarder_;\n"
      str += "};\n\n"

      src = ""
      ctorSet.each do |argSet, callBase|
        src += "#{className}::#{className}(#{decoratorName}* pDecorator#{argSet}) : "
        src += "#{callBase}pDecorator_(pDecorator), pForwarder_(0) "
        src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"
        src += "#{className}::#{className}(#{decoratorName}& decorator#{argSet}) : "
        src += "#{callBase}pDecorator_(&decorator), pForwarder_(0) "
        src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"

        src += "#{className}::#{className}(#{forwarderName}* pForwarder#{argSet}) : "
        src += "#{callBase}pDecorator_(0), pForwarder_(pForwarder) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"
        src += "#{className}::#{className}(#{forwarderName}& forwarder#{argSet}) : "
        src += "#{callBase}pDecorator_(0), pForwarder_(&forwarder) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"
      end

      src += "#{className}::~#{className}(void) {\n"
      src += "    if (pDecorator_ && (pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} == this)) "
      src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = 0; }\n"
      src += "    if (pDecorator_ && (pDecorator_->#{Mockgen::Constants::VARNAME_CLASS_MOCK} == this)) "
      src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_CLASS_MOCK} = 0; }\n"
      src += "    if (pForwarder_ && (pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} == this)) "
      src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = 0; }\n}\n\n"

      return str, src
    end

    def formatDecoratorClass(decoratorName, mockClassName, baseName)
      header = @templateHeader.empty? ? "" : (@templateHeader + " ")
      str =  "#{header}#{@typename} #{decoratorName} : public #{baseName} {\n"
      str += "public:\n"

      if @constructorSet.empty?
        str += ConstructorBlock.new("", baseName).makeDefaultConstructor(decoratorName)
      else
        @constructorSet.each do |constructor|
          str += constructor.makeDecoratorDef(decoratorName)
        end
      end

      str += "    virtual ~#{decoratorName}(void) {}\n"
      str += collectDecoratorDef([])
      str += "    #{mockClassName}* #{Mockgen::Constants::VARNAME_INSTANCE_MOCK};\n"
      str += "    static #{mockClassName}* #{Mockgen::Constants::VARNAME_CLASS_MOCK};\n"
      str += "};\n\n"
      str
    end

    def formatForwarderClass(forwarderName, mockClassName, baseName)
      header = @templateHeader.empty? ? "" : (@templateHeader + " ")
      str =  "#{header}#{@typename} #{forwarderName} {\n"
      str += "public:\n"
      str += "    #{forwarderName}(#{baseName}* pActual) : pActual_(pActual), "
      str += "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
      str += "    #{forwarderName}(#{baseName}& actual) : pActual_(&actual), "
      str += "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
      str += "    virtual ~#{forwarderName}(void) {}\n"
      str += collectForwarderDef([])
      str += "    #{baseName}* pActual_;\n"
      str += "    #{mockClassName}* #{Mockgen::Constants::VARNAME_INSTANCE_MOCK};\n"
      str += "};\n\n"
      str
    end

    def collectMockDef(derivedSet)
      collectFunctionDef(derivedSet, :collectMockDef, :makeMockDef)
    end

    def collectDecoratorDef(derivedSet)
      collectFunctionDef(derivedSet, :collectDecoratorDef, :makeDecoratorDef)
    end

    def collectForwarderDef(derivedSet)
      collectFunctionDef(derivedSet, :collectForwarderDef, :makeForwarderDef)
    end

    # If find an overriding function (which has same name, args and const
    # modifier as of its base class), call the most subclass definition.
    def collectFunctionDef(derivedSet, methodClass, methodFunc)
      str = ""
      functionSet = []

      @memberFunctionSet.each do |member|
        next unless member.canTraverse
        next if derivedSet.any? { |f| f.override?(member) }
        name = getFullname
        str += member.send(methodFunc, name)
        functionSet << member
      end

      # Search base classes for functions
      @baseClassBlockSet.each do |block|
        str += block.send(methodClass, [derivedSet, functionSet].flatten)
      end

      str
    end
  end

  # Factory class for variant type blocks
  class BlockFactory
    def initialize
    end

    # Top-level namespace
    def createRootBlock
      RootBlock.new("")
    end

    def createBlock(line, parentBlock)
      block = BaseBlock.new(line)
      newBlock = nil

      # Switch by the first keyword of the line
      if md = line.match(/^(.*\S)\s*{/)
        words = line.split(/\s+/)
        case words[0]
        when "namespace"
          newBlock = NamespaceBlock.new(line)
        when "class"
          newBlock = ClassBlock.new(line)
        when "template"
          if words.any? { |word| word == "class" }
            newBlock = ClassBlock.new(line)
          end
        end
      end

      unless newBlock
        if md = line.match(/^(.*\S)\s*;/)
          words = line.split(/\s+/)
          case words[0]
          when "extern"
            newBlock = ExternVariableStatement.new(line)
          end
        end
      end

      # Delegate to the current class block
      newBlock = parentBlock.parseChildren(line) unless newBlock

      block = newBlock if newBlock
      block
    end
  end

  # Parse input file lines and format output strings
  class CppFileParser
    # cppNameSpace : namespace of generated codes
    # originalFilename : a file before processed by clang
    # convertedFilename : a file after processed by clang
    def initialize(cppNameSpace, originalFilename, convertedFilename)
      abort if cppNameSpace.nil? || cppNameSpace.empty?
      @cppNameSpace = cppNameSpace
      @inputFilename = originalFilename
      @blockFactory = BlockFactory.new

      # Current parsing block
      @block = @blockFactory.createRootBlock
      @typeAliases = ""
      @varAliases = ""
      @varDecls = ""
      @varDefs = ""

      # Allow nil for testing
      return unless originalFilename
      return unless convertedFilename

      File.open(convertedFilename, "r") { |file|
        readAllLines(file)
      }

      buildClassTree
      makeClassSet
    end

    # Write generated codes to arg files
    def writeToFiles(classFilename, typeSwapperFilename, varSwapperFilename, declFilename, defFilename)
      beginNamespace = "namespace #{@cppNameSpace} {\n\n"
      endNamespace = "} // namespace #{@cppNameSpace}\n\n"
      usingNamespace = "using namespace #{@cppNameSpace};\n"

      writeClassFilename(classFilename, beginNamespace, endNamespace, usingNamespace)
      writeTypeSwapperFile(typeSwapperFilename, classFilename, beginNamespace, endNamespace, usingNamespace)
      writeVarSwapperFile(varSwapperFilename, declFilename, beginNamespace, endNamespace, usingNamespace)
      writeDeclFile(declFilename, classFilename, beginNamespace, endNamespace, usingNamespace)
      writeDefFilename(defFilename, classFilename, declFilename, beginNamespace, endNamespace, usingNamespace)
      0
    end

    ## Implementation detail (public for testing)
    def readAllLines(file)
      while rawLine = file.gets
        line = Mockgen::Common::LineWithoutCRLF.new(rawLine).line
        # Remove leading and trailing spaces and discard empty lines
        md = line.match(/^\s*(.*)\s*$/)
        next if md.nil?
        sentence = md[1]
        next if sentence.empty?
        parseLine(sentence)
      end
    end

    # line : leading and trailing spaces and CRLF must be removed
    # Parse and discard inline function definitions
    def parseLine(line)
      block = @blockFactory.createBlock(line, @block)

      if (line[0] == "}") && (line[-1] == "{")
      # } else {
      elsif (line[0] == "}")
        # End of a block
        childBlock = @block
        parentBlock = @block.parent
        @block = parentBlock
        eliminateUnusedBlock(parentBlock, childBlock)
      else
        # Connect "... {" and "... ;" lines
        @block.connect(block)
        # Beginning of a block
        if (line[-1] == "{")
          @block = block
        end
      end
    end

    def eliminateUnusedBlock(parentBlock, childBlock)
      parentBlock.disconnect(childBlock) unless childBlock.canTraverse
    end

    def buildClassTree
      # classSet : class name => block
      classSet = collectClasses(@block.children)
      connectClasses(@block.children, classSet)

      varSet = collectVariables(@block.children)
      @typeAliases, @varAliases, @varDecls, @varDefs = makeTypeVarAliases(varSet, classSet)
    end

    def collectClasses(rootBlockSet)
      classSet = {}  # class name => block
      lambdaToBlock = lambda do |block|
        fullname = block.getFullname
        classSet[fullname] = block unless fullname.empty?
      end

      doForAllBlocks(rootBlockSet, lambdaToBlock, :isClass?)
      classSet
    end

    def connectClasses(rootBlockSet, classSet)
      lambdaToBlock = lambda { |block| block.setBaseClass(classSet) }
      doForAllBlocks(rootBlockSet, lambdaToBlock, :isClass?)
    end

    def collectVariables(rootBlockSet)
      varSet = []  # variable name => block
      lambdaToBlock = lambda do |block|
        fullname = block.getFullname
        varSet << [block.varName, fullname, block.className] unless fullname.empty?
      end

      doForAllBlocks(rootBlockSet, lambdaToBlock, :isClassVariable?)
      varSet
    end

    def makeTypeVarAliases(varSet, classSet)
      typeSwapperStr = ""
      varSwapperStr = ""
      declStr = ""
      defStr = ""

      varSet.each do |varName, varFullname, className|
        typeSwapperS, varSwapperS, declS, defS = makeTypeVarAliasElements(classSet, varName, varFullname, className)
        typeSwapperStr += typeSwapperS
        varSwapperStr += varSwapperS
        declStr += declS
        defStr += defS
      end

      return typeSwapperStr, varSwapperStr, declStr, defStr
    end

    def makeTypeVarAliasElements(classSet, varName, varFullname, className)
      block = classSet[className]
      return "", "", "", "" unless block

      mockName = className + Mockgen::Constants::CLASS_POSTFIX_DECORATOR
      actualClassName = block.decoratorName
      actualVarName = varName + Mockgen::Constants::CLASS_POSTFIX_FORWARDER

      classBasename = block.getNamespace
      usingLine = (classBasename != className) ? "using #{className};\n" : ""
      typeSwapperStr = usingLine
      typeSwapperStr += '#' + "define #{classBasename} ::#{@cppNameSpace}::#{actualClassName}\n"

      usingLine = (varName != varFullname) ? "using #{varFullname};\n" : ""
      varSwapperStr = usingLine
      varSwapperStr += '#' + "define #{varName} ::#{@cppNameSpace}::#{actualVarName}\n"

      # Const member variables and templates are not supported yet
      actualClassName = block.forwarderName
      declStr = "extern #{actualClassName} #{actualVarName};\n"
      defStr  = "#{actualClassName} #{actualVarName}(#{varFullname});\n"

      return typeSwapperStr, varSwapperStr, declStr, defStr
    end

    def makeClassSet
      lambdaToBlock = lambda { |block| block.makeClassSet }
      doForAllBlocks(@block.children, lambdaToBlock, :isClass?)

      # Cascade another method
      self
    end

    # Breadth-first search blocks and apply arg lambdas to them
    def doForAllBlocks(rootBlockSet, lambdaToBlock, labelToCheck)
      blockSet = rootBlockSet

      while(!blockSet.empty?)
        nextBlockSet = []
        blockSet.each do |block|
          lambdaToBlock.call(block) if block.send(labelToCheck)
          nextBlockSet.concat(block.children)
        end

        blockSet = nextBlockSet
      end
    end

    def writeClassFilename(filename, beginNamespace, endNamespace, usingNamespace)
      File.open(filename, "w") do |file|
        file.puts getClassFileHeader(@inputFilename, filename)
        file.puts beginNamespace
        lambdaToBlock = lambda do |block|
          str = block.getStringToClassFile
          file.puts str if str
        end
        doForAllBlocks(@block.children, lambdaToBlock, :isClass?)
        file.puts endNamespace
        file.puts getIncludeGuardFooter
      end
    end

    def writeTypeSwapperFile(filename, classFilename, beginNamespace, endNamespace, usingNamespace)
      File.open(filename, "w") do |file|
        file.puts getSwapperHeader(classFilename, filename, "Class")
        file.puts @typeAliases
        file.puts ""
        file.puts getIncludeGuardFooter
      end
    end

    def writeVarSwapperFile(filename, declFilename, beginNamespace, endNamespace, usingNamespace)
      File.open(filename, "w") do |file|
        file.puts getSwapperHeader(declFilename, filename, "Variable")
        file.puts @varAliases
        file.puts ""
        file.puts getIncludeGuardFooter
      end
    end

    def writeDeclFile(filename, classFilename, beginNamespace, endNamespace, usingNamespace)
      File.open(filename, "w") do |file|
        file.puts getDeclHeader(classFilename, filename)
        file.puts beginNamespace
        file.puts @varDecls
        file.puts ""
        file.puts endNamespace
        file.puts getIncludeGuardFooter
      end
    end

    def writeDefFilename(filename, classFilename, declFilename, beginNamespace, endNamespace, usingNamespace)
      File.open(filename, "w") do |file|
        file.puts getDefHeader(@inputFilename, classFilename, declFilename)
        file.puts beginNamespace
        file.puts @varDefs
        file.puts ""
        file.puts endNamespace
        file.puts usingNamespace

        lambdaToBlock = lambda do |block|
          str = block.getStringToSourceFile
          file.puts str if str
        end
        doForAllBlocks(@block.children, lambdaToBlock, :isClass?)
      end
    end

    def getClassFileHeader(inputFilename, outClassFilename)
      str =  "// Mock and forwarder class definitions\n"
      str += "// This file is machine generated.\n\n"
      str += getIncludeGuardHeader(outClassFilename)
      str += "#include <gmock/gmock.h>\n"
      str += getIncludeDirective(inputFilename)
      str += "\n"
      str += "#define MOCK_OF(className) "+ "::#{@cppNameSpace}::className"
      str += '##' + Mockgen::Constants::CLASS_POSTFIX_MOCK + "\n"
      str += "#define DECORATOR(className) "+ "::#{@cppNameSpace}::className"
      str += '##' + Mockgen::Constants::CLASS_POSTFIX_DECORATOR + "\n"
      str += "#define FORWARDER(className) "+ "::#{@cppNameSpace}::className"
      str += '##' + Mockgen::Constants::CLASS_POSTFIX_FORWARDER + "\n"
      str += "#define INSTANCE_OF(varName) "+ "::#{@cppNameSpace}::varName"
      str += '##' + Mockgen::Constants::CLASS_POSTFIX_FORWARDER + "\n"
      str += "\n"
      str
    end

    def getSwapperHeader(outDeclFilename, outSwapperFilename, headWord)
      str =  "// #{headWord} name swapper definition\n"
      str += "// This file is machine generated.\n"
      str += "// Before including this file, define swapped classes and declare swapped variables\n"
      str += "\n"
      str += getIncludeGuardHeader(outSwapperFilename)
      str += getIncludeDirective(outDeclFilename)
      str += "\n"
      str
    end

    def getDeclHeader(outClassFilename, outDeclFilename)
      str =  "// Variable declarations\n"
      str += "// This file is machine generated.\n\n"
      str += getIncludeGuardHeader(outDeclFilename)
      str += getIncludeDirective(outClassFilename)
      str += "\n"
      str
    end

    def getDefHeader(inputFilename, outClassFilename, outDeclFilename)
      str =  "// Variable definitions\n"
      str += "// This file is machine generated.\n\n"
      str += getIncludeDirective(outDeclFilename)
      str += "\n"
      str
    end

    def getIncludeGuardHeader(filename)
      name = getIncludeGuardName(filename)
      str =  "#ifndef #{name}\n"
      str += "#define #{name}\n\n"
      str
    end

    def getIncludeGuardFooter
      "#endif\n"
    end

    def getIncludeGuardName(filename)
      # Split a camel case filename (treat digits as lower cases)
      name = filename.split(/\//)[-1]
      return "" if (name.nil? || name.empty?)
      name.gsub(/([^[[:upper:]]])([[:upper:]])/, '\1_\2').upcase.split(/\./).join("_")
    end

    def getIncludeDirective(filename)
      nameSet = filename.split(/\//)
      name = (nameSet.nil? || nameSet.empty? || nameSet[-1].empty?) ? nil : nameSet[-1]
      name.nil? ? "" : ('#include "' + name + '"' + "\n")
    end
  end

  # Parse command line arguments and launcher the parser
  class MockGenLauncher
    def initialize(argv)
      abort if argv.size < 7

      argSet = argv.dup
      @inputFilename = argSet.shift
      @convertedFilename = argSet.shift
      @outClassFilename = argSet.shift
      @outTypeSwapperFilename = argSet.shift
      @outVarSwapperFilename = argSet.shift
      @outDeclFilename = argSet.shift
      @outDefFilename = argSet.shift
      @clangArgs = quotePath(argSet)

      # Can set later
      @cppNameSpace = Mockgen::Constants::GENERATED_SYMBOL_NAMESPACE
    end

    def generate
      # Tempfile.create cannot create a tempfile in MinGW
      # and need to specify a filename for clang to output.
      # It is also useful to know how clang format .hpp files.
      system("#{Mockgen::Constants::CLANG_COMMAND} #{@clangArgs} #{@inputFilename} > #{@convertedFilename}")
      parse(@cppNameSpace, @inputFilename, @convertedFilename)

      # Value to return as process status code
      0
    end

    ## Implementation detail (public for testing)
    def quotePath(argv)
      quoteNext = false
      quoting = false

      argSet = []
      currentWord = ""
      argv.each do |word|
        if (word == "-cxx-isystem")
          argSet[-1] = argSet[-1] + '"' if !argSet.empty? && quoting
          currentWord = word
          quoteNext = true
          quoting = true
        elsif quoteNext
          currentWord = '"' + word
          quoteNext = false
        else
          currentWord = word
        end
        argSet << currentWord
      end

      argSet[-1] = argSet[-1] + '"' if !argSet.empty? && quoting
      argSet.join(" ")
    end

    def parse(cppNameSpace, inputFilename, convertedFilename)
      parser = CppFileParser.new(cppNameSpace, inputFilename, convertedFilename)
      args = [@outClassFilename, @outTypeSwapperFilename, @outVarSwapperFilename]
      args.concat([@outDeclFilename, @outDefFilename])
      parser.writeToFiles(*args)
    end
  end
end
