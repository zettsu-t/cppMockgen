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
  class TypeStringWithoutModifier
    attr_reader :strSet
    def initialize(typeStrSet)
      strSet = typeStrSet.map { |str| str.split(/([\s\*&]+)/) }.flatten
      @strSet = strSet.reject do |typeword|
        Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_SET.any? { |word| word == typeword.strip } ||
          typeword =~ /^\s*$/
      end
    end
  end

  # Block-scoped typedef set
  class TypeAliasSet
    attr_reader :aliasSet
    def initialize
      @aliasSet = {}
    end

    def add(aliasName, actualName)
      resolve(aliasName, actualName) unless @aliasSet.key?(aliasName)
    end

    def merge(outerSet)
      outerSet.aliasSet.each do |aliasName, actualName|
        @aliasSet[aliasName] = actualName unless @aliasSet.key?(aliasName)
      end

      @aliasSet.dup.each do |aliasName, actualName|
        resolve(aliasName, actualName)
      end
    end

    def resolve(aliasName, actualName)
      actualNameStr = actualName.dup
      actualNameWordSet = TypeStringWithoutModifier.new([actualName]).strSet

      while true
        previousStr = actualNameStr.dup
        newTypeSet = actualNameWordSet.map do |word|
          @aliasSet.key?(word) ? @aliasSet[word] : word
        end

        actualNameWordSet = newTypeSet
        actualNameStr = newTypeSet.join(" ")
        # Transform aliases until no more conversions needed
        break if actualNameStr == previousStr
      end

      @aliasSet[aliasName] = actualNameStr
    end

    # remove system internal definitions such as __uint32_t
    def removeSystemInternalSymbols
      @aliasSet.reject! { |key, value| isSystemInternalSymbol(key) || isSystemInternalSymbol(value) }
    end

    def isSystemInternalSymbol(str)
      str.include?("__") || (!str.empty? && str[0] == "_")
    end

    def resolveAlias(typeStr)
      typeStrSet = TypeStringWithoutModifier.new([typeStr]).strSet

      typeStrSet.map do |typeword|
        word = typeword.strip
        @aliasSet.key?(word) ? @aliasSet[word] : word
      end.join(" ")
    end
  end

  # Generic Block Structure
  class BaseBlock
    attr_reader :parent, :children, :typeAliasSet

    # line : Head of block line (leading and trailing spaces must be removed)
    def initialize(line)
      @line = line      # Headline of a block
      @parent = nil     # nil for the root block
      @children = []    # Children order by addition
      @typedefBlock = nil  # type alias for this block
      @typeAliasSet = TypeAliasSet.new  # type aliases in this block scope
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

    # Attach an type alias block for "typedef struct tagName {"
    def attachTypedefBlock(block)
      @typedefBlock = block if block
    end

    # Set an alias for "} alias;"
    def setTypedef(name)
      @typedefBlock.setAlias(name) if @typedefBlock
    end

    def collectAliases
      @children.each do |child|
        typeAliasSet = child.collectAliasesInBlock(@typeAliasSet)
      end

      @typeAliasSet = typeAliasSet
    end

    def resolveAlias(typeStr)
      result = typeStr.dup
      block = self

      while(result == typeStr && block)
        result = block.typeAliasSet.resolveAlias(typeStr)
        block = block.parent
      end

      result
    end

    ## Derived classes override methods below
    # Return if need to traverse this block
    def canTraverse
      false
    end

    # Do not mock structs in extern "C" {}
    def canMock
      result = true
      block = parent
      while block
        result &= block.canMock
        block = block.parent
      end

      result
    end

    def isClass?
      false
    end

    def isNonMemberInstanceOfClass?
      false
    end

    # Class and struct names are treated as namespaces
    def getNamespace
      ""
    end

    def collectAliasesInBlock(typeAliasSet)
      @typedefBlock.collectAliasesInBlock(typeAliasSet) if @typedefBlock
      typeAliasSet
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

    # To mock only undefined references
    def filterByReferences(arg)
      true
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

    # Treat type(*func)(args...) as a variable having a function pointer
    def isPointerToFunction(line)
      # Recursive regular expression
      pattern = Regexp.new('((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)')
      newline = ChompAfterDelimiter.new(ChompAfterDelimiter.new(line, ";").str, "{").str
      phraseSet = newline.gsub(/\(/, ' (').gsub(/\)/, ') ').gsub(/\s+/, ' ').scan(pattern)

      return false unless phraseSet
      elementSet = phraseSet.map { |p| p[1] }.compact
      return false if elementSet.size < 2
      !(phraseSet[-1])[1].nil? && elementSet[-2].include?("*")
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

    def canMock
      canTraverse() && super
    end

    def getNamespace
      @name
    end
  end

  # Extern "C" block
  class ExternCBlock < BaseBlock
    def initialize(line)
      super
    end

    def canTraverse
      true
    end

    def canMock
      false
    end

    def getNamespace
      ""
    end
  end

  # Simple typedef
  class TypedefBlock < BaseBlock
    def initialize(line)
      super

      @actualTypeSet = []
      @typeAlias = nil
      @trailing = false
      typeStrSet = []

      if line[-1] == "{"
        wordSet = line.tr("{","").split(" ")
        # typedef struct tagName {
        if (wordSet.size >= 2)
          typeStrSet = wordSet[1..-1]
          @trailing = true
        end
      elsif
        wordSet = line.tr(";","").split(" ")
        if (wordSet.size >= 3)
          # typedef struct Name Alias;
          typeStrSet = wordSet[1..-2]
          @typeAlias = wordSet[-1]
        end
      end

      @actualTypeSet = TypeStringWithoutModifier.new(typeStrSet).strSet
    end

    def canTraverse
      return !@typeAlias.nil?
    end

    # set an alias after its definition
    def setAlias(name)
      @typeAlias = name unless (@actualTypeSet.size >= 1) && (@actualTypeSet[-1] == name)
    end

    def collectAliasesInBlock(typeAliasSet)
      return typeAliasSet if @typeAlias.nil?
      actualTypeStr = @actualTypeSet.join(" ")
      typeAliasSet.add(@typeAlias, actualTypeStr)
      typeAliasSet
    end
  end

  # Split "result( *f )( arg )" into [["result", nil], ["(*f)", "*f"], ["(arg)", "arg"]]]
  class StringOfParenthesis
    def initialize(line)
      @line = line
    end

    def parse
      # Recursive regular expresstion to split () (()) () ...
      pattern = Regexp.new('((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)')
      phraseSet = @line.gsub(/\(/, ' (').gsub(/\)/, ') ').gsub(/\s+/, ' ').scan(pattern)

      # Remove spaces between parenthesis
      spacePattern = Regexp.new('\s*(\(|\))\s*')
      phraseSet.map do |phrase, captured|
        left = phrase ? phrase.gsub(spacePattern, '\1') : nil
        right = captured ? captured.gsub(spacePattern, '\1')[1..-2] : nil
        [left, right]
      end
    end
  end

  # Extract type of a variable
  class TypedVariable
    def initialize(line)
      @line = line
    end

    def parse(serial)
      # Remove default argument
      newArgStr = @line.dup

      splitter = ChompAfterDelimiter.new(@line, "=")
      poststr = splitter.str
      defaultValueBlock = splitter.tailStr
      str, arrayBlock = splitArrayBlock(poststr)

      # Assume T(f)(args) as a pointer to a function
      # T(f[])(args) is not supported
      phraseSet = StringOfParenthesis.new(str).parse
      if phraseSet && (phraseSet.map { |p| p[1] }.compact.size >= 2)
        return parseFuncPtrArg(phraseSet, defaultValueBlock, serial)
      end

      wordSet = str.gsub(/([\*&])/, ' \1 ').strip.split(" ")
      if (wordSet.size <= 1 ||
          Mockgen::Constants::MEMFUNC_WORD_END_OF_TYPE_SET.any? { |word| word == wordSet[-1] })
        argType = wordSet.join(" ")
        argName = "dummy#{serial}"
        newArgStr = "#{str} #{argName}"
        newArgStr += arrayBlock if arrayBlock
      else
        argType = wordSet[0..-2].join(" ")
        argName = wordSet[-1]
      end

      argType += arrayBlock if arrayBlock
      return argType, argName, newArgStr
    end

    def parseAsArgument(serial)
      parse(serial)
    end

    def parseAsMemberVariable
      argType, argName, newArgStr = parse(0)
      # move int[] a to int a[]
      typeStr, arrayBlock = splitArrayBlock(argType)
      if arrayBlock
        argType = typeStr
        argName += arrayBlock
      end
      return argType, argName
    end

    def splitArrayBlock(phrase)
      # Check int array[] and int[] array
      arrayBlock = nil
      str = phrase
      leftpos = phrase.index("[")
      rightpos = phrase.index("]")
      if leftpos && rightpos && (leftpos < rightpos)
        arrayBlock = phrase[leftpos..rightpos]
        str = phrase.dup
        str.slice!(leftpos, rightpos - leftpos + 1)
      end

      return str, arrayBlock
    end

    def parseFuncPtrArg(phraseSet, defaultValueBlock, serial)
      phCount = 0
      argTypeSet = []
      argName = nil
      newArgStrSet = []

      phraseSet.reverse.each do |phrase, inParenthesis|
        if inParenthesis
          phCount += 1
          if phCount == 2
            argType, argName, argStr = extractFuncPtrName(inParenthesis, serial)
            argTypeSet << "(#{argType})"
            newArgStrSet << "(#{argStr})"
          else
            argTypeSet << phrase
            newArgStrSet << phrase
          end
        else
          argTypeSet << phrase
          newArgStrSet << phrase
        end
      end

      newArgStrSet.insert(0, defaultValueBlock) if defaultValueBlock
      return argTypeSet.reverse.join(" "), argName, newArgStrSet.reverse.join(" ")
    end

    def extractFuncPtrName(phrase, serial)
      argType = phrase.gsub(/[^\*]/, "")
      name = phrase.tr("*", "")
      argStr = phrase

      if (name.empty?)
        argType = phrase.include?("*") ? phrase : ""
        name = "dummy#{serial}"
        argStr = argType + name
      end

      return argType, name, argStr
    end
  end

  # Extract argument variables from a typed argument list
  class ArgVariableSet
    attr_reader :preFuncSet, :postFuncSet, :funcName
    attr_reader :argSetStr, :argSetWithoutDefault, :argTypeStr, :argNameStr

    def initialize(line)
      # Allow nil for testing
      return unless line

      replacedLine = replaceNullExpression(line)
      argSetStr, @preFuncSet, @postFuncSet, @funcName = splitByArgSet(replacedLine)
      @argSetStr, @argSetWithoutDefault, @argTypeStr, @argNameStr = extractArgSet(argSetStr) if argSetStr
    end

    def replaceNullExpression(line)
      result = line.dup
      Mockgen::Constants::KEYWORD_NULL_EXPR_CLANG_SET.each do |keyword|
        result.gsub!(keyword, Mockgen::Constants::KEYWORD_NULL_EXPR_CPP)
      end
      result
    end

    def splitByArgSet(line)
      argSetStr = nil
      preFuncSet = []
      postFuncSet = []
      funcName = ""

      phraseSet = StringOfParenthesis.new(line).parse
      phraseSet.reverse.each do |phrase, inParenthesis|
        if inParenthesis
          if argSetStr
            preFuncSet << phrase
          else
            md = phrase.match(/^\((.*)\)$/)
            argSetStr = md[1]
          end
        else
          if argSetStr
            preFuncSet << phrase
          else
            postFuncSet << phrase
          end
        end
      end

      # Preserve delimiters *, & and white spaces
      preFuncWordSet = preFuncSet.reverse.join(" ").split(/([\*&\s]+)/)
      funcName = preFuncWordSet[-1].strip unless preFuncWordSet.empty?
      preFuncStr = (preFuncWordSet.size > 1) ? preFuncWordSet[0..-2].map(&:strip).join(" ").gsub(/\s+/, " ").strip : ""
      return argSetStr, preFuncStr, postFuncSet.reverse.join(" "), funcName
    end

    def extractArgSet(line)
      argSetStr = line.strip
      return ["", "", "", ""] if argSetStr.empty? || (argSetStr == "void")

      serial = 1
      argTypeSet = []
      argNameSet = []
      newArgStrSet = []
      argSetWithoutDefaultSet = []
      argSetStr.split(/,/).each do |argStr|
        argType, argName, newArgStr = TypedVariable.new(argStr.strip).parseAsArgument(serial)
        argTypeSet << argType
        argNameSet << argName
        newArgStrSet << newArgStr

        poststr = ChompAfterDelimiter.new(newArgStr, "=").str
        argSetWithoutDefaultSet << poststr
        serial += 1
      end

      return newArgStrSet.join(","), argSetWithoutDefaultSet.join(","), argTypeSet.join(","), argNameSet.join(",")
    end
  end

  # External or class member variable (not extern typename)
  class VariableStatement < BaseBlock
    # the className is CV removed and dereferenced
    attr_reader :varName, :className

    def initialize(line)
      super
      @varName = ""
      # array[4] -> array
      @varNameNoCardinality = varName
      @className = ""
      @typeStr = ""
      @canTraverse = false
      parse(line.gsub(/\s*[{;].*$/,"").strip)
    end

    def canTraverse
      @canTraverse
    end

    def filterByReferences(reference)
      reference.memberName == @varNameNoCardinality
    end

    def makeStubDef(className)
      "#{@typeStr} #{getFullname()};\n"
    end

    ## Implementation detail (public for testing)
    def parse(line)
      return if line.empty? || line[-1] == ")" || line.include?("__")

      # Exclude member functions and type aliases
      wordSet = line.split(" ")
      return if wordSet.nil? || wordSet.empty?
      return if Mockgen::Constants::MEMVAR_FIRST_WORD_REJECTED_SET.any? { |word| word == wordSet[0] }
      return if Mockgen::Constants::MEMVAR_LAST_WORD_REJECTED_SET.any? { |word| word == wordSet[-1] }

      newWordSet = wordSet.reject do |word|
        Mockgen::Constants::MEMVAR_FIRST_WORD_EXCLUDED_SET.any? { |key| key == word }
      end
      return if newWordSet.size < 2

      className = getNonTypedFullname("")
      typeName, varName = TypedVariable.new(newWordSet.join(" ")).parseAsMemberVariable
      return if typeName.empty? || varName.empty?

      @varName = varName
      @varNameNoCardinality = ChompAfterDelimiter.new(varName, "[").str
      @className = ChompAfterDelimiter.new(typeName, "[").str.split(/[\*&\s]+/)[-1]
      @typeStr = typeName
      @canTraverse = true
    end

    def getFullname
      getNonTypedFullname(@varName)
    end
  end

  # Extern Variable (not extern typename)
  class ExternVariableStatement < VariableStatement
    def initialize(line)
      super
    end

    def isNonMemberInstanceOfClass?
      @canTraverse
    end
  end

  # Extern Variable (not extern typename)
  class MemberVariableStatement < VariableStatement
    def initialize(line)
      super
      # Now use only static (class instance) variables
      @canTraverse = false unless line =~ /\bstatic\b/
    end
  end

  # Remove unrelated trailing sentence
  class ChompAfterDelimiter
    attr_reader :str, :tailStr

    def initialize(argStr, delimiter)
      pos = argStr.index(delimiter)
      @str = pos ? argStr[0..(pos-1)].rstrip : argStr.dup
      @tailStr = pos ? argStr[pos..-1] : nil
    end
  end

  # Compare argument types between a linker output and a source file
  class FunctionReferenceSet
    def initialize(block, reference, name, argTypeStr, postFunc)
      @scopedBlock = block
      @name = name
      @name ||= ""
      @argTypeStr = argTypeStr
      @argTypeStr ||= ""
      @postFunc = postFunc
      @postFunc ||= ""
      @refName = reference.memberName if reference
      @refName ||= ""
      @refArgTypeStr = reference.argTypeStr if reference
      @refArgTypeStr ||= ""
      @refPostFunc = reference.postFunc if reference
      @refPostFunc ||= ""
    end

    def compare
      # Resolve typedefs because linkers know the exact type
      # after its aliases are solved but the clang front end does not
      # know the aliases
      argTypeStr = sortArgTypeStr(@scopedBlock.resolveAlias(@argTypeStr))
      refArgTypeStr = sortArgTypeStr(@scopedBlock.resolveAlias(@refArgTypeStr))

      # const char * and char const * are equivalent
      # Distinguish const and non-const functions
      (@refName == @name) &&
        (argTypeStr == refArgTypeStr) &&
        (postFunctionPhrase(@postFunc) == postFunctionPhrase(@refPostFunc))
    end

    def sortArgTypeStr(argTypeStr)
      # Remove spaces between * and &
      str = argTypeStr.gsub(/([\*&,]+)\s*/, '\1')
      # Do not sort beyond * and &
      # Do not mix diffrent types
      str.split(/([\*&,])/).map { |phrase| phrase.split(" ").sort }.join("")
    end

    def postFunctionPhrase(phrase)
      phrase.split(" ").map do |poststr|
        Mockgen::Constants::MEMFUNC_WORD_COMPARED_SET.any? { |word| word == poststr } ? poststr : ""
      end.join("")
    end

    # Collect type aliases in the scoped block
    def collectAliases
      typeAliasSetChain = []

      block = @scopedBlock
      while block
        typeAliasSetChain << block.typeAliasSet
        block = block.parent
      end

      typeAliasSetChain
    end
  end

  # Constructor with arbitrary number of arguments
  # Templates are not supported yet
  class ConstructorBlock < BaseBlock
    def initialize(line, className)
      super(line)
      @className = className
      @valid, @typedArgSet, @argTypeStr, @callBase = parse(line, className)
    end

    def parse(line, className)
      phrase = removeInitializerList(line)
      return [false, "", ""] unless md = phrase.match(/^\s*#{className}\s*\(\s*(.*)\s*\)/)
      typedArgSet = md[1]

      argVariableSet = ArgVariableSet.new(phrase)
      typedArgSet = argVariableSet.argSetStr
      argSet = argVariableSet.argNameStr
      argTypeStr = argVariableSet.argTypeStr

      # Add a comma to cascade other initializer arguments
      callBase = (argSet.empty?) ? "" : "#{className}(#{argSet}), "
      [true, typedArgSet, argTypeStr, callBase]
    end

    def removeInitializerList(line)
      ChompAfterDelimiter.new(line, ":").str
    end

    def canTraverse
      @valid
    end

    def filterByReferences(reference)
      FunctionReferenceSet.new(self, reference, @className, @argTypeStr, "").compare
    end

    # Non default constructive base classes are not supported yet
    def makeStubDef
      fullname = getNonTypedFullname(@className)
      "#{fullname}::#{@className}(#{@typedArgSet}) {}\n"
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

  class DestructorBlock < BaseBlock
    def initialize(line, className)
      @className = className
      @name = "~#{className}"
      # Create a non-virtual default destructor if arg line is nil
      parsedLine = line
      parsedLine ||= "~#{@name} ()"

      super(parsedLine)
      @valid = (parsedLine =~ /#{@name}\s*\(/) ? true : false
    end

    def canTraverse
      @valid
    end

    def filterByReferences(reference)
      reference.memberName == @name
    end

    def makeStubDef
      fullname = getNonTypedFullname(@className)
      "#{fullname}::~#{@className}() {}\n"
    end
  end

  # Remove attribute
  class LineWithoutAttribute
    attr_reader :str

    def initialize(line)
      @str = removeAttribute(line)
    end

    # Remove attributes and qualifiers
    def removeAttribute(phrase)
      newphrase = phrase.dup

      Mockgen::Constants::KEYWORD_ATTRIBUTE_WITH_ARGS.each do |keyword|
        # Recursive regular expression
        newphrase.gsub!(/\b#{keyword}\s*(?<p>\(\s*[^(]*(\g<p>|([^()]*)\s*)\s*\)|)\s*/,"")
      end

      Mockgen::Constants::KEYWORD_ATTRIBUTE_WITHOUT_ARGS.each do |keyword|
        newphrase.gsub!(/\b#{keyword}\b/, "")
      end

      newphrase.strip
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
      @argTypeStr = ""
      @argSet = ""
      @funcName = ""
      @typedArgSet = ""
      @typedArgSetWithoutDefault = ""
      @argSignature = ""
      @postFunc = ""

      # Remove trailing ; and {
      body = line
      if md = line.match(/^([^{;]*)[{;]/)
        body = md[1]
      end

      # Remove trailing spaces
      parse(body.rstrip)
    end

    def canTraverse
      @valid
    end

    def filterByReferences(reference)
      FunctionReferenceSet.new(self, reference, @funcName, @argTypeStr, @postFunc).compare
    end

    ## Public methods added on the base class
    def override?(block)
      @argSignature == block.argSignature
    end

    def makeMockDef(className)
      constStr = (@constMemfunc) ? "_CONST" : ""

      numberOfArgs = @typedArgSetWithoutDefault.split(/,/).size
      # Treat void-only arg empty
      numberOfArgs = 0 if @argSet.empty?

      str = "    MOCK#{constStr}_METHOD#{numberOfArgs}"
      str += "(#{@funcName},#{@returnType}(#{@typedArgSetWithoutDefault}));\n"
      str
    end

    def makeStubDef(className)
      constStr = (@constMemfunc) ? "const " : ""

      numberOfArgs = @typedArgSetWithoutDefault.split(/,/).size
      # Treat void-only arg empty
      numberOfArgs = 0 if @argSet.empty?

      str = "#{@returnType} #{className}::#{@funcName}(#{@typedArgSetWithoutDefault}) #{constStr}{\n"
      # Force to cast enums
      returnType = @returnType.tr("&","").strip
      str += "    return" + (@returnVoid ? "" : " static_cast<#{returnType}>(0)") + ";\n"
      str += "}\n"
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
      # Split by , and collect variable names ahead ,
      # Note : assuming variable names are not omitted
      argVariableSet = ArgVariableSet.new(line)
      @typedArgSet = argVariableSet.argSetStr
      return false unless @typedArgSet
      @typedArgSetWithoutDefault = argVariableSet.argSetWithoutDefault

      @preFunc = argVariableSet.preFuncSet
      postFunc = argVariableSet.postFuncSet
      funcName = argVariableSet.funcName
      @argTypeStr = argVariableSet.argTypeStr
      @argSet  = argVariableSet.argNameStr

      # Exclude destructors
      return false if funcName.include?("~")
      # Skip pure virtual functions
      return false if isPureVirtual(postFunc)
      # Operators are not supported
      return false if line.match(/\boperator\b/)

      @constMemfunc = isConstMemberFunction(postFunc)
      @staticMemfunc, @returnType, @returnVoid = extractReturnType(@preFunc)
      @decl = @returnType + " #{funcName}(" + @typedArgSet + ")"
      @decl = @decl+ " " + postFunc unless postFunc.empty?

      @argSignature = extractArgSignature(funcName, @argTypeStr, @constMemfunc)

      @funcName = funcName
      @postFunc = postFunc
      @valid = true
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
    def extractArgSignature(funcName, argTypeStr, constMemfunc)
      constStr = constMemfunc ? "const" : ""
      "#{funcName}(#{argTypeStr})#{constStr}"
    end

    def splitByReferenceMarks(phrase)
      phrase.gsub(/([\*&])/, ' \1 ').split(/\s+/).reject { |w| w =~ /^\s*$/ }
    end
  end

  # Class and struct
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
      @filtered = false    # Filter undefined functions
      @destructor = nil    # None or one instance

      # Remove trailing ; and {
      body = line
      if md = line.match(/^([^{;]*)[{;]/)
        body = md[1]
      end

      # Template is not supported yet
      @valid = false
      unless body.match(/^\s*template\s+/)
        # Determine whether this block can be handled after parsed
        @valid = parseClassName(body)
      end

      # One or more constructors
      @destructor = nil
      @constructorSet = []
      @memberFunctionSet = []
      @memberVariableSet = []

      # Candidates to make stubs
      @undefinedDestructor = nil
      @undefinedConstructorSet = []
      @undefinedFunctionSet = []
      @undefinedVariableSet = []

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
        destructorBlock = DestructorBlock.new(line, @name)
        if (destructorBlock.canTraverse)
          newBlock = destructorBlock
          @destructor = destructorBlock
        elsif isConstructor(line)
          newBlock = ConstructorBlock.new(line, @name)
          @constructorSet << newBlock if newBlock.canTraverse
        elsif isPointerToFunction(line)
          # Not supported yet
        else
          newBlock = MemberVariableStatement.new(line)
          if newBlock.canTraverse
            @memberVariableSet << newBlock
          else
            newBlock = MemberFunctionBlock.new(line)
            @memberFunctionSet << newBlock if newBlock.canTraverse
          end
        end
        block = newBlock if !newBlock.nil? && newBlock.canTraverse
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

    def filterByReferences(referenceSet)
      constructorBlockSet = []
      functionBlockSet = []
      variableBlockSet = []
      @filtered = false

      return if referenceSet.nil? || !referenceSet.valid
      referenceSet.refSet.each do |ref|
        next if ref.classFullname != getFullname
        @filtered = true

        # Create a default destructor if it does not exist
        @destructor ||= DestructorBlock.new(nil, @name)
        @undefinedDestructor = @destructor if @destructor.filterByReferences(ref)

        @constructorSet.each do |block|
          constructorBlockSet << block if block.filterByReferences(ref)
        end

        @memberFunctionSet.each do |block|
          functionBlockSet << block if block.filterByReferences(ref)
        end

        # Assume instance variables do not appear in referenceSet
        @memberVariableSet.each do |block|
          variableBlockSet << block if block.filterByReferences(ref)
        end
      end

      @undefinedConstructorSet = constructorBlockSet.uniq
      @undefinedFunctionSet = functionBlockSet.uniq
      @undefinedVariableSet = variableBlockSet.uniq
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

    def makeStubSet
      @mockClassDef = ""
      @mockClassFunc = formatStub()
      @decoratorClassDef = ""
      @decoratorClassDef = ""
      @staticMockDef = ""
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

      return false if Mockgen::Constants::CLASS_NAME_EXCLUDED_SET.any? { |name| name == @name }

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

      src += formatStub()
      return str, src
    end

    def formatStub
      src = ""
      name = getFullname()

      @undefinedConstructorSet.each do |member|
        src += member.makeStubDef if member.canTraverse
      end

      if !@undefinedDestructor.nil? && @undefinedDestructor.canTraverse
        src += @undefinedDestructor.makeStubDef
      end

      @undefinedFunctionSet.each do |member|
        src += member.makeStubDef(name) if member.canTraverse
      end

      @undefinedVariableSet.each do |member|
        src += member.makeStubDef(name) if member.canTraverse
      end

      src
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

      @memberFunctionSet.each do |member|
        next unless member.canTraverse
        next if derivedSet.any? { |f| f.override?(member) }
        name = getFullname
        str += member.send(methodFunc, name)
        derivedSet << member
      end

      # Search base classes for functions
      @baseClassBlockSet.each do |block|
        # Mix functions of sibling base classes assuming
        # D is derived from B1, B2, B1:f and F2:f are defined
        # but D:f is not defined.
        # Though calling D:f is ambiguous in this case,
        # it is practical to create exactly one Mock(D)::f.
        str += block.send(methodClass, derivedSet)
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

    def createBlock(argLine, parentBlock)
      line = LineWithoutAttribute.new(argLine).str
      block = BaseBlock.new(line)
      newBlock = nil
      typedefBlock = nil

      # Switch by the first keyword of the line
      if md = line.match(/^(.*\S)\s*{/)
        words = line.split(/\s+/)
        if words[0] == "typedef"
          typedefBlock = TypedefBlock.new(line)
          line.gsub!(/^typedef\s+/, "")
          words.shift
        end

        case words[0]
        when "namespace"
          newBlock = NamespaceBlock.new(line)
        when "extern"
          newBlock = ExternCBlock.new(line) if words[1] == '"C"'
        when "class"
          newBlock = ClassBlock.new(line)
        # class is a syntactic sugar as private struct
        when "struct"
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
          when "typedef"
            newBlock = TypedefBlock.new(line)
          when "extern"
            newBlock = ExternVariableStatement.new(line)
          end
        end
      end

      # Delegate to the current class block
      newBlock = parentBlock.parseChildren(line) unless newBlock

      if newBlock
        block = newBlock
        block.attachTypedefBlock(typedefBlock)
      end
      block
    end
  end

  # Class name and its instances
  class ClassInstance
    attr_reader :typeSwapperStr, :varSwapperStr, :declStr, :defStr

    def initialize(typeSwapperStr, varSwapperStr, declStr, defStr)
      @typeSwapperStr = typeSwapperStr
      @varSwapperStr = varSwapperStr
      @declStr = declStr
      @defStr = defStr
    end
  end

  # All class names and their instances
  class ClassInstanceMap
    def initialize
      @set = {}
    end

    def add(className, typeSwapperStr, varSwapperStr, declStr, defStr)
      if @set.key?(className)
        entry = @set[className]
      else
        entry = Array.new
        @set[className] = entry
      end

      entry << ClassInstance.new(typeSwapperStr, varSwapperStr, declStr, defStr)
    end

    def getInstanceSet(className)
      @set.key?(className) ? @set[className] : []
    end
  end

  # Undefined reference
  class UndefinedReference
    attr_reader :fullname, :classFullname, :memberName, :argTypeStr, :postFunc

    def initialize(line)
      @fullname, @classFullname, @memberName, @argTypeStr, @postFunc = parse(line)
    end

    def parse(line)
      fullname = nil

      if md = line.match(/#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE}\s+\W([^\(]+)[\(']/)
        symbol = md[1]
        prefix = (symbol.scan("::").size > 1) ? "::" : ""
        fullname = prefix + symbol

        classFullname = ""
        memberName = ""
        nameSet = symbol.split("::")
        if (!nameSet.nil? && !nameSet.empty?)
          classFullname = prefix + nameSet[0..-2].join("::")
          memberName = nameSet[-1]
        end

        argTypeStr = nil
        postFuncSet = []
        phraseSet = StringOfParenthesis.new(line.tr("`'","")).parse
        phraseSet.reverse.each do |phrase, inParenthesis|
          if inParenthesis
            argTypeStr = inParenthesis
            break
          end
          poststr = phrase.strip
          # Remove override and final when it compares to references
          postFuncSet << poststr
        end
        postFunc = argTypeStr ? postFuncSet.join("") : ""
      end

      return fullname, classFullname, memberName, argTypeStr, postFunc
    end
  end

  # Undefined reference set
  class UndefinedReferenceSet
    attr_reader :valid, :refSet

    def initialize(filename)
      @valid = false
      @refSet = []
      return unless filename

      # Filename may not exist in clean build
      if File.exist?(filename)
        File.open(filename, "r") { |file|
          @refSet = readAllLines(file)
        }
      end

      @valid = !@refSet.empty?
    end

    ## Implementation detail (public for testing)
    def readAllLines(file)
      refSet = []
      while rawLine = file.gets
        ref = UndefinedReference.new(rawLine.chomp)
        refSet << ref if ref.fullname
      end
      refSet
    end
  end

  # Parse input file lines and format output strings
  class CppFileParser
    # cppNameSpace : namespace of generated codes
    # originalFilename : a file before processed by clang
    # convertedFilename : a file after processed by clang
    def initialize(cppNameSpace, originalFilename, linkLogFilename, convertedFilename, stubOnly)
      abort if cppNameSpace.nil? || cppNameSpace.empty?
      @cppNameSpace = cppNameSpace
      @inputFilename = originalFilename
      @blockFactory = BlockFactory.new
      @stubOnly = stubOnly

      # Current parsing block
      @block = @blockFactory.createRootBlock
      @classInstanceMap = ClassInstanceMap.new

      # Allow nil for testing
      return unless originalFilename
      return unless convertedFilename

      File.open(convertedFilename, "r") { |file|
        readAllLines(file)
      }

      referenceSet = parseLinkLog(linkLogFilename)
      buildClassTree(referenceSet)
      makeClassSet
    end

    # Write generated codes to arg files
    def writeToFiles(argClassFilename, argTypeSwapperFilename, argVarSwapperFilename, argDeclFilename, argDefFilename)
      beginNamespace = "namespace #{@cppNameSpace} {\n\n"
      endNamespace = "} // namespace #{@cppNameSpace}\n\n"
      usingNamespace = "using namespace #{@cppNameSpace};\n"

      classFilenameSet = []
      typeSwapperFilenameSet = []
      varSwapperFilenameSet = []
      declFilenameSet = []

      index = 0
      serial = 1
      sizeOfSet = Mockgen::Constants::GENERATED_BLOCKS_PER_SOURCE
      blockSet = @block.children.select(&:canTraverse).compact

      while(index < blockSet.size)
        subBlockSet = blockSet.slice(index, sizeOfSet)

        suffix = "_#{serial}."
        classFilename = argClassFilename.gsub(".", suffix)
        typeSwapperFilename = argTypeSwapperFilename.gsub(".", suffix)
        varSwapperFilename = argVarSwapperFilename.gsub(".", suffix)
        declFilename = argDeclFilename.gsub(".", suffix)
        defFilename = argDefFilename.gsub(".", suffix)

        argBlockSet = @stubOnly ? [] : subBlockSet
        writeClassFilename(classFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        writeTypeSwapperFile(typeSwapperFilename, classFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        writeVarSwapperFile(varSwapperFilename, declFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        writeDeclFile(declFilename, classFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)

        if @stubOnly
          writeStubFilename(defFilename, @inputFilename, subBlockSet)
        else
          writeDefFilename(defFilename, classFilename, declFilename, beginNamespace, endNamespace, usingNamespace, subBlockSet)
        end

        classFilenameSet << classFilename
        typeSwapperFilenameSet << typeSwapperFilename
        varSwapperFilenameSet << varSwapperFilename
        declFilenameSet << declFilename

        serial += 1
        index += sizeOfSet
      end

      writeAggregatedFiles(argClassFilename, classFilenameSet)
      writeAggregatedFiles(argTypeSwapperFilename, typeSwapperFilenameSet)
      writeAggregatedFiles(argVarSwapperFilename, varSwapperFilenameSet)
      writeAggregatedFiles(argDeclFilename, declFilenameSet)
      0
    end

    ## Implementation detail (public for testing)
    def readAllLines(file)
      while rawLine = file.gets
        line = Mockgen::Common::LineWithoutCRLF.new(rawLine).line.strip
        next if line.empty?
        parseLine(line.strip)
      end
    end

    # line : leading and trailing spaces and CRLF must be removed
    # Parse and discard inline function definitions
    def parseLine(line)
      block = @blockFactory.createBlock(line, @block)

      # } else {
      if (line[0] == "}") && (line[-1] == "{")
      # End of a block
      elsif (line[0] == "}")
        # End of a block
        wordSet = line.tr(";", "").split(" ")
        @block.setTypedef(wordSet[1]) if wordSet.size > 1

        childBlock = @block
        parentBlock = @block.parent
        @block = parentBlock
      else
        # Connect "... {" and "... ;" lines
        @block.connect(block)
        # Beginning of a block
        if (line[-1] == "{")
          @block = block
        end
      end
    end

    def eliminateUnusedBlock(block)
      parent = block.parent
      parent.disconnect(block) if parent
    end

    def eliminateAllUnusedBlock(argBlock)
      argBlock.children.each do |child|
        if child.canTraverse && child.canMock
          eliminateAllUnusedBlock(child)
        else
          eliminateUnusedBlock(child)
        end
      end
    end

    def parseLinkLog(linkLogFilename)
      UndefinedReferenceSet.new(linkLogFilename)
    end

    def buildClassTree(referenceSet)
      # Resolve aliases before find undefined references
      collectTypedefs(@block)
      # classSet : class name => block
      classSet = collectClasses(@block.children, referenceSet)
      connectClasses(@block.children, classSet)

      varSet = collectVariables(@block.children)
      @classInstanceMap = makeTypeVarAliases(varSet, classSet)

      eliminateAllUnusedBlock(@block)
    end

    def collectClasses(rootBlockSet, referenceSet)
      classSet = {}  # class name => block
      lambdaToBlock = lambda do |block|
        block.filterByReferences(referenceSet)
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

      doForAllBlocks(rootBlockSet, lambdaToBlock, :isNonMemberInstanceOfClass?)
      varSet
    end

    def collectTypedefs(rootBlock)
      lambdaToBlock = lambda do |block|
        block.collectAliases
      end

      # Construct top-level namespace typedefs
      rootBlock.collectAliases
      # Construct block scoped typedefs
      doForAllBlocks(rootBlock.children, lambdaToBlock, :canTraverse)

      # Construct top-level namespace typedefs including extern "C" {}
      typedefArray = collectTopLevelTypedefSet(rootBlock).flatten.compact
      rootTypedef = rootBlock.typeAliasSet
      typedefArray.each { |typedefSet| rootTypedef.merge(typedefSet) }
      rootBlock.typeAliasSet.removeSystemInternalSymbols
    end

    def makeTypeVarAliases(varSet, classSet)
      classInstanceMap = ClassInstanceMap.new
      varSet.each do |varName, varFullname, className|
        elements = makeTypeVarAliasElements(classSet, varName, varFullname, className)
        classInstanceMap.add(className, *elements)
      end

      classInstanceMap
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
      if @stubOnly
        lambdaToBlock = lambda { |block| block.makeStubSet }
      else
        lambdaToBlock = lambda { |block| block.makeClassSet }
      end
      doForAllBlocks(@block.children, lambdaToBlock, :isClass?)

      # Cascade another method
      self
    end

    # Breadth-first search blocks and apply arg lambdas to them
    def doForAllBlocks(argBlockSet, lambdaToBlock, labelToCheck)
      blockSet = argBlockSet

      while(!blockSet.empty?)
        nextBlockSet = []
        blockSet.each do |block|
          lambdaToBlock.call(block) if block.send(labelToCheck)
          nextBlockSet.concat(block.children)
        end

        blockSet = nextBlockSet
      end
    end

    def collectTopLevelTypedefSet(argBlock)
      typeAliasSetArray = []
      argBlock.children.each do |child|
        typeAliasSetArray << child.typeAliasSet if child.getNamespace.empty?
        typeAliasSetArray << collectTopLevelTypedefSet(child) if child.getNamespace.empty?
      end

      typeAliasSetArray
    end

    def writeFile(filename, labelAttr, preStr, postStr, blockSet)
      lineDelimiter = "\n"

      strSet = []
      lambdaToBlock = lambda do |block|
        @classInstanceMap.getInstanceSet(block.getFullname).each do |instance|
          lineSet = instance.send(labelAttr)
          strSet << lineSet
        end
      end

      File.open(filename, "w") do |file|
        file.puts preStr
        doForAllBlocks(blockSet, lambdaToBlock, :isClass?)
        # Write each definition exactly once
        file.puts strSet.uniq
        file.puts ""
        file.puts postStr
      end
    end

    def writeClassFilename(filename, beginNamespace, endNamespace, usingNamespace, blockSet)
      File.open(filename, "w") do |file|
        file.puts getClassFileHeader(@inputFilename, filename)
        file.puts beginNamespace
        lambdaToBlock = lambda do |block|
          str = block.getStringToClassFile
          file.puts str if str
        end
        doForAllBlocks(blockSet, lambdaToBlock, :isClass?)
        file.puts endNamespace
        file.puts getIncludeGuardFooter
      end
    end

    def writeTypeSwapperFile(filename, classFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getSwapperHeader(classFilename, filename, "Class")
      postStr = getIncludeGuardFooter
      writeFile(filename, :typeSwapperStr, preStr, postStr, blockSet)
    end

    def writeVarSwapperFile(filename, declFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getSwapperHeader(declFilename, filename, "Variable")
      postStr = getIncludeGuardFooter
      writeFile(filename, :varSwapperStr, preStr, postStr, blockSet)
    end

    def writeDeclFile(filename, classFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getDeclHeader(classFilename, filename) + "\n" + beginNamespace
      postStr = endNamespace + "\n" + getIncludeGuardFooter
      writeFile(filename, :declStr, preStr, postStr, blockSet)
    end

    def writeDefFilename(filename, classFilename, declFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getDefHeader(@inputFilename, classFilename, declFilename) + "\n" + beginNamespace
      postStr = endNamespace + "\n" + usingNamespace
      writeFile(filename, :defStr, preStr, postStr, blockSet)
      writeSourceFilename(filename, blockSet)
    end

    def writeStubFilename(filename, inputFilename, blockSet)
      File.open(filename, "w") do |file|
        file.puts getIncludeDirective(inputFilename)
      end
      writeSourceFilename(filename, blockSet)
    end

    def writeSourceFilename(filename, blockSet)
      File.open(filename, "a") do |file|
        lambdaToBlock = lambda do |block|
          str = block.getStringToSourceFile
          file.puts str if str
        end
        doForAllBlocks(blockSet, lambdaToBlock, :isClass?)
      end
    end

    # Write include files to include all
    def writeAggregatedFiles(filename, includedFilenameSet)
      File.open(filename, "w") do |file|
        file.puts "// Include files to all\n"
        file.puts "// This file is machine generated.\n\n"
        file.puts getIncludeGuardHeader(filename)
        includedFilenameSet.each do |includedFilename|
          file.puts getIncludeDirective(includedFilename)
        end
        file.puts ""
        file.puts getIncludeGuardFooter
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
      abort if argv.size < 8

      argSet = argv.dup
      @inputFilename = argSet.shift
      @inLinkLogFilename = argSet.shift
      @convertedFilename = argSet.shift
      @outClassFilename = argSet.shift
      @outTypeSwapperFilename = argSet.shift
      @outVarSwapperFilename = argSet.shift
      @outDeclFilename = argSet.shift
      @outDefFilename = argSet.shift
      @clangArgs = quotePath(argSet)
      @stubOnly = false

      # Can set later
      @cppNameSpace = Mockgen::Constants::GENERATED_SYMBOL_NAMESPACE
    end

    def generate
      # Tempfile.create cannot create a tempfile in MinGW
      # and need to specify a filename for clang to output.
      # It is also useful to know how clang format .hpp files.
      system("#{Mockgen::Constants::CLANG_COMMAND} #{@clangArgs} #{@inputFilename} > #{@convertedFilename}")

      parseHeader(@cppNameSpace, @inputFilename, @inLinkLogFilename, @convertedFilename, @stubOnly)

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

    def parseHeader(cppNameSpace, inputFilename, linkLogFilename, convertedFilename, stubOnly)
      parser = CppFileParser.new(cppNameSpace, inputFilename, linkLogFilename, convertedFilename, stubOnly)
      args = [@outClassFilename, @outTypeSwapperFilename, @outVarSwapperFilename]
      args.concat([@outDeclFilename, @outDefFilename])
      parser.writeToFiles(*args)
    end
  end
end
