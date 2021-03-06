#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Collect C++ class definitions in a .hpp file and generate codes.
# + Decorator class which contains MOCK_*METHOD* methods
# + Forwarder variable (class instance) to delegate methods its
#   original variable or a mock instance
# + Stubs to undefined functions
#
# Collect free standing functions in a .hpp file and generate codes.
# + Forwarder class and global variable instance to switch call to the
#   functions or their mocks
# + Stubs to undefined functions

require 'fileutils'
require 'open3'
require 'tempfile'
require_relative './mockgenConst.rb'
require_relative './mockgenCommon.rb'

include MockgenFeatures

module Mockgen
  class MockgenInvalidArgumentError < StandardError
  end

  class MockgenRuntimeError < StandardError
  end

  class RegexpMetaCharacter
    def initialize
    end

    def contained?(str)
      Mockgen::Constants::REGEXP_META_CHARACTER_SET.any? { |c| str.include?(c) }
    end
  end

  class TypeStringWithoutModifier
    attr_reader :strSet
    def initialize(typeStrSet)
      strSet = typeStrSet.map do |str|
        str.split(/([\[\]])/).map { |substr| substr.split(/([\s\*&]+)/) }
      end.flatten
      @strSet = strSet.reject do |typeword|
        word = typeword.strip
        word.empty? || Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_MAP.key?(word)
      end.map { |word| word.strip }
    end
  end

  class SymbolWithHeadNamespaceDelimiter
    attr_reader :prefix, :fullname, :nameSet
    def initialize(name, isClassName)
      @prefix = ""
      @nameSet = name.split("::").reject(&:empty?)

      if (@nameSet.size <= (isClassName ? 1 : 2))
        @fullname = name.dup
      else
        @prefix = "::"
        @fullname = ((name[0..1] != "::") ? @prefix : "") + name
      end
    end
  end

  # split by commas except ones in pointers to functions
  class BaseArgumentSplitter
    attr_reader :argSet
    def initialize(line, parser, delimiter)
      argSet = nil

      if line.include?("(")
        argSet = Mockgen::Common::StringOfParenthesis.new(line).send(parser).map do |element|
          element[1].nil? ? element[0].gsub(/,/,",,") : element[0]
        end.join(delimiter).split(/,,/).reject(&:empty?)
      else
        # Parse fast and keep spaces in words between commas
        prefix = ""
        argSet = line.split(",").map do |word|
          newWord = prefix + word.strip
          prefix = delimiter
          newWord
        end
      end

      @argSet = argSet
    end
  end

  class ArgumentSplitter < BaseArgumentSplitter
    def initialize(line)
      super(line, :parse, "")
    end
  end

  class ArgumentSplitterWithSpaces < BaseArgumentSplitter
    attr_reader :argSet
    def initialize(line)
      super(line, :parseAndKeepSpaces, " ")
    end
  end

  # Discard template parameters
  class TemplateParameterListFilter
    attr_reader :str
    def initialize(line)
      @str = Mockgen::Common::StringOfAngleBrackets.new(line).parse.map do |element|
        element[1].nil? ? element[0].strip : "<>"
      end.join(" ")
    end
  end

  # Block-scoped typedef set
  class TypeAliasSet
    # Public only to merge other instances
    attr_reader :aliasSet

    def initialize
      @aliasSet = {}
    end

    def empty?
      @aliasSet.empty?
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
        newTypeSet = resolveLocal(actualNameWordSet)
        actualNameWordSet = newTypeSet
        actualNameStr = removeRedundantSpaces(newTypeSet.join(" "))

        # Transform aliases until no more conversions needed
        break if actualNameStr == previousStr
      end

      @aliasSet[aliasName] = actualNameStr
    end

    # remove system internal definitions such as __uint32_t
    def removeSystemInternalSymbols
      @aliasSet.reject! { |key, value| isSystemInternalSymbol?(key) || isSystemInternalSymbol?(value) }
    end

    def isSystemInternalSymbol?(str)
      str.include?("__") || (!str.empty? && str[0] == "_")
    end

    def resolveAlias(typeStr)
      removeRedundantSpaces(resolveLocal(TypeStringWithoutModifier.new([typeStr]).strSet).join(" "))
    end

    def resolveLocal(actualNameWordSet)
      if MockgenFeatures.useLiteralRegexp
        # Regular expressions with literal strings are faster than ones of variable strings.
        # Do not forget to escape backslashes in strings and not to in literal regexp.
        # If it is wrong, the regexp shown below splits input words by "s".
        actualNameWordSet.map do |phrase|
          # resolve parameter lists in pointers to functions
          phrase.split(/([\(\)\*&,\s]+)/).map do |word|
            (!word.match(/[^\(\)\*&,\s]/).nil? && @aliasSet.key?(word)) ? @aliasSet[word] : word
          end.join("")
        end
      else
        # Slow but avoiding repeating a char set
        charSet = "\\(\\)\\*&,\\s"
        actualNameWordSet.map do |phrase|
          # resolve parameter lists in pointers to functions
          phrase.split(/([#{charSet}]+)/).map do |word|
            (!word.match(/[^#{charSet}]/).nil? && @aliasSet.key?(word)) ? @aliasSet[word] : word
          end.join("")
        end
      end
    end

    def removeRedundantSpaces(str)
      str.gsub(/\s+([\(\)])/, '\1').gsub(/([\(\)])\s+/, '\1')
    end
  end

  class NamespaceFilter
    def initialize(value)
      @filterSet = []
      return unless value

      @nameSet = []
      internalFilter = -> name { ((name.size > 2) && (name[0..1] == "__")) }

      case value
      when "testing"
        @nameSet << "testing"
      when "internal"
        @nameSet << "testing"
        @filterSet << internalFilter
      when "std"
        @nameSet.concat(["testing", "std", "boost", "mpl_"])
        @filterSet << internalFilter
      end

      nameFilter = -> name { @nameSet.any? { |keyword| keyword == name } }
      @filterSet << nameFilter
    end

    def exclude?(name)
      @filterSet.any? { |filter| filter.call(name) }
    end
  end

  # Not support classes in namespaces and inner classes
  class ClassNameFilter
    def initialize(classNameFilterOutSet, testedFilenameGlobSet, findStatementFilterSet, explicitClassNameSet)
      @filterOutSet = nil
      @simpleFilterOutSet = nil
      @classNameSet = nil
      @varNameSet = nil
      @explicitClassNameSet = explicitClassNameSet
      @invalidPatternSet = []

      if (testedFilenameGlobSet.empty?)
        @filterOutSet = classNameFilterOutSet.nil? ? [] : classNameFilterOutSet
        @simpleFilterOutSet = @filterOutSet.reject { |pattern| pattern.include?("::") }
      else
        @classNameSet = {}
        @varNameSet = collectClassName(testedFilenameGlobSet, findStatementFilterSet)
      end
    end

    def addVariable(className, varName)
      @classNameSet[className] = true if @classNameSet && @varNameSet && @varNameSet.key?(varName)
    end

    # match not for namespace and class name qualifiers
    def excludeSimpleName?(name)
      result = false

      # Cannot exclude classes by the while list here
      # because the classes may not have their forward declarations.
      # 1. class ExplicitlyNeeded;
      # 2. extern ExplicitlyNeeded g_explicitlyNeeded;  // discard the next definition line
      # 3. class ExplicitlyNeeded {};  // discarded by g_explicitlyNeeded
      # 4. extern ExplicitlyNeeded g_explicitlyNeeded;  // cannot discard the definition already parsed
      result = match(@simpleFilterOutSet, name) ? true : false unless @classNameSet
      result
    end

    # match for namespace and class name qualifiers
    def excludeFullname?(name)
      result = false
      if @classNameSet
        result = !(@classNameSet.key?(name) || match(@explicitClassNameSet, name))
      else
        result = @filterOutSet ? match(@filterOutSet, name) : false
      end
      result
    end

    def match(patternSet, name)
      patternSet.any? do |pattern|
        result = false
        begin
          result = name.match(/#{pattern}/)
        rescue RegexpError => e
          # The filter should convert the pattern to a Regexp and check if it is valid
          unless @invalidPatternSet.include?(pattern)
            # Once per pattern
            warn "Invalid pattern #{pattern}"
            @invalidPatternSet << pattern
          end
        end
        result
      end
    end

    def collectClassName(testedFilenameGlobSet, findStatementFilterSet)
      varNameSet = {}

      testedFilenameGlobSet.each do |pattern|
        Dir.glob(pattern).each do |filename|
          findClassName(filename, findStatementFilterSet, varNameSet)
        end
      end

      return varNameSet
    end

    def findClassName(filename, findStatementFilterSet, varNameSet)
      File.open(filename, "r") { |file|
        while line = file.gets
          parseLine(line, findStatementFilterSet, varNameSet)
        end
      }
    end

    def parseLine(line, findStatementFilterSet, varNameSet)
      findStatementFilterSet.map do |pattern|
        pattern.encode("UTF-8", *Mockgen::Constants::CHARACTER_ENCODING_PARAMETER_SET)
      end.each do |pattern|
        # Not preprocessed and collect classes more needed
        inputLine = line.encode("UTF-8", *Mockgen::Constants::CHARACTER_ENCODING_PARAMETER_SET)
        inputLine.scan(/\b(#{pattern})\s*\./).each do |array|
          varName = array[0]
          varNameSet[varName] = true unless varName.empty?
        end
      end
    end
  end

  # Filter to select functions and classes
  class SymbolFilter
    # definedReferenceSet : an instance of DefinedReferenceSet
    # undefinedReferenceSet : an instance of UndefinedReferenceSet
    # functionNameFilterSet, classNameFilterOutSet : an array of regrep strings
    attr_reader :definedReferenceSet, :undefinedReferenceSet, :functionNameFilterSet
    attr_reader :classNameFilterOutSet, :fillVtable, :noOverloading

    def initialize(definedReferenceSet, undefinedReferenceSet, functionNameFilterSet, classNameFilterOutSet, fillVtable, noOverloading)
      @definedReferenceSet = definedReferenceSet
      @undefinedReferenceSet = undefinedReferenceSet
      @functionNameFilterSet = functionNameFilterSet
      @classNameFilterOutSet = classNameFilterOutSet
      @fillVtable = fillVtable
      @noOverloading = noOverloading
    end
  end

  # Generic Block Structure
  class BaseBlock
    attr_reader :parent, :children, :typeAliasSet, :keyForParent

    # line : A head line of a block (leading and trailing spaces must be removed)
    def initialize(line)
      @line = line     # headline of a block
      @parent = nil    # nil for the root block
      @children = []   # children order by addition
      @childMap = {}   # mapping names to its index of @children
      @removedChildMap = {}  # blocks to erase
      @typedefBlock = nil    # type alias for this block

      # A unique name for a parent of this block
      # Do not set this if a block has no uniqueness such as namespaces and "extern C"
      @keyForParent = nil

      # type aliases in this block scope
      # clang splits "typedef struct tagName {} Name;" into struct and typedef
      # so this is not used now.
      @typeAliasSet = TypeAliasSet.new
    end

    # Skip to parse child members
    def skippingParse
      false
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

    # Workaround for LLVM 5.0.0
    def removeRedundantNamespace(str)
      return str unless str
      (str.strip == "std::size_t") ? "size_t" : str
    end

    def resolveAlias(typeStr)
      result = removeRedundantNamespace(typeStr).dup
      previous = result.dup

      while true
        block = self
        while(block)
          result = removeRedundantNamespace(block.typeAliasSet.resolveAlias(result))
          block = block.parent
        end

        # converged
        break if result == previous
        previous = result.dup
      end

      result
    end

    def findType(typeStr)
      aliasType = resolveAlias(typeStr)
      found = (aliasType != typeStr)
      canInitializeByZero = false
      primitive = aliasType.split(nil).any? { |word| Mockgen::Constants::MEMFUNC_WORD_RESERVED_TYPE_MAP.key?(word) }

      if primitive
        found = true
        canInitializeByZero = true
      end

      return found, canInitializeByZero
    end

    ## Derived classes override methods below
    # Return if need to traverse this block
    def canTraverse?
      false
    end

    # Do not mock structs in extern "C" {}
    def canMock?
      allOfParents?(:canMock?)
    end

    def isNamespace?
      false
    end

    def isFreeFunction?
      false
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

    def getFullNamespace
      getNonTypedFullname(getNamespace)
    end

    # If this block defines a typename, return the typename
    def getTypename
      nil
    end

    # Return a type-specific initializer if defined
    def getInitializer
      nil
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

    def getStringToDeclFile
      nil
    end

    def getStringSetToDeclInlineFile
      nil
    end

    def getStringToSwapperFile
      nil
    end

    def getStringOfStub
      nil
    end

    def getStringOfVariableDefinition
      nil
    end

    # To mock only undefined references
    def filterByReferenceSet(filter)
      true
    end

    # Append :: prefix to the arg name in a namespace
    # Be consistent for clang output
    def addTopNamespace(name)
      pos = name.index("::")
      ((pos.nil? || pos == 0) ? "" : "::") + name
    end

    # Append :: prefix to call a free function instead of a member function
    def getNameFromTopNamespace(name)
      pos = name.index("::")
      ((!pos.nil? && pos == 0) ? "" : "::") + name
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

    def allOfParents?(labelToCheck)
      result = true
      block = parent
      while block
        result &= block.send(labelToCheck)
        block = block.parent
      end

      result
    end

    def setParent(block)
      @parent = block
    end

    # Nil and empty strings are also acceptable
    def setKeyForParent(str)
      @keyForParent = (str.nil? || str.empty?) ? nil : str
    end

    def addChild(block)
      key = block.keyForParent

      if key.nil?
        # The block has no uniqueness for children of its parent
        @children << block
      else
        if !@childMap.key?(key)
          @children << block
          @childMap[key] = true
        end
      end
    end

    def removeChild(block)
      # Mark the block to erase later
      @removedChildMap[block] = true
    end

    def eraseChildren
      # Filter children at once to avoid O(n^2) loop
      @children = @children.reject{ |child| @removedChildMap.key?(child) }.uniq
      @removedChildMap = {}
    end
  end

  # Root block as the top-level namespace
  class RootBlock < BaseBlock
    def initialize(line)
      super
    end

    def canTraverse?
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

      @valid = isValid?(@name)
    end

    def canTraverse?
      @valid
    end

    def canMock?
      canTraverse?() && super
    end

    def isNamespace?
      true
    end

    def getNamespace
      @name
    end

    def isValid?(argName)
      # empty name is valid
      return false if argName.include?(Mockgen::Constants::NAMESPACE_COMPILER_INTERNAL)
      Mockgen::Constants::NAMESPACE_SKIPPED_SET.all? do |name|
        pos = argName.index(name)
        pos.nil? || pos > 0
      end
    end
  end

  # Extern "C" block
  class ExternCBlock < BaseBlock
    def initialize(line)
      super
    end

    def canTraverse?
      true
    end

    def canMock?
      false
    end

    def isNamespace?
      true
    end

    def getNamespace
      ""
    end
  end

  # Simple typedef
  class TypedefBlock < BaseBlock
    def initialize(line)
      super

      @typeAlias = nil
      @actualTypeStr = ""

      if line
        body = line.tr(";", "")
        typeAlias, actualTypeStr = parsePointerToFunction(body)
        typeAlias, actualTypeStr = parseType(body) unless actualTypeStr
        setup(typeAlias, actualTypeStr)
      end
    end

    def parseUsingDirective(aliasName, actualType)
      typeAlias = aliasName.strip

      actualTypeStr = nil
      phraseSet = splitPointerToFunction(actualType, 3)
      if phraseSet
        phraseSet[-2][0].tr!("^()*&", "")
        actualTypeStr = formatPointerToFunction(phraseSet)
      else
        actualTypeStr = formatType([actualType])
      end

      setup(typeAlias, actualTypeStr)
    end

    def setup(typeAlias, actualTypeStr)
      @typeAlias = typeAlias
      @actualTypeStr = actualTypeStr
      setKeyForParent(@typeAlias)
    end

    def parsePointerToFunction(line)
      phraseSet = splitPointerToFunction(line, 4)
      return nil, nil unless phraseSet

      # remove the typedef keyword
      phraseSet.shift
      if MockgenFeatures.useLiteralRegexp
        # Regular expressions with literal strings are faster than ones of variable strings.
        md = phraseSet[-2][1].match(/([\*&\s]*)([^\*&\s]+)([\*&\s]*)/)
      else
        charSet = "\\*&\\s"
        md = phraseSet[-2][1].match(/([#{charSet}]*)([^#{charSet}]+)([#{charSet}]*)/)
      end

      return nil, nil unless md

      typeAlias = md[2].strip
      # remove the alias name
      phraseSet[-2][0] = "(#{md[1]}#{md[3]})".tr(" ", "")
      actualTypeStr = formatPointerToFunction(phraseSet)
      return typeAlias, actualTypeStr
    end

    def splitPointerToFunction(line, minSize)
      # return fast
      return nil unless line.include?("(")

      # decltype and default arguments also contain ()
      phraseSet = Mockgen::Common::StringOfParenthesis.new(line).parse
      ((phraseSet.size < minSize) || phraseSet[-1][1].nil? || phraseSet[-2][1].nil?) ? nil : phraseSet
    end

    def formatPointerToFunction(phraseSet)
      argSet = phraseSet.pop
      ptrStr = phraseSet.pop

      str = phraseSet.map do |phrase|
        TypeStringWithoutModifier.new([phrase[0]]).strSet
      end.flatten.join(" ")

      str += ptrStr[0].tr(" ", "") + "("
      str += argSet[1].split(",").map do |phrase|
        TypeStringWithoutModifier.new([phrase]).strSet.join(" ")
      end.join(",") + ")"
    end

    def parseType(line)
      typeAlias = nil
      typeStrSet = []

      # clang splits the idiom which define a struct and its alias simultaniously,
      # from "typedef struct tagCstyleStruct {} CstyleStruct;"
      # to "struct tagCstyleStruct {}; and
      # "typedef struct tagCstyleStruct CstyleStruct;"

      # clang writes a typedef to a pointer in the form of
      # "Type *PTYPE", not "Type* PTYPE"
      wordSet = TypeStringWithoutModifier.new([line]).strSet
      if (wordSet.size >= 3)
        # typedef struct Name Alias;
        typeAlias = wordSet[-1]
        typeStrSet = wordSet[1..-2]
      end

      return typeAlias, formatType(typeStrSet)
    end

    def formatType(typeStrSet)
      TypeStringWithoutModifier.new(typeStrSet).strSet.join(" ")
    end

    def canTraverse?
      return !@typeAlias.nil?
    end

    def getTypename
      @typeAlias
    end

    # set an alias after its definition
    def setAlias(name)
      @typeAlias = name
    end

    def collectAliasesInBlock(typeAliasSet)
      return typeAliasSet if @typeAlias.nil?
      typeAliasSet.add(@typeAlias, @actualTypeStr)
      typeAliasSet
    end
  end

  # Alias of a type or a namespace
  class UsingBlock < BaseBlock
    def initialize(line)
      super
      @aliasName, @actualName = parse(line)
      setKeyForParent(@aliasName)
    end

    def parse(line)
      aliasName = nil
      actualName = nil

      # Handle only simple type aliases
      unless line.split(nil).any? { |word| ["namespace", "template"].any? { |key| word == key } }
        if topMd = line.strip.match(/^\s*using\s+([^=]+)(=.+)/)
          if md = topMd[2].tr(";{","").strip.match(/^=\s*(.+)/)
            aliasName = topMd[1].strip
            actualName = md[1]
          end
        end
      end

      return aliasName, actualName
    end

    def getTypedefBlock
      block = @actualName ? TypedefBlock.new(nil) : nil
      block.parseUsingDirective(@aliasName, @actualName) if block
      block
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

      splitter = Mockgen::Common::ChompAfterDelimiter.new(@line, "=", "")
      mainBlock = splitter.str
      defaultValueBlock = splitter.tailStr
      str, arrayBlock = splitArrayBlock(mainBlock)

      # Assume T(f)(args) as a pointer to a function
      # T(f[])(args) is not supported
      phraseSet = str.include?("(") ? Mockgen::Common::StringOfParenthesis.new(str).parse : nil

      if phraseSet && (phraseSet.map { |p| p[1] }.compact.size >= 2)
        return parseFuncPtrArg(phraseSet, defaultValueBlock, serial)
      end

      wordSet = str.gsub(/([\*&])/, ' \1 ').strip.split(nil)
      if ((wordSet.size <= 1) || Mockgen::Constants::MEMFUNC_WORD_END_OF_TYPE_MAP.key?(wordSet[-1]))
        argType = wordSet.join(" ")
        argName = "dummy#{serial}"
        newArgStr = "#{str} #{argName}"
        newArgStr += arrayBlock if arrayBlock
      else
        argType = wordSet[0..-2].join(" ")
        # Copy a namespace of the type to the arg
        argName = addNamespaceToDefaultVariable(argType, wordSet[-1])
        newArgStr = addNamespaceToDefaultVariable(argType, newArgStr)
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

    def extractFuncPtrName(argPhrase, serial)
      name = "dummy#{serial}"
      argSet = argPhrase.split(/(::|\*|&|\s+)/).map{ |word| word.strip }.reject(&:empty?)
      return "", name, name if argSet.empty?

      modifieCharSet = ["*", "&"]
      pos = nil
      (argSet.size-1).downto(0) do |i|
        word = argSet[i]
        break if word == "::"
        next if modifieCharSet.any? { |key| word == key }
        pos = i
        break
      end

      if pos
        name = argSet[pos]
        argSet.delete_at(pos)
      end

      modifierSet, bodySet = argSet.partition { |word| modifieCharSet.any? { |key| word == key } }
      modifierStr = modifierSet.join("")
      bodyStr = bodySet.join("")
      memFn = bodyStr.include?("::")
      argType = memFn ? (bodyStr + modifierStr) : (modifierStr + bodyStr)
      argStr = memFn ? (bodyStr + modifierStr + name) :  (modifierStr + bodyStr + name)
      return argType, name, argStr
    end

    def addNamespaceToDefaultVariable(namespaceStr, varName)
      return varName unless namespaceStr && namespaceStr.include?("::")
      prefix = namespaceStr.split(/(:+)/)[0..-2].join("")
      varName.gsub(/(=\s*)/, '\1' + prefix)
    end
  end

  # Extract argument variables from a typed argument list
  class ArgVariableSet
    attr_reader :preFuncSet, :postFuncSet, :funcName, :pureVirtual, :arity
    attr_reader :argSetStr, :argSetWithoutDefault, :argTypeStr, :argNameStr

    def initialize(line)
      # Allow nil for testing
      return unless line

      replacedLine = replaceNullExpression(line)
      argSetStr, @preFuncSet, @postFuncSet, @funcName, @pureVirtual = splitByArgSet(replacedLine)
      @argSetStr, @argSetWithoutDefault, @argTypeStr, @argNameStr, @arity = extractArgSet(argSetStr) if argSetStr
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
      pureVirtual = false

      phraseSet = Mockgen::Common::StringOfParenthesis.new(line).parse
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

      postFuncPhrase = postFuncSet.reverse.join(" ")
      md = postFuncPhrase.match(/([^=]*)=\s*0(.*)/)
      pureVirtual = !md.nil?
      postFuncStr = (md ? (md[1] + md[2]) : postFuncPhrase).rstrip

      return argSetStr, preFuncStr, postFuncStr, funcName, pureVirtual
    end

    def extractArgSet(line)
      argSetStr = line.strip
      return ["", "", "", "", 0] if argSetStr.empty? || (argSetStr == "void")

      serial = 1
      argTypeSet = []
      argNameSet = []
      newArgStrSet = []
      argSetWithoutDefaultSet = []

      ArgumentSplitterWithSpaces.new(argSetStr).argSet.each do |argStr|
        argType, argName, newArgStr = TypedVariable.new(argStr.strip).parseAsArgument(serial)
        argTypeSet << argType
        argNameSet << argName
        newArgStrSet << newArgStr

        poststr = Mockgen::Common::ChompAfterDelimiter.new(newArgStr, "=", "").str
        argSetWithoutDefaultSet << poststr
        serial += 1
      end

      return newArgStrSet.join(","), argSetWithoutDefaultSet.join(","), argTypeSet.join(","), argNameSet.join(","), serial - 1
    end
  end

  # Class and function template
  class TemplateParameter
    attr_reader :generic, :specialized, :typenameSet, :varSet, :type, :post

    def initialize(line)
      @generic = nil
      @specialized = nil
      @typenameSet = []
      @varSet = ""
      @type = ""
      @post = ""
      parse(line)
    end

    # Split template
    def parse(line)
      phraseSet = Mockgen::Common::StringOfAngleBrackets.new(line).parse
      return if (phraseSet.size < 3)
      return if phraseSet[0][0].strip != "template"

      generic = nil
      typeStrSet = []
      postStrSet = []
      phraseSet.shift
      phraseSet.each do |phraseStr, phraseInBlackets|
        if phraseInBlackets.nil?
          str = phraseStr.strip
          if @specialized.nil?
            typeStrSet << str
          else
            postStrSet << str
          end
        else
          str = phraseInBlackets.strip
          if generic.nil?
            generic = str
          elsif @specialized.nil?
            @specialized = str
          end
        end
      end

      generic ||= ""
      genericSet = []
      @varSet = generic.split(",").map do |str|
        # Delete default type if specified
        phrase = Mockgen::Common::ChompAfterDelimiter.new(str.strip, "=", "").str
        genericSet << phrase
        wordSet = phrase.split(/[\s\.]+/)
        postfix = str.include?("...") ? "..." : ""
        # Treat typename as typename...
        @typenameSet << wordSet[0]
        wordSet.empty? ? "" : (wordSet[-1] + postfix)
      end.join(",")

      @generic = genericSet.join(", ")
      @type = typeStrSet.join(" ")
      @post = postStrSet.join(" ")
    end
  end

  # External or class member variable (not extern typename)
  class VariableStatement < BaseBlock
    # the className is CV removed and dereferenced
    attr_reader :varName, :className, :arrayStr

    def initialize(line)
      super
      @varName = ""
      # array[4] -> array
      @varNameNoCardinality = @varName
      @className = ""
      @typeStr = ""
      @canTraverse = false
      @arrayStr = ""

      parse(line.gsub(/\s*[{;].*$/,"").strip)
      # Not support to template variables for C++14
      setKeyForParent(@varName)
    end

    def canTraverse?
      @canTraverse
    end

    def filterByReferenceSet(filter)
      filter.undefinedReferenceSet.getReferenceSetByFullname(getFullname()).any? do |reference|
        !reference.memberName.nil? && (reference.memberName == @varNameNoCardinality)
      end
    end

    def makeStubDef(className)
      makeStubDefWithLocalType(className, nil)
    end

    def makeStubDefWithLocalType(className, localTypeTable)
      prefix = ""
      fullname = getFullname()
      varname = (fullname[0..1] == "::") ? fullname[2..-1] : fullname

      found, canInitializeByZero = findType(@typeStr)
      localType = (!localTypeTable.nil? && localTypeTable.key?(@typeStr)) ? localTypeTable[@typeStr] : nil
      unless found
        # Class variable definitions require qualified names with their class names.
        prefix = ""
        classNamePrefix = "#{className}::"

        # Prevent from throwing RegexpError
        return "" if RegexpMetaCharacter.new.contained?(classNamePrefix)
        unless @typeStr.match(/#{classNamePrefix}[^:]+$/)
          # Convert to a qualified name if not so yet
          prefix = (!className.empty? && localType) ? classNamePrefix : ""
        end
      end

      initialValue = localType ? localType.getInitializer : nil
      # LLVM requires initializer of constant pointers (not pointers to const variables)
      initialValue = "0" if @typeStr.match(/\*\s*const/)
      initializer = initialValue ? " = #{initialValue}" : ""
      "#{prefix}#{@typeStr} #{varname}#{initializer};\n"
    end

    ## Implementation detail (public for testing)
    def parse(line)
      # definitions may be in the form var = (value) so treat the line
      # as a functions if possible before parse it as a variable
      return if line.empty? || line.include?("__")

      # Exclude member functions and type aliases
      wordSet = line.split(nil)
      return if wordSet.nil? || wordSet.empty?
      return if Mockgen::Constants::MEMVAR_FIRST_WORD_REJECTED_MAP.key?(wordSet[0])
      return if Mockgen::Constants::MEMVAR_LAST_WORD_REJECTED_MAP.key?(wordSet[-1])
      newWordSet = wordSet.reject { |word| Mockgen::Constants::MEMVAR_FIRST_WORD_EXCLUDED_MAP.key?(word) }
      return if newWordSet.size < 2

      className = getNonTypedFullname("")
      typeName, varName = TypedVariable.new(newWordSet.join(" ")).parseAsMemberVariable
      return if typeName.empty? || varName.empty?

      @varName = varName
      varStr = Mockgen::Common::ChompAfterDelimiter.new(varName, "[", "")
      @varNameNoCardinality = varStr.str
      @arrayStr = varStr.tailStr
      @arrayStr ||= ""

      @className = Mockgen::Common::ChompAfterDelimiter.new(typeName, "[", "").str.split(/[\*&\s]+/)[-1]
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

  class MemberVariableStatement < VariableStatement
    def initialize(line)
      super
      # Now use only static (class instance) variables
      @canTraverse = false unless line =~ /\bstatic\b/
    end
  end

  class ResolvedArgTypeSetStr
    attr_reader :str

    def initialize(scopedBlock, argTypeSetStr)
      @str = sortArgTypeSetStr(scopedBlock, argTypeSetStr)
    end

    def sortArgTypeSetStr(scopedBlock, argTypeSetStr)
      originalTypeStr = argTypeSetStr.split(",").map do |argTypeStr|
        # Treat [] as * because [] in parameter lists is a syntactic sugar of *
        # and convert it before resolved typenames
        scopedBlock.resolveAlias(argTypeStr.gsub(/\[[^\]]*\]/, "*"))
      end.join(",")
      sortArgTypeStr(originalTypeStr)
    end

    def sortArgTypeStr(argTypeStr)
      # Remove spaces between * and &
      str = argTypeStr.gsub(/([\*&,]+)\s*/, '\1')
      # Do not sort beyond * and &
      # Do not mix different types
      str.split(/([\*&,])/).map { |phrase| phrase.split(nil).sort }.join(" ").gsub(/\s+/," ")
    end
  end

  # Compare argument types between a linker output and a source file
  class FunctionReferenceSet
    def initialize(block, reference, fullname, name, argTypeStr, postFunc, noMatchingTypes)
      @scopedBlock = block
      @fullname = fullname
      @fullname ||= ""
      @name = name
      @name ||= ""
      @argTypeStr = argTypeStr
      @argTypeStr ||= ""
      @postFunc = postFunc
      @postFunc ||= ""

      @refFullname = reference.fullname if reference
      @refFullname ||= ""
      @refName = reference.memberName if reference
      @refName ||= ""
      @refArgTypeStr = reference.argTypeStr if reference  # nullable
      @refPostFunc = reference.postFunc if reference
      @refPostFunc ||= ""
      @noMatchingTypes = noMatchingTypes
    end

    def compare
      result = false

      # Can disregard argument types in .c files
      if @refArgTypeStr && !@noMatchingTypes
        # Resolve typedefs because linkers know the exact type
        # after its aliases are solved but the clang front end does not
        # know the aliases
        argTypeStr = sortArgTypeSetStr(@argTypeStr)
        refArgTypeStr = sortArgTypeSetStr(@refArgTypeStr)

        # const char * and char const * are equivalent
        # Distinguish const and non-const functions
        result = (@refName == @name) &&
                 (argTypeStr == refArgTypeStr) &&
                 (postFunctionPhrase(@postFunc) == postFunctionPhrase(@refPostFunc))
      else
        # "extern C" discards argument types
        refFullname = (@refFullname[0..1] == "::") ? @refFullname[2..-1] : @refFullname
        fullname = (@fullname[0..1] == "::") ? @fullname[2..-1] : @fullname
        result = (refFullname == fullname)
      end

      result
    end

    def sortArgTypeSetStr(argTypeSetStr)
      ResolvedArgTypeSetStr.new(@scopedBlock, argTypeSetStr).str
    end

    def postFunctionPhrase(phrase)
      phrase.split(nil).map do |poststr|
        Mockgen::Constants::MEMFUNC_WORD_COMPARED_MAP.key?(poststr) ? poststr : ""
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
    attr_reader :arity, :argSetStr

    def initialize(line, className)
      # Use line as a keyName to overloading
      super(line)
      @className = className
      @callBase = ""
      @valid, @typedArgSet, @typedArgSetWithoutDefault, @argTypeStr, @typeStr, @argSetStr, @arity, keyForParent = parse(line, className)
      @resolvedArgTypeStr = nil

      # A function name is not enough to support overloading
      setKeyForParent(keyForParent) if @valid
    end

    def getResolvedArgTypeStr(classBlock)
      # cache results
      @resolvedArgTypeStr ||= ResolvedArgTypeSetStr.new(classBlock, @argTypeStr).str
      @resolvedArgTypeStr
    end

    def parse(line, className)
      # Accept Constructor<T>(T& arg)
      elementStr = line
      typeStr = ""

      if line.include?("<")
        elementSet = []
        Mockgen::Common::StringOfAngleBrackets.new(line).parse.each do |element, inBlacket|
          if inBlacket.nil?
            elementSet << element
          else
            typeStr = element if typeStr.empty?
          end
        end
        elementStr = elementSet.join(" ")
      end

      phrase = removeInitializerList(elementStr)
      valid = false
      typedArgSet = ""
      typedArgSetWithoutDefault = ""
      argTypeStr = ""
      callBase = ""
      argSetStr = ""
      arity = 0
      keyForParent = line.dup

      # Prevent from throwing RegexpError
      # Same as ClassBlock.isConstructor?
      if !RegexpMetaCharacter.new.contained?(className) &&
         md = phrase.match(/^\s*((explicit|inline)\s+)*#{className}\s*\(\s*(.*)\s*\)/)
        typedArgSet = md[3]

        argVariableSet = ArgVariableSet.new(phrase)
        typedArgSet = argVariableSet.argSetStr
        typedArgSetWithoutDefault = argVariableSet.argSetWithoutDefault
        argSetStr = argVariableSet.argNameStr
        argTypeStr = argVariableSet.argTypeStr
        arity = argVariableSet.arity
        valid = true

        # Same as ClassBlock.isConstructor?
        linemd = line.match(/^\s*((explicit|inline)\s+)*(.*)/)
        keyForParent = linemd[3] if linemd
      end

      [valid, typedArgSet, typedArgSetWithoutDefault, argTypeStr, typeStr, argSetStr, arity, keyForParent]
    end

    def removeInitializerList(line)
      Mockgen::Common::ChompAfterDelimiter.new(line, ":", "::").str
    end

    def canTraverse?
      @valid
    end

    def filterByReferenceSet(filter)
      fullname = getNonTypedFullname(@className)
      noOverloading = filter.noOverloading
      filter.undefinedReferenceSet.getReferenceSetByFullname(fullname).any? do |reference|
        FunctionReferenceSet.new(self, reference, fullname, @className, @argTypeStr, "", noOverloading).compare
      end
    end

    def setBaseClassName(className)
      # Add a comma to cascade other initializer arguments
      @callBase = (@argSetStr.empty?) ? "" : "#{className}#{@typeStr}(#{@argSetStr}), "
    end

    # Non-default constructive base classes are not supported yet
    def makeStubDef(classBlock, className)
      fullname = getNonTypedFullname(@className)
      # Exclude redundant class name qualifiers
      funcname = (@parent && @parent.isClass?) ? fullname : "#{fullname}::#{@className}"

      initializerStr = classBlock.getConstructorArgStrSet(self)
      "#{funcname}(#{@typedArgSet}) #{initializerStr}{}\n"
    end

    # Empty if call a constructor without arguments
    # Default values included
    def getTypedArgsForBaseClass
      # Add a comma to cascade other initializer arguments
      @typedArgSet.empty? ? "" : ", #{@typedArgSet}"
    end

    # Get args without default values
    def getTypedArgsWithoutValue
      # Add a comma to cascade other initializer arguments
      @typedArgSetWithoutDefault.empty? ? "" : ", #{@typedArgSetWithoutDefault}"
    end

    def getCallForBaseClassInitializer
      @callBase
    end

    # initMember must not be empty
    def makeDef(className, arg, initMember)
      if arg.empty?
        argStr = @typedArgSet.empty? ? "void" : @typedArgSet
      else
        # Attach arguments to base classes to allow default variables
        argStr = @typedArgSet.empty? ? arg : "#{arg},#{@typedArgSet}"
      end

      "    #{className}(#{argStr}) : #{@callBase}#{initMember} {}\n"
    end

    # initMember must not be empty
    def makeDefWithDefaultBaseConstructor(className, arg, initMember)
      argStr = arg.empty? ? "void" : arg
      "    #{className}(#{argStr}) : #{initMember} {}\n"
    end
  end

  class DestructorBlock < BaseBlock
    def initialize(line, className)
      @className = className
      @name = "~#{className}"

      # Create a non-virtual default destructor if arg line is nil
      parsedLine = line
      if (!line.nil? && line.include?("<"))
        parsedLine = Mockgen::Common::StringOfAngleBrackets.new(line).parse.select do |element|
          element[1].nil?
        end.join(" ")
      end
      parsedLine ||= "~#{@name} ()"

      super(parsedLine)
      @valid = (parsedLine =~ /#{@name}\s*\(/) ? true : false
      setKeyForParent(@name) if @valid
    end

    def canTraverse?
      @valid
    end

    def filterByUndefinedReferenceSet(undefinedReferenceSet, classFullname)
      fullname = "#{classFullname}::#{@name}"

      undefinedReferenceSet.getReferenceSetByFullname(fullname).any? do |reference|
        !reference.memberName.nil? && reference.memberName == @name
      end
    end

    def makeStubDef(className)
      fullname = getNonTypedFullname(@className)
      # Exclude redundant class name qualifiers
      funcname = "#{fullname}::#{@name}"
      if (@parent && @parent.isClass?)
        pos = fullname.rindex(":")
        funcname = fullname.insert(pos + 1, "~") if pos
      end

      "#{funcname}(void) {}\n"
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

      # It is redundant but fast to exclude the phrase early
      if Mockgen::Constants::KEYWORD_ATTRIBUTE_WITH_ARGS.any? { |word| phrase.include?(word) }
        Mockgen::Constants::KEYWORD_ATTRIBUTE_WITH_ARGS.each do |keyword|

          # A recursive regular expression that matches balanced parenthesis and
          # substitutes __attribute__((thiscall))) to ).
          newphrase.gsub!(/\b#{keyword}\s*(?<p>\((?:(?>[^()]+)|\g<p>)*\)\s*)\s*/,"")
        end
      end

      if Mockgen::Constants::KEYWORD_ATTRIBUTE_WITHOUT_ARGS.any? { |word| phrase.include?(word) }
        Mockgen::Constants::KEYWORD_ATTRIBUTE_WITHOUT_ARGS.each do |keyword|
          newphrase.gsub!(/\b#{keyword}\b/, "")
        end
      end

      newphrase.strip
    end
  end

  # Class member and non-member (free) function
  class FunctionBlock < BaseBlock
    attr_reader :valid, :funcName, :argSignature
    def initialize(line)
      super
      @valid = false
      @constMemfunc = false  # Free functions never be const
      @staticMemfunc = false
      @returnType == ""
      @returnVoid = false
      @decl = ""
      @argTypeStr = ""
      @argSet = ""
      @funcName = ""
      @switchName = ""
      @typedArgSet = ""
      @typedArgSetWithoutDefault = ""
      @argSignature = ""
      @postFunc = ""
      @defaultNoForwardingToMock = false
      @noMatchingTypes = false

      # Remove trailing ; and {
      body = line
      if md = line.match(/^([^{;]*)[{;]/)
        body = md[1]
      end

      # Remove trailing spaces
      parse(body.rstrip)

      # A function name is not enough to support overloading
      setKeyForParent(line) if @valid
      @argTypeStrSet = [@argTypeStr]
    end

    # Workaround for LLVM 5.0.0
    def setParent(block)
      super

      argTypeStrAlias = nil
      if !block.nil? && block.isClass? && @argTypeStr
        name = block.getNamespace
        argTypeStrAlias = @argTypeStr.gsub(/(\A|[^A-Za-z\d_:])#{name}::/, '\1')
      end

      @argTypeStrSet = [@argTypeStr, argTypeStrAlias].compact.uniq
    end

    def canTraverse?
      @valid
    end

    # No matching arguments between this function declaration and a linker log
    def setNoMatchingTypes
      @noMatchingTypes = true
    end

    def filterByReferenceSet(filter)
      fullname = getNonTypedFullname(@funcName)
      noMatchingTypes = @noMatchingTypes || filter.noOverloading
      filter.undefinedReferenceSet.getReferenceSetByFullname(fullname).any? do |reference|
        @argTypeStrSet.any? do |argTypeStr|
          FunctionReferenceSet.new(self, reference, fullname, @funcName, argTypeStr, @postFunc, noMatchingTypes).compare
        end
      end
    end

    def makeMockDef(className, postfix)
      constStr = (@constMemfunc) ? "_CONST" : ""

      numberOfArgs = ArgumentSplitter.new(@typedArgSetWithoutDefault).argSet.size
      # Treat void-only arg empty
      numberOfArgs = 0 if @argSet.empty?

      str = "    MOCK#{constStr}_METHOD#{numberOfArgs}#{postfix}"
      str += "(#{@funcName},#{@returnType}(#{@typedArgSetWithoutDefault}));\n"
      str
    end

    # outerName is a class name or a namespace
    def makeStubDef(outerName)
      constStr = (@constMemfunc) ? "const " : ""

      numberOfArgs = ArgumentSplitter.new(@typedArgSetWithoutDefault).argSet.size
      # Treat void-only arg empty
      numberOfArgs = 0 if @argSet.empty?

      # clang -cc1 adds classname:: to a return type for a member function
      # if the type is in a class scope.
      prefix = outerName.empty? ? "" : "#{outerName}::"
      str = "#{@returnType} #{prefix}#{@funcName}(#{@typedArgSetWithoutDefault}) #{constStr}{\n"
      returnType = @returnType.tr("&","").strip
      # Support to return a struct instance
      if @returnVoid
        str += "    return;\n"
      else
        found, canInitializeByZero = findType(returnType)
        canInit = @returnType.include?("*") || canInitializeByZero
        init = canInit ? " = 0" : ""
        str += "    #{returnType} result#{init};\n"
        str += "    return result;\n"
      end
      str += "}\n"
      str
    end

    ## Implementation detail (public for testing)
    def parse(line)
      return if isVariable?(line)

      # Split by , and collect variable names ahead ,
      # Note : assuming variable names are not omitted
      argVariableSet = ArgVariableSet.new(line)
      @typedArgSet = argVariableSet.argSetStr
      return false unless @typedArgSet
      @typedArgSetWithoutDefault = argVariableSet.argSetWithoutDefault

      @preFunc = argVariableSet.preFuncSet
      postFunc = argVariableSet.postFuncSet
      pureVirtual = argVariableSet.pureVirtual
      funcName = argVariableSet.funcName
      @argTypeStr = argVariableSet.argTypeStr
      @argSet  = argVariableSet.argNameStr

      return false if Mockgen::Constants::KEYWORD_FUNCTION_LIKE_EXPRESSION.any? { |name| name == funcName }
      return false if funcName.empty?
      # Avoid __ in variable names
      switchName = funcName + ((funcName[-1] == "_") ? "" : "_") + Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX

      # Exclude destructors
      return false if funcName.include?("~")
      # Operators are not supported
      return false if line.match(/\boperator\b/)

      @constMemfunc = isConstMemberFunction?(postFunc)
      @staticMemfunc, @returnType, @returnVoid = extractReturnType(@preFunc)
      @decl = @returnType + " #{funcName}(" + @typedArgSet + ")"
      @decl = @decl+ " " + postFunc unless postFunc.empty?

      @argSignature = extractArgSignature(funcName, @argTypeStr, @constMemfunc)

      @funcName = funcName
      @switchName = switchName
      @postFunc = postFunc

      # va_arg is not supported
      @valid = true unless @argSignature.include?("...")
    end

    def isVariable?(line)
      result = false
      found = false
      previousWord = ""

      # treat var = (value); as a definition of a variable
      Mockgen::Common::StringOfParenthesis.new(line).parse.each do |outer, inner|
        found = true if outer == "=" && previousWord != "operator"
        result = true if found && !inner.nil?
        previousWord = outer.strip
      end

      result
    end

    def isConstMemberFunction?(phrase)
      return false if phrase.empty?
      phrase.split(nil).any? { |word| word == "const" }
    end

    # Remove non-type-related keywords
    def extractReturnType(phrase)
      wordSet = splitByReferenceMarks(phrase)
      staticMemfunc = wordSet.include?("static")

      returnType = wordSet.reject do |word|
        Mockgen::Constants::MEMFUNC_WORD_SKIPPED_MAP.key?(word)
      end.join(" ")

      # Distinguish void and void*
      returnVoid = (returnType == "void")

      [staticMemfunc, returnType, returnVoid]
    end

    # List each type of arguments and omit variables.
    # Distinguish const and non-const member function.
    # Discard return type because it may be covariant and
    # be determined uniquely by function name and argument types
    def extractArgSignature(funcName, argTypeStr, constMemfunc)
      constStr = constMemfunc ? "const" : ""
      "#{funcName}(#{argTypeStr})#{constStr}"
    end

    def splitByReferenceMarks(phrase)
      phrase.gsub(/([\*&])/, ' \1 ').split(nil).reject { |w| w =~ /^\s*$/ }
    end

    def setDefaultNoForwardingToMock(defaultNoForwardingToMock)
      @defaultNoForwardingToMock = defaultNoForwardingToMock
    end

    def makeSwitchToMock(isStatic)
      # C++11 initializer
      prefix = isStatic ? "static " : ""
      value = @defaultNoForwardingToMock ? Mockgen::Constants::MEMFUNC_FORWARDING_ON_VALUE :
                Mockgen::Constants::MEMFUNC_FORWARDING_OFF_VALUE
      init = isStatic ? "" : " {#{value}}"
      str = "    #{prefix}#{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_TYPE} #{@switchName}#{init};\n"
      str
    end

    protected
    def makeForwarderDefImpl(forwardingTarget, overrideStr, definedNameSet)
      # One variable per function and switch all overloaded functions
      str = definedNameSet.key?(@funcName) ? "" : makeSwitchToMock(false)
      definedNameSet[@funcName] = true

      # Leave override keyword
      str += "    #{@decl} #{overrideStr}{ if (#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} && !#{@switchName}) { "

      if (@returnVoid)
        str += "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}->#{@funcName}(#{@argSet}); return; } "
        str += "#{forwardingTarget}#{@funcName}(#{@argSet});"
      else
        str += "return #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}->#{@funcName}(#{@argSet}); } "
        str += "return #{forwardingTarget}#{@funcName}(#{@argSet});"
      end

      str += " }\n"
      str
    end
  end

  # Class member function
  # Templates are not supported yet
  class MemberFunctionBlock < FunctionBlock
    def initialize(line)
      super
      @virtual = (@preFunc && @preFunc.match(/\bvirtual\b/)) ? true : false
      @definition = line.match(/\{\s*$/) ? true : false
      @superMemberSet = []
      @templateParam = nil

      if (line.match(/\A\s*template\b/) || (@funcName.include?("<") && @funcName.include?(">")))
        @valid = false
        setKeyForParent(nil)
      end
    end

    ## Public methods added in the base class
    def override?(block)
      @argSignature == block.argSignature
    end

    # If a member function is virtual in a base class,
    # virtual keyword can be omitted in its derived classes
    def addSuperMember(member)
      @superMemberSet << member unless @superMemberSet.include?(member)
    end

    def virtual?
      @virtual || @superMemberSet.any?(&:virtual?)
    end

    def virtualDeclaration?
      virtual? && !@definition
    end

    # Let definedStaticNameSet a Hash to make switches,
    # false not to make them
    def makeDecoratorDef(className, definedNameSet, definedStaticNameSet)
      mockVarname = @staticMemfunc ? Mockgen::Constants::VARNAME_CLASS_MOCK : Mockgen::Constants::VARNAME_INSTANCE_MOCK
      decl = @staticMemfunc ? "static #{@decl}" : @decl
      overrideStr = getOverrideStr(@decl)

      # One variable per function and switch all overloaded functions
      str = ""
      # This script cannot make switch variables for template classes
      # because this script does not know which types the classes are
      # specialized for.
      if definedStaticNameSet.kind_of?(Hash)
        # Switches to mock methods of the decorator need to be class instances
        # because test cases cannot access decorator instance variables.
        str += definedNameSet.key?(@funcName) ? "" : makeSwitchToMock(true)
        str += "    #{decl} #{overrideStr}{ if (#{mockVarname} && !#{@switchName}) { "
        definedNameSet[@funcName] = @switchName
        definedStaticNameSet[@funcName] = @switchName
      else
        str += "    #{decl} #{overrideStr}{ if (#{mockVarname}) { "
      end

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

    def makeForwarderDef(className, definedNameSet)
      makeForwarderDefImpl("static_cast<#{className}*>(pActual_)->", getOverrideStr(@decl), definedNameSet)
    end

    def getOverrideStr(decl)
      overrideStr = (Mockgen::Constants::CPP11_MODE && virtual? && decl.match(/\boverride\b/).nil?) ? "override " : ""
    end

    ## Implementation detail (public for testing)
  end

  # Templates are not supported yet
  class FreeFunctionBlock < FunctionBlock
    attr_reader :valid

    def initialize(line)
      body = line
      if md = line.match(/^extern\s+(.*)/)
        body = md[1]
      elsif line.include?("inline")
        body = ""
        if md = line.match(/^(static\s+)?inline\s+(static\s+)?([^\{]+){/)
          # Exclude system functions
          body = md[3].strip unless md[3].include?("__")
        end
      end

      super(body)

      # Exclude standard and system internal headers
      @valid = false if @funcName.match(/^[\da-z_]+$/) || @funcName.match(/^_[A-Z]/)
      @valid = false if Mockgen::Constants::FREE_FUNCTION_FILTER_OUT_WORD_SET.any? { |word| line.include?(word) }
      @alreadyDefined = false
    end

    def filterByReferenceSet(filter)
      @alreadyDefined = (!filter.definedReferenceSet.nil? && filter.definedReferenceSet.freeFunctionDefined?(@funcName))
      super
    end

    def isFreeFunction?
      @valid
    end

    def filter(filterSet)
      return @valid if filterSet.empty?

      @valid &= filterSet.any? do |funcFilter|
        # If the funcFilter throws RegexpError, the caller should catch it
        !funcName.match(/#{funcFilter}/).nil?
      end
      @valid
    end

    def makeMockDef(className, postfix)
      @alreadyDefined ? "" : super
    end

    def makeStubDef(outerName)
      @alreadyDefined ? "" : super
    end

    def makeForwarderDef(className, definedNameSet)
      # Add "::" to call a free function and prevent infinite calling
      # to a member function itself
      @alreadyDefined ? "" : makeForwarderDefImpl(getNameFromTopNamespace(getNonTypedFullname(className)), "", definedNameSet)
    end

    def getSwapperDef(varName)
      (!@valid || @alreadyDefined) ? "" : "#define #{@funcName} #{varName}.#{@funcName}\n"
    end
  end

  class FreeFunctionSet < BaseBlock
    # Be public to merge
    attr_reader :funcSet, :undefinedFunctionSet

    def initialize(namespaceBlock)
      @block = namespaceBlock
      @funcSet = []
      @undefinedFunctionSet = []
    end

    def getFullNamespace
      @block.getFullNamespace
    end

    def filterByReferenceSet(filter)
      @funcSet.each do |func|
        @undefinedFunctionSet << func if func.filterByReferenceSet(filter)
      end
    end

    def getStringToClassFile
      @mockClassDef + @forwarderClassDef
    end

    def getStringToDeclFile
      @funcDecl
    end

    def getStringSetToDeclInlineFile
      return ["", ""] if @forwarderVarName.nil? || @funcSet.empty?

      prefix = @forwarderVarName + "."
      forwarder = @funcSet.map do |func|
        funcName = func.funcName
        "#define #{funcName}#{Mockgen::Constants::CLASS_POSTFIX_INLINE} #{prefix}#{funcName}\n"
      end.join("")

      renamer = @funcSet.map do |func|
        funcName = func.funcName
        "#define #{funcName}#{Mockgen::Constants::CLASS_POSTFIX_INLINE} #{funcName}\n"
      end.join("")

      [forwarder, renamer]
    end

    def getStringToSwapperFile
      @funcSwapDef
    end

    def getStringOfStub
      @mockClassFunc
    end

    def getStringOfVariableDefinition
      @forwarderVarDef
    end

    def needStub?
      !@undefinedFunctionSet.empty?
    end

    def merge(otherSet)
      @funcSet.concat(otherSet.funcSet)
      @undefinedFunctionSet.concat(otherSet.undefinedFunctionSet)
    end

    def filter(filterSet)
      @funcSet.reject! { |f| !f.filter(filterSet) }
      @undefinedFunctionSet.reject! { |f| !f.filter(filterSet) }
    end

    # Generate class definition texts
    def makeClassSet
      uniqFunctions
      prefix = Mockgen::Constants::CLASS_FREE_FUNCTION_SET
      nameSpaceStr = @block.getFullNamespace
      nameSpaceStr = (!nameSpaceStr.empty? && nameSpaceStr[0] != ":") ? "::#{nameSpaceStr}" : nameSpaceStr
      className = surpressUnderscores(prefix + nameSpaceStr.gsub("::", "_"))
      mockName = surpressUnderscores(className + Mockgen::Constants::CLASS_POSTFIX_MOCK)
      forwarderName = surpressUnderscores(className + Mockgen::Constants::CLASS_POSTFIX_FORWARDER)

      @mockClassDef, @mockClassFunc = formatMockClass(mockName, forwarderName)
      @forwarderClassDef, @funcDecl, @funcSwapDef, @forwarderVarName, @forwarderVarDef = formatForwarderClass(forwarderName, mockName)
    end

    def makeStubSet
      uniqFunctions
      @mockClassDef = ""
      @mockClassFunc = formatStub()
      @forwarderClassDef = ""
      @funcDecl = ""
      @funcSwapDef = ""
      @forwarderVarDef = ""
    end

    def add(func)
      @funcSet << func if func.valid
    end

    # Unify same functions that occur in different include directives
    def uniqFunctions
      @funcSet.uniq! { |func| func.argSignature }
      @undefinedFunctionSet.uniq! { |func| func.argSignature }
    end

    def formatMockClass(mockClassName, forwarderName)
      str = ""
      src = ""

      unless @funcSet.empty?
        str += "class #{forwarderName};\n"
        str += "class #{mockClassName} {\n"
        str += "public:\n"
        str += "    #{mockClassName}(#{forwarderName}* pForwarder);\n"
        str += "    #{mockClassName}(#{forwarderName}& forwarder);\n"
        str += "    ~#{mockClassName}(void);\n"

        str += @funcSet.map do |func|
          func.valid ? func.makeMockDef(mockClassName, "") : ""
        end.join("")

        str += "private:\n"
        str += "    #{forwarderName}* pForwarder_;\n"
        str += "};\n\n"

        # Need to merge with the classBlock's method
        src += "#{mockClassName}::#{mockClassName}(#{forwarderName}* pForwarder) : "
        src += "pForwarder_(pForwarder) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"
        src += "#{mockClassName}::#{mockClassName}(#{forwarderName}& forwarder) : "
        src += "pForwarder_(&forwarder) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"
        src += "#{mockClassName}::~#{mockClassName}(void) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = 0; }\n\n"
      end

      src += formatStub()
      return str, src
    end

    def formatStub
      str = ""
      unless @undefinedFunctionSet.empty?
        nameSpaceSet = @block.getNonTypedFullname(@block.getNamespace).split("::").reject { |name| name.empty? }
        str += nameSpaceSet.map { |name| "namespace #{name} {\n" }.join("") unless nameSpaceSet.empty?
        str += @undefinedFunctionSet.map do |func|
          func.valid ? (func.makeStubDef("") + "\n") : ""
        end.join("")
        str += "}\n" * nameSpaceSet.size unless nameSpaceSet.empty?
      end

      str
    end

    def formatForwarderClass(forwarderName, mockClassName)
      return "", "", "", "", "" if @funcSet.empty?

      str =  "class #{mockClassName};\n"
      str += "class #{forwarderName} {\n"
      str += "public:\n"
      str += "    #{forwarderName}(void) : "
      str += "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"

      definedNameSet = {}
      str += @funcSet.map do |func|
        func.makeForwarderDef("", definedNameSet)
      end.join("")

      str += "    #{mockClassName}* #{Mockgen::Constants::VARNAME_INSTANCE_MOCK};\n"
      str += "};\n\n"

      # Class name should begin with an upper case
      varName = forwarderName[0].downcase + forwarderName[1..-1]
      varline = "#{forwarderName} #{varName};\n"
      varDecl = "extern " + varline
      varSwap = @funcSet.map { |func| func.getSwapperDef(varName) }.join("")

      src = varline
      return str, varDecl, varSwap, varName, src
    end

    def surpressUnderscores(str)
      str.squeeze("_")
    end
  end

  class FreeFunctionSwapper
    attr_reader :beginPhrase, :endPhrase, :body

    def initialize(guardName, functionSetArray)
      nameSet = [guardName, Mockgen::Constants::MARK_FOR_GENERATED_CPP].compact
      @beginPhrase = "#if " + nameSet.map { |name| "!defined(#{name})" }.join(" && ") + "\n"
      @endPhrase = "#endif\n"

      strSet = functionSetArray.map do |functionSet|
        functionSet.getStringSetToDeclInlineFile
      end.transpose

      @body = (strSet.size < 2) ? ""  : (strSet[0].join("") + "#else\n" + strSet[1].join(""))
    end
  end

  # Parameter set to create ClassBlock instances
  class ClassBlockParameterSet
    attr_reader :filterToMock, :filterToAll
    def initialize(filterToMock, filterToAll)
      @filterToMock = filterToMock
      @filterToAll = filterToAll
    end
  end

  # Class and struct
  class ClassBlock < BaseBlock
    attr_reader :mockName, :decoratorName, :forwarderName
    # public for its derived classes
    attr_reader :subConstructorSet

    def initialize(line, parameterSet = nil)
      super(line)
      @filterToMock = parameterSet ? parameterSet.filterToMock : nil
      @filterToAll = parameterSet ? parameterSet.filterToAll : nil
      @templateParam = nil # Template <T...>
      @name = ""           # Class name
      @mockName = ""
      @decoratorName = ""
      @forwarderName = ""
      @uniqueName = @name     # Unique name for inner class name
      @uniqueFilename = @name # Unique name for inner class name
      @typename = "class"  # class or struct
      @pub = false         # Now parsing public members
      @private = true      # Now parsing private members
      @destructor = nil    # None or one instance
      @alreadyDefined = false
      @filteredOut = false
      @noParsingDetail = false
      @defaultNoForwardingToMock = false

      # Remove trailing ; and {
      body = line
      if md = line.match(/^([^{;]*)[{;]/)
        body = md[1]
      end

      # Determine whether this block can be handled after parsed
      @valid = parseClassName(body)
      if @valid
        setKeyForParent(@name)
        @filteredOut = @filterToMock.excludeSimpleName?(@name) if @filterToMock
        @noParsingDetail = @filterToAll.excludeSimpleName?(@name) if @filterToAll
      end

      # One or more constructors
      @destructor = nil
      @constructorSet = []
      @subConstructorSet = []
      @publicMemberFunctionSet = []
      @protectedMemberFunctionSet = []
      @memberVariableSet = []
      # Including protected and private members
      @allConstructorSet = []
      @allMemberFunctionSet = []
      @allMemberVariableSet = []

      # Candidates to make stubs
      @undefinedDestructor = nil
      @undefinedConstructorSet = []
      @undefinedFunctionSet = []
      @undefinedVariableSet = []

      # Base classes
      @baseClassNameSet = parseInheritance(body)
      @baseClassBlockSet = []

      # Local typenames
      @localTypeTable = {}
    end

    # Name an inner class
    def connect(child)
      super
      name = child.getTypename
      if name
        @localTypeTable[name] = child
        # clang -cc1 adds classname:: to types of static variables.
        # It may cause errors if an outer class and namespace have the same name of this class.
        @localTypeTable["#{@name}::#{name}"] = child
      end
      child.setUniqueName if child.isClass?
    end

    def setUniqueName
      fullname = getNonTypedFullname(@name)
      # Remove head ::
      name = (fullname[0..1] == "::") ? fullname[2..-1] : fullname
      @uniqueName = name.gsub("::", "_#{Mockgen::Constants::CLASS_SPLITTER_NAME}_").squeeze("_")
      # Collapse consecutive _s in getFilenamePostfix
      @uniqueFilename = name.gsub("::", "_")

      # Update generated class names
      setClassNameSet
      @uniqueName
    end

    def getFilenamePostfix
      ("_" + @uniqueFilename).squeeze("_")
    end

    # Skip parsing child members
    def skippingParse
      !@pub
    end

    def canTraverse?
      @valid
    end

    def canMock?
      # exclude PODs
      @valid && !@filteredOut && (!@allConstructorSet.empty? || !@allMemberFunctionSet.empty? || needStub?)
    end

    def isClass?
      @valid
    end

    # Class and struct names can be treated as namespaces
    def getNamespace
      @name
    end

    def getTypename
      @name
    end

    # Parse a class member described in the arg line and return its block
    def parseChildren(line)
      return nil if parseAccess(line)
      parsed, block = parseQuick(line)
      return block if parsed
      block = nil

      # Disregard private members (need to change in considering the NVI idiom)
      newBlock = nil
      destructorBlock = DestructorBlock.new(line, @name)
      if (destructorBlock.canTraverse?)
        newBlock = destructorBlock
        @destructor = destructorBlock
      elsif isConstructor?(line)
        newBlock = ConstructorBlock.new(line, @name)
        @constructorSet << newBlock if @pub && newBlock.canTraverse?
        @subConstructorSet << newBlock if !@private && newBlock.canTraverse?
        @allConstructorSet << newBlock if newBlock.canTraverse?
      elsif isPointerToFunction?(line)
      # Not supported yet
      else
        newBlock = MemberFunctionBlock.new(line)
        if newBlock.canTraverse?
          @allMemberFunctionSet << newBlock
          @publicMemberFunctionSet << newBlock if @pub
          @protectedMemberFunctionSet << newBlock if !@pub && !@private
        elsif
          newBlock = addMemberVariable(line)
        end
      end

      block = newBlock if !newBlock.nil? && newBlock.canTraverse?
      block
    end

    def addMemberVariable(line)
      block = MemberVariableStatement.new(line)
      if block.canTraverse?
        if @pub
          @memberVariableSet << block
        end
        @allMemberVariableSet << block
      end
      block
    end

    def parseAccess(line)
      md = line.match(/^(p\S+)\s*:/)
      if (md)
        case(md[1])
        when "public"
          @pub = true
          @private = false
          return true
        when "protected"
          @pub = false
          @private = false
          return true
        when "private"
          @pub = false
          @private = true
          return true
        end
      end

      false
    end

    # Quick parse when most of members are discarded
    def parseQuick(line)
      return false, nil if !@noParsingDetail
      return true, nil if line.include?("{")

      # append a space to the line to end with the space if the line
      # has no trailing whiltespaces.
      md = line.match(/^\s*static\s+[\dA-Za-z_]+\s+[\dA-Za-z_]+\s*(.*)/)
      matched = (!md.nil? && (md[1].empty? || md[1][0] != "("))
      block = matched ? addMemberVariable(line) : nil
      return true, block
    end

    # Accept Constructor<T>(T& arg)
    def isConstructor?(line)
      str = line
      if line.include?("<")
        str = Mockgen::Common::StringOfAngleBrackets.new(line).parse.select do |element|
          element[1].nil?
        end.join(" ")
        # Already checked if @name contains meta characters
      end

      # Same as ConstructorBlock.parse
      str.match(/^\s*((explicit|inline)\s+)*#{@name}\s*\(.*\)/) ? true : false
    end

    # Treat type(*func)(args...) as a variable having a function pointer
    def isPointerToFunction?(line)
      # Recursive regular expression
      pattern = Regexp.new('((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)')
      body = Mockgen::Common::ChompAfterDelimiter.new(line, ";", "").str
      newline = Mockgen::Common::ChompAfterDelimiter.new(body, "{", "").str
      phraseSet = newline.gsub("(", " (").gsub(")", ") ").gsub(/\s+/, ' ').scan(pattern)

      return false unless phraseSet
      elementSet = phraseSet.map { |p| p[1] }.compact
      return false if elementSet.size < 2
      !(phraseSet[-1])[1].nil? && elementSet[-2].include?("*")
    end

    def getStringToClassFile
      @mockClassDef + @decoratorClassDef + @classDefInHpp
    end

    def getStringToSourceFile
      @staticMockDef + @classDefInCpp
    end

    def filterByReferenceSet(filter)
      constructorBlockSet = []
      variableBlockSet = []

      fullname = getFullname()
      @alreadyDefined = true if !filter.definedReferenceSet.nil? &&
                                filter.definedReferenceSet.classDefined?(
                                  fullname, filter.definedReferenceSet.relativeNamespaceOnly)
      # avoid matching if already filtered out
      @filteredOut |= @filterToMock.excludeFullname?(fullname) if @filterToMock && !@filteredOut
      return unless canFilterByReferenceSet(filter)

      # Create a default destructor if it does not exist
      @destructor ||= DestructorBlock.new(nil, @uniqueName)
      @undefinedDestructor = @destructor if @destructor.filterByUndefinedReferenceSet(filter.undefinedReferenceSet, fullname)

      @allConstructorSet.each do |block|
        constructorBlockSet << block if block.filterByReferenceSet(filter)
      end

      # Assume instance variables do not appear in referenceSet
      @allMemberVariableSet.each do |block|
        variableBlockSet << block if block.filterByReferenceSet(filter)
      end

      @undefinedConstructorSet = constructorBlockSet.uniq
      @undefinedVariableSet = variableBlockSet.uniq
    end

    def filterByReferenceSetWithSuper(filter)
      return unless canFilterByReferenceSet(filter)
      functionBlockSet = []
      fullname = getFullname()

      @allMemberFunctionSet.each do |block|
        functionBlockSet << block if block.filterByReferenceSet(filter)
      end

      # collect all undefined virtual functions to create vtables
      # if none of the virtual functions is defined (implemented).
      nonVirtualOnly = functionBlockSet.all? { |block| !block.virtual? }
      if filter.fillVtable && nonVirtualOnly && filter.undefinedReferenceSet.needVtable?(fullname)
        @allMemberFunctionSet.each do |block|
          functionBlockSet << block if block.virtualDeclaration?
        end
      end

      @undefinedFunctionSet = functionBlockSet.uniq
    end

    def canFilterByReferenceSet(filter)
      !filter.undefinedReferenceSet.nil? && filter.undefinedReferenceSet.valid
    end

    ## Public methods added on the base class
    # Get class name with its namespaces
    def needStub?
      @valid && (!@undefinedDestructor.nil? || !@undefinedConstructorSet.empty? ||
                 !@undefinedFunctionSet.empty? || !@undefinedVariableSet.empty?)
    end

    def getFullname
      fullname = getNonTypedFullname(@name)
      @templateParam ? "template <#{@templateParam.generic}> #{fullname}" : fullname
    end

    # Set base classes
    # fullNameToBlock : { fullname => block }
    def setBaseClass(fullNameToBlock)
      @baseClassNameSet.each do |name|
        block = fullNameToBlock[name]
        @baseClassBlockSet << block if block
      end
    end

    # Return arity of this class to make constructor
    def getConstructorArity
      return 0 if @constructorSet.empty?

      set = @constructorSet.map do |block|
        str = block.getTypedArgsForBaseClass
        (str.empty? || str == "void") ? 0 : str.count(",")
      end
      set.min
    end

    def getConstructorArgStrSet(ctorBlock)
      strSet = @baseClassBlockSet.map do |baseBlock|
        makeConstructorArgStr(ctorBlock, baseBlock)
      end.compact

      # Empty parameter lists in base classes imply they do not
      # require explicit base class initialization.
      strSet.empty? ? "": (": " + strSet.join(", ") + " ")
    end

    # Search for parameter lists of constructors in a base class of
    # this class to find a constructor that has an exact or most
    # likely argument list
    def makeConstructorArgStr(ctorBlock, baseBlock)
      argTypeStr = ctorBlock.getResolvedArgTypeStr(self)

      # List constructors of which parameter lists the parameter list
      # of the constructor in this class covers.
      blockSet = baseBlock.subConstructorSet.map do |baseCtor|
        baseArgTypeStr = baseCtor.getResolvedArgTypeStr(baseBlock)
        containArgList?(argTypeStr, baseArgTypeStr) ? [baseArgTypeStr, baseCtor] : nil
      end.compact.sort { |l, r| l[0] <=> r[0] }

      # The last and longest parameter list is most likely or exact
      # for the constructor of this class.
      return nil if blockSet.empty?

      # No explicit initializer needed for constructors without arguments
      baseArgTypeStr = (blockSet[-1])[0]
      return nil if baseArgTypeStr.empty?
      argStr = ctorBlock.argSetStr

      # Extract leading arguments
      arity = (blockSet[-1])[1].arity
      str = argStr.split(",")[0..(arity-1)].join(",")

      # Return a base class initializer
      str.empty? ? nil : (baseBlock.getFullname() + "(" + str + ")")
    end

    # Return true if the inner string begins from the outer string
    # and return false if not
    def containArgList?(outer, inner)
      return true if inner.empty? || (outer == inner)
      pos = outer.index(inner)
      return false if pos.nil? || (pos != 0)

      # False when sub matching between commas fails
      innerSet = ArgumentSplitter.new(inner).argSet
      outerSet = ArgumentSplitter.new(outer).argSet
      return false if innerSet.empty? || outerSet.empty? || (innerSet.size > outerSet.size)
      (0..([innerSet.size, outerSet.size].min-1)).all? { |i| innerSet[i] == outerSet[i] }
    end

    # Generate class definition texts
    def makeClassSet
      if @noParsingDetail
        makeStubSet
      else
        @mockClassDef = ""
        @classDefInHpp = ""
        @classDefInCpp = ""
        @decoratorClassDef = ""
        @decoratorClassDef = ""
        @staticMockDef = ""

        unless @alreadyDefined
          fullname = getNonTypedFullname(@name)
          @mockClassDef, @classDefInHpp, @classDefInCpp = formatMockClass(@mockName, @decoratorName, @forwarderName, fullname)
          @decoratorClassDef, classDefInCpp = formatDecoratorClass(@decoratorName, @mockName, fullname)
          @classDefInCpp += classDefInCpp
          @decoratorClassDef += formatForwarderClass(@forwarderName, @mockName, fullname)
          @staticMockDef = getVariableDefinitionExample(@templateParam, @mockName, @decoratorName, @forwarderName, fullname)
        end
      end
    end

    def makeStubSet
      @mockClassDef = ""
      @classDefInHpp = ""
      @classDefInCpp = @alreadyDefined ? "" : formatStub()
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
      # Discard words between class/struct keyword and a class name
      wordSet = Mockgen::Common::ChompAfterDelimiter.new(line, ":", "::").str.strip.split(nil)
      name = wordSet.empty? ? "" : wordSet[-1]

      classLine = line
      if line.include?("template")
        param = TemplateParameter.new(Mockgen::Common::ChompAfterDelimiter.new(line, ":", "::").str)
        # Ignore specialized classes
        return false if param.specialized || param.type.empty?
        classLine = param.type
        @templateParam = param
      end

      if md = classLine.match(/^#{typenameStr}\s+(\S+)/)
        # Prevent from throwing RegexpError later
        @name = name unless RegexpMetaCharacter.new.contained?(name)
      else
        return false
      end
      # uniqueName is an alias of name until changed
      @uniqueName = @name
      @uniqueFilename = @name
      return false if Mockgen::Constants::CLASS_NAME_EXCLUDED_MAP.key?(@name)
      # Set generated class names
      setClassNameSet

      @typename = typenameStr
      @pub = (typenameStr == "struct")
      @private = (typenameStr == "class")
      true
    end

    def setClassNameSet
      @mockName = @uniqueName + Mockgen::Constants::CLASS_POSTFIX_MOCK
      @decoratorName = @uniqueName + Mockgen::Constants::CLASS_POSTFIX_DECORATOR
      @forwarderName = @uniqueName + Mockgen::Constants::CLASS_POSTFIX_FORWARDER
    end

    def parseInheritance(line)
      nameSet = []
      return nameSet unless line.include?(":") && (md = line.match(/:\s*(.*)$/))

      md[1].split(/,\s*/).each do |sentence|
        wordSet = sentence.split(nil)
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

    # Actually this should be called for leaf classes in each class
    # hierarchy to reduce execution time
    def markMemberFunctionSetVirtual
      markMemberFunctionSetVirtualSub([])
    end

    def markMemberFunctionSetVirtualSub(derivedSet)
      # Connect overriding and overridden functions to check whether
      # they are virtual
      @allMemberFunctionSet.each do |member|
        next unless member.canTraverse?

        # Need to improve to match faster
        derivedMemberSet = derivedSet.select { |f| f.override?(member) }
        derivedMemberSet.each do |derivedMember|
          derivedMember.addSuperMember(member)
        end

        derivedSet << member if derivedMemberSet.empty?
      end

      @baseClassBlockSet.each do |block|
        # Duplicate not to mix super classes
        block.markMemberFunctionSetVirtualSub(derivedSet.dup)
      end
    end

    def setDefaultNoForwardingToMock(defaultNoForwardingToMock)
      @defaultNoForwardingToMock = defaultNoForwardingToMock
      @allMemberFunctionSet.each { |block| block.setDefaultNoForwardingToMock(defaultNoForwardingToMock) }
    end

    # A function needs to be public to be forwarded from outside of its class
    def canForwardToFunction(func)
      return false unless func.canTraverse?
      @publicMemberFunctionSet.include?(func)
    end

    # A function needs to be protected to be forwarded from its derived class
    def canDecorateFunction(func)
      canForwardToFunction(func) || @protectedMemberFunctionSet.include?(func)
    end

    # Can mock private and protected functions if virtual
    def canMockFunction(func)
      canDecorateFunction(func) || func.virtual?
    end

    def formatMockClass(className, decoratorName, forwarderName, baseName)
      postfix = @templateParam.nil? ? "" : "_T"
      decoratorType = getTypedTemplate(@templateParam, decoratorName)
      forwarderType = getTypedTemplate(@templateParam, forwarderName)

      str =  getTemplateDeclaration(@templateParam, decoratorName) + ";\n"
      str += getTemplateDeclaration(@templateParam, forwarderName) + ";\n"
      str += getClassDefinition(@templateParam, className, baseName) + " {\n"
      str += "public:\n"

      # Template classes need their namespace to call their constructor
      @constructorSet.each { |constructor| constructor.setBaseClassName(baseName) }

      typedArgsArray = @constructorSet.empty? ? [""] : @constructorSet.map(&:getTypedArgsForBaseClass)
      typedArgsArrayWithoutDefault = @constructorSet.empty? ? [""] : @constructorSet.map(&:getTypedArgsWithoutValue)
      callBaseArray = @constructorSet.empty? ? [""] : @constructorSet.map(&:getCallForBaseClassInitializer)
      ctorSet = typedArgsArrayWithoutDefault.zip(callBaseArray)

      typedArgsArray.each do |argSet|
        str += "    #{className}(#{decoratorType}* pDecorator#{argSet});\n"
        str += "    #{className}(#{decoratorType}& decorator#{argSet});\n"
        str += "    #{className}(#{forwarderType}* pForwarder#{argSet});\n"
        str += "    #{className}(#{forwarderType}& forwarder#{argSet});\n"
      end

      str += "    ~#{className}(void);\n"
      str += collectMockDef([], postfix)
      str += "private:\n"
      str += "    #{decoratorType}* pDecorator_;\n"
      str += "    #{forwarderType}* pForwarder_;\n"
      str += "};\n\n"

      typeVar = @templateParam ? "<#{@templateParam.varSet}>" : ""
      typedClassName = getTemplateHeader(@templateParam) + className + typeVar

      # Solve circular dependency between a mock and its decorator/forwarder
      src = ""
      ctorSet.each do |argSet, callBase|
        src += "#{typedClassName}::#{className}(#{decoratorType}* pDecorator#{argSet}) : "
        src += "#{callBase}pDecorator_(pDecorator), pForwarder_(0) "
        src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"

        src += "#{typedClassName}::#{className}(#{decoratorType}& decorator#{argSet}) : "
        src += "#{callBase}pDecorator_(&decorator), pForwarder_(0) "
        src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"

        src += "#{typedClassName}::#{className}(#{forwarderType}* pForwarder#{argSet}) : "
        src += "#{callBase}pDecorator_(0), pForwarder_(pForwarder) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"

        src += "#{typedClassName}::#{className}(#{forwarderType}& forwarder#{argSet}) : "
        src += "#{callBase}pDecorator_(0), pForwarder_(&forwarder) "
        src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = this; }\n"
      end

      src += "#{typedClassName}::~#{className}(void) {\n"
      src += "    if (pDecorator_ && (pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} == this)) "
      src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = 0; }\n"
      src += "    if (pDecorator_ && (pDecorator_->#{Mockgen::Constants::VARNAME_CLASS_MOCK} == this)) "
      src += "{ pDecorator_->#{Mockgen::Constants::VARNAME_CLASS_MOCK} = 0; }\n"
      src += "    if (pForwarder_ && (pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} == this)) "
      src += "{ pForwarder_->#{Mockgen::Constants::VARNAME_INSTANCE_MOCK} = 0; }\n}\n\n"

      # Define member functions in non-template classes exact once
      srcInHpp = @templateParam ? src : ""
      srcInCpp = (@templateParam ? "" : src) + formatStub()
      return str, srcInHpp, srcInCpp
    end

    def formatStub
      src = ""
      name = getFullname()

      begin
        @undefinedConstructorSet.each do |member|
          src += member.makeStubDef(self, name) if member.canTraverse?
        end

        if !@undefinedDestructor.nil? && @undefinedDestructor.canTraverse?
          src += @undefinedDestructor.makeStubDef(name)
        end

        @undefinedFunctionSet.each do |member|
          src += member.makeStubDef(name) if member.canTraverse?
        end

        @undefinedVariableSet.each do |member|
          src += member.makeStubDefWithLocalType(name, @localTypeTable) if member.canTraverse?
        end
      rescue => e
        warn e.message
        raise e unless MockgenFeatures.ignoreAllExceptions
      end

      src
    end

    def formatDecoratorClass(decoratorName, mockClassName, baseName)
      # class can inherit struct
      str = getClassDefinition(@templateParam, decoratorName, baseName) + " {\n"
      str += "public:\n"

      initMember = "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0)"
      mockType = getTypedTemplate(@templateParam, mockClassName)

      definedStaticNameSet = @templateParam ? false : {}
      str += formatConstrutorSet(baseName, decoratorName, "", initMember)
      str += "    virtual ~#{decoratorName}(void) {}\n"
      str += collectDecoratorDef([], {}, definedStaticNameSet)
      str += "    #{mockType}* #{Mockgen::Constants::VARNAME_INSTANCE_MOCK};\n"
      str += "    static #{mockType}* #{Mockgen::Constants::VARNAME_CLASS_MOCK};\n"
      str += "};\n\n"

      varStr = ""
      value = @defaultNoForwardingToMock ? Mockgen::Constants::MEMFUNC_FORWARDING_ON_VALUE :
                Mockgen::Constants::MEMFUNC_FORWARDING_OFF_VALUE
      if definedStaticNameSet.kind_of?(Hash) && !definedStaticNameSet.empty?
        varStr += "namespace #{Mockgen::Constants::GENERATED_SYMBOL_NAMESPACE} {\n"
        definedStaticNameSet.each do |funcName, switchName|
          varStr += "    #{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_TYPE} "
          varStr += "#{decoratorName}::#{switchName} = #{value};\n"
        end
        varStr += "}\n"
      end

      return str, varStr
    end

    def formatForwarderClass(forwarderName, mockClassName, baseName)
      # Inherit a base class to refer class scope enums of the class
      # class can inherit struct
      str = getClassDefinition(@templateParam, forwarderName, baseName) + " {\n"
      str += "public:\n"
      baseType = getTypedTemplate(@templateParam, baseName)

      [["#{baseType}* pActual", "pActual_(pActual)"],
       ["#{baseType}& actual",  "pActual_(&actual)"]].each do |arg, initMember|
        initMember += ", #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0)"
        str += formatConstrutorSet(baseName, forwarderName, arg, initMember)
      end

      mockType = getTypedTemplate(@templateParam, mockClassName)
      str += "    virtual ~#{forwarderName}(void) {}\n"
      str += collectForwarderDef([], {})
      str += "    #{baseType}* pActual_;\n"
      str += "    #{mockType}* #{Mockgen::Constants::VARNAME_INSTANCE_MOCK};\n"
      str += "};\n\n"
      str
    end

    def formatConstrutorSet(baseName, className, arg, initMember)
      str = ""
      if @constructorSet.empty?
        str += ConstructorBlock.new("", baseName).makeDefWithDefaultBaseConstructor(className, arg, initMember)
      else
        @constructorSet.each do |constructor|
          str += constructor.makeDef(className, arg, initMember)
        end
      end

      str
    end

    def collectMockDef(derivedSet, postfix)
      collectFunctionDef(derivedSet, :collectMockDef, :makeMockDef, :canMockFunction, postfix, nil)
    end

    # Let definedStaticNameSet a Hash to make switches,
    # false not to make them
    def collectDecoratorDef(derivedSet, definedNameSet, definedStaticNameSet)
      collectFunctionDef(derivedSet, :collectDecoratorDef, :makeDecoratorDef, :canDecorateFunction,
                         definedNameSet, definedStaticNameSet)
    end

    def collectForwarderDef(derivedSet, definedNameSet)
      collectFunctionDef(derivedSet, :collectForwarderDef, :makeForwarderDef, :canForwardToFunction,
                         definedNameSet, nil)
    end

    # If find an overriding function (which has same name, args and const
    # modifier as of its base class), call the most subclass definition.
    def collectFunctionDef(derivedSet, methodClass, methodFunc, filterFunc, extraArg1, extraArg2)
      str = ""
      name = getTypedTemplate(@templateParam, getNonTypedFullname(@name))

      @allMemberFunctionSet.each do |member|
        next unless send(filterFunc, member)
        next if derivedSet.any? { |f| f.override?(member) }

        # Add member
        argSet = [methodFunc, name]
        argSet << extraArg1 unless extraArg1.nil?
        argSet << extraArg2 unless extraArg2.nil?
        str += member.send(*argSet)
        derivedSet << member
      end

      # Search base classes for functions
      @baseClassBlockSet.each do |block|
        # Mix functions of sibling base classes assuming
        # D is derived from B1, B2, B1:f and F2:f are defined
        # but D:f is not defined.
        # Though calling D:f is ambiguous in this case,
        # it is practical to create exactly one Mock(D)::f.
        argSet = [methodClass, derivedSet]
        argSet << extraArg1 unless extraArg1.nil?
        argSet << extraArg2 unless extraArg2.nil?
        str += block.send(*argSet)
      end

      str
    end

    def getClassDefinition(templateParam, className, baseName)
      postbase = (templateParam && templateParam.generic) ? "<#{templateParam.varSet}>" : ""
      getTemplateDeclaration(templateParam, className) + " : public #{baseName}#{postbase}"
    end

    def getTemplateDeclaration(templateParam, className)
      header = getTemplateHeader(templateParam)
      "#{header}class #{className}"
    end

    def getTemplateHeader(templateParam)
      (templateParam && templateParam.generic) ? "template <#{templateParam.generic}> " : ""
    end

    def getTypedTemplate(templateParam, className)
      className + (templateParam ? "<#{templateParam.varSet}>" : "")
    end

    # Testers must define typed mocks that they need
    # because the mocks do not know their concrete types needed.
    def getVariableDefinitionExample(templateParam, mockName, decoratorName, forwarderName, fullname)
      templateParam ?
        getDefinitionExample(templateParam, mockName, decoratorName, forwarderName, fullname) :
        "#{mockName}* #{decoratorName}::#{Mockgen::Constants::VARNAME_CLASS_MOCK};\n"
    end

    def getDefinitionExample(templateParam, mockName, decoratorName, forwarderName, fullname)
        mockType = getTypedTemplate(templateParam, mockName)
        decoratorType = getTypedTemplate(templateParam, decoratorName)
        forwarderType = getTypedTemplate(templateParam, forwarderName)
        name = Mockgen::Constants::GENERATED_SYMBOL_NAMESPACE

        str =  "/* Tester must define these types and variables\n"

        typeParamSet = []
        serial = 1
        templateParam.typenameSet.each do |typenameStr|
          typeStr = ""
          if ["typename", "class"].include?(typenameStr)
            typeStr = "DataType#{serial}"
            str += "using #{typeStr} = int; (or other appropriate type)\n"
          else
            typeStr = serial.to_s
            str += "++ set an appropriate value to type parameter #{serial} ++\n"
          end
          typeParamSet << typeStr
          serial += 1
        end

        typeParamSet = "<" + typeParamSet.join(",") + ">"
        str += " ** Class (static) variable template **\n"
        str += "template <#{templateParam.generic}> #{mockType}* #{decoratorType}::#{Mockgen::Constants::VARNAME_CLASS_MOCK};\n"

        str += " ** Specialized class template to test **\n"
        str += "template class #{fullname}#{typeParamSet};\n"

        str += " ** Generated classes**\n"
        str += "namespace #{name} {\n"
        str += " ** Specialized class variable in the decorator class template **\n"
        str += "    template #{mockName}#{typeParamSet}* #{decoratorName}#{typeParamSet}::pClassMock_;\n"
        str += "}\n"
        str += " ** Type aliases in a test fixture **\n"
        str += "    using Tested = #{fullname}#{typeParamSet};\n"
        str += "    using Decorator = #{name}::#{decoratorName}#{typeParamSet};\n"
        str += "    using Forwarder = #{name}::#{forwarderName}#{typeParamSet};\n"
        str += "    using Mock = #{name}::#{mockName}#{typeParamSet};\n"
        str += "*/\n\n"
        str
    end
  end

  # enum and enum class
  class EnumBlock < BaseBlock
    def initialize(line)
      super
      @name = nil
      @zeroInitializer = nil
      @initializer = nil
      @prefix = ""

      if md = line.tr("{;", "").match(/^enum\s+(class\s+)?(\S+)/)
        @name = md[2]
        @prefix = md[1].nil? ? "" : "#{@name}::"
        # zero may not be a member of this enum type
        @zeroInitializer = "static_cast<#{@name}>(0)"
      end

      setKeyForParent(@name)
    end

    def getTypename
      @name
    end

    def getInitializer
      @initializer ? @initializer : @zeroInitializer
    end

    def parseChildren(line)
      if md = line.strip.match(/^([^\s=,]+)/)
        @initializer = @prefix + md[1] unless @initializer
      end

      # return no blocks
      nil
    end
  end

  class UnionBlock < BaseBlock
    def initialize(line)
      super
      @name = nil
      if md = line.tr("{;", "").match(/^union\s+(\S+)/)
        @name = md[1]
      end

      setKeyForParent(@name)
    end

    def getTypename
      @name
    end
  end

  # Factory class for variant type blocks
  class BlockFactory
    def initialize(noMatchingTypesInCsource, namespaceFilter, classParameter)
      @noMatchingTypesInCsource = noMatchingTypesInCsource
      @namespaceFilter = namespaceFilter.nil? ? NamespaceFilter.new(nil) : namespaceFilter
      @classParameter = classParameter
    end

    # Top-level namespace
    def createRootBlock
      RootBlock.new("")
    end

    def createBlock(argLine, parentBlock)
      skip = false
      line = LineWithoutAttribute.new(argLine).str
      block = BaseBlock.new(line)
      newBlock = nil
      typedefBlock = nil

      # Switch by the first keyword of the line
      # Should merge this and parse in classBlock to handle inner classes
      if md = line.match(/^(.*\S)\s*{/) && (parentBlock.nil? || !parentBlock.skippingParse)
        words = line.split(nil)
        if words[0] == "typedef"
          typedefBlock = TypedefBlock.new(line)
          line.gsub!(/^typedef\s+/, "")
          words.shift
        end

        case words[0]
        when "namespace"
          newBlock = NamespaceBlock.new(line)
          skip = @namespaceFilter.exclude?(newBlock.getNamespace)
        when "extern"
          newBlock = ExternCBlock.new(line) if words[1] == '"C"'
        when "class"
          newBlock = ClassBlock.new(line, @classParameter)
        # class is a syntactic sugar as private struct
        when "struct"
          newBlock = ClassBlock.new(line)
        when "template"
          if words.any? { |word| word == "class" }
            newBlock = ClassBlock.new(line)
          end
        when "enum"
          newBlock = EnumBlock.new(line)
        when "union"
          newBlock = UnionBlock.new(line)
        when "inline"
          # No stab need for inline functions
          newBlock = FreeFunctionBlock.new(line) if ["(", ")"].all? { |keyword| line.include?(keyword) }
        when "static"
          # No stab need for inline functions
          newBlock = FreeFunctionBlock.new(line) if ["(", ")", "inline"].all? { |keyword| line.include?(keyword) }
        end
      end

      # Discard forward declarations
      unless newBlock
        if md = line.match(/^(.*\S)\s*;/)
          words = md[1].split(nil)
          case words[0]
          when "typedef"
            newBlock = TypedefBlock.new(line)
          when "using"
            newBlock = UsingBlock.new(line)
            newBlock = newBlock.getTypedefBlock
          when "extern"
            if (words.size >= 3 && words[-1][-1] == ")")
              newBlock = FreeFunctionBlock.new(line)
              newBlock.setNoMatchingTypes if @noMatchingTypesInCsource
            else
              newBlock = ExternVariableStatement.new(line)
            end
          end
        end
      end

      # Delegate to the current class block
      newBlock = parentBlock.parseChildren(line) unless newBlock

      filter = @classParameter ? @classParameter.filterToMock : nil
      if filter && newBlock && newBlock.isNonMemberInstanceOfClass?
        @classParameter.filterToMock.addVariable(newBlock.className, newBlock.varName)
      end

      if newBlock
        block = newBlock
        block.attachTypedefBlock(typedefBlock)
      end

      return block, skip
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

    # Check whether this instance has something to write to files
    def empty?
      [@typeSwapperStr, @varSwapperStr, @declStr, @defStr].all?(&:empty?)
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

    # Remove instances that have nothing to write to files
    def cleanUp
      @set.values.each { |entry| entry.reject!(&:empty?) }
      @set.reject! { |key, value| value.empty? }
    end

    def getInstanceSet(className)
      @set.key?(className) ? @set[className] : []
    end
  end

  # Defined reference
  class DefinedReference
    attr_reader :functionName, :className, :relativeClassName, :isFreeFunction

    def initialize(line)
      @functionName, @className, @relativeClassName, @isFreeFunction = parse(line)
    end

    def parse(line)
      functionName = nil
      className = nil
      relativeClassName = nil
      isFreeFunction = false

      wordSet = line.split(nil)
      if (wordSet.size >= 2)
        name = wordSet[0]
        typename = wordSet[1]
        isFunction = (typename == Mockgen::Constants::CTAGS_TYPENAME_FUNCTION_DEFINITION)

        if isFunction
          functionName = name
          # Extract a function name with a class name
          nameWithColon = "::" + name + "("
          nameSet = wordSet.select { |word| word.include?(nameWithColon) }

          # ctags does not trace absolute namespace from the top namespace
          if nameSet.empty?
            # ctags does not write a function in a namespace as namespace::function()
            isFreeFunction = true
          else
            fullname = nameSet[0]
            namespaceSet = fullname.split("::")
            className = namespaceSet[-2] if namespaceSet.size >= 2
            pos = fullname.rindex("::")
            relativeClassName = fullname[0..(pos-1)] if pos
          end
        end
      end

      return functionName, className, relativeClassName, isFreeFunction
    end
  end

  class StrippedFullname
    attr_reader :name

    def initialize(name)
      @name = nil
      if name
        varname = Mockgen::Common::ChompAfterDelimiter.new(name, "[", "").str
        @name = (varname.size >= 2 && varname[0..1] == "::") ? varname[2..-1] : varname.dup
      end
    end
  end

  # Defined reference set
  class DefinedReferenceSet
    attr_reader :relativeNamespaceOnly

    def initialize(filenameSet)
      @relativeNamespaceOnly = Mockgen::Constants::MODE_CHECK_CLASSNAME_IN_RELATIVE_NAMESPACES_ONLY
      @refFunctionSet, @refClassNameSet = readAllFiles(filenameSet)
    end

    # It is possible to return another reference that in other namespaces
    # because ctags does not always describes absolute namespaces from the top
    def freeFunctionDefined?(name)
      @refFunctionSet.key?(stripFullname(name)) ? true : false
    end

    def classDefined?(relativeClassName, relativeNamespaceOnly)
      className = relativeClassName.split("::")[-1]
      return false unless @refClassNameSet.key?(className)

      # Absolute from the top namespace
      pos = relativeClassName.index("::")
      absolute = (!pos.nil? && (pos == 0))
      strippedName = stripFullname(relativeClassName)

      found = false
      if absolute && !relativeNamespaceOnly
        found = @refClassNameSet[className].any? do |ref|
          ref.relativeClassName == strippedName
        end
      else
        found = @refClassNameSet[className].any? do |ref|
          # Prevent from throwing RegexpError
          !RegexpMetaCharacter.new.contained?(ref.relativeClassName) &&
            strippedName.match(/\b#{ref.relativeClassName}$/) ? true : false
        end
      end

      found
    end

    ## Implementation detail (public for testing)
    def readAllFiles(filenameSet)
      refClassNameSet = {}
      refFunctionSet = {}

      filenameSet.each do |filename|
        readFile(filename, refFunctionSet, refClassNameSet)
      end

      return refFunctionSet, refClassNameSet
    end

    def readFile(filename, refFunctionSet, refClassNameSet)
      # Do not use preprocessor to prevent expanding include files
      str = `#{Mockgen::Constants::CTAGS_COMMAND} #{Mockgen::Constants::CTAGS_OPTIONS} #{filename}`
      parseAllLines(str.split("\n"), refFunctionSet, refClassNameSet)
    end

    def parseAllLines(lineSet, refFunctionSet, refClassNameSet)
      lineSet.each do |rawLine|
        inputLine = rawLine.encode("UTF-8", *Mockgen::Constants::CHARACTER_ENCODING_PARAMETER_SET)
        line = Mockgen::Common::LineWithoutCRLF.new(inputLine).line.strip
        ref = DefinedReference.new(line)
        add(ref, refFunctionSet, refClassNameSet)
      end
    end

    def add(ref, refFunctionSet, refClassNameSet)
      functionName = ref.functionName
      refFunctionSet[functionName] = ref if functionName && ref.isFreeFunction

      className = ref.className
      relativeClassName = ref.relativeClassName
      if className && relativeClassName
        refArray = refClassNameSet.key?(className) ? refClassNameSet[className] : []
        refArray << ref unless refArray.any? { |member| member.relativeClassName == relativeClassName }
        refClassNameSet[className] = refArray
      end
    end

    def stripFullname(name)
      StrippedFullname.new(name).name
    end
  end

  # Undefined reference
  class UndefinedReference
    attr_reader :isVtable, :classFullname, :fullname, :memberName, :argTypeStr, :postFunc

    def initialize(line)
      @isVtable, @classFullname, @fullname, @memberName, @argTypeStr, @postFunc = parse(line)
    end

    def getGlobalVariableStub(typeName)
      namespaceSet = @fullname.split("::").reject(&:empty?)
      varname = namespaceSet.pop

      str = namespaceSet.empty? ? "" : namespaceSet.map { |name| "namespace #{name} {\n" }.join("")
      str += "#{typeName} #{varname};\n"
      str += namespaceSet.empty? ? "" : (("}" * namespaceSet.size) + "\n")
    end

    def parse(argLine)
      fullname = nil
      line = argLine.tr("'`", "").strip

      isVtable, classFullname, fullname = parseVtable(line)
      return isVtable, classFullname, fullname, nil, nil, nil if isVtable
      return parseSymbol(line)
    end

    def parseVtable(line)
      isVtable = false
      classFullname = nil
      fullname = nil

      if md = line.match(/#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE}\s+(vtable|typeinfo)\s+for\s+(.+)/)
        isVtable = true
        classFullname, fullname, memberName = parseNameSet(md[2], isVtable)
      end

      return isVtable, classFullname, fullname
    end

    def parseSymbol(line)
      isVtable = false
      classFullname = nil
      fullname = nil
      memberName = nil
      argTypeStr = nil
      postFunc = nil

      # extract a function or variable name
      unless md = line.match(/#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE}\s+([^\(]+)/)
        return isVtable, classFullname, fullname, memberName, argTypeStr, postFunc
      end

      symbol = md[1]
      classFullname, fullname, memberName = parseNameSet(symbol, false)
      argTypeStr = nil
      postFuncSet = []

      # extract a while declaration
      md = line.match(/#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE}\s+(.+)/)
      body = md[1]

      phraseSet = Mockgen::Common::StringOfParenthesis.new(body).parse
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
      return isVtable, classFullname, fullname, memberName, argTypeStr, postFunc
    end

    def parseNameSet(symbol, isClass)
      classFullname = ""
      memberName = ""

      symbolStr = SymbolWithHeadNamespaceDelimiter.new(symbol, isClass)
      prefix = symbolStr.prefix
      fullname = symbolStr.fullname
      nameSet = symbolStr.nameSet

      if (!nameSet.nil? && !nameSet.empty?)
        memberName = nameSet.pop unless isClass
        classFullname = prefix + nameSet[0..-1].join("::")
      end

      return classFullname, fullname, memberName
    end
  end

  # Undefined reference set
  class UndefinedReferenceSet
    attr_reader :valid

    def initialize(filename)
      @valid = false
      @vtableSet = {}
      @refSet = []
      @refFullnameSet = {}
      return unless filename

      # Filename may not exist in clean build
      if File.exist?(filename)
        File.open(filename, "r") { |file|
          @vtableSet, @refSet, @refFullnameSet = readAllLines(file)
        }
      end

      @valid = !@refSet.empty?
    end

    # make a stub variable that is not a class static member
    def getGlobalVariableStub(varFullname, typeName)
      getReferenceSetByFullname(varFullname).map do |ref|
        ref.getGlobalVariableStub(typeName)
      end
    end

    # Filter by a namespace::functionName
    def getReferenceSetByFullname(fullname)
      getReferenceSet(stripFullname(fullname), @refFullnameSet, @refSet)
    end

    ## Implementation detail (public for testing)
    def readAllLines(file)
      vtableSet = {}
      refSet = []
      refFullnameSet = {}

      while rawLine = file.gets
        line = rawLine.encode("UTF-8", *Mockgen::Constants::CHARACTER_ENCODING_PARAMETER_SET).chomp
        ref = UndefinedReference.new(line)
        if ref.isVtable
          vtableSet[ref.classFullname] = ref
        else
          add(ref, refSet, refFullnameSet)
        end
      end

      return vtableSet, refSet, refFullnameSet
    end

    def getReferenceSet(name, refMap, defaultSet)
      name.nil? ? defaultSet : (refMap.key?(name) ? refMap[name] : [])
    end

    def needVtable?(className)
      @vtableSet.key?(className)
    end

    def add(ref, refSet, refFullnameSet)
      fullname = ref.fullname
      if fullname
        key = stripFullname(fullname)
        refSet << ref
        # A key is unique for each namespace/class::name.
        refFullnameSet[key] = [] unless refFullnameSet.key?(key)

        # Arrays contain overloaded functions but function
        # overloading is rare in user defined functions and
        # most of the arrays have only one element.
        refFullnameSet[key] << ref
      end
    end

    def stripFullname(name)
      StrippedFullname.new(name).name
    end
  end

  # Parameter set to handle input and output files
  class CppFileParameterSet
    # cppNameSpace      : namespace of generated codes
    # inputFilename     : a file before processed by clang
    # convertedFilename : a file after processed by clang
    attr_reader :cppNameSpace, :inputFilename, :linkLogFilename, :convertedFilename
    attr_reader :stubOnly, :functionNameFilterSet, :classNameFilterOutSet, :sourceFilenameSet
    attr_reader :defaultNoForwardingToMock, :noMatchingTypesInCsource, :fillVtable
    attr_reader :noOverloading, :varOnly, :updateChangesOnly, :discardNamespaceValue
    attr_reader :classNameExcludedSet, :testedFilenameGlobSet, :findStatementFilterSet, :explicitClassNameSet
    attr_reader :mockGuardName, :noSwapInlineFunction

    def initialize(cppNameSpace, inputFilename, linkLogFilename, convertedFilename,
                   stubOnly, functionNameFilterSet, classNameFilterOutSet, sourceFilenameSet,
                   defaultNoForwardingToMock, fillVtable, noOverloading, varOnly,
                   updateChangesOnly, discardNamespaceValue, classNameExcludedSet,
                   testedFilenameGlobSet, findStatementFilterSet, explicitClassNameSet,
                   mockGuardName, noSwapInlineFunction)
      @cppNameSpace = cppNameSpace
      @inputFilename = inputFilename
      @linkLogFilename = linkLogFilename
      @convertedFilename = convertedFilename
      @stubOnly = stubOnly
      @functionNameFilterSet = functionNameFilterSet
      @classNameFilterOutSet = classNameFilterOutSet
      @sourceFilenameSet = sourceFilenameSet
      @defaultNoForwardingToMock = defaultNoForwardingToMock
      @fillVtable = fillVtable
      @noOverloading = noOverloading
      @varOnly = varOnly
      @updateChangesOnly = updateChangesOnly
      @discardNamespaceValue = discardNamespaceValue
      @classNameExcludedSet = classNameExcludedSet
      @testedFilenameGlobSet = testedFilenameGlobSet
      @findStatementFilterSet = findStatementFilterSet
      @explicitClassNameSet = explicitClassNameSet
      @mockGuardName = mockGuardName
      @noSwapInlineFunction = noSwapInlineFunction

      # Assume *.c in C, not in C++
      @noMatchingTypesInCsource = hasCsourceFilesOnly?(sourceFilenameSet) &&
                                  Mockgen::Constants::MODE_DEFAULT_NO_MATCHING_TYPES_IN_C_SOURCES
    end

    def hasCsourceFilesOnly?(sourceFilenameSet)
      !sourceFilenameSet.nil? && !sourceFilenameSet.empty? &&
        sourceFilenameSet.all? { |filename| filename =~ /\.c$/ }
    end
  end

  # Input and output file name set
  class CppIoFilenameSet
    attr_reader :classFilename, :typeSwapperFilename, :varSwapperFilename
    attr_reader :declFilename, :defFilename, :numberOfClassInFile

    def initialize(classFilename, typeSwapperFilename, varSwapperFilename,
                   declFilename, defFilename, numberOfClassInFile)
      @classFilename = classFilename
      @typeSwapperFilename = typeSwapperFilename
      @varSwapperFilename = varSwapperFilename
      @declFilename = declFilename
      @defFilename = defFilename
      @numberOfClassInFile = numberOfClassInFile
    end
  end

  # Parse input file lines and format output strings
  class CppFileParser
    def initialize(parameterSet)
      namespaceFilter = NamespaceFilter.new(parameterSet.discardNamespaceValue)
      filterToMockClasses = ClassNameFilter.new(parameterSet.classNameFilterOutSet,
                                                parameterSet.testedFilenameGlobSet,
                                                parameterSet.findStatementFilterSet,
                                                parameterSet.explicitClassNameSet)
      filterToAllClasses = ClassNameFilter.new(parameterSet.classNameExcludedSet, [], [], [])
      classParameter = ClassBlockParameterSet.new(filterToMockClasses, filterToAllClasses)
      @cppNameSpace = parameterSet.cppNameSpace
      @inputFilename = parameterSet.inputFilename
      @blockFactory = BlockFactory.new(parameterSet.noMatchingTypesInCsource, namespaceFilter, classParameter)
      @stubOnly = parameterSet.stubOnly
      @varOnly = parameterSet.varOnly
      @defaultNoForwardingToMock = parameterSet.defaultNoForwardingToMock
      @updateChangesOnly = parameterSet.updateChangesOnly
      @mockGuardName = parameterSet.mockGuardName

      # Current parsing block
      @block = @blockFactory.createRootBlock
      @classInstanceMap = ClassInstanceMap.new
      @variableMockStr = ""

      # Allow nil for testing
      return unless @inputFilename
      return unless parameterSet.convertedFilename

      File.open(parameterSet.convertedFilename, "r") { |file|
        readAllLines(file)
      }

      definedReferenceSet = parseSourceFileSet(parameterSet.sourceFilenameSet)
      undefinedReferenceSet = parseLinkLog(parameterSet.linkLogFilename)

      # Resolve aliases before finding undefined references of free functions
      collectTypedefs(@block)
      filter = SymbolFilter.new(definedReferenceSet, undefinedReferenceSet, parameterSet.functionNameFilterSet,
                                parameterSet.classNameFilterOutSet, parameterSet.fillVtable, parameterSet.noOverloading)
      @functionSetArray = buildFreeFunctionSet(filter)
      makeFreeFunctionSet(@functionSetArray)

      buildClassTree(filter)
      makeClassSet
      @classInstanceMap.cleanUp
    end

    # write to a new file and replace it to file "filename" in update
    def getOutFilename(filename)
      newFilename = filename + ".new"
      @updateChangesOnly ? newFilename : filename
    end

    def updateFile(filename, outFilename)
      # if both files are identical, file "filename" already contains
      # mocks and stubs that are created just now
      if (filename != outFilename)
        if system("cmp -s #{filename} #{outFilename}")
          FileUtils.rm(outFilename, {:force => true})
        else
          # include cases where file "filename" does not exist
          FileUtils.mv(outFilename, filename, {:force => true})
        end
      end
    end

    # Write generated codes to arg files
    def writeToFiles(filenameSet)
      beginNamespace = "namespace #{@cppNameSpace} {\n\n"
      endNamespace = "} // namespace #{@cppNameSpace}\n\n"
      usingNamespace = "using namespace #{@cppNameSpace};\n"

      classFilenameSet = []
      typeSwapperFilenameSet = []
      varSwapperFilenameSet = []
      declFilenameSet = []

      classFilename, declFilename = writeStubSetToFile(filenameSet, beginNamespace, endNamespace, usingNamespace)
      return 0 if @varOnly

      classFilenameSet << classFilename
      declFilenameSet << declFilename

      classSet, typeSwapperSet, varSwapperSet, declSet = writeMockSetToFile(filenameSet, beginNamespace, endNamespace, usingNamespace)
      classFilenameSet.concat(classSet)
      typeSwapperFilenameSet.concat(typeSwapperSet)
      varSwapperFilenameSet.concat(varSwapperSet)
      declFilenameSet.concat(declSet)

      writeAggregatedFiles(filenameSet.classFilename, classFilenameSet)
      writeAggregatedFiles(filenameSet.typeSwapperFilename, typeSwapperFilenameSet)
      writeAggregatedFiles(filenameSet.varSwapperFilename, varSwapperFilenameSet)
      writeAggregatedFiles(filenameSet.declFilename, declFilenameSet)
      0
    end

    def writeStubSetToFile(filenameSet, beginNamespace, endNamespace, usingNamespace)
      # Write all stubs to one file
      blockSet = @functionSetArray
      postfix = Mockgen::Constants::CLASS_POSTFIX_STUB

      classFilename = addPostfixToBasename(filenameSet.classFilename, postfix)
      varSwapperFilename = addPostfixToBasename(filenameSet.varSwapperFilename, postfix)
      declFilename = addPostfixToBasename(filenameSet.declFilename, postfix)
      defFilename = addPostfixToBasename(filenameSet.defFilename, postfix)
      declInlineFilename = addPostfixToBasename(filenameSet.declFilename, postfix + Mockgen::Constants::CLASS_POSTFIX_INLINE)

      unless @varOnly
        writeFreeFunctionFile(classFilename, @inputFilename, beginNamespace, endNamespace, nil,
                              blockSet, :getStringToClassFile, nil, true, true, false)
        writeFreeFunctionFile(declFilename, classFilename, beginNamespace, endNamespace, nil,
                              blockSet, :getStringToDeclFile, nil, false, true, false)
        writeFreeFunctionFile(varSwapperFilename, declFilename, nil, nil, usingNamespace,
                              blockSet, :getStringToSwapperFile, nil, false, true, false)
        writeFreeFunctionFile(defFilename, declFilename, nil, nil, usingNamespace,
                              blockSet, :getStringOfStub, nil, false, false, true)
        writeFreeInlineFunctionFile(declInlineFilename, declFilename, usingNamespace, @mockGuardName, blockSet)
      end
      appendToFile(defFilename, @variableMockStr, true)

      unless @stubOnly
        writeFreeFunctionFile(defFilename, nil, beginNamespace, endNamespace, nil,
                              blockSet, :getStringOfVariableDefinition, "a", false, false, true)
      end

      return classFilename, declFilename
    end

    def writeMockSetToFile(filenameSet, beginNamespace, endNamespace, usingNamespace)
      blockSet = collectClassesToWrite(@block.children).flatten.compact
      sizeOfSet = filenameSet.numberOfClassInFile ? filenameSet.numberOfClassInFile : blockSet.size
      classFilenameSet = []
      typeSwapperFilenameSet = []
      varSwapperFilenameSet = []
      declFilenameSet = []

      index = 0
      serial = 1
      while(index < blockSet.size)
        argBlockSet = blockSet.slice(index, sizeOfSet)
        postfix = (filenameSet.numberOfClassInFile.nil? || filenameSet.numberOfClassInFile > 1) ?
                    "_#{serial}" : argBlockSet[0].getFilenamePostfix

        classFilename = addPostfixToBasename(filenameSet.classFilename, postfix)
        typeSwapperFilename = addPostfixToBasename(filenameSet.typeSwapperFilename, postfix)
        varSwapperFilename = addPostfixToBasename(filenameSet.varSwapperFilename, postfix)
        declFilename = addPostfixToBasename(filenameSet.declFilename, postfix)
        defFilename = addPostfixToBasename(filenameSet.defFilename, postfix)

        writeClassFile(classFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        writeTypeSwapperFile(typeSwapperFilename, classFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        writeVarSwapperFile(varSwapperFilename, declFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        writeDeclFile(declFilename, classFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)

        if @stubOnly
          writeStubFile(defFilename, @inputFilename, argBlockSet)
        else
          writeDefFile(defFilename, classFilename, declFilename, beginNamespace, endNamespace, usingNamespace, argBlockSet)
        end

        classFilenameSet << classFilename
        typeSwapperFilenameSet << typeSwapperFilename
        varSwapperFilenameSet << varSwapperFilename
        declFilenameSet << declFilename

        serial += 1
        index += sizeOfSet
      end

      return classFilenameSet, typeSwapperFilenameSet, varSwapperFilenameSet, declFilenameSet
    end

    ## Implementation detail (public for testing)
    def readAllLines(file)
      currentDepth = 0   # current nesting level of {}
      keyDepth = nil     # discard {} blocks at this or deeper nesting level

      while rawLine = file.gets
        # Do not expand tabs because it is slow
        inputLine = rawLine.encode("UTF-8", *Mockgen::Constants::CHARACTER_ENCODING_PARAMETER_SET)
        line = Mockgen::Common::LineWithoutCRLF.new(inputLine).line.strip
        next if line.empty?
        # Discard preprocessor directives
        next if line[0] == "#"

        # Workaround for LLVM 4.0.0
        # separate "... };\n"s without "{" in a line
        newline, count, semicolon = adjustClosingBlock(line)
        currentDepth, keyDepth = parseLine(newline.strip, currentDepth, keyDepth)
        1.upto(count) do |i|
          str = "}" + ((count == i) ? semicolon : "")
          currentDepth, keyDepth = parseLine(str, currentDepth, keyDepth)
        end
      end
    end

    def adjustClosingBlock(line)
      md = line.match(/(.*[^\s\}][\s\}]+)(;?)\s*$/)
      return line, 0, "" unless md
      newline = md[1]
      semicolon = md[2]

      # unquote the line except quotes between quotes
      unquoted = newline.gsub(/('|")[^'"]\1/,"")
      count = unquoted.count("}") - unquoted.count("{")
      return line, 0, "" if count <= 0

      rest = count
      (newline.size - 1).downto(0) do |i|
        next if newline[i] != "}"
        newline[i] = " "
        rest -= 1
        break if rest <= 0
      end

      return newline, count, semicolon
    end

    # line : leading and trailing spaces and CRLF must be removed
    # Parse and discard inline function definitions
    def parseLine(line, currentDepth, keyDepth)
      newDepth = [currentDepth + line.count("{") - line.count("}"), 0].max
      skipInner = parseBlock(line, !keyDepth.nil?, (!keyDepth.nil? && (newDepth < keyDepth))) && (newDepth > currentDepth)
      skipping = !keyDepth.nil? && (newDepth >= keyDepth)
      newKeyDepth = (keyDepth.nil? && skipInner) ? (currentDepth + 1) : (skipping ? keyDepth : nil)
      return newDepth, newKeyDepth
    end

    def parseBlock(line, skipping, closing)
      skipInner = false

      # } else {
      if (line[0] == "}") && (line[-1] == "{")
      # End of a block
      elsif (line[0] == "}")
        if (!skipping || closing)
          # End of a block
          wordSet = line.tr(";", "").split(nil)
          @block.setTypedef(wordSet[1]) if wordSet.size > 1

          childBlock = @block
          parentBlock = @block.parent
          @block = parentBlock
        end
      elsif !skipping
        begin
          # Discard all template parameter lists
          block, skipInner = @blockFactory.createBlock(line, @block)
        rescue => e
          warn e.message
          raise e unless MockgenFeatures.ignoreAllExceptions
          block = BaseBlock.new(line)
        end

        # Connect "... {" and "... ;" lines
        @block.connect(block)
        # Beginning of a block
        if (line[-1] == "{")
          @block = block
        end
      end

      skipInner
    end

    def eliminateUnusedBlock(block)
      parent = block.parent
      parent.disconnect(block) if parent
    end

    def eliminateAllUnusedBlock(argBlock)
      argBlock.children.each do |child|
        if @stubOnly && child.isClass?
          if child.canTraverse? && child.needStub?
            eliminateAllUnusedBlock(child)
          else
            eliminateUnusedBlock(child)
          end
        else
          if child.canTraverse? && child.canMock?
            eliminateAllUnusedBlock(child)
          else
            eliminateUnusedBlock(child)
          end
        end
      end

      # Erases blocks after marks
      argBlock.eraseChildren
    end

    def parseSourceFileSet(sourceFilenameSet)
      DefinedReferenceSet.new(sourceFilenameSet)
    end

    def parseLinkLog(linkLogFilename)
      UndefinedReferenceSet.new(linkLogFilename)
    end

    def buildFreeFunctionSet(filter)
      rootFreeFunctionSet = FreeFunctionSet.new(@block)
      freeFunctionSetArray = collectFreeFunctions(rootFreeFunctionSet, @block, filter.functionNameFilterSet)

      freeFunctionSetArray.each do |funcSet|
        funcSet.filterByReferenceSet(filter)
      end

      mergeFreeFunctionSetArray(freeFunctionSetArray)
    end

    # Merge scattered definitions in a namespace or in the top-level namespace
    def mergeFreeFunctionSetArray(freeFunctionSetArray)
      nameSet = {}
      allSet = []

      freeFunctionSetArray.each do |freeFunctionSet|
        name = freeFunctionSet.getFullNamespace()
        if nameSet.key?(name)
          # Merge same namespaces including extern "C"
          nameSet[name].merge(freeFunctionSet)
        else
          nameSet[name] = freeFunctionSet
          allSet << freeFunctionSet
        end
      end

      allSet
    end

    def buildClassTree(filter)
      # classSet : class name => block
      classSet = collectClasses(@block.children, filter)
      connectClasses(@block.children, classSet)
      filterVirtualFunctions(@block.children, filter)

      varSet = collectVariables(@block.children)
      @classInstanceMap = makeTypeVarAliases(varSet, classSet)
      @variableMockStr = makeVariableStubs(varSet, filter)

      # Leave free functions
      eliminateAllUnusedBlock(@block)
    end

    def collectClasses(rootBlockSet, filter)
      classSet = {}  # class name => block
      lambdaToBlock = lambda do |block|
        block.setDefaultNoForwardingToMock(@defaultNoForwardingToMock)
        block.filterByReferenceSet(filter)
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

    def filterVirtualFunctions(rootBlockSet, filter)
      # Check overriding after connecting super-sub classes
      lambdaToMark = lambda { |block| block.markMemberFunctionSetVirtual }
      doForAllBlocks(@block.children, lambdaToMark, :isClass?)
      lambdaToFilter = lambda { |block| block.filterByReferenceSetWithSuper(filter) }
      doForAllBlocks(@block.children, lambdaToFilter, :isClass?)
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
      doForAllBlocks(rootBlock.children, lambdaToBlock, :canTraverse?)

      # Construct top-level namespace typedefs including extern "C" {}
      typedefArray = collectTopLevelTypedefSet(rootBlock).flatten.compact
      rootTypedef = rootBlock.typeAliasSet
      typedefArray.each { |typedefSet| rootTypedef.merge(typedefSet) }
      rootBlock.typeAliasSet.removeSystemInternalSymbols
    end

    def makeVariableStubs(varSet, filter)
      varSet.map do |varName, varFullname, typeName|
        filter.undefinedReferenceSet.getGlobalVariableStub(varFullname, typeName)
      end.flatten.compact.sort.uniq.join("\n")
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

      # should move this to a parse phase...
      varNameStr = Mockgen::Common::ChompAfterDelimiter.new(varName, "[", "")
      varFullnameStr = Mockgen::Common::ChompAfterDelimiter.new(varFullname, "[", "")

      mockName = className + Mockgen::Constants::CLASS_POSTFIX_DECORATOR
      actualClassName = block.decoratorName
      actualVarName = varNameStr.str + Mockgen::Constants::CLASS_POSTFIX_FORWARDER

      classBasename = block.getNamespace
      usingLine = (classBasename != className) ? "using #{className};\n" : ""
      typeSwapperStr = usingLine
      typeSwapperStr += '#' + "define #{classBasename} ::#{@cppNameSpace}::#{actualClassName}\n"

      usingLine = (varNameStr.str != varFullnameStr.str) ? "using #{varFullnameStr.str};\n" : ""
      varSwapperStr = usingLine
      varSwapperStr += '#' + "define #{varNameStr.str} ::#{@cppNameSpace}::#{actualVarName}\n"

      # Const member variables and templates are not supported yet
      actualClassName = block.forwarderName

      arrayStr = varNameStr.tailStr
      arrayStr ||= ""
      declStr = "extern #{actualClassName} #{actualVarName}#{arrayStr};\n"

      # A variable declaration does not tell its constructor's arguments.
      # To prevent from searching its definition, this script assumes that
      # a constructor that have the least arity (may be a default constructor)
      # is used and it receives 0s.
      arity = block.getConstructorArity
      argStr = varFullname + ",0" * arity

      # Define arrays in a test case
      defStr = "#{actualClassName} #{actualVarName}(#{argStr});\n" unless varFullnameStr.tailStr
      defStr ||= ""

      return typeSwapperStr, varSwapperStr, declStr, defStr
    end

    def makeFreeFunctionSet(functionSetArray)
      functionSetArray.map do |functionSet|
        if @stubOnly
          functionSet.makeStubSet
        else
          functionSet.makeClassSet
        end
      end
    end

    def makeClassSet
      begin
        if @stubOnly
          lambdaToBlock = lambda { |block| block.makeStubSet }
        else
          lambdaToBlock = lambda { |block| block.makeClassSet }
        end
        doForAllBlocks(@block.children, lambdaToBlock, :isClass?)
      rescue => e
        warn e.message
        raise e unless MockgenFeatures.ignoreAllExceptions
      end

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

    # No recursive finding blocks
    def doForBlockSet(blockSet, lambdaToBlock)
      blockSet.each do |block|
        lambdaToBlock.call(block)
      end
    end

    def collectTopLevelTypedefSet(argBlock)
      typeAliasSetArray = []
      argBlock.children.each do |child|
        typeAliasSet = child.typeAliasSet
        typeAliasSetArray << typeAliasSet if (child.getNamespace.empty? && !typeAliasSet.empty?)
        typeAliasSetArray << collectTopLevelTypedefSet(child) if child.getNamespace.empty?
      end

      typeAliasSetArray
    end

    def collectClassesToWrite(blockSet)
      # Depth-first search
      result = []
      blockSet.each do |block|
        next unless block.canTraverse?
        result << block if block.isClass?
        # Calls should flatten result
        result << collectClassesToWrite(block.children)
      end
      result
    end

    def collectFreeFunctions(freeFunctionSet, argBlock, filterSet)
      newSetArray = collectFreeFunctionArray(freeFunctionSet, argBlock, filterSet)
      newSetArray.each do |set|
        begin
          set.filter(filterSet)
        rescue => e
          # If a filter throws RegexpError, ignore the filter
          warn e.message
          raise e if MockgenFeatures.mockGenTestingException
        end
      end

      newSetArray
    end

    def collectFreeFunctionArray(freeFunctionSet, argBlock, filterSet)
      currentSet = freeFunctionSet
      newSetArray = [currentSet]
      previousSet = nil

      argBlock.children.each do |block|
        next unless block.canTraverse?

        if block.isFreeFunction?
          currentSet.add(block) if block.valid
          block.setDefaultNoForwardingToMock(@defaultNoForwardingToMock)
        elsif block.isNamespace?
          # Depth first
          innerSet = FreeFunctionSet.new(block)
          newSetArray.concat(collectFreeFunctionArray(innerSet, block, filterSet))
        end
      end

      newSetArray
    end

    # Convert ./dir/base.hpp to ./dir/base_1.hpp
    # Insert                      postfix ^^
    def addPostfixToBasename(name, postfix)
      str = name.dup
      pos = name.rindex(".")

      if pos
        str.insert(pos, postfix)
      else
        str += postfix
      end

      str
    end

    def writeAndUpdateFile(filename, labelAttr, preStr, postStr, blockSet)
      outFilename = getOutFilename(filename)
      writeFile(filename, outFilename, labelAttr, preStr, postStr, blockSet)
      updateFile(filename, outFilename)
    end

    def writeFile(filename, outFilename, labelAttr, preStr, postStr, blockSet)
      lineDelimiter = "\n"

      strSet = []
      lambdaToBlock = lambda do |block|
        @classInstanceMap.getInstanceSet(block.getFullname).each do |instance|
          lineSet = instance.send(labelAttr)
          strSet << lineSet
        end
      end

      File.open(outFilename, "w") do |file|
        file.puts preStr
        doForBlockSet(blockSet, lambdaToBlock)
        # Write each definition exactly once
        file.puts strSet.uniq
        file.puts ""
        file.puts postStr
      end
    end

    # always update stub files
    def writeFreeFunctionFile(filename, includeFilename, beginNamespace, endNamespace, usingNamespace,
                              blockSet, labelGetStr, mode, writeMacro, needGuard, markFile)
      filemode = mode
      filemode ||= "w"
      File.open(filename, filemode) do |file|
        str = markFile ? Mockgen::Constants::MARK_FOR_GENERATED_LINES : ""
        if needGuard
          str += getClassFileHeader(includeFilename, filename, writeMacro)
        else
          str += getIncludeDirective(includeFilename) if includeFilename
        end

        file.puts str unless str.empty?
        file.puts beginNamespace if beginNamespace
        file.puts usingNamespace if usingNamespace
        lambdaToBlock = lambda do |block|
          str = block.send(labelGetStr)
          file.puts str if str && !str.empty?
        end
        doForBlockSet(blockSet, lambdaToBlock)
        file.puts endNamespace if endNamespace
        file.puts getIncludeGuardFooter if needGuard
      end
    end

    def writeFreeInlineFunctionFile(filename, includeFilename, usingNamespace, mockGuardName, functionSetArray)
      swapper = FreeFunctionSwapper.new(mockGuardName, functionSetArray)

      File.open(filename, "w") do |file|
        str = getIncludeGuardHeader(filename)
        str += swapper.beginPhrase
        str += getIncludeDirective(includeFilename)
        str += usingNamespace
        str += swapper.body
        str += swapper.endPhrase
        str += getIncludeGuardFooter
        file.puts str
      end
    end

    def appendToFile(filename, str, markFile)
      File.open(filename, "a") do |file|
        file.puts Mockgen::Constants::MARK_FOR_GENERATED_LINES if markFile
        file.puts str
      end
    end

    def writeClassFile(filename, beginNamespace, endNamespace, usingNamespace, blockSet)
      outFilename = getOutFilename(filename)

      File.open(outFilename, "w") do |file|
        file.puts getClassFileHeader(@inputFilename, filename, true)
        file.puts beginNamespace
        lambdaToBlock = lambda do |block|
          str = block.getStringToClassFile
          file.puts str if str && !str.empty?
        end
        doForBlockSet(blockSet, lambdaToBlock)
        file.puts endNamespace
        file.puts getIncludeGuardFooter
      end

      updateFile(filename, outFilename)
    end

    def writeTypeSwapperFile(filename, classFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getSwapperHeader(classFilename, filename, "Class")
      postStr = getIncludeGuardFooter
      writeAndUpdateFile(filename, :typeSwapperStr, preStr, postStr, blockSet)
    end

    def writeVarSwapperFile(filename, declFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getSwapperHeader(declFilename, filename, "Variable")
      postStr = getIncludeGuardFooter
      writeAndUpdateFile(filename, :varSwapperStr, preStr, postStr, blockSet)
    end

    def writeDeclFile(filename, classFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = getDeclHeader(classFilename, filename) + "\n" + beginNamespace
      postStr = endNamespace + "\n" + getIncludeGuardFooter
      writeAndUpdateFile(filename, :declStr, preStr, postStr, blockSet)
    end

    def writeDefFile(filename, classFilename, declFilename, beginNamespace, endNamespace, usingNamespace, blockSet)
      preStr = Mockgen::Constants::MARK_FOR_GENERATED_LINES
      preStr += getDefHeader(@inputFilename, classFilename, declFilename) + "\n" + beginNamespace
      postStr = endNamespace + "\n" + usingNamespace

      outFilename = getOutFilename(filename)
      writeFile(filename, outFilename, :defStr, preStr, postStr, blockSet)
      writeSourceFile(filename, outFilename, blockSet)
      updateFile(filename, outFilename)
    end

    def writeStubFile(filename, inputFilename, blockSet)

      outFilename = getOutFilename(filename)
      File.open(outFilename, "w") do |file|
        file.puts Mockgen::Constants::MARK_FOR_GENERATED_LINES
        file.puts getIncludeDirective(inputFilename)
      end
      writeSourceFile(filename, outFilename, blockSet)
      updateFile(filename, outFilename)
    end

    def writeSourceFile(filename, outFilename, blockSet)
      File.open(outFilename, "a") do |file|
        lambdaToBlock = lambda do |block|
          str = block.getStringToSourceFile
          file.puts str if str && !str.empty?
        end
        doForBlockSet(blockSet, lambdaToBlock)
      end
    end

    # Write header files to include all
    def writeAggregatedFiles(filename, includedFilenameSet)
      outFilename = getOutFilename(filename)

      File.open(outFilename, "w") do |file|
        file.puts "// Include files to all\n"
        file.puts "// This file is machine generated.\n\n"
        file.puts getIncludeGuardHeader(filename)
        includedFilenameSet.each do |includedFilename|
          file.puts getIncludeDirective(includedFilename)
        end
        file.puts ""
        file.puts getIncludeGuardFooter
      end

      updateFile(filename, outFilename)
    end

    def getClassFileHeader(inputFilename, outClassFilename, writeMacro)
      str =  "// Mock and forwarder class definitions\n"
      str += "// This file is machine generated.\n\n"
      str += getIncludeGuardHeader(outClassFilename)
      str += "#include <gmock/gmock.h>\n"
      str += getIncludeDirective(inputFilename)
      str += "\n"

      if (writeMacro)
        [["MOCK_OF", "className", Mockgen::Constants::CLASS_POSTFIX_MOCK],
         ["DECORATOR", "className", Mockgen::Constants::CLASS_POSTFIX_DECORATOR],
         ["FORWARDER", "className", Mockgen::Constants::CLASS_POSTFIX_FORWARDER],
         ["INSTANCE_OF", "varName", Mockgen::Constants::CLASS_POSTFIX_FORWARDER]
        ].each do |name, arg, postfix|
          str += "#ifndef #{name}\n"
          str += "#define #{name}(#{arg}) "+ "::#{@cppNameSpace}::#{arg}"
          str += '##' + postfix + "\n"
          str += "#endif\n"
        end
      end
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
      name = filename.split("/")[-1]
      return "" if (name.nil? || name.empty?)
      # Collapse consecutive _s
      name.gsub(/([^[[:upper:]]])([[:upper:]])/, '\1_\2').upcase.split(".").join("_").squeeze("_")
    end

    def getIncludeDirective(filename)
      nameSet = filename.split("/")
      name = (nameSet.nil? || nameSet.empty? || nameSet[-1].empty?) ? nil : nameSet[-1]
      name.nil? ? "" : ('#include "' + name + '"' + "\n")
    end
  end

  # Parse command line arguments and launcher the parser
  class MockGenLauncher
    def initialize(argv)
      abort if argv.size < 9

      argSet = argv.dup
      mode = argSet.shift
      @stubOnly = Mockgen::Constants::ARGUMENT_SET_STUB_ONLY.any? { |word| word == mode.strip }
      @varOnly = (mode.strip == Mockgen::Constants::ARGUMENT_MODE_VAR)

      @functionNameFilterSet = []
      @classNameFilterOutSet = []
      @classNameExcludedSet = []

      @testedFilenameGlobSet = []
      @findStatementFilterSet = []
      @explicitClassNameSet = []

      @sourceFilenameSet = []
      @numberOfClassInFile = nil
      @outHeaderFilename = nil
      @defaultNoForwardingToMock = false
      @systemHeaderSet = Mockgen::Constants::CLANG_SYSTEM_HEADER_DEFAULT_SET.dup
      @checkInternalSystemPath = false
      @fillVtable = false
      @noOverloading = false
      @updateChangesOnly = false
      @discardNamespaceValue = nil
      @mockGuardName = nil
      @noSwapInlineFunction = false

      while(!argSet.empty?)
        if (argSet[0] == Mockgen::Constants::ARGUMENT_NO_FORWARDING_TO_MOCK)
          @defaultNoForwardingToMock = true
          argSet.shift
          next
        end

        keyword = argSet[0]
        # allow redundant -s and convert --opt to -opt
        optionWord = (keyword.empty? || (keyword[0] != "-")) ? keyword : ("-" + keyword[1..-1].tr('-',''))
        # Unquote
        optionValue = argSet[1].tr('\'"','')

        caught = false
        case optionWord
        when Mockgen::Constants::ARGUMENT_CHECK_INTERNAL_SYSTEM_PATH
          argSet.shift
          @checkInternalSystemPath = true
          caught = true
        when Mockgen::Constants::ARGUMENT_FILL_VTABLE
          argSet.shift
          @fillVtable = true
          caught = true
        when Mockgen::Constants::ARGUMENT_NO_OVERLOADING
          argSet.shift
          @noOverloading = true
          caught = true
        when Mockgen::Constants::ARGUMENT_UPDATE_CHANGES_ONLY
          argSet.shift
          @updateChangesOnly = true
          @numberOfClassInFile = 1
          caught = true
        # Continuous "when" clauses do not fall through. It is different from C++ switch-case.
        when Mockgen::Constants::ARGUMENT_IGNORE_INTERNAL_ERROR, Mockgen::Constants::ARGUMENT_IGNORE_INTERNAL_ERRORS then
          argSet.shift
          MockgenFeatures.ignoreAllExceptions = true
          caught = true
        when Mockgen::Constants::ARGUMENT_NO_SWAP_INLINE_FUNCTION, Mockgen::Constants::ARGUMENT_NO_SWAP_INLINE_FUNCTIONS then
          argSet.shift
          @noSwapInlineFunction = true
          caught = true
        end

        next if caught
        break if argSet.size < 2

        if (!optionValue.empty? && optionValue[0] == "-")
          raise MockgenInvalidArgumentError, "Invalid option value #{optionWord} #{optionValue}"
        end

        stopParsing = false
        case optionWord
        when Mockgen::Constants::ARGUMENT_FUNCTION_NAME_FILTER
          argSet.shift(2)
          @functionNameFilterSet << optionValue
        when Mockgen::Constants::ARGUMENT_CLASS_NAME_FILTER_OUT
          argSet.shift(2)
          @classNameFilterOutSet << optionValue
        when Mockgen::Constants::ARGUMENT_EXCLUDE_CLASS_NAME
          argSet.shift(2)
          @classNameExcludedSet << optionValue
        when Mockgen::Constants::ARGUMENT_SOURCE_FILENAME_FILTER
          argSet.shift(2)
          @sourceFilenameSet << optionValue
        when Mockgen::Constants::ARGUMENT_TESTED_FILENAME_GLOB
          argSet.shift(2)
          @testedFilenameGlobSet << optionValue
        when Mockgen::Constants::ARGUMENT_FIND_STATEMENT_FILTER
          argSet.shift(2)
          @findStatementFilterSet << optionValue
        when Mockgen::Constants::ARGUMENT_EXPLICIT_CLASS_NAME
          argSet.shift(2)
          @explicitClassNameSet << optionValue
        when Mockgen::Constants::ARGUMENT_SPLIT_FILES_FILTER
          argSet.shift(2)
          value = optionValue.to_i
          @numberOfClassInFile = value if (value > 0) && !@updateChangesOnly
        when Mockgen::Constants::ARGUMENT_OUT_HEADER_FILENAME
          argSet.shift(2)
          @outHeaderFilename = optionValue
        when Mockgen::Constants::ARGUMENT_SYSTEM_PATH
          argSet.shift(2)
          @systemHeaderSet << optionValue
        when Mockgen::Constants::ARGUMENT_DISCARD_NAMESPACE, Mockgen::Constants::ARGUMENT_DISCARD_NAMESPACES then
          argSet.shift(2)
          @discardNamespaceValue = optionValue
        when Mockgen::Constants::ARGUMENT_MOCK_GUARD_NAME
          argSet.shift(2)
          @mockGuardName = optionValue
        else
          if (!optionWord.empty? && optionWord[0] == "-")
            raise MockgenInvalidArgumentError, "Invalid option #{optionWord}"
          end

          stopParsing = true
        end

        break if stopParsing
      end

      @inputFilename = argSet.shift
      @inLinkLogFilename = argSet.shift
      @convertedFilename = argSet.shift
      @outClassFilename = argSet.shift
      @outTypeSwapperFilename = argSet.shift
      @outVarSwapperFilename = argSet.shift
      @outDeclFilename = argSet.shift
      @outDefFilename = argSet.shift

      @systemPathSet = selectSystemPath(argSet)
      @clangArgs = quotePath(argSet)
      # Can set later
      @cppNameSpace = Mockgen::Constants::GENERATED_SYMBOL_NAMESPACE
    end

    def selectSystemPath(argSet)
      wordSet = argSet.dup
      pathSet = []

      while(!wordSet.empty?)
        arg = wordSet.shift
        key = Mockgen::Constants::CLANG_SYSTEM_HEADER_OPTION_SET.find do |opt|
          i = arg.index(opt)
          !i.nil? && (i == 0)
        end
        next unless key

        if arg.size > key.size
          # extract /usr/include from -isystem/usr/include
          pathSet << arg[(key.size)..-1]
        else
          # Expect each path is passed as one argument even if it includes
          # whitespaces (i.e. quoted in command lines).
          pathSet << wordSet.shift unless wordSet.empty?
        end
      end

      pathSet
    end

    def generate
      isystemArgs, isystemPaths = collectInternalIsystem(@clangArgs)
      collectNonInternalIsystemHeaders(@clangArgs, @systemPathSet, isystemPaths)

      # Tempfile.create cannot create a tempfile in MinGW
      # and need to specify a filename for clang to output.
      # It is also useful to know how clang format .hpp files.
      command = "#{Mockgen::Constants::CLANG_COMMAND} #{@clangArgs} #{isystemArgs} #{@inputFilename} > #{@convertedFilename}".gsub('\\"', '"')
      system(command)

      parameterSet = CppFileParameterSet.new(@cppNameSpace, @inputFilename, @inLinkLogFilename, @convertedFilename,
                                             @stubOnly, @functionNameFilterSet, @classNameFilterOutSet,
                                             @sourceFilenameSet, @defaultNoForwardingToMock,
                                             @fillVtable, @noOverloading, @varOnly,
                                             @updateChangesOnly, @discardNamespaceValue, @classNameExcludedSet,
                                             @testedFilenameGlobSet, @findStatementFilterSet, @explicitClassNameSet,
                                             @mockGuardName, @noSwapInlineFunction)
      parseHeader(parameterSet)

      # Value to return as process status code
      0
    end

    # launch clang -### and extract -internal-isystem paths
    def collectInternalIsystem(argStr)
      clangArgStr = selectClangNonCc1Options(argStr)
      # replace -cc1 options
      [["-cc1",""], ["-triple","-target"], ["-ast-print",""]].each do |fromWord, toWord|
        clangArgStr.gsub!(fromWord, toWord)
      end

      # clang++ -### causes errors for non-existing source files
      sourceFilename = @sourceFilenameSet.empty? ? @inputFilename : @sourceFilenameSet[0]
      command = "#{Mockgen::Constants::CLANG_COMMAND} -### #{clangArgStr} #{sourceFilename}"
      stdoutstr, stderrstr, status = Open3.capture3(command)

      # Ignore the error status if found internal headers
      if @checkInternalSystemPath
        unless stderrstr.index(Mockgen::Constants::CLANG_INTERNAL_SYSTEM_HEADER_OPTION)
          raise MockgenRuntimeError, "#{command} failed"
        end
      end

      # May return an empty string on Cygwin
      selectInternalIsystem(stderrstr)
    end

    def selectClangNonCc1Options(argStr)
      clangArgStr = argStr.dup
      # replace -cc1 options
      [["-cc1",""], ["-triple","-target"], ["-ast-print",""]].each do |fromWord, toWord|
        clangArgStr.gsub!(fromWord, toWord)
      end

      clangArgStr
    end

    def selectInternalIsystem(clangOutStr)
      argSet = []
      pathSet = []
      popNext = false

      clangOutStr.split('" "').each do |word|
        if (word == "-internal-isystem")
          argSet << word
          popNext = true
        else
          if popNext
            # quote "C:Program Files"
            # squeeze redundant backslashes
            path = '"' + word.squeeze("\\") + '"'
            argSet << path
            pathSet << path
            popNext = false
          end
          popNext = false
        end
      end

      return argSet.join(" "), pathSet
    end

    # launch clang -H and extract included header files except internal-isystem headers
    def collectNonInternalIsystemHeaders(argStr, systemPaths, internalPaths)
      return unless @outHeaderFilename

      headerSet = []
      @sourceFilenameSet.each do |inputFilename|
        clangArgStr = selectClangNonCc1Options(argStr).gsub("-cxx-isystem", "-isystem")
        command = "#{Mockgen::Constants::CLANG_COMMAND} -H #{clangArgStr} #{inputFilename}"
        stdoutstr, stderrstr, status = Open3.capture3(command)

        allSystemPaths = [@systemHeaderSet, systemPaths, internalPaths].flatten
        headerSet.concat(selectNonInternalIsystemHeaders(stderrstr, allSystemPaths))
      end

      File.open(@outHeaderFilename, "w") do |file|
        headerSet.uniq.each do |header|
          str = '#include "' + header + '"'
          file.puts str
        end
      end
    end

    def selectNonInternalIsystemHeaders(logStr, argIsystemPaths)
      # clang++ -H writes paths with delimiter /
      isystemPaths = argIsystemPaths.map { |path| path.gsub(/\\+/,"/") }

      logStr.split("\n").map do |line|
        headerFilename = nil
        # Leading ... indicate nesting level of a header file
        if md = line.chomp.match(/^\.+\s+(.+)/)
          filename = File.absolute_path(md[1]).gsub(/\\+/,"/")
          headerFilename = (isystemPaths.all? { |path| filename.index(path).nil? }) ? filename : nil
        end
        headerFilename
      end.select{ |path| !path.nil? }.uniq
    end

    ## Implementation detail (public for testing)
    def quotePath(argv)
      quoteNext = false
      quoting = false

      argSet = []
      currentWord = ""
      argv.each do |word|
        next if word.empty?
        if (word[0] == "-")
          # break by any options other than -cxx-isystem
          argSet[-1] = argSet[-1] + '"' if !argSet.empty? && quoting
          currentWord = word
          if (word == "-cxx-isystem")
            quoteNext = true
            quoting = true
          else
            quoteNext = false
            quoting = false
          end
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

    def parseHeader(parameterSet)
      parser = CppFileParser.new(parameterSet)
      parser.writeToFiles(CppIoFilenameSet.new(@outClassFilename, @outTypeSwapperFilename, @outVarSwapperFilename,
                                               @outDeclFilename, @outDefFilename, @numberOfClassInFile))
    end
  end
end
