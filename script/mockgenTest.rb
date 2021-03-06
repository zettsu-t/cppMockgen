#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require_relative './mockgenImpl.rb'
require 'tempfile'
require 'test/unit'

# Omit the module name in this testing
include Mockgen
include MockgenFeatures

TestInputSourceFileForIsystem = "tested_src/linkErrorForTesting.c"
TestInputSourceFileSet = ["tested_src/mockgenSampleBody.cpp", "tested_src/mockgenSampleUser.cpp"]
TestMockRefClass = Struct.new(:classFullname, :fullname, :memberName, :argTypeStr, :postFunc)

def getTestSwitchMockVarnameWithValue(funcname, value)
  "    #{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_TYPE} " +
    "#{funcname}_#{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX} {#{value}};\n"
end

def getTestSwitchMockVarname(funcname)
  getTestSwitchMockVarnameWithValue(funcname, Mockgen::Constants::MEMFUNC_FORWARDING_OFF_VALUE)
end

def getTestSwitchMockStaticVarnameWithValue(funcname, value)
  "    static #{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_TYPE} " +
    "#{funcname}_#{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX};\n"
end

def getTestSwitchMockStaticVarname(funcname)
  getTestSwitchMockStaticVarnameWithValue(funcname, Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX)
end

def getTestSwitchMockCondition(funcname, varname)
  "#{varname} && !#{funcname}_#{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX}"
end

def checkIfExceptionCaught(expr)
  [false, true].map do |ignore|
    caught = false
    MockgenFeatures.ignoreAllExceptions = ignore
    begin
      expr.call
    rescue => e
      caught = true
    ensure
      MockgenFeatures.ignoreAllExceptions = false
    end
    caught
  end
end

class TestUtf8Encoding < Test::Unit::TestCase
  def test_removeByteOrderMark
    str = "   DEFG"
    str.setbyte(0, 0xef)
    str.setbyte(1, 0xbb)
    str.setbyte(2, 0xbf)
    newStr = str.encode("UTF-8", *Mockgen::Constants::CHARACTER_ENCODING_PARAMETER_SET).tr(" ","")
    assert_true(newStr.include?("DEFG"))
  end
end

class TestChompAfterDelimiter < Test::Unit::TestCase
  data(
    'empty 1' => ["", ":", "", "", nil],
    'empty 2' => ["", ":", "::", "", nil],
    'not split' => ["Func()", ":", "", "Func()", nil],
    'constructor' => ["Derived() : Base()", ":", "", "Derived()", ": Base()"],
    'assignment' => ["int a = 1", "=", "", "int a", "= 1"],
    'mixed' => ["int a = (int)1", "=", "", "int a", "= (int)1"],
    'ignore :: 1' => ["Derived(::) : Base", ":", "::", "Derived(::)", ": Base"],
    'ignore :: 2' => ["::Name::Derived(::) : Base", ":", "::", "::Name::Derived(::)", ": Base"],
    'ignore :: 3' => ["Derived(::) : Base { body", "{", "::", "Derived(::) : Base", "{ body"],
    ': at end' => ["Derived() :", ":", "::", "Derived()", ":"])
  def test_chompAfterDelimiter(data)
    line, delimiter, excludedStr, expected, expectedTail  = data
    splitter = Mockgen::Common::ChompAfterDelimiter.new(line, delimiter, excludedStr)
    assert_equal(expected, splitter.str)
    assert_equal(expectedTail, splitter.tailStr)
  end
end

class TestStringOfBrackets < Test::Unit::TestCase
  def test_parenthesis
    line = "((a))( ( ( b ) ) )"
    expected = [["((a))", "(a)"], ["(((b)))", "((b))"]]
    pattern = '((?>[^\s(]+|(\((?>[^()]+|\g<-1>)*\)))+)'
    assert_equal(expected, Mockgen::Common::StringOfBrackets.new(line).parse(pattern, '\s*(\(|\))\s*'))
  end

  def test_angleBrackets
    line = "template <class T, class S = tmp<T>>"
    expected = [["template", nil], ["<class T, class S = tmp<T>>", "class T, class S = tmp<T>"]]
    pattern = '((?>[^\s<]+|(<(?>[^<>]+|\g<-1>)*>))+)'
    assert_equal(expected, Mockgen::Common::StringOfBrackets.new(line).parse(pattern, '\s*(<|>)\s*'))
  end
end

class TestStringOfParenthesis < Test::Unit::TestCase
  data(
    'empty' => ["", [], []],
    'one' => ["()", [["()", ""]], [["()", ""]]],
    'one+str1' => ["(a)", [["(a)", "a"]], [["(a)", "a"]]],
    'one+str2' => ["( a )", [["(a)", "a"]], [["( a )", "a"]]],
    'two' => ["()( )", [["()", ""], ["()", ""]], [["()", ""], ["( )", ""]]],
    'two+str' => ["( a ) ( b )", [["(a)", "a"], ["(b)", "b"]], [["( a )", "a"], ["( b )", "b"]]],
    'nested' => ["((a))( ( ( b ) ) )", [["((a))", "(a)"], ["(((b)))", "((b))"]], [["( (a) )", "(a)"], ["( ( ( b ) ) )", "( ( b ) )"]]],
    'out1' => ["f()", [["f", nil], ["()", ""]], [["f", nil], ["()", ""]]],
    'out2' => ["f ()g", [["f", nil], ["()", ""], ["g", nil]], [["f", nil], ["()", ""], ["g", nil]]],
    'funcPtr1' => [" result(*f)(arg)", [["result", nil], ["(*f)", "*f"], ["(arg)", "arg"]],
                   [["result", nil], ["(*f)", "*f"], ["(arg)", "arg"]]],
    'funcPtr2' => [" const T* (*func) (void) const; ",
                   [["const", nil], ["T*", nil], ["(*func)", "*func"], ["(void)", "void"], ["const;", nil]],
                   [["const", nil], ["T*", nil], ["(*func)", "*func"], ["(void)", "void"], ["const;", nil]]],
    'funcPtr3' => [" T (*func) (int, long);",
                   [["T", nil], ["(*func)", "*func"], ["(int, long)", "int, long"], [";", nil]],
                   [["T", nil], ["(*func)", "*func"], ["(int, long)", "int, long"], [";", nil]]],
    'funcPtr4' => [" T*(*Class::func)(int, long); ",
                   [["T*", nil], ["(*Class::func)", "*Class::func"], ["(int, long)", "int, long"], [";", nil]],
                   [["T*", nil], ["(*Class::func)", "*Class::func"], ["(int, long)", "int, long"], [";", nil]]])
  def test_splitByOuterParenthesis(data)
    line, expected, expectedWithSpaces = data
    assert_equal(expected, Mockgen::Common::StringOfParenthesis.new(line).parse)
    assert_equal(expectedWithSpaces, Mockgen::Common::StringOfParenthesis.new(line).parseAndKeepSpaces)
  end
end

class TestStringOfAngleBrackets < Test::Unit::TestCase
  data(
    'empty' => ["", []],
    'one' => ["<>", [["<>", ""]]],
    'one+str1' => ["<typename T>", [["<typename T>", "typename T"]]],
    'one+str2' => ["< typename T >", [["<typename T>", "typename T"]]],
    'template empty' => ["template <>", [["template", nil], ["<>", ""]]],
    'template one' => ["template <class T>", [["template", nil], ["<class T>", "class T"]]],
    'template two' => ["template <class T, size_t S>",
                       [["template", nil], ["<class T, size_t S>", "class T, size_t S"]]],
    'specialized' => ["template <> class T<int>",
                      [["template", nil], ["<>", ""], ["class", nil], ["T", nil], ["<int>", "int"]]],
    'nested' => ["template <class T, class S = tmp<T>>",
                 [["template", nil], ["<class T, class S = tmp<T>>", "class T, class S = tmp<T>"]]])
  def test_splitByOuterAngleBrackets(data)
    line, expected = data
    assert_equal(expected, Mockgen::Common::StringOfAngleBrackets.new(line).parse)
  end
end

class TestMockRefSetClass
  attr_reader :valid

  def initialize(valid, refSet)
    @valid = valid
    @refSet = refSet
  end

  def getReferenceSetByFullname(fullname)
    @refSet
  end
end

class TestMockRefVatbleSetClass < TestMockRefSetClass
  def initialize(valid, refSet, fullname)
    super(valid, refSet)
    @fullname = fullname
  end

  def needVtable?(fullname)
    @fullname == fullname
  end
end

class TestMockRefMonoSetClass
  attr_reader :valid

  def initialize(ref)
    @valid = true
    @refSet = [ref]
  end

  def invalidate
    @valid = false
  end

  def getReferenceSetByFullname(fullname)
    @refSet
  end
end

class TestConstants < Test::Unit::TestCase
  def test_constants
    assert_equal(Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_SET.size,
                 Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_MAP.size)
    assert_equal(Mockgen::Constants::CLASS_NAME_EXCLUDED_MAP.size,
                 Mockgen::Constants::CLASS_NAME_EXCLUDED_SET.size)
    assert_equal(Mockgen::Constants::MEMFUNC_WORD_SKIPPED_SET.size,
                 Mockgen::Constants::MEMFUNC_WORD_SKIPPED_MAP.size)
    assert_equal(Mockgen::Constants::MEMFUNC_WORD_RESERVED_TYPE_SET.size,
                 Mockgen::Constants::MEMFUNC_WORD_RESERVED_TYPE_MAP.size)
    assert_equal(Mockgen::Constants::MEMFUNC_WORD_COMPARED_SET.size,
                 Mockgen::Constants::MEMFUNC_WORD_COMPARED_MAP.size)
    assert_equal(Mockgen::Constants::MEMVAR_FIRST_WORD_REJECTED_SET.size,
                 Mockgen::Constants::MEMVAR_FIRST_WORD_REJECTED_MAP.size)
    assert_equal(Mockgen::Constants::MEMVAR_FIRST_WORD_EXCLUDED_SET.size,
                 Mockgen::Constants::MEMVAR_FIRST_WORD_EXCLUDED_MAP.size)
    assert_equal(Mockgen::Constants::MEMVAR_LAST_WORD_REJECTED_SET.size,
                 Mockgen::Constants::MEMVAR_LAST_WORD_REJECTED_MAP.size)
  end
end

class TestTypeStringWithoutModifier < Test::Unit::TestCase
  def test_metaCharacters
    Mockgen::Constants::REGEXP_META_CHARACTER_SET.each do |c|
      [c, "Name" + c, c + "Name"].each do |str|
        assert_true(RegexpMetaCharacter.new.contained?(str))
      end
    end
  end

  def test_nonMtaCharacters
    ["", " ", ":", ",", "_"].each do |c|
      assert_false(RegexpMetaCharacter.new.contained?(c))
    end
  end
end

class TestTypeStringWithoutModifier < Test::Unit::TestCase
  def test_removeReservedWord
    Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_SET.each do |key|
      name = "ConcreteName"
      assert_equal([name], TypeStringWithoutModifier.new([key, name]).strSet)
    end
  end

  data(
    'space' => ["  ", []],
    'primitive' => ["class T**", ["T", "**"]],
    'spaces' => ["class T * ", ["T", "*"]],
    'reference' => ["struct S&", ["S", "&"]],
    'array' => ["struct S[]", ["S", "[", "]"]],
    'array with its size' => ["struct S[sizeof(T)]", ["S", "[", "sizeof(T)", "]"]])
  def test_removeReservedWordWithModifier(data)
    arg, expected = data
    assert_equal(expected, TypeStringWithoutModifier.new([arg]).strSet)
  end
end

class TestSymbolWithHeadNamespaceDelimiter < Test::Unit::TestCase
  data(
    'empty' => ["", "", "", []],
    'simple short' => ["v", "", "v", ["v"]],
    'simple long'  => ["var", "", "var", ["var"]],
    '::simple' => ["::var", "", "::var", ["var"]],
    'member short' => ["c::Member", "", "c::Member", ["c", "Member"]],
    'member long'  => ["ClassName::Member", "", "ClassName::Member", ["ClassName", "Member"]],
    '::member' => ["::ClassName::Member", "", "::ClassName::Member", ["ClassName", "Member"]],
    'namespace short' => ["n::Class::Member", "::", "::n::Class::Member", ["n", "Class" ,"Member"]],
    'namespace long'  => ["Name::Class::Member", "::", "::Name::Class::Member", ["Name", "Class" ,"Member"]],
    '::namespace' => ["::Name::Class::Member", "::", "::Name::Class::Member", ["Name", "Class" ,"Member"]])
  def test_parseMember(data)
    name, expectedPrefix, expectedFullname, expectedNameSet = data
    symbol = SymbolWithHeadNamespaceDelimiter.new(name, false)
    assert_equal(expectedPrefix, symbol.prefix)
    assert_equal(expectedFullname, symbol.fullname)
    assert_equal(expectedNameSet, symbol.nameSet)
  end

  data(
    'empty' => ["", "", "", []],
    'simple short' => ["c", "", "c", ["c"]],
    'simple long'  => ["Class", "", "Class", ["Class"]],
    '::simple' => ["::Class", "", "::Class", ["Class"]],
    'namespace short' => ["n::Class", "::", "::n::Class", ["n", "Class"]],
    'namespace long'  => ["Name::Class", "::", "::Name::Class", ["Name", "Class"]],
    '::namespace' => ["::Name::Class", "::", "::Name::Class", ["Name", "Class"]])
  def test_parseClassname(data)
    name, expectedPrefix, expectedFullname, expectedNameSet = data
    symbol = SymbolWithHeadNamespaceDelimiter.new(name, true)
    assert_equal(expectedPrefix, symbol.prefix)
    assert_equal(expectedFullname, symbol.fullname)
    assert_equal(expectedNameSet, symbol.nameSet)
  end
end

class TestArgumentSplitter < Test::Unit::TestCase
  data(
    'empty' => ["", [], []],
    'primitive' => ["int", ["int"], ["int"]],
    'primitives' => ["int, long", ["int", "long"], ["int", " long"]],
    'pointers' => ["int *, long*", ["int *", "long*"], ["int *", " long*"]],
    'pointer to a function' => ["int, void(*f)(int,long), long*", ["int", "void(*f)(int,long)", "long*"],
                                ["int", " void (*f) (int,long) ",  " long*"]])
  def test_all(data)
    line, expected, expectedWithSpaces = data
    assert_equal(expected, ArgumentSplitter.new(line).argSet)
    assert_equal(expectedWithSpaces, ArgumentSplitterWithSpaces.new(line).argSet)
  end
end

class TestTemplateParameterListFilter < Test::Unit::TestCase
  data(
    'empty' => ["", ""],
    'word' => ["int", "int"],
    'words' => ["class Name", "class Name"],
    'one' => ["template <typename T> class Name", "template <> class Name"],
    'multi' => ["template <typename T> using Array = typename std::vector<T>;",
                "template <> using Array = typename std::vector <> ;"],
    'nested' => ["template <class T, class S = NameSpace::tmp<T>> Name",
                 "template <> Name"])
  def test_all(data)
    line, expected = data
    assert_equal(expected, TemplateParameterListFilter.new(line).str)
  end
end

class TestTypeAliasSet < Test::Unit::TestCase
  def test_add
    typeAliasSet = TypeAliasSet.new
    assert_true(typeAliasSet.empty?)

    typeAliasSet.add("uint32_t", "unsigned int")
    assert_false(typeAliasSet.empty?)
    expected = {"uint32_t" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)

    typeAliasSet.add("UINT", "uint32_t")
    expected = {"uint32_t" => "unsigned int", "UINT" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)

    typeAliasSet.add("puint32_t", "unsigned int*")
    expected = {"uint32_t" => "unsigned int", "puint32_t" => "unsigned int *", "UINT" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)

    typeAliasSet.add("cuint32_t", "const unsigned int")
    expected = {"cuint32_t" => "const unsigned int", "uint32_t" => "unsigned int",
                "puint32_t" => "unsigned int *", "UINT" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)

    typeAliasSet.add("UINT", "uint64_t")
    assert_equal(expected, typeAliasSet.aliasSet)
  end

  def test_merge
    outerSet = TypeAliasSet.new
    outerSet.add("uint32_t", "unsigned int")
    outerSet.add("UINT", "uint64_t")
    innerSet = TypeAliasSet.new
    innerSet.add("UINT", "uint32_t")

    innerSet.merge(outerSet)
    expected = {"UINT" => "uint64_t", "uint32_t" => "unsigned int"}
    assert_equal(expected, outerSet.aliasSet)
    expected = {"UINT" => "unsigned int", "uint32_t" => "unsigned int"}
    assert_equal(expected, innerSet.aliasSet)
  end

  def test_resolve
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.resolve("uint32_t", "unsigned int")
    expected = {"uint32_t" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)

    typeAliasSet.resolve("UINT", "uint32_t")
    expected = {"uint32_t" => "unsigned int", "UINT" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)

    typeAliasSet.resolve("PUINT", "uint32_t*")
    expected = {"PUINT" => "unsigned int *", "uint32_t" => "unsigned int", "UINT" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)
  end

  def test_resolvePointerToFunction
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.resolve("uint32_t", "unsigned int")

    name = "FuncPtr"
    typeAliasSet.resolve(name, "void(*)(uint32_t)")
    expected = {"uint32_t" => "unsigned int", name => "void(*)(unsigned int)"}
    assert_equal(expected, typeAliasSet.aliasSet)

    name = "FuncPtr2"
    typeAliasSet.resolve("UINT", "uint32_t")
    typeAliasSet.resolve(name, "void(*)(UINT*)")
    actual = typeAliasSet.aliasSet[name]
    assert_equal("void(*)(unsigned int *)", actual)

    name = "FuncPtr3"
    typeAliasSet.resolve("PLONG", "long*")
    typeAliasSet.resolve(name, "void(*)(PLONG)")
    actual = typeAliasSet.aliasSet[name]
    assert_equal("void(*)(long *)", actual)

    name = "FuncPtr4"
    typeAliasSet.resolve(name, "void(*)(Type&)")
    actual = typeAliasSet.aliasSet[name]
    assert_equal("void(*)(Type &)", actual)

    name = "FuncPtr5"
    typeAliasSet.resolve(name, "void(*)(FuncPtr2)")
    actual = typeAliasSet.aliasSet[name]
    assert_equal("void(*)(void(*)(unsigned int *))", actual)
  end

  def test_resolveNestedPointerToFunction
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.resolve("uint32_t", "unsigned int")
    typeAliasSet.resolve("funcptr", "void(*)(uint32_t)")
    typeAliasSet.resolve("outerptr", "void(*)(funcptr,uint32_t)")

    expected = {"funcptr"=>"void(*)(unsigned int)",
                "outerptr"=>"void(*)(void(*)(unsigned int),unsigned int)",
                "uint32_t"=>"unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)
  end

  def test_removeSystemInternalSymbols
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.add("uint32_t", "unsigned int")
    typeAliasSet.add("__uint32_t", "unsigned int")
    expected = {"__uint32_t" => "unsigned int", "uint32_t" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)
    typeAliasSet.removeSystemInternalSymbols

    expected = {"uint32_t" => "unsigned int"}
    assert_equal(expected, typeAliasSet.aliasSet)
  end

  data(
    'primitive' => "__int32_t",
    'offset' => "_off_t",
    'system' => "__pthread_t")
  def test_isSystemInternalSymbolTrue(data)
    str = data
    typeAliasSet = TypeAliasSet.new
    assert_true(typeAliasSet.isSystemInternalSymbol?(str))
  end

  data(
    'nullptr_t' => "nullptr_t",
    'nullptr' => "decltype(nullptr)")
  def test_isSystemInternalSymbolFalse(data)
    str = data
    typeAliasSet = TypeAliasSet.new
    assert_false(typeAliasSet.isSystemInternalSymbol?(str))
  end

  data(
    'primitive' => ["uint32_t", "unsigned int"],
    'const' => ["const uint32_t", "const unsigned int"],
    'pointer' => ["uint32_t *", "unsigned int *"],
    'reference' => ["uint32_t &", "unsigned int &"],
    'unknown' => ["int32_t", "int32_t"])
  def test_resolveIntAlias(data)
    typeAlias, expected = data
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.add("uint32_t", "unsigned int")
    assert_equal(expected, typeAliasSet.resolveAlias(typeAlias))
  end

  def test_resolveStructIntAlias
    Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_SET.each do |key|
      typeAliasSet = TypeAliasSet.new
      typeAliasSet.add("Alias", "Name")
      assert_equal("Name", typeAliasSet.resolveAlias("Alias"))
    end
  end

  data(
    'primitive' => ["void(*)(uint32_t)", "void(*)(unsigned int)"],
    'pointer' => ["void(*)(uint32_t*)", "void(*)(unsigned int *)"],
    'unknown' => ["void(*)(int32_t)", "void(*)(int32_t)"],
    'unknown pointer' => ["void(*)(int32_t*)", "void(*)(int32_t *)"])
  def test_resolveAliasPointerToFunction(data)
    typeAlias, expected = data
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.add("uint32_t", "unsigned int")
    assert_equal(expected, typeAliasSet.resolveAlias(typeAlias))
  end

  data(
    'empty' => [[], []],
    'primitive' => [["uint32_t", "uint"], ["unsigned int", "uint"]],
    'pointer' => [["uint32_t*"], ["unsigned int*"]],
    'reference' => [["uint32_t&"], ["unsigned int&"]],
    'multiple arguments' => [["uint32_t,uint32_t*,uint"], ["unsigned int,unsigned int*,uint"]],
    'spaces' => [["uint32_t uint32_t* uint"], ["unsigned int unsigned int* uint"]],
    'function' => [["(uint32_t,uint32_t*)"], ["(unsigned int,unsigned int*)"]])
  def test_resolveLocal(data)
    wordSet, expected = data
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.add("uint32_t", "unsigned int")
    assert_equal(expected, typeAliasSet.resolveLocal(wordSet))
  end

  data(
    's' => [["systemTime"], ["unsigned long long"]],
    's&*' => [["systemTime * &"], ["unsigned long long * &"]],
    '()' => [["(decltype)(var_system)"], ["(decltype)(var_system)"]])
  def test_resolveLocalMeta(data)
    wordSet, expected = data
    typeAliasSet = TypeAliasSet.new
    typeAliasSet.add("systemTime", "unsigned long long")
    assert_equal(expected, typeAliasSet.resolveLocal(wordSet))
  end

  def test_removeRedundantSpaces
    ["(v)", "  (v)", "(  v)", " ( v)", "(v) ", " (v  )", "( v )  "].each do |str|
      assert_equal("(v)", TypeAliasSet.new.removeRedundantSpaces(str))
    end
  end
end

class TestNamespaceFilter < Test::Unit::TestCase
  def filterNameSet(filter)
    ["_name", "_Name", "userDefined", "user__Defined"].all? do |name|
      assert_false(filter.exclude?(name))
    end
  end

  def test_empty
    filter = NamespaceFilter.new(nil)
    filterNameSet(filter)
    ["__cxxabiv1", "__gnu_cxx", "__gnu_debug", "std", "boost", "mpl_", "testing"].all? do |name|
      assert_false(filter.exclude?(name))
    end
  end

  def test_testing
    filter = NamespaceFilter.new("testing")
    filterNameSet(filter)

    assert_true(filter.exclude?("testing"))
    ["__cxxabiv1", "__gnu_cxx", "__gnu_debug", "std", "boost", "mpl_"].all? do |name|
      assert_false(filter.exclude?(name))
    end
  end

  def test_internal
    filter = NamespaceFilter.new("internal")
    filterNameSet(filter)

    ["testing", "__cxxabiv1", "__gnu_cxx", "__gnu_debug"].all? do |name|
      assert_true(filter.exclude?(name))
    end

    ["std", "boost", "mpl_"].all? do |name|
      assert_false(filter.exclude?(name))
    end
  end

  def test_std
    filter = NamespaceFilter.new("internal")
    filterNameSet(filter)

    ["__cxxabiv1", "__gnu_cxx", "__gnu_debug", "std", "boost", "mpl_"].all? do |name|
      assert_true(filter.exclude?(name))
    end
  end
end

class SilentClassNameFilter < ClassNameFilter
  attr_reader :message

  def initialize(patternSet)
    super(patternSet, [], [], [])
    @message = ""
  end

  def warn(message)
    @message += message
  end
end

class TestClassNameFilter < Test::Unit::TestCase
  data(
    'nil' => [nil, false, nil],
    'empty pattern' => [[], false, nil],
    'empty string' => [["^$"], false, nil],
    'invalid' => [["*(*"], false, "*(*"],
    'all' => [[".*"], true, nil],
    'valid' => [["Name"], true, nil],
    'invalid and valid' => [["*(*", "Name"], true, "*(*"],
    'valid and invalid' => [["Name", "*)*"], true, nil])
  def test_withoutNamespace(data)
    patternSet, expected, message = data
    classname = "TargetNameA"
    fullname = "Name::TargetNameA"
    filter = SilentClassNameFilter.new(patternSet)
    assert_equal(expected, filter.excludeSimpleName?(classname))
    assert_equal(expected, filter.excludeFullname?(name))

    expectedMessage = message.nil? ? "" : "Invalid pattern #{message}"
    assert_equal(expectedMessage, filter.message)
  end

  data(
    'all' => [["A::.*"], true],
    'full' => [["A::NameB"], true],
    'partial' => [["A::Name"], true],
    'different namespace' => [["B::.*"], false],
    'different classname' => [["A::NameC"], false])
  def test_namespace(data)
    patternSet, expected = data
    classname = "NameB"
    fullname = "A::NameB"
    filter = ClassNameFilter.new(patternSet, [], [], [])

    assert_true(filter.instance_variable_get(:@simpleFilterOutSet).empty?)
    assert_equal(false, filter.excludeSimpleName?(classname))
    assert_equal(expected, filter.excludeFullname?(fullname))
  end

  def test_initializeSet
    filter = ClassNameFilter.new([], ["NotExist"], [], [])
    assert_true(filter.instance_variable_get(:@classNameSet).empty?)
    assert_true(filter.instance_variable_get(:@varNameSet).empty?)
  end

  def test_addVariable
    filter = ClassNameFilter.new([], [], [], [])
    className = "NameC"
    varName = "VarName"
    filter.addVariable(className, varName)

    filter.instance_variable_set(:@classNameSet, {})
    filter.addVariable(className, varName)
    assert_true(filter.instance_variable_get(:@classNameSet).empty?)

    filter.instance_variable_set(:@varNameSet, {})
    filter.addVariable(className, varName)
    assert_true(filter.instance_variable_get(:@classNameSet).empty?)

    filter.instance_variable_set(:@varNameSet, {varName => true})
    filter.addVariable(className, varName)
    assert_true(filter.instance_variable_get(:@classNameSet).key?(className))
    assert_true(filter.instance_variable_get(:@classNameSet)[className])
  end

  data(
    'not matched' => ["Name", false, true],
    'list' => ["NameC", false, false],
    'explicit' => ["EX", false, false])
  def test_excludeSimpleName(data)
    name, expectedSimple, expectedFull = data
    filter = ClassNameFilter.new([], [], [], ["E"])
    filter.instance_variable_set(:@classNameSet, {"NameC" => true})
    assert_equal(expectedSimple, filter.excludeSimpleName?(name))
    assert_equal(expectedFull, filter.excludeFullname?(name))
  end

  def test_warn
    filter = SilentClassNameFilter.new(["*(*", "C.*", "*)*", "*(*", "A::B("])
    ["NameA", "NameB"].each do |name|
      assert_false(filter.excludeSimpleName?(name))
      assert_false(filter.excludeFullname?(name))
    end

    assert_equal("Invalid pattern *(*Invalid pattern *)*Invalid pattern A::B(", filter.message)
  end

  data(
    'lf' => "A0.",
    'var' => "A0.member",
    'function' => "A0.call(1)",
    'space 1' => " A0.call(1)",
    'space 2' => " A0 . call(1)")
  def test_parseLineVar(data)
    varNameSet = {}
    filter = ClassNameFilter.new([], [], [], [])
    filterSet = ["x", "[A-Z]\\d"]

    2.times do
      filter.parseLine(data, filterSet, varNameSet)
      assert_equal(1, varNameSet.size)
      assert_true(varNameSet.key?("A0"))
    end
  end

  def test_parseLineMultiple
    line = "+A0.member+A1.member+A2. member"
    varNameSet = {}
    filter = ClassNameFilter.new([], [], [], [])
    filterSet = ["x", "[A-Z]\\d"]

    filter.parseLine(line, filterSet, varNameSet)
    assert_equal(3, varNameSet.size)
    ["A0", "A1", "A2"].each do |var|
      assert_true(varNameSet.key?(var))
    end
  end

  def test_parseLineEmpty
    line = ".member"
    varNameSet = {}
    filter = ClassNameFilter.new([], [], [], [])
    filterSet = [""]

    filter.parseLine(line, filterSet, varNameSet)
    assert_true(varNameSet.empty?)
  end

  data(
    'prefix' => "AA0.member",
    'postfix' => "A00.member",
    'not matched' => "A.call(1)",
    'no member' => "A0")
  def test_parseLineNotMatched(data)
    varNameSet = {}
    filter = ClassNameFilter.new([], [], [], [])
    filterSet = ["x", "[A-Z]\\d"]

    filter.parseLine(data, filterSet, varNameSet)
    assert_true(varNameSet.empty?)
  end
end

class TestSymbolFilter < Test::Unit::TestCase
  def test_all
    definedReferenceSet = 1
    undefinedReferenceSet = "b"
    functionNameFilterSet = ["c", "d"]
    classNameFilterOutSet = ["e", "f"]
    fillVtable = "g"
    noOverloading = "h"

    filter = SymbolFilter.new(definedReferenceSet, undefinedReferenceSet, functionNameFilterSet,
                              classNameFilterOutSet, fillVtable, noOverloading)
    assert_equal(definedReferenceSet, filter.definedReferenceSet)
    assert_equal(undefinedReferenceSet, filter.undefinedReferenceSet)
    assert_equal(functionNameFilterSet, filter.functionNameFilterSet)
    assert_equal(classNameFilterOutSet, filter.classNameFilterOutSet)
    assert_equal(fillVtable, filter.fillVtable)
    assert_equal(noOverloading, filter.noOverloading)
  end
end

class MinimumSymbolFilter < SymbolFilter
  def initialize(definedReferenceSet, undefinedReferenceSet)
    super(definedReferenceSet, undefinedReferenceSet, [], [], false, false)
  end

  def setNoOverloading
    @noOverloading = true
  end
end

class MinimumVtableFilter < SymbolFilter
  def initialize(definedReferenceSet, undefinedReferenceSet, fillVtable)
    super(definedReferenceSet, undefinedReferenceSet, [], [], fillVtable, false)
  end
end

class TestBaseBlock < Test::Unit::TestCase
  data(
    'word' => "Varname",
    'sentence' => "Type Varname")
  def test_initialize(data)
    line = data
    block = BaseBlock.new(line)
    assert_false(block.skippingParse)
    assert_equal(line, block.instance_variable_get(:@line))
    assert_nil(block.parent)
    assert_equal([], block.children)
    assert_equal({}, block.instance_variable_get(:@childMap))
    assert_equal({}, block.instance_variable_get(:@removedChildMap))
    assert_nil(block.keyForParent)
  end

  def test_connect
    parentBlock = BaseBlock.new("Parent")
    childBlock1 = BaseBlock.new("Child1")
    childBlock2 = BaseBlock.new("Child2")
    grandChildBlock = BaseBlock.new("grandChild")

    parentBlock.connect(childBlock1)
    assert_nil(parentBlock.parent)
    assert_equal([childBlock1], parentBlock.children)
    assert_equal(parentBlock, childBlock1.parent)
    assert_equal([], childBlock1.children)

    parentBlock.connect(childBlock2)
    assert_nil(parentBlock.parent)
    assert_equal([childBlock1, childBlock2], parentBlock.children)
    assert_equal(parentBlock, childBlock2.parent)
    assert_equal([], childBlock2.children)

    childBlock1.connect(grandChildBlock)
    assert_nil(parentBlock.parent)
    assert_equal(parentBlock, childBlock1.parent)
    assert_equal(childBlock1, grandChildBlock.parent)
    assert_equal([grandChildBlock], childBlock1.children)

    # Add it and remove it later if childBlock1 is already a parentBlock's child
    parentBlock.connect(childBlock1)
    assert_equal([childBlock1, childBlock2, childBlock1], parentBlock.children)
  end

  def test_disconnect
    parentBlock = BaseBlock.new("Parent")
    childBlock1 = BaseBlock.new("Child1")
    childBlock2 = BaseBlock.new("Child2")
    grandChildBlock = BaseBlock.new("grandChild")
    parentBlock.connect(childBlock1)
    parentBlock.connect(childBlock2)
    childBlock1.connect(grandChildBlock)

    parentBlock.disconnect(childBlock1)
    parentBlock.eraseChildren
    assert_nil(childBlock1.parent)
    assert_equal([childBlock2], parentBlock.children)

    parentBlock.disconnect(childBlock2)
    parentBlock.eraseChildren
    assert_nil(childBlock2.parent)
    assert_equal([], parentBlock.children)

    # Do nothing if unrelated blocks
    parentBlock.disconnect(grandChildBlock)
    parentBlock.eraseChildren
    assert_equal(childBlock1, grandChildBlock.parent)
    assert_equal([], parentBlock.children)
  end

  def test_attachTypedefBlock
    block = BaseBlock.new("Parent")
    typedefBlock = BaseBlock.new("")
    block.attachTypedefBlock(typedefBlock)
    assert_equal(typedefBlock, block.instance_variable_get(:@typedefBlock))
  end

  def test_collectAliases
    block = BaseBlock.new("")
    assert_true(block.collectAliases.aliasSet.empty?)
  end

  data(
    'nil' => [nil, nil],
    'not size_t' => ["size", "size"],
    'size_t' => ["size_t", "size_t"],
    'std::size_t' => ["std::size_t", "size_t"],
    'spaces' => [" std::size_t ", "size_t"],
    'not std::size_t' => ["std::size_tx", "std::size_tx"])
  def test_removeRedundantNamespace(data)
    name, expected = data
    assert_equal(expected, BaseBlock.new("").removeRedundantNamespace(name))
  end

  def test_resolveAlias
    block = BaseBlock.new("")
    name = "uint32_t"
    assert_equal(name, block.resolveAlias(name))
  end

  def test_resolveSizeAlias
    child = BaseBlock.new("")
    child.typeAliasSet.add("aliasName", "aliasSize")

    parent = BaseBlock.new("")
    parent.typeAliasSet.add("aliasSize", "mySize")
    parent.connect(child)

    grandParent = BaseBlock.new("")
    grandParent.typeAliasSet.add("mySize", "std::size_t")
    grandParent.connect(parent)

    [[child, "aliasName"], [parent, "aliasSize"], [grandParent, "mySize"]].each do |block, name|
      assert_equal("size_t", block.resolveAlias(name))
    end
  end

  def test_findType
    block = ExternVariableStatement.new("Type a;")
    found, canInitializeByZero = block.findType("int")
    assert_true(found)
    assert_true(canInitializeByZero)

    found, canInitializeByZero = block.findType("Type")
    assert_false(found)
    assert_false(canInitializeByZero)

    def block.resolveAlias(typeStr)
      return "long"
    end

    found, canInitializeByZero = block.findType("Type")
    assert_true(found)
    assert_true(canInitializeByZero)

    found, canInitializeByZero = block.findType("const Type")
    assert_true(found)
    assert_true(canInitializeByZero)

    def block.resolveAlias(typeStr)
      return "Alias"
    end

    found, canInitializeByZero = block.findType("Type")
    assert_true(found)
    assert_false(canInitializeByZero)
  end

  def test_setTypedef
    BaseBlock.new("").setTypedef("Name")
  end

  def test_canTraverse?
    assert_false(BaseBlock.new("").canTraverse?)
  end

  def test_canMock?
    child = BaseBlock.new("")
    assert_true(child.canMock?)

    parent = BaseBlock.new("")
    parent.connect(child)
    def parent.canMock?
      false
    end

    assert_false(parent.canMock?)
    assert_false(child.canMock?)
  end

  def test_isNamespace?
    assert_false(BaseBlock.new("").isNamespace?)
  end

  def test_isFreeFunction?
    assert_false(BaseBlock.new("").isFreeFunction?)
  end

  def test_isClass?
    assert_false(BaseBlock.new("").isClass?)
  end

  def test_isNonMemberInstanceOfClass?
    assert_false(BaseBlock.new("").isNonMemberInstanceOfClass?)
  end

  def test_getNamespace
    assert_equal("", BaseBlock.new("Name").getNamespace)
  end

  def test_getFullNamespace
    assert_equal("", BaseBlock.new("Name").getFullNamespace)
  end

  def test_getTypename
    assert_nil(BaseBlock.new("Name").getTypename)
  end

  def test_getInitializer
    assert_nil(BaseBlock.new("Name").getInitializer)
  end

  def test_collectAliasesInBlock
    assert_nil(BaseBlock.new("Name").collectAliasesInBlock(nil))
  end

  def test_parseChildren
    assert_nil(BaseBlock.new("").parseChildren("Line"))
  end

  def test_getStringToClassFile
    assert_nil(BaseBlock.new("").getStringToClassFile)
  end

  def test_getStringToDeclFile
    assert_nil(BaseBlock.new("").getStringToDeclFile)
  end

  def test_getStringSetToDeclInlineFile
    assert_nil(BaseBlock.new(["", ""]).getStringSetToDeclInlineFile)
  end

  def test_getStringToSwapperFile
    assert_nil(BaseBlock.new("").getStringToSwapperFile)
  end

  def test_getStringOfStub
    assert_nil(BaseBlock.new("").getStringOfStub)
  end

  def test_getStringOfVariableDefinition
    assert_nil(BaseBlock.new("").getStringOfVariableDefinition)
  end

  def test_filterByReferenceSet(referenceSet)
    assert_true(BaseBlock.new("").filterByReferenceSet(nil))
  end

  data(
    'notNamespace' => ["Name", "Name"],
    'topNamespace' => ["::Name", "::Name"],
    'relativeNestedNamespace' => ["::Name::A::B", "Name::A::B"],
    'topNestedNamespace' => ["::Name::A::B", "::Name::A::B"])
  def test_addTopNamespace(data)
    expected, name = data
    assert_equal(expected, BaseBlock.new("").addTopNamespace(name))
  end

  data(
    'notNamespace' => ["::Name", "Name"],
    'topNamespace' => ["::Name", "::Name"],
    'relativeNestedNamespace' => ["::Name::A::B", "Name::A::B"],
    'topNestedNamespace' => ["::Name::A::B", "::Name::A::B"])
  def test_getNameFromTopNamespace(data)
    expected, name = data
    assert_equal(expected, BaseBlock.new("").getNameFromTopNamespace(name))
  end

  def test_getNonTypedFullname
    name = "Symbol"
    nsBlockA = NamespaceBlock.new("namespace R::A")
    nsBlockB = NamespaceBlock.new("namespace B")
    nsBlockE = NamespaceBlock.new("namespace")  # unnamed namespace
    block = NamespaceBlock.new("")

    assert_equal("", block.getNonTypedFullname(""))
    assert_equal(name, block.getNonTypedFullname(name))

    nsBlockB.connect(block)
    assert_equal("::B::#{name}", block.getNonTypedFullname(name))

    nsBlockA.connect(nsBlockE)
    nsBlockE.connect(nsBlockB)
    assert_equal("::R::A::B::#{name}", block.getNonTypedFullname(name))
  end

  data(
    'empty' => [[], true],
    'all' => [[true, true], true],
    'none' => [[false, false], false],
    'not all' => [[true, false, true], false])
  def test_allOfParents?(data)
    valueSet, expected = data
    block = NamespaceBlock.new("")

    child = block
    valueSet.each do |value|
      parent = NamespaceBlock.new("")
      parent.instance_variable_set(:@value, value)
      def parent.check
        @value
      end

      parent.connect(child)
      child = parent
    end

    assert_equal(expected, block.allOfParents?(:check))
  end

  def test_setParent
    block = BaseBlock.new("")
    parent = BaseBlock.new("").setTypedef("Name")
    block.setParent(parent)
    assert_equal(parent, block.parent)
  end

  data(
    'nil' => [nil, nil],
    'empty' => ["", nil],
    'string' => ["name", "name"])
  def test_setKeyForParent(data)
    key, expected = data
    block = BaseBlock.new("")
    block.setKeyForParent(key)
    assert_equal(expected, block.keyForParent)
  end

  def test_addChild
    keyA = "A"
    blockA1 = BaseBlock.new("")
    blockA1.setKeyForParent(keyA)
    blockA2 = BaseBlock.new("")
    blockA2.setKeyForParent(keyA)

    parent = BaseBlock.new("")
    [blockA1, blockA2, blockA1].each do |block|
      parent.addChild(block)
      assert_equal([blockA1], parent.children)
      childMap = parent.instance_variable_get(:@childMap)
      assert_equal(1, childMap.size)
      assert_true(childMap.key?(keyA))
    end

    blockB1 = BaseBlock.new("")
    blockB2 = BaseBlock.new("")
    expected = 2
    [blockB1, blockB2, blockB1].each do |block|
      parent.addChild(block)
      assert_equal(expected, parent.children.size)
      expected += 1
      assert_equal(1, parent.instance_variable_get(:@childMap).size)
    end

    assert_equal([blockA1, blockB1, blockB2, blockB1], parent.children)
  end

  def test_removeAndEraseChildren
    blockA1 = BaseBlock.new("")
    blockA1.setKeyForParent("A")
    blockA2 = BaseBlock.new("")
    blockA2.setKeyForParent("B")
    blockA3 = BaseBlock.new("")
    blockA3.setKeyForParent("C")
    blockB1 = BaseBlock.new("")
    blockB2 = BaseBlock.new("")

    parent = BaseBlock.new("")
    [blockA1, blockA2, blockA3, blockA2, blockB1, blockB2, blockB2].each do |block|
      parent.addChild(block)
    end
    assert_equal(6, parent.children.size)
    assert_equal(3, parent.instance_variable_get(:@childMap).size)
    assert_true(parent.instance_variable_get(:@removedChildMap).empty?)

    parent.removeChild(blockA2)
    parent.removeChild(blockB2)
    assert_equal(6, parent.children.size)
    assert_equal(3, parent.instance_variable_get(:@childMap).size)
    assert_equal(2, parent.instance_variable_get(:@removedChildMap).size)

    parent.eraseChildren
    assert_equal(3, parent.children.size)
    assert_equal([blockA1, blockA3, blockB1], parent.children)
    assert_true(parent.instance_variable_get(:@removedChildMap).empty?)
  end
end

class TestRootBlock < Test::Unit::TestCase
  def test_canTraverse?
    assert_true(RootBlock.new("").canTraverse?)
  end
end

class TestNamespaceBlock < Test::Unit::TestCase
  data(
    'not namespace' => ["name {", "", ""],
    'unnamed namespace' => ["namespace {", "", ""],
    'simple namespace' => ["namespace A {", "A", "A"],
    'nested namespace' => ["namespace A::B {", "A::B", "::A::B"],
    'more nested namespace' => ["namespace Ab::Cd::e {", "Ab::Cd::e", "::Ab::Cd::e"])
  def test_initializeAndGetNamespace(data)
    line, expected, expectedFull = data
    block = NamespaceBlock.new(line)
    assert_true(block.isNamespace?)
    assert_equal(expected, block.getNamespace)
    assert_equal(expectedFull, block.getFullNamespace)
  end

  data(
    'unnamed namespace' => "",
    'simple namespace' => "A",
    'nested namespace' => "A::B")
  def test_canTraverse?(data)
    name = data
    assert_true(NamespaceBlock.new("namespace #{name} {").canTraverse?)
    assert_true(NamespaceBlock.new("namespace #{name} {").canMock?)
    assert_true(NamespaceBlock.new("namespace #{name} {").isValid?(name))
  end

  data(
    'C++ standard' => "std",
    'Boost C++ Libraries' => "boost",
    'Google Test' => "testing",
    'C++ internal 1' => "__cxx",
    'C++ internal 2' => "_CPP__")
  def test_cannotTraverse(data)
    name = data
    assert_false(NamespaceBlock.new("namespace #{name} {").canTraverse?)
    assert_false(NamespaceBlock.new("namespace #{name} {").canMock?)
    assert_false(NamespaceBlock.new("namespace #{name} {").isValid?(name))
  end

  def test_canMock?
    Mockgen::Constants::NAMESPACE_SKIPPED_SET.each do |name|
      block = NamespaceBlock.new("namespace #{name} {")
      subBlock = NamespaceBlock.new("namespace NameA {")
      block.connect(subBlock)
      assert_false(block.canMock?)
      assert_false(subBlock.canMock?)
    end
  end
end

class TestExternCBlock < Test::Unit::TestCase
  def test_externC
    block = ExternCBlock.new('extern "C" {')
    assert_true(block.canTraverse?)
    assert_true(block.isNamespace?)
    assert_equal("", block.getNamespace())
  end

  def test_canMock?
    assert_false(ExternCBlock.new("").canMock?)
  end
end

class TestTypedefBlock < Test::Unit::TestCase
  data(
    'direct typedef' => ["typedef unsigned int UINT32", "using UINT32 = unsigned int", "unsigned int", "UINT32"],
    'indirect typedef' => ["typedef uint32_t UINT32", "using UINT32 = uint32_t", "uint32_t", "UINT32"],
    'pointer' => ["typedef uint32_t *PUINT32", "using PUINT32 = uint32_t*", "uint32_t *", "PUINT32"],
    'pointer to a function' => ["typedef void(*Func)(void)", "using Func = void(*)(void)", "void(*)(void)", "Func"],
    'reference to a function' => ["typedef void(&Func)(void)", "using Func = void(&)(void)", "void(&)(void)", "Func"],
    'funcptr and arguments' => ["typedef void(*Func)(int,long)", "using Func = void(*)(int,long)",
                                "void(*)(int,long)", "Func"],
    'funcptr and return type' => ["typedef const char*(*Func)(int,long)", "using Func = const char*(*)(int,long)",
                                  "const char *(*)(int,long)", "Func"])
  def test_initializeTypedef(data)
    line, lineUsing, expectedTypeSet, expectedAlias = data
    [TypedefBlock.new(line + ";"), UsingBlock.new(lineUsing + ";").getTypedefBlock].each do |block|
      assert_equal(expectedTypeSet, block.instance_variable_get(:@actualTypeStr))
      assert_equal(expectedAlias, block.instance_variable_get(:@typeAlias))
      assert_true(block.canTraverse?)
      assert_equal(expectedAlias, block.getTypename)
      assert_equal(expectedAlias, block.keyForParent)
    end

    assert_equal(expectedAlias, UsingBlock.new(lineUsing + ";").keyForParent)
  end

  def test_initializeNotTypedef
    block = TypedefBlock.new("namespace A")
    assert_false(block.canTraverse?)
    assert_nil(block.getTypename)
    assert_nil(block.keyForParent)
  end

  data(
    'one word' => ["UINT32", "uint32_t", "uint32_t"],
    'words' => ["UINT32", "unsigned int", "unsigned int"],
    'pointer' => ["UINT32", "unsigned int**", "unsigned int **"],
    'reference' => ["UINT32", "unsigned int&", "unsigned int &"],
    'pointer to a function' => ["Func", "void(*)(void)", "void(*)(void)"],
    'reference to a function' => ["Func", "void(&)(void)", "void(&)(void)"],
    'pointer to a function with spaces' => [" Func ", " void ( * ) ( void ) ", "void(*)(void)"],
    'pointer to a function with arguments' => ["Func", "void(*)(int, long)", "void(*)(int,long)"],
    'pointer to a function with return value' => ["Func", "const char*(*)(int, const int*)", "const char *(*)(int,const int *)"])
  def test_parseUsingDirective(data)
    aliasName, actualType, expectedType = data
    block = TypedefBlock.new(nil)
    block.parseUsingDirective(aliasName, actualType)
    assert_equal(aliasName.strip, block.instance_variable_get(:@typeAlias))
    assert_equal(expectedType, block.instance_variable_get(:@actualTypeStr))
  end

  def test_extract
    str = "^()*&abc^"
    expected = "()*&"
    assert_equal(expected, str.gsub(/[^\(\)\*&]/, ""))
    assert_equal(expected, str.tr("^()*&", ""))
  end

  data(
    'using' => ["void (*)(void)", 3],
    'typedef' => ["typedef void(*)(void)", 4],
    'typedef with s' => ["typedef strings(*)(strings)", 4])
  def test_splitPointerToFunction(data)
    line, minSize = data
    block = TypedefBlock.new(nil)
    phraseSet = block.splitPointerToFunction(line, minSize)
    assert_not_nil(phraseSet)
    assert_equal(minSize, phraseSet.size)
    assert_equal("(*)", phraseSet[-2][0])
  end

  data(
    '0' => "",
    '1-1' => "var",
    '1-2' => "(void)",
    '2-1' => "Func(void)",
    '2-2' => "(*)(void)",
    '3-1' => "Func(void)const",
    '3-2' => "Func * (void)")
  def test_notSplitPointerToFunction(data)
    line, minSize = data
    block = TypedefBlock.new(nil)
    phraseSet = block.splitPointerToFunction(line, 3)
    assert_nil(phraseSet)
  end

  data(
    'base' => [[["void",""],["(*)",""],["","void"]], "void(*)(void)"],
    'result' => [[["const char*",""],["(*)",""],["","void"]], "const char *(*)(void)"],
    'arguments' => [[["Type&",""],["(*)",""],["","int, const char*"]], "Type &(*)(int,const char *)"],
    'redundant spaces' => [[[" Type & ",""],["( * )",""],[""," int, const char * "]], "Type &(*)(int,const char *)"])
  def test_formatPointerToFunction(data)
    phraseSet, expected = data
    block = TypedefBlock.new(nil)
    assert_equal(expected, block.formatPointerToFunction(phraseSet))
  end

  data(
    'base' => ["void AliasType", "void"],
    'multi words 1' => ["const Type* AliasType", "const Type *"],
    'multi words 2' => ["const Type *AliasType", "const Type *"],
    'redundant word' => ["enum Type& AliasType", "Type &"],
    'redundant spaces' => [" const Type & AliasType ", "const Type &"])
  def test_parseType(data)
    line, expected = data
    block = TypedefBlock.new(nil)

    typeAlias, actual = block.parseType("typedef #{line}")
    assert_equal("AliasType", typeAlias)
    assert_equal(expected, actual)
  end

  data(
    'base' => [["void"], "void"],
    'multi' => [["const", "Type", "*"], "const Type *"],
    'redundant spaces' => [[" const ", " Type ", " * "], "const Type *"])
  def test_formatType(data)
    typeStrSet, expected = data
    block = TypedefBlock.new(nil)
    assert_equal(expected, block.formatType(typeStrSet))
  end

  def test_collectAliasesInBlock
    block = BaseBlock.new("Parent")
    blockIndirect = TypedefBlock.new("typedef uint32_t UINT32;")
    blockDirect = TypedefBlock.new("typedef unsigned int uint32_t;")

    typeAliasSet = blockIndirect.collectAliasesInBlock(TypeAliasSet.new)
    assert_equal("uint32_t", typeAliasSet.aliasSet["UINT32"])

    typeAliasSet = blockDirect.collectAliasesInBlock(TypeAliasSet.new)
    assert_equal("unsigned int", typeAliasSet.aliasSet["uint32_t"])
  end

  def test_setTypedefAndResolveAliasChain
    block = BaseBlock.new("Parent")
    typedefBlock = BaseBlock.new("")

    aliasName = "Name"
    typeName = "tagName"
    block.setTypedef(aliasName)

    typedefBlock = TypedefBlock.new("typedef struct #{typeName} {")
    block.attachTypedefBlock(typedefBlock)
    block.setTypedef(aliasName)
    assert_equal({aliasName => typeName}, block.collectAliasesInBlock(TypeAliasSet.new).aliasSet)
  end

  def test_collectAliases
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    block = BaseBlock.new("Parent")
    rootBlock.connect(block)
    typeName = "tagName"

    typedefBlock = TypedefBlock.new("typedef struct #{typeName} {")
    block.attachTypedefBlock(typedefBlock)

    blockDirect = TypedefBlock.new("typedef unsigned int uint32_t;")
    blockIndirect = TypedefBlock.new("typedef uint32_t UINT32;")
    block.connect(blockDirect)
    block.connect(blockIndirect)

    aliasName = "Name"
    block.setTypedef(aliasName)
    block.collectAliases
    assert_equal({"UINT32"=>"unsigned int", "uint32_t"=>"unsigned int"}, block.typeAliasSet.aliasSet)
    rootBlock.collectAliases

    assert_equal("unsigned int", block.resolveAlias("UINT32"))
    assert_equal("unsigned int", block.resolveAlias("uint32_t"))
    assert_equal("uint64_t", block.resolveAlias("uint64_t"))
    assert_equal(typeName, rootBlock.resolveAlias(aliasName))
  end

  def test_resolveAliasInHierarchy
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    rootTypedefBlockA = TypedefBlock.new("typedef int32 MyInt;")
    rootTypedefBlockB = TypedefBlock.new("typedef aStruct T;")
    rootBlock.connect(rootTypedefBlockA)
    rootBlock.connect(rootTypedefBlockB)

    ecBlock = ExternCBlock.new('extern "C" {')
    rootBlock.connect(ecBlock)
    ecTypedefBlock = TypedefBlock.new("typedef int64 MyInt;")
    ecBlock.connect(ecTypedefBlock)

    rootBlock.collectAliases
    ecBlock.collectAliases
    assert_equal("int32", rootBlock.resolveAlias("MyInt"))
    assert_equal("int64", ecBlock.resolveAlias("MyInt"))
    assert_equal("aStruct", rootBlock.resolveAlias("T"))
    assert_equal("aStruct", ecBlock.resolveAlias("T"))
    assert_equal("U", ecBlock.resolveAlias("U"))
  end
end

class TestUsingBlock < Test::Unit::TestCase
  data(
    'empty' => "",
    'namesapce' => "using namespace std;",
    'malformed' => "using = int;",
    'template' => "template <typename T> using Array = typename std::vector<T>;")
  def test_initializeTypedef(data)
    assert_nil(UsingBlock.new(data).getTypedefBlock)
  end
end

class TestTypedVariable < Test::Unit::TestCase
  data(
    'primitive1' => ["int a", "int", "a"],
    'primitive2' => [" int a", "int", "a"],
    'primitive3' => [" int a ", "int", "a"],
    'pointer' => ["void* p", "void *", "p"],
    'double pointer' => ["void **p", "void * *", "p"],
    'reference' => ["void& a", "void &", "a"],
    'reference to pointer' => ["void*&p", "void * &", "p"],
    'default argument1' => ["void*&p=nullptr", "void * &", "p"],
    'default argument2' => ["int a=(bool)(0)", "int", "a"],
    'decltype' => ["decltype(g) a", "decltype(g)", "a"]
  )
  def test_parseArgSet(data)
    line, expectedArgType, expectedArgName = data
    argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
    assert_equal(line, newArgStr)
  end

  data(
    'premitive' => ["int", "int", "int dummy2"],
    'pointer' => ["void*", "void *", "void* dummy2"],
    'reference' => ["int&", "int &", "int& dummy2"],
    'pointer to const' => ["const void *", "const void *", "const void * dummy2"],
    'reference to const' => ["const T &", "const T &", "const T & dummy2"])
  def test_parseArgSetWithoutPtrRefName(data)
    line, expectedArgType, expectedArgStr = data
    argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(2)
    assert_equal(expectedArgType, argType)
    assert_equal("dummy2", argName)
    assert_equal(expectedArgStr, newArgStr)
  end

  data(
    'array' => ["int a[]", "int[]", "a", "int a[]"],
    'array with size' => ["int a[10]", "int[10]", "a", "int a[10]"],
    'array without varname' => ["int[]", "int[]", "dummy3", "int dummy3[]"],
    'array with size1' => ["int[10]", "int[10]", "dummy3", "int dummy3[10]"],
    'array with size2' => ["T[10]", "T[10]", "dummy3", "T dummy3[10]"])
  def test_parseArgSetArray(data)
    line, expectedArgType, expectedArgName, expectedArgStr = data
    argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(3)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
    assert_equal(expectedArgStr, newArgStr)
  end

  def test_parseArgSetWithoutVariableName
    ["bool", "char", "short", "int", "long", "float", "double",
     "unsigned", "size_t", "ssize_t", "uintptr_t", "intptr_t", "ptrdiff_t",
     "int8_t", "int16_t", "int32_t", "int64_t",
     "uint8_t", "uint16_t", "uint32_t", "uint64_t"].each do |word|
      line = word
      argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(4)
      assert_equal(word, argType)
      assert_equal("dummy4", argName)
      assert_equal("#{word} dummy4", newArgStr)

      line = "const " + word
      argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(5)
      assert_equal(line, argType)
      assert_equal("dummy5", argName)
      assert_equal("#{line} dummy5", newArgStr)
    end
  end

  data(
    'empty' => ["int a", "int", "a", "int a"],
    'array' => ["int a[1]", "int", "a[1]", "int a[1]"],
    'pointer' => ["const void* p", "const void *", "p", "const void* p"])
  def test_parseAsMemberVariable(data)
    line, expectedArgType, expectedArgName, expectedArgStr = data
    argType, argName = TypedVariable.new(line).parseAsMemberVariable
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
  end

  data(
    'pointer to a function' => ["int(*func)()", "int (*) ()", "func", "int (*func) ()"],
    'pointer to a member function' => ["int(C::func*)()", "int (C::*) ()", "func", "int (C::func*) ()"])
  def test_parseFuncPtr(data)
    line, expectedArgType, expectedArgName, expectedArgStr = data
    argType, argName = TypedVariable.new(line).parse(1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
  end

  data(
    'empty' => ["", "", nil],
    'primitive' => ["a", "a", nil],
    'array' => ["a[]", "a", "[]"],
    'array size' => ["a[1]", "a", "[1]"])
  def test_splitArrayBlock(data)
    line, expectedName, expectedArrayBlock = data
    name, arrayBlock = TypedVariable.new(line).splitArrayBlock(line)
    assert_equal(expectedName, name)
    assert_equal(expectedArrayBlock, arrayBlock)
  end

  data(
    'empty' => ["int()()", "int () ()", "dummy1", "int (dummy1) ()"],
    'no varname' => ["int(*)()", "int (*) ()", "dummy1", "int (*dummy1) ()"],
    'varname' => ["int(f)()", "int () ()", "f", "int (f) ()"],
    'one word' => ["int(*f)()", "int (*) ()", "f", "int (*f) ()"],
    'default var' => ["int(*f)() = callback", "int (*) ()", "f", "int (*f) () = callback"],
    'multi words' => ["const void* (*f)()", "const void* (*) ()", "f", "const void* (*f) ()"],
    'member function' => ["void(ClassName::*f)(int)", "void (ClassName::*) (int)", "f", "void (ClassName::*f) (int)"],
    'member without name' => ["void(ClassName::*)(int)", "void (ClassName::*) (int)", "dummy1", "void (ClassName::*dummy1) (int)"])
  def test_parseArgSetFuncPtr(data)
    line, expectedArgType, expectedArgName, expectedNewArgStr = data
    argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
    assert_equal(expectedNewArgStr, newArgStr)
  end

  data(
    'enum' => ["Name::Enum e = value", "Name::Enum", "e", "Name::Enum e = Name::value"],
    'funcPtr' => ["Name::Result(*f)() = callback", "Name::Result (*) ()", "f", "Name::Result (*f) () = callback"])
  def test_parseNamespace(data)
    line, expectedArgType, expectedArgName, expectedArgStr = data
    argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
    assert_equal(expectedArgStr, newArgStr)
  end

  data(
    'empty' => ["", "", "dummy1", "dummy1"],
    'ptr' => ["*", "*", "dummy1", "*dummy1"],
    'ref' => ["&", "&", "dummy1", "&dummy1"],
    'name' => ["f", "", "f", "f"],
    'ptrName1' => ["*f", "*", "f", "*f"],
    'ptrName2' => ["f*", "*", "f", "*f"],
    'refName1' => ["&f", "&", "f", "&f"],
    'refName2' => ["f&", "&", "f", "&f"],
    'memFuncPtr 1-1' => ["ClassName::f*", "ClassName::*", "f", "ClassName::*f"],
    'memFuncPtr 1-2' => ["ClassName::*f", "ClassName::*", "f", "ClassName::*f"],
    'memFuncPtr 1-3' => ["ClassName:: *f", "ClassName::*", "f", "ClassName::*f"],
    'memFuncPtr 1-4' => ["*ClassName::f", "ClassName::*", "f", "ClassName::*f"],
    'memFuncPtr 2-1' => ["ClassName::*", "ClassName::*", "dummy1", "ClassName::*dummy1"],
    'memFuncPtr 2-2' => ["ClassName::*", "ClassName::*", "dummy1", "ClassName::*dummy1"],
    'memFuncPtr 2-3' => ["*ClassName::", "ClassName::*", "dummy1", "ClassName::*dummy1"],
    'memFuncRef 1-1' => ["ClassName::f&", "ClassName::&", "f", "ClassName::&f"],
    'memFuncRef 1-2' => ["ClassName::&f", "ClassName::&", "f", "ClassName::&f"],
    'memFuncRef 1-3' => ["ClassName::& f", "ClassName::&", "f", "ClassName::&f"],
    'memFuncRef 1-4' => ["&ClassName::f", "ClassName::&", "f", "ClassName::&f"],
    'memFuncRef 2-1' => ["ClassName::&", "ClassName::&", "dummy1", "ClassName::&dummy1"],
    'memFuncRef 2-2' => ["ClassName::&", "ClassName::&", "dummy1", "ClassName::&dummy1"],
    'memFuncRef 2-3' => ["&ClassName::", "ClassName::&", "dummy1", "ClassName::&dummy1"])
  def test_extractFuncPtrName(data)
    line, expectedArgType, expectedName, expectedArgStr = data
    argType, name, argStr = TypedVariable.new(nil).extractFuncPtrName(line, 1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedName, name)
    assert_equal(expectedArgStr, argStr)
  end

  data(
    'empty' => [nil, "var", "var"],
    'no namespace' => [nil, "var = 1", "var = 1"],
    'no default value' => ["Type::Enum", "e", "e"],
    'with namespace' => ["Type::Enum", "e = first", "e = Type::first"],
    'nested namespace' => ["ClassName::Type::Enum", "e = first", "e = ClassName::Type::first"])
  def test_addNamespaceToDefaultVariable(data)
    namespaceStr, varName, expected = data
    actual = TypedVariable.new(nil).addNamespaceToDefaultVariable(namespaceStr, varName)
    assert_equal(expected, actual)
  end
end

class TestArgVariableSet < Test::Unit::TestCase
  data(
    'not replaced' => ["Func()", "Func()"],
    'replaced1' => ["Func(const void* p = __null)", "Func(const void* p = NULL)"],
    'replaced2' => ["Func(const void* p = <null expr>)", "Func(const void* p = NULL)"])
  def test_replaceNullExpression(data)
    line, expected = data
    assert_equal(expected, ArgVariableSet.new(nil).replaceNullExpression(line))
  end

  data(
    'ConstructorNoArgs' => ["Func()", "", "", "Func", false, 0,
                            "", "", "", ""],
    'ConstructorOneArg' => ["Func(int a)", "", "", "Func", false, 1,
                            "int a", "int a", "int", "a"],
    'ConstructorTwoArgs' => ["Func (int a,long b)", "", "", "Func", false, 2,
                             "int a,long b", "int a,long b", "int,long", "a,b"],
    'FuncTwoArgs' => ["int Func (int a,long b)", "int", "", "Func", false, 2,
                      "int a,long b", "int a,long b", "int,long", "a,b"],
    'FuncTwoTypes' => ["void Func (int,long)", "void", "", "Func", false, 2,
                       "int dummy1,long dummy2", "int dummy1,long dummy2", "int,long", "dummy1,dummy2"],
    'FuncTwoArgsPostfix' => ["void* Func (int a) const override", "void *", "const override", "Func",
                             false, 1, "int a", "int a", "int", "a"],
    'FuncPtr1' => ["int Func (int(f)(int)) const", "int", "const", "Func", false, 1,
                   "int (f) (int)", "int (f) (int)", "int () (int)", "f"],
    'FuncPtr2' => ["int Func(long a,int(*f)(int))const", "int", "const", "Func", false, 2,
                   "long a,int (*f) (int)", "long a,int (*f) (int)", "long,int (*) (int)", "a,f"],
    'FuncPtr3' => ["int Func (int(*f)(int),void(*g)(void*)) const", "int", "const", "Func", false, 2,
                   "int (*f) (int),void (*g) (void*)", "int (*f) (int),void (*g) (void*)", "int (*) (int),void (*) (void*)", "f,g"],
    'DefaultArgs' => ["int Func (int a=0,long b=(long)0)", "int", "", "Func", false, 2,
                      "int a=0,long b= (long) 0", "int a,long b", "int,long", "a,b"],
    'DefaultNullArgs1' => ["int Func (void* p=<null expr>)", "int", "", "Func", false, 1,
                           "void* p=NULL", "void* p", "void *", "p"],
    'DefaultNullArgs2' => ["int Func(long a,int(*f)(int)=aFunc)const", "int", "const", "Func", false, 2,
                           "long a,int (*f) (int) =aFunc", "long a,int (*f) (int)", "long,int (*) (int)", "a,f"],
    'Pure virtual' => ["int Func (int(f)(int)) const = 0", "int", "const", "Func", true, 1,
                   "int (f) (int)", "int (f) (int)", "int () (int)", "f"]
  )
  def test_splitByArgSet(data)
    line, expectedPre, expectedPost, expectedFuncname, expectedPureVirtual, expectedArity,
    expectedSet, expectedSetWithoutDefault, expectedType, expectedName = data

    argVariableSet = ArgVariableSet.new(line)
    assert_equal(expectedPre, argVariableSet.preFuncSet)
    assert_equal(expectedPost, argVariableSet.postFuncSet)
    assert_equal(expectedFuncname, argVariableSet.funcName)
    assert_equal(expectedPureVirtual, argVariableSet.pureVirtual)
    assert_equal(expectedArity, argVariableSet.arity)
    assert_equal(expectedSet, argVariableSet.argSetStr)
    assert_equal(expectedSetWithoutDefault, argVariableSet.argSetWithoutDefault)
    assert_equal(expectedType, argVariableSet.argTypeStr)
    assert_equal(expectedName, argVariableSet.argNameStr)
  end

  data(
    'no space' => ["int Func (void)=0", ""],
    'one space' => ["int Func (void) =0", ""],
    'two spaces' => ["int Func (void) = 0", ""],
    'const' => ["int Func (void)const=0", "const"])
  def test_pureVirtual(data)
    line, expectedPost = data
    argVariableSet = ArgVariableSet.new(line)
    assert_true(argVariableSet.pureVirtual)
    assert_equal(expectedPost, argVariableSet.postFuncSet)
  end

  data(
    'no args' => ["", "", "", 0],
    'void arg' => ["void", "", "", 0],
    'primitive' => ["int arg", "int", "arg", 1],
    'array1' => ["T[] array", "T[]", "array", 1],
    'array2' => ["T[2] array", "T[2]", "array", 1],
    'reference1' => ["int& arg", "int &", "arg", 1],
    'reference2' => ["int & arg", "int &", "arg", 1],
    'pointer1' => ["void* arg", "void *", "arg", 1],
    'pointer2' => ["void * arg", "void *", "arg", 1],
    'pointer double' => ["void ** arg", "void * *", "arg", 1],
    'pointer and const' => ["const void * const arg", "const void * const", "arg", 1],
    'pointer and reference' => ["void*& arg", "void * &", "arg", 1],
    'multiple args' => ["int a,long b", "int,long", "a,b", 2],
    'mixed args' => ["int a,long b,const void* p", "int,long,const void *", "a,b,p", 3])
  def test_extractArgSet(data)
    line, expectedArgTypeSet, expectedArgNameSet, expectedArity = data
    argSetStr, argSetWithoutDefault, argTypeSet, argNameSet, arity = ArgVariableSet.new(nil).extractArgSet(" #{line} ")

    expected = (line == "void") ? "" : line
    assert_equal(expected, argSetStr)
    assert_equal(expected, argSetWithoutDefault)
    assert_equal(expectedArgTypeSet, argTypeSet)
    assert_equal(expectedArgNameSet, argNameSet)
    assert_equal(expectedArity, arity)
  end

  data(
    'pointer to a function' => ["int a,T(*f)(int, long)=FuncPtr,int b",
                                "int a,T (*f) (int, long) =FuncPtr,int b",
                                "int a,T (*f) (int, long),int b",
                                "int,T (*) (int, long),int", "a,f,b", 3],
    'pointer to a member function' => ["int a,T(C::f*)(int, long)=FuncPtr,int b",
                                       "int a,T (C::*f) (int, long) =FuncPtr,int b",
                                       "int a,T (C::*f) (int, long),int b",
                                       "int,T (C::*) (int, long),int", "a,f,b", 3])
  def test_extractFuncPtrArgSet(data)
    line, expected, expectedWithoutDefault, expectedArgTypeSet, expectedArgNameSet, expectedArity = data
    argSetStr, argSetWithoutDefault, argTypeSet, argNameSet, arity = ArgVariableSet.new(nil).extractArgSet(" #{line} ")

    assert_equal(expected, argSetStr)
    assert_equal(expectedWithoutDefault, argSetWithoutDefault)
    assert_equal(expectedArgTypeSet, argTypeSet)
    assert_equal(expectedArgNameSet, argNameSet)
    assert_equal(expectedArity, arity)
  end

  data(
    'primitive' => ["int", "int dummy1", "int", "dummy1"],
    'pointer' => ["void*", "void* dummy1", "void *", "dummy1"],
    'array1' => ["T[]", "T dummy1[]", "T[]", "dummy1"],
    'array2' => ["T[10]", "T dummy1[10]", "T[10]", "dummy1"],
    'two args' => ["void*,int", "void* dummy1,int dummy2", "void *,int", "dummy1,dummy2"])
  def test_extractArgSetVariableName(data)
    line, expectedArgStrSet, expectedArgTypeSet, expectedArgNameSet = data
    argSetStr, argSetWithoutDefault, argTypeSet, argNameSet = ArgVariableSet.new(nil).extractArgSet(" #{line} ")

    assert_equal(expectedArgStrSet, argSetStr)
    assert_equal(expectedArgStrSet, argSetWithoutDefault)
    assert_equal(expectedArgTypeSet, argTypeSet)
    assert_equal(expectedArgNameSet, argNameSet)
  end
end

class TestTemplateParameter < Test::Unit::TestCase
  data(
    'template class' => ['template <typename T> class Name',
                         "typename T", nil, ["typename"], "T", "class Name", ""],
    'default type' => ['template <typename T = int> class Name',
                       "typename T", nil, ["typename"], "T", "class Name", ""],
    'variadic template class1' => ['template <typename... Ts> class Name',
                                   "typename... Ts", nil, ["typename"], "Ts...", "class Name", ""],
    'variadic template class2' => ['template <typename ...Ts> class Name',
                                   "typename ...Ts", nil, ["typename"], "Ts...", "class Name", ""],
    'full specialization class' => ['template <> class Name<int>', "", "int", [], "", "class Name", ""],
    'partial specialization class' => ['template <typename T, size_t I> class Name<T,8>',
                                       "typename T, size_t I", "T,8", ["typename", "size_t"], "T,I", "class Name", ""],
    'template function' => ['template <typename T> const T* func',
                            "typename T", nil, ["typename"], "T", "const T* func", ""],
    'variadic template function' => ['template <typename... Ts> const T* func',
                                     "typename... Ts", nil, ["typename"], "Ts...", "const T* func", ""],
    'specialization function' => ['template <> const T* func<int>()',
                                  "", "int", [], "", "const T* func", "()"])
  def test_parse(data)
     line, generic, specialized, typenameSet, varSet, type, post = data

     param = TemplateParameter.new(line)
     assert_equal(generic, param.generic)
     assert_equal(specialized, param.specialized)
     assert_equal(typenameSet, param.typenameSet)
     assert_equal(varSet, param.varSet)
     assert_equal(type, param.type)
     assert_equal(post, param.post)
  end
end

class TestExternVariableStatement < Test::Unit::TestCase
  data(
    'primitive' => ['Type a;', "Type", "a"],
    'pointer1'  => ['class Type *pA;',  "Type", "pA"],
    'pointer2'  => ['struct Type * pA;', "Type", "pA"],
    'reference1' => ['static Type &a;',  "Type", "a"],
    'reference2' => ['const Type & a;', "Type", "a"],
    'decltype' => ['decltype(A) a;', "decltype(A)", "a"],
    'initializer 1' => ['Type a = 1;', "Type", "a"],
    'initializer 2' => ['Type a = (1 << 3);', "Type", "a"],
    'initializer 3' => ['Type a = 1 + (1 << 3);', "Type", "a"])
  def test_canTraverse?(data)
     line, type, var = data
     block = ExternVariableStatement.new("extern #{line}")
     assert_equal(var, block.varName)
     assert_equal(type, block.className)
     assert_equal("", block.arrayStr)
     assert_true(block.canTraverse?)
     assert_true(block.isNonMemberInstanceOfClass?)
     assert_equal(var, block.getFullname)
     assert_equal(var, block.keyForParent)
  end

  # definitions may be in the form var = (value) so treat lines
  # as functions if possible before parse them as variables
  data(
    'class name' => 'class BigInt',
    'system variable' => 'struct sys __sys')
  def test_cannotTraverseNonVariable(data)
    line = data
    block = ExternVariableStatement.new("extern #{line}")
    assert_false(block.canTraverse?)
    assert_false(block.isNonMemberInstanceOfClass?)
    assert_nil(block.keyForParent)
  end

  data(
    'using' => 'using Count = int',
    'typedef' => 'typedef int Count')
  def test_cannotTraverseTypeAlias(data)
    line = data
    block = ExternVariableStatement.new("#{line}")
    assert_false(block.canTraverse?)
    assert_false(block.isNonMemberInstanceOfClass?)
    assert_nil(block.keyForParent)
  end

  def test_filterByReferenceSet
    varBlockA = ExternVariableStatement.new("extern int a;")
    varBlockB = ExternVariableStatement.new("extern int b;")
    arrayBlock = ExternVariableStatement.new("extern int a[10];")

    referenceClass = Struct.new(:memberName)
    reference = referenceClass.new("a")
    nilReference = referenceClass.new(nil)

    assert_true(varBlockA.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(reference))))
    assert_true(arrayBlock.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(reference))))
    assert_false(varBlockA.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(nilReference))))
    assert_false(varBlockB.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(reference))))

    refSet = TestMockRefSetClass.new(true, [reference, referenceClass.new("b")])
    assert_true(varBlockA.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet)))
    assert_true(varBlockB.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet)))
  end

  def test_makeStubDef
    block = ClassBlock.new("class Name {")
    varBlock = ExternVariableStatement.new("int a;")
    block.connect(varBlock)
    assert_equal("int Name::a;\n", varBlock.makeStubDef(""))

    varBlock2 = ExternVariableStatement.new("LocalType a;")
    block.connect(varBlock2)
    assert_equal("LocalType Name::a;\n", varBlock2.makeStubDef("Name"))

    [nil, {"OuterType" => true}].each do |table|
      assert_equal("LocalType Name::a;\n", varBlock2.makeStubDefWithLocalType("Name", table))
    end

    assert_equal("Name::LocalType Name::a;\n", varBlock2.makeStubDefWithLocalType("Name", {"LocalType" => BaseBlock.new("")}))
  end

  def test_makeStubDefWithInitializer
    block = ClassBlock.new("class Name {")
    varBlock = ExternVariableStatement.new("static int * const pInt;")
    block.connect(varBlock)
    assert_equal("int * const Name::pInt = 0;\n", varBlock.makeStubDef(""))

    varBlock = ExternVariableStatement.new("static const int * pIntValue;")
    block.connect(varBlock)
    assert_equal("const int * Name::pIntValue;\n", varBlock.makeStubDef(""))

    block = ClassBlock.new("class Name {")
    varBlock = ExternVariableStatement.new("static EnumType e;")
    block.connect(varBlock)

    child = BaseBlock.new("")
    def child.getInitializer
      "INIT"
    end

    table = {"EnumType" => child}
    tableWithName = {"EnumType" => child, "Name::EnumType" => child}
    assert_equal("Name::EnumType Name::e = INIT;\n", varBlock.makeStubDefWithLocalType("Name", table))
    assert_equal("Name::EnumType Name::e = INIT;\n", varBlock.makeStubDefWithLocalType("Name", tableWithName))
  end

  def test_makeStubDefWithClassName
    block = ClassBlock.new("class Name {")
    varBlock = ExternVariableStatement.new("static Name::EnumType e;")
    block.connect(varBlock)

    child = BaseBlock.new("")
    def child.getInitializer
      "INIT"
    end

    tableWithName = {"Name::EnumType" => child}
    assert_equal("Name::EnumType Name::e = INIT;\n", varBlock.makeStubDefWithLocalType("Name", tableWithName))
  end

  data(
    'blank' => ['Type a[];', "Type", "a[]", "[]"],
    'num' => ['Type a [ 1 ];', "Type", "a[ 1 ]", "[ 1 ]"],
    'pointer'  => ['class Type *pA[ size ];',  "Type", "pA[ size ]", "[ size ]"])
  def test_parseArray(data)
     line, type, var, arrayStr = data
     block = ExternVariableStatement.new("extern #{line}")
     assert_true(block.canTraverse?)
     assert_true(block.isNonMemberInstanceOfClass?)
     assert_equal(type, block.className)
     assert_equal(var, block.getFullname)
     assert_equal(arrayStr, block.arrayStr)
  end

  # getFullname() is tested in the base class.
end

class TestMemberVariableStatement < Test::Unit::TestCase
  data(
    'primitive' => ['static Type a;', "Type", "a"],
    'initializer 1' => ['static Type a = 1;', "Type", "a"],
    'initializer 2' => ['static Type a = (1 << 3);', "Type", "a"],
    'initializer 3' => ['static Type a = 1 + (1 << 3);', "Type", "a"])
  def test_canTraverse?(data)
     line, type, var = data
     block = MemberVariableStatement.new("#{line}")
     assert_true(block.canTraverse?)
     assert_false(block.isNonMemberInstanceOfClass?)
     assert_equal(type, block.className)
     assert_equal(var, block.getFullname)
  end
end

class TestResolvedArgTypeSetStr < Test::Unit::TestCase
  data(
    'empty' => ["", ""],
    'one arg' => ["const C*", "char const *"],
    'multiple args' => ["volatile V*, const C*", "void volatile * , char const *"],
    '[] as *' => ["const C[]", "char const *"],
    '[n] as *' => ["const C[100]", "char const *"],
    'multiple []s' => ["volatile V[], const C[]", "void volatile * , char const *"])
  def test_sortArgTypeSetStr(data)
    arg, expected = data
    block = BaseBlock.new("")
    def block.resolveAlias(str)
      str.gsub("C","char").gsub("V","void")
    end

    assert_equal(expected, ResolvedArgTypeSetStr.new(block, arg).str)
  end

  data(
    'empty' => ["", ""],
    'primitive' => ["int", "int"],
    'a pointer to a const object' => ["const char*", "char const *"],
    'a const pointer to an object' => ["char* const", "char * const"],
    'double pointer' => ["const char** const", "char const * * const"],
    'reference' => ["const char*&", "char const * &"],
    'multiple args' => ["int,const char*&,T const &", "int , char const * & , T const &"])
  def test_sortArgTypeStr(data)
    arg, expected = data
    block = BaseBlock.new("")
    assert_equal(expected, ResolvedArgTypeSetStr.new(block, "").sortArgTypeStr(arg))
  end
end

class TestFunctionReferenceSet < Test::Unit::TestCase
  data(
    'all nils' => [nil, nil, nil, nil, nil, nil],
    'unset' => [nil, nil, nil, "", "", ""],
    'empty' => ["", "", "", "", "", ""],
    'no args' => ["nameA", "", "", "nameA", "", ""],
    'const function' => ["nameA", "", "const", "nameA", "", "const"],
    'primitive' => ["nameA", "int", "", "nameA", "int", ""],
    'pointer' => ["nameA", "char const *", "", "nameA", "const char*", ""],
    'multiple args' => ["nameA", "char * const, T&", "", "nameA", "char* const, T &", ""])
  def test_compareTrue(data)
    refName, refArgTypeStr, refPostFunc, name, argTypeStr, postFunc = data
    referenceClass = Struct.new(:fullname, :memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refName, refArgTypeStr, refPostFunc)
    block = BaseBlock.new("")
    assert_true(FunctionReferenceSet.new(block, reference, name, name, argTypeStr, postFunc, false).compare())
  end

  data(
    'explicit top level' => "::",
    'implicit top level' => "")
  def test_compareExternCtrue(data)
    prefix = data
    name = "nameA"
    fullname = "A::" + name
    othername = "A::B::" + name
    referenceClass = Struct.new(:fullname, :memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(prefix + fullname, name, nil, nil)
    block = BaseBlock.new("")

    assert_true(FunctionReferenceSet.new(block, reference, fullname, name, "", "", false).compare())
    assert_true(FunctionReferenceSet.new(block, reference, fullname, name, "int", "", false).compare())
    assert_true(FunctionReferenceSet.new(block, reference, "::" + fullname, name, "int", "", false).compare())
    assert_false(FunctionReferenceSet.new(block, reference, name, name, "", "", false).compare())
    assert_false(FunctionReferenceSet.new(block, reference, othername, name, "", "", false).compare())
  end

  data(
    'no args' => ["", "", "", true],
    'exact types' => ["", "int", "int", true],
    'arg in reference' => ["", "int", "", true],
    'arg in declaration' => ["", "", "int", true],
    'different types' => ["", "long long", "int", true],
    'different names' => ["B", "", "", false])
  def test_compareWithoutArgumentTypes(data)
    name = "nameA"
    refNamePostfix, refArgTypeStr, argTypeStr, expected = data
    refName = name + refNamePostfix
    referenceClass = Struct.new(:fullname, :memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refName, refArgTypeStr, "")
    block = BaseBlock.new("")

    assert_equal(expected, FunctionReferenceSet.new(block, reference, name, name, argTypeStr, "", true).compare())
  end

  data(
    'name' => ["nameA", "", "", "nameB", "", ""],
    'pointer' => ["nameA", "int", "", "nameA", "int*", ""],
    'arguments' => ["nameA", "int", "", "nameA", "int,int", ""],
    'argument constness' => ["nameA", "char const *", "", "nameA", "char * const", ""],
    'function constness' => ["nameA", "char const *", "const", "nameA", "char * const", ""])
  def test_compareFalse(data)
    refName, refArgTypeStr, refPostFunc, name, argTypeStr, postFunc = data
    referenceClass = Struct.new(:fullname, :memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refName, refArgTypeStr, refPostFunc)
    block = BaseBlock.new("")

    assert_false(FunctionReferenceSet.new(block, reference, name, name, argTypeStr, postFunc, false).compare())
    assert_equal(refName == name, FunctionReferenceSet.new(block, reference, name, name, argTypeStr, postFunc, true).compare())
  end

  data(
    'not alias' => ["nameA", "int", "int"],
    'primitive' => ["nameA", "unsigned int", "uint32_t"],
    'pointer' => ["nameA", "unsigned int*", "puint32_t"])
  def test_compareTypeAlias(data)
    refName, refArgTypeStr, argTypeStr = data
    referenceClass = Struct.new(:fullname, :memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refName, refArgTypeStr, "")
    block = BaseBlock.new("")

    typedefBlockI = TypedefBlock.new("typedef unsigned int uint32_t;")
    typedefBlockP = TypedefBlock.new("typedef unsigned int* puint32_t;")
    block.connect(typedefBlockI)
    block.connect(typedefBlockP)
    block.collectAliases
    assert_true(FunctionReferenceSet.new(block, reference, refName, refName, argTypeStr, "", false).compare())
  end

  def test_sortArgTypeSetStr
    arg = "volatile V[], const C[]"
    expected = "void volatile * , char const *"
    block = BaseBlock.new("")
    def block.resolveAlias(str)
      str.gsub("C","char").gsub("V","void")
    end

    assert_equal(expected, FunctionReferenceSet.new(block, nil, "", "", "", "", false).sortArgTypeSetStr(arg))
  end

  data(
    'empty' => ["", ""],
    'const' => ["const", "const"],
    'override' => ["override", ""],
    'const override' => ["const override", "const"])
  def test_postFunctionPhrase(data)
    arg, expected = data
    block = BaseBlock.new("")
    assert_equal(expected, FunctionReferenceSet.new(block, nil, "", "", "", "", false).postFunctionPhrase(arg))
  end

  def test_collectAliases
    blockParent = NamespaceBlock.new("A")
    blockChild = NamespaceBlock.new("B")
    blockParent.connect(blockChild)

    def blockParent.typeAliasSet
      1
    end

    def blockChild.typeAliasSet
      2
    end

    set = FunctionReferenceSet.new(blockChild, nil, "", "", "", "", false)
    assert_equal([2, 1], set.collectAliases)
  end
end

class TestConstructorBlock < Test::Unit::TestCase
  data(
    '1' => "=",
    '2' => " =",
    '3' => " = ",
    '4' => "= ")
  def test_parseDefaultConstructor(data)
    op = data
    className = "NameC"
    line = className + "()(void)#{op}default"
    block = ConstructorBlock.new(line, className)
    assert_true(block.canTraverse?)
    assert_equal(line, block.keyForParent)
  end

  data(
    'no args' =>
    ["NameC()", 0, "", "", "", "", "", ""],
    'one arg' =>
    ["NameC(long a)", 1, "long a", "long", ", long a", "", "a", "::A::NameC(a), "],
    'two args' =>
    ["NameC(long a, const void* p)", 2, "long a,const void* p", "long,const void *",
     ", long a,const void* p", "", "a,p", "::A::NameC(a,p), "],
    'initializer list' =>
    ["NameC(long a) : Base(a)", 1, "long a", "long", ", long a", "", "a", "::A::NameC(a), "],
    'template' =>
    ["NameC<T>(T* a) : Base(a)", 1, "T* a", "T *", ", T* a", "<T>", "a", "::A::NameC<T>(a), "])
  def test_initializeAndAll(data)
    line, expectArity, expectedTASet, expectedATStr, expectedArgsBC,
    expectedTypeStr, expectedArgSetStr, expectedCallBase = data
    name = "NameC"

    ["", "explicit ", " inline ", " explicit  inline ", "inline explicit "].each do |prefix|
      block = ConstructorBlock.new(prefix + line, name)
      valid, typedArgSet, typedArgSetWithoutDefault, argTypeStr, typeStr, argSetStr = block.parse(line, name)
      assert_true(valid)
      assert_true(block.canTraverse?)
      assert_equal(expectArity, block.arity)
      assert_equal(expectedTASet, typedArgSet)
      assert_equal(expectedTASet, typedArgSetWithoutDefault)
      assert_equal(expectedATStr, argTypeStr)
      assert_equal(expectedTypeStr, typeStr)
      assert_equal(expectedArgSetStr, argSetStr)
      assert_equal(expectedArgSetStr, block.argSetStr)
      assert_equal(expectedArgsBC, block.getTypedArgsForBaseClass)
      assert_equal(expectedArgsBC, block.getTypedArgsWithoutValue)
      assert_equal("", block.getCallForBaseClassInitializer)
      assert_equal(line, block.keyForParent)

      block.setBaseClassName("::A::NameC")
      className = "Dereived"
      initMember = "extra(0)"
      argStr = expectedTASet.empty? ? "void" : expectedTASet
      expected = "    #{className}(#{argStr}) : #{expectedCallBase}#{initMember} {}\n"
      assert_equal(expected, block.makeDef(className, "", initMember))

      expected = "    #{className}(void) : #{initMember} {}\n"
      assert_equal(expected, block.makeDefWithDefaultBaseConstructor(className, "", initMember))

      arg = "int extra"
      expectedArg = expectedTASet.empty? ? arg : "#{arg},#{expectedTASet}"
      expected = "    #{className}(#{expectedArg}) : #{expectedCallBase}#{initMember} {}\n"
      assert_equal(expected, block.makeDef(className, arg, initMember))

      expected = "    #{className}(#{arg}) : #{initMember} {}\n"
      assert_equal(expected, block.makeDefWithDefaultBaseConstructor(className, arg, initMember))
    end
  end

  data(
    'two args' =>
    ["NameC<T>(long a = 0, const void* p = void)",
     "long a = 0,const void* p = void",
     "long a,const void* p",
     "long,const void *",
     ", long a = 0,const void* p = void",
     ", long a,const void* p",
     "<T>", "a,p", "::A::NameC<T>(a,p), "])
  def test_initializeWithDefaultValue(data)
    line, expectedTypedArgSet, expectedTypedArgSetWithoutDefault, expectedArgTypeStr,
    expectedTypedArgsForBaseClass, expectedTypedArgsWithoutValue,
    expectedTypeStr, expectedArgSet, expectedCallBase = data

    name = "NameC"
    block = ConstructorBlock.new(line, name)
    valid, typedArgSet, typedArgSetWithoutDefault, argTypeStr, typeStr, argSet = block.parse(line, name)
    assert_true(valid)
    assert_equal(expectedTypedArgSet, typedArgSet)
    assert_equal(expectedTypedArgSetWithoutDefault, typedArgSetWithoutDefault)
    assert_equal(expectedArgTypeStr, argTypeStr)
    assert_equal(expectedTypeStr, typeStr)
    assert_equal(expectedArgSet, argSet)
    assert_equal(expectedTypedArgsForBaseClass, block.getTypedArgsForBaseClass)
    assert_equal(expectedTypedArgsWithoutValue, block.getTypedArgsWithoutValue)
    assert_equal("", block.getCallForBaseClassInitializer)
    assert_equal(line, block.keyForParent)

    block.setBaseClassName("::A::NameC")
    decoratorName = "Dereived"
    initMember = "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0)"
    expected = "    #{decoratorName}(#{expectedTypedArgSet}) : " +
               "#{expectedCallBase}#{initMember} {}\n"
    assert_equal(expected, block.makeDef(decoratorName, "", initMember))
  end

  def test_invalidConstructor
    block = ConstructorBlock.new("", "Name")
    assert_false(block.canTraverse?)
    assert_nil(block.keyForParent)
  end

  data(
    'no initializer' => "NameC(int a)",
    'with initializer list' => "NameC(int a) : Base(0), a_(a)")
  def test_removeInitializerList(data)
    line = data
    block = ConstructorBlock.new("", "NameC")
    assert_equal("NameC(int a)", block.removeInitializerList(line))
  end

  data(
    'no resolving' => [false, "long , Type"],
    'resolve' => [true, "long , int *"])
  def test_getResolvedArgTypeStr(data)
    resolving, expected = data
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    rootBlock.connect(TypedefBlock.new("typedef int * Type;"))

    className = "NameC"
    classBlock = ClassBlock.new("class #{className} {")
    rootBlock.connect(classBlock)

    block = ConstructorBlock.new("#{className}(long a, Type b)", className)
    classBlock.connect(block)
    rootBlock.collectAliases if resolving
    2.times { assert_equal(expected, block.getResolvedArgTypeStr(classBlock)) }
  end

  def test_filterByReferenceSet
    line = Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE
    line += " `NameC::NameC(int)'"
    reference = UndefinedReference.new(line)

    className = "NameC"
    block = ConstructorBlock.new("NameC(int);", className)
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(reference))))

    otherLine = Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE
    otherLine += " `NameC::Func()'"
    referenceOther = UndefinedReference.new(otherLine)
    refSet = TestMockRefSetClass.new(true, [referenceOther, reference])
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet)))

    block = ConstructorBlock.new("~NameC();", className)
    assert_false(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(reference))))
  end

  def test_filterByReferenceSetNoOverloading
    className = "NameC"
    line = Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE
    line += " `#{className}::#{className}(int)'"
    reference = UndefinedReference.new(line)

    classBlock = ClassBlock.new("class #{className} {")
    block = ConstructorBlock.new("#{className}();", className)
    classBlock.connect(block)

    filter = MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(reference))
    assert_false(block.filterByReferenceSet(filter))
    filter.setNoOverloading
    assert_true(block.filterByReferenceSet(filter))
  end

  def test_makeStubDefInClass
    className = "NameC"
    classBlock = ClassBlock.new("class #{className} {")
    def classBlock.getConstructorArgStrSet(arg)
      ": Base() "
    end

    block = ConstructorBlock.new("#{className}()", className)
    assert_equal("NameC::NameC() : Base() {}\n", block.makeStubDef(classBlock, className))

    classBlock.connect(block)
    assert_equal("::NameC::NameC() : Base() {}\n", block.makeStubDef(classBlock, className))
  end

  def test_makeStubDefInNamespace
    className = "NameC"
    block = ConstructorBlock.new("#{className}()", className)
    nsBlock = NamespaceBlock.new("namespace A")
    nsBlock.connect(block)

    classBlock = ClassBlock.new("class ClassName {")
    def classBlock.getConstructorArgStrSet(arg)
      ""
    end

    assert_equal("::A::NameC::NameC() {}\n", block.makeStubDef(classBlock, className))
  end

  def test_makeStubDefInNamespaceAndClass
    className = "NameC"
    block = ConstructorBlock.new("#{className}()", className)
    classBlock = ClassBlock.new("class #{className} {")
    classBlock.connect(block)
    def classBlock.getConstructorArgStrSet(arg)
      ": ::A::Base(0) "
    end

    nsBlock = NamespaceBlock.new("namespace A")
    nsBlock.connect(classBlock)
    assert_equal("::A::NameC::NameC() : ::A::Base(0) {}\n", block.makeStubDef(classBlock, className))
  end

  def test_makeDefaultConstructor
    name = "NameC"
    block = ConstructorBlock.new("", name)
    decoratorName = "Dereived"
    initMember = "#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0)"
    expected = "    #{decoratorName}(void) : #{initMember} {}\n"
    assert_equal(expected, block.makeDefWithDefaultBaseConstructor(decoratorName, "", initMember))
  end
end

class TestDestructorBlock < Test::Unit::TestCase
  data(
    'nil' => nil,
    'empty' => "~NameD()",
    'noexcept' => "~NameD() noexcept",
    'void' => "~NameD(void)",
    'space' => "~NameD(void)",
    'template 1' => "~NameD<int>(void)",
    'template 2' => "~NameD <int> (void)",
    'nested template 1' => "~NameD<std::vector<int>>(void)",
    'nested template 2' => "~NameD <std::vector < int > > (void)")
  def test_parseDestructor(data)
    line = data
    className = "NameD"
    block = DestructorBlock.new(line, className)
    assert_true(block.canTraverse?)
    assert_equal("~NameD", block.keyForParent)
  end

  data(
    '1' => "=",
    '2' => " =",
    '3' => " = ",
    '4' => "= ")
  def test_parseDefaultDestructor(data)
    ["", "virtual"].each do |prefix|
      op = data
      className = "NameD"
      line = prefix + "~" + className + "()(void)#{op}default"
      block = DestructorBlock.new(line, className)
      assert_true(block.canTraverse?)
      assert_equal("~NameD", block.keyForParent)
    end
  end

  def test_invalidDestructor
    block = DestructorBlock.new("", "Name")
    assert_false(block.canTraverse?)
    assert_nil(block.keyForParent)
  end

  data(
    'any' => [".*", "x", true, false],
    'matched' => ["Name", "x", true, false],
    'not matched' => ["D", "x", false, false],
    'invalid' => ["*(*", "x", false, false],
    'nodetail' => ["x", "ame", false, true],
    'both' => ["Nam", "ame", true, true])
  def test_filterByName(data)
    patternName, patternDetail, expectedFilter, expectedDetail = data
    nameFilter = SilentClassNameFilter.new([patternName])
    noDetailFilter = SilentClassNameFilter.new([patternDetail])
    block = ClassBlock.new("class NameC {", ClassBlockParameterSet.new(nameFilter, noDetailFilter))
    assert_equal(expectedFilter, block.instance_variable_get(:@filteredOut))
    assert_equal(expectedDetail, block.instance_variable_get(:@noParsingDetail))
  end

  def test_filterByReferenceSet
    className = "NameD"
    classBlock = ClassBlock.new("class #{className} {")
    block = DestructorBlock.new("NameD::~NameD();", className)
    classBlock.connect(block)

    refDestructor = TestMockRefClass.new(className, "#{className}::~NameD", "~NameD", nil, nil)
    assert_true(block.filterByUndefinedReferenceSet(TestMockRefMonoSetClass.new(refDestructor), className))

    refConstructor = TestMockRefClass.new(className, "#{className}::NameD", "NameD", nil, nil)
    assert_false(block.filterByUndefinedReferenceSet(TestMockRefMonoSetClass.new(refConstructor), className))

    refSet = TestMockRefSetClass.new(true, [refDestructor, refConstructor])
    assert_true(block.filterByUndefinedReferenceSet(refSet, className))

    def refDestructor.memberName
      nil
    end
    assert_false(block.filterByUndefinedReferenceSet(TestMockRefMonoSetClass.new(refDestructor), className))
  end

  def test_makeStubDefInClass
    className = "NameD"
    block = DestructorBlock.new("#{className}::~#{className}();", className)
    assert_equal("NameD::~NameD(void) {}\n", block.makeStubDef(className))

    classBlock = ClassBlock.new("class #{className} {")
    classBlock.connect(block)
    assert_equal("::NameD::~NameD(void) {}\n", block.makeStubDef(className))
  end

  def test_makeStubDefInNamespace
    className = "NameD"
    block = DestructorBlock.new("NameD::~NameD();", className)
    nsBlock = NamespaceBlock.new("namespace A")
    nsBlock.connect(block)
    assert_equal("::A::NameD::~NameD(void) {}\n", block.makeStubDef(className))
  end

  def test_makeStubDefInNamespaceAndClass
    className = "NameD"
    block = DestructorBlock.new("NameD::~NameD();", className)
    classBlock = ClassBlock.new("class #{className} {")
    classBlock.connect(block)

    nsBlock = NamespaceBlock.new("namespace A")
    nsBlock.connect(block)
    assert_equal("::A::NameD::~NameD(void) {}\n", block.makeStubDef(className))
  end

  data(
    'constructor' => "NameD()",
    'constructor arg' => "NameD(int a)",
    'constructor space' => "NameD (int a)")
  def test_parseNotDestructor(data)
    line = data
    className = "NameD"
    block = DestructorBlock.new(line, className)
    assert_false(block.canTraverse?)
  end
end

class TestLineWithoutAttribute < Test::Unit::TestCase
  data(
    'empty' => "__attribute__(()) int",
    'no value1' => "__attribute__((packed_)) int",
    'no value2' => "__attribute__(( unused )) int",
    'no value3' => "__attribute__  (( packed )) int",
    'value1' => "__attribute__((align(16))) int",
    'value2' => "__attribute__(( align(16) )) int",
    'value3' => "__attribute__(( align ( 16 ) )) int",
    'alinged' => "__aligned( 16 ) int")
  def test_removeAttribute(data)
    phrase = data
    assert_equal("int", LineWithoutAttribute.new(phrase).str)
  end

  data(
    'empty' => "",
    'not contain' => "static",
    'contain1' => "attributeA",
    'contain2' => "__packedA",
    'contain3' => "__packed__")
  def test_removeAttributeNoChanging(data)
    phrase = data
    assert_equal(phrase, LineWithoutAttribute.new(phrase).str)
  end

  data(
    'prefix' =>
    ["__attribute__((unused)) static int Func2(void)", "static int Func2(void)"],
    'postfix' =>
    ["static int Func2(void) __attribute__((unused));", "static int Func2(void) ;"],
    'var' =>
    ["int var __attribute__(( align ( 32 ) ) );", "int var ;"],
    'packed' =>
    ["__packed struct S {", "struct S {"],
    'triple )' =>
    ["int f(void(ClassName::*f)(int)__attribute__((thiscall)))", "int f(void(ClassName::*f)(int))"])
  def test_removeAttributeFromLine(data)
    phrase, expected = data
    assert_equal(expected, LineWithoutAttribute.new(phrase).str)
  end
end

class TestMemberFunctionBlock < Test::Unit::TestCase
  data(
    'empty' => ["", [""]],
    'primitive' => ["int i", ["int"]],
    'size_t' => ["size_t s", ["size_t"]],
    'std::size_t' => ["std::size_t s", ["std::size_t"]],
    'exact class' => ["NameC::T t", ["NameC::T", "T"]],
    'different class' => ["NameC2::T t", ["NameC2::T"]],
    'different namespace' => ["M::NameC::T t", ["M::NameC::T"]],
    'lower' => ["aNameC::T t", ["aNameC::T"]],
    'upper' => ["ANameC::T t", ["ANameC::T"]],
    'digit' => ["1NameC::T t", ["1NameC::T"]],
    'underbar' => ["_NameC::T t", ["_NameC::T"]],
    'multi' => ["NameC::T t, NameC::U i, NameC::V", ["NameC::T,NameC::U,NameC::V", "T,U,V"]],
    'multi left' => ["NameC2::T t, NameC::U i, NameC::V", ["NameC2::T,NameC::U,NameC::V", "NameC2::T,U,V"]],
    'multi middle' => ["NameC::T t, NameC2::U i, NameC::V", ["NameC::T,NameC2::U,NameC::V", "T,NameC2::U,V"]],
    'multi right' => ["NameC::T t, NameC::U i, NameC2::V", ["NameC::T,NameC::U,NameC2::V", "T,U,NameC2::V"]])
  def test_setParent(data)
    argstr, expected = data
    className = "NameC"
    classBlock = ClassBlock.new("class #{className} {")
    block = MemberFunctionBlock.new("int func(#{argstr});")
    block.setParent(classBlock)
    assert_equal(expected, block.instance_variable_get(:@argTypeStrSet))
  end

  data(
    'void no args' =>
    ["void Func()",
     "void Func()",
     "Func()",
     false, false, "void", true, "", "Func", "Func_", "", "", false],
    'end with _' =>
    ["void Func_()",
     "void Func_()",
     "Func_()",
     false, false, "void", true, "", "Func_", "Func_", "", "", false],
    'one word type and function post modifier' =>
    ["int Func2(long a) const override",
     "int Func2(long a) const override",
     "Func2(long)const",
     true, false, "int", false, "a", "Func2", "Func2_", "long a", "const override", false],
    'static and attribute' =>
    ['static int Func(long b)',
     "int Func(long b)",
     "Func(long)",
     false, true, "int", false, "b", "Func", "Func_", "long b", "", false],
    'multiple word type and poset modifier' =>
    ["virtual const void* Func(int a, const T* p);",
     "const void * Func(int a,const T* p)",
     "Func(int,const T *)",
     false, false, "const void *", false, "a,p", "Func", "Func_", "int a,const T* p", "", true],
    'inline and reference' =>
    ["inline T&* Func(T& a) const",
     "T & * Func(T& a) const",
     "Func(T &)const",
     true, false, "T & *", false, "a", "Func", "Func_", "T& a", "const", false],
    'pointer to a function' =>
   ["void Func(int(*f)(int,long),char c)const",
     "void Func(int (*f) (int,long),char c) const",
     "Func(int (*) (int,long),char)const",
     true, false, "void", true, "f,c", "Func", "Func_", "int (*f) (int,long),char c", "const", false],
    'pointer to a member function' =>
   ["void Func(int(Name::*f)(int,long),char c)const",
     "void Func(int (Name::*f) (int,long),char c) const",
     "Func(int (Name::*) (int,long),char)const",
     true, false, "void", true, "f,c", "Func", "Func_", "int (Name::*f) (int,long),char c", "const", false])
  def test_initializeAndParse(data)
    line, decl, argSignature, constMemfunc, staticMemfunc, returnType, returnVoid,
    argSet, funcName, switchName, typedArgSet, postFunc, virtual = data

    ["", "{", " {", ";", " ;"].each do |suffix|
      fullLine = line + suffix;
      block = MemberFunctionBlock.new(fullLine)
      assert_true(block.valid)
      assert_true(block.canTraverse?)
      assert_equal(suffix.include?("{"), block.instance_variable_get(:@definition))
      assert_equal(constMemfunc, block.instance_variable_get(:@constMemfunc))
      assert_equal(staticMemfunc, block.instance_variable_get(:@staticMemfunc))
      assert_equal(returnType, block.instance_variable_get(:@returnType))
      assert_equal(returnVoid, block.instance_variable_get(:@returnVoid))
      assert_equal(decl, block.instance_variable_get(:@decl))
      assert_equal(argSet, block.instance_variable_get(:@argSet))
      assert_equal(funcName, block.funcName)
      assert_equal(fullLine, block.keyForParent)
      assert_equal(switchName + Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX, block.instance_variable_get(:@switchName))
      assert_equal(typedArgSet, block.instance_variable_get(:@typedArgSet))
      assert_equal(argSignature, block.argSignature)
      assert_equal(postFunc, block.instance_variable_get(:@postFunc))
      assert_false(block.instance_variable_get(:@defaultNoForwardingToMock))

      assert_equal(virtual, block.virtual?)
      assert_equal(!suffix.include?("{") && virtual, block.virtualDeclaration?)
      assert_equal([], block.instance_variable_get(:@superMemberSet))
      assert_nil(block.instance_variable_get(:@templateParam))
    end
  end

  data(
    'empty' => "",
    'desctructor' => "~Dtor(void)",
    'virtual desctructor' => "virtual ~Dtor(void)",
    'copy constructor' => "T& operator=(const T& rhs)",
    'operator' => "bool operator<(void)",
    'va_arg' => "int printf(const char *format, ...)",
    'variable 1' => "int var = (1 << 3)",
    'variable 2' => "decltype(T) var = (1 << 3)",
    'variable 3' => "decltype(T) var = 1 + (1 << 3)",
    'static_assert' => 'static_assert(false, "Error")',
    'template 1' => 'int Func<T>(long b)',
    'template 2' => 'int Func<T,U>(long b)',
    'template 3' => 'template <typename T> T Get(void)',
    'template 4' => ' template<typename T> T Get(void)')
  def test_cannotInitializeAndParse(data)
    line = data
    ["", "{", " {", ";", " ;"].each do |suffix|
      block = MemberFunctionBlock.new(line + suffix)
      assert_false(block.valid)
      assert_false(block.canTraverse?)
      assert_nil(block.keyForParent)
    end
  end

  data(
    'empty' => ["", false],
    'default constructor' => ["Ctor(void) = default", false],
    'default argument' => ["void f(T* p=(nullptr))", false],
    'decltype and function' => ["decltype(T) void f(void)", false],
    'copy constructor' => ["T& operator = (const T& rhs)", false],
    'variable' => ["int var = (1 << 3)", true],
    'decltype and variable 1' => ["decltype(T) var = (1 << 3)", true],
    'decltype and variable 2' => ["decltype(T) var = 1 + (1 << 3)", true])
  def test_isVariable(data)
    line, expected = data
    block = MemberFunctionBlock.new(line)
    assert_equal(expected, block.isVariable?(line))
  end

  def test_filterByReferenceSet
    refMatched1 = TestMockRefClass.new("", "Func1", "Func1", "int", "const")
    refMatched2 = TestMockRefClass.new("", "Name::Func1", "Func1", "int", "const")
    refUnmatched1 = TestMockRefClass.new("", "Func1", "Func1", "int", "")
    refUnmatched2 = TestMockRefClass.new("", "Func1", "Func1", "int*", "const")

    block = MemberFunctionBlock.new("void Func1(int) const")
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refMatched1))))
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refMatched2))))
    assert_false(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refUnmatched1))))
    assert_false(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refUnmatched2))))

    refSet = TestMockRefSetClass.new(true, [refUnmatched1, refMatched1])
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet)))
  end

  data(
    'name' => ["Size", [true, true]],
    'class' => ["Name::Size", [true, false]],
    'other class' => ["Other::Size", [false, false]],
    'namespace' => ["Outer::Name::Size", [false, false]])
  def test_filterByReferenceSetWithInnerTypedef(data)
    refArgType, expectedSet = data
    funcRef = TestMockRefClass.new("", "Func", "Func", refArgType, "")

    [["Name::Size", "Size"], expectedSet].transpose.each do |argType, expected|
      block = MemberFunctionBlock.new("void Func(#{argType} s);")
      classBlock = ClassBlock.new("class Name {")
      block.setParent(classBlock)
      count = argType.include?("::") ? 2 : 1
      assert_equal(count, block.instance_variable_get(:@argTypeStrSet).size)
      assert_equal(expected, block.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(funcRef))))
    end
  end

  def test_filterByPointerToFunction
    refFreeFunction = TestMockRefClass.new("", "Func", "Func", "void (*)(int,long)", "const")
    refMemberFunction = TestMockRefClass.new("", "Func", "Func", "void (Top::Class::*)(int,long)", "const")
    blockFreeFunction = MemberFunctionBlock.new("void Func(void (*f)(int,long)) const")
    blockMemberFunction = MemberFunctionBlock.new("void Func(void (Top::Class::*g)(int,long)) const")

    assert_true(blockFreeFunction.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refFreeFunction))))
    assert_true(blockMemberFunction.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refMemberFunction))))
    assert_false(blockMemberFunction.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refFreeFunction))))
    assert_false(blockFreeFunction.filterByReferenceSet(MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refMemberFunction))))
  end

  def test_filterByReferenceSetNoOverloading
    className = "NameC"
    classBlock = ClassBlock.new("class #{className} {")
    block = MemberFunctionBlock.new("void Func1(void) const")
    classBlock.connect(block)

    refUnmatched = TestMockRefClass.new("", "#{className}::Func1", "Func1", "int*", "const")
    filter = MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(refUnmatched))
    assert_false(block.filterByReferenceSet(filter))
    filter.setNoOverloading
    assert_true(block.filterByReferenceSet(filter))
  end

  def test_override?
    argSet = ["", "int", "int *", "int &", "int,int",
              "int,const T", "int,const T *", "int,const T &"]

    0.upto((argSet.size)-1) do |left|
      0.upto((argSet.size)-1) do |right|
        leftBlock = MemberFunctionBlock.new("")
        rightBlock = MemberFunctionBlock.new("")
        leftBlock.instance_variable_set(:@argSignature, "Func(#{argSet[left]})")
        rightBlock.instance_variable_set(:@argSignature, "Func(#{argSet[right]})")
        assert_equal((left == right), leftBlock.override?(rightBlock))
        assert_equal((left == right), rightBlock.override?(leftBlock))
      end
    end

    argSet.each do |arg|
      leftBlock = MemberFunctionBlock.new("")
      rightBlock = MemberFunctionBlock.new("")
      constBlock = MemberFunctionBlock.new("")
      leftBlock.instance_variable_set(:@argSignature, "FuncA(#{arg})")
      rightBlock.instance_variable_set(:@argSignature, "FuncB(#{arg})")
      constBlock.instance_variable_set(:@argSignature, "FuncA(#{arg})const")
      assert_false(leftBlock.override?(rightBlock))
      assert_false(leftBlock.override?(constBlock))
    end
  end

  def test_overridePureVirtual?
    line = "virtual int Func(void) const"
    leftBlock = MemberFunctionBlock.new(line + ";")
    rightBlock = MemberFunctionBlock.new(line + " =0;")
    assert_true(leftBlock.valid)
    assert_true(rightBlock.valid)
    assert_true(leftBlock.override?(rightBlock))
    assert_true(rightBlock.override?(leftBlock))
  end

  def test_addSuperMember
    ["", "{", ";"].each do |suffix|
      line = "void Func()" + suffix
      baseBlock = MemberFunctionBlock.new("virtual #{line}")
      middleBlock = MemberFunctionBlock.new("#{line}")
      subBlock = MemberFunctionBlock.new("#{line}")

      assert_true(baseBlock.instance_variable_get(:@virtual))
      assert_false(middleBlock.instance_variable_get(:@virtual))
      assert_false(subBlock.instance_variable_get(:@virtual))

      subBlock.addSuperMember(middleBlock)
      middleBlock.addSuperMember(baseBlock)

      [baseBlock, middleBlock, subBlock].each do |block|
        assert_true(block.virtual?)
        assert_equal(!suffix.include?("{"), block.virtualDeclaration?)
      end

      assert_equal(0, baseBlock.instance_variable_get(:@superMemberSet).size)
      assert_equal(1, middleBlock.instance_variable_get(:@superMemberSet).size)
      assert_equal(1, subBlock.instance_variable_get(:@superMemberSet).size)

      # Ignore an already bound block
      subBlock.addSuperMember(middleBlock)
      assert_equal(1, subBlock.instance_variable_get(:@superMemberSet).size)
    end
  end

  data('off instance' => [false, false, "", " {false}"],
       'off class' => [false, true, "static ", ""],
       'on instance' => [true, false, "", " {true}"],
       'on class' => [true, true, "static ", ""])
  def test_setDefaultNoForwardingToMock(data)
    arg, isStatic, prefix, init = data
    line = (isStatic ? "static " : "") + "void Func(void);"
    block = MemberFunctionBlock.new(line)
    block.setDefaultNoForwardingToMock(arg)
    assert_equal(arg, block.instance_variable_get(:@defaultNoForwardingToMock))

    name = "Func_" + Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_POSTFIX
    expected = "    #{prefix}#{Mockgen::Constants::MEMFUNC_FORWARD_SWITCH_TYPE} #{name}#{init};\n"
    assert_equal(expected, block.makeSwitchToMock(isStatic))
  end

  data(
    'no args' =>
    ["void Func(void)",
     "    MOCK_METHOD0(Func,void())",
     "void Super::Func() {\n    return;\n}\n",
     "    void Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { pMock_->Func(); return; } " +
     "Super::Func(); }\n",
     "    void Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { pMock_->Func(); return; } " +
     "static_cast<Super*>(pActual_)->Func(); }\n"],
    'no args const' =>
    ["void Func() const override",
     "    MOCK_CONST_METHOD0(Func,void())",
     "void Super::Func() const {\n    return;\n}\n",
     "    void Func() const override { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { pMock_->Func(); return; } " +
     "Super::Func(); }\n",
     "    void Func() const override { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { pMock_->Func(); return; } " +
     "static_cast<Super*>(pActual_)->Func(); }\n"],
    'one arg return primitive' =>
    ["int Func()",
     "    MOCK_METHOD0(Func,int())",
     "int Super::Func() {\n    int result = 0;\n    return result;\n}\n",
     "    int Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { return pMock_->Func(); } " +
     "return Super::Func(); }\n",
     "    int Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { return pMock_->Func(); } " +
     "return static_cast<Super*>(pActual_)->Func(); }\n"],
    'one arg return struct' =>
    ["StructT Func()",
     "    MOCK_METHOD0(Func,StructT())",
     "StructT Super::Func() {\n    StructT result;\n    return result;\n}\n",
     "    StructT Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { return pMock_->Func(); } " +
     "return Super::Func(); }\n",
     "    StructT Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { return pMock_->Func(); } " +
     "return static_cast<Super*>(pActual_)->Func(); }\n"],
    'one arg return pointer' =>
    ["const void* Func(int a)",
     "    MOCK_METHOD1(Func,const void *(int a))",
     "const void * Super::Func(int a) {\n    const void * result = 0;\n    return result;\n}\n",
     "    const void * Func(int a) { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { return pMock_->Func(a); } " +
     "return Super::Func(a); }\n",
     "    const void * Func(int a) { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { return pMock_->Func(a); } " +
     "return static_cast<Super*>(pActual_)->Func(a); }\n"],
    'two args return reference' =>
     ["T& Func(int a, const void* p)",
      "    MOCK_METHOD2(Func,T &(int a,const void* p))",
      "T & Super::Func(int a,const void* p) {\n    T result;\n    return result;\n}\n",
      "    T & Func(int a,const void* p) { if (" + getTestSwitchMockCondition("Func", "pMock_") +
      ") { return pMock_->Func(a,p); } " +
      "return Super::Func(a,p); }\n",
      "    T & Func(int a,const void* p) { if (" + getTestSwitchMockCondition("Func", "pMock_") +
      ") { return pMock_->Func(a,p); } " +
      "return static_cast<Super*>(pActual_)->Func(a,p); }\n"])
  def test_makeDefSet(data)
     line, mock, stub, decorator, forwarder = data
     className = "Super"
     block = MemberFunctionBlock.new(line + ";")

     assert_true(block.valid)
     expectedMock = mock + ";\n"
     assert_equal(expectedMock, block.makeMockDef(className, ""))
     assert_equal(stub, block.makeStubDef(className))

     switchToMock = getTestSwitchMockVarname("Func")
     switchToMockStatic = getTestSwitchMockStaticVarname("Func")
     assert_equal(switchToMock, block.makeSwitchToMock(false))
     assert_equal(switchToMockStatic, block.makeSwitchToMock(true))

     definedNameSet = {}
     definedStaticNameSet = {}
     assert_equal(switchToMockStatic + decorator, block.makeDecoratorDef(className, definedNameSet, definedStaticNameSet))
     assert_equal(1, definedNameSet.size)
     assert_true(definedNameSet.key?("Func"))
     assert_true(definedStaticNameSet.key?("Func"))

     definedNameSet = {}
     assert_equal(switchToMock + forwarder, block.makeForwarderDef(className, definedNameSet))
     assert_equal(1, definedNameSet.size)
     assert_true(definedNameSet.key?("Func"))

     expected = expectedMock.gsub(/(METHOD\d)/, '\1_T')
     assert_equal(expected, block.makeMockDef(className, "_T"))
  end

  data(
    'no args' =>
    ["void Func(void)",
     "    MOCK_METHOD0(Func,void())",
     "void Super::Func() {\n    return;\n}\n",
     "    void Func() { if (pMock_) { pMock_->Func(); return; } " +
     "Super::Func(); }\n",
     "    void Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
     ") { pMock_->Func(); return; } " +
     "static_cast<Super*>(pActual_)->Func(); }\n"])
  def test_makeDefSetForTemplate(data)
     line, mock, stub, decorator, forwarder = data
     className = "Super"
     block = MemberFunctionBlock.new(line + ";")

     assert_true(block.valid)
     expectedMock = mock + ";\n"
     assert_equal(expectedMock, block.makeMockDef(className, ""))
     assert_equal(stub, block.makeStubDef(className))

     switchToMock = getTestSwitchMockVarname("Func")
     switchToMockStatic = getTestSwitchMockStaticVarname("Func")
     assert_equal(switchToMock, block.makeSwitchToMock(false))
     assert_equal(switchToMockStatic, block.makeSwitchToMock(true))

     definedNameSet = {}
     assert_equal(decorator, block.makeDecoratorDef(className, definedNameSet, false))
     assert_true(definedNameSet.empty?)

     definedNameSet = {}
     assert_equal(switchToMock + forwarder, block.makeForwarderDef(className, definedNameSet))
     assert_equal(1, definedNameSet.size)
     assert_true(definedNameSet.key?("Func"))

     expected = expectedMock.gsub(/(METHOD\d)/, '\1_T')
     assert_equal(expected, block.makeMockDef(className, "_T"))
  end

  def test_makeDefSetAlias(data)
     line, mock, stub, decorator, forwarder = data
     className = "Super"
     block = MemberFunctionBlock.new("const T Func(int a);")

     def block.findType(returnType)
       return true, true
     end

     assert_true(block.valid)
     expectedStub = "const T Super::Func(int a) {\n    const T result = 0;\n    return result;\n}\n"
     assert_equal(expectedStub, block.makeStubDef(className))
  end

  data(
    'no args' =>
    ["static int Func(long a)",
     getTestSwitchMockStaticVarname("Func") +
     "    static int Func(long a) { if (" + getTestSwitchMockCondition("Func", "pClassMock_") + ") " +
     "{ return pClassMock_->Func(a); } return Super::Func(a); }\n"])
  def test_makeDecoratorDef(data)
     line, decorator = data
     className = "Super"
     block = MemberFunctionBlock.new(line + ";")

     assert_true(block.valid)
     assert_equal(decorator, block.makeDecoratorDef(className, {}, {}))
  end

  data(
    'free function' =>
    ["void Func(long (*)(void (*)(int)), long) const;",
     "void Name::Func(long (*dummy1) (void(*)(int)),long dummy2) const {\n    return;\n}\n",
     "    MOCK_CONST_METHOD2(Func,void(long (*dummy1) (void(*)(int)),long dummy2));\n"],
    'member function' =>
    ["const char* Func(int (Top::Class::*)(int, void const**)) const",
     "const char * Name::Func(int (Top::Class::*dummy1) (int, void const**)) const {\n    const char * result = 0;\n    return result;\n}\n",
     "    MOCK_CONST_METHOD1(Func,const char *(int (Top::Class::*dummy1) (int, void const**)));\n"])
  def test_makeForPointerToFunction(data)
    line, expectedStub, expectedDecorator = data
    className = "Name"
    block = MemberFunctionBlock.new(line)
    assert_equal(expectedStub, block.makeStubDef(className))
    assert_equal(expectedDecorator, block.makeMockDef(className, ""))
  end

  data(
    'no args void' =>
    ["void Func()", "void", "Func", ""],
    'return type' =>
    ["int Func2(void)", "int", "Func2", ""],
    'static' =>
    ["static int Func2(void)", "static int", "Func2", ""],
    'attribute static' =>
    ["static int Func2(void)", "static int", "Func2", ""],
    'inline' =>
    ["inline void Func(int a)", "inline void", "Func", ""],
    'virtual and pointer' =>
    ["virtual void* Func(int a, const T* p)", "virtual void *", "Func", ""],
    'pointer **' =>
    ["virtual void **Func(int a, const T* p)", "virtual void **", "Func", ""],
    'override' =>
    ["int Func(long a) const override", "int", "Func", "const override"],
    'type which includes reserved word static' =>
    ["staticType Func()", "staticType", "Func", ""],
    'function name which includes reserved word static' =>
    ["int staticA(int a)", "int", "staticA", ""],
    'function name which includes reserved word operator' =>
    ["int operatorB(int a)", "int", "operatorB", ""])
  def test_parseLineAccepted(data)
    line, expectedPreFunc, expectedFuncName, expectedPostFunc = data
    block = MemberFunctionBlock.new("")

    assert_true(block.parse(line))
    assert_equal(expectedPreFunc, block.instance_variable_get(:@preFunc))
    assert_equal(expectedFuncName, block.instance_variable_get(:@funcName))
    assert_equal(expectedPostFunc, block.instance_variable_get(:@postFunc))
  end

  data(
    'empty' => "",
    'desctructor' => "~Dtor(void)",
    'virtual desctructor' => "virtual ~Dtor(void)",
    'copy constructor' => "T& operator=(const T& rhs)",
    'operator <' => "bool operator<(void)",
    'operator new' => "static void *operator new(std::size_t s)",
    'operator delete' => "static void operator delete(void *p)")
  def test_parseLineRejected(data)
    line = data
    block = MemberFunctionBlock.new("")
    assert_false(block.parse(line))
  end

  data(
    'empty' => "",
    'pure virtual' => "=0",
    'override' => "override",
    'final' => "final")
  def test_isConstMemberFunction?(data)
    phrase = data
    block = MemberFunctionBlock.new("")
    assert_false(block.isConstMemberFunction?(phrase))
    assert_true(block.isConstMemberFunction?("const " + phrase))
    assert_true(block.isConstMemberFunction?("const #{phrase} {"))
  end

  data(
    'void' => ["void", "void", true],
    'primitive' => ["int",  "int",  false],
    'void pointer' => ["void*", "void *", false],
    'void double pointer' => ["void**", "void * *", false],
    'reference' => ["int&", "int &", false],
    'const reference1' => ["const Type&", "const Type &", false],
    'const reference2' => ["const Type &", "const Type &", false],
    'va_arg' => ["...", "...", false])
  def test_extractReturnType(data)
    phrase, expected, isVoid = data
    block = MemberFunctionBlock.new("")
    staticMemfunc, returnType, returnVoid = block.extractReturnType(phrase)
    assert_equal(expected, returnType)
    assert_equal(isVoid, returnVoid)
    assert_false(staticMemfunc)

    Mockgen::Constants::MEMFUNC_WORD_SKIPPED_SET.each do |prefix|
      staticMemfunc, returnType, returnVoid = block.extractReturnType("#{prefix} #{phrase}")
      assert_equal(expected, returnType)
      assert_equal(isVoid, returnVoid)
      assert_equal((prefix == "static") ? true : false, staticMemfunc)
    end
  end

  data(
    'no args or void' => ["", ""],
    'primitive' => ["int", "int"],
    'reference1' => ["int&", "int&"],
    'reference2' => ["int &", "int &"],
    'pointer1' => ["void*", "void*"],
    'pointer2' => ["void *", "void *"],
    'pointer double' => ["void * *", "void * *"],
    'pointer and const' => ["const void * const", "const void * const"],
    'pointer and reference' => ["void * &", "void * &"],
    'multiple args' => ["int,long", "int,long"],
    'mixed args' => ["int,long,const void *", "int,long,const void *"])
  def test_extractArgSignature(data)
    phrase, typeSet = data
    funcName = "Func"

    block = MemberFunctionBlock.new("")
    expected = "#{funcName}(#{typeSet})"
    assert_equal(expected, block.extractArgSignature(funcName, phrase, false))
    expected = "#{funcName}(#{typeSet})const"
    assert_equal(expected, block.extractArgSignature(funcName, phrase, true))
  end

  data(
    'empty' => ["", []],
    'void' => ["void", ["void"]],
    'pointer' => ["void*", ["void","*"]],
    'pointer2' => ["void**", ["void","*","*"]],
    'pointer3' => ["void *** ", ["void","*","*","*"]],
    'reference' => ["void& ", ["void","&"]],
    'pointer and reference' => ["void*&", ["void","*","&"]])
  def test_splitByReferenceMarks(data)
    phrase, expected = data
    block = MemberFunctionBlock.new("")
    assert_equal(expected, block.splitByReferenceMarks(phrase))
  end

  data(
    'non virtual' => ["void Func()", ""],
    'virtual' => ["virtual void Func()", "override "],
    'override' => ["void Func() override", ""],
    'virtual override' => ["virtual void Func() override", ""])
  def test_getOverrideStr(data)
    decl, expected = data
    assert_true(Mockgen::Constants::CPP11_MODE)
    block = MemberFunctionBlock.new(decl)
    assert_equal(expected, block.getOverrideStr(decl))
  end
end

class TestFreeFunctionBlock < Test::Unit::TestCase
  data(
    'inline' => "inline",
    'static inline' => "static inline",
    'inline static' => "inline static")
  def test_inlineFunction(data)
    line = data + " int Func(long) {"
    block = FreeFunctionBlock.new(line)
    assert_true(block.valid)
    assert_equal("Func", block.funcName)
    assert_equal("Func(long)", block.argSignature)
  end

  data(
    'declaration' => "inline int Func(long);",
    'system' => "inline int __System(void) {",
    'static_assert' => 'static_assert(true, "Error")')
  def test_notInlineFunction(data)
    line = data
    block = FreeFunctionBlock.new(line)
    assert_false(block.valid)
  end

  data(
    'empty' => "",
    'libc' => "time",
    'thread' => "pthread_create",
    'number' => "bin2hex",
    'system' => "_IO")
  def test_invalid(data)
    line = data
    block = FreeFunctionBlock.new(line)
    assert_false(block.valid)
    assert_nil(block.keyForParent)
  end

  def test_setNoMatchingTypes
    line = "extern int Func(int a);"
    block = FreeFunctionBlock.new(line)
    assert_equal("int Func(int a);", block.keyForParent)

    assert_false(block.instance_variable_get(:@noMatchingTypes))
    block.setNoMatchingTypes
    assert_true(block.instance_variable_get(:@noMatchingTypes))
  end

  def test_filterByDefinedReferenceSet
    line = "extern int Func(int a);"
    block = FreeFunctionBlock.new(line)
    assert_equal("int Func(int a);", block.keyForParent)
    assert_false(block.instance_variable_get(:@alreadyDefined))

    assert_false(block.makeMockDef("", "").empty?)
    assert_false(block.makeStubDef("").empty?)
    assert_false(block.makeForwarderDef("", {}).empty?)

    defRefSet = Object.new()
    def defRefSet.freeFunctionDefined?(arg)
      true
    end

    ref = UndefinedReference.new("")
    block.filterByReferenceSet(MinimumSymbolFilter.new(defRefSet, TestMockRefMonoSetClass.new(ref)))
    assert_true(block.instance_variable_get(:@alreadyDefined))

    assert_equal("", block.makeMockDef("", ""))
    assert_equal("", block.makeStubDef(""))
    assert_equal("", block.makeForwarderDef("", {}))
  end

  def test_filterByUndefinedReferenceSet
    block = FreeFunctionBlock.new("extern int Func(int a);")
    assert_true(block.isFreeFunction?)
    assert_equal("Func", block.funcName)
    assert_equal("Func(int)", block.argSignature)

    defRefSet = Object.new()
    def defRefSet.freeFunctionDefined?(arg)
      false
    end

    prefix = Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE + " "
    ref = UndefinedReference.new(prefix + "`Func(int)'")
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(defRefSet, TestMockRefMonoSetClass.new(ref))))

    ref = UndefinedReference.new(prefix + "`Func'")
    assert_true(block.filterByReferenceSet(MinimumSymbolFilter.new(defRefSet, TestMockRefMonoSetClass.new(ref))))

    ref = UndefinedReference.new(prefix + "`A::Func'")
    assert_false(block.filterByReferenceSet(MinimumSymbolFilter.new(defRefSet, TestMockRefMonoSetClass.new(ref))))
  end

  data(
    'empty' => [[], true],
    'exact' => [['FuncA'], true],
    'word' => [['\bFuncA\b'], true],
    'sentence' => [['^FuncA$'], true],
    'regex' => [['Func.'], true],
    'any' => [['.*'], true],
    'not exact' => [['FuncB'], false],
    'wrong regex' => [['Func..'], false],
    'wrong space' => [[' FuncA'], false])
  def test_filter(data)
    filterSet, expected = data
    func = FreeFunctionBlock.new("extern void FuncA();")
    assert_true(func.valid)
    assert_equal(expected, func.filter(filterSet))
    assert_equal(expected, func.valid)
  end

  def test_makeForwarderDef
    block = FreeFunctionBlock.new("extern int Func(int a);")
    expected = getTestSwitchMockVarname("Func")
    expected += "    int Func(int a) { if (" + getTestSwitchMockCondition("Func", "pMock_")
    expected += ") { return pMock_->Func(a); } return ::Func(a); }\n"
    assert_equal(expected, block.makeForwarderDef("", {}))

    nsBlock = NamespaceBlock.new("namespace A")
    nsBlock.connect(block)
    expected = getTestSwitchMockVarname("Func")
    expected += "    int Func(int a) { if (" + getTestSwitchMockCondition("Func", "pMock_")
    expected += ") { return pMock_->Func(a); } return ::A::Func(a); }\n"
    assert_equal(expected, block.makeForwarderDef("", {}))
  end

  def test_getSwapperDef
    name = "var"
    block = FreeFunctionBlock.new("")
    assert_equal("", block.getSwapperDef(name))

    block = FreeFunctionBlock.new("extern int Func(int a);")
    assert_equal("#define Func #{name}.Func\n", block.getSwapperDef(name))

    block.instance_variable_set(:@alreadyDefined, true)
    assert_equal("", block.getSwapperDef(name))
  end
end

class TestMockFreeFunction
  attr_reader :valid
  def initialize(num)
    @valid = true
    @num = num
  end

  def filter(filterSet)
    @valid = filterSet.any? { |f| f.call(@num) }
  end
end

class TestFreeFunctionSet < Test::Unit::TestCase
  def test_initialize
    block = RootBlock.new("")
    funcSet = FreeFunctionSet.new(block)
    assert_false(funcSet.needStub?)

    funcSet.makeStubSet
    assert_equal("", funcSet.getStringToClassFile)
    assert_equal("", funcSet.getStringToDeclFile)
    assert_equal(["", ""], funcSet.getStringSetToDeclInlineFile)
    assert_equal("", funcSet.getStringToSwapperFile)
    assert_equal("", funcSet.getStringOfStub)
    assert_equal("", funcSet.getStringOfVariableDefinition)

    funcSet.makeClassSet
    assert_equal("", funcSet.getStringToClassFile)
    assert_equal("", funcSet.getStringToDeclFile)
    assert_equal(["", ""], funcSet.getStringSetToDeclInlineFile)
    assert_equal("", funcSet.getStringToSwapperFile)
    assert_equal("", funcSet.getStringOfStub)
    assert_equal("", funcSet.getStringOfVariableDefinition)
  end

  def test_getFullNamespace
    block = RootBlock.new("")
    funcSet = FreeFunctionSet.new(block)
    assert_equal("", funcSet.getFullNamespace)

    def block.getFullNamespace
      "::A::B"
    end
    assert_equal("::A::B", funcSet.getFullNamespace)
  end

  def test_merge
    block = RootBlock.new("")
    funcSetA = FreeFunctionSet.new(block)
    funcSetB = FreeFunctionSet.new(block)

    funcSetB.instance_variable_set(:@funcSet, [1,2])
    funcSetB.instance_variable_set(:@undefinedFunctionSet, [3])

    funcSetA.merge(funcSetB)
    assert_equal(2, funcSetA.funcSet.size)
    assert_equal(1, funcSetA.undefinedFunctionSet.size)
  end

  def test_filter
    block = RootBlock.new("")

    funcSetBlock = FreeFunctionSet.new(block)
    makeArray = lambda { |n| 1.upto(n).map { |i| TestMockFreeFunction.new(i) } }
    funcSetBlock.instance_variable_set(:@funcSet, makeArray.call(12))
    funcSetBlock.instance_variable_set(:@undefinedFunctionSet, makeArray.call(24))

    expr2 = lambda { |arg| arg.even? }
    expr3 = lambda { |arg| arg % 3 == 0 }
    funcSetBlock.filter([expr2, expr3])
    # filter numbers of 0,2,3,4 + 6N
    assert_equal(8, funcSetBlock.instance_variable_get(:@funcSet).size)
    assert_equal(16, funcSetBlock.instance_variable_get(:@undefinedFunctionSet).size)
  end

  data(
    'none' => [[], ["", ""]],
    'one' => [["extern void FuncA();"],
              ["#define FuncA_Inline varName.FuncA\n", "#define FuncA_Inline FuncA\n"]],
    'two' => [["extern void FuncA();", "extern int funcB(int a);"],
              ["#define FuncA_Inline varName.FuncA\n#define funcB_Inline varName.funcB\n",
               "#define FuncA_Inline FuncA\n#define funcB_Inline funcB\n"]])
  def test_getStringSetToDeclInlineFile(data)
    lineSet, expected = data
    block = RootBlock.new("")
    funcSet = FreeFunctionSet.new(block)

    lineSet.each do |line|
      func = FreeFunctionBlock.new(line)
      block.connect(func)
      funcSet.add(func)
    end

    funcSet.instance_variable_set(:@forwarderVarName, "varName")
    actual = funcSet.getStringSetToDeclInlineFile
    assert_equal(expected, actual)
    assert_equal(lineSet.size, actual[0].scan("#define").size)
    assert_equal(lineSet.size, actual[1].scan("#define").size)
  end

  def test_noStubs
    block = RootBlock.new("")
    funcSet = FreeFunctionSet.new(block)

    funcA = FreeFunctionBlock.new("extern void FuncA();")
    funcB = FreeFunctionBlock.new("extern int FuncB(int a);")
    block.connect(funcA)
    block.connect(funcB)
    funcSet.add(funcA)
    funcSet.add(funcB)
    assert_false(funcSet.needStub?)

    funcSet.makeStubSet
    assert_equal("", funcSet.getStringToClassFile)
    assert_equal("", funcSet.getStringOfStub)

    funcSet.makeClassSet
    expected =  "class All_Forwarder;\n"
    expected += "class All_Mock {\n"
    expected += "public:\n"
    expected += "    All_Mock(All_Forwarder* pForwarder);\n"
    expected += "    All_Mock(All_Forwarder& forwarder);\n"
    expected += "    ~All_Mock(void);\n"
    expected += "    MOCK_METHOD0(FuncA,void());\n"
    expected += "    MOCK_METHOD1(FuncB,int(int a));\n"
    expected += "private:\n"
    expected += "    All_Forwarder* pForwarder_;\n"
    expected += "};\n\n"
    expected += "class All_Mock;\n"
    expected += "class All_Forwarder {\n"
    expected += "public:\n"
    expected += "    All_Forwarder(void) : pMock_(0) {}\n"
    expected += getTestSwitchMockVarname("FuncA")
    expected += "    void FuncA() { if (" + getTestSwitchMockCondition("FuncA", "pMock_") + ") { pMock_->FuncA(); return; } ::FuncA(); }\n"
    expected += getTestSwitchMockVarname("FuncB")
    expected += "    int FuncB(int a) { if (" + getTestSwitchMockCondition("FuncB", "pMock_") + ") { return pMock_->FuncB(a); } return ::FuncB(a); }\n"
    expected += "    All_Mock* pMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, funcSet.getStringToClassFile)

    expected =  "All_Mock::All_Mock(All_Forwarder* pForwarder) : pForwarder_(pForwarder) "
    expected += "{ pForwarder_->pMock_ = this; }\n"
    expected += "All_Mock::All_Mock(All_Forwarder& forwarder) : pForwarder_(&forwarder) "
    expected += "{ pForwarder_->pMock_ = this; }\n"
    expected += "All_Mock::~All_Mock(void) "
    expected += "{ pForwarder_->pMock_ = 0; }\n\n"
    assert_equal(expected, funcSet.getStringOfStub)

    expectedSwapper =  "#define FuncA all_Forwarder.FuncA\n"
    expectedSwapper += "#define FuncB all_Forwarder.FuncB\n"
    assert_equal(expectedSwapper, funcSet.getStringToSwapperFile)

    expectedVarName = "all_Forwarder"
    assert_equal(expectedVarName, funcSet.instance_variable_get(:@forwarderVarName))
    assert_equal("All_Forwarder #{expectedVarName};\n", funcSet.getStringOfVariableDefinition)
  end

  def test_inNamespace
    block = NamespaceBlock.new("namespace A")
    funcSet = FreeFunctionSet.new(block)
    func = FreeFunctionBlock.new("extern void Func();")
    block.connect(func)
    funcSet.add(func)

    funcSet.makeClassSet
    expected =  "class All_A_Forwarder;\n"
    expected += "class All_A_Mock {\n"
    expected += "public:\n"
    expected += "    All_A_Mock(All_A_Forwarder* pForwarder);\n"
    expected += "    All_A_Mock(All_A_Forwarder& forwarder);\n"
    expected += "    ~All_A_Mock(void);\n"
    expected += "    MOCK_METHOD0(Func,void());\n"
    expected += "private:\n"
    expected += "    All_A_Forwarder* pForwarder_;\n"
    expected += "};\n\n"
    expected += "class All_A_Mock;\n"
    expected += "class All_A_Forwarder {\n"
    expected += "public:\n"
    expected += "    All_A_Forwarder(void) : pMock_(0) {}\n"
    expected += getTestSwitchMockVarname("Func")
    expected += "    void Func() { if (" + getTestSwitchMockCondition("Func", "pMock_")
    expected += ") { pMock_->Func(); return; } ::A::Func(); }\n"
    expected += "    All_A_Mock* pMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, funcSet.getStringToClassFile)

    expected =  "All_A_Mock::All_A_Mock(All_A_Forwarder* pForwarder) : pForwarder_(pForwarder) "
    expected += "{ pForwarder_->pMock_ = this; }\n"
    expected += "All_A_Mock::All_A_Mock(All_A_Forwarder& forwarder) : pForwarder_(&forwarder) "
    expected += "{ pForwarder_->pMock_ = this; }\n"
    expected += "All_A_Mock::~All_A_Mock(void) "
    expected += "{ pForwarder_->pMock_ = 0; }\n\n"
    assert_equal(expected, funcSet.getStringOfStub)

    expected = "All_A_Forwarder all_A_Forwarder;\n"
    assert_equal("extern " + expected, funcSet.getStringToDeclFile)
    assert_equal("#define Func all_A_Forwarder.Func\n", funcSet.getStringToSwapperFile)
    assert_equal(expected, funcSet.getStringOfVariableDefinition)
  end

  def test_makeStubAtTopLevel
    block = RootBlock.new("")
    funcSet = FreeFunctionSet.new(block)

    refArray = []
    [["extern void FuncA(int a);", "FuncA", nil],
     ["extern int FuncB(int a);", "FuncB", "int"],
     ["extern int FuncB(int a, long B);", "FuncB", "int,long"]].each do |line, name, arg|
      ref = TestMockRefClass.new("", name, name, arg, "")
      refArray << ref
      func = FreeFunctionBlock.new(line)
      block.connect(func)
      funcSet.add(func)
    end

    refSet = TestMockRefSetClass.new(true, refArray)
    funcSet.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet))
    assert_true(funcSet.needStub?)

    expectedStub =  "void FuncA(int a) {\n    return;\n}\n\n"
    expectedStub += "int FuncB(int a) {\n    int result = 0;\n    return result;\n}\n\n"
    expectedStub += "int FuncB(int a,long B) {\n    int result = 0;\n    return result;\n}\n\n"

    funcSet.makeStubSet
    assert_equal(expectedStub, funcSet.getStringOfStub)

    expected = "All_Forwarder all_Forwarder;\n"
    funcSet.makeClassSet
    assert_equal(expected, funcSet.getStringOfVariableDefinition)
  end

  def test_makeStubInNamespace
    blockParent = NamespaceBlock.new("namespace A")
    block = NamespaceBlock.new("namespace B")
    blockParent.connect(block)
    funcSet = FreeFunctionSet.new(block)

    ["extern void Func();",
     "extern int Func(int a);",
     "extern int Func(int a, long b);"].each_with_index do |line, i|
      func = FreeFunctionBlock.new(line)
      block.connect(func) if i == 0
      funcSet.add(func)
    end

    ref = TestMockRefClass.new("", "A::B::Func", "Func", nil, "")
    refSet = TestMockRefSetClass.new(true, [ref])
    funcSet.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet))
    assert_true(funcSet.needStub?)

    expectedStub = "namespace A {\nnamespace B {\nvoid Func() {\n    return;\n}\n\n}\n}\n"
    funcSet.makeStubSet
    assert_equal(expectedStub, funcSet.getStringOfStub)

    expected = "All_A_B_Forwarder all_A_B_Forwarder;\n"
    funcSet.makeClassSet
    assert_equal(expected, funcSet.getStringOfVariableDefinition)
  end

  def test_uniqFunctions
    block = NamespaceBlock.new("namespace B")
    funcSet = FreeFunctionSet.new(block)

    ["extern int Func(int a);",
     "extern int Func(int b);"].each do |line|
      func = FreeFunctionBlock.new(line)
      def func.filterByReferenceSet(filter)
        true
      end
      block.connect(func)
      funcSet.add(func)
    end

    refSet = TestMockRefSetClass.new(true, [1])
    funcSet.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet))

    assert_equal(2, funcSet.funcSet.size)
    assert_equal(2, funcSet.undefinedFunctionSet.size)
    funcSet.uniqFunctions
    assert_equal(1, funcSet.funcSet.size)
    assert_equal(1, funcSet.undefinedFunctionSet.size)
  end

  data(
    'none' => ["ClassName", "ClassName"],
    'one' => ["Class_Name", "Class_Name"],
    'multi' => ["Class__Name", "Class_Name"])
  def test_surpressUnderscores(data)
    str, expected = data
    block = NamespaceBlock.new("namespace B")
    funcSet = FreeFunctionSet.new(block)
    assert_equal(expected, funcSet.surpressUnderscores(str))
  end
end

class TestFreeFunctionSwapper < Test::Unit::TestCase
  def test_empty
    swapper = FreeFunctionSwapper.new(nil, [])
    assert_equal("#if !defined(" + Mockgen::Constants::MARK_FOR_GENERATED_CPP + ")\n", swapper.beginPhrase)
    assert_equal("", swapper.body)
    assert_equal("#endif\n", swapper.endPhrase)
  end

  def test_guardOnly
    swapper = FreeFunctionSwapper.new("IN_CPP", [])
    assert_equal("#if !defined(IN_CPP) && !defined(" + Mockgen::Constants::MARK_FOR_GENERATED_CPP + ")\n", swapper.beginPhrase)
    assert_equal("", swapper.body)
    assert_equal("#endif\n", swapper.endPhrase)
  end

  def test_all
    mockClass = Struct.new(:getStringSetToDeclInlineFile)
    funcSetA = mockClass.new(["A1", "a2"])
    funcSetB = mockClass.new(["B1", "b2"])
    funcSetC = mockClass.new(["C1", "c2"])
    swapper = FreeFunctionSwapper.new("IN_CPP", [funcSetA, funcSetB, funcSetC])
    assert_equal("#if !defined(IN_CPP) && !defined(" + Mockgen::Constants::MARK_FOR_GENERATED_CPP + ")\n", swapper.beginPhrase)
    assert_equal("A1B1C1#else\na2b2c2", swapper.body)
    assert_equal("#endif\n", swapper.endPhrase)
  end
end

class TestClassBlockParameterSet < Test::Unit::TestCase
  def test_all
    filterToMock = "a"
    filterToAll = "b"
    parameterSet = ClassBlockParameterSet.new(filterToMock, filterToAll)
    assert_equal(filterToMock, parameterSet.filterToMock)
    assert_equal(filterToAll, parameterSet.filterToAll)
  end
end

class TestClassBlock < Test::Unit::TestCase
  data(
    'base class' => ["class NameC", "NameC", "class", false, []],
    'derived class' => ["class NameC : public Base", "NameC", "class", false, ["Base"]],
    'derived class and namespace' => ["class NameC : public A::Base", "NameC", "class", false, ["::A::Base"]],
    'derived struct' => ["struct NameS : Base", "NameS", "struct", true, ["Base"]])
  def test_initializeAndParse(data)
    line, name, typeName, pub, baseClassNameSet = data

    ["", "template <typename T, size_t S> "].each do |header|
      block = ClassBlock.new(header + line + " {")
      assert_true(block.instance_variable_get(:@valid))
      assert_true(block.canTraverse?)
      assert_true(block.isClass?)
      assert_equal(name, block.getTypename)

      assert_equal(name, block.getNamespace)
      assert_equal(name, block.instance_variable_get(:@name))
      assert_equal(name, block.instance_variable_get(:@uniqueName))
      assert_equal(name, block.instance_variable_get(:@uniqueFilename))
      assert_equal(name, block.keyForParent)

      assert_equal("#{name}#{Mockgen::Constants::CLASS_POSTFIX_MOCK}", block.mockName)
      assert_equal("#{name}#{Mockgen::Constants::CLASS_POSTFIX_DECORATOR}", block.decoratorName)
      assert_equal("#{name}#{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}", block.forwarderName)

      assert_equal(typeName, block.instance_variable_get(:@typename))
      assert_equal(pub, block.instance_variable_get(:@pub))
      assert_equal(!pub, block.instance_variable_get(:@private))
      assert_equal(baseClassNameSet, block.instance_variable_get(:@baseClassNameSet))

      if header.empty?
        assert_nil(block.instance_variable_get(:@templateParam))
      else
        assert_not_nil(block.instance_variable_get(:@templateParam))
      end
    end
  end

  def test_initializeInnerClass
    block = ClassBlock.new("class Outer {")
    innerBlockA = ClassBlock.new("class Inner {")
    innerBlockB = ClassBlock.new("class Name {")
    block.connect(innerBlockA)
    innerBlockA.connect(innerBlockB)

    assert_equal("Outer", block.keyForParent)
    assert_equal("Inner", innerBlockA.keyForParent)
    assert_equal("Name", innerBlockB.keyForParent)

    name = "Outer_in_Inner"
    assert_equal(name, innerBlockA.instance_variable_get(:@uniqueName))
    assert_equal("Outer_Inner", innerBlockA.instance_variable_get(:@uniqueFilename))
    assert_equal("Outer_in_Inner_in_Name", innerBlockB.instance_variable_get(:@uniqueName))
    assert_equal("Outer_Inner_Name", innerBlockB.instance_variable_get(:@uniqueFilename))
    assert_true(block.instance_variable_get(:@localTypeTable).key?("Inner"))
    assert_true(innerBlockA.instance_variable_get(:@localTypeTable).key?("Name"))

    assert_equal("#{name}#{Mockgen::Constants::CLASS_POSTFIX_MOCK}", innerBlockA.mockName)
    assert_equal("#{name}#{Mockgen::Constants::CLASS_POSTFIX_DECORATOR}", innerBlockA.decoratorName)
    assert_equal("#{name}#{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}", innerBlockA.forwarderName)
  end

  # Partial template specialization is not supported
  def test_initializeAndParseTemplate
    block = ClassBlock.new("template <> class NameC<T> {")
    assert_false(block.canTraverse?)
    assert_false(block.isClass?)
    assert_nil(block.keyForParent)
  end

  def test_setUniqueNameAndGetFilenamePostfix
    block = ClassBlock.new("class Name {")
    assert_equal("Name", block.setUniqueName)
    assert_equal("_Name", block.getFilenamePostfix)

    blockA = ClassBlock.new("class Defined {")
    blockA.connect(block)
    assert_equal("Defined_in_Name", block.setUniqueName)
    assert_equal("_Defined_Name", block.getFilenamePostfix)

    blockB = ClassBlock.new("class B {")
    blockB.connect(blockA)
    assert_equal("B_in_Defined_in_Name", block.setUniqueName)
    assert_equal("_B_Defined_Name", block.getFilenamePostfix)

    nsBlock = NamespaceBlock.new("namespace NameSpace")
    nsBlock.connect(blockB)
    assert_equal("NameSpace_in_B_in_Defined_in_Name", block.setUniqueName)
    assert_equal("_NameSpace_B_Defined_Name", block.getFilenamePostfix)
  end

  def test_setUniqueNameSpecial
    block = ClassBlock.new("class outer {")
    innerBlockA = ClassBlock.new("class _inner {")
    assert_equal("_inner", innerBlockA.instance_variable_get(:@uniqueName))
    assert_equal("_inner", innerBlockA.instance_variable_get(:@uniqueFilename))

    block.connect(innerBlockA)
    assert_equal("outer_in_inner", innerBlockA.instance_variable_get(:@uniqueName))
    assert_equal("outer__inner", innerBlockA.instance_variable_get(:@uniqueFilename))
    assert_equal("_outer_inner", innerBlockA.getFilenamePostfix)

    innerBlockB = ClassBlock.new("class _in {")
    innerBlockA.connect(innerBlockB)
    assert_equal("outer_in_inner_in_in", innerBlockB.instance_variable_get(:@uniqueName))
    assert_equal("outer__inner__in", innerBlockB.instance_variable_get(:@uniqueFilename))
    assert_equal("_outer_inner_in", innerBlockB.getFilenamePostfix)
  end

  def test_parseChildrenClass
    block = ClassBlock.new("class NameC")
    assert_true(block.skippingParse)

    assert_nil(block.parseChildren("public:"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_equal(false, block.instance_variable_get(:@private))
    assert_false(block.skippingParse)

    assert_nil(block.parseChildren("protected:"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_equal(false, block.instance_variable_get(:@private))
    assert_true(block.skippingParse)

    assert_nil(block.parseChildren("public :"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_equal(false, block.instance_variable_get(:@private))
    assert_equal([], block.instance_variable_get(:@publicMemberFunctionSet))
    assert_equal([], block.instance_variable_get(:@protectedMemberFunctionSet))
    assert_equal([], block.instance_variable_get(:@allMemberFunctionSet))
    assert_false(block.skippingParse)

    nonFuncLine = "typedef Count int;"
    assert_nil(block.parseChildren(nonFuncLine))

    varLine = "static Count count_;"
    assert_not_nil(block.parseChildren(varLine))
    assert_true(block.instance_variable_get(:@publicMemberFunctionSet).empty?)
    assert_true(block.instance_variable_get(:@protectedMemberFunctionSet).empty?)
    assert_not_equal([], block.instance_variable_get(:@allMemberVariableSet))
    assert_false(block.canMock?)

    funcLine = "void Func();"
    assert_not_nil(block.parseChildren(funcLine))
    assert_false(block.instance_variable_get(:@publicMemberFunctionSet).empty?)
    assert_true(block.instance_variable_get(:@protectedMemberFunctionSet).empty?)
    assert_not_equal([], block.instance_variable_get(:@allMemberFunctionSet))
    assert_true(block.canMock?)

    constructorLine = "NameC();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))
    assert_not_equal([], block.subConstructorSet)
    assert_not_equal([], block.instance_variable_get(:@allConstructorSet))

    destructorLine = "~NameC();"
    assert_not_nil(block.parseChildren(destructorLine))
    assert_not_nil(block.instance_variable_get(:@destructor))

    block.parseChildren("private:")
    funcLine = "void FuncPrivate();"
    assert_not_nil(block.parseChildren(funcLine))
    assert_true(block.instance_variable_get(:@protectedMemberFunctionSet).empty?)

    block.parseChildren("protected:")
    funcLine = "void FuncProtected();"
    assert_not_nil(block.parseChildren(funcLine))
    assert_false(block.instance_variable_get(:@protectedMemberFunctionSet).empty?)
  end

  def test_parseChildrenClassProtected
    block = ClassBlock.new("class NameC")
    block.parseChildren("protected:")

    constructorLine = "NameC();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_equal([], block.instance_variable_get(:@constructorSet))
    assert_not_equal([], block.subConstructorSet)
    assert_not_equal([], block.instance_variable_get(:@allConstructorSet))
  end

  def test_parseChildrenClassPrivate
    block = ClassBlock.new("class NameC")
    assert_true(block.skippingParse)

    varLine = "static Count count_;"
    assert_not_nil(block.parseChildren(varLine))
    assert_equal([], block.instance_variable_get(:@memberVariableSet))
    assert_not_equal([], block.instance_variable_get(:@allMemberVariableSet))
    assert_false(block.canMock?)

    funcLine = "void Func();"
    assert_not_nil(block.parseChildren(funcLine))
    assert_not_equal([], block.instance_variable_get(:@allMemberFunctionSet))
    assert_true(block.canMock?)

    constructorLine = "NameC();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_equal([], block.instance_variable_get(:@constructorSet))
    assert_equal([], block.subConstructorSet)
    assert_not_equal([], block.instance_variable_get(:@allConstructorSet))
  end

  def test_parseTemplateClass
    block = ClassBlock.new("class NameC")
    assert_nil(block.parseChildren("public:"))

    constructorLine = "NameC<T,U>();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))
    assert_not_equal([], block.instance_variable_get(:@allConstructorSet))

    destructorLine = "~NameC<T,U>();"
    assert_not_nil(block.parseChildren(destructorLine))
    assert_not_nil(block.instance_variable_get(:@destructor))

    funcLine = "void Func<T>();"
    assert_nil(block.parseChildren(funcLine))
    assert_true(block.instance_variable_get(:@publicMemberFunctionSet).empty?)

    funcLine = "void Func<T,U>();"
    assert_nil(block.parseChildren(funcLine))
    assert_true(block.instance_variable_get(:@publicMemberFunctionSet).empty?)

    funcLine = "void Func();"
    assert_not_nil(block.parseChildren(funcLine))
    assert_false(block.instance_variable_get(:@publicMemberFunctionSet).empty?)
  end

  # A struct is treated as a class with default public access
  def test_parseChildrenStruct
    block = ClassBlock.new("struct NameS")
    assert_nil(block.parseChildren("protected:"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_equal(false, block.instance_variable_get(:@private))
    assert_nil(block.parseChildren("public:"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_equal(false, block.instance_variable_get(:@private))
    assert_false(block.canMock?)

    constructorLine = "NameS();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))
    assert_true(block.canMock?)

    assert_nil(block.parseChildren("private :"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_equal(true, block.instance_variable_get(:@private))

    funcLine = "void Func();"
    nonFuncLine = "typedef Count int;"
    assert_not_nil(block.parseChildren(funcLine))

    assert_nil(block.parseChildren("public :"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_equal(false, block.instance_variable_get(:@private))
    assert_nil(block.parseChildren(nonFuncLine))
    assert_not_nil(block.parseChildren(funcLine))
  end

  def test_addMemberVariable
    block = ClassBlock.new("class NameC")

    varLine = "static Count count_;"
    varBlock = block.addMemberVariable(varLine)
    assert_not_nil(varBlock)
    assert_true(varBlock.canTraverse?)
    assert_equal(0, block.instance_variable_get(:@memberVariableSet).size)
    assert_equal(1, block.instance_variable_get(:@allMemberVariableSet).size)

    assert_nil(block.parseChildren("public:"))
    varLine = "static int icount_;"
    varBlock = block.addMemberVariable(varLine)
    assert_not_nil(varBlock)
    assert_true(varBlock.canTraverse?)
    assert_equal(1, block.instance_variable_get(:@memberVariableSet).size)
    assert_equal(2, block.instance_variable_get(:@allMemberVariableSet).size)
  end

  data(
    'no spaces' => ["", ""],
    'with spaces' => [" ", " "],
    'end with a semicolon' => [" ", ";"],
    'array' => [" ", "[4]"])
  def test_parseChildrenQuick(data)
    prefix, postfix = data
    parameterSet = ClassBlockParameterSet.new(nil, ClassNameFilter.new([".*"], [], [], []))
    classBlock = ClassBlock.new("class NameC", parameterSet)
    assert_true(classBlock.instance_variable_get(:@noParsingDetail))

    line = "#{prefix}static Count count_#{postfix}"
    parsed, block = classBlock.parseQuick(line)
    assert_true(parsed)
    assert_not_nil(block)
    assert_true(block.canTraverse?)

    block = classBlock.parseChildren(line)
    assert_not_nil(block)
    assert_true(block.canTraverse?)
  end

  data(
    'instance variable' => "Count count_",
    'member function' => ["void Func();"],
    'class function 1' => ["static void Func();"],
    'class function 2' => ["static void Func() {"])
  def test_parseAndDiscardChildrenQuick(data)
    prefix, postfix = data
    parameterSet = ClassBlockParameterSet.new(nil, ClassNameFilter.new([".*"], [], [], []))
    classBlock = ClassBlock.new("class NameC", parameterSet)
    assert_true(classBlock.instance_variable_get(:@noParsingDetail))

    line = "#{prefix}Count count_#{postfix}"
    parsed, block = classBlock.parseQuick(line)
    assert_true(parsed)
    assert_nil(block)

    block = classBlock.parseChildren(line)
    assert_nil(block)
  end

  def test_parseAccess
    block = ClassBlock.new("")

    [["public:", true, true, false],
     ["protected:", true, false, false],
     ["public:", true, true, false],
     ["private:", true, false, true],
     ["publicFunc", false, false, true],
     ["public:", true, true, false]].each do |line, expected, expectedPub, expectedPrivate|
      assert_equal(expected, block.parseAccess(line))
      assert_equal(expectedPub, block.instance_variable_get(:@pub))
      assert_equal(expectedPrivate, block.instance_variable_get(:@private))
    end
  end

  def test_filterByDefinedReferenceSet
    classname = "NameC"
    block = ClassBlock.new("class " + classname)
    assert_false(block.instance_variable_get(:@alreadyDefined))
    assert_false(block.instance_variable_get(:@filteredOut))
    assert_false(block.instance_variable_get(:@defaultNoForwardingToMock))

    def block.formatStub
      "a"
    end

    def block.formatMockClass(a,b,c,d)
      return "b", "B", "bB"
    end

    def block.formatDecoratorClass(a,b,c)
      return "c", "d"
    end

    def block.formatForwarderClass(a,b,c)
      return "e"
    end

    block.makeClassSet
    assert_false(block.getStringToClassFile.empty?)
    block.makeStubSet
    assert_false(block.getStringToSourceFile.empty?)

    refSet = Object.new()
    def refSet.relativeNamespaceOnly
      false
    end

    def refSet.classDefined?(arg, relativeNamespaceOnly)
      true
    end

    block.filterByReferenceSet(MinimumSymbolFilter.new(refSet, nil))
    assert_true(block.instance_variable_get(:@alreadyDefined))
    block.makeClassSet
    assert_true(block.getStringToClassFile.empty?)
    block.makeStubSet
    assert_true(block.getStringToSourceFile.empty?)
  end

  def test_filterByUndefinedReferenceSet
    classname = "NameC"
    block = ClassBlock.new("class " + classname)
    assert_nil(block.parseChildren("public :"))
    funcLineDef = "void FuncDefined();"
    childBlockD = block.parseChildren(funcLineDef)
    block.connect(childBlockD)
    assert_false(block.needStub?)

    funcname = "FuncNotDefined"
    funcLineNonDef = "void #{funcname}(int a) const;"
    childBlockU = block.parseChildren(funcLineNonDef)
    block.connect(childBlockU)
    assert_equal([childBlockD, childBlockU], block.children)
    assert_equal(0, block.instance_variable_get(:@undefinedFunctionSet).size)

    ref = TestMockRefClass.new(classname, funcname, funcname, "int", "const")
    refSet = TestMockRefSetClass.new(true, [ref])

    block.filterByReferenceSetWithSuper(MinimumSymbolFilter.new(nil, refSet))
    assert_equal([childBlockU], block.instance_variable_get(:@undefinedFunctionSet))
    assert_true(block.canTraverse?)
    assert_true(block.needStub?)
  end

  data(
    'do nothing' => [false, false, "", 0],
    'undefined without vtable' => [true, false, "", 6],
    'none of virtual functions is defined with vtable' => [false, true, "", 4],
    'a non virtual function is defined with vtable 1' => [[true, false, false, false, false, false], true, "", 5],
    'all non virtual functions are defined with vtable' => [true, true, "", 6],
    'other class' => [false, true, "A", 0])
  def test_filterByVtable(data)
    undefined, fillVtable, postfix, expected = data

    classname = "Name"
    block = ClassBlock.new("class " + classname)
    assert_equal(0, block.instance_variable_get(:@undefinedFunctionSet).size)

    assert_nil(block.parseChildren("public :"))
    defSet = ["void FuncUndefined1();", "void FuncUndefined2();"]
    undefSet = ["virtual void Undefined();", "virtual void Undefined(int);",
                "virtual void Undefined() const;", "virtual void Undefined2(int);"]
    allSet = [defSet, undefSet].flatten

    allSet.each do |line|
      child = block.parseChildren(line)
      child.define_singleton_method(:filterByReferenceSet) do |filter|
        undefined.kind_of?(Array) ? undefined.shift : undefined
      end
      block.connect(child)
    end

    refSet = TestMockRefVatbleSetClass.new(true, [], classname + postfix)
    filter = MinimumVtableFilter.new(nil, refSet, fillVtable)

    block.filterByReferenceSetWithSuper(filter)
    assert_equal(expected, block.instance_variable_get(:@undefinedFunctionSet).size)
  end

  data(
    'empty' => [[], false, true],
    'exact matching name' => [["NameC"], true, false],
    'partial matching name' => [["Name"], true, false],
    'no matching name' => [["NameD"], false, true],
    'exact matching namespace' => [["Utility::NameC"], false, false],
    'partial matching namespace' => [["lity::Name"], false, false],
    'no matching namespace' => [["CommpnUtility::NameD"], false, true])
  def test_filterByName(data)
    patternSet, expectedFilteredOut, expectedCanMock = data
    classname = "NameC"
    filter = SilentClassNameFilter.new(patternSet)
    block = ClassBlock.new("class " + classname + "{", ClassBlockParameterSet.new(filter, nil))
    assert_equal(expectedFilteredOut, block.instance_variable_get(:@filteredOut))

    assert_nil(block.parseChildren("public :"))
    funcLineDef = "void FuncDefined();"
    childBlockD = block.parseChildren(funcLineDef)
    block.connect(childBlockD)
    nsBlock = NamespaceBlock.new("namespace Utility")
    nsBlock.connect(block)

    block.filterByReferenceSet(SymbolFilter.new(nil, nil, [], patternSet, false, false))
    assert_equal(expectedCanMock, block.canMock?)
  end

  data(
    'empty pattern' => [[], false],
    'empty string' => [["^$"], false],
    'invalid' => [["*(*"], false],
    'valid' => [["Name"], true],
    'invalid and valid' => [["*(*", "Name"], true],
    'valid and invalid' => [["Name", "*(*"], true])
  def test_filterInvalidRegexp(data)
    patternSet, expected = data
    classname = "TargetNameA"
    filter = SilentClassNameFilter.new(patternSet)
    block = ClassBlock.new("class " + classname + "{", ClassBlockParameterSet.new(filter, nil))

    def block.canFilterByReferenceSet(filter)
      false
    end

    filter = Struct.new(:definedReferenceSet, :classNameFilterOutSet).new(nil, patternSet)
    block.filterByReferenceSet(filter)
    assert_equal(expected, block.instance_variable_get(:@filteredOut))
  end

  def test_canFilterByReferenceSet
    block = ClassBlock.new("class NameC")
    assert_false(block.canFilterByReferenceSet(MinimumSymbolFilter.new(nil, nil)))

    ref = TestMockRefMonoSetClass.new(nil)
    assert_true(block.canFilterByReferenceSet(MinimumSymbolFilter.new(nil, ref)))
    ref.invalidate
    assert_false(block.canFilterByReferenceSet(MinimumSymbolFilter.new(nil, ref)))
  end

  data(
    'destructor' => ["~NameC", "~NameC();"],
    'constructor' => ["NameC", "NameC();"],
    'member function' => ["Func", "int Func();"],
    'member variable' => ["a_", "static int a_;"])
  def test_needStub(data)
    memberName, line = data
    classname = "NameC"
    block = ClassBlock.new("class " + classname)
    # private
    memberBlock = block.parseChildren(line)
    block.connect(memberBlock)
    assert_false(block.needStub?)

    if memberName.include?("~")
      def memberBlock.filterByUndefinedReferenceSet(ref, fullname)
        true
      end
    else
      def memberBlock.filterByReferenceSet(filter)
        true
      end
    end

    ref = TestMockRefClass.new(classname, "#{classname}::#{memberName}", memberName, "", "")

    filter = MinimumSymbolFilter.new(nil, TestMockRefMonoSetClass.new(ref))
    block.filterByReferenceSet(filter)
    block.filterByReferenceSetWithSuper(filter)
    assert_true(block.needStub?)
    assert_true(block.canMock?)
  end

  def test_getFullname
    nsBlock = NamespaceBlock.new("namespace A")
    block = ClassBlock.new("")
    nsBlock.connect(block)

    block.instance_variable_set(:@name, "ClassName")
    assert_equal("::A::ClassName", block.getFullname)

    param = Object.new
    def param.generic
      "size_t N"
    end

    block.instance_variable_set(:@templateParam, param)
    assert_equal("template <size_t N> ::A::ClassName", block.getFullname)
  end

  def test_setBaseClass
    block = ClassBlock.new("class Derived : public A::Name, public B::Name")
    table = {"::A::Name" => "a", "::B::Name" => "b", "Name" => "x"}
    block.setBaseClass(table)
    assert_equal(["a", "b"], block.instance_variable_get(:@baseClassBlockSet))
  end

  data(
    'trivially constructive' => [[], 0],
    'explicit no args' => [["void"], 0],
    'one arg' => [[", int"], 1],
    'two arg' => [[", int, long"], 2],
    'can default constructive' => [["", ", int, T* p"], 0],
    'at least one arg' => [[", int", ", int, T* p"], 1])
  def test_getConstructorArity(data)
    argStrSet, expected = data
    ctorClass = Struct.new(:getTypedArgsForBaseClass)
    ctorSet = argStrSet.map { |argStr| ctorClass.new(argStr) }

    block = ClassBlock.new("")
    block.instance_variable_set(:@constructorSet, ctorSet)
    assert_equal(expected, block.getConstructorArity)
  end

  def test_getConstructorArgStrSet
    baseBlock1 = ClassBlock.new("class Base1")
    baseBlock1.parseChildren("public:")
    baseBlock1.parseChildren("Base1(int a);")

    ctorBlock = baseBlock1.subConstructorSet[0]
    assert_equal("", baseBlock1.getConstructorArgStrSet(ctorBlock))

    baseBlock2 = ClassBlock.new("class Base2")
    baseBlock2.parseChildren("protected:")
    baseBlock2.parseChildren("Base2(int b, long c);")

    baseBlock3 = ClassBlock.new("class Base3")
    baseBlock3.parseChildren("protected:")
    baseBlock3.parseChildren("Base3(void);")

    baseBlock4 = ClassBlock.new("class Base4")

    derivedBlock = ClassBlock.new("class Derived : public Base1, public Base2, public Base3, public Base4")
    ["public:", "Derived(int d, long e);"].each do |line|
      derivedBlock.parseChildren(line)
    end

    table = {"Base1" => baseBlock1, "Base2" => baseBlock2, "Base3" => baseBlock3,
             "Base4" => baseBlock4, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    ctorBlock = derivedBlock.subConstructorSet[0]
    assert_equal(": Base1(d), Base2(d,e) ", derivedBlock.getConstructorArgStrSet(ctorBlock))
  end

  def test_makeConstructorArgStrVoid
    baseBlock = ClassBlock.new("class Base")
    baseBlock.parseChildren("protected:")
    baseBlock.parseChildren("Base(void);")

    derivedBlock = ClassBlock.new("class Derived : public Base")
    ["public:", "Derived();", "Derived(int d);", "Derived(int d, char* p);"].each do |line|
      derivedBlock.parseChildren(line)
    end

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    derivedBlock.subConstructorSet.each do |ctorBlock|
      assert_nil(derivedBlock.makeConstructorArgStr(ctorBlock, baseBlock))
    end
  end

  def test_makeConstructorArgStrOne
    baseBlock = ClassBlock.new("class Base")
    baseBlock.parseChildren("protected:")
    baseBlock.parseChildren("Base(int b);")

    derivedBlock = ClassBlock.new("class Derived : public Base")
    derivedBlock.parseChildren("public:")

    testSet = [["Derived(void);", nil], ["Derived(int d);", "Base(d)"],
               ["Derived(int);", "Base(dummy1)"], ["Derived(int d, char* p);", "Base(d)"],
               ["Derived(long d);", nil]]
    testSet.each do |line, expected|
      derivedBlock.parseChildren(line)
    end

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    testSet.each_with_index do |expected, i|
      ctorBlock = derivedBlock.subConstructorSet[i]
      assert_equal(expected[1], derivedBlock.makeConstructorArgStr(ctorBlock, baseBlock))
    end
  end

  def test_makeConstructorArgStrMulti
    baseBlock = ClassBlock.new("class Base")
    baseBlock.parseChildren("protected:")
    baseBlock.parseChildren("Base(int b1, int b2, void(*f)(int));")

    derivedBlock = ClassBlock.new("class Derived : public Base")
    derivedBlock.parseChildren("public:")

    testSet = [["Derived(int d1);", nil],
               ["Derived(int d1, int d2, char* p);", nil],
               ["Derived(int d1, int d2, void(*g)(int));", "Base(d1,d2,g)"],
               ["Derived(int d1, int d2, void(*g)(long));", nil],
               ["Derived(int d1, int d2, void(*g)(int), long h);", "Base(d1,d2,g)"]]
    testSet.each do |line, expected|
      derivedBlock.parseChildren(line)
    end

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    testSet.each_with_index do |expected, i|
      ctorBlock = derivedBlock.subConstructorSet[i]
      assert_equal(expected[1], derivedBlock.makeConstructorArgStr(ctorBlock, baseBlock))
    end
  end

  def test_makeConstructorArgStrMatching
    baseBlock = ClassBlock.new("class Base")
    ["protected:", "Base(int b1);", "Base(int b1, int b2);",
     "Base(long b1);", "Base(long b1, long b2);"].each do |line|
      baseBlock.parseChildren(line)
    end

    derivedBlock = ClassBlock.new("class Derived : public Base")
    derivedBlock.parseChildren("public:")

    testSet = [["Derived(char d1);", nil],
               ["Derived(int d1);", "Base(d1)"],
               ["Derived(int d1, long d2);", "Base(d1)"],
               ["Derived(int d1, int* d2);", "Base(d1)"],
               ["Derived(int d1, int d2);", "Base(d1,d2)"],
               ["Derived(int d1, int d2, int d3);", "Base(d1,d2)"],
               ["Derived(long d1);", "Base(d1)"],
               ["Derived(long d1, int d2);", "Base(d1)"],
               ["Derived(long d1, long d2);", "Base(d1,d2)"],
               ["Derived(long d1, long d2, long d3);", "Base(d1,d2)"]]
    testSet.each do |line, expected|
      derivedBlock.parseChildren(line)
    end

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    testSet.each_with_index do |expected, i|
      ctorBlock = derivedBlock.subConstructorSet[i]
      assert_equal(expected[1], derivedBlock.makeConstructorArgStr(ctorBlock, baseBlock))
    end
  end

  def test_makeConstructorArgStrTypedef
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    ["typedef int * PtrA;", "typedef long * PtrB;"].each do |line|
      rootBlock.connect(TypedefBlock.new(line))
    end

    baseBlock = ClassBlock.new("class Base")
    baseBlock.parseChildren("protected:")
    baseBlock.parseChildren("Base(PtrA pA, long* pB);")
    rootBlock.connect(baseBlock)

    derivedBlock = ClassBlock.new("class Derived : public Base")
    derivedBlock.parseChildren("public:")
    derivedBlock.parseChildren("Derived(int* a, PtrB b, int c);")
    rootBlock.connect(derivedBlock)
    rootBlock.collectAliases

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    ctorBlock = derivedBlock.subConstructorSet[0]
    assert_equal("Base(a,b)", derivedBlock.makeConstructorArgStr(ctorBlock, baseBlock))
  end

  def test_makeConstructorArgStrFuncPtrTypedef
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    rootBlock.connect(TypedefBlock.new("typedef int MyInt;"))

    baseBlock = ClassBlock.new("class Base")
    baseBlock.parseChildren("protected:")
    baseBlock.parseChildren("Base(void(*pB)(MyInt));")
    rootBlock.connect(baseBlock)

    derivedBlock = ClassBlock.new("class Derived : public Base")
    derivedBlock.parseChildren("public:")
    derivedBlock.parseChildren("Derived(void(*pD)(MyInt))")
    rootBlock.connect(derivedBlock)
    rootBlock.collectAliases

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)

    ctorBlock = derivedBlock.subConstructorSet[0]
    assert_equal("Base(pD)", derivedBlock.makeConstructorArgStr(ctorBlock, baseBlock))
  end

  data(
    'empty' => ["", "", true],
    'containts empty' => ["int", "", true],
    'exact' => ["int", "int", true],
    'pointer 1' => ["int*", "int", false],
    'pointer 2' => ["int", "int*", false],
    'subset'    => ["int, long", "int", true],
    'pointer 3' => ["int, long*", "int, long", false],
    'pointer 4' => ["int, long**", "int, long*", false],
    'pointer 5' => ["int*, long", "int, long", false],
    'pointer to a function 1' => ["void(*)(void)", "void(*)(void)", true],
    'pointer to a function 2-1' => ["int, void(*)(int, long)", "int, void(*)(int, long)", true],
    'pointer to a function 2-2' => ["int, void(*)(int, long)", "int", true],
    'pointer to a function 3-1' => ["void(*)(int, long), int", "void(*)(int, long), int", true],
    'pointer to a function 3-2' => ["void(*)(int, long), int", "void(*)(int, long)", true],
    'pointer to a function 4' => ["int, void(*)(int, long)", "int, void(*)(int)", false],
    'pointer to a function 5' => ["int, void(*)(int, long*)", "int, void(*)(int, long)", false])
  def test_containArgList(data)
    outer, inner, expected = data
    block = ClassBlock.new("")
    assert_equal(expected, block.containArgList?(outer, inner))
  end

  def test_makeClassSetForExcludedClass
    parameterSet = ClassBlockParameterSet.new(nil, ClassNameFilter.new([".*"], [], [], []))
    block = ClassBlock.new("class NameC", parameterSet)
    called = false
    block.define_singleton_method(:makeStubSet) { called = true }
    block.makeClassSet
    assert_true(called)
  end

  data(
    'class' => ["class ClassName", false, "ClassName", "class", false],
    'struct' => ["struct StructName", false, "StructName", "struct", true],
    'template class' => ["template <typename T> class ClassName",
                         true, "ClassName", "class", false],
    'template struct' => ["template <typename T, typename S> struct StructName",
                          true, "StructName", "struct", true],
    'template class ::' => ["template <typename T = Name::Class> class ClassName",
                            true, "ClassName", "class", false])
  def test_parseClassName(data)
    line, isTemplate, name, typename, pub = data
    block = ClassBlock.new("")
    assert_true(block.parseClassName(line))

    if isTemplate
      assert_not_nil(block.instance_variable_get(:@templateParam))
    else
      assert_nil(block.instance_variable_get(:@templateParam))
    end

    assert_equal(name, block.instance_variable_get(:@name))

    assert_equal(name + Mockgen::Constants::CLASS_POSTFIX_MOCK, block.instance_variable_get(:@mockName))
    assert_equal(name + Mockgen::Constants::CLASS_POSTFIX_DECORATOR, block.instance_variable_get(:@decoratorName))
    assert_equal(name + Mockgen::Constants::CLASS_POSTFIX_FORWARDER, block.instance_variable_get(:@forwarderName))

    assert_equal(typename, block.instance_variable_get(:@typename))
    assert_equal(pub, block.instance_variable_get(:@pub))
    assert_equal(!pub, block.instance_variable_get(:@private))
  end

  data(
    'class' => ["class", false],
    'template class' => ["template <typename T> class", true])
  def test_parseClassNameWithExtraWord(data)
    line, isTemplate = data
    [" ", " attr_added", " attr added"].each do |extra|
      name = "ClassName"
      block = ClassBlock.new("")
      assert_true(block.parseClassName(line + extra + " " + name))
      assert_equal(name, block.instance_variable_get(:@name))

      if isTemplate
        assert_not_nil(block.instance_variable_get(:@templateParam))
      else
        assert_nil(block.instance_variable_get(:@templateParam))
      end
    end
  end

  def test_parseClassNameExcluded
    block = ClassBlock.new("")
    assert_false(block.parseClassName("class $$$"))
  end

  data(
    'extern' => "extern class ClassName",
    'union' => "union UnionName")
  def test_parseNonClassName(data)
    line = data
    block = ClassBlock.new("")
    assert_false(block.parseClassName(line))
  end

  data(
    'empty' => ["", []],
    'implicit private' => [" : BaseClass", []],
    'explicit private' => [" : private BaseClass", []],
    'protected' => [" : protected BaseClass", []],
    'public' => [" : public BaseClass", ["BaseClass"]],
    'multuple1' => [" : public Base1, public Base2", ["Base1", "Base2"]],
    'multuple2' => [":public Base1,public Base2", ["Base1", "Base2"]],
    'mixed' => [" : Base1, public Base2", ["Base2"]])
  def test_parseClassInheritance(data)
    line, expected = data
    block = ClassBlock.new("")
    assert_equal(expected, block.parseInheritance("class Name" + line))
  end

  data(
    'empty' => ["", []],
    'implicit public' => [" : BaseClass", ["BaseClass"]],
    'exlicit public' => [" : public BaseClass", ["BaseClass"]],
    'protected' => [" : protected BaseClass", []],
    'private' => [" : private BaseClass", []],
    'mixed' => [" : Base1, public Base2, private Base3", ["Base1", "Base2"]])
  def test_parseStructInheritance(data)
    line, expected = data
    block = ClassBlock.new("")
    block.instance_variable_set(:@typename, "struct")
    assert_equal(expected, block.parseInheritance("struct Name" + line))
  end

  data(
    'void1' => "NameC()",
    'void2' => " NameC () = default",
    'void3' => " NameC ( void ) = default",
    'one' => "NameC(int a)",
    'two' => " NameC ( int a, const void* p )",
    'template' => " NameC<T>(T& arg)")
  def test_isConstructor?(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_true(block.isConstructor?(line))
  end

  data(
    'explicit 1' => "explicit NameC()",
    'explicit 2' => " explicit  NameC()",
    'inline 1' => "inline NameC()",
    'inline 2' => "  inline NameC()",
    'explicit inline' => "  explicit  inline  NameC()",
    'inline explicit' => "inline explicit NameC()")
  def test_isExplicitConstructor?(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_true(block.isConstructor?(line))
  end

  data(
    'dtor1' => "~NameC()",
    'dtor2' => "~NameC ( void ) ",
    'member' => "int NameCFunc(int a)")
  def test_isNotConstructor(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_false(block.isConstructor?(line))
  end

  data(
    'no args' => 'int (*f)() {',
    'args' => 'int (*f)(int a, int b);',
    'nested args' => 'int (*f)(decltype(A) a, int b)',
    'decltype result' => '(decltype X)(*f)(decltype(A) a, int b)')
  def test_isPointerToFunction?(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_true(block.isPointerToFunction?(line))
  end

  data(
    'return pointer1' => 'decltype(A)* f(int a)',
    'return pointer2' => 'decltype(A) *f(int a)')
  def test_isNotPointerToFunction(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_false(block.isPointerToFunction?(line))
  end

  def test_getNonTypedFullname
    nsBlockA = NamespaceBlock.new("namespace R::A")
    nsBlockB = NamespaceBlock.new("namespace B")
    nsBlockC = NamespaceBlock.new("namespace C")
    block = ClassBlock.new("")

    name = "ClassName"
    assert_equal("ClassName", block.getNonTypedFullname(name))
    nsBlockB.connect(block)
    nsBlockB.connect(nsBlockC)
    assert_equal("::B::ClassName", block.getNonTypedFullname(name))

    nsBlockA.connect(nsBlockB)
    assert_equal("::R::A::B::ClassName", block.getNonTypedFullname(name))
  end

  def test_markMemberFunctionSetVirtualShort
    line = "void Func();"
    funcBaseLine = "virtual " + line
    funcDerivedLine = line
    funcBase = MemberFunctionBlock.new(funcBaseLine)
    funcDerived = MemberFunctionBlock.new(funcDerivedLine)
    blockBase = ClassBlock.new("class Base")
    blockDerived = ClassBlock.new("class Derived")
    blockDerived.instance_variable_set(:@baseClassBlockSet, [blockBase])

    blockBase.connect(funcBase)
    blockDerived.connect(funcDerived)
    addMemberFunction(blockBase, funcBase)
    addMemberFunction(blockDerived, funcDerived)
    assert_true(funcBase.virtual?)
    assert_false(funcDerived.virtual?)

    str = blockDerived.markMemberFunctionSetVirtual
    assert_true(funcBase.virtual?)
    assert_true(funcDerived.virtual?)
  end

  def test_markMemberFunctionSetVirtualFull
    linePub = "void FuncPub();"
    linePubOther = "void FuncOther();"
    lineProtected = "void FuncProtected();"
    linePrivate = "void FuncPrivate();"
    lineProtectedImpl = "void protectedImpl();"
    linePrivateImpl = "void privateImpl();"

    # NVI idiom
    blockBase = ClassBlock.new("class Base")
    blockBase.parseChildren("public:")
    blockBase.parseChildren(linePub)
    blockBase.parseChildren("protected:")
    blockBase.parseChildren("virtual " + lineProtected)
    blockBase.parseChildren(lineProtectedImpl)
    blockBase.parseChildren("private:")
    blockBase.parseChildren("virtual " + linePrivate)
    blockBase.parseChildren(linePrivateImpl)

    blockDerived = ClassBlock.new("class Derived")
    blockDerived.parseChildren("public:")
    blockDerived.parseChildren(linePubOther)

    blockDerived.parseChildren("protected:")
    # Omit virtual keyword but it is valid
    blockDerived.parseChildren(lineProtected)
    # Hiding base class definition
    blockDerived.parseChildren(lineProtectedImpl)

    blockDerived.parseChildren("private:")
    # Omit virtual keyword but it is valid
    blockDerived.parseChildren(linePrivate)
    # Hiding base class definition
    blockDerived.parseChildren(linePrivateImpl)
    blockDerived.instance_variable_set(:@baseClassBlockSet, [blockBase])

    [[["FuncOther"], :@publicMemberFunctionSet],
     [["FuncProtected", "protectedImpl"], :@protectedMemberFunctionSet]
    ].each do |expectedNameSet, varName|
      assert_equal(expectedNameSet, blockDerived.instance_variable_get(varName).map(&:funcName))
    end

    [[["FuncPub"], :@publicMemberFunctionSet],
     [["FuncProtected", "protectedImpl"], :@protectedMemberFunctionSet]
    ].each do |expectedNameSet, varName|
      assert_equal(expectedNameSet, blockBase.instance_variable_get(varName).map(&:funcName))
    end

    blockDerived.markMemberFunctionSetVirtual
    expected = [["FuncOther", false], ["FuncProtected", true], ["protectedImpl", false],
                ["FuncPrivate", true], ["privateImpl", false]]
    blockDerived.instance_variable_get(:@allMemberFunctionSet).each_with_index do |func, i|
      assert_equal(expected[i][0], func.funcName)
      assert_equal(expected[i][1], func.virtual?)
    end

    expected = [["FuncPub", false], ["FuncProtected", true], ["protectedImpl", false],
                ["FuncPrivate", true], ["privateImpl", false]]
    blockBase.instance_variable_get(:@allMemberFunctionSet).each_with_index do |func, i|
      assert_equal(expected[i][0], func.funcName)
      assert_equal(expected[i][1], func.virtual?)
    end
  end

  data(false, true)
  def test_setDefaultNoForwardingToMock(data)
    arg = data
    block = ClassBlock.new("class NameC")
    block.parseChildren("public:")
    funcBlock = block.parseChildren("void Func();")

    block.setDefaultNoForwardingToMock(arg)
    assert_equal(arg, block.instance_variable_get(:@defaultNoForwardingToMock))
    assert_equal(arg, funcBlock.instance_variable_get(:@defaultNoForwardingToMock))
  end

  def test_canForwardToDecorateMockFunction
    block = ClassBlock.new("class Name")
    funcBlock = BaseBlock.new("")
    def funcBlock.virtual?
      false
    end

    assert_false(block.canForwardToFunction(funcBlock))
    assert_false(block.canDecorateFunction(funcBlock))
    assert_false(block.canMockFunction(funcBlock))

    def funcBlock.canTraverse?
      true
    end
    assert_false(block.canForwardToFunction(funcBlock))
    assert_false(block.canDecorateFunction(funcBlock))
    assert_false(block.canMockFunction(funcBlock))

    block.instance_variable_set(:@publicMemberFunctionSet, [funcBlock])
    assert_true(block.canForwardToFunction(funcBlock))
    assert_true(block.canDecorateFunction(funcBlock))
    assert_true(block.canMockFunction(funcBlock))

    block.instance_variable_set(:@publicMemberFunctionSet, [])
    block.instance_variable_set(:@protectedMemberFunctionSet, [funcBlock])
    assert_false(block.canForwardToFunction(funcBlock))
    assert_true(block.canDecorateFunction(funcBlock))
    assert_true(block.canMockFunction(funcBlock))

    block.instance_variable_set(:@protectedMemberFunctionSet, [])
    def funcBlock.virtual?
      true
    end
    assert_false(block.canForwardToFunction(funcBlock))
    assert_false(block.canDecorateFunction(funcBlock))
    assert_true(block.canMockFunction(funcBlock))
  end

  def addMemberFunction(block, func)
    block.instance_variable_get(:@publicMemberFunctionSet) << func
    block.instance_variable_get(:@allMemberFunctionSet) << func
  end

  def setMemberFunction(block, funcSet)
    funcSet.each do |func|
      block.connect(func)
    end
    block.instance_variable_get(:@publicMemberFunctionSet).concat(funcSet)
    block.instance_variable_get(:@allMemberFunctionSet).concat(funcSet)
  end

  def setMemberVariable(block, varSet)
    varSet.each do |var|
      block.connect(var)
    end
    block.instance_variable_get(:@memberVariableSet).concat(varSet)
    block.instance_variable_get(:@allMemberVariableSet).concat(varSet)
  end

  def getClassBlockToFormat(baseName, trivialConstructor)
    name = "Tested"
    block = ClassBlock.new("class #{name} : public Base")
    blockBase = ClassBlock.new("class #{baseName}")
    table = { "Base" => blockBase }
    block.setBaseClass(table)

    unless trivialConstructor
      block.parseChildren("public:")
      constructorVoid = block.parseChildren("Tested(void);")
      constructorArg1 = block.parseChildren("Tested(int a);")

      [constructorVoid, constructorArg1].each do |constructor|
        constructor.setBaseClassName(name)
        block.connect(constructor)
      end
    end

    func = MemberFunctionBlock.new("void Func()")
    funcConst = MemberFunctionBlock.new("virtual void Func() const override")
    funcOverloaded = MemberFunctionBlock.new("void Func(int a)")
    funcBaseLine = MemberFunctionBlock.new("int Other(long a, T* b)")

    setMemberFunction(block, [func])
    setMemberFunction(blockBase, [func, funcConst, funcOverloaded ,funcBaseLine])
    block
  end

  def getClassBlockWithConstructorArgs(name)
    block = ClassBlock.new("class #{name}")
    block.parseChildren("public:")
    funcVoid = block.parseChildren("#{name}(void)")
    funcArg1 = block.parseChildren("#{name}(int a)")
    funcArg2 = block.parseChildren("#{name}(int a, int b = 0)")

    [funcVoid, funcArg1, funcArg2].each do |func|
      func.setBaseClassName(name)
      block.connect(func)
    end
    block
  end

  def getClassBlockWithStub(name, baseName)
    block = ClassBlock.new("class #{name} : public #{baseName}")
    blockBase = ClassBlock.new("class #{baseName}")
    table = { "Base" => blockBase }
    block.setBaseClass(table)

    func = MemberFunctionBlock.new("void FuncStub()")
    funcConst = MemberFunctionBlock.new("void FuncStub() const override")
    funcOther = MemberFunctionBlock.new("void FuncOther(int a)")
    funcDefaultValue = MemberFunctionBlock.new("void FuncDefaultValue(int a, int b = 0)")
    varStub = MemberVariableStatement.new("static int varStub")
    varOther = MemberVariableStatement.new("static int varOther")

    setMemberFunction(block, [func, funcConst, funcOther, funcDefaultValue])
    setMemberFunction(blockBase, [func, funcConst, funcOther, funcDefaultValue])
    setMemberFunction(block, [func, funcConst, funcOther])
    setMemberFunction(blockBase, [func, funcConst, funcOther])
    setMemberVariable(block, [varStub, varOther])
    setMemberVariable(blockBase, [varStub, varOther])
    return block, blockBase
  end

  def getClassBlockWithDefault(name, baseName)
    block = ClassBlock.new("class #{name} : public #{baseName}")
    blockBase = ClassBlock.new("class #{baseName}")
    table = { "Base" => blockBase }
    block.setBaseClass(table)

    func = MemberFunctionBlock.new("void FuncStub(NameSpace::Enum e = first);")
    setMemberFunction(block, [func]);
    setMemberFunction(blockBase, [func]);
    return block, blockBase
  end

  def getTemplateClassBlock(name)
    block = ClassBlock.new("template <typename T> class #{name} {")
    assert_not_nil(block.instance_variable_get(:@templateParam))

    block.parseChildren("public:")
    constructorVoid = block.parseChildren("#{name}<T>(void);")
    constructorArg1 = block.parseChildren("#{name}<T>(int a);")
    memfunc = block.parseChildren("virtual void Func(void);")

    [constructorVoid, constructorArg1].each do |func|
      func.setBaseClassName(name)
    end

    [constructorVoid, constructorArg1, memfunc].each do |func|
      block.connect(func)
    end

    block
  end

  def test_formatMockClass
    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"
    block = getClassBlockToFormat(baseName, true)

    actualDecl, actualHpp, actualCpp = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    expected =  "class #{decoratorName};\n"
    expected += "class #{forwarderName};\n"
    expected += "class #{mockName} : public #{testedName} {\n"
    expected += "public:\n"
    expected += "    #{mockName}(#{decoratorName}* pDecorator);\n"
    expected += "    #{mockName}(#{decoratorName}& decorator);\n"
    expected += "    #{mockName}(#{forwarderName}* pForwarder);\n"
    expected += "    #{mockName}(#{forwarderName}& forwarder);\n"
    expected += "    ~#{mockName}(void);\n"
    expected += "    MOCK_METHOD0(Func,void());\n"
    expected += "    MOCK_CONST_METHOD0(Func,void());\n"
    expected += "    MOCK_METHOD1(Func,void(int a));\n"
    expected += "    MOCK_METHOD2(Other,int(long a,T* b));\n"
    expected += "private:\n"
    expected += "    Decorator* pDecorator_;\n"
    expected += "    Forwarder* pForwarder_;\n"
    expected += "};\n\n"
    assert_equal(expected, actualDecl)

    expected =  "#{mockName}::#{mockName}(#{decoratorName}* pDecorator) : " +
                "pDecorator_(pDecorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "#{mockName}::#{mockName}(#{decoratorName}& decorator) : " +
                "pDecorator_(&decorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "#{mockName}::#{mockName}(#{forwarderName}* pForwarder) : " +
                "pDecorator_(0), pForwarder_(pForwarder) { pForwarder_->pMock_ = this; }\n"
    expected += "#{mockName}::#{mockName}(#{forwarderName}& forwarder) : " +
                "pDecorator_(0), pForwarder_(&forwarder) "
    expected += "{ pForwarder_->pMock_ = this; }\n"
    expected += "#{mockName}::~#{mockName}(void) {\n" +
                "    if (pDecorator_ && (pDecorator_->pMock_ == this)) { pDecorator_->pMock_ = 0; }\n"
    expected += "    if (pDecorator_ && (pDecorator_->pClassMock_ == this)) { pDecorator_->pClassMock_ = 0; }\n"
    expected += "    if (pForwarder_ && (pForwarder_->pMock_ == this)) { pForwarder_->pMock_ = 0; }\n}\n\n"
    assert_equal("", actualHpp)
    assert_equal(expected, actualCpp)
  end

  def test_formatMockClassWithArgs
    name = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block = getClassBlockWithConstructorArgs(name)
    actualDecl, actualHpp, actualCpp = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    ["#{mockName}(#{decoratorName}* pDecorator)",
     "#{mockName}(#{decoratorName}& decorator)",
     "#{mockName}(#{forwarderName}* pForwarder)",
     "#{mockName}(#{forwarderName}& forwarder)",
     "#{mockName}(#{decoratorName}* pDecorator, int a)",
     "#{mockName}(#{decoratorName}& decorator, int a)",
     "#{mockName}(#{forwarderName}* pForwarder, int a)",
     "#{mockName}(#{forwarderName}& forwarder, int a)"].each do |expected|
      assert_true(actualDecl.include?(expected))
      assert_equal("", actualHpp)
      assert_true(actualCpp.include?(expected))
    end

    ["#{mockName}(#{decoratorName}* pDecorator, int a,int b = 0)",
     "#{mockName}(#{decoratorName}& decorator, int a,int b = 0)",
     "#{mockName}(#{forwarderName}* pForwarder, int a,int b = 0)",
     "#{mockName}(#{forwarderName}& forwarder, int a,int b = 0)"].each do |expected|
      assert_true(actualDecl.include?(expected))
    end

    ["#{mockName}(#{decoratorName}* pDecorator, int a,int b)",
     "#{mockName}(#{decoratorName}& decorator, int a,int b)",
     "#{mockName}(#{forwarderName}* pForwarder, int a,int b)",
     "#{mockName}(#{forwarderName}& forwarder, int a,int b)"].each do |expected|
      assert_true(actualCpp.include?(expected))
    end

    ["#{testedName}(a), pDecorator_(pDecorator), pForwarder_(0)",
     "#{testedName}(a), pDecorator_(&decorator), pForwarder_(0)",
     "#{testedName}(a), pDecorator_(0), pForwarder_(pForwarder)",
     "#{testedName}(a), pDecorator_(0), pForwarder_(&forwarder)",
    ].each do |expected|
      assert_true(actualCpp.include?(expected))
    end
  end

  data(
    'non const' => ["", "void Tested::FuncStub() {\n    return;\n}\n"],
    'const' => ["const", "void Tested::FuncStub() const {\n    return;\n}\n"])
  def test_formatMockClassWithStub(data)
    postFunc, expected = data
    expectedFuncStub = "Tested::~Tested(void) {}\n" + expected
    expectedVarStub = "int BaseClass::varStub;\n"
    expectedStub = expectedFuncStub + expectedVarStub

    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block, blockBase = getClassBlockWithStub(testedName, baseName)
    refFunc = TestMockRefClass.new(testedName, "#{testedName}::FuncStub", "FuncStub", "", postFunc)
    refDestructor = TestMockRefClass.new(testedName, "#{testedName}::~#{testedName}", "~#{testedName}", "", postFunc)
    refVar = TestMockRefClass.new(testedName, "#{testedName}::varStub", "varStub", "", "")
    refSet = TestMockRefSetClass.new(true, [refFunc, refDestructor, refVar])

    filter = MinimumSymbolFilter.new(nil, refSet)
    block.filterByReferenceSet(filter)
    block.filterByReferenceSetWithSuper(filter)

    actualDecl, actualHpp, actualCpp = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    assert_equal(expectedStub, block.formatStub)

    # Filter the derived class
    expected =  "class #{decoratorName};\n"
    expected += "class #{forwarderName};\n"
    expected += "class #{mockName} : public #{testedName} {\n"
    expected += "public:\n"
    expected += "    #{mockName}(#{decoratorName}* pDecorator);\n"
    expected += "    #{mockName}(#{decoratorName}& decorator);\n"
    expected += "    #{mockName}(#{forwarderName}* pForwarder);\n"
    expected += "    #{mockName}(#{forwarderName}& forwarder);\n"
    expected += "    ~#{mockName}(void);\n"
    expected += "    MOCK_METHOD0(FuncStub,void());\n"
    expected += "    MOCK_CONST_METHOD0(FuncStub,void());\n"
    expected += "    MOCK_METHOD1(FuncOther,void(int a));\n"
    expected += "    MOCK_METHOD2(FuncDefaultValue,void(int a,int b));\n"
    expected += "private:\n"
    expected += "    Decorator* pDecorator_;\n"
    expected += "    Forwarder* pForwarder_;\n"
    expected += "};\n\n"
    assert_equal(expected, actualDecl)

    expected =  "#{mockName}::#{mockName}(#{decoratorName}* pDecorator) : " +
                "pDecorator_(pDecorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "#{mockName}::#{mockName}(#{decoratorName}& decorator) : " +
                "pDecorator_(&decorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "#{mockName}::#{mockName}(#{forwarderName}* pForwarder) : " +
                "pDecorator_(0), pForwarder_(pForwarder) { pForwarder_->pMock_ = this; }\n"
    expected += "#{mockName}::#{mockName}(#{forwarderName}& forwarder) : " +
                "pDecorator_(0), pForwarder_(&forwarder) "
    expected += "{ pForwarder_->pMock_ = this; }\n"
    expected += "#{mockName}::~#{mockName}(void) {\n" +
                "    if (pDecorator_ && (pDecorator_->pMock_ == this)) { pDecorator_->pMock_ = 0; }\n"
    expected += "    if (pDecorator_ && (pDecorator_->pClassMock_ == this)) { pDecorator_->pClassMock_ = 0; }\n"
    expected += "    if (pForwarder_ && (pForwarder_->pMock_ == this)) { pForwarder_->pMock_ = 0; }\n}\n\n"
    expected += expectedFuncStub
    expected += expectedVarStub
    assert_equal("", actualHpp)
    assert_equal(expected, actualCpp)
    assert_true(block.canTraverse?)

    # Do not filter the base class
    filter = MinimumSymbolFilter.new(nil, refSet)
    blockBase.filterByReferenceSet(filter)
    blockBase.filterByReferenceSetWithSuper(filter)

    actualDecl, actualHpp, actualCpp = blockBase.formatMockClass(mockName, decoratorName, forwarderName, baseName)
    assert_equal(2, actualDecl.scan("FuncStub").size)
    assert_equal(1, actualDecl.scan("FuncOther").size)
    assert_equal(1, actualCpp.scan("FuncStub").size)
    assert_equal("", actualHpp)
    assert_false(actualCpp.include?("FuncOther"))
    assert_true(blockBase.canTraverse?)
  end

  def test_formatStubClassWithConstructor
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    rootBlock.connect(TypedefBlock.new("typedef int MyInt;"))

    baseBlock = ClassBlock.new("class Base")
    baseBlock.parseChildren("protected:")
    baseBlock.parseChildren("Base(MyInt b);")
    rootBlock.connect(baseBlock)

    derivedBlock = ClassBlock.new("class Derived : public Base")
    derivedBlock.parseChildren("public:")
    line = "Derived(int d);"
    derivedBlock.parseChildren(line)
    rootBlock.connect(derivedBlock)
    rootBlock.collectAliases

    table = {"Base" => baseBlock, "Derived" => derivedBlock}
    derivedBlock.setBaseClass(table)
    derivedBlock.instance_variable_set(:@undefinedConstructorSet, derivedBlock.subConstructorSet)
    assert_equal("Derived::Derived(int d) : Base(d) {}\n", derivedBlock.formatStub)
  end


  def test_formatStubRaise
    block = ClassBlock.new("class Base")
    block.instance_variable_set(:@undefinedConstructorSet, nil)

    message = ""
    block.define_singleton_method(:warn) { |line| message += line }

    expr = -> {block.formatStub}
    caught, ignored = checkIfExceptionCaught(expr)
    assert_true(caught)
    assert_false(ignored)
    assert_false(message.empty?)
  end

  def test_formatMockClassWithDefault
    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block, blockBase = getClassBlockWithDefault(testedName, baseName)
    refFunc = TestMockRefClass.new(testedName, "FuncStub", "FuncStub", "NameSpace::Enum", "")
    refSet = TestMockRefSetClass.new(true, [refFunc])

    filter = MinimumSymbolFilter.new(nil, refSet)
    block.filterByReferenceSet(filter)
    block.filterByReferenceSetWithSuper(filter)

    actualDecl, actualDef = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    assert_true(actualDecl.include?("MOCK_METHOD1(FuncStub,void(NameSpace::Enum e));\n"))
    assert_true(block.formatStub.include?("void Tested::FuncStub(NameSpace::Enum e) {"))
  end

  def test_formatMockClassAfterFilterOutAll
    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block, blockBase = getClassBlockWithStub(testedName, baseName)
    assert_true(block.canTraverse?)

    ["FuncStub", "FuncOther"].each do |funcname|
      ref = TestMockRefClass.new(testedName, funcname, funcname, "", "const")
      refSet = TestMockRefSetClass.new(true, [ref])
      block.filterByReferenceSet(MinimumSymbolFilter.new(nil, refSet))
    end

    actualDecl, actualDef = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    assert_true(block.canTraverse?)
    assert_true(blockBase.canTraverse?)
  end

  def test_formatMockTemplateClass
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    decoratorType = decoratorName + "<T>"
    forwarderType = forwarderName + "<T>"
    testedName = "Tested"

    block = getTemplateClassBlock(testedName)
    assert_true(block.canTraverse?)

    actualDecl, actualHpp, actualCpp = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    expected =  "template <typename T> class #{decoratorName};\n"
    expected += "template <typename T> class #{forwarderName};\n"
    expected += "template <typename T> class #{mockName} : public #{testedName}<T> {\n"
    expected += "public:\n"
    expected += "    #{mockName}(#{decoratorType}* pDecorator);\n"
    expected += "    #{mockName}(#{decoratorType}& decorator);\n"
    expected += "    #{mockName}(#{forwarderType}* pForwarder);\n"
    expected += "    #{mockName}(#{forwarderType}& forwarder);\n"
    expected += "    #{mockName}(#{decoratorType}* pDecorator, int a);\n"
    expected += "    #{mockName}(#{decoratorType}& decorator, int a);\n"
    expected += "    #{mockName}(#{forwarderType}* pForwarder, int a);\n"
    expected += "    #{mockName}(#{forwarderType}& forwarder, int a);\n"
    expected += "    ~#{mockName}(void);\n"
    expected += "    MOCK_METHOD0_T(Func,void());\n"
    expected += "private:\n"
    expected += "    #{decoratorType}* pDecorator_;\n"
    expected += "    #{forwarderType}* pForwarder_;\n"
    expected += "};\n\n"
    assert_equal(expected, actualDecl)

    expected =  "template <typename T> #{mockName}<T>::#{mockName}(#{decoratorType}* pDecorator) : " +
                "pDecorator_(pDecorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{decoratorType}& decorator) : " +
                "pDecorator_(&decorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{forwarderType}* pForwarder) : " +
                "pDecorator_(0), pForwarder_(pForwarder) { pForwarder_->pMock_ = this; }\n"
    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{forwarderType}& forwarder) : " +
                "pDecorator_(0), pForwarder_(&forwarder) { pForwarder_->pMock_ = this; }\n"

    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{decoratorType}* pDecorator, int a) : " +
                "#{testedName}<T>(a), pDecorator_(pDecorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{decoratorType}& decorator, int a) : " +
                "#{testedName}<T>(a), pDecorator_(&decorator), pForwarder_(0) { pDecorator_->pMock_ = this; }\n"
    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{forwarderType}* pForwarder, int a) : " +
                "#{testedName}<T>(a), pDecorator_(0), pForwarder_(pForwarder) { pForwarder_->pMock_ = this; }\n"
    expected += "template <typename T> #{mockName}<T>::#{mockName}(#{forwarderType}& forwarder, int a) : " +
                "#{testedName}<T>(a), pDecorator_(0), pForwarder_(&forwarder) { pForwarder_->pMock_ = this; }\n"


    expected += "template <typename T> #{mockName}<T>::~#{mockName}(void) {\n" +
                "    if (pDecorator_ && (pDecorator_->pMock_ == this)) { pDecorator_->pMock_ = 0; }\n"
    expected += "    if (pDecorator_ && (pDecorator_->pClassMock_ == this)) { pDecorator_->pClassMock_ = 0; }\n"
    expected += "    if (pForwarder_ && (pForwarder_->pMock_ == this)) { pForwarder_->pMock_ = 0; }\n}\n\n"
    assert_equal(expected, actualHpp)
    assert_equal("", actualCpp)
  end

  data(false, true)
  def test_formatDecoratorClass(data)
    switchValue = data ? "true" : "false"

    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    testedName = "Tested"
    block = getClassBlockToFormat(baseName, true)

    actualDef, actualCpp = block.formatDecoratorClass(decoratorName, mockName, testedName)
    expected  = "class #{decoratorName} : public #{testedName} {\n"
    expected += "public:\n"
    expected += "    #{decoratorName}(void) : pMock_(0) {}\n"
    expected += "    virtual ~#{decoratorName}(void) {}\n"
    expected += getTestSwitchMockStaticVarnameWithValue("Func", switchValue)
    expected += "    void Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(); return; } #{testedName}::Func(); }\n"
    expected += "    void Func() const override { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(); return; } #{baseName}::Func(); }\n"
    expected += "    void Func(int a) { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(a); return; } #{baseName}::Func(a); }\n"
    expected += getTestSwitchMockStaticVarnameWithValue("Other", switchValue)
    expected += "    int Other(long a,T* b) { if (" + getTestSwitchMockCondition("Other", "pMock_") +
                ") { return pMock_->Other(a,b); } return #{baseName}::Other(a,b); }\n"
    expected += "    #{mockName}* pMock_;\n"
    expected += "    static Mock* pClassMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, actualDef)

    expected =  "namespace #{Mockgen::Constants::GENERATED_SYMBOL_NAMESPACE} {\n"
    expected += "    bool Decorator::Func_nomock_ = false;\n"
    expected += "    bool Decorator::Other_nomock_ = false;\n}\n"
    assert_equal(expected, actualCpp)
  end

  def test_formatDecoratorClassWithArgs
    name = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block = getClassBlockWithConstructorArgs(name)
    actualDef, actualCpp = block.formatDecoratorClass(decoratorName, mockName, testedName)

    ["#{decoratorName}(void) : #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n",
     "#{decoratorName}(int a) : #{name}(a), #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n",
     "#{decoratorName}(int a,int b = 0) : #{name}(a,b), #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    ].each do |expected|
      assert_true(actualDef.include?(expected))
     end
  end

  def test_formatDecoratorTemplateClass
    mockName = "Mock"
    decoratorName = "Decorator"
    testedName = "Tested"
    block = getTemplateClassBlock(testedName)

    actualDef, actualCpp = block.formatDecoratorClass(decoratorName, mockName, testedName)
    expected  = "template <typename T> class #{decoratorName} : public #{testedName}<T> {\n"
    expected += "public:\n"
    expected += "    #{decoratorName}(void) : pMock_(0) {}\n"
    expected += "    #{decoratorName}(int a) : #{testedName}<T>(a), pMock_(0) {}\n"
    expected += "    virtual ~#{decoratorName}(void) {}\n"
    expected += "    void Func() override { if (pMock_) { pMock_->Func(); return; } #{testedName}<T>::Func(); }\n"
    expected += "    #{mockName}<T>* pMock_;\n"
    expected += "    static Mock<T>* pClassMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, actualDef)
    assert_equal("", actualCpp)
  end

  def test_formatForwarderClass
    baseName = "BaseClass"
    mockName = "Mock"
    forwarderName = "Forwarder"
    testedName = "Tested"
    block = getClassBlockToFormat(baseName, false)

    actual = block.formatForwarderClass(forwarderName, mockName, testedName)
    expected =  "class #{forwarderName} : public #{testedName} {\n"
    expected += "public:\n"
    expected += "    #{forwarderName}(#{testedName}* pActual) : pActual_(pActual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}* pActual,int a) : Tested(a), pActual_(pActual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}& actual) : pActual_(&actual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}& actual,int a) : Tested(a), pActual_(&actual), pMock_(0) {}\n"
    expected += "    virtual ~#{forwarderName}(void) {}\n"
    expected += getTestSwitchMockVarname("Func")
    expected += "    void Func() { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(); return; } static_cast<#{testedName}*>(pActual_)->Func(); }\n"
    expected += "    void Func() const override { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(); return; } static_cast<#{baseName}*>(pActual_)->Func(); }\n"
    expected += "    void Func(int a) { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(a); return; } static_cast<#{baseName}*>(pActual_)->Func(a); }\n"
    expected += getTestSwitchMockVarname("Other")
    expected += "    int Other(long a,T* b) { if (" + getTestSwitchMockCondition("Other", "pMock_") +
                ") { return pMock_->Other(a,b); } return static_cast<#{baseName}*>(pActual_)->Other(a,b); }\n"
    expected += "    #{testedName}* pActual_;\n"
    expected += "    #{mockName}* pMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, actual)
  end

  def test_formatForwarderTemplateClass
    mockName = "Mock"
    forwarderName = "Forwarder"
    testedName = "Tested"
    block = getTemplateClassBlock(testedName)

    actual = block.formatForwarderClass(forwarderName, mockName, testedName)
    expected =  "template <typename T> class #{forwarderName} : public #{testedName}<T> {\n"
    expected += "public:\n"
    expected += "    #{forwarderName}(#{testedName}<T>* pActual) : pActual_(pActual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}<T>* pActual,int a) : Tested<T>(a), pActual_(pActual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}<T>& actual) : pActual_(&actual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}<T>& actual,int a) : Tested<T>(a), pActual_(&actual), pMock_(0) {}\n"
    expected += "    virtual ~#{forwarderName}(void) {}\n"
    expected += getTestSwitchMockVarname("Func")
    expected += "    void Func() override { if (" + getTestSwitchMockCondition("Func", "pMock_") +
                ") { pMock_->Func(); return; } static_cast<#{testedName}<T>*>(pActual_)->Func(); }\n"
    expected += "    #{testedName}<T>* pActual_;\n"
    expected += "    #{mockName}<T>* pMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, actual)
  end

  def test_formatConstrutorSet
    baseName = "BaseClass"
    className = "Mock"
    initMember = "data_(1)"

    block = getClassBlockWithConstructorArgs(baseName)
    actual = block.formatConstrutorSet(baseName, className, "", initMember)
    expected =  "    #{className}(void) : #{initMember} {}\n"
    expected += "    #{className}(int a) : #{baseName}(a), #{initMember} {}\n"
    expected += "    #{className}(int a,int b = 0) : #{baseName}(a,b), #{initMember} {}\n"
    assert_equal(expected, actual)

    actual = block.formatConstrutorSet(baseName, className, "T* p", initMember)
    expected =  "    #{className}(T* p) : #{initMember} {}\n"
    expected += "    #{className}(T* p,int a) : #{baseName}(a), #{initMember} {}\n"
    expected += "    #{className}(T* p,int a,int b = 0) : #{baseName}(a,b), #{initMember} {}\n"
    assert_equal(expected, actual)
  end

  def getClassBlockToCollect
    funcLine = "void Func()"
    block = ClassBlock.new("class Tested : public Base")
    func = MemberFunctionBlock.new(funcLine)
    block.connect(func)
    addMemberFunction(block, func)
    return block, func, funcLine
  end

  def connectFunctionBlockToCollect(block)
    funcBaseLine = "void Other()"
    blockBase = ClassBlock.new("class Base")
    funcBase = MemberFunctionBlock.new(funcBaseLine)
    blockBase.connect(funcBase)
    addMemberFunction(blockBase, funcBase)

    table = { "Base" => blockBase }
    block.setBaseClass(table)
    return blockBase, funcBaseLine
  end

  def connectMemberFunctionBlockToCollect(blockBase, funcLine)
    funcOverriden = MemberFunctionBlock.new(funcLine)
    blockBase.connect(funcOverriden)
    addMemberFunction(blockBase, funcOverriden)
  end

  def connectFuncNotOverriden(block, blockBase, testedMethod, funcLine, argSet)
    funcNotOverriden = MemberFunctionBlock.new(funcLine + " const")
    assert_not_equal("", block.send(testedMethod, [funcNotOverriden], *argSet))
    blockBase.connect(funcNotOverriden)
    addMemberFunction(blockBase, funcNotOverriden)
  end

  def connectFuncConstNotOverriden(blockBase)
    funcNotOverriden = MemberFunctionBlock.new("void Func(int a)")
    blockBase.connect(funcNotOverriden)
    addMemberFunction(blockBase, funcNotOverriden)
  end

  def test_collectMockDef
    lambdaCheck1 = lambda { |actual, funcLine| actual.include?("Func") }
    lambdaCheck2 = lambda { |actual, funcBaseLine| actual.include?("Other") }
    postfix = "_EXTRA"

    block, func, funcLine = getClassBlockToCollect
    actual = block.collectMockDef([], postfix)
    assert_equal(1, actual.lines.count)
    assert_true(lambdaCheck1.call(actual, funcLine))
    assert_equal("", block.collectMockDef([func], postfix))

    blockBase, funcBaseLine = connectFunctionBlockToCollect(block)
    actual = block.collectMockDef([], postfix)
    assert_equal(2, actual.lines.count)
    assert_true(lambdaCheck2.call(actual, funcBaseLine))

    connectMemberFunctionBlockToCollect(blockBase, funcLine)
    actual = block.collectMockDef([], postfix)
    assert_equal(2, actual.lines.count)

    funcNotOverriden = MemberFunctionBlock.new(funcLine + " const")
    assert_not_equal("", block.collectMockDef([funcNotOverriden], postfix))
    blockBase.connect(funcNotOverriden)
    addMemberFunction(blockBase, funcNotOverriden)
    actual = block.collectMockDef([], postfix)
    assert_equal(3, actual.lines.count)

    connectFuncConstNotOverriden(blockBase)
    actual = block.collectMockDef([], postfix)
    assert_equal(4, actual.lines.count)
    assert_true(actual.include?(postfix))
  end

  def test_collectDecoratorDef
    lambdaCheck1 = lambda { |actual, funcLine| actual.include?(funcLine) }
    lambdaCheck2 = lambda { |actual, funcBaseLine| actual.include?(funcBaseLine) }

    block, func, funcLine = getClassBlockToCollect
    actual = block.collectDecoratorDef([], {}, {})
    assert_equal(2, actual.lines.count)
    assert_true(lambdaCheck1.call(actual, funcLine))
    assert_equal("", block.collectDecoratorDef([func], {}, {}))

    blockBase, funcBaseLine = connectFunctionBlockToCollect(block)
    actual = block.collectDecoratorDef([], {}, {})

    assert_equal(4, actual.lines.count)
    assert_true(lambdaCheck2.call(actual, funcBaseLine))

    connectMemberFunctionBlockToCollect(blockBase, funcLine)
    actual = block.collectDecoratorDef([], {}, {})
    assert_equal(4, actual.lines.count)

    # Generate one mock and no new switch variable
    connectFuncNotOverriden(block, blockBase, :collectDecoratorDef, funcLine, [{}, {}])
    actual = block.collectDecoratorDef([], {}, {})
    assert_equal(5, actual.lines.count)

    connectFuncConstNotOverriden(blockBase)
    actual = block.collectDecoratorDef([], {}, {})
    assert_equal(6, actual.lines.count)
  end

  def test_collectForwarderDef
    lambdaCheck1 = lambda { |actual, funcLine| actual.include?("Func") }
    lambdaCheck2 = lambda { |actual, funcBaseLine| actual.include?("Other") }

    block, func, funcLine = getClassBlockToCollect
    actual = block.collectForwarderDef([], {})
    assert_equal(2, actual.lines.count)
    assert_true(lambdaCheck1.call(actual, funcLine))
    assert_equal("", block.collectForwarderDef([func], {}))

    blockBase, funcBaseLine = connectFunctionBlockToCollect(block)
    actual = block.collectForwarderDef([], {})

    assert_equal(4, actual.lines.count)
    assert_true(lambdaCheck2.call(actual, funcBaseLine))

    connectMemberFunctionBlockToCollect(blockBase, funcLine)
    actual = block.collectForwarderDef([], {})
    assert_equal(4, actual.lines.count)

    # Generate one mock and no new switch variable
    connectFuncNotOverriden(block, blockBase, :collectForwarderDef, funcLine, [{}])
    actual = block.collectForwarderDef([], {})
    assert_equal(5, actual.lines.count)

    connectFuncConstNotOverriden(blockBase)
    actual = block.collectForwarderDef([], {})
    assert_equal(6, actual.lines.count)
  end

  def test_collectFunctionAll
    block = ClassBlock.new("class Tested")
    ["public:", "void FuncPublic();", "protected:", "void FuncProtected();",
     "private:", "virtual void FuncPrivate();", "void Impl();"].each do |line|
      funcBlock = block.parseChildren(line)
      def funcBlock.makeMockDef(name, postfix)
        "a"
      end

      def funcBlock.makeDecoratorDef(name, definedNameSet, definedStaticNameSet)
        return "b"
      end

      def funcBlock.makeForwarderDef(name, definedNameSet)
        "c"
      end
    end

    assert_equal(3, block.collectMockDef([], "").size)
    assert_equal(2, block.collectDecoratorDef([], {}, {}).size)
    assert_equal(1, block.collectForwarderDef([], {}).size)
  end

  def test_collectBaseFunctionDef
    funcBaseLine = "void FuncBase()"
    funcBaseA = MemberFunctionBlock.new(funcBaseLine)
    funcBaseB = MemberFunctionBlock.new(funcBaseLine)
    blockBaseA = ClassBlock.new("class BaseA")
    blockBaseB = ClassBlock.new("class BaseB")

    blockBaseA.connect(funcBaseA)
    blockBaseB.connect(funcBaseB)
    addMemberFunction(blockBaseA, funcBaseA)
    addMemberFunction(blockBaseB, funcBaseB)

    block = ClassBlock.new("class BaseD")
    block.instance_variable_set(:@baseClassBlockSet, [blockBaseA, blockBaseB])

    def block.checkFunc(func)
      true
    end

    lambdaCheck = lambda { |n| true }
    str = block.collectFunctionDef([], :collectMockDef, lambdaCheck, :checkFunc, "", nil)
    assert_equal("    MOCK_METHOD0(FuncBase,void());\n", str)
  end

  def test_collectTemplateClassDef
    funcBaseLine = "virtual void FuncBase()"
    funcBase = MemberFunctionBlock.new(funcBaseLine)
    blockBase = ClassBlock.new("class BaseA")
    blockBase.connect(funcBase)
    addMemberFunction(blockBase, funcBase)

    funcLine = "virtual int Func(int a)"
    func = MemberFunctionBlock.new(funcLine)
    block = ClassBlock.new("template <typename T> class BaseD")
    block.instance_variable_set(:@baseClassBlockSet, [blockBase])
    block.connect(func)
    addMemberFunction(block, func)

    def block.checkFunc(func)
      true
    end

    lambdaCheck = lambda { |n| true }
    str = block.collectMockDef([], "_T")
    assert_equal("    MOCK_METHOD1_T(Func,int(int a));\n    MOCK_METHOD0_T(FuncBase,void());\n", str)
  end

  data(
    'not template' => ["class Name", "class Name : public BaseName", "Name"],
    'one param' => ["template <typename T> class Name",
                    "template <typename T> class Name : public BaseName<T>", "Name<T>"],
    'param set' => ["template <typename T, size_t S> class Name",
                    "template <typename T, size_t S> class Name : public BaseName<T,S>", "Name<T,S>"])
  def test_getClassDefinition(data)
    line, expected, expectedTyped = data
    isTemplate = line.include?("template")
    templateParam = TemplateParameter.new(line)
    argTemplate = isTemplate ? templateParam : nil

    block = ClassBlock.new("")
    assert_equal(expected, block.getClassDefinition(argTemplate, "Name", "BaseName"))

    pos = expected.index(":")
    expectedStr = expected[0..(pos-2)]
    assert_equal(expectedStr, block.getTemplateDeclaration(argTemplate, "Name"))

    pos = expected.index(" class")
    expectedStr = pos ? (expected[0..(pos-1)] + " ") : ""
    assert_equal(expectedStr, block.getTemplateHeader(argTemplate))
    assert_equal(expectedTyped, block.getTypedTemplate(argTemplate, "Name"))

    unless argTemplate
      actual = block.getVariableDefinitionExample(argTemplate, "Name", "Decorator", nil, nil)
      assert_true(actual.include?("#{expectedTyped}* Decorator::pClassMock_;\n"))
      assert_equal(isTemplate, actual.include?("//"))
    end
  end

  def test_getDefinitionExample
    block = ClassBlock.new("class NameA")
    templateParam = TemplateParameter.new("template <class C, typename T, size_t S, typename ...Ts> class NameA")
    mockName = "MA"
    decoratorName = "DA"
    forwarderName = "FA"
    fullname = "::NS::NameA"

    expected =  "/* Tester must define these types and variables\n"
    expected += "using DataType1 = int; (or other appropriate type)\n"
    expected += "using DataType2 = int; (or other appropriate type)\n"
    expected += "++ set an appropriate value to type parameter 3 ++\n"
    expected += "using DataType4 = int; (or other appropriate type)\n"
    expected += " ** Class (static) variable template **\n"
    expected += "template <class C, typename T, size_t S, typename ...Ts> MA<C,T,S,Ts...>* DA<C,T,S,Ts...>::pClassMock_;\n"
    expected += " ** Specialized class template to test **\n"
    expected += "template class ::NS::NameA<DataType1,DataType2,3,DataType4>;\n"
    expected += " ** Generated classes**\n"
    expected += "namespace MyUnittest {\n"
    expected += " ** Specialized class variable in the decorator class template **\n"
    expected += "    template MA<DataType1,DataType2,3,DataType4>* DA<DataType1,DataType2,3,DataType4>::pClassMock_;\n"
    expected += "}\n"
    expected += " ** Type aliases in a test fixture **\n"
    expected += "    using Tested = ::NS::NameA<DataType1,DataType2,3,DataType4>;\n"
    expected += "    using Decorator = MyUnittest::DA<DataType1,DataType2,3,DataType4>;\n"
    expected += "    using Forwarder = MyUnittest::FA<DataType1,DataType2,3,DataType4>;\n"
    expected += "    using Mock = MyUnittest::MA<DataType1,DataType2,3,DataType4>;\n"
    expected += "*/\n"
    expected += "\n"

    actual = block.getVariableDefinitionExample(templateParam, mockName, decoratorName, forwarderName, fullname)
    assert_equal(expected, actual)
    actual = block.getDefinitionExample(templateParam, mockName, decoratorName, forwarderName, fullname)
    assert_equal(expected, actual)
  end

  # Test makeClassSet in testing CppFileParser
end

class TestEnumBlock < Test::Unit::TestCase
  data(
    'enum' => "enum Month",
    'enum declaration' => "enum Month;",
    'enum definition1' => "enum Month{",
    'enum definition2' => "enum Month {",
    'enum class' => "enum class Month{")
  def test_construct(data)
    block = EnumBlock.new(data)
    name = "Month"
    assert_equal(name, block.getTypename)
    assert_equal(name, block.keyForParent)
    assert_equal("static_cast<Month>(0)", block.getInitializer)
  end

  def test_failToConstruct
    block = EnumBlock.new("")
    assert_nil(block.getTypename)
    assert_nil(block.getInitializer)
    assert_nil(block.keyForParent)
  end

  def test_parseChildren
    block = EnumBlock.new("enum Month {")
    assert_equal("static_cast<Month>(0)", block.getInitializer)
    assert_nil(block.parseChildren("January"))
    assert_equal("January", block.getInitializer)
    assert_nil(block.parseChildren("February"))
    assert_equal("January", block.getInitializer)

    block = EnumBlock.new("enum class Month {")
    assert_nil(block.parseChildren("January"))
    assert_equal("Month::January", block.getInitializer)
  end
end

class TestUnionBlock < Test::Unit::TestCase
  data(
    'union' => "union Any",
    'union declaration' => "union Any;",
    'union definition1' => "union Any{",
    'union definition2' => "union Any {")
  def test_construct(data)
    name = "Any"
    block = UnionBlock.new(data)
    assert_equal(name, block.getTypename)
    assert_equal(name, block.keyForParent)
  end

  def test_failToConstruct
    block = UnionBlock.new("")
    assert_nil(block.getTypename)
    assert_nil(block.keyForParent)
  end
end

class TestBlockFactory < Test::Unit::TestCase
  def test_createRootBlock
    assert_true(BlockFactory.new(false, nil, nil).createRootBlock.canTraverse?)
  end

  def test_createBlock
    factory = BlockFactory.new(false, nil, nil)
    rootBlock = factory.createRootBlock

    line = "namespace NameSpaceA {"
    block, skip = factory.createBlock(line, rootBlock)
    assert_equal("NameSpaceA", block.getNamespace)
    assert_false(skip)

    line = 'extern "C"'
    block, skip = factory.createBlock(line, rootBlock)
    assert_equal("", block.getNamespace)

    line = "class ClassName {"
    classBlock, skip = factory.createBlock(line, rootBlock)
    assert_true(classBlock.isClass?)

    # Define as a private member
    line = "class InnerClass {"
    innerClassBlock, skip = factory.createBlock(line, classBlock)
    assert_false(innerClassBlock.isClass?)

    line = "struct ClassName {"
    classBlock, skip = factory.createBlock(line, rootBlock)
    assert_true(classBlock.isClass?)

    # Define as a public member
    line = "class InnerClass {"
    innerClassBlock, skip = factory.createBlock(line, classBlock)
    assert_true(innerClassBlock.isClass?)

    line = "template <typename T> class ClassName {"
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(block.isClass?)

    line = "typedef unsigned int uint32_tj;"
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(block.canTraverse?)

    line = "using uint32_ptr_t = unsigned int *;"
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(block.canTraverse?)
    assert_true(block.is_a?(TypedefBlock))

    line = "extern const ClassName& obj;"
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(block.canTraverse?)

    line = "extern void FuncDef() {"
    block, skip = factory.createBlock(line, rootBlock)
    assert_false(block.canTraverse?)

    line = "extern void FuncDecl();"
    block, skip = factory.createBlock(line, rootBlock)
    assert_false(block.instance_variable_get(:@noMatchingTypes))

    line = "enum class EnumClassName {"
    enumClassBlock, skip = factory.createBlock(line, classBlock)
    assert_equal("EnumClassName", enumClassBlock.getTypename)

    line = "enum EnumName {"
    enumBlock, skip = factory.createBlock(line, classBlock)
    assert_equal("EnumName", enumBlock.getTypename)

    line = "FIRST_MEMBER,"
    block, skip = factory.createBlock(line, enumBlock)
    assert_equal("FIRST_MEMBER", enumBlock.getInitializer)

    line = "enum class UnionName {"
    unionBlock, skip = factory.createBlock(line, classBlock)
    assert_equal("UnionName", unionBlock.getTypename)

    line = "public:"
    block, skip = factory.createBlock(line, classBlock)
    assert_not_nil(block)

    line = "void Func(int a)"
    block, skip = factory.createBlock(line, classBlock)
    assert_not_nil(block)
  end

  data(
    'struct' => "struct",
    'class' => "class")
  def test_createBlockWithTypedef(data)
    typeStr = data
    factory = BlockFactory.new(false, nil, nil)
    rootBlock = factory.createRootBlock

    line = "typedef #{typeStr} tagName {"
    block, skip = factory.createBlock(line, rootBlock)
    assert_not_nil(block.instance_variable_get(:@typedefBlock))
  end

  data(
    'enum' => ["enum", "EnumBlock"],
    'enum class' => ["enum class", "EnumBlock"],
    'union' => ["union", "UnionBlock"])
  def test_createUserDefinedType(data)
    typeStr, className = data
    line = "#{typeStr} Month {"
    assert_equal("Month", Object.const_get(className).new(line).getTypename)
  end

  data(
    'inline' => "inline",
    'static inline' => "static inline",
    'inline static' => "inline static")
  def test_createInlineFunction(data)
    line = data + " int Func(long) {"

    factory = BlockFactory.new(false, nil, nil)
    rootBlock = factory.createRootBlock
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(block.valid)
  end

  data(
    'declaration' => "inline int Func(long);",
    'variable' => "static int var;")
  def test_createNotInlineFunction(data)
    line = data
    factory = BlockFactory.new(false, nil, nil)
    rootBlock = factory.createRootBlock
    block, skip = factory.createBlock(line, rootBlock)
    assert_false(block.canTraverse?)
  end

  def test_skipBlock
    filter = NamespaceFilter.new(nil)
    def filter.exclude?(name)
      true
    end

    factory = BlockFactory.new(false, filter, nil)
    rootBlock = factory.createRootBlock
    line = "namespace NameSpaceA {"
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(skip)
  end

  def test_filterBlock
    filterToMock = ClassNameFilter.new(["A"], [], [], [])
    filterToAll = ClassNameFilter.new(["B"], [], [], [])
    factory = BlockFactory.new(false, nil, ClassBlockParameterSet.new(filterToMock, filterToAll))

    line = "class InnerClass {"
    block, skip = factory.createBlock(line, block)
    assert_equal(filterToMock, block.instance_variable_get(:@filterToMock))
    assert_equal(filterToAll, block.instance_variable_get(:@filterToAll))
  end

  def test_noMatchingTypesInCsource
    factory = BlockFactory.new(true, nil, nil)
    rootBlock = factory.createRootBlock
    line = "extern void Func();"
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(block.instance_variable_get(:@noMatchingTypes))
  end

  def test_addVariable
    varname = "g_obj"
    classname = "NameC"
    nameFilter = ClassNameFilter.new([], ["NotExist"], [], [])
    noDetailFilter = SilentClassNameFilter.new([])
    classParameter = ClassBlockParameterSet.new(nameFilter, noDetailFilter)
    nameFilter.instance_variable_set(:@varNameSet, {varname => true})

    factory = BlockFactory.new(true, nil, classParameter)
    line = "extern #{classname} #{varname};"
    rootBlock = factory.createRootBlock
    block, skip = factory.createBlock(line, rootBlock)
    assert_true(nameFilter.instance_variable_get(:@classNameSet).key?(classname))
  end
end

class TestClassMock < Mockgen::BaseBlock
  attr_reader :name, :getFullname, :children, :visited, :typeAliasSet

  def initialize(name, getFullname, children)
    super("")
    @name = name
    @getFullname = getFullname
    @children = children
    @visited = false
    @typeAliasSet = TypeAliasSet.new
    @defaultNoForwardingToMock = false
  end

  def isClass?
    true
  end

  def isNonMemberInstanceOfClass?
    false
  end

  def setBaseClass(classSet)
    @visited = true if classSet
  end

  def markMemberFunctionSetVirtual
    false
  end

  def setDefaultNoForwardingToMock(defaultNoForwardingToMock)
    @defaultNoForwardingToMock = defaultNoForwardingToMock
  end

  def filterByReferenceSetWithSuper(filter)
  end
end

class TestClassInstance < Test::Unit::TestCase
  def test_initialize
    setAll = ClassInstance.new("typeSwap", "varSwap", "decl", "def")
    assert_false(setAll.empty?)

    setPartial = ClassInstance.new("", "varSwap", "decl", "def")
    assert_false(setPartial.empty?)

    setNone = ClassInstance.new("", "", "", "")
    assert_true(setNone.empty?)
  end
end

class TestClassInstanceMap < Test::Unit::TestCase
  def test_initialize
    instanceMap = ClassInstanceMap.new
    instanceMap.add("NameA", "", "", "decl", "")
    instanceMap.add("NameA", "", "", "", "def")
    instanceMap.add("NameB", "", "", "", "")

    assert_equal(2, instanceMap.getInstanceSet("NameA").size)
    assert_equal(1, instanceMap.getInstanceSet("NameB").size)
    assert_equal(0, instanceMap.getInstanceSet("NameC").size)

    instanceMap.cleanUp
    assert_equal(0, instanceMap.getInstanceSet("NameB").size)
  end
end

class TestDefinedReference < Test::Unit::TestCase
  data(
    'empty' => ["", nil, nil, nil, false],
    'member' => ["anObject  member  6 dir/body.cpp ::Sample1::Types::DerivedClass anObject;",
                 nil, nil, nil, false],
    'free function' => ["main  function  4 ./dir/main.cpp int main(int argc, char* argv[]) {",
                        "main", nil, nil, true],
    'member function' => ["Func  function 34 ./dir/file.cpp int NameA::Func(void) { return 0; }",
                          "Func", "NameA", "NameA", false],
    'inner class' => ["Func  function 34 ./dir/file.cpp int Outer::NameA::Func(void) { return 0; }",
                      "Func", "NameA", "Outer::NameA", false])
  def test_parse(data)
    line, expectedFunctionName, expectedClassName,
    expectedRelativeClassName, expectedIsFreeFunction = data

    ref = DefinedReference.new(line)
    assert_equal(expectedFunctionName, ref.functionName)
    assert_equal(expectedClassName, ref.className)
    assert_equal(expectedRelativeClassName, ref.relativeClassName)
    assert_equal(expectedIsFreeFunction, ref.isFreeFunction)
  end
end

class TestStrippedFullname < Test::Unit::TestCase
  data(
    'nil' => [nil, nil],
    'name' => ["Name", "Name"],
    'top level' => ["::Name", "Name"],
    'namespace' => ["Namespace::Name", "Namespace::Name"],
    'array' => ["array[10]", "array"],
    'mixed' => ["::name::array[10]", "name::array"])
  def test_all(data)
    arg, expected = data
    assert_equal(expected, StrippedFullname.new(arg).name)
  end
end

class TestDefinedReferenceSet < Test::Unit::TestCase
  def setup
    @relativeNamespaceOnly = false
  end

  def test_initialize
    refSet = DefinedReferenceSet.new(TestInputSourceFileSet)
    assert_equal(Mockgen::Constants::MODE_CHECK_CLASSNAME_IN_RELATIVE_NAMESPACES_ONLY, refSet.relativeNamespaceOnly)
    assert_false(@relativeNamespaceOnly)

    # Check the input files
    ["SampleFunc", "SampleFuncArray"].each do |name|
      assert_true(refSet.freeFunctionDefined?(name))
    end

    ["BaseClass", "TopLevelClass"].each do |name|
      assert_true(refSet.classDefined?(name, @relativeNamespaceOnly))
    end
  end

  def test_parseAllLines
    refSet = DefinedReferenceSet.new([])

    lineSet = ["main  function  4 ./dir/main.cpp int main(int argc, char* argv[]) {\n",
               "Func  function 34 ./dir/file.cpp int NameA::Func(void) { return 0; }\n",
               "Func  function 34 ./dir/file.cpp int Outer::NameB::Func(void) { return 0; }\n"]
    refClassNameSet = {}
    refFunctionSet = {}
    refSet.parseAllLines(lineSet, refFunctionSet, refClassNameSet)

    assert_not_nil(refFunctionSet["main"])
    assert_not_nil(refClassNameSet["NameA"])
    assert_not_nil(refClassNameSet["NameB"])
  end

  def test_addNothing
    refSet = DefinedReferenceSet.new([])
    refFunctionSet = {}
    refClassNameSet = {}

    refClass = Struct.new(:functionName, :className, :relativeClassName, :isFreeFunction)
    ref = refClass.new(nil, nil, nil, false)
    refSet.add(ref, refFunctionSet, refClassNameSet)
    assert_true(refFunctionSet.empty?)
    assert_true(refClassNameSet.empty?)

    functionName = "func"
    ref = refClass.new(functionName, nil, nil, false)
    refSet.add(ref, refFunctionSet, refClassNameSet)
    assert_true(refFunctionSet.empty?)
  end

  def test_addFreeFunction
    refSet = DefinedReferenceSet.new([])
    refFunctionSet = {}
    refClassNameSet = {}

    functionName = "func"
    className = "NameA"

    refClass = Struct.new(:functionName, :className, :relativeClassName, :isFreeFunction)
    1.upto(2) do |i|
      ref = refClass.new(functionName, nil, nil, true)
      refSet.add(ref, refFunctionSet, refClassNameSet)
      assert_equal(1, refFunctionSet.size)
      assert_not_nil(refFunctionSet[functionName])
      assert_true(refClassNameSet.empty?)
    end

    refSet.instance_variable_set(:@refFunctionSet, refFunctionSet)
    [["", true], ["::", true], ["A", false]].each do |prefix, expected|
      assert_equal(expected, refSet.freeFunctionDefined?(prefix + functionName))
    end
  end

  def test_addClass
    refSet = DefinedReferenceSet.new([])
    refFunctionSet = {}
    refClassNameSet = {}

    refClass = Struct.new(:functionName, :className, :relativeClassName, :isFreeFunction)
    className = "NameA"
    memberFunctionName = "memfunc"
    1.upto(2) do |i|
      ref = refClass.new(memberFunctionName, className, className, false)
      refSet.add(ref, refFunctionSet, refClassNameSet)
      assert_true(refFunctionSet.empty?)
      assert_equal(1, refClassNameSet.size)
      assert_not_nil(refClassNameSet[className])
      assert_equal(1, refClassNameSet[className].size)
    end

    refSet.instance_variable_set(:@refClassNameSet, refClassNameSet)
    [["", true], ["::", true], ["A", false]].each do |prefix, expected|
      assert_equal(expected, refSet.classDefined?(prefix + className, @relativeNamespaceOnly))
    end

    relativeClassName = "Outer::" + className
    1.upto(2) do |i|
      ref = refClass.new(memberFunctionName, className, relativeClassName, false)
      refSet.add(ref, refFunctionSet, refClassNameSet)
      assert_equal(1, refClassNameSet.size)
      assert_not_nil(refClassNameSet[className])
      assert_equal(2, refClassNameSet[className].size)
    end
  end

  def test_addInnerClass
    refSet = DefinedReferenceSet.new([])
    refFunctionSet = {}
    refClassNameSet = {}

    refClass = Struct.new(:functionName, :className, :relativeClassName, :isFreeFunction)
    className = "NameA"
    memberFunctionName = "memfunc"
    relativeClassName = "Outer::" + className

    ref = refClass.new(memberFunctionName, className, relativeClassName, false)
    refSet.add(ref, refFunctionSet, refClassNameSet)
    assert_equal(1, refClassNameSet.size)
    assert_not_nil(refClassNameSet[className])
    assert_equal(1, refClassNameSet[className].size)

    refSet.instance_variable_set(:@refClassNameSet, refClassNameSet)
    assert_false(refSet.classDefined?(className, @relativeNamespaceOnly))

    ["", "::", "Extra::"].each do |prefix|
      assert_true(refSet.classDefined?(prefix + relativeClassName, @relativeNamespaceOnly))
    end

    ["A", "::Extra::"].each do |prefix|
      assert_false(refSet.classDefined?(prefix + relativeClassName, @relativeNamespaceOnly))
    end
  end

  def test_addClassInNamespace
    refSet = DefinedReferenceSet.new([])
    refClass = Struct.new(:functionName, :className, :relativeClassName, :isFreeFunction)
    nameSpace = "SpaceA"
    className = "NameB"
    relativeClassName = nameSpace + "::" + className

    ref = refClass.new("memfunc", className, relativeClassName, false)
    refFunctionSet = {}
    refClassNameSet = {}
    refSet.add(ref, refFunctionSet, refClassNameSet)
    assert_equal(1, refClassNameSet.size)
    refSet.instance_variable_set(:@refClassNameSet, refClassNameSet)

    [["", false, true], ["", true, true], ["::", false, false], ["::", true, true]
    ].each do |prefix, relativeNamespaceOnly, expected|
      assert_equal(expected, refSet.classDefined?(prefix + "NameSpaceA::" + relativeClassName, relativeNamespaceOnly))
    end
  end

  def test_stripFullname
    refSet = DefinedReferenceSet.new([])
    assert_equal("Name", refSet.stripFullname("Name"))
  end
end

class TestUndefinedReference < Test::Unit::TestCase
  data(
    'top level' => ["var", "NameC var;\n"],
    'namespace' => ["A::var", "namespace A {\nNameC var;\n}\n"],
    'two namespaces' => ["A::B::var", "namespace A {\nnamespace B {\nNameC var;\n}}\n"],
    'three namespaces' => ["A::B::C::var", "namespace A {\nnamespace B {\nnamespace C {\nNameC var;\n}}}\n"])
  def test_getGlobalVariableStub(data)
    symbol, expected = data
    line = "#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE} #{symbol}"
    ref = UndefinedReference.new(line)
    assert_equal(expected, ref.getGlobalVariableStub("NameC"))
  end

  data(
    'toplevel' => ["undefined reference to VTABLE for `TopLevelClass'", "TopLevelClass"],
    'in a namespace' => ["undefined reference to VTABLE for `A::TopLevelClass'", "::A::TopLevelClass"],
    'in namespaces' => ["undefined reference to VTABLE for `A::B::TopLevelClass'", "::A::B::TopLevelClass"])
  def test_parseVtable(data)
    ["vtable", "typeinfo"].each do |keyword|
      rawLine, expectedFullname = data
      line = rawLine.sub(/VTABLE/, keyword)
      ref = UndefinedReference.new(line)

      assert_true(ref.isVtable)
      assert_not_nil(ref.fullname)
      assert_equal(expectedFullname, ref.fullname)
      assert_equal(expectedFullname, ref.classFullname)
      assert_nil(ref.memberName)
      assert_nil(ref.argTypeStr)
      assert_nil(ref.postFunc)
    end
  end

  data(
    'toplevel' => ["undefined reference to `TopLevelClass::GetValue()'",
                   "TopLevelClass::GetValue", "TopLevelClass", "GetValue", "", ""],
    'destructor' => ["undefined reference to `TopLevelClass::~TopLevelClass()'",
                   "TopLevelClass::~TopLevelClass", "TopLevelClass", "~TopLevelClass", "", ""],
    'namespace' => ["undefined reference to `Sample1::Types::DerivedClass::FuncAdded(long, const T*) const",
                    "::Sample1::Types::DerivedClass::FuncAdded", "::Sample1::Types::DerivedClass",
                    "FuncAdded", "long, const T*", "const"],
    'C symbol' => ["undefined reference to `sym'", "sym", "", "sym", nil, ""],
    'array' => ["undefined reference to `ClassNotInstanciated::arrayMissing'",
                "ClassNotInstanciated::arrayMissing", "ClassNotInstanciated", "arrayMissing", nil, ""],
    'full sentence' => ["obj/file.o:file2.cpp:(.rdataSym+0x40): undefined reference to `funcMissing(long) const'",
                        "funcMissing", "", "funcMissing", "long", "const"]
  )
  def test_parseSymbol(data)
    line, expectedFullname, expectedClassFullname,
    expectedMemberName, expectedArgTypeStr, expectedPostFunc = data

    ref = UndefinedReference.new(line)
    assert_false(ref.isVtable)
    assert_not_nil(ref.fullname)
    assert_equal(expectedFullname, ref.fullname)
    assert_equal(expectedClassFullname, ref.classFullname)
    assert_equal(expectedMemberName, ref.memberName)
    assert_equal(expectedArgTypeStr, ref.argTypeStr)
    assert_equal(expectedPostFunc, ref.postFunc)
  end

  data(
    'empty' => ["", false, "", "", ""],
    'variable' => ["var", false, "", "var", "var"],
    'class' => ["ClassName", true, "ClassName", "ClassName", ""],
    'variable in a namespace' => ["name::var", false, "name", "name::var", "var"],
    'class in a namespace' => ["name::ClassName", true, "::name::ClassName", "::name::ClassName", ""],
    'variable in two namespaces' => ["A::B::var", false, "::A::B", "::A::B::var", "var"],
    'class in two namespaces' => ["A::B::ClassName", true, "::A::B::ClassName", "::A::B::ClassName", ""],
    'variable in two namespaces 2' => ["::A::B::var", false, "::A::B", "::A::B::var", "var"],
    'class in two namespaces 2' => ["::A::B::ClassName", true, "::A::B::ClassName", "::A::B::ClassName", ""],
    'variable in three namespaces' => ["A::B::C::var", false, "::A::B::C", "::A::B::C::var", "var"],
    'class in three namespaces' => ["A::B::C::ClassName", true, "::A::B::C::ClassName", "::A::B::C::ClassName", ""])
  def test_parseNameSet(data)
    symbol, isClass, expectedClassFullname, expectedFullname, expectedMemberName = data
    ref = UndefinedReference.new("")
    classFullname, fullname, memberName = ref.parseNameSet(symbol, isClass)
    assert_equal(expectedClassFullname, classFullname)
    assert_equal(expectedFullname, fullname)
    assert_equal(expectedMemberName, memberName)
  end

  data(
    'free function' => ["Func(long (*)(void (*)(int)), long) const", "Func",
                        "long(*)(void(*)(int)), long", "const"],
    'member function' => ["Name::Func(int (Top::Class::*)(int, void const**)) const",
                          "Name::Func", "int(Top::Class::*)(int, void const**)", "const"])
  def test_parsePointerToFunction(data)
    line, expectedFullname, expectedArgTypeStr, expectedPostFunc = data

    ref = UndefinedReference.new("undefined reference to `" + line + "`")
    assert_equal(expectedFullname, ref.fullname)
    assert_equal(expectedArgTypeStr, ref.argTypeStr)
    assert_equal(expectedPostFunc, ref.postFunc)
  end
end

class TestUndefinedReferenceSet < Test::Unit::TestCase
  def test_getGlobalVariableStub
    referenceSet = UndefinedReferenceSet.new(nil)

    ref1 = Object.new
    ref2 = Object.new
    def ref1.getGlobalVariableStub(typeName)
      typeName + "1"
    end

    def ref2.getGlobalVariableStub(typeName)
      typeName + "2"
    end

    refFullnameSet = {"var" => [ref1, ref2], "A::B::varC" => [ref2]}
    referenceSet.instance_variable_set(:@refFullnameSet, refFullnameSet)
    ["", "::"].each do |prefix|
      assert_equal(["Name1", "Name2"], referenceSet.getGlobalVariableStub("#{prefix}var", "Name"))
      assert_equal(["Class2"], referenceSet.getGlobalVariableStub("#{prefix}A::B::varC", "Class"))
    end
  end

  def test_readAllLinesForVtable(data)
    referenceSet = UndefinedReferenceSet.new(nil)

    refClass = Struct.new(:isVtable, :classFullname)
    nameSet = ["classA", "B::classA", "C::B::ClassA", "B::classA", nil]
    lineSet = nameSet.compact.map do |name|
      "#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE} vtable for #{name}\n"
    end << "#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE} typeinfo for #{nameSet[-1]}\n"

    file = Object.new
    file.define_singleton_method(:gets) { lineSet.shift }
    vtableSet, refSet, refFullnameSet = referenceSet.readAllLines(file)

    expectedSet = ["classA", "::B::classA", "::C::B::ClassA"]
    assert_true(expectedSet.all? { |name| vtableSet.key?(name) })
    referenceSet.instance_variable_set(:@vtableSet, vtableSet)
    assert_true(expectedSet.all? { |name| needVtable?(name) })
  end

  def test_readAllLinesForReferences
    referenceSet = UndefinedReferenceSet.new(nil)

    refClass = Struct.new(:isVtable, :classFullname)
    nameSet = ["NameA", "B::NameA", "C::B::Name", "B::NameA", nil]
    lineSet = nameSet.compact.map { |name| "#{Mockgen::Constants::KEYWORD_UNDEFINED_REFERENCE} `#{name}'\n" }

    file = Object.new
    file.define_singleton_method(:gets) { lineSet.shift }
    vtableSet, refSet, refFullnameSet = referenceSet.readAllLines(file)
    assert_true(vtableSet.empty?)
    assert_true(refSet.size == nameSet.compact.size)
  end

  def test_getReferenceSetByFullname
    referenceSet = UndefinedReferenceSet.new(nil)

    refSet = [1, 2]
    refFullnameSet = {"NameA" => 1, "NameB" => 2}
    referenceSet.instance_variable_set(:@refFullnameSet, refFullnameSet)
    referenceSet.instance_variable_set(:@refSet, refSet)

    assert_equal(refSet, referenceSet.getReferenceSetByFullname(nil))
    assert_equal([], referenceSet.getReferenceSetByFullname("Name"))
    assert_equal(1, referenceSet.getReferenceSetByFullname("NameA"))
    assert_equal(1, referenceSet.getReferenceSetByFullname("::NameA"))
    assert_equal(2, referenceSet.getReferenceSetByFullname("NameB"))
    assert_equal(2, referenceSet.getReferenceSetByFullname("NameB[10]"))
  end

  def test_getReferenceSet
    name = "Name"
    refMap = { name => 1 }
    defaultSet = 2

    refSet = UndefinedReferenceSet.new(nil)
    assert_equal(defaultSet, refSet.getReferenceSet(nil, refMap, defaultSet))
    assert_equal(1, refSet.getReferenceSet(name, refMap, defaultSet))
    assert_equal([], refSet.getReferenceSet(name + "A", refMap, defaultSet))
  end

  def test_add
    referenceSet = UndefinedReferenceSet.new(nil)
    refClass = Struct.new(:fullname)

    refSet = []
    refFullnameSet = {}

    1.upto(2) do |i|
      ref = refClass.new("NameA")
      referenceSet.add(ref, refSet, refFullnameSet)
      assert_equal(i, refSet.size)
      assert_equal(1, refFullnameSet.size)
    end

    ref = refClass.new("NameB")
    referenceSet.add(ref, refSet, refFullnameSet)
    assert_equal(3, refSet.size)
    assert_equal(2, refFullnameSet.size)
  end

  def test_stripFullname
    refSet = UndefinedReferenceSet.new(nil)
    assert_equal("Name", refSet.stripFullname("::Name"))
  end
end

class TestCppFileParameterSet < Test::Unit::TestCase
  def test_all
    cppNameSpace = "a"
    inputFilename = "b"
    linkLogFilename = "c"
    convertedFilename = "d"
    stubOnly = true
    functionNameFilterSet = ["f"]
    classNameFilterOutSet = ["g"]
    sourceFilenameSet = ["h"]
    defaultNoForwardingToMock = "i"
    fillVtable = "j"
    noOverloading = "k"
    varOnly = "l"
    updateChangesOnly = "m"
    discardNamespaceValue = "n"
    classNameExcludedSet = ["o"]
    testedFilenameGlobSet = ["p"]
    findStatementFilterSet = ["q"]
    explicitClassNameSet = ["r"]
    mockGuardName = "s"
    noSwapInlineFunction = "t"

    parameterSet = CppFileParameterSet.new(cppNameSpace, inputFilename, linkLogFilename, convertedFilename,
                                           stubOnly, functionNameFilterSet, classNameFilterOutSet,
                                           sourceFilenameSet, defaultNoForwardingToMock,
                                           fillVtable, noOverloading, varOnly,
                                           updateChangesOnly, discardNamespaceValue, classNameExcludedSet,
                                           testedFilenameGlobSet, findStatementFilterSet, explicitClassNameSet,
                                           mockGuardName, noSwapInlineFunction)
    assert_equal(cppNameSpace, parameterSet.cppNameSpace)
    assert_equal(inputFilename, parameterSet.inputFilename)
    assert_equal(linkLogFilename, parameterSet.linkLogFilename)
    assert_equal(convertedFilename, parameterSet.convertedFilename)
    assert_equal(stubOnly, parameterSet.stubOnly)
    assert_equal(functionNameFilterSet, parameterSet.functionNameFilterSet)
    assert_equal(classNameFilterOutSet, parameterSet.classNameFilterOutSet)
    assert_equal(sourceFilenameSet, parameterSet.sourceFilenameSet)
    assert_equal(defaultNoForwardingToMock, parameterSet.defaultNoForwardingToMock)
    assert_equal(fillVtable, parameterSet.fillVtable)
    assert_equal(noOverloading, parameterSet.noOverloading)
    assert_equal(varOnly, parameterSet.varOnly)
    assert_equal(updateChangesOnly, parameterSet.updateChangesOnly)
    assert_equal(discardNamespaceValue, parameterSet.discardNamespaceValue)
    assert_false(parameterSet.noMatchingTypesInCsource)
    assert_equal(classNameExcludedSet, parameterSet.classNameExcludedSet)
    assert_equal(testedFilenameGlobSet, parameterSet.testedFilenameGlobSet)
    assert_equal(findStatementFilterSet, parameterSet.findStatementFilterSet)
    assert_equal(explicitClassNameSet, parameterSet.explicitClassNameSet)
    assert_equal(mockGuardName, parameterSet.mockGuardName)
    assert_equal(noSwapInlineFunction, parameterSet.noSwapInlineFunction)
  end

  data(
    'nil' => [nil, false],
    'empty' => [[], false],
    'c only' => [["a.c"], true],
    'c only multi' => [["a.c", "b.c"], true],
    'cc only' => [["a.cc"], false],
    'cpp only' => [["a.cpp"], false],
    'mixed' => [["a.cpp", "b.c"], false])
  def test_hasCsourceFilesOnly(data)
    arg, expected = data
    parameterSet = CppFileParameterSet.new(nil, nil, nil, nil, nil, nil, nil, arg,
                                           nil, false, false, false, false, nil,
                                           [], [], [], [], nil, false)
    assert_equal(expected, parameterSet.noMatchingTypesInCsource)
    assert_equal(expected, parameterSet.hasCsourceFilesOnly?(arg))
  end
end

class TestCppIoFilenameSet < Test::Unit::TestCase
  def test_all
    classFilename = "a"
    typeSwapperFilename = "b"
    varSwapperFilename = "c"
    declFilename = "d"
    defFilename = "e"
    numberOfClassInFile = 7

    filenameSet = CppIoFilenameSet.new(classFilename, typeSwapperFilename, varSwapperFilename,
                                       declFilename, defFilename, numberOfClassInFile)
    assert_equal(classFilename, filenameSet.classFilename)
    assert_equal(typeSwapperFilename, filenameSet.typeSwapperFilename)
    assert_equal(varSwapperFilename, filenameSet.varSwapperFilename)
    assert_equal(declFilename, filenameSet.declFilename)
    assert_equal(defFilename, filenameSet.defFilename)
    assert_equal(numberOfClassInFile, filenameSet.numberOfClassInFile)
  end
end

class CppFileParserNilArgSet < CppFileParameterSet
  def initialize(cppNameSpace)
    super(cppNameSpace, nil, nil, nil, false, [], [], [],
          false, false, false, false, false, nil,
          [], [], [], [], nil, false)
  end
end

class TestCppFileParser < Test::Unit::TestCase
  data(
    'empty' => [nil, nil],
    'mock'  => ["A", nil],
    'all'   => [nil, "B"],
    'both'  => ["A", "B"])
  def test_classParameter(data)
    patternMock, patternAll = data

    argSet = CppFileParserNilArgSet.new("NameSpace")
    argSet.instance_variable_set(:@classNameFilterOutSet, [patternMock]) if patternMock
    argSet.instance_variable_set(:@classNameExcludedSet, [patternAll]) if patternAll
    parser = CppFileParser.new(argSet)
    factory = parser.instance_variable_get(:@blockFactory)
    classParameter = factory.instance_variable_get(:@classParameter)

    assert_equal(!patternMock.nil?, classParameter.filterToMock.excludeSimpleName?(patternMock))
    assert_equal(!patternAll.nil?, classParameter.filterToAll.excludeSimpleName?(patternAll))
  end

  def test_getOutFilename
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    filename = "___FileNotExistInTesting___._cpp"
    assert_equal(filename, parser.getOutFilename(filename))

    parser.instance_variable_set(:@updateChangesOnly, true)
    assert_equal(filename + ".new", parser.getOutFilename(filename))
  end

  def test_updateFile
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    filename = "___FileNotExistInTesting___._cpp"
    # Nothing happens on filesystems
    parser.updateFile(filename, filename)
    parser.updateFile(filename + ".old", filename)
  end

  def test_readAllLines
    lines = ["class A {",
             "# 12",  # A preprocessor directive
             "};",
             "class B {",
             "    void func(void) { return; } };",
             "class C {",
             "};",
             "void freeFunc(void) {",
             "    return;} ",
             "class D {",
             "};",
             ""]
    expectedSet = [
      ["class A {", 0, nil],
      ["};", 1, nil],
      ["class B {", 0, nil],
      ["void func(void) { return; }", 1, nil],
      ["};", 1, nil],
      ["class C {", 0, nil],
      ["};", 1, nil],
      ["void freeFunc(void) {", 0, nil],
      ["return;", 1, nil],
      ["}", 1, nil],
      ["class D {", 0, nil],
      ["};", 1, nil],
    ]

    results = []
    linenumber = 0

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parser.define_singleton_method(:parseLine) do |line, currentDepth, keyDepth|
      results << [line, currentDepth, keyDepth]
      super(line, currentDepth, keyDepth)
    end

    file = Object.new
    file.define_singleton_method(:gets) do
      line = (linenumber >= lines.size) ? nil : lines[linenumber]
      linenumber += 1
      line
    end

    parser.readAllLines(file)
    assert_equal(expectedSet.size, results.size)
    expectedSet.each_with_index do |expected, i|
      assert_true(i < results.size)
      assert_equal(expected, results[i])
    end
  end

  data(
    'empty' => ['', nil, 0, ""],
    'var'   => ['a = 1;', nil, 0, ""],
    'close block 1' => ["};", nil, 0, ""],
    'close block 2' => [" }", nil, 0, ""],
    'close block 3' => [" }}", nil, 0, ""],
    'non block 1' => ["os << '}';", nil, 0, ""],
    'non block 2' => ['os << "}";', nil, 0, ""],
    'class'  => ['class C { };', nil, 0, ""],
    'block'  => ['void f() { return; }', nil, 0, ""],
    'nested' => ['void f() { { return; } }', nil, 0, ""],
    'quoted 1' => ["void f() { os << '}';}", nil, 0, ""],
    'quoted 2' => ['void f() { os << "}";}', nil, 0, ""],
    'open 1' => ['void f() { return; }}', 'void f() { return; } ', 1, ""],
    'open 2' => ['void f() { return; }} } ', 'void f() { return; }    ', 2, ""],
    'open 3' => ['void f() { return; }}}};', 'void f() { return; }   ', 3, ";"],
    'quoted 3' => ['void f() { os << "}"; }}}', 'void f() { os << "}"; }  ', 2, ""],
  )
  def test_adjustClosingBlock(data)
    line, expectedLine, expectedCount, expectedEnd = data
    expectedLine = line.dup unless expectedLine

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    actual, count, semicolon = parser.adjustClosingBlock(line)
    assert_equal(expectedLine, actual)
    assert_equal(expectedCount, count)
    assert_equal(expectedEnd, semicolon)
  end

  data(
    'empty' => ["", 0, nil, false, 0, nil, false, false],
    'enter a block' => ["{", 0, nil, false, 1, nil, false, false],
    'enter a inner block' => ["{", 1, nil, false, 2, nil, false, false],
    'enter a key block 1' => ["{", 0, nil, true, 1, 1, false, false],
    'enter a key block 2' => ["{", 1, nil, true, 2, 2, false, false],
    'enter a key block with multi level 1' => ["{{",  1, nil, true, 3, 2, false, false],
    'enter a key block with multi level 2' => ["{{{", 0, nil, true, 3, 1, false, false],
    'reenter a key block 1' => ["{",  1, 1, true, 2, 1, true, false],
    'reenter a key block 2' => ["{{", 2, 1, true, 4, 1, true, false],
    'else clause 1' => ["} else {", 0, nil, false, 0, nil, false, false],
    'else clause 2' => ["} else {", 1, nil, false, 1, nil, false, false],
    'else clause 3' => ["} else {", 1, nil, true,  1, nil, false, false],
    'else clause skipped 1' => ["} else {", 1, 1, false, 1, 1, true, false],
    'else clause skipped 2' => ["} else {", 2, 1, false, 2, 1, true, false],
    'exit a block 1' => ["}", 1, nil, false, 0, nil, false, false],
    'exit blocks' =>   ["}}", 3, nil, false, 1, nil, false, false],
    'exit too many blocks' => ["}}}", 1, nil, false, 0, nil, false, false],
    'exit not a key block' => ["}", 2, 1, false, 1, 1, true, false],
    'exit a key block' => ["}",  1, 1, false, 0, nil, true, true],
    'exit a key and not key blocks' => ["}}", 2, 1, false, 0, nil, true, true],
    'exit too many key blocks 1' => ["}}}", 3, 1, false, 0, nil, true, true],
    'exit too many key blocks 2' => ["}}}", 1, 1, false, 0, nil, true, true]
  )
  def test_parseLine(data)
    line, currentDepth, keyDepth, skipInner, expectedDepth, expectedKeyDepth, expectedSkipping, expectedClosing = data

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_true(parser.instance_variable_get(:@variableMockStr).empty?)
    actualSkipping = nil
    actualClosing = nil
    parser.define_singleton_method(:parseBlock) do |line, skipping, closing|
      actualSkipping = skipping
      actualClosing = closing
      skipInner
    end

    newDepth, newKeyDepth = parser.parseLine(line, currentDepth, keyDepth)
    assert_equal(expectedDepth, newDepth)
    assert_equal(expectedKeyDepth, newKeyDepth)
    assert_equal(expectedSkipping, actualSkipping)
    assert_equal(expectedClosing, actualClosing)
  end

  def test_parseLineAndBlock
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_true(parser.instance_variable_get(:@variableMockStr).empty?)

    [["comment", 0, 0],
     ["class Base {", 0, 1],
     ["public:", 1, 1],
     ["void FuncA() {", 1, 2],
     ["if (0) {", 2, 3],
     ["} else {", 3, 3],
     ["}", 3, 2],
     ["}", 2, 1],
     ["void FuncB();", 1, 1],
     ["};", 1, 0],
     ["};", 1, 0]].each do |line, currentDepth, expectedDepth|
      depth, newKeyDepth = parser.parseLine(line, currentDepth, nil)
      assert_equal(expectedDepth, depth)
      assert_nil(newKeyDepth)
    end
  end

  def test_parseBlock
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_true(parser.instance_variable_get(:@variableMockStr).empty?)

    argSet = [false, false]
    originalBlock = parser.instance_variable_get(:@block)
    assert_false(parser.parseBlock("comment", *argSet))
    assert_equal(originalBlock, parser.instance_variable_get(:@block))

    parser.parseBlock("class Base {", *argSet)
    classBlock = parser.instance_variable_get(:@block)
    assert_not_equal(originalBlock, classBlock)

    parser.parseBlock("public:", *argSet)
    parser.parseBlock("void FuncA() {", *argSet)
    funcBlockA = parser.instance_variable_get(:@block)
    assert_not_equal(classBlock, funcBlockA)

    parser.parseBlock("if (0) {", *argSet)
    ifBlock = parser.instance_variable_get(:@block)
    assert_not_equal(funcBlockA, ifBlock)

    parser.parseBlock("} else {", *argSet)
    assert_equal(ifBlock, parser.instance_variable_get(:@block))

    parser.parseBlock("}", *argSet)
    assert_equal(funcBlockA, parser.instance_variable_get(:@block))
    parser.parseBlock("}", *argSet)
    assert_equal(classBlock, parser.instance_variable_get(:@block))
    assert_equal(classBlock, funcBlockA.parent)

    parser.parseBlock("void FuncB();", *argSet)
    assert_equal(classBlock, parser.instance_variable_get(:@block))

    parser.parseBlock("};", *argSet)
    assert_equal(originalBlock, parser.instance_variable_get(:@block))
    assert_equal(3, classBlock.children.size)
    assert_equal(funcBlockA, classBlock.children[1])
    assert_false(classBlock.instance_variable_get(:@publicMemberFunctionSet).empty?)
  end

  def test_parseSkippedBlock
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_true(parser.instance_variable_get(:@variableMockStr).empty?)
    factory = parser.instance_variable_get(:@blockFactory)
    factory.define_singleton_method(:createBlock) { |argLine, parentBlock| [BaseBlock.new(""), true] }

    [["namespace A {", false, false, true, true],
     ["namespace B {", false, false, true, true],
     ["namespace C {", true,  false, false, false],
     ["class Name {",  true,  false, false, false],
     ["};", true, false, false, false],
     ["}",  true, true,  false, true],
     ["}", false, true,  false, true],
     ["}", false, false, false, true]].each do |line, skipping, closing, expectedSkipInner, updated|
      originalBlock = parser.instance_variable_get(:@block)
      assert_equal(expectedSkipInner, parser.parseBlock(line, skipping, closing))
      assert_equal(originalBlock, parser.instance_variable_get(:@block)) unless updated
      assert_not_equal(originalBlock, parser.instance_variable_get(:@block)) if updated
    end
  end

  def test_parseTypedefAndStruct
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))

    argSet = [false, false]
    parser.parseBlock("typedef class tagName {", *argSet)
    block = parser.instance_variable_get(:@block)
    assert_true(block.isClass?)
    assert_not_nil(block)

    parser.parseBlock("void FuncA();", *argSet)
    parser.parseBlock("} Name;", *argSet)

    typedefBlock = block.instance_variable_get(:@typedefBlock)
    assert_not_nil(typedefBlock)
    assert_true(typedefBlock.canTraverse?)
  end

  def test_parseLineRaise
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parser.instance_variable_set(:@blockFactory, nil)
    message = ""
    parser.define_singleton_method(:warn) { |line| message += line }

    argSet = [false, false]
    expr = -> {parser.parseBlock("class Name {", *argSet)}
    caught, ignored = checkIfExceptionCaught(expr)
    assert_true(caught)
    assert_false(ignored)
    assert_false(message.empty?)
  end

  def test_eliminateUnusedBlock
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parentBlock = BaseBlock.new("Parent")
    childBlock = BaseBlock.new("Child")
    parentBlock.connect(childBlock)
    assert_equal(parentBlock, childBlock.parent)
    assert_equal([childBlock], parentBlock.children)

    parser.eliminateUnusedBlock(parentBlock)
    parser.eliminateUnusedBlock(childBlock)
    assert_nil(childBlock.parent)

    assert_equal([childBlock], parentBlock.children)
    parentBlock.eraseChildren
    assert_equal([], parentBlock.children)
  end

  def test_eliminateAllUnusedBlock
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parentBlock = BaseBlock.new("Parent")
    childBlock = BaseBlock.new("Child")
    parentBlock.connect(childBlock)

    parser.eliminateAllUnusedBlock(childBlock)
    assert_not_nil(childBlock.parent)

    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    nsBlock = NamespaceBlock.new("namespace std {")
    rootBlock.connect(nsBlock)
    nsBlock.connect(parentBlock)

    parser.eliminateAllUnusedBlock(rootBlock)
    assert_nil(nsBlock.parent)
    assert_equal([], rootBlock.children)
  end

  def test_mergeFreeFunctionSetArray
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))

    rootBlock = RootBlock.new("")
    ns1 = NamespaceBlock.new("namespace A {")
    ns2 = NamespaceBlock.new("namespace A {")
    ec1 = ExternCBlock.new('extern "C" {')
    ec2 = ExternCBlock.new('extern "C" {')
    rootBlock.connect(ns1)
    rootBlock.connect(ns2)
    rootBlock.connect(ec1)
    ec1.connect(ec2)

    freeFunctionSetArray = [FreeFunctionSet.new(rootBlock),
                            FreeFunctionSet.new(ns1),
                            FreeFunctionSet.new(ec1),
                            FreeFunctionSet.new(ec2),
                            FreeFunctionSet.new(ns2)]

    serial = 1
    freeFunctionSetArray.each do |set|
      set.instance_variable_set(:@funcSet, [serial])
      set.instance_variable_set(:@undefinedFunctionSet, [serial])
      serial += 1
    end

    actual = parser.mergeFreeFunctionSetArray(freeFunctionSetArray)
    assert_equal(2, actual.size)
    assert_equal([1,3,4], actual[0].instance_variable_get(:@funcSet))
    assert_equal([2,5], actual[1].instance_variable_get(:@funcSet))
  end

  data(
    'empty' => [[], 3],
    'non number' => [['Sample', 'Other'], 2],
    'number' => [['Func\\d'], 1])
  def test_collectFreeFunctions(data)
    filterSet, expected = data
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    rootBlock = RootBlock.new("")
    nsBlock = NamespaceBlock.new("namespace A {")
    ecBlock = ExternCBlock.new('extern "C" {')
    rootBlock.connect(nsBlock)
    rootBlock.connect(ecBlock)

    funcSetRoot = FreeFunctionSet.new(rootBlock)
    funcSetNamespace = FreeFunctionSet.new(nsBlock)
    nsBlock.connect(funcSetNamespace)
    funcSetC = FreeFunctionSet.new(ecBlock)
    ecBlock.connect(funcSetC)

    funcRootA = FreeFunctionBlock.new("extern int FuncSampleA(int a);")
    funcRootB = FreeFunctionBlock.new("extern int FuncOtherB(int a);")
    funcRoot1 = FreeFunctionBlock.new("extern int Func1(int a);")
    rootBlock.connect(funcRootA)
    rootBlock.connect(funcRootB)
    rootBlock.connect(funcRoot1)

    funcRootC = FreeFunctionBlock.new("extern int FuncSampleC(int a);")
    funcRootD = FreeFunctionBlock.new("extern int FuncOtherD(int a);")
    funcRoot2 = FreeFunctionBlock.new("extern int Func2(int a);")
    nsBlock.connect(funcRootC)
    nsBlock.connect(funcRootD)
    nsBlock.connect(funcRoot2)

    actual = parser.collectFreeFunctions(funcSetRoot, rootBlock, filterSet)
    assert_equal(3, actual.size)
    assert_equal(expected, actual[0].instance_variable_get(:@funcSet).size)
    assert_equal(expected, actual[1].instance_variable_get(:@funcSet).size)
  end

  def test_collectFreeFunctionsRaise
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    message = ""
    parser.define_singleton_method(:warn) { |line| message += line }

    rootBlock = RootBlock.new("")
    funcSetRoot = FreeFunctionSet.new(rootBlock)
    funcRoot = FreeFunctionBlock.new("extern int Func1(int a);")
    rootBlock.connect(funcRoot)

    parser.collectFreeFunctions(funcSetRoot, rootBlock, ['Func\\d'])

    MockgenFeatures.mockGenTestingException = true
    caught = false
    begin
      parser.collectFreeFunctions(funcSetRoot, rootBlock, ['Func('])
    rescue
      caught = true
    ensure
      MockgenFeatures.mockGenTestingException = false
    end

    assert_true(caught)
    assert_false(message.empty?)
  end

  data(
    'empty' => ["", "", ""],
    'no extension empty postfix' => ["base", "", "base"],
    'empty postfix' => ["base.hpp", "", "base.hpp"],
    'no extension' => ["base", "_1", "base_1"],
    'with extension' => ["base.hpp", "_1", "base_1.hpp"],
    'current dir' => ["./base.hpp", "_1", "./base_1.hpp"],
    'parent dir' => ["../dir/base.hpp", "_1", "../dir/base_1.hpp"],
    'begin with period' => [".ext", "_1", "_1.ext"],
    'end with period' => ["base.", "_1", "base_1."])
  def test_addPostfixToBasename(data)
    name, postfix, expected = data
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal(expected, parser.addPostfixToBasename(name, postfix))
  end

  def test_buildClassTree
    [false, true].each do |testBuild|
      blockA = TestClassMock.new("A", "ClassA", [])
      blockB = TestClassMock.new("B", "", [])
      blockC = TestClassMock.new("C", "", [blockB, blockA])
      blockSet = [blockA, blockB, blockC]
      parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))

      if (testBuild)
        blockD = TestClassMock.new("D", "", [blockC])
        parser.instance_variable_set(:@block, blockD)
        parser.buildClassTree(MinimumSymbolFilter.new(nil, nil))
      else
        classSet = parser.collectClasses([blockC], MinimumSymbolFilter.new(nil, nil))
        assert_equal({ "ClassA" => blockA }, classSet)
        parser.connectClasses([blockC], classSet)
      end
      assert_true(blockSet.all?(&:visited))
    end
  end

  def test_collectClasses
    block = TestClassMock.new("Base", "A::Base", [])
    blockV = ExternVariableStatement.new("extern Base varB_;")

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal({"A::Base" => block}, parser.collectClasses([block, blockV], MinimumSymbolFilter.new(nil, nil)))
  end

  def test_filterOutClassInSourceFile
    blockA = ClassBlock.new("class NameA {")
    blockB = ClassBlock.new("class NameB {")

    refSet = Object.new
    def refSet.relativeNamespaceOnly
      false
    end

    def refSet.classDefined?(relativeClassName, relativeNamespaceOnly)
      relativeClassName.include?("A")
    end

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parser.collectClasses([blockA, blockB], MinimumSymbolFilter.new(refSet, nil))
    assert_true(blockA.instance_variable_get(:@alreadyDefined))
    assert_false(blockB.instance_variable_get(:@alreadyDefined))
  end

  def test_filterOutFreeFunctionInSourceFile
    blockA = FreeFunctionBlock.new("extern int FuncA(int a);")
    blockB = FreeFunctionBlock.new("extern int FuncB(int a);")

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))

    defRefSet = Object.new
    def defRefSet.freeFunctionDefined?(name)
      name.include?("A")
    end

    undefRefSet = Object.new
    def undefRefSet.getReferenceSetByFullname(fullname)
      []
    end

    parser.instance_variable_set(:@presetFunctinSet, [blockA, blockB])
    def parser.collectFreeFunctions(a,b,c)
      @presetFunctinSet
    end

    def parser.mergeFreeFunctionSetArray(a)
    end

    parser.buildFreeFunctionSet(MinimumSymbolFilter.new(defRefSet, undefRefSet))
    assert_true(blockA.instance_variable_get(:@alreadyDefined))
    assert_false(blockB.instance_variable_get(:@alreadyDefined))
  end

  def test_connectClasses
    blockB = TestClassMock.new("Base", "A::Base", [])
    blockD = ClassBlock.new("class Derived : public A::Base {")
    classSet = { "::A::Base" => blockB, "::Derived" => blockD }

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parser.connectClasses([blockB, blockD], classSet)
    assert_equal(["::A::Base"], blockD.instance_variable_get(:@baseClassNameSet))
  end

  def test_collectVariables
    ns = NamespaceBlock.new("namespace A")
    varB = ExternVariableStatement.new("extern Base varB_;")
    varD = ExternVariableStatement.new("extern Derived varD_;")
    ns.connect(varB)
    ns.connect(varD)

    blockSet = [varB, varD]
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    expected = [["varB_", "::A::varB_", "Base"],["varD_", "::A::varD_", "Derived"]]
    assert_equal(expected, parser.collectVariables(blockSet))
  end

  def test_collectTypedefs
    rootBlock = RootBlock.new("")
    ns = NamespaceBlock.new("namespace A {")
    ec = ExternCBlock.new('extern "C" {')
    rootBlock.connect(ns)
    rootBlock.connect(ec)
    subec = ExternCBlock.new('extern "C" {')
    ec.connect(subec)

    aliasNs = TypedefBlock.new("typedef long Counter;")
    ns.connect(aliasNs)

    aliasStruct = TypedefBlock.new("typedef struct tagStructName {")
    aliasStruct.setAlias("StructName")
    ec.connect(aliasStruct)

    aliasInt = TypedefBlock.new("typedef int INT;")
    subec.connect(aliasInt)

    aliasSystem = TypedefBlock.new("typedef int __int;")
    subec.connect(aliasSystem)

    aliasRoots = TypedefBlock.new("typedef long long LongCounter;")
    rootBlock.connect(aliasRoots)

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parser.collectTypedefs(rootBlock)
    assert_equal({"Counter"=>"long"}, ns.typeAliasSet.aliasSet)
    assert_equal({"StructName"=>"tagStructName"}, ec.typeAliasSet.aliasSet)
    assert_equal({"INT"=>"int", "__int"=>"int"}, subec.typeAliasSet.aliasSet)
    assert_equal({"LongCounter"=>"long long", "StructName"=>"tagStructName", "INT"=>"int"}, rootBlock.typeAliasSet.aliasSet)
  end

  def test_makeVariableStubs
    varSet = [["", "var1", "NameA"], ["", "var2", "NameB"], ["", "var1", "NameA"]]

    refSet = Object.new
    def refSet.getGlobalVariableStub(varFullname, typeName)
      return varFullname + typeName
    end

    testFilter = Struct.new(:undefinedReferenceSet)
    filter = testFilter.new(refSet)

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal("var1NameA\nvar2NameB", parser.makeVariableStubs(varSet, filter))
  end

  def test_makeTypeVarAliases
    classNameB = "Base"
    classNameD = "Derived"
    blockB = ClassBlock.new("class #{classNameB}")
    blockD = ClassBlock.new("class #{classNameD}")
    ns = NamespaceBlock.new("namespace A")
    rootBlock = RootBlock.new("")
    rootBlock.connect(ns)
    ns.connect(blockB)
    rootBlock.connect(blockD)
    blockSet = [blockB, blockD]

    nsName = "TestNameSpace"
    parser = CppFileParser.new(CppFileParserNilArgSet.new(nsName))

    varNameB = "varB_"
    varNameC = "varC_"
    varNameD = "varD_"
    varSet = [[varNameB, "::#{varNameB}", blockB.getFullname], [varNameD, "::A::#{varNameD}", blockD.getFullname]]
    classSet = {blockB.getFullname => blockB, blockD.getFullname => blockD}

    varSet.concat([[varNameC, "::#{varNameC}", blockB.getFullname]])
    classInstanceMap = parser.makeTypeVarAliases(varSet, classSet)

    tsStr = ""
    vsStr = ""
    declStr = ""
    defStr = ""
    blockSet.each do |block|
      tsStr += classInstanceMap.getInstanceSet(block.getFullname).map(&:typeSwapperStr).join()
      vsStr += classInstanceMap.getInstanceSet(block.getFullname).map(&:varSwapperStr).join()
      declStr += classInstanceMap.getInstanceSet(block.getFullname).map(&:declStr).join()
      defStr += classInstanceMap.getInstanceSet(block.getFullname).map(&:defStr).join()
    end

    postfix = Mockgen::Constants::CLASS_POSTFIX_DECORATOR
    head =  "using ::A::#{classNameB};\n#define #{classNameB} ::#{nsName}::#{classNameB}#{postfix}\n"
    expectedNoDuplication = head + "#define #{classNameD} ::#{nsName}::#{classNameD}#{postfix}\n"
    # Duplate the definition of classNameB
    expectedDuplication = head + expectedNoDuplication
    assert_equal(expectedDuplication, tsStr)
    assert_equal(expectedNoDuplication, tsStr.split("\n").uniq.join("\n") + "\n")

    postfix = Mockgen::Constants::CLASS_POSTFIX_FORWARDER
    expected =  "using ::#{varNameB};\n#define #{varNameB} ::#{nsName}::#{varNameB}#{postfix}\n"
    expected += "using ::#{varNameC};\n#define #{varNameC} ::#{nsName}::#{varNameC}#{postfix}\n"
    expected += "using ::A::#{varNameD};\n#define #{varNameD} ::#{nsName}::#{varNameD}#{postfix}\n"
    assert_equal(expected, vsStr)

    expected =  "extern #{classNameB}#{postfix} #{varNameB}#{postfix};\n"
    expected += "extern #{classNameB}#{postfix} #{varNameC}#{postfix};\n"
    expected += "extern #{classNameD}#{postfix} #{varNameD}#{postfix};\n"
    assert_equal(expected, declStr)

    expected =  "#{classNameB}#{postfix} #{varNameB}#{postfix}(::#{varNameB});\n"
    expected += "#{classNameB}#{postfix} #{varNameC}#{postfix}(::#{varNameC});\n"
    expected += "#{classNameD}#{postfix} #{varNameD}#{postfix}(::A::#{varNameD});\n"
    assert_equal(expected, defStr)
  end

  data(
    'topLevel' => false,
    'in a namespace' => true)
  def test_makeTypeVarAliasElements(data)
    useNamespace = data
    ns = NamespaceBlock.new("namespace A")
    className = "NameB"
    block = ClassBlock.new("class #{className}")
    ns.connect(block) if useNamespace

    classSet = {className => block }
    varName = "var_"
    varFullname = useNamespace ? "::A::#{varName}" : "#{varName}"

    nsName = "NameSpace"
    parser = CppFileParser.new(CppFileParserNilArgSet.new(nsName))
    postfix = Mockgen::Constants::CLASS_POSTFIX_DECORATOR
    tsStr, vsStr, declStr, defStr = parser.makeTypeVarAliasElements(classSet, varName, varFullname, className)
    assert_equal("#define #{className} ::#{nsName}::#{className}#{postfix}\n", tsStr)

    postfix = Mockgen::Constants::CLASS_POSTFIX_FORWARDER
    expected = (useNamespace ? "using ::A::#{varName};\n" : "")
    expected += "#define #{varName} ::#{nsName}::#{varName}#{postfix}\n"
    assert_equal(expected, vsStr)
    assert_equal("extern #{className}#{postfix} #{varName}#{postfix};\n", declStr)
    assert_equal("#{className}#{postfix} #{varName}#{postfix}(#{varFullname});\n", defStr)

    tsStr, vsStr, declStr, defStr = parser.makeTypeVarAliasElements(classSet, varName, varFullname, "NoClass")
    assert_equal("", tsStr)
    assert_equal("", vsStr)
    assert_equal("", declStr)
    assert_equal("", defStr)
  end

  data(
    'topLevel' => false,
    'in a namespace' => true)
  def test_makeTypeVarAliasElementsForArray(data)
    useNamespace = data
    ns = NamespaceBlock.new("namespace A")
    className = "NameB"
    block = ClassBlock.new("class #{className}")
    ns.connect(block) if useNamespace

    classSet = {className => block }
    varBase = "aVariable"
    arratStr = "[32]"
    varName = varBase + arratStr
    varFullname = useNamespace ? "::A::#{varName}" : "#{varName}"

    nsName = "NameSpace"
    parser = CppFileParser.new(CppFileParserNilArgSet.new(nsName))
    postfix = Mockgen::Constants::CLASS_POSTFIX_DECORATOR
    tsStr, vsStr, declStr, defStr = parser.makeTypeVarAliasElements(classSet, varName, varFullname, className)
    assert_equal("#define #{className} ::#{nsName}::#{className}#{postfix}\n", tsStr)

    postfix = Mockgen::Constants::CLASS_POSTFIX_FORWARDER
    expected = (useNamespace ? "using ::A::#{varBase};\n" : "")
    expected += "#define #{varBase} ::#{nsName}::#{varBase}#{postfix}\n"
    assert_equal(expected, vsStr)

    expected = "extern #{className}#{postfix} #{varBase}#{postfix}#{arratStr};\n"
    assert_equal(expected, declStr)
    assert_equal("", defStr)
  end

  def test_makeTypeVarAliasWithArgs
    className = "NameB"
    block = ClassBlock.new("class #{className}")
    block.parseChildren("public:")
    ctorBlock = block.parseChildren("#{className}(int a,T* p);")
    block.connect(ctorBlock)

    classSet = {className => block }
    varName = "var_"

    nsName = "NameSpace"
    parser = CppFileParser.new(CppFileParserNilArgSet.new(nsName))
    postfix = Mockgen::Constants::CLASS_POSTFIX_DECORATOR
    tsStr, vsStr, declStr, defStr = parser.makeTypeVarAliasElements(classSet, varName, varName, className)
    assert_equal("#define #{className} ::#{nsName}::#{className}#{postfix}\n", tsStr)

    postfix = Mockgen::Constants::CLASS_POSTFIX_FORWARDER
    expected = "#define #{varName} ::#{nsName}::#{varName}#{postfix}\n"
    assert_equal(expected, vsStr)
    assert_equal("extern #{className}#{postfix} #{varName}#{postfix};\n", declStr)
    assert_equal("#{className}#{postfix} #{varName}#{postfix}(#{varName},0,0);\n", defStr)
  end

  def test_makeClassSetRaise
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    parser.instance_variable_set(:@block, nil)
    message = ""
    parser.define_singleton_method(:warn) { |line| message += line }

    expr = -> {parser.makeClassSet}
    caught, ignored = checkIfExceptionCaught(expr)
    assert_true(caught)
    assert_false(ignored)
    assert_false(message.empty?)
  end

  def test_doForAllBlocksAndBlockSet
    blockA = TestClassMock.new("A", "", [])
    blockB = TestClassMock.new("B", "", [blockA])
    blockC = TestClassMock.new("C", "", [])
    blockD = TestClassMock.new("D", "", [blockC])
    blockE = TestClassMock.new("E", "", [blockD, blockB])

    log = ""
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    lambdaToBlock = lambda { |block| log = log + block.name }
    parser.doForAllBlocks([blockE], lambdaToBlock, :isClass?)
    assert_equal("EDBCA", log)

    log = ""
    parser.doForBlockSet([blockA, blockD], lambdaToBlock)
    assert_equal("AD", log)
  end

  def test_collectTopLevelTypedefSet
    rootBlock = BlockFactory.new(false, nil, nil).createRootBlock
    ecBlock1 = ExternCBlock.new('extern "C" {')
    ecBlock2 = ExternCBlock.new('extern "C" {')
    rootBlock.connect(ecBlock1)
    rootBlock.connect(ecBlock2)

    ecTypedefBlock = TypedefBlock.new("typedef int64 MyInt;")
    ecBlock2.connect(ecTypedefBlock)
    ecBlock2.collectAliases

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal(1, parser.collectTopLevelTypedefSet(rootBlock).flatten.size)
  end

  def test_collectClassesToWrite
    mockClass = Struct.new(:canTraverse?, :isClass?, :children, :name)
    # [A[c, D[*G, *H]], B[e[I, J], *F] : * = class, lower case = cannot traverse
    blockJ = mockClass.new(false, true, [], "J")
    blockI = mockClass.new(false, true, [], "I")
    blockH = mockClass.new(true, true, [], "H")
    blockG = mockClass.new(true, true, [], "G")
    blockF = mockClass.new(true, true, [], "F")
    blockE = mockClass.new(false, false, [blockI, blockJ], "E")
    blockD = mockClass.new(true, false, [blockG, blockH], "D")
    blockC = mockClass.new(false, false, [], "C")
    blockB = mockClass.new(true, false, [blockE, blockF], "B")
    blockA = mockClass.new(true, false, [blockC, blockD], "A")

    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    actual = parser.collectClassesToWrite([blockA, blockB])
    assert_equal([[[blockG, [], blockH, []]], [blockF, []]], actual)
    assert_equal("GHF", actual.flatten.map(&:name).join(""))
  end

  data(
    'mock' => true,
    'othres' => false)
  def test_getClassFileHeader(data)
    writeMacro = data
    inputFilename = "in.hpp"
    outClassFilename = "out.hpp"
    nsName = "MyTest"
    parser = CppFileParser.new(CppFileParserNilArgSet.new(nsName))
    actual = parser.getClassFileHeader(inputFilename, outClassFilename, writeMacro)

    assert_true(actual.include?("generated"))
    assert_true(actual.include?("#include <gmock/gmock.h>\n"))

    expected =  "#ifndef MOCK_OF\n#define MOCK_OF(className) ::MyTest::className###{Mockgen::Constants::CLASS_POSTFIX_MOCK}\n#endif\n"
    expected += "#ifndef DECORATOR\n#define DECORATOR(className) ::MyTest::className###{Mockgen::Constants::CLASS_POSTFIX_DECORATOR}\n#endif\n"
    expected += "#ifndef FORWARDER\n#define FORWARDER(className) ::MyTest::className###{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}\n#endif\n"
    expected += "#ifndef INSTANCE_OF\n#define INSTANCE_OF(varName) ::MyTest::varName###{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}\n#endif\n"
    assert_equal(writeMacro, actual.include?(expected))
  end

  def test_getSwapperHeader
    declFilename = "decl.hpp"
    swapperFilename = "swap.hpp"
    headWord = "Title"
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    actual = parser.getSwapperHeader(declFilename, swapperFilename, headWord)

    expected =  "// #{headWord} name swapper definition\n"
    assert_true(actual.include?(expected))
    assert_true(actual.include?("generated"))

    expected =  "#ifndef SWAP_HPP\n#define SWAP_HPP\n\n"
    expected += "#include " + '"' + declFilename + '"' +"\n\n"
    assert_true(actual.include?(expected))
  end

  def test_getDeclHeader
    classFilename = "class.hpp"
    declFilename = "decl.hpp"
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    expected = "// Variable declarations\n// This file is machine generated.\n\n"
    expected += "#ifndef DECL_HPP\n#define DECL_HPP\n\n"
    expected += "#include " + '"' + classFilename + '"' +"\n\n"
    assert_equal(expected, parser.getDeclHeader(classFilename, declFilename))
  end

  def test_getDefHeader
    filename = "out.hpp"
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    expected = "// Variable definitions\n// This file is machine generated.\n\n"
    expected += "#include " + '"' + filename + '"' +"\n\n"
    assert_equal(expected, parser.getDefHeader("ifilename", "ofilename", filename))
  end

  data(
    'file' => ["headerPub.hpp", "#ifndef HEADER_PUB_HPP\n#define HEADER_PUB_HPP\n\n"],
    'dir'  => ["top/sub/HeaderImpl.hpp", "#ifndef HEADER_IMPL_HPP\n#define HEADER_IMPL_HPP\n\n"])
  def test_getIncludeGuardHeader(data)
    filename, expected = data
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal(expected, parser.getIncludeGuardHeader(filename))
  end

  def test_getIncludeGuardFooter
    assert_equal("#endif\n",CppFileParser.new(CppFileParserNilArgSet.new("NameSpace")).getIncludeGuardFooter)
  end

  data(
    'empty' => ["", ""],
    'lower' => ["header.hpp", "HEADER_HPP"],
    'upper' => ["HEADER.hpp", "HEADER_HPP"],
    'camel1' => ["localFileHeader.hpp", "LOCAL_FILE_HEADER_HPP"],
    'camel2' => ["LocalFileHeader.hpp", "LOCAL_FILE_HEADER_HPP"],
    'num1' => ["bin2Text.hpp", "BIN2_TEXT_HPP"],
    'num2' => ["4parseText.hpp", "4PARSE_TEXT_HPP"],
    'underbars' => ["4_parse__Text_.hpp", "4_PARSE_TEXT_HPP"],
    'dir1' => ["top/sub/HeaderImpl.hpp", "HEADER_IMPL_HPP"],
    'dir2' => ["top/suB/headerImpl.hpp", "HEADER_IMPL_HPP"])
  def test_getIncludeGuardName(data)
    filename, expected = data
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal(expected, parser.getIncludeGuardName(filename))
  end

  data(
    'empty' => "",
    'basename' => "header.hpp",
    'single dir' => "dir/header.hpp",
    'multi dirs' => "top/sub/detail/header.hpp")
  def test_getIncludeDirective(data)
    filename = data
    expected = filename.empty? ? "" : "#include \"header.hpp\"\n"
    parser = CppFileParser.new(CppFileParserNilArgSet.new("NameSpace"))
    assert_equal(expected, parser.getIncludeDirective(filename))
  end

  # Test the methods write* with .cpp files
end

class TestMockGenLauncher < Test::Unit::TestCase
  data(
    'stub' => ["stub", true,  false],
    'mock' => ["mock", false, false],
    'var' =>  ["var",  true,  true])
  def test_mode(data)
    arg, expectedStubOnly, expectedVarOnly = data
    args = [arg, "", "", "", "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    assert_equal(expectedStubOnly, target.instance_variable_get(:@stubOnly))
    assert_equal(expectedVarOnly, target.instance_variable_get(:@varOnly))
  end

  def test_quote
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB]
    ['\'1\'', '\'2"', '"3\'', '"4"'].each do |str|
      args.concat([Mockgen::Constants::ARGUMENT_CLASS_NAME_FILTER_OUT, str])
    end
    args.concat(getOptionSet())

    target = MockGenLauncher.new(args)
    assert_equal(["1", "2", "3", "4"], target.instance_variable_get(:@classNameFilterOutSet))
  end

  def test_invalidOption
    option = "-unknownKey"
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, option, "1"]
    args.concat(getOptionSet())
    assert_raise(MockgenInvalidArgumentError, "Invalid option #{option}") { MockGenLauncher.new(args) }
  end

  def test_invalidOptionValue
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_FUNCTION_NAME_FILTER, "-file"]
    args.concat(getOptionSet())
    assert_raise(MockgenInvalidArgumentError, "Invalid option -filter -file") { MockGenLauncher.new(args) }
  end

  def test_selectSystemPath
    serial = 1
    argSet = []
    expected = []

    Mockgen::Constants::CLANG_SYSTEM_HEADER_OPTION_SET.each do |opt|
      path = "Path#{serial}"
      argSet << opt << path
      expected <<= path
      serial += 1

      path = "Path with space#{serial}"
      argSet << opt << path
      expected <<= path
      serial += 1

      path = "Path#{serial}"
      argSet << "#{opt}#{path}"
      expected <<= path
      serial += 1

      path = "Path with space#{serial}"
      argSet << "#{opt}#{path}"
      expected <<= path
      serial += 1
    end.join(" ")
    argSet << Mockgen::Constants::CLANG_SYSTEM_HEADER_OPTION_SET[0]

    args = ["mock", "", "", "", "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    assert_equal(expected, target.selectSystemPath(argSet))
    assert_equal(Mockgen::Constants::CLANG_SYSTEM_HEADER_DEFAULT_SET,
                 target.instance_variable_get(:@systemHeaderSet))
    assert_false(target.instance_variable_get(:@checkInternalSystemPath))
    assert_false(target.instance_variable_get(:@fillVtable))
    assert_false(target.instance_variable_get(:@noOverloading))
    assert_false(target.instance_variable_get(:@updateChangesOnly))
    assert_nil(target.instance_variable_get(:@discardNamespaceValue))
    assert_nil(target.instance_variable_get(:@mockGuardName))
    assert_false(target.instance_variable_get(:@noSwapInlineFunction))
  end

  def test_collectInternalIsystemFail
    args = ["mock", Mockgen::Constants::ARGUMENT_SOURCE_FILENAME_FILTER,
            "FileNotExist", Mockgen::Constants::ARGUMENT_CHECK_INTERNAL_SYSTEM_PATH,
            "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    assert_raise(MockgenRuntimeError) { target.collectInternalIsystem("-cc1") }
  end

  def test_collectInternalIsystemPass
    command = "#{Mockgen::Constants::CLANG_COMMAND} -v"
    stdoutstr, stderrstr, status = Open3.capture3(command)
    assert_true(status.success?)

    # assume /usr/bin/clang++ does not write a -internal-isystem list
    keyInstalledDir = "InstalledDir:"
    installedInDefaultDir = !stderrstr.include?(keyInstalledDir) || stderrstr.match(/#{keyInstalledDir}\s+\/usr\/bin/)
    check = installedInDefaultDir ? "" : Mockgen::Constants::ARGUMENT_CHECK_INTERNAL_SYSTEM_PATH
    args = ["mock", Mockgen::Constants::ARGUMENT_SOURCE_FILENAME_FILTER,
            TestInputSourceFileForIsystem, check, "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    assert_nothing_thrown(MockgenRuntimeError) { target.collectInternalIsystem("-cc1") }
  end

  def test_selectClangNonCc1Options
    str = "clang++.exe -cc1 -triple x86_64-pc-windows-gnu -ast-print -internal-isystem /usr/include"
    expected = "clang++.exe  -target x86_64-pc-windows-gnu  -internal-isystem /usr/include"

    args = ["mock", "", "", "", "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    actual = target.selectClangNonCc1Options(str)
    assert_equal(expected, actual)
  end

  def test_selectInternalIsystem
    str =  '"C:\\Program Files\\LLVM\\bin\\clang++.exe" '
    str += '"-cc1" "-triple" "x86_64-pc-windows-gnu" "-ast-print" "-internal-isystem" '
    str += '"C:\\Program Files\\mingw-w64\\x86_64-4.9.2-posix-seh-rt_v3-rev1'
    str += '\\mingw64\\x86_64-w64-mingw32\\include\\c++" '
    str += '"-internal-isystem" '
    str += '"C:\\Program Files\\\\LLVM\\bin\\..\\lib\\clang\\3.8.0\\include" '
    str += '"-lstdc++" "-lmingw32"'

    args = ["mock", "", "", "", "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    actualStr, pathSet = target.selectInternalIsystem(str)

    path1 = '"C:\\Program Files\\mingw-w64\\x86_64-4.9.2-posix-seh-rt_v3-rev1'
    path1 += '\\mingw64\\x86_64-w64-mingw32\\include\\c++"'
    path2 = '"C:\\Program Files\\LLVM\\bin\\..\\lib\\clang\\3.8.0\\include"'

    expected = '-internal-isystem '+ path1
    expected += ' -internal-isystem '+ path2
    assert_equal(expected, actualStr)
    assert_equal([path1, path2], pathSet)
  end

  def test_collectNonInternalIsystemHeaders
    args = ["mock", "", "", "", "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    target.collectNonInternalIsystemHeaders(nil, nil, nil)
  end

  def test_selectNonInternalIsystemHeaders
    logStr =  ". tested_include/mockgenSample1.hpp\n"
    logStr += ". tested_include/mockgenSample2.hpp\n"
    logStr += ". tested_include/mockgenSample1.hpp\n"
    logStr += ".. /usr/include/time.h\n"
    logStr += ".. /usr/lib/gcc/x86_64-pc-cygwin/5.4.0/include/c++/cstdlib\n"
    logStr += "... C:\\\\Program Files\\\\mingw-w64\\\\x86_64-4.9.2-posix-seh-rt_v3-rev1\\\\"
    logStr += "mingw64\\\\x86_64-w64-mingw32\\\\include\\\\c++\\\\cstdlib\n"
    logStr += "src/source.cpp:2:5: error\n"
    logStr += "Error ... other.hpp\n"

    argIsystemPaths = ["/usr/include", "/usr/lib/gcc/x86_64-pc-cygwin/5.4.0/include/c++",
                       "C:\\\\Program Files\\\\mingw-w64\\\\x86_64-4.9.2-posix-seh-rt_v3-rev1" +
                       "\\\\mingw64\\\\x86_64-w64-mingw32\\\\include\\\\c++"]

    dir = Dir::pwd.chomp
    expected = ["#{dir}/tested_include/mockgenSample1.hpp", "#{dir}/tested_include/mockgenSample2.hpp"]
    args = ["mock", "", "", "", "", "", "", "", ""]
    target = MockGenLauncher.new(args)
    actual = target.selectNonInternalIsystemHeaders(logStr, argIsystemPaths)
    assert_equal(expected, actual)
  end

  def test_quotePathPath
    commonArgs = [Mockgen::Constants::ARGUMENT_MODE_STUB,
                  "input.hpp", "converted.hpp", "linker_log.txt", "class.hpp", "typeSwapper.hpp",
                  "varSwapper.hpp", "swapper.hpp", "swapper.hpp",
                  "-cc1", "-ast-print", "-fblocks", "-fgnu-keywords", "-x", "c++"]
    cygwinPath = ["/usr/include", "/usr/include/w32api", "/usr/lib/gcc/x86_64-pc-cygwin/4.9.3/include"]
    expectedCommon = "-cc1 -ast-print -fblocks -fgnu-keywords -x c++ ".freeze

    args = [commonArgs, cygwinPath.map{ |w| ["-cxx-isystem", w] }].flatten
    target = MockGenLauncher.new(args)

    expected = expectedCommon.dup
    expected += '-cxx-isystem "/usr/include" -cxx-isystem "/usr/include/w32api" '
    expected += '-cxx-isystem "/usr/lib/gcc/x86_64-pc-cygwin/4.9.3/include"'
    assert_equal(expected, target.instance_variable_get(:@clangArgs))

    mingwInclude = "mingw-w64/x86_64-4.9.2-posix-seh-rt_v3-rev1/mingw64/x86_64-w64-mingw32/include"
    mingwPath = ["-cxx-isystem", "C:/Program", "Files/#{mingwInclude}",
                 "-cxx-isystem", "C:/Program", "Files/#{mingwInclude}/c++",
                 "-cxx-isystem", "C:/Program", "Files/#{mingwInclude}/c++/x86_64-w64-mingw32",
                 "-DOPTION"]
    args = [commonArgs, mingwPath].flatten.map { |w| w.tr("/", "\\") }
    target = MockGenLauncher.new(args)

    expected = expectedCommon
    expected += '-cxx-isystem "C:/Program Files/' + mingwInclude + '" '
    expected += '-cxx-isystem "C:/Program Files/' + mingwInclude + '/c++" '
    expected += '-cxx-isystem "C:/Program Files/' + mingwInclude + '/c++/x86_64-w64-mingw32"'
    expected += " -DOPTION"
    assert_equal(expected.tr("/", "\\"), target.instance_variable_get(:@clangArgs))

    assert_false(target.instance_variable_get(:@defaultNoForwardingToMock))
  end

  def getOptionSet
    ["input.hpp", "converted.hpp", "linker_log.txt", "class.hpp", "typeSwapper.hpp",
     "varSwapper.hpp", "swapper.hpp", "swapper.hpp",
     "-cc1", "-ast-print", "-fblocks", "-fgnu-keywords", "-x", "c++"]
  end

  def test_defaultNoForwardingToMock
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB,
            Mockgen::Constants::ARGUMENT_NO_FORWARDING_TO_MOCK]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_true(target.instance_variable_get(:@defaultNoForwardingToMock))
  end

  def test_parseFilterFunction
    filterSet = ["Utility", '"Utility"', "write_.*"]
    0.upto(filterSet.size) do |i|
      expected = ((i > 0) ? filterSet[0..(i-1)] : []).map { |word| word.tr('"','') }
      filterArgs = expected.map { |filter| [Mockgen::Constants::ARGUMENT_FUNCTION_NAME_FILTER, filter] }.flatten

      args = [Mockgen::Constants::ARGUMENT_MODE_STUB]
      args.concat(filterArgs)
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_equal(expected, target.instance_variable_get(:@functionNameFilterSet))
    end
  end

  def test_parseFilterOutClass
    filterSet = ["Impl", '"Impl"', "impl_.*"]
    0.upto(filterSet.size) do |i|
      expected = ((i > 0) ? filterSet[0..(i-1)] : []).map { |word| word.tr('"','') }
      filterArgs = expected.map { |filter| [Mockgen::Constants::ARGUMENT_CLASS_NAME_FILTER_OUT, filter] }.flatten

      args = [Mockgen::Constants::ARGUMENT_MODE_MOCK]
      args.concat(filterArgs)
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_equal(expected, target.instance_variable_get(:@classNameFilterOutSet))
    end
  end

  def test_parseExcludeClassName
    filterSet = ["Detail", '\'Detail\'', "Detail_.*"]
    0.upto(filterSet.size) do |i|
      expected = ((i > 0) ? filterSet[0..(i-1)] : []).map { |word| word.tr('\'','') }
      filterArgs = expected.map { |filter| [Mockgen::Constants::ARGUMENT_EXCLUDE_CLASS_NAME, filter] }.flatten

      args = [Mockgen::Constants::ARGUMENT_MODE_MOCK]
      args.concat(filterArgs)
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_equal(expected, target.instance_variable_get(:@classNameExcludedSet))
    end
  end

  def test_parseSourceFilename
    filterSet = ["a.cpp", '"src/b.cpp"', "../../src/c.cpp"]
    0.upto(filterSet.size) do |i|
      expected = ((i > 0) ? filterSet[0..(i-1)] : []).map { |word| word.tr('"','') }
      sourceArgs = expected.map { |filter| [Mockgen::Constants::ARGUMENT_SOURCE_FILENAME_FILTER, filter] }.flatten

      args = [Mockgen::Constants::ARGUMENT_MODE_STUB]
      args.concat(sourceArgs)
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_equal(expected, target.instance_variable_get(:@sourceFilenameSet))
    end
  end

  def test_parseTestedFilenameGlobSet
    globSet = ['"src/*.c"', 'detail/*.cpp']
    0.upto(globSet.size) do |i|
      expected = ((i > 0) ? globSet[0..(i-1)] : []).map { |word| word.tr('"','') }
      globArgs = expected.map { |filter| [Mockgen::Constants::ARGUMENT_TESTED_FILENAME_GLOB, filter] }.flatten

      args = [Mockgen::Constants::ARGUMENT_MODE_MOCK]
      args.concat(globArgs)
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_equal(expected, target.instance_variable_get(:@testedFilenameGlobSet))
    end
  end

  data(
    '-find' => [Mockgen::Constants::ARGUMENT_FIND_STATEMENT_FILTER, :@findStatementFilterSet],
    '-classname' => [Mockgen::Constants::ARGUMENT_EXPLICIT_CLASS_NAME, :@explicitClassNameSet])
  def test_parseFindStatementFilterSet(data)
    opt, var = data
    filterSet = ["A-Z", '"My.*"', "Common::Utility::.*"]
    0.upto(filterSet.size) do |i|
      expected = ((i > 0) ? filterSet[0..(i-1)] : []).map { |word| word.tr('"','') }
      filterArgs = expected.map { |filter| [opt, filter] }.flatten

      args = [Mockgen::Constants::ARGUMENT_MODE_MOCK]
      args.concat(filterArgs)
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_equal(expected, target.instance_variable_get(var))
    end
  end

  data(
    'default' => Mockgen::Constants::ARGUMENT_OUT_HEADER_FILENAME,
    'do not split' => "-outheaderfile",
    'split' => "-out-header-file")
  def test_parseOutHeaderFilename(data)
    expected = "outFile.hpp"
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB]
    args.concat([data, expected])
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_equal(expected, target.instance_variable_get(:@outHeaderFilename))
  end

  def test_parseSystemPath
    path1 = "/opt/gcc5"
    path2 = "/opt/gcc6"
    pathSet = [path1, path2]
    expected = Mockgen::Constants::CLANG_SYSTEM_HEADER_DEFAULT_SET.dup.concat(pathSet)

    args = [Mockgen::Constants::ARGUMENT_MODE_STUB]
    pathSet.each { |path| args.concat([Mockgen::Constants::ARGUMENT_SYSTEM_PATH, path]) }
    args.concat(getOptionSet())

    target = MockGenLauncher.new(args)
    assert_equal(expected, target.instance_variable_get(:@systemHeaderSet))
  end

  data(
    'default' => Mockgen::Constants::ARGUMENT_CHECK_INTERNAL_SYSTEM_PATH,
    'do not split' => "-checkinternalsystempath",
    'split' => "-check-internal-system-path",
    'leading hyphens 2' => "--check-internal-system-path",
    'leading hyphens 3' => "---check-internal-system-path")
  def test_checkInternalSystemPathPass(data)
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, data]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_true(target.instance_variable_get(:@checkInternalSystemPath))
  end

  def test_checkInternalSystemPathFail
    # Without a leading hyphen
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, "check-internal-system-path"]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_false(target.instance_variable_get(:@checkInternalSystemPath))
  end

  def test_checkFillVtable
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_FILL_VTABLE]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_true(target.instance_variable_get(:@fillVtable))
  end

  def test_checkNoOverloading
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_NO_OVERLOADING]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_true(target.instance_variable_get(:@noOverloading))
  end

  def test_checkUpdateChangesOnly1
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_UPDATE_CHANGES_ONLY]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_true(target.instance_variable_get(:@updateChangesOnly))
    assert_equal(1, target.instance_variable_get(:@numberOfClassInFile))
  end

  def test_checkUpdateChangesOnly2
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_UPDATE_CHANGES_ONLY,
            Mockgen::Constants::ARGUMENT_SPLIT_FILES_FILTER, "23"]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)

    assert_true(target.instance_variable_get(:@updateChangesOnly))
    assert_equal(1, target.instance_variable_get(:@numberOfClassInFile))
  end

  data(
    'simple' => Mockgen::Constants::ARGUMENT_IGNORE_INTERNAL_ERROR,
    'multi' => Mockgen::Constants::ARGUMENT_IGNORE_INTERNAL_ERRORS)
  def test_ignoreInternalErrors(data)
    assert_false(MockgenFeatures.ignoreAllExceptions)
    MockgenFeatures.ignoreAllExceptions = false

    caught = false
    begin
      args = [Mockgen::Constants::ARGUMENT_MODE_STUB, data]
      args.concat(getOptionSet())
      target = MockGenLauncher.new(args)
      assert_true(MockgenFeatures.ignoreAllExceptions)
    rescue => e
      caught = true
      raise e
    ensure
      MockgenFeatures.ignoreAllExceptions = false
    end

    assert_false(caught)
  end

  data(
    'simple' => Mockgen::Constants::ARGUMENT_DISCARD_NAMESPACE,
    'multi' => Mockgen::Constants::ARGUMENT_DISCARD_NAMESPACES)
  def test_discardNamespaces(data)
    value = "SystemInternal"
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, data, value]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_equal(value, target.instance_variable_get(:@discardNamespaceValue))
  end

  def test_mockGuard
    value = "GENERATING_MOCK"
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_MOCK_GUARD_NAME, value]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_equal(value, target.instance_variable_get(:@mockGuardName))
  end

  data(
    'simple' => Mockgen::Constants::ARGUMENT_NO_SWAP_INLINE_FUNCTION,
    'multi' => Mockgen::Constants::ARGUMENT_NO_SWAP_INLINE_FUNCTIONS)
  def test_noSwapInlineFunction(data)
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, data]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_true(target.instance_variable_get(:@noSwapInlineFunction))
  end

  data(
    'each' => ["1", 1],
    'bind' => ["2", 2],
    'zero invalid' => ["0", nil],
    'empty' => ["", nil],
    'not a number' => ["abc", nil])
  def test_parseSplitFiles(data)
    arg, expected = data
    args = [Mockgen::Constants::ARGUMENT_MODE_STUB, Mockgen::Constants::ARGUMENT_SPLIT_FILES_FILTER, arg]
    args.concat(getOptionSet())
    target = MockGenLauncher.new(args)
    assert_equal(expected, target.instance_variable_get(:@numberOfClassInFile))
  end

  # Test the generate and parse methods with .cpp files
end
