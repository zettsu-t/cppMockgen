#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require_relative './mockgenImpl.rb'
require "tempfile"
require 'test/unit'

# Omit the module name in this testing
include Mockgen

class TestTypeStringWithoutModifier < Test::Unit::TestCase
  def test_removeReservedWord
    Mockgen::Constants::KEYWORD_USER_DEFINED_TYPE_SET.each do |key|
      name = "ConcreteName"
      assert_equal([name], TypeStringWithoutModifier.new([key, name]).strSet)
    end
  end

  data(
    'primitive' => ["class T**", ["T", "**"]],
    'spaces' => ["class T * ", ["T", "*"]],
    'reference' => ["struct S&", ["S", "&"]])
  def test_removeReservedWordWithModifier(data)
    arg, expected = data
    assert_equal(expected, TypeStringWithoutModifier.new([arg]).strSet)
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
    assert_true(typeAliasSet.isSystemInternalSymbol(str))
  end

  data(
    'nullptr_t' => "nullptr_t",
    'nullptr' => "decltype(nullptr)")
  def test_isSystemInternalSymbolFalse(data)
    str = data
    typeAliasSet = TypeAliasSet.new
    assert_false(typeAliasSet.isSystemInternalSymbol(str))
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

    # Do nothing if childBlock1 is already a parentBlock's child
    parentBlock.connect(childBlock1)
    assert_equal([childBlock1, childBlock2], parentBlock.children)
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
    assert_nil(childBlock1.parent)
    assert_equal([childBlock2], parentBlock.children)

    parentBlock.disconnect(childBlock2)
    assert_nil(childBlock2.parent)
    assert_equal([], parentBlock.children)

    # Do nothing if unrelated blocks
    parentBlock.disconnect(grandChildBlock)
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

  def test_resolveAlias
    block = BaseBlock.new("")
    name = "uint32_t"
    assert_equal(name, block.resolveAlias(name))
  end

  def test_setTypedef
    BaseBlock.new("").setTypedef("Name")
  end

  def test_canTraverse
    assert_false(BaseBlock.new("").canTraverse)
  end

  def test_canMock
    child = BaseBlock.new("")
    assert_true(child.canMock)

    parent = BaseBlock.new("")
    parent.connect(child)
    def parent.canMock
      false
    end

    assert_false(parent.canMock)
    assert_false(child.canMock)
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

  def test_collectAliasesInBlock
    assert_nil(BaseBlock.new("Name").collectAliasesInBlock(nil))
  end

  def test_parseChildren
    assert_nil(BaseBlock.new("").parseChildren("Line"))
  end

  def test_getStringToClassFile
    assert_nil(BaseBlock.new("").getStringToClassFile)
  end

  def test_getStringToSwapperFile
    assert_nil(BaseBlock.new("").getStringToSwapperFile)
  end

  def filterByReferences(referenceSet)
    assert_true(BaseBlock.new("").filterByReferences(nil))
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
end

class TestRootBlock < Test::Unit::TestCase
  def test_canTraverse
    assert_true(RootBlock.new("").canTraverse)
  end
end

class TestNamespaceBlock < Test::Unit::TestCase
  data(
    'not namespace' => ["name {", ""],
    'unnamed namespace' => ["namespace {", ""],
    'simple namespace' => ["namespace A {", "A"],
    'nested namespace' => ["namespace A::B {", "A::B"],
    'more nested namespace' => ["namespace Ab::Cd::e {", "Ab::Cd::e"])
  def test_initializeAndGetNamespace(data)
    line, expected = data
    block = NamespaceBlock.new(line)
    assert_equal(expected, block.getNamespace)
  end

  data(
    'unnamed namespace' => "",
    'simple namespace' => "A",
    'nested namespace' => "A::B")
  def test_canTraverse(data)
    name = data
    assert_true(NamespaceBlock.new("namespace #{name} {").canTraverse)
    assert_true(NamespaceBlock.new("namespace #{name} {").canMock)
  end

  data(
    'C++ standard' => "std",
    'Boost C++ Libraries' => "boost",
    'Google Test' => "testing",
    'C++ internal 1' => "__cxx",
    'C++ internal 2' => "_CPP__")
  def test_cannotTraverse(data)
    name = data
    assert_false(NamespaceBlock.new("namespace #{name} {").canTraverse)
    assert_false(NamespaceBlock.new("namespace #{name} {").canMock)
  end

  def test_canMock
    Mockgen::Constants::NAMESPACE_SKIPPED_SET.each do |name|
      block = NamespaceBlock.new("namespace #{name} {")
      subBlock = NamespaceBlock.new("namespace NameA {")
      block.connect(subBlock)
      assert_false(block.canMock)
      assert_false(subBlock.canMock)
    end
  end
end

class TestExternCBlock < Test::Unit::TestCase
  def test_externC
    block = ExternCBlock.new('extern "C" {')
    assert_true(block.canTraverse)
    assert_equal("", block.getNamespace())
  end

  def test_canMock
    assert_false(ExternCBlock.new("").canMock)
  end
end

class TestTypedefBlock < Test::Unit::TestCase
  data(
    'direct typedef' => ["typedef unsigned int UINT32", ["unsigned", "int"], "UINT32"],
    'indirect typedef' => ["typedef uint32_t UINT32", ["uint32_t"], "UINT32"],
    'pointer' => ["typedef uint32_t *PUINT32", ["uint32_t", "*"], "PUINT32"])
  def test_initializeTypedef(data)
    line, expectedTypeSet, expectedAlias = data
    block = TypedefBlock.new(line + ";")
    assert_equal(expectedTypeSet, block.instance_variable_get(:@actualTypeSet))
    assert_equal(expectedAlias, block.instance_variable_get(:@typeAlias))
    assert_true(block.canTraverse)
  end

  def test_initializeNotTypedef
    block = TypedefBlock.new("namespace A")
    assert_false(block.canTraverse)
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
    rootBlock = BlockFactory.new.createRootBlock
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
    rootBlock = BlockFactory.new.createRootBlock
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

class TestStringOfParenthesis < Test::Unit::TestCase
  data(
    'empty' => ["", []],
    'one' => ["()", [["()", ""]]],
    'one+str1' => ["(a)", [["(a)", "a"]]],
    'one+str2' => ["( a )", [["(a)", "a"]]],
    'two' => ["()( )", [["()", ""], ["()", ""]]],
    'two+str' => ["( a ) ( b )", [["(a)", "a"], ["(b)", "b"]]],
    'nested' => ["((a))( ( ( b ) ) )", [["((a))", "(a)"], ["(((b)))", "((b))"]]],
    'out1' => ["f()", [["f", nil], ["()", ""]]],
    'out2' => ["f ()g", [["f", nil], ["()", ""], ["g", nil]]],
    'funcPtr1' => [" result(*f)(arg)", [["result", nil], ["(*f)", "*f"], ["(arg)", "arg"]]],
    'funcPtr2' => [" const T* (*func) (void) const; ",
                   [["const", nil], ["T*", nil], ["(*func)", "*func"], ["(void)", "void"], ["const;", nil]]])
  def test_splitByOuterParenthesis(data)
    line, expected = data
    assert_equal(expected, StringOfParenthesis.new(line).parse)
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
    'pointer' => ["const void* p", "const void *", "p", "const void* p"],
    'pointer to function' => ["int(*func)()", "int (*) ()", "func", "int (*func) ()"])
  def test_parseAsMemberVariable(data)
    line, expectedArgType, expectedArgName, expectedArgStr = data
    argType, argName = TypedVariable.new(line).parseAsMemberVariable
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
  end

  data(
    'empty' => ["int()()", "int () ()", "dummy1", "int (dummy1) ()"],
    'no varname' => ["int(*)()", "int (*) ()", "dummy1", "int (*dummy1) ()"],
    'varname' => ["int(f)()", "int () ()", "f", "int (f) ()"],
    'one word' => ["int(*f)()", "int (*) ()", "f", "int (*f) ()"],
    'default var' => ["int(*f)() = callback", "int (*) ()", "f", "int (*f) () = callback"],
    'multi words' => ["const void* (*f)()", "const void* (*) ()", "f", "const void* (*f) ()"])
  def test_parseArgSetFuncPtr(data)
    line, expectedArgType, expectedArgName, expectedNewArgStr = data
    argType, argName, newArgStr = TypedVariable.new(line).parseAsArgument(1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedArgName, argName)
    assert_equal(expectedNewArgStr, newArgStr)
  end

  data(
    'empty' => ["", "", "dummy1", "dummy1"],
    'ptr' => ["*", "*", "dummy1", "*dummy1"],
    'name' => ["f", "", "f", "f"],
    'ptrName1' => ["*f", "*", "f", "*f"],
    'ptrName2' => ["f**", "**", "f", "f**"])
  def test_extractFuncPtrName(data)
    line, expectedArgType, expectedName, expectedArgStr = data
    argType, name, argStr = TypedVariable.new(nil).extractFuncPtrName(line, 1)
    assert_equal(expectedArgType, argType)
    assert_equal(expectedName, name)
    assert_equal(expectedArgStr, argStr)
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
    'ConstructorNoArgs' => ["Func()", "", "", "Func",
                            "", "", "", ""],
    'ConstructorOneArg' => ["Func(int a)", "", "", "Func",
                            "int a", "int a", "int", "a"],
    'ConstructorTwoArgs' => ["Func (int a,long b)", "", "", "Func",
                             "int a,long b", "int a,long b", "int,long", "a,b"],
    'FuncTwoArgs' => ["int Func (int a,long b)", "int", "", "Func",
                      "int a,long b", "int a,long b", "int,long", "a,b"],
    'FuncTwoTypes' => ["void Func (int,long)", "void", "", "Func",
                       "int dummy1,long dummy2", "int dummy1,long dummy2", "int,long", "dummy1,dummy2"],
    'FuncTwoArgsPostfix' => ["void* Func (int a) const override", "void *", "const override",
                             "Func", "int a", "int a", "int", "a"],
    'FuncPtr1' => ["int Func (int(f)(int)) const", "int", "const", "Func",
                   "int (f) (int)", "int (f) (int)", "int () (int)", "f"],
    'FuncPtr2' => ["int Func(long a,int(*f)(int))const", "int", "const", "Func",
                   "long a,int (*f) (int)", "long a,int (*f) (int)", "long,int (*) (int)", "a,f"],
    'FuncPtr3' => ["int Func (int(*f)(int),void(*g)(void*)) const", "int", "const", "Func",
                   "int (*f) (int),void (*g) (void*)", "int (*f) (int),void (*g) (void*)", "int (*) (int),void (*) (void*)", "f,g"],
    'DefaultArgs' => ["int Func (int a=0,long b=(long)0)", "int", "", "Func",
                      "int a=0,long b=(long)0", "int a,long b", "int,long", "a,b"],
    'DefaultNullArgs1' => ["int Func (void* p=<null expr>)", "int", "", "Func",
                           "void* p=NULL", "void* p", "void *", "p"],
    'DefaultNullArgs2' => ["int Func(long a,int(*f)(int)=aFunc)const", "int", "const", "Func",
                           "long a,int (*f) (int) =aFunc", "long a,int (*f) (int)", "long,int (*) (int)", "a,f"],
  )
  def test_splitByArgSet(data)
    line, expectedPre, expectedPost, expectedFuncname, expectedSet, expectedSetWithoutDefault, expectedType, expectedName = data

    argVariableSet = ArgVariableSet.new(line)
    assert_equal(expectedPre, argVariableSet.preFuncSet)
    assert_equal(expectedPost, argVariableSet.postFuncSet)
    assert_equal(expectedFuncname, argVariableSet.funcName)
    assert_equal(expectedSet, argVariableSet.argSetStr)
    assert_equal(expectedSetWithoutDefault, argVariableSet.argSetWithoutDefault)
    assert_equal(expectedType, argVariableSet.argTypeStr)
    assert_equal(expectedName, argVariableSet.argNameStr)
  end

  data(
    'no args' => ["", "", ""],
    'void arg' => ["void", "", ""],
    'primitive' => ["int arg", "int", "arg"],
    'array1' => ["T[] array", "T[]", "array"],
    'array2' => ["T[2] array", "T[2]", "array"],
    'reference1' => ["int& arg", "int &", "arg"],
    'reference2' => ["int & arg", "int &", "arg"],
    'pointer1' => ["void* arg", "void *", "arg"],
    'pointer2' => ["void * arg", "void *", "arg"],
    'pointer double' => ["void ** arg", "void * *", "arg"],
    'pointer and const' => ["const void *  const arg", "const void * const", "arg"],
    'pointer and reference' => ["void*& arg", "void * &", "arg"],
    'multiple args' => ["int a,long b", "int,long", "a,b"],
    'mixed args' => ["int a,long b,const void* p", "int,long,const void *", "a,b,p"])
  def test_extractArgSet(data)
    line, expectedArgTypeSet, expectedArgNameSet = data
    argSetStr, argSetWithoutDefault, argTypeSet, argNameSet = ArgVariableSet.new(nil).extractArgSet(" #{line} ")

    expected = (line == "void") ? "" : line
    assert_equal(expected, argSetStr)
    assert_equal(expected, argSetWithoutDefault)
    assert_equal(expectedArgTypeSet, argTypeSet)
    assert_equal(expectedArgNameSet, argNameSet)
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

class TestExternVariableStatement < Test::Unit::TestCase
  def setup
  end

  data(
    'primitive' => ['Type a;', "Type", "a"],
    'pointer1'  => ['class Type *pA;',  "Type", "pA"],
    'pointer2'  => ['struct Type * pA;', "Type", "pA"],
    'reference1' => ['static Type &a;',  "Type", "a"],
    'reference2' => ['const Type & a;', "Type", "a"],
    'decltype' => ['decltype(A) a;', "decltype(A)", "a"])
  def test_canTraverse(data)
     line, type, var = data
     block = ExternVariableStatement.new("extern #{line}")
     assert_true(block.canTraverse)
     assert_true(block.isNonMemberInstanceOfClass?)
     assert_equal(type, block.className)
     assert_equal(var, block.getFullname)
     assert_equal("", block.arrayStr)
  end

  data(
    'class name' => 'class BigInt',
    'system variable' => 'struct sys __sys',
    'function1' => 'void Func(void);',
    'function2' => 'void Func(void) const;',
    'function3' => 'void Func(void) override;',
    'function4' => 'void Func(void) final;',
    'function5' => 'void Func(void) &;',
    'function6' => 'void Func(void) &&;')
  def test_cannotTraverseNonVariable(data)
    line = data
    block = ExternVariableStatement.new("extern #{line}")
    assert_false(block.canTraverse)
    assert_false(block.isNonMemberInstanceOfClass?)
  end

  data(
    'using' => 'using Count = int',
    'typedef' => 'typedef int Count')
  def test_cannotTraverseTypeAlias(data)
    line = data
    block = ExternVariableStatement.new("#{line}")
    assert_false(block.canTraverse)
    assert_false(block.isNonMemberInstanceOfClass?)
  end

  data(
    'blank' => ['Type a[];', "Type", "a[]", "[]"],
    'num' => ['Type a [ 1 ];', "Type", "a[ 1 ]", "[ 1 ]"],
    'pointer'  => ['class Type *pA[ size ];',  "Type", "pA[ size ]", "[ size ]"])
  def test_parseArray(data)
     line, type, var, arrayStr = data
     block = ExternVariableStatement.new("extern #{line}")
     assert_true(block.canTraverse)
     assert_true(block.isNonMemberInstanceOfClass?)
     assert_equal(type, block.className)
     assert_equal(var, block.getFullname)
     assert_equal(arrayStr, block.arrayStr)
  end

  # getFullname() is tested in the base class.
end

class TestMemberVariableStatement < Test::Unit::TestCase
  def setup
  end

  data(
    'primitive' => ['static Type a;', "Type", "a"])
  def test_canTraverse(data)
     line, type, var = data
     block = MemberVariableStatement.new("extern #{line}")
     assert_true(block.canTraverse)
     assert_false(block.isNonMemberInstanceOfClass?)
     assert_equal(type, block.className)
     assert_equal(var, block.getFullname)
  end
end

class TestChompAfterDelimiter < Test::Unit::TestCase
  data(
    'not replaced' => ["Func()", ":", "Func()", nil],
    'constructor' => ["Derived() : Base()", ":", "Derived()", ": Base()"],
    'assignment' => ["int a = 1", "=", "int a", "= 1"],
    'mixed' => ["int a = (int)1", "=", "int a", "= (int)1"])
  def test_chompAfterDelimiter(data)
    line, delimiter, expected, expectedTail  = data
    splitter = ChompAfterDelimiter.new(line, delimiter)
    assert_equal(expected, splitter.str)
    assert_equal(expectedTail, splitter.tailStr)
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
    referenceClass = Struct.new(:memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refArgTypeStr, refPostFunc)
    block = BaseBlock.new("")
    assert_true(FunctionReferenceSet.new(block, reference, name, argTypeStr, postFunc).compare())
  end

  data(
    'name' => ["nameA", "", "", "nameB", "", ""],
    'pointer' => ["nameA", "int", "", "nameA", "int*", ""],
    'arguments' => ["nameA", "int", "", "nameA", "int,int", ""],
    'argument constness' => ["nameA", "char const *", "", "nameA", "char * const", ""],
    'function constness' => ["nameA", "char const *", "const", "nameA", "char * const", ""])
  def test_compareFalse(data)
    refName, refArgTypeStr, refPostFunc, name, argTypeStr, postFunc = data
    referenceClass = Struct.new(:memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refArgTypeStr, refPostFunc)
    block = BaseBlock.new("")
    assert_false(FunctionReferenceSet.new(block, reference, name, argTypeStr, postFunc).compare())
  end

  data(
    'not alias' => ["nameA", "int", "int"],
    'primitive' => ["nameA", "unsigned int", "uint32_t"],
    'pointer' => ["nameA", "unsigned int*", "puint32_t"])
  def test_compareTypeAlias(data)
    refName, refArgTypeStr, argTypeStr = data
    referenceClass = Struct.new(:memberName, :argTypeStr, :postFunc)
    reference = referenceClass.new(refName, refArgTypeStr, "")
    block = BaseBlock.new("")

    typedefBlockI = TypedefBlock.new("typedef unsigned int uint32_t;")
    typedefBlockP = TypedefBlock.new("typedef unsigned int* puint32_t;")
    block.connect(typedefBlockI)
    block.connect(typedefBlockP)
    block.collectAliases
    assert_true(FunctionReferenceSet.new(block, reference, refName, argTypeStr, "").compare())
  end

  data(
    'empty' => ["", ""],
    'primitive' => ["int", "int"],
    'a pointer to a const object' => ["const char*", "charconst*"],
    'a const pointer to an object' => ["char* const", "char*const"],
    'double pointer' => ["const char** const", "charconst**const"],
    'reference' => ["const char*&", "charconst*&"],
    'multiple args' => ["int,const char*&,T const &", "int,charconst*&,Tconst&"])
  def test_sortArgTypeStr(data)
    arg, expected = data
    block = BaseBlock.new("")
    assert_equal(expected, FunctionReferenceSet.new(block, nil, "", "", "").sortArgTypeStr(arg))
  end

  data(
    'empty' => ["", ""],
    'const' => ["const", "const"],
    'override' => ["override", ""],
    'const override' => ["const override", "const"])
  def test_postFunctionPhrase(data)
    arg, expected = data
    block = BaseBlock.new("")
    assert_equal(expected, FunctionReferenceSet.new(block, nil, "", "", "").postFunctionPhrase(arg))
  end
end

class TestConstructorBlock < Test::Unit::TestCase
  data(
    'no args' =>
    ["NameC()", "", "", "", ""],
    'one arg' =>
    ["NameC(long a)", "long a", "long", ", long a", "NameC(a), "],
    'two args' =>
    ["NameC(long a, const void* p)", "long a,const void* p", "long,const void *",
     ", long a,const void* p", "NameC(a,p), "],
    'initializer list' =>
    ["NameC(long a) : Base(a)", "long a", "long", ", long a", "NameC(a), "])
  def test_initializeAndAll(data)
    line, expectedTypedArgSet, expectedArgTypeStr, expectedArgsForBaseClass, expectedCallBase = data
    name = "NameC"

    block = ConstructorBlock.new(line, name)
    valid, typedArgSet, argTypeStr, callBase = block.parse(line, name)
    assert_true(valid)
    assert_equal(expectedTypedArgSet, typedArgSet)
    assert_equal(expectedArgTypeStr, argTypeStr)
    assert_equal(expectedCallBase, callBase)
    assert_equal(expectedArgsForBaseClass, block.getTypedArgsForBaseClass)
    assert_equal(expectedCallBase, block.getCallForBaseClassInitializer)

    decoratorName = "Dereived"
    expected = "    #{decoratorName}(#{expectedTypedArgSet}) : " +
      "#{expectedCallBase}#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    assert_equal(expected, block.makeDecoratorDef(decoratorName))
  end

  data(
    'no initializer' => "NameC(int a)",
    'with initializer list' => "NameC(int a) : Base(0), a_(a)")
  def test_removeInitializerList(data)
    line = data
    block = ConstructorBlock.new("", "NameC")
    assert_equal("NameC(int a)", block.removeInitializerList(line))
  end

  def test_makeStubDef
    block = ConstructorBlock.new("", "NameC")
    assert_equal("NameC::NameC() {}\n", block.makeStubDef())
  end

  def test_makeDefaultConstructor
    name = "NameC"
    block = ConstructorBlock.new("", name)
    decoratorName = "Dereived"
    expected = "    #{decoratorName}(void) : #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    assert_equal(expected, block.makeDefaultConstructor(decoratorName))
  end
end

class TestDestructorBlock < Test::Unit::TestCase
  data(
    'nil' => nil,
    'empty' => "~NameD()",
    'void' => "~NameD(void)",
    'space' => "~NameD(void)")
  def test_parseDestructor(data)
    line = data
    className = "NameD"
    block = DestructorBlock.new(line, className)
    assert_true(block.canTraverse)

    nsBlock = NamespaceBlock.new("namespace A")
    nsBlock.connect(block)
    assert_equal("::A::NameD::~NameD() {}\n", block.makeStubDef)

    refClass = Struct.new(:memberName)
    ref = refClass.new("~NameD")
    assert_true(block.filterByReferences(ref))
    ref = refClass.new("NameD")
    assert_false(block.filterByReferences(ref))
  end

  data(
    'constructor' => "NameD()",
    'constructor arg' => "NameD(int a)",
    'constructor space' => "NameD (int a)")
  def test_parseNotDestructor(data)
    line = data
    className = "NameD"
    block = DestructorBlock.new(line, className)
    assert_false(block.canTraverse)
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
    ["__packed struct S {", "struct S {"])
  def test_removeAttributeFromLine(data)
    phrase, expected = data
    assert_equal(expected, LineWithoutAttribute.new(phrase).str)
  end
end

class TestMemberFunctionBlock < Test::Unit::TestCase
  data(
    'void no args' =>
    ["void Func()",
     "void Func()",
     "Func()",
     false, false, "void", true, "", "Func", "", ""],
    'one word type and function post modifier' =>
    ["int Func2(long a) const override {",
     "int Func2(long a) const override",
     "Func2(long)const",
     true, false, "int", false, "a", "Func2", "long a", "const override"],
    'static and attribute' =>
    ['static int Func(long b)',
     "int Func(long b)",
     "Func(long)",
     false, true, "int", false, "b", "Func", "long b", ""],
    'multiple word type and poset modifier' =>
    ["virtual const void* Func(int a, const T* p);",
     "const void * Func(int a,const T* p)",
     "Func(int,const T *)",
     false, false, "const void *", false, "a,p", "Func", "int a,const T* p", ""],
    'inline and reference' =>
    ["inline T&* Func(T& a) const",
     "T & * Func(T& a) const",
     "Func(T &)const",
     true, false, "T & *", false, "a", "Func", "T& a", "const"])
  def test_initializeAndParse(data)
    line, decl, sig, cmf, smf, rt, rv, aset, fn, taset, pf = data

    ["", "{", " {", ";", " ;"].each do |suffix|
      block = MemberFunctionBlock.new(line + suffix)
      assert_true(block.valid)
      assert_true(block.canTraverse)
      assert_equal(cmf, block.instance_variable_get(:@constMemfunc))
      assert_equal(smf, block.instance_variable_get(:@staticMemfunc))
      assert_equal(rt, block.instance_variable_get(:@returnType))
      assert_equal(rv, block.instance_variable_get(:@returnVoid))
      assert_equal(decl, block.instance_variable_get(:@decl))
      assert_equal(aset, block.instance_variable_get(:@argSet))
      assert_equal(fn, block.instance_variable_get(:@funcName))
      assert_equal(taset, block.instance_variable_get(:@typedArgSet))
      assert_equal(sig, block.instance_variable_get(:@argSignature))
      assert_equal(pf, block.instance_variable_get(:@postFunc))
    end
  end

  data(
    'empty' => "",
    'desctructor' => "~Dtor(void)",
    'virtual desctructor' => "virtual ~Dtor(void)",
    'copy constructor' => "T& operator=(const T& rhs)",
    'operator' => "bool operator<(void)")
  def test_cannotInitializeAndParse(data)
    line = data
    ["", "{", " {", ";", " ;"].each do |suffix|
      block = MemberFunctionBlock.new(line + suffix)
      assert_false(block.valid)
      assert_false(block.canTraverse)
    end
  end

  def test_filterByReferences
    refClass = Struct.new(:classFullname, :memberName, :argTypeStr, :postFunc)
    refMatched = refClass.new("", "Func1", "int", "const")
    refUnmatched1 = refClass.new("", "Func1", "int", "")
    refUnmatched2 = refClass.new("", "Func1", "int*", "const")

    block = MemberFunctionBlock.new("void Func1(int) const")
    assert_true(block.filterByReferences(refMatched))
    assert_false(block.filterByReferences(refUnmatched1))
    assert_false(block.filterByReferences(refUnmatched2))
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
        if (left == right)
          assert_true(leftBlock.override?(rightBlock))
        else
          assert_false(leftBlock.override?(rightBlock))
        end
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

  data(
    'no args' =>
    ["void Func(void)",
     "    MOCK_METHOD0(Func,void())",
     "void Super::Func() {\n    return;\n}\n",
     "    void Func() { if (pMock_) { pMock_->Func(); return; } " +
     "Super::Func(); }\n",
     "    void Func() { if (pMock_) { pMock_->Func(); return; } " +
     "static_cast<Super*>(pActual_)->Func(); }\n"],
    'no args const' =>
    ["void Func2() const override",
     "    MOCK_CONST_METHOD0(Func2,void())",
     "void Super::Func2() const {\n    return;\n}\n",
     "    void Func2() const override { if (pMock_) { pMock_->Func2(); return; } " +
     "Super::Func2(); }\n",
     "    void Func2() const { if (pMock_) { pMock_->Func2(); return; } " +
     "static_cast<Super*>(pActual_)->Func2(); }\n"],
    'one arg return pointer' =>
    ["const void* Func(int a)",
      "    MOCK_METHOD1(Func,const void *(int a))",
      "const void * Super::Func(int a) {\n    return static_cast<const void *>(0);\n}\n",
      "    const void * Func(int a) { if (pMock_) { return pMock_->Func(a); } " +
      "return Super::Func(a); }\n",
      "    const void * Func(int a) { if (pMock_) { return pMock_->Func(a); } " +
      "return static_cast<Super*>(pActual_)->Func(a); }\n"],
    'two args return reference' =>
     ["T& Func(int a, const void* p)",
      "    MOCK_METHOD2(Func,T &(int a,const void* p))",
      "T & Super::Func(int a,const void* p) {\n    return static_cast<T>(0);\n}\n",
      "    T & Func(int a,const void* p) { if (pMock_) { return pMock_->Func(a,p); } " +
      "return Super::Func(a,p); }\n",
      "    T & Func(int a,const void* p) { if (pMock_) { return pMock_->Func(a,p); } " +
      "return static_cast<Super*>(pActual_)->Func(a,p); }\n"])
  def test_makeDefSet(data)
     line, mock, stub, decorator, forwarder = data
     className = "Super"
     block = MemberFunctionBlock.new(line + ";")

     assert_true(block.valid)
     assert_equal(mock + ";\n", block.makeMockDef(className))
     assert_equal(stub, block.makeStubDef(className))
     assert_equal(decorator, block.makeDecoratorDef(className))
     assert_equal(forwarder, block.makeForwarderDef(className))
  end

  data(
    'no args' =>
    ["static int Func(long a)",
     "    static int Func(long a) { if (pClassMock_) " +
     "{ return pClassMock_->Func(a); } return Super::Func(a); }\n"])
  def test_makeDecoratorDef(data)
     line, decorator = data
     className = "Super"
     block = MemberFunctionBlock.new(line + ";")

     assert_true(block.valid)
     assert_equal(decorator, block.makeDecoratorDef(className))
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
    'const' => "const",
    'override' => "const override")
  def test_isPureVirtual(data)
    phrase = data
    block = MemberFunctionBlock.new("")
    assert_false(block.isPureVirtual(phrase))
    ["=0", "= 0", " = 0"].each do |suffix|
      assert_true(block.isPureVirtual("#{phrase}#{suffix}"))
    end
  end

  data(
    'empty' => "",
    'pure virtual' => "=0",
    'override' => "override",
    'final' => "final")
  def test_isConstMemberFunction(data)
    phrase = data
    block = MemberFunctionBlock.new("")
    assert_false(block.isConstMemberFunction(phrase))
    assert_true(block.isConstMemberFunction("const " + phrase))
    assert_true(block.isConstMemberFunction("const #{phrase} {"))
  end

  data(
    'void' => ["void", "void", true],
    'primitive' => ["int",  "int",  false],
    'void pointer' => ["void*", "void *", false],
    'void double pointer' => ["void**", "void * *", false],
    'reference' => ["int&", "int &", false],
    'const reference1' => ["const Type&", "const Type &", false],
    'const reference2' => ["const Type &", "const Type &", false])
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
end

class TestClassBlock < Test::Unit::TestCase
  data(
    'base class' => ["class NameC", "", "NameC", "class", false, []],
    'derived class' => ["class NameC : public Base", "", "NameC", "class", false, ["Base"]],
    'derived struct' => ["struct NameS : Base", "", "NameS", "struct", true, ["Base"]])
  def test_initializeAndParse(data)
    line, th, name, tn, pub, bnSet = data
    block = ClassBlock.new(line + " {")
    assert_true(block.instance_variable_get(:@valid))
    assert_true(block.canTraverse)
    assert_true(block.isClass?)

    assert_equal(name, block.getNamespace)
    assert_equal(th, block.instance_variable_get(:@templateHeader))
    assert_equal(name, block.instance_variable_get(:@name))

    assert_equal(block.mockName, "#{name}#{Mockgen::Constants::CLASS_POSTFIX_MOCK}")
    assert_equal(block.decoratorName, "#{name}#{Mockgen::Constants::CLASS_POSTFIX_DECORATOR}")
    assert_equal(block.forwarderName, "#{name}#{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}")

    assert_equal(tn, block.instance_variable_get(:@typename))
    assert_equal(pub, block.instance_variable_get(:@pub))
    assert_equal(bnSet, block.instance_variable_get(:@baseClassNameSet))
  end

  # Template is not supported yet
  data(
    'template class' => [
      "template <typename T> class NameC : public B", "template <typename T>",
      "NameC", "class", false, ["B"]],
    'template class multi types' => [
      "template <typename T, typename S> struct StructName : private B, protected C, public D, E",
      "template <typename T, typename S>", "StructName", "struct", true, ["D", "E"]])
  def test_initializeAndParseTemplate(data)
    line, th, name, tn, pub, bnSet = data
    block = ClassBlock.new(line + " {")
    assert_false(block.canTraverse)
    assert_false(block.isClass?)
  end

  def test_parseChildrenClass
    block = ClassBlock.new("class NameC")
    assert_true(block.skippingParse)

    assert_nil(block.parseChildren("public:"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_false(block.skippingParse)

    assert_nil(block.parseChildren("protected:"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_true(block.skippingParse)

    assert_nil(block.parseChildren("public :"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_equal([], block.instance_variable_get(:@memberFunctionSet))
    assert_false(block.skippingParse)

    nonFuncLine = "typedef Count int;"
    assert_nil(block.parseChildren(nonFuncLine))

    varLine = "static Count count_;"
    assert_not_nil(block.parseChildren(varLine))
    assert_not_equal([], block.instance_variable_get(:@memberVariableSet))
    assert_false(block.canMock)

    funcLine = "void Func();"
    assert_not_nil(block.parseChildren(funcLine))
    assert_not_equal([], block.instance_variable_get(:@memberFunctionSet))
    assert_true(block.canMock)

    constructorLine = "NameC();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))

    destructorLine = "~NameC();"
    assert_not_nil(block.parseChildren(destructorLine))
    assert_not_nil(block.instance_variable_get(:@destructor))

    assert_nil(block.parseChildren("private :"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_nil(block.parseChildren(funcLine))
  end

  # A struct is treated as a class with default public access
  def test_parseChildrenStruct
    block = ClassBlock.new("struct NameS")
    assert_nil(block.parseChildren("protected:"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_nil(block.parseChildren("public:"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_false(block.canMock)

    constructorLine = "NameS();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))
    assert_true(block.canMock)

    assert_nil(block.parseChildren("private :"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_equal([], block.instance_variable_get(:@memberFunctionSet))

    funcLine = "void Func();"
    nonFuncLine = "typedef Count int;"
    assert_nil(block.parseChildren(funcLine))
    assert_equal(block.children, block.instance_variable_get(:@memberFunctionSet))

    assert_nil(block.parseChildren("public :"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_nil(block.parseChildren(nonFuncLine))
    assert_not_nil(block.parseChildren(funcLine))
  end

  def test_parseAccess
    block = ClassBlock.new("")

    [["public:", true, true],
     ["protected:", true, false],
     ["public:", true, true],
     ["private:", true, false],
     ["publicFunc", false, false],
     ["public:", true, true]].each do |line, expected, expectedPub|
      assert_equal(expected, block.parseAccess(line))
      assert_equal(expectedPub, block.instance_variable_get(:@pub))
    end
  end

  def test_filterByReferences
    classname = "NameC"
    block = ClassBlock.new("class " + classname)
    assert_nil(block.parseChildren("public :"))
    funcLineDef = "void FuncDefined();"
    childBlockD = block.parseChildren(funcLineDef)
    block.connect(childBlockD)

    funcname = "FuncNotDefined"
    funcLineNonDef = "void #{funcname}(int a) const;"
    childBlockU = block.parseChildren(funcLineNonDef)
    block.connect(childBlockU)
    assert_equal([childBlockD, childBlockU], block.children)
    assert_equal(0, block.instance_variable_get(:@undefinedFunctionSet).size)

    refClass = Struct.new(:classFullname, :memberName, :argTypeStr, :argTypeStr, :postFunc)
    refSetClass = Struct.new(:valid, :refSet)
    ref = refClass.new(classname, funcname, "", "int", "const")
    refSet = refSetClass.new(true, [ref])

    block.filterByReferences(refSet)
    assert_equal([childBlockU], block.instance_variable_get(:@undefinedFunctionSet))
    assert_equal(2, block.instance_variable_get(:@memberFunctionSet).size)
    assert_true(block.canTraverse)
  end

  def test_getFullname
    nsBlock = NamespaceBlock.new("namespace A")
    block = ClassBlock.new("")
    nsBlock.connect(block)

    block.instance_variable_set(:@name, "ClassName")
    assert_equal("::A::ClassName", block.getFullname)
    block.instance_variable_set(:@templateHeader, "template <size_t N>")
    assert_equal("template <size_t N> ::A::ClassName", block.getFullname)
  end

  def test_setBaseClass
    block = ClassBlock.new("class Derived : public A::Name, public B::Name")
    table = {"::A::Name" => "a", "::B::Name" => "b", "Name" => "x"}
    block.setBaseClass(table)
    assert_equal(["a", "b"], block.instance_variable_get(:@baseClassBlockSet))
  end

  data(
    'class' => ["class ClassName", "", "ClassName", "class", false],
    'struct' => ["struct StructName", "", "StructName", "struct", true],
    'template class' => ["template <typename T> class ClassName",
                         "template <typename T>", "ClassName", "class", false],
    'template struct' => ["template <typename T, typename S> struct StructName",
                          "template <typename T, typename S>", "StructName", "struct", true])
  def test_parseClassName(data)
    line, th, name, tn, pub= data
    block = ClassBlock.new("")
    assert_true(block.parseClassName(line))
    assert_equal(th, block.instance_variable_get(:@templateHeader))
    assert_equal(name, block.instance_variable_get(:@name))

    assert_equal(name + Mockgen::Constants::CLASS_POSTFIX_MOCK, block.instance_variable_get(:@mockName))
    assert_equal(name + Mockgen::Constants::CLASS_POSTFIX_DECORATOR, block.instance_variable_get(:@decoratorName))
    assert_equal(name + Mockgen::Constants::CLASS_POSTFIX_FORWARDER, block.instance_variable_get(:@forwarderName))

    assert_equal(tn, block.instance_variable_get(:@typename))
    assert_equal(pub, block.instance_variable_get(:@pub))
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
    'two' => " NameC ( int a, const void* p )")
  def test_isConstructor(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_true(block.isConstructor(line))
  end

  data(
    'dtor1' => "~NameC()",
    'dtor2' => "~NameC ( void ) ",
    'member' => "int NameCFunc(int a)")
  def test_isNotConstructor(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_false(block.isConstructor(line))
  end

  data(
    'no args' => 'int (*f)() {',
    'args' => 'int (*f)(int a, int b);',
    'nested args' => 'int (*f)(decltype(A) a, int b)',
    'decltype result' => '(decltype X)(*f)(decltype(A) a, int b)')
  def test_isPointerToFunction(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_true(block.isPointerToFunction(line))
  end

  data(
    'return pointer1' => 'decltype(A)* f(int a)',
    'return pointer2' => 'decltype(A) *f(int a)')
  def test_isNotPointerToFunction(data)
    line = data
    block = ClassBlock.new("class NameC")
    assert_false(block.isPointerToFunction(line))
  end

  data(
    'empty' => ["", "::A::ClassName"],
    'template' => ["template <typename T>", "::A::ClassName<T>"],
    'template multi types' => ["template <typename T, size_t N>", "::A::ClassName<T, N>"])
  def test_getTypedFullname(data)
    th, expected = data
    nsBlock = NamespaceBlock.new("namespace A")
    block = ClassBlock.new("")
    nsBlock.connect(block)

    className = "ClassName"
    block.instance_variable_set(:@name, className)
    block.instance_variable_set(:@templateHeader, th)
    assert_equal(expected, block.getTypedFullname)
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

  def addMemberFunction(block, func)
    block.instance_variable_get(:@memberFunctionSet) << func
  end

  def setMemberFunction(block, funcSet)
    funcSet.each do |func|
      block.connect(func)
    end
    block.instance_variable_get(:@memberFunctionSet).concat(funcSet)
  end

  def setMemberVariable(block, varSet)
    varSet.each do |var|
      block.connect(var)
    end
    block.instance_variable_get(:@memberVariableSet).concat(varSet)
  end

  def getClassBlockToFormat(baseName)
    block = ClassBlock.new("class Tested : public Base")
    blockBase = ClassBlock.new("class #{baseName}")
    table = { "Base" => blockBase }
    block.setBaseClass(table)

    func = MemberFunctionBlock.new("void Func()")
    funcConst = MemberFunctionBlock.new("void Func() const override")
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
    funcArg = block.parseChildren("#{name}(int a)")
    block.connect(funcVoid)
    block.connect(funcArg)
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
    varStub = MemberVariableStatement.new("static int varStub")
    varOther = MemberVariableStatement.new("static int varOther")

    setMemberFunction(block, [func, funcConst, funcOther])
    setMemberFunction(blockBase, [func, funcConst, funcOther])
    setMemberVariable(block, [varStub, varOther])
    setMemberVariable(blockBase, [varStub, varOther])
    return block, blockBase
  end

  def test_formatMockClass
    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"
    block = getClassBlockToFormat(baseName)

    actualDecl, actualDef = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
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
    assert_equal(expected, actualDef)
  end

  def test_formatMockClassWithArgs
    name = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block = getClassBlockWithConstructorArgs(name)
    actualDecl, actualDef = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    ["#{mockName}(#{decoratorName}* pDecorator)",
     "#{mockName}(#{decoratorName}& decorator)",
     "#{mockName}(#{forwarderName}* pForwarder)",
     "#{mockName}(#{forwarderName}& forwarder)",
     "#{mockName}(#{decoratorName}* pDecorator, int a)",
     "#{mockName}(#{decoratorName}& decorator, int a)",
     "#{mockName}(#{forwarderName}* pForwarder, int a)",
     "#{mockName}(#{forwarderName}& forwarder, int a)"].each do |expected|
      assert_true(actualDecl.include?(expected))
      assert_true(actualDef.include?(expected))
    end

    ["#{name}(a), pDecorator_(pDecorator), pForwarder_(0)",
     "#{name}(a), pDecorator_(&decorator), pForwarder_(0)",
     "#{name}(a), pDecorator_(0), pForwarder_(pForwarder)",
     "#{name}(a), pDecorator_(0), pForwarder_(&forwarder)"].each do |expected|
      assert_true(actualDef.include?(expected))
    end
  end

  data(
    'non const' => ["", "void Tested::FuncStub() {\n    return;\n}\n"],
    'const' => ["const", "void Tested::FuncStub() const {\n    return;\n}\n"])
  def test_formatMockClassWithStub(data)
    postFunc, expected = data
    expectedFuncStub = "Tested::~Tested() {}\n" + expected
    expectedVarStub = "int ::BaseClass::varStub;\n"
    expectedStub = expectedFuncStub + expectedVarStub

    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block, blockBase = getClassBlockWithStub(testedName, baseName)
    refClass = Struct.new(:classFullname, :memberName, :argTypeStr, :postFunc)
    refSetClass = Struct.new(:valid, :refSet)
    refFunc = refClass.new(testedName, "FuncStub", "", postFunc)
    refDestructor = refClass.new(testedName, "~#{testedName}", "", postFunc)
    refVar = refClass.new(testedName, "varStub", "", "")
    refSet = refSetClass.new(true, [refFunc, refDestructor, refVar])
    block.filterByReferences(refSet)

    actualDecl, actualDef = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
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
    assert_equal(expected, actualDef)
    assert_true(block.canTraverse)

    # Do not filter the base class
    blockBase.filterByReferences(refSet)
    actualDecl, actualDef = blockBase.formatMockClass(mockName, decoratorName, forwarderName, baseName)
    assert_true(actualDecl.include?("FuncStub"))
    assert_true(actualDecl.include?("FuncOther"))
    assert_false(actualDef.include?("FuncStub"))
    assert_false(actualDef.include?("FuncOther"))
    assert_true(blockBase.canTraverse)
  end

  def test_formatMockClassAfterFilterOutAll
    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block, blockBase = getClassBlockWithStub(testedName, baseName)
    assert_true(block.canTraverse)

    ["FuncStub", "FuncOther"].each do |funcname|
      refClass = Struct.new(:classFullname, :memberName, :argTypeStr, :postFunc)
      refSetClass = Struct.new(:valid, :refSet)
      ref = refClass.new(testedName, funcname, "", "const")
      refSet = refSetClass.new(true, [ref])
      block.filterByReferences(refSet)
    end

    actualDecl, actualDef = block.formatMockClass(mockName, decoratorName, forwarderName, testedName)
    assert_true(block.canTraverse)
    assert_true(blockBase.canTraverse)
  end

  def test_formatDecoratorClass
    baseName = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    testedName = "Tested"
    block = getClassBlockToFormat(baseName)

    actual = block.formatDecoratorClass(decoratorName, mockName, testedName)
    expected  = "class #{decoratorName} : public #{testedName} {\n"
    expected += "public:\n"
    expected += "    #{decoratorName}(void) : pMock_(0) {}\n"
    expected += "    virtual ~#{decoratorName}(void) {}\n"
    expected += "    void Func() { if (pMock_) { pMock_->Func(); return; } " +
                "#{testedName}::Func(); }\n"
    expected += "    void Func() const override { if (pMock_) { pMock_->Func(); return; } " +
                "#{baseName}::Func(); }\n"
    expected += "    void Func(int a) { if (pMock_) { pMock_->Func(a); return; } #{baseName}::Func(a); }\n"
    expected += "    int Other(long a,T* b) { if (pMock_) { return pMock_->Other(a,b); } " +
                "return #{baseName}::Other(a,b); }\n"
    expected += "    #{mockName}* pMock_;\n"
    expected += "    static Mock* pClassMock_;\n"
    expected += "};\n\n"
    assert_equal(expected, actual)
  end

  def test_formatDecoratorClassWithArgs
    name = "BaseClass"
    mockName = "Mock"
    decoratorName = "Decorator"
    forwarderName = "Forwarder"
    testedName = "Tested"

    block = getClassBlockWithConstructorArgs(name)
    actual = block.formatDecoratorClass(decoratorName, mockName, testedName)

    ["#{decoratorName}() : #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n",
     "#{decoratorName}(int a) : #{name}(a), #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    ].each do |expected|
      assert_true(actual.include?(expected))
     end
  end

  def test_formatForwarderClass
    baseName = "BaseClass"
    mockName = "Mock"
    forwarderName = "Forwarder"
    testedName = "Tested"
    block = getClassBlockToFormat(baseName)

    actual = block.formatForwarderClass(forwarderName, mockName, testedName)
    expected =  "class #{forwarderName} {\n"
    expected += "public:\n"
    expected += "    #{forwarderName}(#{testedName}* pActual) : pActual_(pActual), pMock_(0) {}\n"
    expected += "    #{forwarderName}(#{testedName}& actual) : pActual_(&actual), pMock_(0) {}\n"
    expected += "    virtual ~#{forwarderName}(void) {}\n"
    expected += "    void Func() { if (pMock_) { pMock_->Func(); return; } " +
                "static_cast<#{testedName}*>(pActual_)->Func(); }\n"
    expected += "    void Func() const { if (pMock_) { pMock_->Func(); return; } " +
                "static_cast<#{baseName}*>(pActual_)->Func(); }\n"
    expected += "    void Func(int a) { if (pMock_) { pMock_->Func(a); return; } " +
                "static_cast<#{baseName}*>(pActual_)->Func(a); }\n"
    expected += "    int Other(long a,T* b) { if (pMock_) { return pMock_->Other(a,b); } " +
                "return static_cast<#{baseName}*>(pActual_)->Other(a,b); }\n"
    expected += "    #{testedName}* pActual_;\n"
    expected += "    #{mockName}* pMock_;\n"
    expected += "};\n\n"
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

  def connectFuncNotOverriden(block, blockBase, testedMethod, funcLine)
    funcNotOverriden = MemberFunctionBlock.new(funcLine + " const")
    assert_not_equal("", block.send(testedMethod, [funcNotOverriden]))
    blockBase.connect(funcNotOverriden)
    addMemberFunction(blockBase, funcNotOverriden)
  end

  def connectFuncConstNotOverriden(blockBase)
    funcNotOverriden = MemberFunctionBlock.new("void Func(int a)")
    blockBase.connect(funcNotOverriden)
    addMemberFunction(blockBase, funcNotOverriden)
  end

  def checkCollectDef(testedMethod, lambdaCheck1, lambdaCheck2)
    block, func, funcLine = getClassBlockToCollect
    actual = block.send(testedMethod, [])
    assert_equal(1, actual.lines.count)
    assert_true(lambdaCheck1.call(actual, funcLine))
    assert_equal("", block.send(testedMethod, [func]))

    blockBase, funcBaseLine = connectFunctionBlockToCollect(block)
    actual = block.send(testedMethod, [])
    assert_equal(2, actual.lines.count)
    assert_true(lambdaCheck2.call(actual, funcBaseLine))

    connectMemberFunctionBlockToCollect(blockBase, funcLine)
    actual = block.send(testedMethod, [])
    assert_equal(2, actual.lines.count)

    connectFuncNotOverriden(block, blockBase, testedMethod, funcLine)
    actual = block.send(testedMethod, [])
    assert_equal(3, actual.lines.count)

    connectFuncConstNotOverriden(blockBase)
    actual = block.send(testedMethod, [])
    assert_equal(4, actual.lines.count)
  end

  def test_collectMockDef
    lambdaCheck1 = lambda { |actual, funcLine| actual.include?("Func") }
    lambdaCheck2 = lambda { |actual, funcBaseLine| actual.include?("Other") }
    checkCollectDef(:collectMockDef, lambdaCheck1, lambdaCheck2)
  end

  def test_collectDecoratorDef
    lambdaCheck1 = lambda { |actual, funcLine| actual.include?(funcLine) }
    lambdaCheck2 = lambda { |actual, funcBaseLine| actual.include?(funcBaseLine) }
    checkCollectDef(:collectDecoratorDef, lambdaCheck1, lambdaCheck2)
  end

  def test_collectForwarderDef
    lambdaCheck1 = lambda { |actual, funcLine| actual.include?("Func") }
    lambdaCheck2 = lambda { |actual, funcBaseLine| actual.include?("Other") }
    checkCollectDef(:collectForwarderDef, lambdaCheck1, lambdaCheck2)
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

    lambdaCheck = lambda { |n| puts n; true }
    str = block.collectFunctionDef([], :collectMockDef, lambdaCheck)
    assert_equal("    MOCK_METHOD0(FuncBase,void());\n", str)
  end

  # Test makeClassSet in testing CppFileParser
end

class TestBlockFactory < Test::Unit::TestCase
  def setup
  end

  def test_createRootBlock
    assert_true(BlockFactory.new.createRootBlock.canTraverse)
  end

  def test_createBlock
    factory = BlockFactory.new
    rootBlock = factory.createRootBlock

    line = "namespace NameSpaceA {"
    assert_equal("NameSpaceA", factory.createBlock(line, rootBlock).getNamespace)

    line = 'extern "C"'
    assert_equal("", factory.createBlock(line, rootBlock).getNamespace)

    line = "class ClassName {"
    classBlock = factory.createBlock(line, rootBlock)
    assert_true(classBlock.isClass?)

    # Define as a private member
    line = "class InnerClass {"
    innerClassBlock = factory.createBlock(line, classBlock)
    assert_false(innerClassBlock.isClass?)

    line = "struct ClassName {"
    classBlock = factory.createBlock(line, rootBlock)
    assert_true(classBlock.isClass?)

    # Define as a public member
    line = "class InnerClass {"
    innerClassBlock = factory.createBlock(line, classBlock)
    assert_true(innerClassBlock.isClass?)

    # Template is not supported yet
    line = "template <typename T> class ClassName {"
    assert_false(factory.createBlock(line, rootBlock).isClass?)

    line = "typedef unsigned int uint32_tj;"
    assert_true(factory.createBlock(line, rootBlock).canTraverse)

    line = "extern const ClassName& obj;"
    assert_true(factory.createBlock(line, rootBlock).canTraverse)

    line = "extern void Func() {"
    assert_false(factory.createBlock(line, rootBlock).canTraverse)

    line = "public:"
    block = factory.createBlock(line, classBlock)
    assert_not_nil(block)

    line = "void Func(int a)"
    block = factory.createBlock(line, classBlock)
    assert_not_nil(block)
  end

  data(
    'struct' => "struct",
    'class' => "class")
  def test_createBlockWithTypedef(data)
    typeStr = data
    factory = BlockFactory.new
    rootBlock = factory.createRootBlock

    line = "typedef #{typeStr} tagName {"
    block = factory.createBlock(line, rootBlock)
    assert_not_nil(block.instance_variable_get(:@typedefBlock))
  end
end

class TestClassMock < Mockgen::BaseBlock
  attr_reader :name, :getFullname, :children, :visited, :typeAliasSet

  def initialize(name, getFullname, children)
    @name = name
    @getFullname = getFullname
    @children = children
    @visited = false
    @typeAliasSet = TypeAliasSet.new
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
end

class TestUndefinedReference < Test::Unit::TestCase
  data(
    'toplevel' => ["undefined reference to `TopLevelClass::GetValue()'",
                   "TopLevelClass::GetValue", "TopLevelClass", "GetValue", ""],
    'destructor' => ["undefined reference to `TopLevelClass::~TopLevelClass()'",
                   "TopLevelClass::~TopLevelClass", "TopLevelClass", "~TopLevelClass", ""],
    'namespace' => ["undefined reference to `Sample1::Types::DerivedClass::FuncAdded(long, const T*) const",
                    "::Sample1::Types::DerivedClass::FuncAdded", "::Sample1::Types::DerivedClass", "FuncAdded", "long,const T*"],
    'array' => ["undefined reference to `ClassNotInstanciated::arrayMissing'",
                "ClassNotInstanciated::arrayMissing", "ClassNotInstanciated", "arrayMissing", nil])
  def test_parseUndefinedReference(data)
    line, expectedFullname, expectedClassFullname, expectedMemberName, expectedArgTypeSet = data
    ref = UndefinedReference.new(line)
    assert_not_nil(ref.fullname)
    assert_equal(expectedFullname, ref.fullname)
    assert_equal(expectedClassFullname, ref.classFullname)
    assert_equal(expectedMemberName, ref.memberName)
  end
end

CppFileParserNilArgSet = [nil, nil, nil, nil].freeze

class TestCppFileParser < Test::Unit::TestCase
  def test_parseLine
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)

    originalBlock = parser.instance_variable_get(:@block)
    parser.parseLine("comment")
    assert_equal(originalBlock, parser.instance_variable_get(:@block))

    parser.parseLine("class Base {")
    classBlock = parser.instance_variable_get(:@block)
    assert_not_equal(originalBlock, classBlock)

    parser.parseLine("public:")
    parser.parseLine("void FuncA() {")
    funcBlockA = parser.instance_variable_get(:@block)
    assert_not_equal(classBlock, funcBlockA)

    parser.parseLine("if (0) {")
    ifBlock = parser.instance_variable_get(:@block)
    assert_not_equal(funcBlockA, ifBlock)

    parser.parseLine("} else {")
    assert_equal(ifBlock, parser.instance_variable_get(:@block))

    parser.parseLine("}")
    assert_equal(funcBlockA, parser.instance_variable_get(:@block))
    parser.parseLine("}")
    assert_equal(classBlock, parser.instance_variable_get(:@block))
    assert_equal(classBlock, funcBlockA.parent)

    parser.parseLine("void FuncB();")
    assert_equal(classBlock, parser.instance_variable_get(:@block))

    parser.parseLine("};")
    assert_equal(originalBlock, parser.instance_variable_get(:@block))
    assert_equal(3, classBlock.children.size)
    assert_equal(funcBlockA, classBlock.children[1])
    assert_not_equal([], classBlock.instance_variable_get(:@memberFunctionSet))
  end

  def test_parseTypedefAndStruct
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)

    parser.parseLine("typedef class tagName {")
    block = parser.instance_variable_get(:@block)
    assert_true(block.isClass?)
    assert_not_nil(block)

    parser.parseLine("void FuncA();")
    parser.parseLine("} Name;")

    typedefBlock = block.instance_variable_get(:@typedefBlock)
    assert_not_nil(typedefBlock)
    assert_true(typedefBlock.canTraverse)
  end

  def test_eliminateUnusedBlock
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    parentBlock = BaseBlock.new("Parent")
    childBlock = BaseBlock.new("Child")
    parentBlock.connect(childBlock)
    assert_equal(parentBlock, childBlock.parent)
    assert_equal([childBlock], parentBlock.children)

    parser.eliminateUnusedBlock(parentBlock)
    parser.eliminateUnusedBlock(childBlock)
    assert_nil(childBlock.parent)
    assert_equal([], parentBlock.children)
  end

  def test_eliminateAllUnusedBlock
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    parentBlock = BaseBlock.new("Parent")
    childBlock = BaseBlock.new("Child")
    parentBlock.connect(childBlock)

    parser.eliminateAllUnusedBlock(childBlock)
    assert_not_nil(childBlock.parent)

    rootBlock = BlockFactory.new.createRootBlock
    nsBlock = NamespaceBlock.new("namespace std {")
    rootBlock.connect(nsBlock)
    nsBlock.connect(parentBlock)

    parser.eliminateAllUnusedBlock(rootBlock)
    assert_nil(nsBlock.parent)
    assert_equal([], rootBlock.children)
  end

  def test_buildClassTree
    [false, true].each do |testBuild|
      blockA = TestClassMock.new("A", "ClassA", [])
      blockB = TestClassMock.new("B", "", [])
      blockC = TestClassMock.new("C", "", [blockB, blockA])
      blockSet = [blockA, blockB, blockC]
      parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)

      if (testBuild)
        blockD = TestClassMock.new("D", "", [blockC])
        parser.instance_variable_set(:@block, blockD)
        parser.buildClassTree(nil)
      else
        classSet = parser.collectClasses([blockC], nil)
        assert_equal({ "ClassA" => blockA }, classSet)
        parser.connectClasses([blockC], classSet)
      end
      assert_true(blockSet.all?(&:visited))
    end
  end

  def test_collectClasses
    block = TestClassMock.new("Base", "A::Base", [])
    blockV = ExternVariableStatement.new("extern Base varB_;")

    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    assert_equal({"A::Base" => block}, parser.collectClasses([block, blockV], nil))
  end

  def test_connectClasses
    blockB = TestClassMock.new("Base", "A::Base", [])
    blockD = ClassBlock.new("class Derived : public A::Base {")
    classSet = { "::A::Base" => blockB, "::Derived" => blockD }

    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
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
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
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

    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    parser.collectTypedefs(rootBlock)
    assert_equal({"Counter"=>"long"}, ns.typeAliasSet.aliasSet)
    assert_equal({"StructName"=>"tagStructName"}, ec.typeAliasSet.aliasSet)
    assert_equal({"INT"=>"int", "__int"=>"int"}, subec.typeAliasSet.aliasSet)
    assert_equal({"LongCounter"=>"long long", "StructName"=>"tagStructName", "INT"=>"int"}, rootBlock.typeAliasSet.aliasSet)
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
    parser = CppFileParser.new(nsName, *CppFileParserNilArgSet)

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
    parser = CppFileParser.new(nsName, *CppFileParserNilArgSet)
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
    parser = CppFileParser.new(nsName, *CppFileParserNilArgSet)
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

  def test_doForAllBlocks
    blockA = TestClassMock.new("A", "", [])
    blockB = TestClassMock.new("B", "", [blockA])
    blockC = TestClassMock.new("C", "", [])
    blockD = TestClassMock.new("D", "", [blockC])
    blockE = TestClassMock.new("E", "", [blockD, blockB])

    log = ""
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    lambdaToBlock = lambda { |block| log = log + block.name }
    parser.doForAllBlocks([blockE], lambdaToBlock, :isClass?)
    assert_equal("EDBCA", log)
  end

  def test_collectTopLevelTypedefSet
    rootBlock = BlockFactory.new.createRootBlock
    ecBlock1 = ExternCBlock.new('extern "C" {')
    ecBlock2 = ExternCBlock.new('extern "C" {')
    rootBlock.connect(ecBlock1)
    rootBlock.connect(ecBlock2)

    ecTypedefBlock = TypedefBlock.new("typedef int64 MyInt;")
    ecBlock2.connect(ecTypedefBlock)
    ecBlock2.collectAliases

    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    assert_equal(1, parser.collectTopLevelTypedefSet(rootBlock).flatten.size)
  end

  def test_getClassFileHeader
    inputFilename = "in.hpp"
    outClassFilename = "out.hpp"
    nsName = "MyTest"
    parser = CppFileParser.new(nsName, *CppFileParserNilArgSet)
    actual = parser.getClassFileHeader(inputFilename, outClassFilename)

    assert_true(actual.include?("generated"))
    assert_true(actual.include?("#include <gmock/gmock.h>\n"))

    expected =  "#define MOCK_OF(className) ::MyTest::className###{Mockgen::Constants::CLASS_POSTFIX_MOCK}\n"
    expected += "#define DECORATOR(className) ::MyTest::className###{Mockgen::Constants::CLASS_POSTFIX_DECORATOR}\n"
    expected += "#define FORWARDER(className) ::MyTest::className###{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}\n"
    expected += "#define INSTANCE_OF(varName) ::MyTest::varName###{Mockgen::Constants::CLASS_POSTFIX_FORWARDER}\n"
    assert_true(actual.include?(expected))
  end

  def test_getSwapperHeader
    declFilename = "decl.hpp"
    swapperFilename = "swap.hpp"
    headWord = "Title"
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
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
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    expected = "// Variable declarations\n// This file is machine generated.\n\n"
    expected += "#ifndef DECL_HPP\n#define DECL_HPP\n\n"
    expected += "#include " + '"' + classFilename + '"' +"\n\n"
    assert_equal(expected, parser.getDeclHeader(classFilename, declFilename))
  end

  def test_getDefHeader
    filename = "out.hpp"
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    expected = "// Variable definitions\n// This file is machine generated.\n\n"
    expected += "#include " + '"' + filename + '"' +"\n\n"
    assert_equal(expected, parser.getDefHeader("ifilename", "ofilename", filename))
  end

  data(
    'file' => ["headerPub.hpp", "#ifndef HEADER_PUB_HPP\n#define HEADER_PUB_HPP\n\n"],
    'dir'  => ["top/sub/HeaderImpl.hpp", "#ifndef HEADER_IMPL_HPP\n#define HEADER_IMPL_HPP\n\n"])
  def test_getIncludeGuardHeader(data)
    filename, expected = data
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    assert_equal(expected, parser.getIncludeGuardHeader(filename))
  end

  def test_getIncludeGuardFooter
    assert_equal("#endif\n", CppFileParser.new("NameSpace", *CppFileParserNilArgSet).getIncludeGuardFooter)
  end

  data(
    'empty' => ["", ""],
    'lower' => ["header.hpp", "HEADER_HPP"],
    'upper' => ["HEADER.hpp", "HEADER_HPP"],
    'camel1' => ["localFileHeader.hpp", "LOCAL_FILE_HEADER_HPP"],
    'camel2' => ["LocalFileHeader.hpp", "LOCAL_FILE_HEADER_HPP"],
    'num1' => ["bin2Text.hpp", "BIN2_TEXT_HPP"],
    'num2' => ["4parseText.hpp", "4PARSE_TEXT_HPP"],
    'dir1' => ["top/sub/HeaderImpl.hpp", "HEADER_IMPL_HPP"],
    'dir2' => ["top/suB/headerImpl.hpp", "HEADER_IMPL_HPP"])
  def test_getIncludeGuardName(data)
    filename, expected = data
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
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
    parser = CppFileParser.new("NameSpace", *CppFileParserNilArgSet)
    assert_equal(expected, parser.getIncludeDirective(filename))
  end

  # Test the methods write* with .cpp files
end

class TestMockGenLauncher < Test::Unit::TestCase
  def test_quotePathPath
    commonArgs = ["input.hpp", "converted.hpp", "linker_log.txt", "class.hpp", "typeSwapper.hpp"]
    commonArgs.concat(["varSwapper.hpp", "swapper.hpp", "swapper.hpp"])
    commonArgs.concat(["-cc1", "-ast-print", "-fblocks", "-fgnu-keywords", "-x", "c++"])

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
                 "-cxx-isystem", "C:/Program", "Files/#{mingwInclude}/c++/x86_64-w64-mingw32"]
    args = [commonArgs, mingwPath].flatten.map { |w| w.tr("/", "\\") }
    target = MockGenLauncher.new(args)

    expected = expectedCommon
    expected += '-cxx-isystem "C:/Program Files/' + mingwInclude + '" '
    expected += '-cxx-isystem "C:/Program Files/' + mingwInclude + '/c++" '
    expected += '-cxx-isystem "C:/Program Files/' + mingwInclude + '/c++/x86_64-w64-mingw32"'
    assert_equal(expected.tr("/", "\\"), target.instance_variable_get(:@clangArgs))
  end

  # Test the generate and parse methods with .cpp files
end
