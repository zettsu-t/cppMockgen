#!/usr/bin/ruby
# -*- coding: utf-8 -*-

require_relative './mockgenImpl.rb'
require "tempfile"
require 'test/unit'

# Omit the module name in this testing
include Mockgen

class TestBaseBlock < Test::Unit::TestCase
  data(
    'word' => "Varname",
    'sentence' => "Type Varname")
  def test_initialize(data)
    line = data
    block = BaseBlock.new(line)
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

  def test_canTraverse
    assert_false(BaseBlock.new("").canTraverse)
  end

  def test_isClass?
    assert_false(BaseBlock.new("").isClass?)
  end

  def test_isClassVariable?
    assert_false(BaseBlock.new("").isClassVariable?)
  end

  def test_getNamespace
    assert_equal("", BaseBlock.new("Name").getNamespace)
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
    'reference2' => ['const Type & a;', "Type", "a"])
  def test_canTraverse(data)
     line, type, var = data
     block = ExternVariableStatement.new("extern #{line}")
     assert_true(block.canTraverse)
     assert_true(block.isClassVariable?)
     assert_equal(type, block.className)
     assert_equal(var, block.getFullname)
  end

  data(
    'class name' => 'class BigInt',
    'function' => 'void Func(void);')
  def test_cannotTraverse(data)
    line = data
    block = ExternVariableStatement.new("extern #{line}")
    assert_false(block.canTraverse)
    assert_false(block.isClassVariable?)
  end

  # getFullname() is tested in the base class.
end

class TestArgVariableSet < Test::Unit::TestCase
  data(
    'no args' => ["", ""],
    'void arg' => ["void", ""],
    'primitive' => ["int arg", "arg"],
    'reference1' => ["int& arg", "arg"],
    'reference2' => ["int & arg", "arg"],
    'pointer1' => ["void* arg", "arg"],
    'pointer2' => ["void * arg", "arg"],
    'pointer double' => ["void ** arg", "arg"],
    'pointer and const' => ["const void *  const arg", "arg"],
    'pointer and reference' => ["void*& arg", "arg"],
    'multiple args' => ["int a, long b", "a, b"],
    'mixed args' => ["int a, long b, const void* p", "a, b, p"])
  def test_initialize(data)
    phrase, expected = data
    assert_equal(expected, ArgVariableSet.new(phrase).str)
  end
end

class TestConstructorBlock < Test::Unit::TestCase
  data(
    'no args' =>
    ["NameC()", "", "", ""],
    'one arg' =>
    ["NameC(long a)", "long a", ", long a", "NameC(a), "],
    'two args' =>
    ["NameC(long a, const void* p)", "long a, const void* p", ", long a, const void* p", "NameC(a, p), "])
  def test_initializeAndAll(data)
    line, expectedTypedArgSet, expectedArgsForBaseClass, expectedCallBase = data
    name = "NameC"

    block = ConstructorBlock.new(line, name)
    valid, typedArgSet, callBase = block.parse(line, name)
    assert_true(valid)
    assert_equal(expectedTypedArgSet, typedArgSet)
    assert_equal(expectedCallBase, callBase)
    assert_equal(expectedArgsForBaseClass, block.getTypedArgsForBaseClass)
    assert_equal(expectedCallBase, block.getCallForBaseClassInitializer)

    decoratorName = "Dereived"
    expected = "    #{decoratorName}(#{expectedTypedArgSet}) : " +
      "#{expectedCallBase}#{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    assert_equal(expected, block.makeDecoratorDef(decoratorName))
  end

  def test_makeDefaultConstructor(decoratorName)
    name = "NameC"
    block = ConstructorBlock.new("", name)
    decoratorName = "Dereived"
    expected = "    #{decoratorName}(void) : #{Mockgen::Constants::VARNAME_INSTANCE_MOCK}(0) {}\n"
    assert_equal(expected, block.makeDefaultConstructor(decoratorName))
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
    ['__attribute__((unused)) static int Func(long b)',
     "int Func(long b)",
     "Func(long)",
     false, true, "int", false, "b", "Func", "long b", ""],
    'multiple word type and poset modifier' =>
    ["virtual const void* Func(int a, const T* p);",
     "const void * Func(int a, const T* p)",
     "Func(int,const T *)",
     false, false, "const void *", false, "a, p", "Func", "int a, const T* p", ""],
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
    'constructor' => "Ctor(void)",
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
     "    MOCK_METHOD0(Func,void(void))",
     "    void Func(void) { if (pMock_) { pMock_->Func(); return; } " +
     "Super::Func(); }\n",
     "    void Func(void) { if (pMock_) { pMock_->Func(); return; } " +
     "static_cast<Super*>(pActual_)->Func(); }\n"],
    'no args const' =>
    ["void Func2() const override",
     "    MOCK_CONST_METHOD0(Func2,void())",
     "    void Func2() const override { if (pMock_) { pMock_->Func2(); return; } " +
     "Super::Func2(); }\n",
     "    void Func2() const { if (pMock_) { pMock_->Func2(); return; } " +
     "static_cast<Super*>(pActual_)->Func2(); }\n"],
    'one arg return pointer' =>
    ["const void* Func(int a)",
      "    MOCK_METHOD1(Func,const void *(int a))",
      "    const void * Func(int a) { if (pMock_) { return pMock_->Func(a); } " +
      "return Super::Func(a); }\n",
      "    const void * Func(int a) { if (pMock_) { return pMock_->Func(a); } " +
      "return static_cast<Super*>(pActual_)->Func(a); }\n"],
    'two args return reference' =>
     ["T& Func(int a, const void* p)",
      "    MOCK_METHOD2(Func,T &(int a, const void* p))",
      "    T & Func(int a, const void* p) { if (pMock_) { return pMock_->Func(a, p); } " +
      "return Super::Func(a, p); }\n",
      "    T & Func(int a, const void* p) { if (pMock_) { return pMock_->Func(a, p); } " +
      "return static_cast<Super*>(pActual_)->Func(a, p); }\n"])
  def test_makeDefSet(data)
     line, mock, decorator, forwarder = data
     className = "Super"
     block = MemberFunctionBlock.new(line + ";")

     assert_true(block.valid)
     assert_equal(mock + ";\n", block.makeMockDef(className))
     assert_equal(decorator, block.makeDecoratorDef(className))
     assert_equal(forwarder, block.makeForwarderDef(className))
  end

  data(
    'no args' =>
    ["__attribute__(unused) static int Func(long a)",
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
    'empty' => "__attribute__(()) int",
    'no value1' => "__attribute__((packed_)) int",
    'no value2' => "__attribute__(( packed_ )) int",
    'no value3' => "__attribute__  (( packed_ )) int",
    'value1' => "__attribute__((align(16))) int",
    'value2' => "__attribute__(( align(16) )) int",
    'value3' => "__attribute__(( align ( 16 ) )) int")
  def test_removeAttribute(data)
    phrase = data
    block = MemberFunctionBlock.new("")
    assert_equal("int", block.removeAttribute(phrase))
  end

  data(
    'empty' => "",
    'not contain' => "static",
    'contain' => "attributeA")
  def test_removeAttributeNoChanging(data)
    phrase = data
    block = MemberFunctionBlock.new("")
    assert_equal(phrase, block.removeAttribute(phrase))
  end

  data(
    'prefix' =>
    ["__attribute__((unused)) static int Func2(void)", "static int Func2(void)"],
    'postfix' =>
    ["static int Func2(void) __attribute__((unused));", "static int Func2(void) ;"],
    'var' =>
    ["int var __attribute__(( align ( 32 ) ) );", "int var ;"])
  def test_removeAttributeFromLine(data)
    line, expected = data
    block = MemberFunctionBlock.new("")
    assert_equal(expected,block. removeAttribute(line))
  end

  data(
    'no args void' =>
    ["void Func()", "void", "Func", "", ""],
    'return type' =>
    ["int Func2(void)", "int", "Func2", "void", ""],
    'static' =>
    ["static int Func2(void)", "static int", "Func2", "void", ""],
    'attribute static' =>
    ["__attribute__((unused)) static int Func2(void)", "static int", "Func2", "void", ""],
    'inline' =>
    ["inline void Func(int a)", "inline void", "Func", "int a", ""],
    'virtual and pointer' =>
    ["virtual void* Func(int a, const T* p)", "virtual void*", "Func", "int a, const T* p", ""],
    'pointer **' =>
    ["virtual void **Func(int a, const T* p)", "virtual void**", "Func", "int a, const T* p", ""],
    'override' =>
    ["int Func(long a) const override", "int", "Func", "long a", "const override"],
    'type which includes reserved word static' =>
    ["staticType Func()", "staticType", "Func", "", ""],
    'function name which includes reserved word static' =>
    ["int staticA(int a)", "int", "staticA", "int a", ""],
    'function name which includes reserved word operator' =>
    ["int operatorB(int a)", "int", "operatorB", "int a", ""])
  def test_splitLineAccepted(data)
    line, expectedPreFunc, expectedFuncName, expectedTypedArgSet, expectedPostFunc = data
    block = MemberFunctionBlock.new("")

    noAttr = block.removeAttribute(line)
    preFunc, funcName, typedArgSet, postFunc = block.splitLine(noAttr)
    assert_equal(expectedPreFunc, preFunc)
    assert_equal(expectedFuncName, funcName)
    assert_equal(expectedTypedArgSet, typedArgSet)
    assert_equal(expectedPostFunc, postFunc)
  end

  data(
    'empty' => "",
    'constructor' => "Ctor(void)",
    'desctructor' => "~Dtor(void)",
    'virtual desctructor' => "virtual ~Dtor(void)",
    'copy constructor' => "T& operator=(const T& rhs)",
    'operator' => "bool operator<(void)")
  def test_splitLineRejected(data)
    line = data
    block = MemberFunctionBlock.new("")
    assert_nil(block.splitLine(line))
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
    'no args' => ["", ""],
    'void arg' => ["void", ""],
    'primitive' => ["int arg", "int"],
    'reference1' => ["int& arg", "int &"],
    'reference2' => ["int & arg", "int &"],
    'pointer1' => ["void* arg", "void *"],
    'pointer2' => ["void * arg", "void *"],
    'pointer double' => ["void ** arg", "void * *"],
    'pointer and const' => ["const void *  const arg", "const void * const"],
    'pointer and reference' => ["void*& arg", "void * &"],
    'multiple args' => ["int a, long b", "int,long"],
    'mixed args' => ["int a, long b, const void* p", "int,long,const void *"])
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
    'derived struct' => ["struct NameS : Base", "", "NameS", "struct", true, ["Base"]],
    'template class' => [
      "template <typename T> class NameC : public B", "template <typename T>",
      "NameC", "class", false, ["B"]],
    'template class multi types' => [
      "template <typename T, typename S> struct StructName : private B, protected C, public D, E",
      "template <typename T, typename S>", "StructName", "struct", true, ["D", "E"]])
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

  def test_parseChildrenClass
    block = ClassBlock.new("class NameC")
    assert_nil(block.parseChildren("public:"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_nil(block.parseChildren("protected:"))
    assert_equal(false, block.instance_variable_get(:@pub))
    assert_nil(block.parseChildren("public :"))
    assert_equal(true, block.instance_variable_get(:@pub))
    assert_equal([], block.instance_variable_get(:@memberFunctionSet))

    nonFuncLine = "typedef Count int;"
    funcLine = "void Func();"
    assert_nil(block.parseChildren(nonFuncLine))
    assert_not_nil(block.parseChildren(funcLine))
    assert_not_equal([], block.instance_variable_get(:@memberFunctionSet))

    constructorLine = "NameC();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))

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

    constructorLine = "NameS();"
    assert_not_nil(block.parseChildren(constructorLine))
    assert_not_equal([], block.instance_variable_get(:@constructorSet))

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
    expected += "    MOCK_METHOD2(Other,int(long a, T* b));\n"
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
    expected += "    int Other(long a, T* b) { if (pMock_) { return pMock_->Other(a, b); } " +
                "return #{baseName}::Other(a, b); }\n"
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
    expected += "    int Other(long a, T* b) { if (pMock_) { return pMock_->Other(a, b); } " +
                "return static_cast<#{baseName}*>(pActual_)->Other(a, b); }\n"
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

    line = "class ClassName {"
    classBlock = factory.createBlock(line, rootBlock)
    assert_true(classBlock.isClass?)

    line = "template <typename T> class ClassName {"
    assert_true(factory.createBlock(line, rootBlock).isClass?)

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
end

class TestClassMock
  attr_reader :name, :getFullname, :children, :visited

  def initialize(name, getFullname, children)
    @name = name
    @getFullname = getFullname
    @children = children
    @visited = false
  end

  def isClass?
    true
  end

  def isClassVariable?
    false
  end

  def setBaseClass(classSet)
    @visited = true if classSet
  end
end

class TestCppFileParser < Test::Unit::TestCase
  def setup
  end

  def test_parseLine
    parser = CppFileParser.new("NameSpace", nil, nil)

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

  def test_eliminateUnusedBlock
    parentBlock = BaseBlock.new("Parent")
    childBlock = BaseBlock.new("Child")
    parentBlock.connect(childBlock)
    assert_equal(parentBlock, childBlock.parent)
    assert_equal([childBlock], parentBlock.children)

    parser = CppFileParser.new("NameSpace", nil, nil)
    parser.eliminateUnusedBlock(parentBlock, childBlock)
    assert_nil(childBlock.parent)
    assert_equal([], parentBlock.children)

    parentBlock.connect(childBlock)
    assert_equal(parentBlock, childBlock.parent)
    assert_equal([childBlock], parentBlock.children)
    def childBlock.canTraverse
      true
    end

    parser.eliminateUnusedBlock(parentBlock, childBlock)
    assert_equal(parentBlock, childBlock.parent)
    assert_equal([childBlock], parentBlock.children)
  end

  def test_buildClassTree
    [false, true].each do |testBuild|
      blockA = TestClassMock.new("A", "ClassA", [])
      blockB = TestClassMock.new("B", "", [])
      blockC = TestClassMock.new("C", "", [blockB, blockA])
      blockSet = [blockA, blockB, blockC]
      parser = CppFileParser.new("NameSpace", nil, nil)

      if (testBuild)
        blockD = TestClassMock.new("D", "", [blockC])
        parser.instance_variable_set(:@block, blockD)
        parser.buildClassTree
      else
        classSet = parser.collectClasses([blockC])
        assert_equal({ "ClassA" => blockA }, classSet)
        parser.connectClasses([blockC], classSet)
      end
      assert_true(blockSet.all?(&:visited))
    end
  end


  def test_collectClasses
    block = TestClassMock.new("Base", "A::Base", [])
    blockV = ExternVariableStatement.new("extern Base varB_;")

    parser = CppFileParser.new("NameSpace", nil, nil)
    assert_equal({"A::Base" => block}, parser.collectClasses([block, blockV]))
  end

  def test_connectClasses
    blockB = TestClassMock.new("Base", "A::Base", [])
    blockD = ClassBlock.new("class Derived : public A::Base {")
    classSet = { "::A::Base" => blockB, "::Derived" => blockD }

    parser = CppFileParser.new("NameSpace", nil, nil)
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
    parser = CppFileParser.new("NameSpace", nil, nil)
    expected = [["varB_", "::A::varB_", "Base"],["varD_", "::A::varD_", "Derived"]]
    assert_equal(expected, parser.collectVariables(blockSet))
  end

  def test_makeTypeVarAliases
    classNameB = "Base"
    classNameD = "Derived"
    blockB = ClassBlock.new("class #{classNameB}")
    blockD = ClassBlock.new("class #{classNameD}")
    ns = NamespaceBlock.new("namespace A")
    ns.connect(blockD)

    nsName = "TestNameSpace"
    parser = CppFileParser.new(nsName, nil, nil)

    varNameB = "varB_"
    varNameD = "varD_"
    varSet = [[varNameB, "::#{varNameB}", classNameB], [varNameD, "::A::#{varNameD}", classNameD]]
    classSet = {classNameB => blockB,  classNameD => blockD }
    tsStr, vsStr, declStr, defStr = parser.makeTypeVarAliases(varSet, classSet)

    postfix = Mockgen::Constants::CLASS_POSTFIX_DECORATOR
    expected = "#define #{classNameB} ::#{nsName}::#{classNameB}#{postfix}\n"
    expected += "#define #{classNameD} ::#{nsName}::#{classNameD}#{postfix}\n"
    assert_equal(expected, tsStr)

    postfix = Mockgen::Constants::CLASS_POSTFIX_FORWARDER
    expected =  "using ::#{varNameB};\n#define #{varNameB} ::#{nsName}::#{varNameB}#{postfix}\n"
    expected += "using ::A::#{varNameD};\n#define #{varNameD} ::#{nsName}::#{varNameD}#{postfix}\n"
    assert_equal(expected, vsStr)

    expected =  "extern #{classNameB}#{postfix} #{varNameB}#{postfix};\n"
    expected += "extern #{classNameD}#{postfix} #{varNameD}#{postfix};\n"
    assert_equal(expected, declStr)

    expected =  "#{classNameB}#{postfix} #{varNameB}#{postfix}(::#{varNameB});\n"
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
    parser = CppFileParser.new(nsName, nil, nil)
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

  def test_doForAllBlocks
    blockA = TestClassMock.new("A", "", [])
    blockB = TestClassMock.new("B", "", [blockA])
    blockC = TestClassMock.new("C", "", [])
    blockD = TestClassMock.new("D", "", [blockC])
    blockE = TestClassMock.new("E", "", [blockD, blockB])

    log = ""
    parser = CppFileParser.new("NameSpace", nil, nil)
    lambdaToBlock = lambda { |block| log = log + block.name }
    parser.doForAllBlocks([blockE], lambdaToBlock, :isClass?)
    assert_equal("EDBCA", log)
  end

  def test_getClassFileHeader
    inputFilename = "in.hpp"
    outClassFilename = "out.hpp"
    nsName = "MyTest"
    parser = CppFileParser.new(nsName, nil, nil)
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
    parser = CppFileParser.new("NameSpace", nil, nil)
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
    parser = CppFileParser.new("NameSpace", nil, nil)
    expected = "// Variable declarations\n// This file is machine generated.\n\n"
    expected += "#ifndef DECL_HPP\n#define DECL_HPP\n\n"
    expected += "#include " + '"' + classFilename + '"' +"\n\n"
    assert_equal(expected, parser.getDeclHeader(classFilename, declFilename))
  end

  def test_getDefHeader
    filename = "out.hpp"
    parser = CppFileParser.new("NameSpace", nil, nil)
    expected = "// Variable definitions\n// This file is machine generated.\n\n"
    expected += "#include " + '"' + filename + '"' +"\n\n"
    assert_equal(expected, parser.getDefHeader("ifilename", "ofilename", filename))
  end

  data(
    'file' => ["headerPub.hpp", "#ifndef HEADER_PUB_HPP\n#define HEADER_PUB_HPP\n\n"],
    'dir'  => ["top/sub/HeaderImpl.hpp", "#ifndef HEADER_IMPL_HPP\n#define HEADER_IMPL_HPP\n\n"])
  def test_getIncludeGuardHeader(data)
    filename, expected = data
    parser = CppFileParser.new("NameSpace", nil, nil)
    assert_equal(expected, parser.getIncludeGuardHeader(filename))
  end

  def test_getIncludeGuardFooter
    assert_equal("#endif\n", CppFileParser.new("NameSpace", nil, nil).getIncludeGuardFooter)
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
    parser = CppFileParser.new("NameSpace", nil, nil)
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
    parser = CppFileParser.new("NameSpace", nil, nil)
    assert_equal(expected, parser.getIncludeDirective(filename))
  end

  # Test the methods write* with .cpp files
end

class TestMockGenLauncher < Test::Unit::TestCase
  def setup
  end

  def test_setFiles
    inFile = "input.hpp"
    convetedFile = "converted.hpp"
    classFile = "class.hpp"
    typeSwapperFile = "typeSwapper.hpp"
    varSwapperFile = "varSwapper.hpp"
    outDeclFile = "swapper.hpp"
    outDefFile = "swapper.hpp"

    args = [inFile, convetedFile, classFile, typeSwapperFile, varSwapperFile, outDeclFile, outDefFile]
    target = MockGenLauncher.new(args)
    assert_equal(inFile, target.instance_variable_get(:@inputFilename))
    assert_equal(convetedFile, target.instance_variable_get(:@convertedFilename))
    assert_equal(classFile, target.instance_variable_get(:@outClassFilename))
    assert_equal(typeSwapperFile, target.instance_variable_get(:@outTypeSwapperFilename))
    assert_equal(varSwapperFile, target.instance_variable_get(:@outVarSwapperFilename))
    assert_equal(outDeclFile, target.instance_variable_get(:@outDeclFilename))
    assert_equal(outDefFile, target.instance_variable_get(:@outDefFilename))
  end

  def test_quotePathPath
    commonArgs = ["input.hpp", "converted.hpp", "class.hpp", "typeSwapper.hpp"]
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
