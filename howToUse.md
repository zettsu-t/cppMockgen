# CppMockGen : C++ mock generator for Google Mock

## Summary

CppMockGen generates codes to test C++ codes. It makes
* Mock classes to describe expectations with Google Mock
* Stub functions to link subset of tested source files

This is applicable to C code, which has the top-level namespace,
no other namespaces and no classes.

### Platform

CppMockGen is a set of ruby scripts and launches the clang front-end
internally. This is my environment to develop it.

* Windows 10 Anniversary Update 64bit Edition (Version 1607, OS build 14393.10)
* Cygwin 64bit version (2.5.2), MinGW-w64 or Bash on Ubuntu on Windows
* Google Test / Mock (1.7.0)
* LLVM, gcc and ruby

I tested CppMockGen with the following versions of tools.

|Package|Cygwin 64bit|MinGW-w64|Bash on Ubuntu on Windows|
|:------|:------|:------|:------|
|LLVM + clang|3.8.1|3.8.1|3.4|
|gcc|5.4.0|4.9.2|4.8.4|
|Ruby|2.2.5p319|2.3.1p112(ActiveScriptRuby)|1.9.3p484|

CppMockGen is executable on Linux but it needs changes on its Makefile
to test CppMockGen with GNU _make_ on Linux. Note that clang++
requires include paths to find g++ header files.

CppMockGen needs command line tools such as _echo_, _ls_, _grep_,
_wc_, _ctags_ in Cygwin or Linux. To run and test CppMockGen on MinGW,
add Cygwin /usr/bin directory to _PATH_ environment variable.

Attached _Makefile_ assumes Google Test / Mock are installed at $HOME/googletest.

### Sample codes

To test CppMockGen, run

```bash
make runthrough
```

from a console. It
* Generates codes from tested source codes
* Links generated, tested and tester codes. It fails intentionally due
  to missing functions.
* Generates stub codes and links them to bin/mockgenSample.exe
  successfully
* Executes mockgenSample.exe and reports its results described with
  Google Test / Google Mock
* Runs tests for script/*.rb and reports its results

and you can find the generated code in generated/ subdirectory.

* converted_*.hpp that the clang front-end generates
* link_error.log that _ld_ (linker) generates
* other *.hpp files that script/mockgen.rb generates

Other subdirectories contain
* script/ CppMockGen implementation
* utility/ C++ helper codes to write tests with the generated code
* tested_include/ and tested_src/ C++ tested code sample
* tester_include/ and tester_src/ C++ tester code (test descriptions) sample

## Generate codes

CppMockGen generates codes that tested and tester codes include or link
* Mock class : Google Mock style class definition. It can attach to a
  forwarder or a decorator instance.
* Forwarder class : class definition to switch invocations to a mock
  or a tested (original) function / class
* Decorator class : class definition to overwrite definitions of a
  tested (original) class to switch invocations to a mock or the
  tested class
* Swapper macro : macro replacing type and variable names with the
  generated code's
* Stub function and variable : definition that is defined in a source
  file but not linked

CppMockGen does not generate codes for system headers, standard C/C++
libraries, Boost C++ libraries, and compiler internal classes and
functions.

All codes quoted below are in the subdirectories of CppMockGen.

### Mock

CppMockGen generates mock classes that contain _MOCK_METHODn_
methods. Here is a generated class to mock a tested class
`DerivedClass`.

```cpp
class DerivedClass_Mock : public ::Sample1::Types::DerivedClass {
public:
    DerivedClass_Mock(DerivedClass_Decorator* pDecorator);
    DerivedClass_Mock(DerivedClass_Decorator& decorator);
    DerivedClass_Mock(DerivedClass_Forwarder* pForwarder);
    DerivedClass_Mock(DerivedClass_Forwarder& forwarder);
    MOCK_METHOD0(Func,void());
    ...
};
```

The constructors take an instance of the decorator or the forwarder to
detach them from a mock instance (detailed later).

_MOCK_METHOD_ is a keyword of Google Mock and used to describe
expectations and actions for testing (see the Google Mock
documentation).

The mock class inherits its tested class to find symbols which are
defined in the tested class and used without qualification.

CppMockGen also generates mock classes for free functions per
namespace. CppMockGen names the classes after their namespaces
concatenated with "_" and the top-level namespace (including extern
"C" functions) has the name "All".

```cpp
class All_Mock { // for the top-level namespace
public:
    All_Mock(All_Forwarder* pForwarder);
    All_Mock(All_Forwarder& forwarder);
    MOCK_METHOD1(defaultCallback,void(int a));
    ...
}

class All_Sample1_Mock { ... } // for namespce ::Sample1
```

### Forwarder

CppMockGen generates forwarder classes to determine whether tested
codes call tested (original) methods or mock (generated) methods. I
assume a situation in which the tested codes call a method in a global
variable or a free function.

```cpp
class DerivedClass_Forwarder : public ::Sample1::Types::DerivedClass {
public:
    DerivedClass_Forwarder(::Sample1::Types::DerivedClass* pActual);
    DerivedClass_Forwarder(::Sample1::Types::DerivedClass& actual);
    void Func() override { if (pMock_ && !Func_nomock_)
        { pMock_->Func(); return; }
        static_cast<::Sample1::Types::DerivedClass*>(pActual_)->Func();
    }
    ...
}
```

The constructors take an instance of the tested class to
forward. Usually, the instance is a global variable.

```cpp
extern ::Sample1::Types::DerivedClass anObject;
```

To mock the instance, we need to replace its type with the
forwarder. "varSwapper.hpp" defines a macro to do this and tested
codes can include "varSwapper.hpp" in testing (surround it with
`#ifdef TESTING ... #endif`). `MyUnittest` is a namespace for
generated codes here.

```cpp
using ::Sample1::Vars::anObject;
#define anObject ::MyUnittest::anObject_Forwarder
```

We can replace calling free functions in the same manner.

```cpp
#define defaultCallback all_Forwarder.defaultCallback
```

We want to swap invocations to `defaultCallback` but we have to avoid
replacing the definition of `defaultCallback`. Tell CppMockGen a file
which contains the definition to do this (see the _Launch the script_
section).

### Decorator

To mock a public and non-virtual method, especially a class
(non-instance) method, we replace its type to a class which can catch
its method invocation. CppMockGen generates decorator classes for this
purpose.

```cpp
class DerivedClass_Decorator : public ::Sample1::Types::DerivedClass {
public:
    DerivedClass_Decorator(void) : pMock_(0) {}
    virtual ~DerivedClass_Decorator(void) {}
    void Func() override { if (pMock_ && !Func_nomock_)
        { pMock_->Func(); return; }
        ::Sample1::Types::DerivedClass::Func();
    }
    ...
}
```

To swap the type in the tested and tester code, include
"typeSwapper*.hpp" that defines a swapper macro.

```cpp
using ::Sample1::Types::DerivedClass;
#define DerivedClass ::MyUnittest::DerivedClass_Decorator
```

### Stub

CppMockGen makes stubs from an input _ld_ log file. If the file
contains an undefined function or a variable, CppMockGen seeks its
declaration and makes its stub.

Stub functions may return uninitialized or meaningless value so it is
just used to remove link errors and its mock should always capture its
invocation.

It is useful to link a subset of tested files. Assume a test case
where it tests a free function. The function (caller) calls another
function (callee) that is defined in a different source file.
* The test case calls the caller which is defined in file A
* The caller calls the callee which is defined in file B

If the test case links the file B, it needs to compile and link files
other than file A and B that contain definitions of functions which
the callee calls. It leads that the test case needs all source files
to test the caller alone. Making stubs automatically eases this
burden.

To avoid compilation errors, stub variables take their initializer
that CppMockGen generates
* 0 for constant pointers : T* const (not pointers to const variables
  : const T*)
* First members of enums and enum classes (may not be equal to 0)
* Unspecified for other cases and implicitly defined default
  initializers are used

### Arguments of constructors for generated classes

The generated mock, forwarder and decorator class inherit a tested
code class. If the tested class is not constructive without arguments,
the generated classes pass arguments of their constructors to the
tested (base) class. Tester and tested codes set arguments of
constructors of the mocks and the decorators as the tested class.

It causes a problem to define a forwarder instance which mocks a
global variable because a signature of a constructor does not tell
what is appropriate to take as arguments. CppMockGen chooses a
constructor that has least arguments (preferably no arguments) and
passes 0s to it. It may cause compilation or runtime errors.

I assume an instance as a global variable has a constructor without
arguments. It is reasonable to avoid the problem that an order to
initialize global variables is not defined in C++ and they should be
initialized after _main_ called (or using the construct on first use
idiom).

## Write test cases with the generated code

See tester_src/mockgenSampleTestBody.cpp for full source codes.

### Use the forwarder

Assume a test case where it swaps calling a method of the global
variable `anObject` to a method of a mock defined.

```cpp
    extern ::Sample1::Types::DerivedClass anObject;

 1  class TestSample : public ::testing::Test{};
 2  TEST_F(TestSample, SwapVariable) {
 3      {
 4          MOCK_OF(DerivedClass) mock(INSTANCE_OF(anObject));
 5          const int expected = 1;
 6          EXPECT_CALL(mock, Func(0, 0)).Times(1).
                WillOnce(::testing::Return(expected));
 7          EXPECT_EQ(expected, SampleFunc());
 8      }
 9      ASSERT_FALSE(INSTANCE_OF(anObject).pMock_);
10  }
```

* Lines 1 and 2 define a Google Test fixture.
* Lines 3 to 8 define a block scope, a mock instance, and `EXPECT_CALL`
* Line 4 defines a mock instance. `MOCK_OF` takes a type argument and
  return its mock type. `INSTANCE_OF` takes a variable name and return
  an instance name of its forwarder variable.
* Line 6 sets an expectation that `mock.Func` will be called once and
  return 1
* Line 7 calls `SampleFunc()`.
  * `SampleFunc()` calls `mock.Func(0, 0)` instead of
    `anObject.Func(0, 0)` via `anObject_Forwarder`
  * `mock.Func(0, 0)` returns preset value 1 and meets the expectation

The mock attaches the forwarder instance at Line 4 and detaches it at
Line 8 after the mock is deleted. The forwarder calls original
definition `anObject.Func(0, 0)` if no mocks are attached.

Note that C macros cannot take `::` so arguments C macros must be
unqualified (i.e. without class names and namespaces). The generated
code describes `using namespace` directives for this purpose.

### Mock free functions

Assume a test case where it tests a free function. The function
(caller) calls another function (callee) that is defined in a
different source file.
* Do not swap the caller function `FreeFunctionCallerSample` to a mock
* Swap and mock the callee function `FreeFunctionCalleeSample`

```cpp
 1  TEST_F(TestSample, TopLevelNamespace) {
 2      EXPECT_EQ(0, FreeFunctionCallerSample());
 3      {
 4          int expected = 1;
 5          MOCK_OF(All) mock(INSTANCE_OF(all));
 6          EXPECT_CALL(mock, FreeFunctionCalleeSample()).Times(1).
                WillOnce(::testing::Return(expected));
 7          EXPECT_EQ(expected, FreeFunctionCallerSample());
 8      }
 9  }
```

* Line 2 calls the caller function and gets a return value of the callee
  function that propagates to the test case.
* Line 5 defines a `mock` instance. The top-level namespace has the
  `All` prefix for its typename and the `all` prefix for its variable
  name.
* Lines 6 sets up the mock to return value 1.
* Lines 7 calls the tested (caller) function that calls the mock
  instead of the callee function.

When you test a free function that is swapped, call the forwarder directly.

```cpp
    MOCK_OF(All) mock(INSTANCE_OF(all));
    INSTANCE_OF(all).TopLevelSampleFunc();
```

### Use the decorator

Assume a test case swaps the type of the global variable `localObject` to its decorator.

```cpp
    extern DerivedClass localObject;

 1  TEST_F(TestSample, MockClassMember) {
 2      {
 3          MOCK_OF(DerivedClass) mock(localObject);
 4          DECORATOR(DerivedClass)::pClassMock_ = &mock;
 5          constexpr int expected = 3;
 6          EXPECT_CALL(mock, StaticFunc()).Times(1).
                WillOnce(::testing::Return(expected));
 7          EXPECT_EQ(expected, localObject.StaticFunc());
 8      }
 9      ASSERT_FALSE(localObject.pClassMock_);
10  }
```

* Line 3 defines a mock instance. `MOCK_OF` takes a decorator instance.
* Line 4 indicates that the decorator forwards all class
  (non-instance) methods to the mock.
* Lines 5-7 means `localObject.StaticFunc()` is mocked and
  `DerivedClass::StaticFunc()` have not been called.
* The mock is detached automatically at Line 8 like the forwarder.

The default behavior of the decorator is forwarding all invocation to
base (i.e. original, decorated) class for all instance methods, NOT
forwarding for class (non-instance) methods. This behavior requires
the explicit setup in Line 4.

Other examples are available in mockgenSampleTestBody.cpp
* `TEST_F(TestSample, SwapType)` mocks instance methods.
* `TEST_F(TestSample, SwapArray)` decorates a C-style variable array.

### Mock methods selectively

The default behavior of the forwarder is mocking its all methods. We
can enable or disable to mock for each function. In this example, the
test case disables to mock `Func`.

```cpp
 1  TEST_F(TestSample, ForwardSelective) {
 2      EXPECT_EQ(0, SampleFunc());
 3      MOCK_OF(DerivedClass) mock(INSTANCE_OF(anObject));
 4      {
 5          INSTANCE_OF(anObject).Func_nomock_ = true;
 6          {
 7              EXPECT_CALL(mock, Func(0, 0)).Times(0);
 8              EXPECT_EQ(0, SampleFunc());
 9          }
10  }
```

Line 5 indicates the mock does not mock `Func` and still mocks other
methods of it. The variable _Func_nomock__ is a switch to do it.

Overloaded functions share a switch variable "their function name +
nomock".

For the forwarders, a switch variable is an instance member variable
for an instance method and a class member variable for and class
method.

For the decorators, a switch variable is always a class member
variable because tester codes may not get instances the decorators.

### Mock functions called via pointers to free functions

We can switch a pointer to a free function to the generated mock, and
mock the free function.

The utility function `GetFreeFunctionSwitch` helps to bridge different
types between the free function and a mock instance method.

```cpp
    using  FuncPtrWithoutArgType = int(*)(void);
    extern FuncPtrWithoutArgType g_funcPtrWithoutArg;
    extern int funcWithoutArg1(void);
    extern int funcWithoutArg2(void);
 1  TEST_F(TestFreeFunctionSwitch, CallFunctionPointer) {
 2      MOCK_OF(All) mock(INSTANCE_OF(all));
 3      {
 4          auto pSwitch = GetFreeFunctionSwitch(
                g_funcPtrWithoutArg, mock, &MOCK_OF(All)::funcWithoutArg1);
 5          EXPECT_EQ(g_returnValueWithoutArg1, callFuncPtrWithoutArg());
 6          g_funcPtrWithoutArg = &funcWithoutArg2;
 7          EXPECT_EQ(g_returnValueWithoutArg2, callFuncPtrWithoutArg());

 8          pSwitch->SwitchToMock();
 9          const int expected = g_returnValueWithoutArg2 + 1;
10          EXPECT_CALL(mock, funcWithoutArg1()).Times(1).
                WillOnce(::testing::Return(expected));
11          EXPECT_EQ(expected, g_funcPtrWithoutArg());
12      }
13      EXPECT_EQ(&funcWithoutArg1, g_funcPtrWithoutArg);
14  }
```

* Line 4 creates an instance of the switch. The return type is a
  std::unique_ptr<> and deletes its instance automatically after the
  instance is out of the scope. C++11 `auto` keyword lets you forget
  the return type.
* Line 5 confirms the value of the pointer to the function is not
  changed from the tested code.
* Lines 6 and 7 changes the value of the pointer in a usual manner.
* Line 8 indicates the pointer forwards to the `mock.funcWithoutArg1`
  as specified in Line 4.
* Lines 10 and 11 mocks the pointer.
* Line 13 confirms the value of the pointer is restored after the
  switch is deleted.

`GetFreeFunctionSwitch` has constraint that tester code can use up to 10
switches per signature (a set of return and argument types) because
they share a template class definition and its class variables.

Macro `MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME` and free function
`GetFreeFunctionSwitchByName` are altanative without the limitation.

```cpp
MACRO_GET_FREE_FUNCTION_SWITCH_BY_NAME(UniqueName)

TEST_F(TestFreeFunctionSwitch, CallFunctionPointer) {
    auto pSwitch = GetFreeFunctionSwitchByName(UniqueName,
        g_funcPtrWithoutArg, mock, &MOCK_OF(All)::funcWithoutArg1);
}
```

The macro must be outside of any block. `UniqueName` is a unique
switch name that a caller set.  It can be same as a name of a global
variable. `GetFreeFunctionSwitchByName` generates new symbols with the
name internally.


## Include generated code from tested codes

### Genereted files

To swap types and variables in tested codes, source files that contain
tested codes need to include the generated code. You may surround
their include directives with `#ifdef TESTING ... #endif` to disable
them in your product code.

CppMockGen sets basenames of the generated files as its
arguments. These basenames are described in the Makefile and you can
change them.

|Filename|Description|
|:-------|:----------|
|mock_*.hpp|Class definitions|
|typeSwapper_*.hpp|Macros to swap tested class types to decorator classes types|
|varDecl_*.hpp|Declarations of forwarder instances|
|varSwapper_*.hpp|Macros to swap tested class instances to forwarder instances|
|varDef_*.cpp|Definitions of class members|

### Launch the script

Run `make` and copy its command log for a quick start of CppMockGen.

```bash
ruby script/mockgen.rb mock  -filter "[A-Z]" -filterout NotMocked -source tested_src/mockgenSampleNotSwapped.cpp -systempath /opt/gcc6 tested_include/mockgenSample1.hpp ./generated/link_error.log (omitted) -cc1 (omitted)
```

Hyphens between words in each option can be omitted. CppMockGen
treats -outheaderfile as -out-header-file.

* -filter "[A-Z]" : CppMockGen excludes free functions of which name
   has no upper cases; system and standard functions.
* -filterout NotMocked : CppMockGen excludes classes of which name
   matches regular expression /NotMocked/
* -source filename : CppMockGen does not swap free functions in
   `filename`. We have to avoid swapping definitions of tested
   functions and need to set this option.
* -outheaderfile filename (shown later) : CppMockGen creates `filename`
  that contains include directives same as source files specified with
  -source options.
* -systempath appends a system and compiler directory such as /opt.
* First filename (mockgenSample1.hpp) : A tested header
  file. CppMockGen parses and makes mocks for classes and free
  functions which are defined in `filename` and in files that
  `filename` includes recursively.
* Second filename (link_error.log) : A log text file _ld_ wrote. It
  may contain undefined symbols. It means no stubs needed that
  `link_error.log` does not exist or contains no undefined symbols.
* Other filenames : CppMockGen writes the files.
* -cc1 and more arguments : CppMockGen passes the arguments to clang
  -cc1. Compile options such as `-I`s, `-D`s, and `-m32` should be
  -same as tested codes.

The first file (tested header file) should include all header files
that tested and testing source files need. You can collect header
files for a source file by `g++ (or clang++) -H file.cpp` manually.
Note that you have to pass `-I`s and `-Ds` to compilers to find header
files that the source files include.

Instead of this, CppMockGen creates an all-in-one header when -source
and -outheaderfile options are specified. The created (output) header
file can be same as the first (input) header file of arguments.

```bash
ruby script/mockgen.rb (omitted) -source file1.cpp -source file2.cpp -outheaderfile ./generated/allInOne.hpp ./generated/allInOne.hpp ./generated/link_error.log (omitted)
```

If all filenames specified with -source have an extension .c,
CppMockGen treats the files as C (not C++) source code and disregards
argument types in a linker error log to filter free functions in the
files. To change this behavior, you can set the constant
`MODE_DEFAULT_NO_MATCHING_TYPES_IN_C_SOURCES` to false.

### Name generated files

*_Stub files contain codes that relate free functions and stubs.

The generated files have names that contain filenames in arguments of
`mockgen.rb` and a serial number by default. It is troublesome to find
which file a tested code should include. CppMockGen gives two options.

* Files that have names same as arguments include other headers. In
  other words, these are aggregated headers.
* If you pass `-split 1` to `mockgen.rb`, CppMockGen gives names for
  output filenames with a class name that the file contains instead of
  serial numbers.
  * Input source file (in arguments) : tested_include/mockgenSample1.hpp
  * Aggregated mock file (in arguments) : generated/converted_mockgenSample1.hpp
  * Output mock file (numbered) : mock_mockgenSample1_1.hpp
  * Output mock file (named after a tested class) : mock_mockgenSample1_DerivedClass.hpp

## And more

More details are described as comments in script/mockgen.rb.
