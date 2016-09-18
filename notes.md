# CppMockGen details

This file describes details of CppMockGen.

## Files

CppMockGen itself is a set of Ruby scripts. Others are C++ helpers,
C++ sample codes and unit test for the scripts.

### Source files

* script/mockgen.rb : a launcher for mockgenImpl.rb
* script/mockgenImpl.rb : implementation of CppMockGen
* script/mockgenCommon.rb : utility classes for CppMockGen
* script/mockgenConst.rb : constants of CppMockGen
* script/mockgenTest.rb : unit tests for classes in CppMockGen
* script/mockgenReplaceTest.rb : unit test to check generated macros
* utility/mockgenTesterUtility.hpp : C++ helper codes to mock free functions

### Makefiles

* Makefile : builds and executes unit test
* Makefile_compile : compiles C++ source files in parallel
* Makefile_vars : defines variables for the Makefiles

### Others

* makeTags.sh : a bash script to make TAGS file for Emacs
* mockGenerator.flt : a WinMerge filter to filter out generated and intermediate files


## Include paths for g++ and clang++

CppMockGen collects compiler-internal include paths via the command shown below.

```bash
clang++ -###
```

Its stderr output includes _-internal-isystem_ values and CppMockGen
passes them to clang -cc1 as its arguments. It works on my platform
(Cygwin64 and MinGW-64) but may not work on other platforms,
especially in which gcc and clang are installed in non-system standard
directories such as /opt.

## Testing CppMockGen

Run all tests on CppMockGen/ (the parent of script/) directory.

### Testing classes in the scripts

To test mockgen{Impl,Common,Const}.rb, run

```bash
ruby script/mockgenTest.rb
```

and it reports results of the tests.

### Testing generated codes

To test generated codes, run

```bash
make runthrough
```

and it tests everything,

1. Generating mocks and stubs, building an executable test with clang++ and running tests
1. Same as above with g++ instead of clang++
1. A case in which default setting is not to mock functions

In all three tests, _make_ executes the steps shown below.

1. Cleans up all generated files.
1. Runs _mockgen.rb_ which generates mocks and writes source
  files. _mockgen.rb_ does not create stubs in this step due to lack
  of _link_error.log_.
1. Checks whether the generated files exist and do not contain
  classes that should be filtered out.
1. Compiles all .cpp and .cc files in parallel.
1. Runs _g++_ (not _clang++_) to link all .o files and it fails due to
  undefined references.
1. Runs _mockgen.rb_ which reads undefined references in
  _link_error.log_, generates stubs and writes source files.
1. Compiles and links in the same way, and creates
  _mockgenSample.exe_ if no error occurred in the compilation.
1. Runs _mockgenSample.exe_. It executes Google Test fixtures and
  reports their result.
1. Runs _mockgenTest.rb_ which tests classes in _mockgen.rb_ and
  reports its result.
1. Runs _mockgenReplaceTest.rb_ which checks files to which
  the clang++/g++ preprocessor converts from .hpp and .cpp files.
  It reads the converted files and checks whether the generated macros
  surely replace keywords of the files.
1. Runs _mockgen.rb_ with different arguments and checks generated
  files. One test is creating a file per a tested class and the other
  test is splitting generated classes into multiple files.

### Build partially

It takes a few minutes to test fully. Other _make_ arguments are
useful to confirm your changes quickly.

|Argument of make|Actions|
|:-------|:----------|
|runthrough_llvm, runthrough_gcc| check all with either clang++ or g++ |
|runthrough_update| check all with the -update-changes-only option |
|all (or no arguments)| generate, compile, and link all source files |
|check| test all without generating the executable |
|compile| compile all source files without changing generated codes |
|generate| generate codes only |
|show| show Makefile variables to debug the Makefile |

## Arguments

_mockgen.rb_ takes arguments for itself and the clang++ front end. See
details in comments of mockgen.rb. The arguments specify

* Whether to generate stubs only or, to generate stubs and mocks [stub | mock]
* How to filter mocked free functions. [-filter].
* How to filter out classes to work around what CppMockGen cannot handle
  properly [-filterout].
* Which files tester codes directly call [-source].
* How to split output files [-split].

It is a reason why CppMockGen requires _-filter_ option that
CppMockGen cannot determine each symbol is in which file (see the
_Limitations_ section).

I expect tested codes are written in camelCase and not in snakeCase.
I suppose it is reasonable to assume symbols in snakeCase are in
system and standard libraries.

If CppMockGen swaps definitions in files which tester codes directly
call, compilation errors occur because `void forwarder.function() {}`
is an invalid definition as a function. _-source_ option prohibits
this.

Output files may be too big in case that tested codes contain a class
that has many methods. To prevent compilers from crashing due to out
of memory, it is better to split generated files. A special argument
_-split 1_ generates files per class and it helps to find which file a
tester code should include.

All mocks for free functions are in one file to write _#include
generated.hpp_ directives easily.

Instead of the first argument "stub" or "mock", you can use "var" to
append global variable stubs to existing .cpp files that already
contain stubs. It is for missing variables at second linking. Other
arguments should be same as when mocks and stubs are generated
previously.

## Limitations

Limitations of CppMockGen come from both features of CppMockGen and
behavior of the clang front end. To remove the later, I have to
rewrite CppMockGen with C++ to call libTooling in LLVM directly.

CppMockGen relies on code the clang front end writes. This means if
you input C++ compliant code and proper include paths to the clang
front end, it passes at least well-formed code to CppMockGen. When the
clang front end reports errors, it may pass something suspicious code
to CppMockGen and CppMockGen may also write or miss some outputs.

### Determine a symbol is in which file

CppMockGen cannot determine each symbol is declared and defined in
which file because the clang++ front end discards filenames in
_#include_ expansion (clang's pretty AST printing is not like a
preprocessor).

CppMockGen applies filters in _-source_ options to NOT preprocessed
files so _#define_ is not applied. It may cause problems if _#define_
changes function names in your .cpp files.

Filters in _-source_ options use _ctags_ to find definitions of
functions and _ctags_ does not solve implicit namespaces. In this
reason, CppMockGen treats namespaces always relative and tries
backward matching to function names in classes and namespaces between
outputs of ctags and clang. It may lead mismatching.

### Template class

Support for templates of CppMockGen is limited. A known issue is it
cannot handle non-template classes that inherit a template class.

```cpp
template <typename T=unsigned long long> class TypedClass :
    public NonTypedBaseClass { ... }
class NonTypedDerivedClass : public TypedClass<int> { ... }
```

The clang front end generates the code shown below. It creates
original template class definition and its specialization for
`TypedClass`. CppMockGen creates mock classes for both classes and
causes compilation errors.

```cpp
template <typename T = unsigned long long> class TypedClass :
    public Sample1::Types::NonTypedBaseClass {
template <typename T = int> class TypedClass :
    public Sample1::Types::NonTypedBaseClass {
```

CppMockGen ignores implicit and explicit instantiations of templates
so does not determine for which types it must be instantiated.
Testers have to instantiate a template and its mock manually.

### The std namespace

You can write `size_t` instead of `std::size_t` in C++ code on some
platform but cannon do it on other platforms.

In the later case, when you drop the `std::` qualifier, the clang
front end cannot find a type alias for size_t and treats size_t as int
without any warnings. It causes compilation errors for generated
codes. Its error message may be hard to understand especially for
templates.

It needs to write `std::size_t` or `using std::size_t` in code which
CppMockGen parses.

### Others

CppMockGen does not support some C++ features such as

* va_arg : Function declarations do not tell CppMockGen how to parse
  variable argument lists.
* Write an array of function pointers `T(f[])(args)` as an argument of
  function declarations : I think this is difficult to read and I
  would like to define and use its type alias.
* Using directives as alias template types
* Operator overloading
* Perfect forwarding and move semantics `(T&&)`
* CppMockGen does not make mocks for member functions with
  ref-qualifier (`&` and `&&`).
* Matching argument types between link errors and header files handles
  only simple syntax. This causes some missing stubs for references
  for arrays, pointers to functions with multiple arguments, pointers
  to member functions and so on.

Instead of using va_arg in C++, I think it is more appropriate to use
boost::any and std::tuple to pass arbitrary type and number of
arguments.

Instead of writing types of pointers to functions as arguments
directly, please consider to define their aliases and write them as
arguments if available.


## Data structure and converting C++ code

CppMockGen consists of three primary components, launcher, C++ data
structure blocks and symbol tables.

### Launcher

These classes parse command line options, launch the clang front end,
and generate codes.

* MockGenLauncher : parses command line options, launches the clang
  front end, and sends its output to a code generator _CppFileParser_.
* CppIoFilenameSet, CppFileParameterSet : hold the command line options.
* CppFileParser : parses the output of clang, generate codes and write
  them to files.

The clang front end outputs a pretty AST (abstract syntax tree) in the
form of C++ source code. _MockGenLauncher_ feeds clang stdout into a
file.

_CppFileParser_ receives the file and constructs C++ blocks from the
AST. It also constructs defined and undefined references from files
specified in the command line options. Then it collects generated
codes in the C++ blocks and writes to files.

### C++ data structure blocks

C++ blocks are data structure similar to blocks and their nesting in
C++ source code.

_BaseBlock_ and its derived classes represent C++ blocks and their
each instance matches a block in the AST. _RootBlock_ is a special
block that represents the top level namespace and has no parent
blocks.

#### Find blocks

The AST defines a block as in a form of multi-lines `{...}` or a
single line `...;`. _BlockFactory_ parses each line in the AST and
creates block instances. Note that indentation of lines in the AST is
not relevant to their block structure.

#### Merge namespace blocks

C++ code can have multiple blocks for one namespace. The top-level
namespace includes extern "C" blocks. An instance of _FreeFunctionSet_
holds free functions in one namespace. In a similar manner, an
instance of _TypeAliasSet_ holds type aliases (typedefs) in one
namespace or other types like a class.

#### Difference with libTooling and ast-dump

They are slightly different from outputs of libTooling and `clang
-ast-dump` on the points that

* A class block contains its templates but ClassTemplateDecl contains
  CXXRecordDecl in libTooling
* CppFileParser interprets but does not create blocks for some data
  structures such as access specifiers (AccessSpecDecl in -ast-dump)
* CppFileParser does not create statement blocks because it needs only
  signatures of functions.

#### Notices

* `struct` is a syntactic sugar that means `class` with default public
  access and inheritance.
* Functions without arguments `f()` is same as `void` argument
  `f(void)` in C++.
* _ld_ writes link errors for [] in parameter lists as *. They are
  equivalent in C.
* If a member function is virtual, its overriding functions are always
  virtual with or without the virtual keyword. CppMockGen traverses a
  class hierarchy to check whether a member function is (implicitly)
  virtual or not.
* C++11 `override` keyword is helpful to ensure method
  overriding. clang warns if `override` is not consistently used in
  a class hierarchy; all or no derived classes use it. CppMockGen
  follows the suggestion to make classes.
* C++ permits missing argument names in function declarations (even if
  it is not kind to programmers). CppMockGen fills missing argument
  names in constructors and functions to forward them.

### Symbol tables

_ClassInstanceMap_ holds a map from a class name to its instances. It
is used to make macros that swap type and variable
names. _ClassInstance_ holds code to swap them.

_UndefinedReferenceSet_ parses a specified _ld_ log file in the
command line options and collects undefined symbols. Each
_UndefinedReference_ instance hold a symbol in the set.

_DefinedReferenceSet_ launches _ctags_ to find definitions of
functions, collects names and their class (if exist) of functions. It
determines which class is possibly defined or not by the class
names. Each _DefinedReference_ instance holds a name in the set.

_SymbolFilter_ is a set of filters which contains string patterns and
the references to filter functions and classes.

_FunctionReferenceSet_ is a helper class which compares symbols ones
in a _ld_ log and in source files. These symbols are different on
these points.
* Type aliases are solved in the _ld_ log and not resolved in the
  source files.
* C++ linkage (mangled) symbols in the _ld_ log contain a name and
  types. It also indicates a symbol is a variable or a function.
  CppFileParser has to select a symbol in the source files in
  consideration of type aliases and method overloading.
* C linkage (not mangled, extern "C") in the _ld_ log symbols do not
  specify their types and whether it is a variable or a function.
  CppFileParser has to find it in the source files by name.
* The _ld_ log does not contain return types and default arguments of
  functions. CppFileParser searches the source files for the return
  types and the default arguments in making their mocks.

### Utility classes

_TypeStringWithoutModifier_ removes C++ reserved words such class and
enum (they are required in their definition but redundant to define
their instances) and splits pointer `*` and reference `&` from a
typename. It is used to resolve type aliases of a typename.

_LineWithoutAttribute_ removes `__attribute__` from an line. Ignoring
`__attribute__((...))` requires exact matching its left and right
parenthesis (two or more pairs). _StringOfParenthesis_ matches it with
a recursive regular expression. _StringOfAngleBrackets_ also matches
nested <>s to parse template parameters.


## Steps to generate code

This section describes steps to generate mock and stub code.

### Create an all-in-one header file (optional)

_CppFileParser_ reads source files, collects header files that the
source files include, and writes it to a file specified with the
-outheaderfile option.

1. Launches _clang++ -H_ with -I and -D options same as for preprocessors.
1. Filters out system headers. It uses partial matching and may cause
a mismatch in your project directories.
1. Writes include directives with absolute paths that helps to build
in any directory.

System headers mean
* Paths explicitly specified with -isystem and -cxx-isystem
* System internal paths that _clang -###_ tells with -internal-isystem
* Linux and Cygwin system directories /usr/include and /usr/lib. If
  you need to add directories such as /opt, use the _-systempath_ option.

When you specify the _-check-internal-system-path_ option for
CppMockGen, it fails if _clang -###_ tells none of -internal-isystem
paths. It implies that clang may not installed properly and cannot
work with gcc.

### Preprocess a header file by the clang front end

Before parsing input files, CppMockGen launches the clang front end to
format the files. clang
* Executes preprocessing; imports headers and expands macros
* Indents lines and formats block structures to put symbol declaration and definition in one line.
* Resolves implicit namespaces
* Removes all comments

clang splits the typedef idiom to define a struct and its alias simultaneously

```cpp
typedef struct tagName {
    ...
} Name;
```

into a definition of the struct and its typedef.

### Parse the header file

_CppFileParser_ reads the preprocessed header file, parse its each
line and creates blocks.

CppMockGen filters out data structures that CppMockGen does not handle.
* Standard C++, Boost C++, Google Test / Google Mock headers
* Compiler internal symbols that contain double underline `(__)`
* Compiler internal symbols that begin with an underline followed
  immediately by an uppercase letter `(_A*)`
* Data structures which are defined in given source files in
  the argument.

### Parse files to make filters for symbols

As described in the section _Symbol tables_, CppMockGen parses a _ld_
output file and source files to filter symbols later.

### Construct class hierarchy

CppFileParser merges and ties the blocks. It contains these items and
applies the filters.
* Collecting free functions in each namespace
* Searching public base classes for a class and connecting them
* Building a tree of blocks to search local typedefs
* Collecting global variable declarations

CppMockGen abandons blocks which are unused in later steps because
standard C++ and Boost C++ headers are large.

### Format and write codes

Each block formats codes. CppFileParser collects them and writes to
files. A block class acts a parser, a data holder, and a code
generator in CppMockGen and, are not split into multiple classes now.
