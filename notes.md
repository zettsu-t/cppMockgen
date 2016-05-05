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
  the clang++/g++ pre-processor converts from .hpp and .cpp files.
  It reads the converted files and checks whether the generated macros
  surely replace keywords of the files.
1. Runs _mockgen.rb_ with different arguments and checks generated
  files. One test is creating a file per a tested class and the other
  test is splitting generated classes into multiple files.

### Build partially

It takes a few minutes to test fully. Other _make_ arguments are useful
to confirm your changes quickly.

|Arguments of make|Actions|
|:-------|:----------|
|runthrough_llvm, runthrough_gcc| check all with either clang++ or g++ |
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
* How To filter out classes to work around what CppMockGen cannot handle
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

Even using _-split 1_, all mocks for free functions are in one file to
write _#include generated.hpp_ directives easily.


## Limitations

Limitations of CppMockGen come from both features of CppMockGen and
behavior of the clang front end. To remove the later, I have to
rewrite CppMockGen with C++ to call libTooling in LLVM directly.

### Determine a symbol is in which file

CppMockGen cannot determine each symbol is declared and defined in
which file because the clang++ front end discards filenames in
_#include_ expansion (clang's pretty AST printing is not like a
preprocessor).

CppMockGen applies filters in _-source_ options to NOT preprocessed
files so _#define_ is not applied. It may cause problems if _#define_
changes function names in your .cpp files.

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

### Others

CppMockGen does not support some C++ features such as

* va_arg : I think it is more appropriate in C++ to use boost::any and
  std::tuple to pass arbitrary type and size arguments.
* Write an array of function pointers _T(f[])(args)_ as an argument of
  function declarations : I think this is difficult to read and
  like to define and use its type alias.
* Operator overloading
