# How to use CppMockGen in your Makefile

This file describes what my Makefile does. I hope it will help
you use CppMockGen in your build system.

## Show example

Run the attached Makefile and you can get a command list to build an
executable.

```bash
make --dry-run clean all 2>&1 | tee makelog.txt
```

This is a full output on my Cygwin environment and contains testing
for CppMockGen. Testing is unnecessary for you and I'll explain key
points of it the next section.

```text
rm -f ././generated/*.hpp ././generated/*.cpp
rm -f bin/mockgenSample ./generated/converted_mockgenSample1.hpp ./generated/mock_mockgenSample1.hpp ./generated/typeSwapper_mockgenSample1.hpp ./generated/varSwapper_mockgenSample1.hpp ./generated/varDecl_mockgenSample1.hpp ./generated/varDef_mockgenSample1.cpp ./generated/link_error.log obj/mockgenSampleTestTypes.o obj/mockgenSampleTestMain.o obj/mockgenSampleTestBody.o obj/mockgenSampleBody.o obj/mockgenSampleNotSwapped.o obj/mockgenSampleUser.o obj/gtest-all.o obj/gmock-all.o  obj/mockgenSampleTestTypes.d  obj/mockgenSampleTestMain.d  obj/mockgenSampleTestBody.d  obj/mockgenSampleBody.d  obj/mockgenSampleNotSwapped.d  obj/mockgenSampleUser.d obj/mockgenSampleTestTypes.processed obj/mockgenSampleTestMain.processed obj/mockgenSampleTestBody.processed obj/mockgenSampleBody.processed obj/mockgenSampleNotSwapped.processed obj/mockgenSampleUser.processed ./obj/*.o ./obj/*.d ././generated/*.hpp ././generated/*.cpp
mkdir -p ././generated ./obj ./bin
ruby script/mockgen.rb mock -ignoreinternalerrors -discardnamespaces internal  -filter '[A-Z]' -filterout "NotMock(ed)?" -exclude "NoStub.*Mock" -source tester_src/mockgenSampleTestTypes.cpp -source tester_src/mockgenSampleTestMain.cpp -source tester_src/mockgenSampleTestBody.cpp -source tested_src/mockgenSampleNotSwapped.cpp -outheaderfile ./generated/all_in_one.hpp -vtable -mockguard GENERATING_MOCK tested_include/mockgenSample1.hpp ./generated/link_error.log ./generated/converted_mockgenSample1.hpp ./generated/mock_mockgenSample1.hpp ./generated/typeSwapper_mockgenSample1.hpp ./generated/varSwapper_mockgenSample1.hpp ./generated/varDecl_mockgenSample1.hpp ./generated/varDef_mockgenSample1.cpp -cc1 -ast-print -fgnu-keywords -x c++  -std=gnu++11  -Itested_include -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -DGENERATING_MOCK
ls ././generated/*_Stub.hpp
ls ././generated/*_1.hpp
ls ./generated/mock_mockgenSample1_1.hpp
grep -q GENERATED_MOCK_CPP_FILE `find ./generated -name "*.hpp" -not -name "*Inline*"` ; test $? -eq 1
grep mockgenSample ./generated/all_in_one.hpp | wc | grep "  2  "
grep "NotMock(ed)?" ./generated/mock_mockgenSample1_1.hpp | wc | grep "  0  "
make -j 5 -f /cygdrive/c/home/zet/trunk/program/mockgen/Makefile_compile
make[1]: Entering directory '/cygdrive/c/home/zet/trunk/program/mockgen'
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -Iutility -Itester_include  -o obj/mockgenSampleTestTypes.processed -E tester_src/mockgenSampleTestTypes.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -Iutility -Itester_include  -o obj/mockgenSampleTestMain.processed -E tester_src/mockgenSampleTestMain.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -Iutility -Itester_include  -o obj/mockgenSampleTestBody.processed -E tester_src/mockgenSampleTestBody.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/mockgenSampleBody.processed -E tested_src/mockgenSampleBody.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/mockgenSampleNotSwapped.processed -E tested_src/mockgenSampleNotSwapped.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/mockgenSampleUser.processed -E tested_src/mockgenSampleUser.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -Iutility -Itester_include  -o obj/mockgenSampleTestTypes.o -c tester_src/mockgenSampleTestTypes.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -Iutility -Itester_include  -o obj/mockgenSampleTestMain.o -c tester_src/mockgenSampleTestMain.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -Iutility -Itester_include  -o obj/mockgenSampleTestBody.o -c tester_src/mockgenSampleTestBody.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/mockgenSampleBody.o -c tested_src/mockgenSampleBody.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/mockgenSampleNotSwapped.o -c tested_src/mockgenSampleNotSwapped.cpp
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/mockgenSampleUser.o -c tested_src/mockgenSampleUser.cpp
g++  -Wall -O2 -std=gnu++11  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/gtest-all.o -c /cygdrive/c/home/zet/googletest/googletest/src/gtest-all.cc
g++  -Wall -O2 -std=gnu++11  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock  -o obj/gmock-all.o -c /cygdrive/c/home/zet/googletest/googlemock/src/gmock-all.cc
make[1]: Leaving directory '/cygdrive/c/home/zet/trunk/program/mockgen'
g++  -o bin/mockgenSample obj/mockgenSampleTestTypes.o obj/mockgenSampleTestMain.o obj/mockgenSampleTestBody.o obj/mockgenSampleBody.o obj/mockgenSampleNotSwapped.o obj/mockgenSampleUser.o obj/gtest-all.o obj/gmock-all.o   -lstdc++ 2>&1 | tee ./generated/link_error.log
```

## What the Makefile does?

This section explains what the Makefile does step-by-step.

### Make mock files

A command to launch CppMockGen is described in the Makefile.

```make
generate: $(ORIGINAL_HEADER) $(GENERATOR_SCRIPT) $(GENERATOR_SCRIPT_FILES) create_dirs
    $(RUBY) $(GENERATOR_SCRIPT)
    $(GENERATOR_MODE)
    $(GENERATOR_SPECIAL_FLAGS)
    $(GENERATOR_NO_FORWARDING_TO_MOCK)
    $(GENERATOR_FILTER)
    $(GENERATOR_SOURCES)
    $(GENERATOR_OUTPUT_HEADER)
    $(GENERATOR_FILL_VTABLE)
    $(GENERATOR_MOCK_GUARD)
    $<
    $(LINK_ERROR_LOG)
    $(GENERATED_FILES)
    $(CLANG_FLAGS)
    $(GENERATOR_FLAGS)
```

And GNU __make__ expands it as below.

```text
ruby script/mockgen.rb
mock
-ignoreinternalerrors -discardnamespaces internal
(__GENERATOR_NO_FORWARDING_TO_MOCK__ is empty)
-filter '[A-Z]' -filterout "NotMock(ed)?" -exclude "NoStub.*Mock"
-source tester_src/mockgenSampleTestTypes.cpp -source tester_src/mockgenSampleTestMain.cpp -source tester_src/mockgenSampleTestBody.cpp -source tested_src/mockgenSampleNotSwapped.cpp
-outheaderfile ./generated/all_in_one.hpp
-vtable
-mockguard GENERATING_MOCK
tested_include/mockgenSample1.hpp
./generated/link_error.log
./generated/converted_mockgenSample1.hpp ./generated/mock_mockgenSample1.hpp ./generated/typeSwapper_mockgenSample1.hpp ./generated/varSwapper_mockgenSample1.hpp ./generated/varDecl_mockgenSample1.hpp ./generated/varDef_mockgenSample1.cpp
-cc1 -ast-print -fgnu-keywords -x c++ -std=gnu++11
-Itested_include -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -DGENERATING_MOCK
```

This command
- generates mocks
- with options for CppMockGen (see [howToUse.md](howToUse.md))
- reads from the .cpp files, tested_include/mockgenSample1.hpp and ./generated/link_error.log
- generates ./generated/all_in_one.hpp and tested_include/mockgenSample1.hpp ... ./generated/varDef_mockgenSample1.cpp
- passes "-cc1 -ast-print -fgnu-keywords -x c++ -std=gnu++11" options to the clang front-end
- with include paths for this build (-I) and system (-isystem). It depends on your environment.

Include-paths are complicated for LLVM on Windows and it will be helpful to run my Makefile and copy-and-paste its command list.

### Edit you C++ source code

As in tested_src/mockgenSampleUser.cpp, let your C++ headers or source files include generated mocks. Note that you have to surround the include directives with the guard __GENERATING_MOCK__ (or other name specified with -mockguard as above) to prevent including generated files in generating themselves.

```cpp
#include "mockgenSample1.hpp"
// Swap class names used later to their decorator
#ifndef GENERATING_MOCK
#include "typeSwapper_mockgenSample1.hpp"
#endif

#include "mockgenSample2.hpp"
// Swap variables already declared to their forwarder
#ifndef GENERATING_MOCK
#include "varSwapper_mockgenSample1.hpp"
#endif
```

### Compile and link C++ code with generated codes

You can compile and link C++ source files as usual.

If you intend to make stubs for missing definitions, write linker outputs to _./generated/link_error.log_ as below. Do not forget to have GNU __make__ ignore error (non-zero) exit status of the linker.

```bash
g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletes
  ...
g++  -o bin/mockgenSample obj/mockgenSampleTestTypes.o obj/mockgenSampleTestMain.o obj/mockgenSampleTestBody.o obj/mockgenSampleBody.o obj/mockgenSampleNotSwapped.o obj/mockgenSampleUser.o obj/gtest-all.o obj/gmock-all.o -lstdc++ 2>&1 | tee ./generated/link_error.log
```

### Make stubs (optional)

If you generate stubs for missing definitions and use them, run CppMockGen again with linker outputs again.

```bash
make --dry-run clean runthrough_gcc 2>&1 | tee makelog.txt
```

This command is exactly same as the first execution (actually missing link_error.log means no stubs are needed to generate).

```text
ruby script/mockgen.rb mock
-ignoreinternalerrors -discardnamespaces internal
-filter '[A-Z]' -filterout "NotMock(ed)?" -exclude "NoStub.*Mock"
-source tester_src/mockgenSampleTestTypes.cpp -source tester_src/mockgenSampleTestMain.cpp -source tester_src/mockgenSampleTestBody.cpp -source tested_src/mockgenSampleNotSwapped.cpp
-outheaderfile ./generated/all_in_one.hpp
-vtable
-mockguard GENERATING_MOCK
tested_include/mockgenSample1.hpp
./generated/link_error.log
./generated/converted_mockgenSample1.hpp
./generated/mock_mockgenSample1.hpp ./generated/typeSwapper_mockgenSample1.hpp ./generated/varSwapper_mockgenSample1.hpp ./generated/varDecl_mockgenSample1.hpp ./generated/varDef_mockgenSample1.cpp
-cc1 -ast-print -fgnu-keywords -x c++ -std=gnu++11
-Itested_include -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletest/googlemock -DGENERATING_MOCK

g++  -g -Wall -O0 -std=gnu++11   -Itested_include -I./generated  -isystem /cygdrive/c/home/zet/googletest/googletest/include -isystem /cygdrive/c/home/zet/googletest/googletest -isystem /cygdrive/c/home/zet/googletest/googlemock/include -isystem /cygdrive/c/home/zet/googletes
  ...
g++  -o bin/mockgenSample obj/mockgenSampleTestTypes.o obj/mockgenSampleTestMain.o obj/mockgenSampleTestBody.o obj/mockgenSampleBody.o obj/mockgenSampleNotSwapped.o obj/mockgenSampleUser.o obj/gtest-all.o obj/gmock-all.o -lstdc++ 2>&1 | tee ./generated/link_error.log
```
