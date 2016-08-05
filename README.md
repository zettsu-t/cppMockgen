# CppMockGen : C++ mock generator for Google Mock

Generating codes to test C++ codes. It makes
* Mock classes to describe expectations with Google Mock
* Stub functions to link subset of tested source files

## Platform

_CppMockGen_ is built on and runs on Cygwin with tools shown
below. MinGW-w64, Bash on Ubuntu on Windows and Linux are also available.

* Windows 10 Anniversary Update 64bit Edition
* Cygwin 64bit version (2.5.2)
* Google Test / Mock (1.7.0)
* LLVM and clang (3.8.1)
* gcc (5.4.0)
* Ruby (2.2.5p319)

## Quick start

Run this command from a console on a directory that contains _Makefile_.

```bash
make runthrough
```

* Generates codes from _tested/_ source codes
* Links generated, tested and tester codes. It fails intentionally due
  to missing functions.
* Generates stub codes and links them to _bin/mockgenSample.exe_
  successfully
* Executes _mockgenSample.exe_ and reports its results described with
  Google Test / Google Mock
* Runs tests for _script/*.rb_ and reports its results

## And more

Please read [howToUse.md](howToUse.md) and [notes.md](notes.md) to know more details.
