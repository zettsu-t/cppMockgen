# CppMockGen : C++ mock generator for Google Mock

Generating code to test C++ code. It makes
* Mock classes to describe expectations with Google Mock
* Stub functions to link subset of tested source files

## Platform

_CppMockGen_ is built on and runs on Cygwin with tools shown
below. MinGW-w64, Bash on Ubuntu on Windows and Linux are also available.

* Windows 10 Anniversary Update 64bit Edition
* Cygwin 64bit version (2.9.0), MinGW-w64 (Distro 15.1) or Bash on Ubuntu on Windows
* Google Test / Mock [(latest)](https://github.com/google/googletest)
* LLVM and clang (5.0.0)
* gcc (7.2.0)
* Ruby (2.4.0)

## Quick start

Run this command from a console on a directory that contains _Makefile_.

```bash
make runthrough
```

* Generates code from _tested/_ source code
* Links generated, tested and tester code. It fails intentionally due
  to missing functions.
* Generates stub code and links them to _bin/mockgenSample.exe_
  successfully
* Executes _mockgenSample.exe_ and reports its results described with
  Google Test / Google Mock
* Runs tests for _script/*.rb_ and reports its results

Attached _Makefile_ assumes Google Test / Mock are installed at $HOME/googletest.

## License

Copyright (c) 2016, 2017 Zettsu Tatsuya

This software is released under the BSD 3-Clause License (the new BSD
license), see [LICENSE.txt](LICENSE.txt). This follows the license of
Google Test and Google Mock.

## And more

Please read [howToUse.md](howToUse.md) and [notes.md](notes.md) to know more details.
