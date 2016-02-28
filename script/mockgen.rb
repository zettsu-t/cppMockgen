#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Google Mock Generator from C++ header files
#
# Usage:
# $ mockgen.rb [stub|mock] [-filter pattern]* 8-filenames clang-options
#
# - stub or mock (specify stub to generate stubs
#   stub : generate stubs
#   mock : generate stubs and mocks
# - none or more sets of -filter pattern
#   regular expression to filter free functions to mock
#   no filters mean to handle all free functions including system headers
# The trailing arguments are in/out filenames.
# - [input file]  input .hpp file
# - [input file]  LD output (link error log) file
# - [output file] intermediate .hpp file that clang writes
# - [output file] class definition
# - [output file] type swapper macro
# - [output file] variable swapper macro
# - [output file] forwarder declaration
# - [output file] forwarder definition
# All other arguments are passed to clang. Specify include paths here.
#
# Before parsing input files, this script launches the clang front end
# to format the files.
# + Execute preprocessing, import headers and expand macros
# + Indent lines. Write a symbol declaration and definition in one line.
# + Resolve implicit namespaces
#
# In my environment, the clang front end requires these option.
# These paths may differ in different environment such as
# + Cygwin/MinGW 64 or 32bit version
# + C++ compiler version
# + Google Test/Mock directories
#
# Cygwin64 + bash:
#   clang++ -cc1 -ast-print -fblocks -fgnu-keywords -x c++ -std=gnu++11
#     -cxx-isystem /usr/include
#     -cxx-isystem /usr/lib/gcc/x86_64-pc-cygwin/4.9.3/include/c++/x86_64-pc-cygwin
#     -cxx-isystem /usr/lib/gcc/x86_64-pc-cygwin/4.9.3/include/c++
#     -cxx-isystem /usr/lib/gcc/x86_64-pc-cygwin/4.9.3/include
#     -cxx-isystem /usr/include/w32api
#     -I ../../../googletest/googletest/include
#     -I ../../../googletest/googletest -I ../../../googletest/googlemock/include
#     -I ../../../googletest/googlemock
#     input.hpp > output.hpp
#
# MinGW64 + cmd: directories must be double-quoted
#   clang++ -cc1 -ast-print -fblocks -fgnu-keywords -x c++ -std=gnu++11
#     -cxx-isystem "C:\Program Files\mingw-w64\x86_64-4.9.2-posix-seh-rt_v3-rev1\
#                   mingw64\x86_64-w64-mingw32\include\c++\x86_64-w64-mingw32"
#     -cxx-isystem "C:\Program Files\mingw-w64\x86_64-4.9.2-posix-seh-rt_v3-rev1\
#                   mingw64\x86_64-w64-mingw32\include\c++"
#     -cxx-isystem "C:\Program Files\mingw-w64\x86_64-4.9.2-posix-seh-rt_v3-rev1\
#                   mingw64\x86_64-w64-mingw32\include"
#     -cxx-isystem "C:\Program Files\mingw-w64\x86_64-4.9.2-posix-seh-rt_v3-rev1\
#                   mingw64\lib\gcc\x86_64-w64-mingw32\4.9.2\include"
#     -I Google Test/Mock header dirs
#     input.hpp > output.hpp

require_relative './mockgenImpl.rb'

# Execute main script and return its result as the exit code
Mockgen::MockGenLauncher.new(ARGV.dup).generate
