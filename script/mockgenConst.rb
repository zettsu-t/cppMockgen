#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Constants

module Mockgen
  module Constants
    # Namespaces not parsed
    NAMESPACE_SKIPPED_SET = ["std", "boost", "testing"].map(&:freeze).freeze
    # Reserved for compiler internal use
    NAMESPACE_COMPILER_INTERNAL = "__".freeze

    # Keyword attribute
    KEYWORD_ATTRIBUTE = "__attribute__".freeze

    # Keywords which are excluded to make mocks in class member functions
    MEMFUNC_WORD_SKIPPED_SET = ["virtual", "inline", "explicit", "static"].map(&:freeze).freeze
    # Keywords to detect no argument variable is specified
    MEMFUNC_WORD_RESERVED_TYPE_SET = [
      "bool", "char", "short", "int", "long", "float", "double",
      "unsigned", "size_t", "ssize_t", "uintptr_t", "intptr_t", "ptrdiff_t",
      "int8_t", "int16_t", "int32_t", "int64_t",
      "uint8_t", "uint16_t", "uint32_t", "uint64_t"].map(&:freeze).freeze
    MEMFUNC_WORD_END_OF_TYPE_SET = ["*", "&", "]", MEMFUNC_WORD_RESERVED_TYPE_SET].flatten.map(&:freeze).freeze

    # The namespace which containts generated codes
    GENERATED_SYMBOL_NAMESPACE = "MyUnittest".freeze

    # Fixed variable names
    VARNAME_INSTANCE_MOCK = "pMock_".freeze
    VARNAME_CLASS_MOCK = "pClassMock_".freeze

    # Postfixes to generated symbols
    CLASS_POSTFIX_DECORATOR = "_Decorator".freeze
    CLASS_POSTFIX_FORWARDER = "_Forwarder".freeze
    CLASS_POSTFIX_MOCK = "_Mock".freeze

    # clang executable
    CLANG_COMMAND = "clang++".freeze
  end
end

Mockgen::Constants.freeze
