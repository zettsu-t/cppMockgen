#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Constants

module Mockgen
  module Constants
    # Argument to specify mode
    ARGUMENT_MODE_STUB = "stub".freeze
    ARGUMENT_MODE_MOCK = "mock".freeze

    # Namespaces not parsed
    NAMESPACE_SKIPPED_SET = ["std", "boost", "mpl_", "testing"].map(&:freeze).freeze
    # Reserved for compiler internal use
    NAMESPACE_COMPILER_INTERNAL = "__".freeze
    # Excluded class names (practically, they should be specified in command line options)
    CLASS_NAME_EXCLUDED_SET = ["$$$"]

    KEYWORD_USER_DEFINED_TYPE_SET = ["struct", "class", "enum", "union"].map(&:freeze).freeze
    # Keyword attribute
    KEYWORD_ATTRIBUTE_WITH_ARGS = ["__attribute__", "__aligned"].map(&:freeze).freeze
    # Keyword qualifier
    KEYWORD_ATTRIBUTE_WITHOUT_ARGS = ["__packed"].map(&:freeze).freeze
    # Keyword nullptr that the cland front end replaced
    KEYWORD_NULL_EXPR_CLANG_SET = ["__null", "<null expr>"].map(&:freeze).freeze
    KEYWORD_NULL_EXPR_CPP = "NULL".freeze
    # Keyword undefined reference
    KEYWORD_UNDEFINED_REFERENCE = "undefined reference to".freeze

    # Pattern to exclude free function mocks and stubs
    FREE_FUNCTION_FILTER_OUT_PATTERN = "^[\da-z_]+$"
    # Tentative workaround for MinGW pthread
    FREE_FUNCTION_FILTER_OUT_WORD_SET = ["_pthread_key_dest"]

    # Keywords which are excluded to make mocks in class member functions
    MEMFUNC_WORD_SKIPPED_SET = ["virtual", "inline", "explicit", "static"].map(&:freeze).freeze
    # Keywords to detect no argument variable is specified
    MEMFUNC_WORD_RESERVED_TYPE_SET = [
      "bool", "char", "short", "int", "long", "float", "double",
      "unsigned", "size_t", "ssize_t", "uintptr_t", "intptr_t", "ptrdiff_t",
      "int8_t", "int16_t", "int32_t", "int64_t",
      "uint8_t", "uint16_t", "uint32_t", "uint64_t"].map(&:freeze).freeze
    MEMFUNC_WORD_END_OF_TYPE_SET = ["*", "&", "]", MEMFUNC_WORD_RESERVED_TYPE_SET].flatten.map(&:freeze).freeze
    MEMFUNC_WORD_COMPARED_SET = ["const", "&", "&&"].map(&:freeze).freeze

    # Keywords which are qualifiers for member functions, do not appear in member variables
    MEMVAR_FIRST_WORD_REJECTED_SET = ["using", "typedef"].map(&:freeze).freeze
    MEMVAR_FIRST_WORD_EXCLUDED_SET = ["extern", "static", "struct", "class"].map(&:freeze).freeze
    MEMVAR_LAST_WORD_REJECTED_SET = ["const", "override", "final", "&", "&&"].map(&:freeze).freeze

    # The namespace which containts generated codes
    GENERATED_SYMBOL_NAMESPACE = "MyUnittest".freeze

    # Split root .cpp into multiple files (not implemented completely)
    GENERATED_BLOCKS_PER_SOURCE = 50

    # Fixed variable names
    VARNAME_INSTANCE_MOCK = "pMock_".freeze
    VARNAME_CLASS_MOCK = "pClassMock_".freeze

    # Postfixes to generated symbols
    CLASS_POSTFIX_DECORATOR = "_Decorator".freeze
    CLASS_POSTFIX_FORWARDER = "_Forwarder".freeze
    CLASS_POSTFIX_MOCK = "_Mock".freeze
    CLASS_FREE_FUNCTION_SET = "All".freeze

    # clang executable
    CLANG_COMMAND = "clang++".freeze
  end
end

Mockgen::Constants.freeze
