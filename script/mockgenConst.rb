#!/usr/bin/ruby
# -*- coding: utf-8 -*-
#
# Constants

module Mockgen
  module Constants
    # Argument to specify mode
    ARGUMENT_MODE_STUB = "stub".freeze
    ARGUMENT_MODE_MOCK = "mock".freeze
    ARGUMENT_MODE_VAR  = "var".freeze
    ARGUMENT_SET_STUB_ONLY = [ARGUMENT_MODE_STUB, ARGUMENT_MODE_VAR].freeze

    # Discards hyphens between words in command line arguments
    ARGUMENT_FUNCTION_NAME_FILTER = "-filter".freeze
    ARGUMENT_CLASS_NAME_FILTER_OUT = "-filterout".freeze
    ARGUMENT_EXCLUDE_CLASS_NAME = "-exclude".freeze
    ARGUMENT_SOURCE_FILENAME_FILTER = "-source".freeze

    ARGUMENT_TESTED_FILENAME_GLOB = "-tested".freeze
    ARGUMENT_FIND_STATEMENT_FILTER = "-find".freeze
    ARGUMENT_EXPLICIT_CLASS_NAME = "-classname".freeze

    ARGUMENT_SPLIT_FILES_FILTER = "-split".freeze
    ARGUMENT_NO_FORWARDING_TO_MOCK = "-nomock".freeze
    ARGUMENT_OUT_HEADER_FILENAME = "-outheaderfile".freeze
    ARGUMENT_SYSTEM_PATH = "-systempath".freeze
    ARGUMENT_CHECK_INTERNAL_SYSTEM_PATH = "-checkinternalsystempath".freeze
    ARGUMENT_FILL_VTABLE = "-vtable".freeze
    ARGUMENT_NO_OVERLOADING = "-nooverloading".freeze
    ARGUMENT_UPDATE_CHANGES_ONLY = "-updatechangesonly".freeze
    ARGUMENT_IGNORE_INTERNAL_ERROR =  "-ignoreinternalerror".freeze
    ARGUMENT_IGNORE_INTERNAL_ERRORS = "-ignoreinternalerrors".freeze
    ARGUMENT_DISCARD_NAMESPACE = "-discardnamespace".freeze
    ARGUMENT_DISCARD_NAMESPACES = "-discardnamespaces".freeze
    ARGUMENT_MOCK_GUARD_NAME = "-mockguard".freeze
    ARGUMENT_NO_SWAP_INLINE_FUNCTION = "-noswapinlinefunction".freeze
    ARGUMENT_NO_SWAP_INLINE_FUNCTIONS = "-noswapinlinefunctions".freeze

    # Hard-coded mode
    MODE_CHECK_CLASSNAME_IN_RELATIVE_NAMESPACES_ONLY = true.freeze
    MODE_DEFAULT_NO_FORWARDING_TO_MOCK = false.freeze
    MODE_DEFAULT_NO_MATCHING_TYPES_IN_C_SOURCES = true.freeze

    # Namespaces not parsed
    NAMESPACE_SKIPPED_SET = ["std", "boost", "mpl_", "testing"].map(&:freeze).freeze
    NAMESPACE_SKIPPED_MAP = Hash[*(NAMESPACE_SKIPPED_SET.map{ |word| [word, true] }.flatten)].freeze

    # Reserved for compiler internal use
    NAMESPACE_COMPILER_INTERNAL = "__".freeze
    # Excluded class names (practically, they should be specified in command line options)
    CLASS_NAME_EXCLUDED_SET = ["$$$"].map(&:freeze).freeze
    CLASS_NAME_EXCLUDED_MAP = Hash[*(CLASS_NAME_EXCLUDED_SET.map{ |word| [word, true] }.flatten)].freeze

    KEYWORD_USER_DEFINED_TYPE_SET = ["struct", "class", "enum", "union"].map(&:freeze).freeze
    KEYWORD_USER_DEFINED_TYPE_MAP = Hash[*(KEYWORD_USER_DEFINED_TYPE_SET.map{ |word| [word, true] }.flatten)].freeze

    # Keyword attribute
    KEYWORD_ATTRIBUTE_WITH_ARGS = ["__attribute__", "__aligned"].map(&:freeze).freeze
    # Keyword qualifier
    KEYWORD_ATTRIBUTE_WITHOUT_ARGS = ["__packed"].map(&:freeze).freeze
    # Keyword nullptr that the cland front end replaced
    KEYWORD_NULL_EXPR_CLANG_SET = ["__null", "<null expr>"].map(&:freeze).freeze
    KEYWORD_NULL_EXPR_CPP = "NULL".freeze
    # Keyword static assertion and the like
    KEYWORD_FUNCTION_LIKE_EXPRESSION = ["static_assert"].map(&:freeze).freeze
    # Keyword undefined reference
    KEYWORD_UNDEFINED_REFERENCE = "undefined reference to".freeze

    # Tentative workaround for MinGW pthread
    FREE_FUNCTION_FILTER_OUT_WORD_SET = ["_pthread_key_dest"].map(&:freeze).freeze

    # Keywords which are excluded to make mocks in class member functions
    MEMFUNC_WORD_SKIPPED_SET = ["virtual", "inline", "explicit", "static"].map(&:freeze).freeze
    # Keywords to detect no argument variable is specified
    MEMFUNC_WORD_RESERVED_TYPE_SET = [
      "bool", "char", "short", "int", "long", "float", "double",
      "unsigned", "size_t", "ssize_t", "uintptr_t", "intptr_t", "ptrdiff_t",
      "int8_t", "int16_t", "int32_t", "int64_t",
      "uint8_t", "uint16_t", "uint32_t", "uint64_t"].map(&:freeze).freeze

    MEMFUNC_WORD_SKIPPED_MAP = Hash[*(MEMFUNC_WORD_SKIPPED_SET.map{ |word| [word, true] }.flatten)].freeze
    MEMFUNC_WORD_RESERVED_TYPE_MAP = Hash[*(MEMFUNC_WORD_RESERVED_TYPE_SET.map{ |word| [word, true] }.flatten)].freeze

    MEMFUNC_WORD_END_OF_TYPE_SET = ["*", "&", "]", MEMFUNC_WORD_RESERVED_TYPE_SET].flatten.map(&:freeze).freeze
    MEMFUNC_WORD_END_OF_TYPE_MAP = Hash[*(MEMFUNC_WORD_END_OF_TYPE_SET.map{ |word| [word, true] }.flatten)].freeze
    MEMFUNC_WORD_COMPARED_SET = ["const", "&", "&&"].map(&:freeze).freeze
    MEMFUNC_WORD_COMPARED_MAP = Hash[*(MEMFUNC_WORD_COMPARED_SET.map{ |word| [word, true] }.flatten)].freeze

    # Forward a function or not
    MEMFUNC_FORWARD_SWITCH_POSTFIX = "nomock_".freeze
    MEMFUNC_FORWARD_SWITCH_TYPE = "bool".freeze
    # Must be same as default value for global variables
    MEMFUNC_FORWARDING_ON_VALUE = "true".freeze
    MEMFUNC_FORWARDING_OFF_VALUE = "false".freeze

    # Keywords which are qualifiers for member functions, do not appear in member variables
    MEMVAR_FIRST_WORD_REJECTED_SET = ["using", "typedef"].map(&:freeze).freeze
    MEMVAR_FIRST_WORD_EXCLUDED_SET = ["extern", "static", "struct", "class"].map(&:freeze).freeze
    MEMVAR_LAST_WORD_REJECTED_SET = ["const", "override", "final", "&", "&&"].map(&:freeze).freeze

    MEMVAR_FIRST_WORD_REJECTED_MAP = Hash[*(MEMVAR_FIRST_WORD_REJECTED_SET.map{ |word| [word, true] }.flatten)].freeze
    MEMVAR_FIRST_WORD_EXCLUDED_MAP = Hash[*(MEMVAR_FIRST_WORD_EXCLUDED_SET.map{ |word| [word, true] }.flatten)].freeze
    MEMVAR_LAST_WORD_REJECTED_MAP = Hash[*(MEMVAR_LAST_WORD_REJECTED_SET.map{ |word| [word, true] }.flatten)].freeze

    # The namespace which contains generated codes
    GENERATED_SYMBOL_NAMESPACE = "MyUnittest".freeze

    # Fixed variable names
    VARNAME_INSTANCE_MOCK = "pMock_".freeze
    VARNAME_CLASS_MOCK = "pClassMock_".freeze

    # Middle name and Postfixes to generated symbols
    CLASS_SPLITTER_NAME = "in".freeze
    CLASS_POSTFIX_DECORATOR = "_Decorator".freeze
    CLASS_POSTFIX_FORWARDER = "_Forwarder".freeze
    CLASS_POSTFIX_STUB = "_Stub".freeze
    CLASS_POSTFIX_MOCK = "_Mock".freeze
    CLASS_POSTFIX_INLINE = "_Inline".freeze

    # Mock for the top level
    CLASS_FREE_FUNCTION_SET = "All".freeze
    # SYMBOL MARK FOR GENERATED .cpp
    MARK_FOR_GENERATED_CPP = "GENERATED_MOCK_CPP_FILE"
    MARK_FOR_GENERATED_LINES = "#ifndef #{MARK_FOR_GENERATED_CPP}\n#define #{MARK_FOR_GENERATED_CPP}\n#endif //#{MARK_FOR_GENERATED_CPP}\n"

    # To prohibit c++11 keywords, set false
    CPP11_MODE = true
    CPP11_MODE.freeze

    # clang executable
    CLANG_COMMAND = "clang++".freeze
    CLANG_INTERNAL_SYSTEM_HEADER_OPTION = "-internal-isystem".freeze
    CLANG_SYSTEM_HEADER_OPTION_SET = ["-isystem", "-cxx-isystem", CLANG_INTERNAL_SYSTEM_HEADER_OPTION].map(&:freeze).freeze
    CLANG_SYSTEM_HEADER_DEFAULT_SET = ["/usr/include", "/usr/lib"].map(&:freeze).freeze

    # ctags
    CTAGS_COMMAND = "ctags".freeze
    CTAGS_OPTIONS = "-x --c++-kinds=f".freeze
    # keywords in TAGS
    CTAGS_TYPENAME_FUNCTION_DEFINITION = "function".freeze

    # Parameter set to ignore Byte Order Mark of UTF-8
    CHARACTER_ENCODING_PARAMETER_SET = [:invalid => :replace, :replace => " "]

    # Meta characters for Ruby regular expressions
    # - is context dependent but always invalid for C++ symbols
    REGEXP_META_CHARACTER_SET = ["-", "(", ")", "[", "]", "{", "}", ".", "?", "+", "*", "|", "\\"].map(&:freeze).freeze
  end
end

Mockgen::Constants.freeze
