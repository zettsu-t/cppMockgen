# Makefile of the mock generator for Google Mock
#
# To use clang++ instead of g++, set the environment variable CXX
# bash : export CXX=clang++
# cmd  : set CXX=clang++

# Write error message in English
export LC_ALL=C

# Include variables
THIS_DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
include $(THIS_DIR)Makefile_vars

# Compile parallel in a sub makefile
MAKEFILE_SUB_COMPILE=$(THIS_DIR)Makefile_compile
MAKEFILE_PARALLEL=-j 5

ALL_UPDATED_VARIABLES+= THIS_DIR MAKEFILE_SUB_COMPILE MAKEFILE_PARALLEL

.PHONY: all runthrough runthrough_llvm runthrough_gcc
.PHONY: runthrough_default_no_mocks runthrough_cxx
.PHONY: check target_with_var_stub compile generate generate_var
.PHONY: test_generate_c_func test_generate_all_in_one_header
.PHONY: test_generate_each test_generate_bulk
.PHONY: clean clean_generated rebuild show showall FORCE

all: $(TARGETS)

runthrough:
	$(MAKE) test_generate_c_func
	$(MAKE) test_generate_all_in_one_header
	$(MAKE) runthrough_llvm
	$(MAKE) runthrough_gcc
	$(MAKE) runthrough_default_no_mocks
	@$(ECHO) -e $(ECHO_START_FG)All tests have completed successfully.$(ECHO_END_FG)

runthrough_llvm: export CXX=clang++
runthrough_llvm: runthrough_cxx

runthrough_gcc: export CXX=g++
runthrough_gcc: runthrough_cxx

runthrough_default_no_mocks: export CXX=g++
runthrough_default_no_mocks: export GENERATOR_NO_FORWARDING_TO_MOCK=-nomock
runthrough_default_no_mocks: export CXX_MOCK_FLAGS=-DNO_FORWARDING_TO_MOCK_IS_DEFAULT
runthrough_default_no_mocks: runthrough_cxx

runthrough_cxx:
	-$(MAKE) clean
	-$(MAKE)
	$(MAKE)
	$(MAKE) target_with_var_stub
	$(MAKE) check
	@$(ECHO) -e $(ECHO_START_BG)All tests have completed with $(CXX)$(ECHO_END_BG)

# Bash on Ubuntu on Windows uses still Ruby 1.9.3p484 which causes errors in data driven testing.
check:
	$(TARGET_EXE)
ifneq ($(BUILD_ON_BASH_ON_UBUNTU_ON_WINDOWS),yes)
	$(RUBY) $(GENERATOR_SCRIPT_TEST)
endif
	$(RUBY) $(PREPROCESS_SCRIPT_TEST) $(PREPROCESS_SCRIPT_TEST_FILES)
	$(MAKE) test_generate_each
	$(MAKE) test_generate_bulk

# Running after clean causes link errors due to undefined functions.
# Run again and make their stubs to link successfully.
$(TARGET_EXE): $(GENERATED_FILES) $(ALL_OBJS) $(PROCESSED_CPPS) FORCE
	$(MAKE) $(MAKEFILE_PARALLEL) -f $(MAKEFILE_SUB_COMPILE)
	$(eval GENERATED_SOURCES := $(notdir $(wildcard $(GENERATED_FILE_DIR)/*.cpp)))
	$(eval GENERATED_OBJS := $(addprefix $(OBJ_DIR)/, $(patsubst %.cpp, %.o, $(GENERATED_SOURCES))))
	$(LD) $(LIBPATH) -o $@ $(ALL_OBJS) $(GENERATED_OBJS) $(LDFLAGS) $(LIBS) 2>&1 | tee $(LINK_ERROR_LOG)

target_with_var_stub: generate_var FORCE
	$(MAKE) $(MAKEFILE_PARALLEL) -f $(MAKEFILE_SUB_COMPILE)
	$(eval GENERATED_SOURCES := $(notdir $(wildcard $(GENERATED_FILE_DIR)/*.cpp)))
	$(eval GENERATED_OBJS := $(addprefix $(OBJ_DIR)/, $(patsubst %.cpp, %.o, $(GENERATED_SOURCES))))
	$(LD) $(LIBPATH) -o $(TARGET_EXE) $(ALL_OBJS) $(GENERATED_OBJS) $(LDFLAGS) $(LIBS) 2>&1 | tee $(LINK_ERROR_LOG)

# This assumes code is already generated
compile:
	$(MAKE) $(MAKEFILE_PARALLEL) -f $(MAKEFILE_SUB_COMPILE)

# Define rules in the sub makefile
$(ALL_OBJS) : ;
$(PROCESSED_CPPS) : ;

$(GENERATED_FILES): generate

generate: $(ORIGINAL_HEADER) $(GENERATOR_SCRIPT) $(GENERATOR_SCRIPT_FILES)
	$(RUBY) $(GENERATOR_SCRIPT) $(GENERATOR_MODE) $(GENERATOR_NO_FORWARDING_TO_MOCK) $(GENERATOR_FILTER) $(GENERATOR_SOURCES) $(GENERATOR_OUTPUT_HEADER) $(GENERATOR_FILL_VTABLE) $< $(LINK_ERROR_LOG) $(GENERATED_FILES) $(CLANG_FLAGS) $(GENERATOR_FLAGS)
	$(LS) ./$(GENERATED_FILE_DIR)/*_Stub.hpp
	$(LS) ./$(GENERATED_FILE_DIR)/*_1.hpp
	$(LS) $(GENERATED_FILE_DIR)/mock_$(ORIGINAL_HEADER_BASENAME)_1.hpp
	$(GREP) mockgenSample $(OUTPUT_HEADER_FILENAME) | $(WC) | $(GREP) "  2  "
	$(GREP) $(GENERATOR_FILTERED_OUT_CLASSNAME) $(GENERATED_FILE_DIR)/mock_$(ORIGINAL_HEADER_BASENAME)_1.hpp | $(WC) | $(GREP) "  0  "

generate_var: $(ORIGINAL_HEADER)
	$(RUBY) $(GENERATOR_SCRIPT) var $(GENERATOR_NO_FORWARDING_TO_MOCK) $(GENERATOR_FILTER) $(GENERATOR_SOURCES) $(GENERATOR_OUTPUT_HEADER) $(GENERATOR_FILL_VTABLE) $< $(LINK_ERROR_LOG) $(GENERATED_FILES) $(CLANG_FLAGS) $(GENERATOR_FLAGS)

# Write stub functions in an link error log file
test_generate_c_func: clean_generated
	$(RUBY) $(GENERATOR_SCRIPT) stub $(GENERATOR_NO_FORWARDING_TO_MOCK) $(GENERATOR_FILTER) $(GENERATOR_INPUT_C_SOURCES) $(GENERATOR_OUTPUT_HEADER) $(GENERATOR_SPLIT_EACH_CLASS) $(ORIGINAL_HEADER) $(TESTED_LINK_ERROR_LOG) $(GENERATED_FILES) $(CLANG_FLAGS) $(GENERATOR_FLAGS)
	$(GREP) $(TESTED_MISSING_FUNCTION_NAME_PREFIX) $(GENERATED_FILE_DIR)/*Stub.cpp | $(WC) -l | $(GREP) "2"

# Create an all-in-one header and generate mocks from it
test_generate_all_in_one_header: clean_generated
	$(RUBY) $(GENERATOR_SCRIPT) $(GENERATOR_MODE) $(GENERATOR_NO_FORWARDING_TO_MOCK) $(GENERATOR_FILTER) $(GENERATOR_SOURCES) $(GENERATOR_OUTPUT_HEADER) $(GENERATOR_SPLIT_EACH_CLASS) $(GENERATOR_CHECK_INTERNAL_SYSTEM_PATH) $(OUTPUT_HEADER_FILENAME) $(LINK_ERROR_LOG) $(GENERATED_FILES) $(CLANG_FLAGS) $(GENERATOR_FLAGS)
	$(LS) ./$(GENERATED_FILE_DIR)/*_Stub.hpp
	$(LS) ./$(GENERATED_FILE_DIR)/*_IObject.cpp
	$(LS) ./$(GENERATED_FILE_DIR)/*_TopLevelClass.cpp
	$(GREP) mockgenSample $(OUTPUT_HEADER_FILENAME) | $(WC) | $(GREP) "  2  "

# Write a mock class in an output class named by the class name
test_generate_each: clean_generated
	$(RUBY) $(GENERATOR_SCRIPT) $(GENERATOR_MODE) $(GENERATOR_NO_FORWARDING_TO_MOCK) $(GENERATOR_FILTER) $(GENERATOR_SOURCES) $(GENERATOR_OUTPUT_HEADER) $(GENERATOR_SPLIT_EACH_CLASS) $(ORIGINAL_HEADER) $(LINK_ERROR_LOG) $(GENERATED_FILES) $(CLANG_FLAGS) $(GENERATOR_FLAGS)
	$(LS) ./$(GENERATED_FILE_DIR)/*_Stub.hpp
	$(LS) ./$(GENERATED_FILE_DIR)/*_IObject.cpp
	$(LS) ./$(GENERATED_FILE_DIR)/*_TopLevelClass.cpp

# Write a number of mock classes in an output class
test_generate_bulk: clean_generated
	$(RUBY) $(GENERATOR_SCRIPT) $(GENERATOR_MODE) $(GENERATOR_NO_FORWARDING_TO_MOCK) $(GENERATOR_FILTER) $(GENERATOR_SOURCES) $(GENERATOR_OUTPUT_HEADER) $(GENERATOR_SPLIT_BULK_CLASSES) $(ORIGINAL_HEADER) $(LINK_ERROR_LOG) $(GENERATED_FILES) $(CLANG_FLAGS) $(GENERATOR_FLAGS)
	$(LS) ./$(GENERATED_FILE_DIR)/*_Stub.hpp
	$(LS) ./$(GENERATED_FILE_DIR)/*_2.hpp

clean: clean_generated
	$(RM) $(TARGETS) $(GENERATED_FILES) $(LINK_ERROR_LOG) $(ALL_OBJS) $(DEPENDS) $(PROCESSED_CPPS) ./$(OBJ_DIR)/*.o ./$(OBJ_DIR)/*.d ./$(GENERATED_FILE_DIR)/*.hpp ./$(GENERATED_FILE_DIR)/*.cpp

clean_generated:
	$(RM) ./$(GENERATED_FILE_DIR)/*.hpp ./$(GENERATED_FILE_DIR)/*.cpp

rebuild: clean all

show:
	$(foreach v, $(ALL_UPDATED_VARIABLES), $(info $(v) = $($(v))))
	$(MAKE) -f $(MAKEFILE_SUB_COMPILE) show

showall:
	$(foreach v, $(.VARIABLES), $(info $(v) = $($(v))))
