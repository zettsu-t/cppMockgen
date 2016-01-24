# Makefile of the mock generator for Google Mock
#
# To use clang++ instead of g++, set the environment variable CXX
# bash : export CXX=g++
# cmd  : set CXX=g++

# Include variables
THIS_DIR:=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))
include $(THIS_DIR)Makefile_vars

# Compile parallel in a sub makefile
MAKEFILE_SUB_COMPILE=$(THIS_DIR)Makefile_compile
MAKEFILE_PARALLEL=-j 5

ALL_UPDATED_VARIABLES+= THIS_DIR MAKEFILE_SUB_COMPILE MAKEFILE_PARALLEL

.PHONY: all check clean rebuild show showall FORCE

all: $(TARGETS)

check:
	$(RUBY) $(GENERATOR_SCRIPT_TEST)
	$(RUBY) $(PREPROCESS_SCRIPT_TEST) $(PREPROCESS_SCRIPT_TEST_FILES)
	$(TARGET_EXE)

$(TARGET_EXE): $(GENERATED_FILES) $(ALL_OBJS) $(PROCESSED_CPPS) FORCE
	$(MAKE) $(MAKEFILE_PARALLEL) -f $(MAKEFILE_SUB_COMPILE)
	$(eval GENERATED_SOURCES := $(notdir $(wildcard $(GENERATED_FILE_DIR)/*.cpp)))
	$(eval GENERATED_OBJS := $(addprefix $(OBJ_DIR)/, $(patsubst %.cpp, %.o, $(GENERATED_SOURCES))))
	$(CXX) $(LIBPATH) -o $@ $(ALL_OBJS) $(GENERATED_OBJS) $(LDFLAGS) $(LIBS)

# Define rules in the sub makefile
$(ALL_OBJS) : ;
$(PROCESSED_CPPS) : ;

$(GENERATED_FILES): $(ORIGINAL_HEADER) $(GENERATOR_SCRIPT) $(GENERATOR_SCRIPT_FILES)
	$(RUBY) $(GENERATOR_SCRIPT) $< $(GENERATED_FILES) $(CLANG_FLAGS)

clean:
	$(RM) $(TARGETS) $(GENERATED_FILES) $(ALL_OBJS) $(DEPENDS) $(PROCESSED_CPPS) ./$(OBJ_DIR)/*.o ./$(OBJ_DIR)/*.d ./$(GENERATED_FILE_DIR)/*.hpp ./$(GENERATED_FILE_DIR)/*.cpp

rebuild: clean all

show:
	$(foreach v, $(ALL_UPDATED_VARIABLES), $(info $(v) = $($(v))))
	$(MAKE) -f $(MAKEFILE_SUB_COMPILE) show

showall:
	$(foreach v, $(.VARIABLES), $(info $(v) = $($(v))))
