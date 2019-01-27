
all: default

default: Release Debug

include ../Makefile_compiler

export

Debug: libforutils
	@$(MAKE) tester_Debug OUTPUT=Debug F90FLAGS="$(F90DEBUGFLAGS) $(F90COMMONFLAGS)" $(MAKEOPT)

Release: libforutils
	@$(MAKE) tester_Release OUTPUT=Release F90FLAGS="$(F90RELEASEFLAGS) $(F90COMMONFLAGS)" $(MAKEOPT)

OUTPUT_DIR=../$(OUTPUT)

libforutils:
	@$(MAKE) -C '..' $(MAKEOPT) $(OUTPUT)


FILES = array_tests ObjectLists_tests interpolation_tests inifile_tests \
        file_tests string_tests ranges_tests run_tests

OBJS = $(patsubst %,$(OUTPUT_DIR)/%.o,$(FILES))

tester_$(OUTPUT): $(OUTPUT_DIR)/libforutils.a $(OBJS)
	$(F90C) $(F90FLAGS) $(OBJS) -o $@ -L$(OUTPUT_DIR) -lforutils

$(OUTPUT_DIR)/%.o: %.f90
	$(F90C) $(F90FLAGS) $(MODLINK)$(OUTPUT_DIR) -c $< -o $(OUTPUT_DIR)/$*.o

clean:
	@$(MAKE) -C '..' $(MAKEOPT) clean

.PHONY: libforutils Debug Release clean
