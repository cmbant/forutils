
all: default

default: Release Debug

include ../Makefile_compiler


Debug: 
	$(MAKE) tester OUTPUT=Debug F90FLAGS="$(F90DEBUGFLAGS) $(F90COMMONFLAGS)" 

Release: 
	$(MAKE) tester OUTPUT=Release F90FLAGS="$(F90RELEASEFLAGS) $(F90COMMONFLAGS)"

OUTPUT_DIR=../$(OUTPUT)

libforutils:
	$(MAKE) -C '..' $(OUTPUT)

FILES = array_tests ObjectLists_tests interpolation_tests inifile_tests \
        file_tests string_tests run_tests

OBJS = $(patsubst %,$(OUTPUT_DIR)/%.o,$(FILES))


tester: libforutils $(OBJS)
	$(F90C) $(F90FLAGS) $(OBJS) -o $@_$(OUTPUT)  -L$(OUTPUT_DIR) -lforutils

$(OUTPUT_DIR)/%.o: %.f90
	$(F90C) $(F90FLAGS) $(MODLINK)$(OUTPUT_DIR) -c $< -o $(OUTPUT_DIR)/$*.o

clean:
	$(MAKE) -C '..' clean
