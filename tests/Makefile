
OUTPUT = Debug
OUTPUT_DIR=../$(OUTPUT)

default: tester

libforutils:
	$(MAKE) -C '..' $(OUTPUT)

FILES = ObjectLists_tests.o interpolation_tests.o inifile_tests.o run_tests.o


F90C = gfortran
F90FLAGS = -cpp -MMD -J$(OUTPUT_DIR) -fmax-errors=4 -fopenmp -fbacktrace


tester: libforutils $(FILES)
	$(F90C) $(F90FLAGS) $(FILES) -o $@  -L$(OUTPUT_DIR) -lforutils

%.o: %.f90
	$(F90C) $(F90FLAGS) -c $< -o $*.o

clean:
	-rm -f *.o *.a *.d core *.mod *.optrpt
