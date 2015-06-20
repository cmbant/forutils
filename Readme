===================
ForUtils
===================
:ForUtils: Various fortran 2003/2008 utility classes.
:Version: 0.1
:Author: Antony Lewis
:Homepage: https://github.com/cmbant/forutils


Description
============

ForUtils is a package of Fortran classes and convenience functions for
Fortran 2003/2008 programs. The utilities comprise these areas:

* ArrayUtils - Find (minimal/maximal) index of an element in an array.
* FileUtils - Class for handling file access.
* IniObjects - Read/Write key/value style configuration files with array and
default value support.
* MatrixUtils - Read/Write matrices and interface to some BLAS/LAPACK routines.
* MiscUtils - Utility functions for optional arguments.
* MpiUtils - Wrappers for mpi-routines to compile without mpi library.
* ObjectLists - Lists of arbitrary objects including specializations for vectors.
* RandUtils - Some functions to generate random numbers.
* RangeUtils - Maintain sets of intervals.
* StringUtils - Utilities for strings, like concat of distinct types a.s.o.


Getting Started
================

Clone this git repository:

  $ git clone https://github.com/cmbant/forutils

Compile:

  $ make all


Dependencies
=============
* Fortran 2008 compatible compiler - E.g., ifort 14+, gfortran 5.2+ (current
trunk will do).
* MPI library - Only when you want the MpiUtils fully functional. Without an
MPI library MpiUtils compile, but the functions are merely no-ops and the
makefile target DebugMPI and ReleaseMPI can not be build.
