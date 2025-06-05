===================
ForUtils
===================
:ForUtils: Various fortran 2003/2008 utility classes.
:Version: 1.0
:Author: Antony Lewis
:Homepage: https://github.com/cmbant/forutils
:Code documentation: https://cosmologist.info/forutils/

.. contents:: Table of Contents
   :depth: 2

Description
============

ForUtils is a modern Fortran utility library that provides essential tools for
scientific computing and data processing. It offers Python-like convenience
functions and object-oriented interfaces for common programming tasks in
Fortran 2003/2008 programs.

**Key Features:**

* ArrayUtils - Find (minimal/maximal) index of an element in an array.
* FileUtils - Classes for handling file access, python-like loadtxt/savetxt functions
* IniObjects - Read/Write name=value configuration files with inheritance, array and default value support.
* MatrixUtils - Read/Write matrices and interface to some BLAS/LAPACK routines.
* MiscUtils - Utility functions for optional arguments.
* MpiUtils - Wrappers for MPI-routines to compile with(out) MPI library.
* ObjectLists - Lists of arbitrary objects including specializations for vectors.
* RandUtils - Some functions to generate random numbers.
* RangeUtils - Maintain sets of equally spaced intervals, e.g. for integration ranges.
* StringUtils - Utilities for strings, like concat of distinct types a.s.o.

For a class summary see the `class trees <https://cosmologist.info/forutils/classes/_index.html>`_.

Getting Started
================

Clone this git repository::

    $ git clone https://github.com/cmbant/forutils

Compile::

    $ make all

This generates the subdirectories Debug and Release and when an MPI library is
available also DebugMPI and ReleaseMPI. Each directory contains a libforutils.a
archive, which can be used directly for static linking on the compiler command
line by giving the absolute filename::

    forutils/<RTYPE>/libforutils.a

or by specifying it as a library::

    -Lforutils/<RTYPE> -lforutils

Specify one of Debug, DebugMPI, Release, or ReleaseMPI for <RTYPE>. The Debug
release types contain debug symbols and use no optimization, while the Release
types use a reasonable level of optimization.


Usage Examples
===============

File I/O Operations
-------------------

**Reading and writing text files:**

.. code-block:: fortran

    use FileUtils
    Type(TTextFile) :: F
    character(LEN=:), allocatable :: line

    ! Write to file
    call F%CreateFile('output.txt')
    call F%Write('#comment header')
    call F%Write('name = ', 3.04)
    call F%Write('test values')
    call F%Close()

    ! Read file line by line, skipping comments
    do while (F%ReadNextContentLine('output.txt', line))
        print *, 'Read: ', line
    end do

**Python-like loadtxt/savetxt functions:**

.. code-block:: fortran

    use FileUtils
    real(kind(1.d0)), allocatable :: matrix(:,:), vector(:)

    ! Load data from text file (like numpy.loadtxt)
    call File%LoadTxt('data.txt', matrix)
    call File%LoadTxt('vector.txt', vector)

    ! Save data to text file (like numpy.savetxt)
    call File%SaveTxt('output_matrix.txt', matrix)
    call File%SaveTxt('output_vector.txt', vector)

String Manipulation
-------------------

**String formatting and manipulation:**

.. code-block:: fortran

    use StringUtils
    character(LEN=:), allocatable :: result, joined

    ! Format strings (printf-style)
    result = FormatString('Test %d for %f %s', 91, 3.04, 'result')
    ! result = 'Test 91 for 3.0400 result'

    ! Join strings with separator
    joined = Join(',', 'apple', 'banana', 'cherry')
    ! joined = 'apple,banana,cherry'

    ! Case conversion
    print *, UpperCase('hello world')  ! 'HELLO WORLD'
    print *, LowerCase('HELLO WORLD')  ! 'hello world'

Object Lists
------------

**Working with string lists:**

.. code-block:: fortran

    use ObjectLists
    Type(TStringList) :: strings

    ! Add strings to list
    call strings%Add('here')
    call strings%Add('there')
    call strings%Add('alpha')

    ! Sort the list
    call strings%Sort()

    ! Access items
    print *, strings%Item(1)  ! 'alpha' (first after sorting)
    print *, strings%Item(3)  ! 'there'

    ! Find index of item
    print *, strings%IndexOf('here')  ! returns index or -1 if not found

    ! Key-value pairs
    call strings%Clear()
    call strings%Add('key1', 'value1')
    call strings%Add('key2', 'value2')
    print *, strings%ValueOf('key1')  ! 'value1'

**Working with numeric lists:**

.. code-block:: fortran

    use ObjectLists
    Type(TRealList) :: numbers
    Type(TRealArrayList) :: arrays
    integer :: i

    ! Add numbers to list
    call numbers%Add(0.5d0)
    call numbers%Add(-3.0d0)
    call numbers%Add(12.0d0)
    do i = 1, 10
        call numbers%Add(i * 1.0d0)
    end do

    ! Sort and access
    call numbers%Sort()
    print *, numbers%Item(1)  ! smallest value
    print *, numbers%AsArray()  ! convert to regular array

    ! Lists of arrays
    call arrays%Add([0.5d0])
    call arrays%Add([-3.0d0, 1.0d0])
    print *, arrays%Item(1,1)  ! first element of first array
    print *, arrays%Item(2,2)  ! second element of second array

Configuration Files
--------------------

**Reading INI-style configuration files:**

.. code-block:: fortran

    use IniObjects
    Type(TIniFile) :: ini
    real :: x
    double precision :: y
    character(LEN=:), allocatable :: path

    ! Read from file
    call ini%Open('config.ini')

    ! Read different types
    path = ini%Read_String('parameter')
    call ini%Read('x', x)
    call ini%Read('x', y)  ! automatic type conversion

    call ini%Close()

    ! Environment variable expansion
    ! If config contains: parameter = test$(PATH)/mypath
    ! It will expand $(PATH) with the actual PATH environment variable

Array Utilities
---------------

**Dynamic array reallocation:**

.. code-block:: fortran

    use ArrayUtils
    real, allocatable :: arr(:)
    integer, allocatable :: iarr(:)

    ! Initial allocation
    allocate(arr(3), source = [1.0, 2.0, 3.0])

    ! Reallocate (preserves existing data)
    call reallocate(arr, 5)  ! expand to 5 elements
    call reallocate(arr, 2)  ! shrink to 2 elements

    ! Works with different types
    call reallocate(iarr, 10)
    iarr(5) = 42
    call reallocate(iarr, 20)  ! iarr(5) still equals 42

Interpolation
-------------

**Cubic spline interpolation:**

.. code-block:: fortran

    use Interpolation
    Type(TCubicSpline) :: spline
    Type(TRegularCubicSpline) :: reg_spline
    real(kind(1.d0)), allocatable :: x(:), f(:)
    real(kind(1.d0)) :: test_x, interpolated_value, derivative
    integer :: i

    ! Prepare data points
    allocate(x(100), f(100))
    do i = 1, 100
        x(i) = 0.5367d0 * real(i, kind(1.d0)) + 0.3d0
        f(i) = (x(i)/47.2d0)**3 + (x(i)/5.5d0)**2 + x(i)*3.5d0 + 4.4579d0
    end do

    ! Initialize irregular spline
    call spline%Init(x, f)

    ! Initialize regular spline (evenly spaced x values)
    call reg_spline%Init(x(1), x(size(x)), 100, values=f)

    ! Interpolate at any point
    test_x = 13.2623d0
    interpolated_value = spline%Value(test_x)
    derivative = spline%Derivative(test_x)

    ! Interpolate arrays of values
    real(kind(1.d0)) :: test_points(3) = [7.3d0, 9.0d0, 34.34643d0]
    real(kind(1.d0)) :: results(3)
    call reg_spline%Array(test_points, results)

Advanced File Operations
------------------------

**Binary file I/O and object serialization:**

.. code-block:: fortran

    use FileUtils, ObjectLists
    Type(TBinaryFile) :: bf
    Type(TStringList) :: list

    ! Save object to binary file
    call list%Add('item1')
    call list%Add('item2')

    call bf%CreateFile('data.bin')
    call list%SaveBinary(bf%unit)
    call bf%Close()

    ! Load object from binary file
    call list%Clear()
    call bf%Open('data.bin')
    call list%ReadBinary(bf%unit)
    call bf%Close()

**File utility functions:**

.. code-block:: fortran

    use FileUtils
    logical :: exists
    character(LEN=:), allocatable :: path, name, ext

    ! Check file existence
    exists = File%Exists('myfile.txt')

    ! Extract file components
    path = File%ExtractPath('/home/user/data.txt')    ! '/home/user/'
    name = File%ExtractName('/home/user/data.txt')    ! 'data.txt'
    ext = File%ExtractExt('/home/user/data.txt')      ! '.txt'

    ! Get file information
    print *, File%Size('myfile.txt')     ! file size in bytes
    print *, File%TxtFileLines('data.txt')   ! number of lines
    print *, File%TxtFileColumns('data.txt') ! number of columns

Testing Your Code
==================

ForUtils includes comprehensive unit tests. To run them::

    $ cd tests
    $ make
    $ ./run_tests.sh

The tests demonstrate practical usage patterns and can serve as additional examples for learning the library.

Quick Reference
================

**Most commonly used modules and types:**

* **FileUtils**: ``TTextFile``, ``TBinaryFile``, ``File%LoadTxt()``, ``File%SaveTxt()``
* **StringUtils**: ``FormatString()``, ``Join()``, ``UpperCase()``, ``LowerCase()``
* **ObjectLists**: ``TStringList``, ``TRealList``, ``TRealArrayList``
* **IniObjects**: ``TIniFile`` for configuration files
* **ArrayUtils**: ``reallocate()`` for dynamic arrays
* **Interpolation**: ``TCubicSpline``, ``TRegularCubicSpline``

**Basic usage pattern:**

.. code-block:: fortran

    program example
        use FileUtils
        use StringUtils
        use ObjectLists
        implicit none

        Type(TTextFile) :: file
        Type(TStringList) :: items
        character(LEN=:), allocatable :: formatted

        ! Create and write to file
        call file%CreateFile('example.txt')
        call file%Write('# Example data file')
        call file%Write(FormatString('Value: %f', 3.14159))
        call file%Close()

        ! Read and process
        call items%AddFromFile('example.txt', nodups=.false.)
        call items%WriteItems()  ! print all items

    end program example

Dependencies
=============
* Fortran 2008 compatible compiler - E.g., ifort 14+, gfortran 6 or higher.
* MPI library - Only when you want the MpiUtils fully functional. Without an MPI library MpiUtils compile, but the functions are merely no-ops and the makefile target DebugMPI and ReleaseMPI can not be built.
