
    program tester
    use ListTests
    use InterpolationTests
    use IniTests
    use FileTests
    implicit none

    integer :: fails =0

    !If anything fails it is quite likely to be a compiler bug
    !Over the years many compiler bugs have appeared.
    fails = fails + RunListTests()
    fails = fails + RunInterpolationTests()
    fails = fails + RunIniTests()
    fails = fails + RunFileTests()
    
    print *, 'Total fails: ', fails
    if (fails>0) error stop 'More than one test failed'

    end program
