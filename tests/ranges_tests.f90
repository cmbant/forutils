    module RangesTests
    use RangeUtils
    implicit none
    contains


    function RunRangesTests() result (fails)
    integer fails
    Type(TRanges) R

    fails = 0
    !Combine set of ranges with specified approx maximum spacing in each
    call R%Add(1.d0,5.d0,4) !1, 2,3, 4, 5
    call R%Add_delta(0.5d0, 2.d0,0.5d0)
    call R%Add_delta(2.d0, 4.d0, 1.1d0) !Should be ignored as wider spacing
    call R%GetArray()
    if (any(R%points /= [0.5d0, 1.d0, 1.5d0, 2.d0, 3.d0, 4.d0, 5.d0])) then
        fails = fails + 1
        print *, 'Error in Ranges', R%Points
    end if
    !Check very close points merged
    call R%Add_delta(1.39999d0, 1.800001d0, 0.1d0)
    call R%GetArray()
    if (any(R%points==1.5d0)) then
        fails=fails+1
        print *, 'Error in ranges merge', R%points
    end if
    call R%Free()
    call R%Add(1d-3, 10.d0, 4, isLog=.true.)
    call R%GetArray()
    call R%Add_delta(1.d0, 15.d0, 1.d0)
    call R%GetArray()
    if (R%points(5)/=2.d0 .or. abs(R%Points(2)-0.01d0)>1e-5) then
        fails = fails + 1
        print *, 'Error in log range', R%Points
    end if

    if (fails==0) print *, 'Ranges OK'
    end function

    end module
