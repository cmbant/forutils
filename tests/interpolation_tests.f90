    module InterpolationTests
    use Interpolation
    use FileUtils
    use FileTests
    implicit None
    integer, parameter, private :: sp= KIND(1.d0)

    contains

    function func(x)
    real(sp) func,x

    func = (x/47.2_sp)**3  + (x/5.5_sp)**2 +x*3.5_sp+4.4579_sp
    end function

    function funcderiv(x)
    real(sp) funcderiv,x

    funcderiv = 3*(x/47.2_sp)**2/47.2_sp  + 2*(x/5.5_sp)/5.5_sp +3.5_sp
    end function

    function gridfunc(x, y) result(z)
    real(sp), intent(in) :: x, y
    real(sp) :: z

    z = 1.25_sp - 0.7_sp*x + 0.4_sp*y + 0.11_sp*x**2 - 0.05_sp*y**2 &
        + 0.03_sp*x*y + 0.015_sp*x**2*y - 0.02_sp*x*y**2 + 0.004_sp*x**3 &
        - 0.003_sp*y**3 + 0.002_sp*x**2*y**2
    end function

    logical function is_regular_grid_node(x, xmin, delta_x, n)
    real(sp), intent(in) :: x, xmin, delta_x
    integer, intent(in) :: n
    real(sp) :: grid_index
    integer :: nearest

    grid_index = (x - xmin)/delta_x
    nearest = nint(grid_index)
    is_regular_grid_node = nearest >= 0 .and. nearest <= n - 1 &
        .and. abs(grid_index - real(nearest, sp)) < 1.e-10_sp
    end function

    logical function is_array_node(x, nodes)
    real(sp), intent(in) :: x, nodes(:)

    is_array_node = any(abs(x - nodes) < 1.e-10_sp)
    end function

    function RunInterpGrid2DTest() result(fails)
    integer, parameter :: nx = 6, ny = 5, nip = 60
    real(sp), parameter :: x(nx) = [-2.0_sp, -0.7_sp, 0.1_sp, 1.4_sp, 2.8_sp, 4.6_sp]
    real(sp), parameter :: y(ny) = [-1.3_sp, -0.2_sp, 0.9_sp, 2.7_sp, 4.0_sp]
    real(sp) :: z(nx, ny), xi(nip), yi(nip), zi(nip), expected(nip), value, target
    integer :: ix, iy, i, error
    integer :: fails
    Type(TInterpGrid2D) :: Grid

    fails = 0
    do iy = 1, ny
        do ix = 1, nx
            z(ix, iy) = gridfunc(x(ix), y(iy))
        end do
    end do

    call Grid%Init(x, y, z)

    if (is_array_node(0.75_sp, x) .or. is_array_node(1.8_sp, y)) then
        fails = fails + 1
        print *, "TInterpGrid2D value test point is on a grid node"
    end if

    value = Grid%Value(0.75_sp, 1.8_sp)
    target = gridfunc(0.75_sp, 1.8_sp)
    if (abs(value - target) < 1e-8_sp) then
        print *, "TInterpGrid2D value OK"
    else
        fails = fails + 1
        print *, "TInterpGrid2D value error"
        print *, value, target
    end if

    do i = 1, nip
        xi(i) = x(1) + (x(nx) - x(1))*real(i, sp)/real(nip + 1, sp)
        yi(i) = y(1) + (y(ny) - y(1))*real(mod(7*i, nip) + 1, sp)/real(nip + 1, sp)
        expected(i) = gridfunc(xi(i), yi(i))
    end do
    if (any([(is_array_node(xi(i), x) .or. is_array_node(yi(i), y), i=1,nip)])) then
        fails = fails + 1
        print *, "TInterpGrid2D array test point is on a grid node"
    end if
    error = 0
    call Grid%Values(nip, xi, yi, zi, error)
    if (error == 0 .and. all(abs(zi - expected) < 1e-8_sp)) then
        print *, "TInterpGrid2D array values OK"
    else
        fails = fails + 1
        print *, "TInterpGrid2D array value error"
        print *, error, maxval(abs(zi - expected))
    end if

    call Grid%Clear()

    end function

    function RunInterpolationTests() result(fails)
    real(sp), allocatable :: x(:),f(:)
    integer i
    integer fails
    Type(TCubicSpline) :: Irreg, IrregLoad
    Type(TRegularCubicSpline) :: Reg, RegLoad
    Type(TLogRegularCubicSpline) :: Reglog
    real(sp) xx
    real(sP), parameter:: testval = 13.2623_sp
    real(sP), parameter:: testarrayval(3) = [7.3_sp, 9._sp,34.34643_sp]
    real(sp) :: outarray(3), funcarray(3)
    real(sp) :: rangeout(2:20), rangefunc(2:20)
    Type(TBinaryFile) :: FB

    fails = 0
    allocate(x(100),f(100))
    do i=1, 100
        x(i) = 0.5367_sp*real(i,sp) + 0.3_sp
        f(i) = func(x(i))
    end do
    call Irreg%Init(x,f)
    call Reg%Init(x(1),x(size(x)),100,values=f)
    call RegLog%init(x(1),x(size(x)),100)
    do i=1, 100
        xx= exp(log(x(1)) + RegLog%delta_x*(i-1))
        RegLog%F(i) = func(xx)
    end do

    if (is_regular_grid_node(testval, Reg%xmin_interp, Reg%delta_x, Reg%n) .or. &
        is_regular_grid_node(log(testval), RegLog%xmin_interp, RegLog%delta_x, RegLog%n)) then
        fails = fails+1
        print *, 'interpolation test point is on a spline node'
    end if

    if (all(abs([Reg%Value(testval), Irreg%Value(testval), RegLog%Value(testval)]-func(testval))<1e-5)) then
        print *,'Value OK'
    else
        fails = fails+1
        print *, 'error'
        print *, Reg%Value(testval), Irreg%Value(testval),RegLog%Value(testval), func(testval)
    end if

    if (all(abs([Reg%Value(x(1)), Irreg%Value(x(1)), RegLog%Value(x(1))]-func(x(1)))<1e-5)) then
        print *,'bottom end value OK'
    else
        fails = fails+1
        print *, 'end error'
        print *, Reg%Value(testval), Irreg%Value(testval),RegLog%Value(testval), func(testval)
    end if

    if (all(abs([Reg%Value(x(100)), Irreg%Value(x(100)), RegLog%Value(x(100))]-func(x(100)))<1e-5)) then
        print *,'top end value OK'
    else
        fails = fails+1
        print *, 'end error'
        print *, Reg%Value(testval), Irreg%Value(testval),RegLog%Value(testval), func(testval)
    end if

    if (all(abs([Reg%Derivative(testval), Irreg%Derivative(testval), RegLog%Derivative(testval)]-funcderiv(testval))<1e-5)) then
        print *,'Derivative OK'
    else
        fails = fails+1
        print *, 'derivative error'
        print *, Reg%Derivative(testval), Irreg%Derivative(testval),RegLog%Derivative(testval), funcderiv(testval)
    end if

    if (any([(is_regular_grid_node(testarrayval(i), Reg%xmin_interp, Reg%delta_x, Reg%n), i=1,3)]) .or. &
        any([(is_regular_grid_node(log(testarrayval(i)), RegLog%xmin_interp, RegLog%delta_x, RegLog%n), i=1,3)])) then
        fails = fails+1
        print *, 'array interpolation test point is on a spline node'
    end if

    do i=1,3
        funcarray(i) = func(testarrayval(i))
    end do
    call Reg%Array(testarrayval, outarray)
    if (all(abs(outarray-funcarray)<1e-7)) then
        print *, 'array value OK'
    else
        fails = fails+1
        print *, 'array error'
        print *, outarray
        print *, funcarray
    end if

    do i=1,3
        funcarray(i) = RegLog%Value(testarrayval(i))
    end do
    call RegLog%Array(testarrayval, outarray)
    if (all(abs(outarray-funcarray)<1e-8)) then
        print *, 'log array value OK'
    else
        fails = fails+1
        print *, 'log array error'
        print *, outarray
        print *, funcarray
    end if

    if (any([(is_regular_grid_node(log(real(i, sp)), RegLog%xmin_interp, RegLog%delta_x, RegLog%n), i=2,20)])) then
        fails = fails+1
        print *, 'log integer range test point is on a spline node'
    end if

    do i=2,20
        rangefunc(i) = RegLog%Value(real(i,sp))
    end do
    call RegLog%Array(2, 20, rangeout)
    if (all(abs(rangeout-rangefunc)<1e-8)) then
        print *, 'log integer range value OK'
    else
        fails = fails+1
        print *, 'log integer range error'
        print *, maxval(abs(rangeout-rangefunc))
    end if

    call FB%CreateFile(temp_file)
    call Irreg%SaveState(FB)
    call FB%Close()
    call FB%Open(temp_file)
    call IrregLoad%loadState(FB)
    call FB%Close()
    if (abs( IrregLoad%Value(testval) -Irreg%Value(testval))<1e-8) then
        print *,'Load bin OK'
    else
        fails = fails+1
        print *, 'Load bin error'
    end if
    call FB%CreateFile(temp_file)
    call Reg%SaveState(FB)
    call FB%Close()
    call FB%Open(temp_file)
    call RegLoad%loadState(FB)
    call FB%Close()
    if (abs( RegLoad%Value(testval) -Reg%Value(testval))<1e-8) then
        print *,'Load bin2 OK'
    else
        fails = fails+1
        print *, 'Load bin2 error'
    end if
    call File%Delete(temp_file)

    fails = fails + RunInterpGrid2DTest()

    end function RunInterpolationTests

    end module InterpolationTests
