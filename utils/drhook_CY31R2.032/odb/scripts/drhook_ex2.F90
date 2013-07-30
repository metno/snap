program drhook_ex2
use yomwatch
use mpl_arg_mod ! tests also mpl_arg_mod -module
implicit none
integer(4) :: n, numargs
real(8), allocatable :: a(:)
integer(4) :: j
character(len=256) a_out
character(len=20) cn
numargs = mpl_iargc()
call getarg(0,a_out)
write(0,*) 'a_out = "'//trim(a_out)//'"'
write(0,*) 'numargs = ',numargs
if (numargs > 0) then
  call getarg(1,cn)
  write(0,*) 'cn = "'//trim(cn)//'"'
  read(cn,'(i20)') n
else
  n = 100
endif
write(0,*) 'n = ',n
allocate(a(n))
do j=1,n
  a(j) = j-1
enddo
! Watch & fail when A gets altered
call dr_hook_watch('MAIN: array A(N)',a,LDABORT=.TRUE.)
call sub1(a,n)
deallocate(a)
end program drhook_ex2

subroutine sub1(a,n)
implicit none
integer(4), intent(in) :: n
real(8), intent(inout) :: a(n)
if (n > 0) call sub2(a(1))
end subroutine sub1

subroutine sub2(s)
implicit none
real(8), intent(inout) :: s
!s = 1/s ! divide by zero removed
s = 1 ! a(1) has now been altered --> watch point should detect this
end subroutine sub2
