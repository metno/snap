program drhook_ex1
implicit none
integer(4), parameter :: n = 100
real(8)    :: a(n)
integer(4) :: j
do j=1,n
  a(j) = j-1
enddo
call sub1(a,n)
end program drhook_ex1

subroutine sub1(a,n)
implicit none
integer(4), intent(in) :: n
real(8), intent(inout) :: a(n)
if (n > 0) call sub2(a(1))
end subroutine sub1

subroutine sub2(s)
implicit none
real(8), intent(inout) :: s
s = 1/s ! divide by zero
end subroutine sub2
