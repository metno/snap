program drhook_ex3
implicit none
integer(4), parameter :: n = 150
real(8)    :: a(n)
integer(4) :: j
do j=1,n
  a(j) = j-1
enddo
do j=1,n
 call sub1(a,j)
enddo
end program drhook_ex3

subroutine sub1(a,n)
implicit none
integer(4), intent(in) :: n
real(8), intent(inout) :: a(n)
integer(4) j
do j=1,n
  if (mod(j,2) == 0) call sub2(a(j))
  a(j) = 2*a(j) + 1
enddo
end subroutine sub1

subroutine sub2(s)
implicit none
real(8), intent(inout) :: s
s = 1/(s+1)
end subroutine sub2
