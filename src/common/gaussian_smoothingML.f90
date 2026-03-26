module gaussian_smoothingML

  implicit none
  private

  public :: build_age_gaussian_kernel_3x3, add_to_field_with_kernel

contains

pure subroutine build_age_gaussian_kernel_3x3(age_hr, kernel)
  integer, intent(in) :: age_hr
  real, intent(out) :: kernel(3,3)

  integer :: ii, jj
  real :: sigma, sum_kernel

  sigma = 0.1 + 0.9 * (1 - exp(-age_hr / 24.0))

  sum_kernel = 0.0
  do jj = -1, 1
    do ii = -1, 1
      kernel(ii+2, jj+2) = exp(-0.5 * ((ii/sigma)**2 + (jj/sigma)**2))
      sum_kernel = sum_kernel + kernel(ii+2, jj+2)
    end do
  end do
  kernel = kernel / sum_kernel
end subroutine

subroutine add_to_field_with_kernel(field, i, j, value, kernel, lost_activity)
  use iso_fortran_env, only: real64
  real(kind=real64), intent(inout) :: field(:,:)
  integer, intent(in) :: i, j
  real, intent(in) :: value
  real, intent(in) :: kernel(3,3)
  real, intent(out) :: lost_activity

  integer :: ii, jj
  integer :: ig, jg
  integer :: i_min, i_max, j_min, j_max

  lost_activity = 0.0
  i_min = lbound(field, 1)
  i_max = ubound(field, 1)
  j_min = lbound(field, 2)
  j_max = ubound(field, 2)

  do jj = -1, 1
    jg = j + jj
    do ii = -1, 1
      ig = i + ii
      if (ig < i_min .or. ig > i_max .or. jg < j_min .or. jg > j_max) then
        lost_activity = lost_activity + value * kernel(ii+2, jj+2)
      else
        !$OMP atomic
        field(ig,jg) = field(ig,jg) + dble(value * kernel(ii+2, jj+2))
      end if
    end do
  end do
end subroutine

end module gaussian_smoothingML
