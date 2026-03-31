module gaussian_smoothingML

  implicit none
  private

  integer, save :: kernel_size = 3
  integer, save :: start_end_ = 1 ! kernel_size / 2
  integer, save :: max_age_hr = 24
  logical, save :: use_gaussian_smoothing = .true.
  real, save :: max_sigma = 1.5 ! kernel_size / 2.
  real, parameter :: min_sigma = 0.1

  public :: build_age_gaussian_kernel, add_to_field_with_kernel, initialize_gaussian_smoothing

  interface add_to_field_with_kernel
    module procedure add_to_field_with_kernel_r8
    module procedure add_to_field_with_kernel_r4
  end interface

contains

subroutine initialize_gaussian_smoothing(kernel_size_in, max_age_hr_in)
  !> Initialize parameters for Gaussian smoothing. sigma will asymtotically approach
  !> max_sigma as age approaches 5*max_age_hr_in. max_sigma will be kernel_size - 1
  !> @param kernel_size_in Size of the Gaussian kernel (must be odd)
  !> @param max_age_hr_in Maximum age in hours for which the kernel will be built
  integer, intent(in) :: kernel_size_in, max_age_hr_in
  if (mod(kernel_size_in, 2) == 0) then
    write(*,*) "Error: kernel_size must be odd. Given: ", kernel_size_in
    stop
  end if
  if (kernel_size_in == 1) then
    use_gaussian_smoothing = .false.
    return
  end if
  kernel_size = kernel_size_in
  start_end_ = int(kernel_size / 2.0)
  max_sigma = real(kernel_size) / 2.0
  max_age_hr = max_age_hr_in
end subroutine

function gaussian_cdf(mu, sigma, x)
  real, intent(in) :: mu, sigma, x
  real :: gaussian_cdf
  gaussian_cdf = 0.5 * (1 + erf((x - mu) / (sigma * sqrt(2.0))))
end function gaussian_cdf

!> Function to compute the 2D Gaussian area using the erf function
real function gaussian_2d_area(a_x, b_x, a_y, b_y, mu_x, mu_y, sigma_x, sigma_y)
    real, intent(in) :: a_x, b_x, a_y, b_y
    real, intent(in) :: mu_x, mu_y, sigma_x, sigma_y
    real :: cdf_x1, cdf_x2, cdf_y1, cdf_y2

    ! Compute the 1D CDF values in the x-direction
    cdf_x1 = gaussian_cdf(mu_x, sigma_x, a_x)
    cdf_x2 = gaussian_cdf(mu_x, sigma_x, b_x)

    ! Compute the 1D CDF values in the y-direction
    cdf_y1 = gaussian_cdf(mu_y, sigma_y, a_y)
    cdf_y2 = gaussian_cdf(mu_y, sigma_y, b_y)

    ! Compute the area as the product of the CDF differences
    gaussian_2d_area = (cdf_x2 - cdf_x1) * (cdf_y2 - cdf_y1)
end function gaussian_2d_area


subroutine build_age_gaussian_kernel(age_hr, kernel)
  integer, intent(in) :: age_hr
  real, allocatable,intent(out) :: kernel(:,:)

  integer :: ii, jj
  real :: sigma, sum_kernel, kernel_max
  real :: a_x, b_x, a_y, b_y, area
  integer :: stat
  character(len=100) :: msg
  real, parameter :: mu = 0.0

  if (.not. use_gaussian_smoothing) then
    return
  end if

  allocate(kernel(kernel_size, kernel_size), stat=stat)
  if (stat /= 0)  then
    write(msg,'(A,I0,A,I0)') "Error allocating kernel of size ", kernel_size, "x", kernel_size
    stop trim(msg)
  end if
  sigma = min_sigma + (max_sigma - min_sigma) * (1 - exp(-1. * age_hr / max_age_hr))

  sum_kernel = 0.0

  kernel_max = gaussian_2d_area(-0.5, 0.5, -0.5, 0.5, mu, mu, sigma, sigma)
  do jj = -1*start_end_, start_end_
    a_y = real(jj) - 0.5
    b_y = real(jj) + 0.5
    do ii = -1*start_end_, start_end_
      a_x = real(ii) - 0.5
      b_x = real(ii) + 0.5
      area = gaussian_2d_area(a_x, b_x, a_y, b_y, mu, mu, sigma, sigma)
      if (10*area > kernel_max) then
        ! avoid dithering effects by setting very small values to zero
        kernel(ii+start_end_+1, jj+start_end_+1) = area
        sum_kernel = sum_kernel + area
      else
        kernel(ii+start_end_+1, jj+start_end_+1) = 0.0
      end if
    end do
  end do
  kernel = kernel / sum_kernel
end subroutine

subroutine add_to_field_with_kernel_r8(field, i, j, value, kernel, lost_activity)
  use iso_fortran_env, only: real64
  real(kind=real64), intent(inout) :: field(:,:)
  integer, intent(in) :: i, j
  real, intent(in) :: value
  real, allocatable, intent(in) :: kernel(:,:)
  real, intent(out) :: lost_activity

  integer :: ii, jj
  integer :: ig, jg
  integer :: i_min, i_max, j_min, j_max

  lost_activity = 0.0

  if (.not. use_gaussian_smoothing) then
    !$OMP atomic
    field(i,j) = field(i,j) + dble(value)
    return
  end if

  i_min = lbound(field, 1)
  i_max = ubound(field, 1)
  j_min = lbound(field, 2)
  j_max = ubound(field, 2)

  do jj = -1*start_end_, start_end_
    jg = j + jj
    do ii = -1*start_end_, start_end_
      ig = i + ii
      if (kernel(ii+start_end_+1, jj+start_end_+1) > 0.0) then
        ! only update the field if the kernel value is non-zero to avoid unnecessary atomic operations
        if (ig < i_min .or. ig > i_max .or. jg < j_min .or. jg > j_max) then
          lost_activity = lost_activity + value * kernel(ii+start_end_+1, jj+start_end_+1)
        else
          !$OMP atomic
          field(ig,jg) = field(ig,jg) + dble(value * kernel(ii+start_end_+1, jj+start_end_+1))
        end if
      end if
    end do
  end do
end subroutine

subroutine add_to_field_with_kernel_r4(field, i, j, value, kernel, lost_activity)
  use iso_fortran_env, only: real32
  real(kind=real32), intent(inout) :: field(:,:)
  integer, intent(in) :: i, j
  real, intent(in) :: value
  real, allocatable, intent(in) :: kernel(:,:)
  real, intent(out) :: lost_activity

  integer :: ii, jj
  integer :: ig, jg
  integer :: i_min, i_max, j_min, j_max

  lost_activity = 0.0

  if (.not. use_gaussian_smoothing) then
    !$OMP atomic
    field(i,j) = field(i,j) + real(value, real32)
    return
  end if

  i_min = lbound(field, 1)
  i_max = ubound(field, 1)
  j_min = lbound(field, 2)
  j_max = ubound(field, 2)

  do jj = -1*start_end_, start_end_
    jg = j + jj
    do ii = -1*start_end_, start_end_
      ig = i + ii
      if (kernel(ii+start_end_+1, jj+start_end_+1) > 0.0) then
        ! only update the field if the kernel value is non-zero to avoid unnecessary atomic operations
        if (ig < i_min .or. ig > i_max .or. jg < j_min .or. jg > j_max) then
          lost_activity = lost_activity + value * kernel(ii+start_end_+1, jj+start_end_+1)
        else
          !$OMP atomic
          field(ig,jg) = field(ig,jg) + real(value * kernel(ii+start_end_+1, jj+start_end_+1), real32)
        end if
      end if
    end do
  end do
end subroutine

end module gaussian_smoothingML
