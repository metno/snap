module gaussian_smoothingML

  implicit none
  private

  integer, save :: kernel_size = 1 ! must be uneven, kernel_size=1 disables smoothing
  integer, save :: start_end_ = 1 ! kernel_size / 2
  integer, save :: max_age_hr = 24
  logical, save :: use_gaussian_smoothing = .false.
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
  use_gaussian_smoothing = .true.
  kernel_size = kernel_size_in
  start_end_ = int(kernel_size / 2.0)
  max_sigma = real(kernel_size) / 2.0
  max_age_hr = max_age_hr_in
end subroutine

subroutine build_age_gaussian_kernel(age_hr, kernel)
  !> age in hours of the particle
  integer, intent(in) :: age_hr
  !> gaussian kernel. Will be allocated if it is not already allocated. Must be of size kernel_size x kernel_size
  real, allocatable,intent(inout) :: kernel(:,:)

  integer :: ii, jj, k
  real :: sigma, sum_kernel, kernel_max
  real :: dcdf_x, dcdf_y, area
  real :: cdf_vals(0:kernel_size)
  integer :: stat
  character(len=100) :: msg
  real, parameter :: sqrt2 = sqrt(2.0)

  if (.not. use_gaussian_smoothing) then
    return
  end if

  if (.not. allocated(kernel)) then
    allocate(kernel(kernel_size, kernel_size), stat=stat)
    if (stat /= 0)  then
      write(msg,'(A,I0,A,I0)') "Error allocating kernel of size ", kernel_size, "x", kernel_size
      stop trim(msg)
    end if
  else if (size(kernel,1) /= kernel_size .or. size(kernel,2) /= kernel_size) then
    deallocate(kernel)
  end if

  sigma = min_sigma + (max_sigma - min_sigma) * (1 - exp(-1. * age_hr / max_age_hr))

  ! Precompute CDF at each cell boundary: position(k) = k - start_end_ - 0.5
  do k = 0, kernel_size
    cdf_vals(k) = 0.5 * (1.0 + erf((real(k - start_end_) - 0.5) / (sigma * sqrt2)))
  end do

  sum_kernel = 0.0
  kernel_max = (cdf_vals(start_end_+1) - cdf_vals(start_end_))**2

  do jj = -1*start_end_, start_end_
    dcdf_y = cdf_vals(jj+start_end_+1) - cdf_vals(jj+start_end_)
    do ii = -1*start_end_, start_end_
      dcdf_x = cdf_vals(ii+start_end_+1) - cdf_vals(ii+start_end_)
      area = dcdf_x * dcdf_y
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
