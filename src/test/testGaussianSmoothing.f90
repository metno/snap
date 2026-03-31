program testGaussianSmoothing
  use gaussian_smoothingML, only: initialize_gaussian_smoothing, build_age_gaussian_kernel, &
                                   add_to_field_with_kernel
  use iso_fortran_env, only: real32, real64
  implicit none

  real, allocatable :: kernel(:,:)
  real :: kernel_sum, lost_activity
  real(kind=real64), allocatable :: field_r8(:,:)
  real(kind=real32), allocatable :: field_r4(:,:)
  real :: centre_young

  integer :: i, j
  real :: tol

  tol = 1e-5

  ! -----------------------------------------------------------------------
  ! Test 1: kernel_size=1 disables smoothing
  ! -----------------------------------------------------------------------
  call initialize_gaussian_smoothing(kernel_size_in=1, max_age_hr_in=48)
  call build_age_gaussian_kernel(24, kernel)
  if (allocated(kernel)) then
    print *, "FAIL test 1: kernel should not be allocated when smoothing is off"
    stop 1
  end if
  print *, "PASS test 1: kernel_size=1 disables smoothing"

  ! -----------------------------------------------------------------------
  ! Test 2: kernel sums to 1 for kernel_size=3
  ! -----------------------------------------------------------------------
  call initialize_gaussian_smoothing(kernel_size_in=3, max_age_hr_in=48)
  call build_age_gaussian_kernel(48, kernel)
  if (.not. allocated(kernel)) then
    print *, "FAIL test 2: kernel not allocated"
    stop 1
  end if
  if (size(kernel, 1) /= 3 .or. size(kernel, 2) /= 3) then
    print *, "FAIL test 2: expected 3x3 kernel, got ", size(kernel,1), "x", size(kernel,2)
    stop 1
  end if
  kernel_sum = sum(kernel)
  if (abs(kernel_sum - 1.0) > tol) then
    print *, "FAIL test 2: kernel sum = ", kernel_sum, ", expected 1.0"
    stop 1
  end if
  print *, "PASS test 2: 3x3 kernel sums to 1, sum =", kernel_sum

  ! -----------------------------------------------------------------------
  ! Test 3: kernel sums to 1 for kernel_size=5
  ! -----------------------------------------------------------------------
  deallocate(kernel)
  call initialize_gaussian_smoothing(kernel_size_in=5, max_age_hr_in=24)
  call build_age_gaussian_kernel(24, kernel)
  if (size(kernel, 1) /= 5 .or. size(kernel, 2) /= 5) then
    print *, "FAIL test 3: expected 5x5 kernel, got ", size(kernel,1), "x", size(kernel,2)
    stop 1
  end if
  kernel_sum = sum(kernel)
  if (abs(kernel_sum - 1.0) > tol) then
    print *, "FAIL test 3: kernel sum = ", kernel_sum, ", expected 1.0"
    stop 1
  end if
  print *, "PASS test 3: 5x5 kernel sums to 1, sum =", kernel_sum

  ! -----------------------------------------------------------------------
  ! Test 4: young particle (age=0) has most weight at centre cell
  ! -----------------------------------------------------------------------
  deallocate(kernel)
  call initialize_gaussian_smoothing(kernel_size_in=3, max_age_hr_in=48)
  call build_age_gaussian_kernel(0, kernel)
  if (kernel(2,2) < kernel(1,1) .or. kernel(2,2) < kernel(1,2) .or. &
      kernel(2,2) < kernel(2,1)) then
    print *, "FAIL test 4: centre cell is not the maximum at age=0"
    stop 1
  end if
  print *, "PASS test 4: centre cell has maximum weight at age=0, centre =", kernel(2,2)

  ! -----------------------------------------------------------------------
  ! Test 5: older age gives more spread (centre weight decreases with age)
  ! -----------------------------------------------------------------------
  deallocate(kernel)
  call initialize_gaussian_smoothing(kernel_size_in=3, max_age_hr_in=48)
  call build_age_gaussian_kernel(6, kernel)
  centre_young = kernel(2,2)

  deallocate(kernel)
  call build_age_gaussian_kernel(48, kernel)
  if (kernel(2,2) >= centre_young) then
    print *, "FAIL test 5: centre weight should decrease as age increases"
    print *, "  centre(age=6)=", centre_young, "  centre(age=48)=", kernel(2,2)
    stop 1
  end if
  print *, "PASS test 5: centre weight decreases with age: age6=", centre_young, &
    " age48=", kernel(2,2)

  ! -----------------------------------------------------------------------
  ! Test 6: add_to_field_with_kernel (real64): total activity conserved
  !         when particle is well inside field
  ! -----------------------------------------------------------------------
  deallocate(kernel)
  call initialize_gaussian_smoothing(kernel_size_in=3, max_age_hr_in=48)
  call build_age_gaussian_kernel(24, kernel)
  allocate(field_r8(10, 10))
  field_r8 = 0.0_real64
  call add_to_field_with_kernel(field_r8, 5, 5, 1.0, kernel, lost_activity)
  if (abs(real(sum(field_r8)) + lost_activity - 1.0) > tol) then
    print *, "FAIL test 6: activity not conserved for real64 field"
    print *, "  sum(field)=", sum(field_r8), "  lost=", lost_activity
    stop 1
  end if
  print *, "PASS test 6: real64 field, activity conserved: sum=", sum(field_r8), &
    " lost=", lost_activity

  ! -----------------------------------------------------------------------
  ! Test 7: add_to_field_with_kernel (real32): total activity conserved
  ! -----------------------------------------------------------------------
  deallocate(field_r8)
  allocate(field_r4(10, 10))
  field_r4 = 0.0_real32
  call add_to_field_with_kernel(field_r4, 5, 5, 1.0, kernel, lost_activity)
  if (abs(real(sum(field_r4)) + lost_activity - 1.0) > tol) then
    print *, "FAIL test 7: activity not conserved for real32 field"
    print *, "  sum(field)=", sum(field_r4), "  lost=", lost_activity
    stop 1
  end if
  print *, "PASS test 7: real32 field, activity conserved: sum=", sum(field_r4), &
    " lost=", lost_activity

  ! -----------------------------------------------------------------------
  ! Test 8: particle at corner loses activity proportional to out-of-bounds
  !         kernel cells; lost_activity > 0
  ! -----------------------------------------------------------------------
  deallocate(field_r4)
  allocate(field_r8(10, 10))
  field_r8 = 0.0_real64
  call add_to_field_with_kernel(field_r8, 1, 1, 1.0, kernel, lost_activity)
  if (lost_activity <= 0.0) then
    print *, "FAIL test 8: expected lost_activity > 0 at corner, got ", lost_activity
    stop 1
  end if
  if (abs(real(sum(field_r8)) + lost_activity - 1.0) > tol) then
    print *, "FAIL test 8: activity not conserved at corner"
    stop 1
  end if
  print *, "PASS test 8: corner particle loses correct fraction: lost=", lost_activity

  ! -----------------------------------------------------------------------
  ! Test 9: kernel all-zeros check does not happen at age 0 with large max_age
  !         (sigma is at min_sigma, so centre cell should be dominant)
  ! -----------------------------------------------------------------------
  deallocate(field_r8)
  deallocate(kernel)
  call initialize_gaussian_smoothing(kernel_size_in=3, max_age_hr_in=1000)
  call build_age_gaussian_kernel(0, kernel)
  if (all(kernel == 0.0)) then
    print *, "FAIL test 9: kernel is all zeros at age=0"
    stop 1
  end if
  print *, "PASS test 9: kernel is non-zero at age=0 with large max_age_hr"

  ! -----------------------------------------------------------------------
  ! Test 10: midpoint is ~1 for very small age (delta-like kernel)
  ! -----------------------------------------------------------------------
  deallocate(kernel)
  call initialize_gaussian_smoothing(kernel_size_in=3, max_age_hr_in=48)
  call build_age_gaussian_kernel(3, kernel)
  if (abs(kernel(2,2) - 1.0) > tol) then
    print *, "FAIL test 10: expected midpoint ~1 for small age, got ", kernel(2,2)
    stop 1
  end if
  print *, "PASS test 10: midpoint is ~1 for small age, midpoint=", kernel(2,2)

  print *, "All tests passed."
end program testGaussianSmoothing
