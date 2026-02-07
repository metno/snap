! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute
! License: GNU GPL v3 or later

module ftestML
  implicit none
  private

  public ftest

  real, parameter :: undef = 1.0e35
  real, parameter :: ud = undef*0.9

!> Purpose: Test field, print min,mean,max values.
  interface ftest
    module procedure ftest_2d, ftest_3d
  end interface

  contains

subroutine slice_stats(field, fmin, fmax, fsum, fmean, contains_undef)
  use iso_fortran_env, only: real64

  real, intent(in) :: field(:, :)
  real, intent(out) :: fmin, fmax, fmean
  real(real64), intent(out) :: fsum
  logical, intent(in) :: contains_undef

  logical, allocatable :: mask(:,:)
  integer :: nx, ny, i, j, ndef

  nx = size(field, 1)
  ny = size(field, 2)

  fmin = huge(fmin)
  fmax = -huge(fmax)
  fsum = 0.0
  if(.not.contains_undef) then
    fmin = minval(field)
    fmax = maxval(field)
    fsum = sum(field)
    ndef = size(field)
  else
    mask = field < ud
    ndef = count(mask)
    fmin = minval(field, mask=mask)
    fmax = maxval(field, mask=mask)
    fsum = sum(field, mask=mask)
  end if

  if(ndef > 0) then
    fmean=fsum/dble(ndef)
  else
    fmin=0.
    fmax=0.
    fmean=0.
  end if
end subroutine slice_stats

subroutine ftest_2d(name, field, contains_undef)
  use iso_fortran_env, only: real64
  USE snapdebug, only: iulog

  character(len=*), intent(in) :: name
  real, intent(in) :: field(:,:)
  logical, optional, intent(in) :: contains_undef

  real :: fmin, fmax, fmean
  real(real64) :: fsum
  logical :: check_for_undef

  check_for_undef = .false.
  if (present(contains_undef)) then
    check_for_undef = contains_undef
  endif

  call slice_stats(field, fmin, fmax, fsum, fmean, check_for_undef)

  write(iulog,fmt='(5x,a8,1x,3x,3(1x,e13.5))') &
      name,  fmin,fmean,fmax

  flush(iulog)
end subroutine ftest_2d

subroutine ftest_3d(name, field, contains_undef, reverse_third_dim)
  use iso_fortran_env, only: real64
  USE snapdebug, only: iulog

  character(len=*), intent(in) :: name
  real, intent(in) :: field(:,:,:)
  logical, optional, intent(in) :: contains_undef
  logical, optional, intent(in) :: reverse_third_dim

  integer :: nx, ny, nk

  integer :: k, kbot, ktop, kstep
  real :: fmin,fmax,fmean
  real(real64) :: fsum
  logical :: has_undef

  has_undef = .false.
  if (present(contains_undef)) then
    has_undef = contains_undef
  endif

  nx = size(field, 1)
  ny = size(field, 2)
  nk = size(field, 3)

  kbot = 1
  kstep = 1
  ktop = nk
  if (present(reverse_third_dim)) then
    if (reverse_third_dim) then
      kbot = nk
      kstep = -1
      ktop = 1
    endif
  endif

  do k=kbot,ktop,kstep
    call slice_stats(field(:,:,k), fmin, fmax, fsum, fmean, has_undef)
    if(nk /= 1) then
      write(iulog,fmt='(5x,a8,1x,i3,3(1x,e13.5))') &
      name,k,fmin,fmean,fmax
    else
      write(iulog,fmt='(5x,a8,1x,3x,3(1x,e13.5))') &
      name,  fmin,fmean,fmax
    end if
  end do
  flush(iulog)

  return
end subroutine ftest_3d

end module ftestML
