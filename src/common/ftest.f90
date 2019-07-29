! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

module ftestML
  implicit none
  private

  public ftest

  real, parameter :: undef = 1.0e35
  real, parameter :: ud = undef*0.9

!> Purpose: Test field, print min,mean,max values.
  interface ftest
    module procedure ftest_2d_orig, ftest_3d_orig, ftest_2d, ftest_3d
  end interface

  contains

subroutine slice_stats(field, fmin, fmax, fsum, fmean, contains_undef)
  use iso_fortran_env, only: real64

  real, intent(in) :: field(:, :)
  real, intent(out) :: fmin, fmax, fmean
  real(real64), intent(out) :: fsum
  logical, intent(in) :: contains_undef

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
    ndef=0
    do j=1,ny
      do i=1,nx
        if(field(i,j) < ud) then
          fmin = min(fmin,field(i,j))
          fmax = max(fmax,field(i,j))
          fsum = fsum+field(i,j)
          ndef = ndef+1
        end if
      end do
    end do
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

subroutine ftest_2d_orig(name,k1,k2,nx,ny,nk,field,iundef)
  use iso_fortran_env, only: real64
  USE snapdebug, only: iulog

  integer, value :: k1, k2
  integer, intent(in) :: nx, ny, nk, iundef
  real, intent(in) :: field(nx,ny)
  character(len=*), intent(in) :: name

  integer :: i,j,k,ndef
  real :: fmin,fmax,fmean
  real, parameter :: undef = 1.0e35
  real, parameter :: ud = undef*0.9

  real(real64) :: fsum

  if (.false.) write(*,*) nk ! Silence compiler warning

  fmin = huge(fmin)
  fmax = -huge(fmax)
  fsum=0.
  if(iundef == 0) then
    fmin = minval(field)
    fmax = maxval(field)
    fsum = sum(field)
    ndef=nx*ny
  else
    ndef=0
    do j=1,ny
      do i=1,nx
        if(field(i,j) < ud) then
          fmin=min(fmin,field(i,j))
          fmax=max(fmax,field(i,j))
          fsum=fsum+field(i,j)
          ndef=ndef+1
        end if
      end do
    end do
  end if

  if(ndef > 0) then
    fmean=fsum/dble(ndef)
  else
    fmin=0.
    fmax=0.
    fmean=0.
  end if
  if(k1 /= k2) then
    write(iulog,fmt='(5x,a8,1x,i3,3(1x,e13.5))') &
      name,k,fmin,fmean,fmax
  else
    write(iulog,fmt='(5x,a8,1x,3x,3(1x,e13.5))') &
      name,  fmin,fmean,fmax
  end if

  flush(iulog)
end subroutine ftest_2d_orig

subroutine ftest_3d_orig(name,k1,k2,nx,ny,nk,field,iundef)
  use iso_fortran_env, only: real64
  USE snapdebug, only: iulog

  implicit none

  integer, value :: k1, k2
  integer, intent(in) :: nx, ny, nk, iundef
  real, intent(in) :: field(nx,ny,nk)
  character(len=*), intent(in) :: name

  integer :: kstep,i,j,k,ndef
  real :: fmin,fmax,fmean
  real, parameter :: undef = 1.0e35
  real, parameter :: ud = undef*0.9

  real(real64) :: fsum

  if(k1 < 1 .OR. k1 > nk) k1=1
  if(k2 < 1 .OR. k2 > nk) k2=nk
  kstep=+1
  if(k1 > k2) kstep=-1

  do k=k1,k2,kstep
    fmin = huge(fmin)
    fmax = -huge(fmax)
    fsum=0.
    if(iundef == 0) then
      fmin = minval(field(:,:,k))
      fmax = maxval(field(:,:,k))
      fsum = sum(field(:,:,k))
      ndef=nx*ny
    else
      ndef=0
    !$OMP PARALLEL DO PRIVATE(j,i) REDUCTION(max : fmax)
    !$OMP&            REDUCTION(min : fmin) REDUCTION( + : fsum, ndef)
    !$OMP&            COLLAPSE(2)
      do j=1,ny
        do i=1,nx
          if(field(i,j,k) < ud) then
            fmin=min(fmin,field(i,j,k))
            fmax=max(fmax,field(i,j,k))
            fsum=fsum+field(i,j,k)
            ndef=ndef+1
          end if
        end do
      end do
    !$OMP END PARALLEL DO
    end if
    if(ndef > 0) then
      fmean=fsum/dble(ndef)
    else
      fmin=0.
      fmax=0.
      fmean=0.
    end if
    if(k1 /= k2) then
      write(iulog,fmt='(5x,a8,1x,i3,3(1x,e13.5))') &
      name,k,fmin,fmean,fmax
    else
      write(iulog,fmt='(5x,a8,1x,3x,3(1x,e13.5))') &
      name,  fmin,fmean,fmax
    end if
  end do
  flush(iulog)

  return
end subroutine ftest_3d_orig
end module ftestML
