! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
!
! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!
module snapfldML
  USE iso_fortran_env, only: real32, real64
  implicit none
  private

!> number of hourly steps between output of fields
  integer, save, public :: nhfout = 3

!>  horizontal wind component (time step 1)
  real(kind=real32), allocatable, save, public :: u1(:,:,:)
!>  horizontal wind component (time step 2)
  real(kind=real32), allocatable, save, public :: u2(:,:,:)
!>  horizontal wind component (time step 1)
  real(kind=real32), allocatable, save, public :: v1(:,:,:)
!>  horizontal wind component (time step 2)
  real(kind=real32), allocatable, save, public :: v2(:,:,:)
!>  sigma_dot (time step 1)
  real(kind=real32), allocatable, save, public :: w1(:,:,:)
!>  sigma_dot (time step 2)
  real(kind=real32), allocatable, save, public :: w2(:,:,:)

!> potential temperature (level 1, surface: abs. temp. 2m) (time step 1)
  real(kind=real32), allocatable, save, public :: t1(:,:,:)
!> potential temperature (level 1, surface: abs. temp. 2m) (time step 2)
  real(kind=real32), allocatable, save, public :: t2(:,:,:)

!> absolute temperature
  real(kind=real32), allocatable, save, public :: t1_abs(:,:,:)
!> absolute temperature
  real(kind=real32), allocatable, save, public :: t2_abs(:,:,:)

  real(kind=real32), allocatable, save, public :: hlevel1(:,:,:)
  real(kind=real32), allocatable, save, public :: hlevel2(:,:,:)
  real(kind=real32), allocatable, save, public :: hlayer1(:,:,:)
  real(kind=real32), allocatable, save, public :: hlayer2(:,:,:)

!> hourly precipitation intensity (mm/hour)
  real(kind=real32), allocatable, save, public :: precip(:,:)
!> instant precipitation intensity in three dimensions (mm/hour)
  real(kind=real32), allocatable, save, public :: precip3d(:,:,:)
!> Cloud water content (mm)
  real(kind=real32), allocatable, save, public :: cw3d(:,:,:)

!> Cloud cover fraction
  real(kind=real32), allocatable, save, public :: cloud_cover(:,:,:)

!> wet scavenging rate
  real(kind=real32), allocatable, save, public :: wscav(:,:,:,:)

!> surface pressure (time step 1)
  real(kind=real32), allocatable, save, public :: ps1(:,:)
!> surface pressure (time step 2)
  real(kind=real32), allocatable, save, public :: ps2(:,:)
!> mean sea level pressure (mslp, not used in computations)
!> for output (possible if nxad=nx and nyad=ny) (time step 1)
  real(kind=real32), allocatable, save, public :: pmsl1(:,:)
!> mean sea level pressure (mslp, not used in computations)
!> for output (possible if nxad=nx and nyad=ny) (time step 2)
  real(kind=real32), allocatable, save, public :: pmsl2(:,:)

!> boundary layer top in sigma coordinate (time step 1)
  real(kind=real32), allocatable, save, public :: bl1(:,:)
!> boundary layer top in sigma coordinate (time step 2)
  real(kind=real32), allocatable, save, public :: bl2(:,:)
!> boundary layer depth in meters (time step 1)
  real(kind=real32), allocatable, save, public :: hbl1(:,:)
!> boundary layer depth in meters (time step 2)
  real(kind=real32), allocatable, save, public :: hbl2(:,:)

!> map ratio in x direction
  real(kind=real32), allocatable, save, public :: xm(:,:)
!> map ratio in y direction
  real(kind=real32), allocatable, save, public :: ym(:,:)
!> grid square area (m**2) )(high output resolution)
  real(kind=real32), allocatable, save, public :: garea(:,:)


!> Work array (low input resolution)
  real(kind=real32), allocatable, save, public :: field1(:,:)
!> Work array (low input resolution)
  real(kind=real32), allocatable, save, public :: field2(:,:)
!> Work array (low input resolution)
  real(kind=real32), allocatable, save, public :: field3(:,:)
!> Work array (low input resolution)
  real(kind=real32), allocatable, save, public :: field4(:,:)
!> 3D Work array (low input resolution)
  real(kind=real32), allocatable, save, public :: field3d1(:,:,:)
!> Work array (high output resolution)
  real(kind=real32), allocatable, save, public :: field_hr1(:,:)
!> Work array (high output resolution)
  real(kind=real32), allocatable, save, public :: field_hr2(:,:)
!> Work array (high output resolution)
  real(kind=real32), allocatable, save, public :: field_hr3(:,:)
!> Work array (high output resolution) for hbl
  real(kind=real32), allocatable, save, public :: hbl_hr(:,:)

!> the ensemble-member to read met-data from
!> default (-1) : no ensemble member
  integer, save, public :: enspos = -1

!> average height of boundary layer (accumulation)
  real(kind=real64), allocatable, save, public :: avghbl(:,:)
!> precipitation  (accumulation)
  real(kind=real64), allocatable, save, public :: avgprec(:,:)
!> accumulation of precipitation from start of run
  real(kind=real64), allocatable, save, public :: accprec(:,:)

!> dry deposition
  real(kind=real64), allocatable, save, public :: depdry(:,:,:)
!> wet deposition
  real(kind=real64), allocatable, save, public :: depwet(:,:,:)

!> accumulated dry deposition
!> This is not weighted by area
  real(kind=real64), allocatable, save, public :: accdry(:,:,:)
!> accumulated wet deposition
!> This is not weighted by area
  real(kind=real64), allocatable, save, public :: accwet(:,:,:)
!> accumulated/integrated concentration
  real(kind=real64), allocatable, save, public :: concacc(:,:,:)
!> average Bq (per square area) in boundary layer (accum.)
!> This is not weighted by area
  real(kind=real64), allocatable, save, public :: avgbq1(:,:,:)
!> average Bq (per square area) above boundary layer (accum.)
!> This is not weighted by area
  real(kind=real64), allocatable, save, public :: avgbq2(:,:,:)

  real(kind=real64), allocatable, save, public :: concen(:,:,:)
!> average Bq (per square area) in each layer (accum.)
!>
!> only used if imodlevel
!> This is not weighted by area
  real(kind=real64), allocatable, save, public :: avgbq(:,:,:,:)

!> Scratch space for finding max column concentration
!>
!> only used if compute_column_max_conc
  real, allocatable, save, public :: max_column_scratch(:,:,:)
!> Max column concentration
!>
!> only used if compute_column_max_conc
  real, allocatable, save, public :: max_column_concentration(:,:)

!> Scratch space for dose calculation
  real, allocatable, save, public :: aircraft_doserate_scratch(:,:,:,:)
!> Doserate
  real, allocatable, save, public :: aircraft_doserate(:,:)
  !> Height of doserates exceeding the given threshold
  real, allocatable, save, public :: aircraft_doserate_threshold_height(:,:)

! > Total released activity per species
  real(kind=real64), allocatable, save, public :: total_activity_released(:)
  ! > Activity lost through exiting domain
  real(kind=real64), allocatable, save, public :: total_activity_lost_domain(:)
  ! > Activity lost through exiting rmlimit, maxage
  real(kind=real64), allocatable, save, public :: total_activity_lost_other(:)

  !> Deposition velocity on the grid per species
  real, allocatable, save, public :: vd_dep(:, :, :)

  real, allocatable, save, public :: xflux(:, :), yflux(:, :), hflux(:, :), z0(:, :), leaf_area_index(:, :), t2m(:, :)
  real(real64), allocatable, save, public :: roa(:,:), ustar(:,:), monin_l(:,:), raero(:,:), vs(:,:), rs(:,:)
end module snapfldML
