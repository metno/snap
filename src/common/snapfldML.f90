! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute
! License: GNU General Public License v3.0 or later

module snapfldML
  USE iso_fortran_env, only: real32, real64
  implicit none
  private

!> number of hourly steps between output of fields
  integer, save, public :: nhfout = 3

!> async io flag, must be set before allocation of fields
  logical, save, public :: use_async_io = .false.

!>  horizontal wind component (time step 1)
  real(kind=real32), allocatable, target, save, public :: u1(:,:,:)
!>  horizontal wind component (time step 2)
  real(kind=real32), allocatable, target, save, public :: u2(:,:,:)
!>  horizontal wind component (time step 3)
  real(kind=real32), allocatable, target, save, public :: u3(:,:,:)
!>  horizontal wind component io
  real(kind=real32), pointer , public :: u_io(:,:,:)
!>  horizontal wind component (time step 1)
  real(kind=real32), allocatable, target, save, public :: v1(:,:,:)
!>  horizontal wind component (time step 2)
  real(kind=real32), allocatable, target, save, public :: v2(:,:,:)
!>  horizontal wind component (time step 3)
  real(kind=real32), allocatable, target, save, public :: v3(:,:,:)
!>  horizontal wind component io
  real(kind=real32), pointer , public :: v_io(:,:,:)
!>  sigma_dot (time step 1)
  real(kind=real32), allocatable, target, save, public :: w1(:,:,:)
!>  sigma_dot (time step 2)
  real(kind=real32), allocatable, target, save, public :: w2(:,:,:)
!>  sigma_dot (time step 3)
  real(kind=real32), allocatable, target, save, public :: w3(:,:,:)
!>  sigma_dot io
  real(kind=real32), pointer , public :: w_io(:,:,:)

!> potential temperature (level 1, surface: abs. temp. 2m) (time step 1)
  real(kind=real32), allocatable, target, save, public :: t1(:,:,:)
!> potential temperature (level 1, surface: abs. temp. 2m) (time step 2)
  real(kind=real32), allocatable, target, save, public :: t2(:,:,:)
!> potential temperature (level 1, surface: abs. temp. 2m) (time step 3)
  real(kind=real32), allocatable, target, save, public :: t3(:,:,:)
!>  potential temperature io
  real(kind=real32), pointer , public :: t_io(:,:,:)

!> absolute temperature
  real(kind=real32), allocatable, target, save, public :: t1_abs(:,:,:)
!> absolute temperature
  real(kind=real32), allocatable, target, save, public :: t2_abs(:,:,:)
!> absolute temperature
  real(kind=real32), allocatable, target, save, public :: t3_abs(:,:,:)
!>  absolute temperature io
  real(kind=real32), pointer , public :: t_abs_io(:,:,:)

  real(kind=real32), allocatable, target, save, public :: hlevel1(:,:,:)
  real(kind=real32), allocatable, target, save, public :: hlevel2(:,:,:)
  real(kind=real32), allocatable, target, save, public :: hlevel3(:,:,:)
  real(kind=real32), pointer , public :: hlevel_io(:,:,:)
  real(kind=real32), allocatable, target, save, public :: hlayer1(:,:,:)
  real(kind=real32), allocatable, target, save, public :: hlayer2(:,:,:)
  real(kind=real32), allocatable, target, save, public :: hlayer3(:,:,:)
  real(kind=real32), pointer , public :: hlayer_io(:,:,:)

!> hourly precipitation intensity (mm/hour)
  real(kind=real32), allocatable, target, save, public :: precip(:,:)
  real(kind=real32), allocatable, target, save, public :: precip_x(:,:)
  real(kind=real32), pointer , public :: precip_io(:,:)

!> instant precipitation intensity in three dimensions (mm/hour)
!> only temporarily used during reading of 3D precipitation from met-data
  real(kind=real32), allocatable, save, public :: precip3d(:,:,:)

!> Cloud water content (mm)
!> only temporarily used during reading of 3D precipitation from met-data
  real(kind=real32), allocatable, save, public :: cw3d(:,:,:)

!> Cloud cover fraction
!> only temporarily used during reading of 3D precipitation from met-data
  real(kind=real32), allocatable, save, public :: cloud_cover(:,:,:)

!> wet scavenging rate
  real(kind=real32), allocatable, target, save, public :: wscav(:,:,:,:)
  real(kind=real32), allocatable, target, save, public :: wscav_x(:,:,:,:)
  real(kind=real32), pointer , public :: wscav_io(:,:,:,:)

!> surface pressure (time step 1)
  real(kind=real32), allocatable, target, save, public :: ps1(:,:)
!> surface pressure (time step 2)
  real(kind=real32), allocatable, target, save, public :: ps2(:,:)
!> surface pressure (time step 3)
  real(kind=real32), allocatable, target, save, public :: ps3(:,:)
!> surface pressure io
  real(kind=real32), pointer , public :: ps_io(:,:)
!> mean sea level pressure (mslp, not used in computations)
!> for output (possible if nxad=nx and nyad=ny) (time step 1)
  real(kind=real32), allocatable, target, save, public :: pmsl1(:,:)
!> mean sea level pressure (mslp, not used in computations)
!> for output (possible if nxad=nx and nyad=ny) (time step 2)
  real(kind=real32), allocatable, target, save, public :: pmsl2(:,:)
!> mean sea level pressure (mslp, not used in computations)
!> for output (possible if nxad=nx and nyad=ny) (time step 3)
  real(kind=real32), allocatable, target, save, public :: pmsl3(:,:)
!> mean sea level pressure io (not used in computations)
  real(kind=real32), pointer , public :: pmsl_io(:,:)

!> boundary layer top in sigma coordinate (time step 1)
  real(kind=real32), allocatable, target, save, public :: bl1(:,:)
!> boundary layer top in sigma coordinate (time step 2)
  real(kind=real32), allocatable, target, save, public :: bl2(:,:)
!> boundary layer top in sigma coordinate (time step 3)
  real(kind=real32), allocatable, target, save, public :: bl3(:,:)
!> boundary layer top in sigma coordinate io
  real(kind=real32), pointer , public :: bl_io(:,:)

!> boundary layer depth in meters (time step 1)
  real(kind=real32), allocatable, target, save, public :: hbl1(:,:)
!> boundary layer depth in meters (time step 2)
  real(kind=real32), allocatable, target, save, public :: hbl2(:,:)
!> boundary layer depth in meters (time step 3)
  real(kind=real32), allocatable, target, save, public :: hbl3(:,:)
!> boundary layer depth in meters io
  real(kind=real32), pointer , public :: hbl_io(:,:)

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
  real(kind=real64), allocatable, save, public :: ml_bq(:,:,:,:)

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
  real, allocatable, target, save, public :: vd_dep(:, :, :)
  real, allocatable, target, save, public :: vd_dep_x(:, :, :)
  real, pointer , public :: vd_dep_io(:, :, :)

  !> Surface stress, heat flux, roughness length and 2m temperature
  !> temporary fields for calculation of vd_dep (not used in computations)
  real, allocatable, save, public :: surface_stress(:, :), hflux(:, :), z0(:, :), t2m(:, :)
  !> Friction velocity, aerosol resistance, meteorological resistance and deposition velocity
  !> temporary fields for calculation of vd_dep (not used in computations)
  real(real64), allocatable, save, public :: ustar(:,:), raero(:,:), my(:,:), nu(:,:)

  public :: swap_fields_after_reading, swap_fields_before_reading

  contains

  subroutine swap_3_fields_3d(field1, field2, field3)
    real(kind=real32), allocatable,intent(inout) :: field1(:,:,:), field2(:,:,:), field3(:,:,:)
    integer :: stat

    call move_alloc(from=field2, to=field1)
    call move_alloc(from=field3, to=field2)
    if (allocated(field1)) then
      allocate(field3, mold=field1, stat=stat)
      if (stat /= 0) then
        stop 'Error allocating field3 in swap_3_fields_3d'
      end if
    end if
  end subroutine swap_3_fields_3d

  subroutine swap_3_fields_2d(field1, field2, field3)
    real(kind=real32), allocatable,intent(inout) :: field1(:,:), field2(:,:), field3(:,:)
    integer :: stat
    call move_alloc(from=field2, to=field1)
    call move_alloc(from=field3, to=field2)
    if (allocated(field1)) then
      allocate(field3, mold=field1, stat=stat)
      if (stat /= 0) then
        stop 'Error allocating field3 in swap_3_fields_2d'
      end if
    end if
  end subroutine swap_3_fields_2d

  subroutine swap_2_fields_2d(field1, field2)
    real(kind=real32), allocatable,intent(inout) :: field1(:,:), field2(:,:)
    integer :: stat

    call move_alloc(from=field2, to=field1)
    if (allocated(field1)) then
      allocate(field2, mold=field1, stat=stat)
      if (stat /= 0) then
        stop 'Error allocating field2 in swap_2_fields_2d'
      end if
    end if
  end subroutine swap_2_fields_2d

  subroutine swap_2_fields_3d(field1, field2)
    real(kind=real32), allocatable, intent(inout) :: field1(:,:,:), field2(:,:,:)
    integer :: stat

    call move_alloc(from=field2, to=field1)
    if (allocated(field1)) then
      allocate(field2, mold=field1, stat=stat)
      if (stat /= 0) then
        stop 'Error allocating field2 in swap_2_fields_3d'
      end if
    end if
  end subroutine swap_2_fields_3d

  subroutine swap_2_fields_4d(field1, field2)
    real(kind=real32), allocatable, intent(inout) :: field1(:,:,:,:), field2(:,:,:,:)
    integer :: stat

    call move_alloc(from=field2, to=field1)
    if (allocated(field1)) then
      allocate(field2, mold=field1, stat=stat)
      if (stat /= 0) then
        stop 'Error allocating field2 in swap_2_fields_4d'
      end if
    end if
  end subroutine swap_2_fields_4d


  subroutine swap_fields_after_reading()
    if (use_async_io) then
      call swap_3_fields_3d(u1, u2, u3)
      u_io => u3
      call swap_3_fields_3d(v1, v2, v3)
      v_io => v3
      call swap_3_fields_3d(w1, w2, w3)
      w_io => w3
      call swap_3_fields_3d(t1, t2, t3)
      t_io => t3
      call swap_3_fields_3d(t1_abs, t2_abs, t3_abs)
      t_abs_io => t3_abs
      call swap_3_fields_3d(hlevel1, hlevel2, hlevel3)
      hlevel_io => hlevel3
      call swap_3_fields_3d(hlayer1, hlayer2, hlayer3)
      hlayer_io => hlayer3

      call swap_2_fields_2d(precip, precip_x)
      precip_io => precip_x
      call swap_2_fields_4d(wscav, wscav_x)
      wscav_io => wscav_x
      call swap_2_fields_3d(vd_dep, vd_dep_x)
      vd_dep_io => vd_dep_x

      call swap_3_fields_2d(ps1, ps2, ps3)
      ps_io => ps3
      call swap_3_fields_2d(pmsl1, pmsl2, pmsl3)
      pmsl_io => pmsl3
      call swap_3_fields_2d(bl1, bl2, bl3)
      bl_io => bl3
      call swap_3_fields_2d(hbl1, hbl2, hbl3)
      hbl_io => hbl3
    end if
  end subroutine swap_fields_after_reading

  subroutine swap_fields_before_reading()
    if (.not. use_async_io) then
      call swap_2_fields_3d(u1, u2)
      u_io => u2
      call swap_2_fields_3d(v1, v2)
      v_io => v2
      call swap_2_fields_3d(w1, w2)
      w_io => w2
      call swap_2_fields_3d(t1, t2)
      t_io => t2
      call swap_2_fields_3d(t1_abs, t2_abs)
      t_abs_io => t2_abs
      call swap_2_fields_3d(hlevel1, hlevel2)
      hlevel_io => hlevel2
      call swap_2_fields_3d(hlayer1, hlayer2)
      hlayer_io => hlayer2

      ! no need to swap precip, wscav and vd_dep, they get just overwritten

      call swap_2_fields_2d(ps1, ps2)
      ps_io => ps2
      call swap_2_fields_2d(pmsl1, pmsl2)
      pmsl_io => pmsl2
      call swap_2_fields_2d(bl1, bl2)
      bl_io => bl2
      call swap_2_fields_2d(hbl1, hbl2)
      hbl_io => hbl2
    end if
  end subroutine swap_fields_before_reading


end module snapfldML
