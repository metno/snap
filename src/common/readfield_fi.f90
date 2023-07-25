! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2021   Norwegian Meteorological Institute

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

!> Module with utilities to read files with fimex
!! @warning The class FimexIO stores internally two references to file and data-handles.
!!    It should therefore not be accessed from two parallel threads
module readfield_fiML

  USE Fimex, ONLY: FimexIO
  USE snapfimexML, ONLY: file_type, conf_file, interpolation, fint, parse_interpolator
  use iso_fortran_env, only: real32, real64, error_unit
  USE ftestML, only: ftest
  USE om2edotML, only: om2edot
  USE milibML, only: mapfield
  USE snaptabML, only: t2thetafac
  USE snapdebug, only: iulog, idebug

  implicit none
  private

  public :: readfield_fi, fi_checkload, check, fimex_open, read_landfractions

  !> @brief load and check an array from a source
  interface fi_checkload
    module procedure :: fi_checkload1d, fi_checkload2d, fi_checkload3d, &
                        fi_checkload2d_64, fi_checkload3d_64
  end interface

contains

!> open a fimex io object, eventuall with an interpolator
!> dies on error
  subroutine fimex_open(filename, fio)
    character(len=*), intent(in) :: filename
    TYPE(FimexIO), intent(out) :: fio

    TYPE(FimexIO) :: fio_intern

    if (fint%method >= 0) then
      call check(fio_intern%open (filename, conf_file, file_type), &
        "Can't make io-object with file:"//trim(filename)//" config: "//conf_file)
      call check(fio%interpolate(fio_intern, fint%method, fint%proj, fint%x_axis, fint%y_axis, fint%unit_is_degree), &
        "Can't interpolate file:"//trim(filename))
    else
      ! copying a fio doesn't seem to work, so I open fio directly
      call check(fio%open (filename, conf_file, file_type), &
        "Can't make io-object with file:"//trim(filename)//" config: "//trim(conf_file))
    end if

  end subroutine fimex_open

!> Read fields from fimex files. (see fimex.F90)
  subroutine readfield_fi(istep, backward, itimei, ihr1, ihr2, itimefi, ierror)
    USE iso_fortran_env, only: error_unit
    USE snapfilML, only: iavail, filef
    USE snapfldML, only: &
      xm, ym, u1, u2, v1, v2, w1, w2, t1, t2, ps1, ps2, pmsl1, pmsl2, &
      hbl1, hbl2, hlayer1, hlayer2, garea, hlevel1, hlevel2, &
      hlayer1, hlayer2, bl1, bl2, enspos, precip, t1_abs, t2_abs, &
      field1
    USE snapgrdML, only: alevel, blevel, vlevel, ahalf, bhalf, vhalf, &
                         gparam, kadd, klevel, ivlevel, imslp, igtype, ivlayer, ivcoor
    USE snapmetML, only: met_params, xy_wind_units, pressure_units, omega_units, &
                         sigmadot_units, temp_units, requires_precip_deaccumulation
    USE snapdimML, only: nx, ny, nk, output_resolution_factor, hres_field
    USE datetime, only: datetime_t, duration_t
    USE readfield_ncML, only: find_index, compute_vertical_coords
!> current timestep (always positive), negative istep means reset
    integer, intent(in) :: istep
!> whether meteorology should be read backwards
    logical, intent(in) :: backward
!> minimal time-offset after itimei
    integer, value :: ihr1
!> maximal time-offset after itimei
    integer, value :: ihr2
!> time since last file input
    type(datetime_t), intent(in) :: itimei
!> final time (output)
    type(datetime_t), intent(out) :: itimefi
!> error (output)
    integer, intent(out) :: ierror

! local variables
    TYPE(FimexIO) :: fio
    integer, save :: ntav1, ntav2 = 0
    character(len=1024), save :: file_name = ""
    character(len=1024), save :: ap_units = pressure_units
    logical, save :: first_time_read = .true.

    integer :: i, j, k, ilevel, i1, i2
    integer :: nhdiff, nhdiff_precip, prev_tstep_same_file
    real :: alev(nk), blev(nk), dxgrid, dygrid
    integer :: kk, ifb, kfb
    real :: dred, red, p, px, ptop
    real :: ptoptmp(1)

    integer :: timepos, timeposm1, nr

    ierror = 0

    if (istep < 0) then
      ! set 'save' variables to default values,
      ntav1 = 0
      ntav2 = 0
    end if

!.. get the correct ensemble/realization position, nr starting with 1, enspos starting with 0
    nr = enspos + 1
    if (enspos <= 0) nr = 1

!..get time offset in hours (as iavail(n)%oHour)
    ntav1 = ntav2
    ntav2 = find_index(istep < 0, backward, itimei, ihr1, ihr2)

    if (ntav2 < 1) then
      write (iulog, *) '*READFIELD* No model level data available'
      write (error_unit, *) '*READFIELD* No model level data available'
      ierror = 1
      return
    end if

    if (idebug == 1) then
      write (iulog, *) 'MODEL LEVEL SEARCH LIST.   ntav2=', ntav2
      write (iulog, *) 'nx,ny,nk: ', nx, ny, nk
      write (iulog, *) 'istep: ', istep
      write (iulog, *) 'itimei, ihr1, ihr2:', itimei, ihr1, ihr2
      write (iulog, *) 'kfb,ifb:', kfb, ifb
      write (iulog, fmt='(7(1x,i4),1x,i6,2i5)') (iavail(ntav2))
      flush (iulog)
    end if

! time between two inputs
! open the correct file, currently opened each time
    file_name = filef(iavail(ntav2)%fileNo)
    call fimex_open(file_name, fio)

!     set timepos and nhdiff
    if (ntav1 /= 0) then
      nhdiff = abs(iavail(ntav2)%oHour - iavail(ntav1)%oHour)
    else
      ! Irrelevant time difference for the first timestep
      nhdiff = 0
    endif

    timepos = iavail(ntav2)%timePos
    timeposm1 = timepos ! Default: No deaccumulation possible
    prev_tstep_same_file = iavail(ntav2)%pavail_same_file
    if (prev_tstep_same_file /= 0) then
      ! previous timestep in same file for deaccumulation, even if not in list
      timeposm1 = iavail(prev_tstep_same_file)%timePos
      nhdiff_precip = abs(iavail(ntav2)%oHour - iavail(prev_tstep_same_file)%oHour)
    else
     if (.not.requires_precip_deaccumulation()) then
       nhdiff_precip = 0
     else
       ! Figure out if the next timestep belongs to the same forecast
       if ((filef(ntav2+1) == filef(ntav2)) .and. (iavail(ntav2+1)%fchour == iavail(ntav2)%fchour)) then
         nhdiff_precip = abs(iavail(ntav2+1)%oHour - iavail(ntav2)%oHour)
       else
         nhdiff_precip = nhdiff
         if (nhdiff == 0) then
           ! Default in case nhdiff not set
           nhdiff_precip = 3
         endif
         write(iulog,'("Deaccumulation of precipitation requires estimate of nhdiff, nhdiff=",I3,"hours")') nhdiff_precip
         write(error_unit,'("Deaccumulation of precipitation requires estimate of nhdiff, nhdiff=",I3,"hours")') nhdiff_precip
       endif
     endif
    endif

    itimefi = datetime_t(iavail(ntav2)%aYear, &
                         iavail(ntav2)%aMonth, &
                         iavail(ntav2)%aDay, &
                         iavail(ntav2)%aHour)
    itimefi = itimefi + duration_t(iavail(ntav2)%fcHour)

    if (idebug == 1) then
      write (iulog, *) 'READING DATA FROM file=', trim(file_name)
      write (iulog, *) 'READING DATA FROM position=', timepos, ' for ', &
        itimefi, ', prev. position=', timeposm1, ', hours:', nhdiff
    end if

    if (.TRUE.) then
      !..move data from input time step 2 to 1

      u1(:, :, :) = u2
      v1(:, :, :) = v2
      w1(:, :, :) = w2
      t1(:, :, :) = t2
      if (allocated(t2_abs)) t1_abs(:,:,:) = t2_abs
      hlevel1(:, :, :) = hlevel2
      hlayer1(:, :, :) = hlayer2

      ps1(:, :) = ps2
      bl1(:, :) = bl2
      hbl1(:, :) = hbl2

      if (imslp /= 0) then
        pmsl1(:, :) = pmsl2
      end if

    end if

    ptop = 100.0
    do k = nk - kadd, 2, -1

      !..input model level no.
      ilevel = klevel(k)

      !..u
      !     Get the varid of the data variable, based on its name.
      call fi_checkload(fio, met_params%xwindv, xy_wind_units, u2(:, :, k), nt=timepos, nz=ilevel, nr=nr)

      !..v
      call fi_checkload(fio, met_params%ywindv, xy_wind_units, v2(:, :, k), nt=timepos, nz=ilevel, nr=nr)
      ! bug in chernobyl borders from destaggering
      where (v2 >= 1e+30)
      v2 = 0.0
      end where

      !..pot.temp. or abs.temp.
      call fi_checkload(fio, met_params%pottempv, temp_units, t2(:, :, k), nt=timepos, nz=ilevel, nr=nr)

      !   TODO read ptop from file (only needed for sigma), but not in emep data
      ptop = 100. ! hPa
      !       if(ivcoor.eq.2) ptop=idata(19)
      !..p0 for hybrid loaded to ptop, ap is a * p0
      if (ivcoor /= 2 .AND. .NOT. met_params%ptopv == '') then
        call fi_checkload(fio, met_params%ptopv, pressure_units, ptoptmp)
        ptop = ptoptmp(1)
        ap_units = ""
      end if
      !..alevel (here) only for eta levels
      if (.NOT. met_params%apv == '') then
        call fi_checkload(fio, met_params%apv, ap_units, alev(k:k), nz=ilevel)
        call fi_checkload(fio, met_params%bv, "", blev(k:k), nz=ilevel)
        if (ivcoor /= 2 .AND. .NOT. met_params%ptopv == '') then
          !..p0 for hybrid loaded to ptop, ap is a * p0
          alev(k) = alev(k)*ptop
        end if
      end if
      if (.NOT. met_params%sigmav == '') then
        ! reusing blev(k) for sigma(k) later
        call fi_checkload(fio, met_params%sigmav, "", blev(k:k), nz=ilevel)
      end if

      !..sigma_dot/eta_dot (0 at surface)
      !..eta: eta_dot (or omega) stored in the same levels as u,v,th.
      if (met_params%sigmadotv == '') then
        w2 = 0
      else
        if (met_params%sigmadot_is_omega) then
          call fi_checkload(fio, met_params%sigmadotv, omega_units, w2(:, :, k), nt=timepos, nz=ilevel, nr=nr)
        else
          call fi_checkload(fio, met_params%sigmadotv, sigmadot_units, w2(:, :, k), nt=timepos, nz=ilevel, nr=nr)
        end if
      end if

    end do ! k=nk-kadd,2,-1

!..surface pressure, 10m wind and possibly mean sea level pressure,
!..precipitation

! ps
    call fi_checkload(fio, met_params%psv, pressure_units, ps2(:, :), nt=timepos, nr=nr)

! u10m
! v10m
    if (.not. met_params%use_model_wind_for_10m) then
      call fi_checkload(fio, met_params%xwind10mv, xy_wind_units, u2(:, :, 1), nt=timepos, nr=nr, nz=1)
      call fi_checkload(fio, met_params%ywind10mv, xy_wind_units, v2(:, :, 1), nt=timepos, nr=nr, nz=1)
    else
      call fi_checkload(fio, met_params%xwindv, xy_wind_units, u2(:, :, 1), nt=timepos, nr=nr, nz=nk)
      call fi_checkload(fio, met_params%ywindv, xy_wind_units, v2(:, :, 1), nt=timepos, nr=nr, nz=nk)
    endif

!..mean sea level pressure, not used in computations,
!..(only for output to results file)
    if (imslp /= 0) then
      if (met_params%mslpv /= '') then
        call fi_checkload(fio, met_params%mslpv, pressure_units, pmsl2(:, :), nt=timepos, nr=nr, nz=1)
      else if (met_params%psv /= '') then
        call fi_checkload(fio, met_params%psv, pressure_units, pmsl2(:, :), nt=timepos, nr=nr, nz=1)
      else
        write (iulog, *) 'Mslp not found. Not important.'
        imslp = 0
      endif
    end if

    if (first_time_read) then
      call compute_vertical_coords(alev, blev, ptop)
    endif

    if (met_params%need_precipitation) then
      call read_precipitation(fio, nhdiff_precip, timepos, timeposm1)
    else
      precip = 0.0
    endif

    call read_drydep(fio, timepos, timeposm1, nr)

    call check(fio%close(), "close fio")

! first time initialized data
    if (first_time_read) then
      first_time_read = .false.

      !..compute map ratio
      call mapfield(1, 0, igtype, gparam, nx, ny, xm, ym, &
                    xm, & ! Ignored when icori = 0
                    dxgrid, dygrid, ierror)
      if (ierror /= 0) then
        write (iulog, *) 'MAPFIELD ERROR. ierror= ', ierror
        write (error_unit, *) 'MAPFIELD ERROR. ierror= ', ierror
        error stop 255
      end if
      gparam(7) = dxgrid
      gparam(8) = dygrid
      !..set garea size of each grid square (m**2) in output grid-size
      field1 = abs((dxgrid/xm)*(dygrid/ym)) / (output_resolution_factor*output_resolution_factor)
      call hres_field(field1, garea, .true.)

      ! end initialization
    end if

    if (met_params%temp_is_abs) then
      if (allocated(t2_abs)) t2_abs(:,:,:) = t2
      !..abs.temp. -> pot.temp.
      do k = 2, nk - kadd
        do j = 1, ny
          do i = 1, nx
            p = alevel(k) + blevel(k)*ps2(i,j)
            t2(i,j,k) = t2(i,j,k)*t2thetafac(p)
          end do
        end do
      end do
    else
      if (allocated(t2_abs)) then
        ! pot.temp -> abs.temp
        do k=2,nk-kadd
          do j = 1, ny
            do i = 1, nx
              p = alevel(k) + blevel(k)*ps2(i,j)
              t2_abs(i,j,k) = t2(i,j,k)/t2thetafac(p)
            end do
          end do
        end do
      endif
    end if

    if (met_params%sigmadot_is_omega) then
      !..omega -> etadot, or rather etadot derived from continuity-equation (mean of both)
      call om2edot
    else if (met_params%sigmadotv == '') then
      !..omega -> etadot, or rather etadot derived from continuity-equation
      call om2edot
      ! om2edot take means of omega (=0) and continuity-equation, -> use only continuity equation
      w2 = 2.0*w2
    end if

!..sigma_dot/eta_dot 0 at surface
    w2(:, :, 1) = 0.0

!..no temperature at or near surface (not used, yet)
    t2(:, :, 1) = -999.0
    if (kadd > 0) then
      !..levels added at the top
      dred = 0.5/float(kadd)
      red = 1.
      kk = nk - kadd
      do k = nk - kadd + 1, nk
        red = red - dred
        u2(:, :, k) = u2(:, :, kk)
        v2(:, :, k) = v2(:, :, kk)
        w2(:, :, k) = w2(:, :, kk)*red
        t2(:, :, k) = t2(:, :, kk)
      end do
    end if

    if (backward) then
      ! backward-calculation, switch sign of winds
      u2 = -u2
      v2 = -v2
      w2 = -w2
    end if

! test---------------------------------------------------------------
    write (iulog, *) 'k,k_model,alevel,blevel,vlevel,p,dp:'
    px = alevel(nk) + blevel(nk)*1000.
    do k = nk, 1, -1
      p = alevel(k) + blevel(k)*1000.
      write (iulog, fmt='(1x,2i5,f9.2,2f9.5,f8.0,f6.0)') &
        k, klevel(k), alevel(k), blevel(k), vlevel(k), p, p - px
      px = p
    end do

! test---------------------------------------------------------------

    if (idebug == 1) then
      call ftest('u  ', u2, reverse_third_dim=.true.)
      call ftest('v  ', v2, reverse_third_dim=.true.)
      call ftest('w  ', w2, reverse_third_dim=.true.)
      call ftest('t  ', t2, reverse_third_dim=.true.)
      call ftest('ps ', ps2)
      if (istep > 0) &
        call ftest('pre', precip(:, :))
    end if

    if (istep == 0) then
      ! test---------------------------------------------------------------
      write (iulog, *) 'k,ahalf,bhalf,vhalf,p,dp:'
      px = ahalf(nk) + bhalf(nk)*1000.
      do k = nk, 1, -1
        p = ahalf(k) + bhalf(k)*1000.
        write (iulog, fmt='(1x,i5,f9.2,2f9.5,f8.0,f6.0)') &
          k, ahalf(k), bhalf(k), vhalf(k), p, p - px
        px = p
      end do
      ! test---------------------------------------------------------------

      !..level table for (vertical) interpolation
      !..(remember that fields are stored bottom to top
      !.. and that all parameters now are in the same levels)
      write (iulog, *) 'ivlevel:'
      write (iulog, *) 'k,i1,i2,vlevel(k+1),vlevel(k)'
      i2 = -1
      do k = nk - 1, 1, -1
        i1 = i2 + 1
        i2 = vlevel(k)*10000.
        if (k == 1) i2 = 10000
        do i = i1, i2
          ivlevel(i) = k
        end do
        write (iulog, *) k, i1, i2, vlevel(k + 1), vlevel(k)
      end do

      !..level table for concentration in each sigma/eta layer
      !..(layers here as in the input model, no '10m' layer,
      !.. but ordering bottom to top, reorder at time of output)
      write (iulog, *) 'ivlayer:'
      write (iulog, *) 'k,i1,i2,vhalf(k+1),vhalf(k)'
      i2 = -1
      do k = nk - 1, 1, -1
        i1 = i2 + 1
        i2 = nint(vhalf(k)*10000.)
        if (k == 1) i2 = 10000
        do i = i1, i2
          ivlayer(i) = k
        end do
        write (iulog, *) k, i1, i2, vhalf(k + 1), vhalf(k)
      end do
    end if

    if (met_params%use_3d_precip) then
      block
        use snapparML, only: ncomp, run_comp
        use snapfldML, only: wscav, cw3d, precip3d, cloud_cover
        use wetdep, only: prepare_wetdep

        integer :: i

        do i=1,ncomp
          if (.not.run_comp(i)%defined%kdrydep == 1) cycle
          call prepare_wetdep(wscav(:,:,:,i), run_comp(i)%defined%radiusmym, precip3d, cw3d, cloud_cover)
        enddo
      end block
    endif
  end subroutine readfield_fi

!> read precipitation
  subroutine read_precipitation(fio, nhdiff, timepos, timeposm1)
    use iso_fortran_env, only: error_unit
    use snapdebug, only: iulog
    use snapmetML, only: met_params, &
                         precip_rate_units, precip_units_ => precip_units, precip_units_fallback
    use snapfldML, only: field1, field2, field3, field4, precip, &
                         enspos, precip

!> open netcdf file
    TYPE(FimexIO), intent(inout) :: fio
!> time difference in hours between two precip fields
    integer, intent(in) :: nhdiff
!> timestep in file
    integer, intent(in) :: timepos
!> previous timestep
    integer, intent(in) :: timeposm1

    integer :: nr
    real :: totalprec
    character(len=10), save :: precip_units = precip_units_
    integer :: ierror

!.. get the correct ensemble/realization position, nr starting with 1, enspos starting with 0
    nr = enspos + 1
    if (enspos <= 0) nr = 1

    if (met_params%use_3d_precip) then
      block ! read_precip
        use snaptabML, only: g
        use snapfldML, only: ps2, precip3d, cw3d, cloud_cover
        use snapgrdML, only: ahalf, bhalf, kadd, klevel
        use snapdimML, only: nx, ny, nk
        real(real64), allocatable :: rain_in_air(:,:), graupel_in_air(:,:), snow_in_air(:,:)
        real(real64), allocatable :: cloud_water(:,:), cloud_ice(:,:)
        real(real64), allocatable :: pdiff(:,:)

        integer :: ilevel, k
        character(len=*), parameter :: mass_fraction_units = "kg/kg"

        allocate(rain_in_air(nx,ny),graupel_in_air(nx,ny),snow_in_air(nx,ny),pdiff(nx,ny))
        allocate(cloud_water(nx,ny),cloud_ice(nx,ny))

        precip3d(:,:,:) = 0.0
        cw3d(:,:,:) = 0.0

        do k=nk-kadd,2,-1
          ilevel = klevel(k)
          call fi_checkload(fio, "mass_fraction_of_rain_in_air_ml", mass_fraction_units, &
                            rain_in_air, nt=timepos, nz=ilevel, nr=nr)
          call fi_checkload(fio, "mass_fraction_of_graupel_in_air_ml", mass_fraction_units, &
                            graupel_in_air, nt=timepos, nz=ilevel, nr=nr)
          call fi_checkload(fio, "mass_fraction_of_snow_in_air_ml", mass_fraction_units, &
                            snow_in_air, nt=timepos, nz=ilevel, nr=nr)

          where (rain_in_air < 0.0)
            rain_in_air = 0.0
          end where
          where (graupel_in_air < 0.0)
            graupel_in_air = 0.0
          end where
          where (snow_in_air < 0.0)
            snow_in_air = 0.0
          end where

          pdiff(:,:) = 100*( (ahalf(k-1) - ahalf(k)) + (bhalf(k-1) - bhalf(k))*ps2 )

          precip3d(:,:,k) = rain_in_air + graupel_in_air + snow_in_air
          precip3d(:,:,k) = precip3d(:,:,k) * pdiff / g

          call fi_checkload(fio, "mass_fraction_of_cloud_condensed_water_in_air_ml", mass_fraction_units, &
                            cloud_water, nt=timepos, nz=ilevel, nr=nr)
          call fi_checkload(fio, "mass_fraction_of_cloud_ice_in_air_ml", mass_fraction_units, &
                            cloud_ice, nt=timepos, nz=ilevel, nr=nr)

          where (cloud_water < 0.0)
            cloud_water = 0.0
          end where
          where (cloud_ice < 0.0)
            cloud_ice = 0.0
          end where
          cw3d(:,:,k) = cloud_water + cloud_ice
          cw3d(:,:,k) = cw3d(:,:,k) * pdiff / g

          call fi_checkload(fio, "cloud_area_fraction_ml", "%", &
                            cloud_cover(:,:,k), nt=timepos, nz=ilevel, nr=nr)
        enddo

        ! Accumulate precipitation from top down
        ! Doing this in the wetdep module
        !do k=(nk-kadd)-1,2,-1
        !  precip3d(:,:,k) = precip3d(:,:,k) + precip3d(:,:,k+1)
        !enddo
      end block
    end if
    if (met_params%precaccumv /= '') then
      !..precipitation between input time 't1' and 't2'
      if (timepos == 1) then
        field1 = 0.0
      else
        call fi_checkload(fio, met_params%precaccumv, precip_units, field1(:, :), nt=timeposm1, nr=nr, nz=1)
      endif
      call fi_checkload(fio, met_params%precaccumv, precip_units, field2(:, :), nt=timepos, nr=nr, nz=1)

      precip(:,:) = (field2 - field1)/nhdiff
    else if (met_params%precstratiaccumv /= '') then
      ! accumulated stratiform and convective precipitation
      !..precipitation between input time 't1' and 't2'
      call fi_checkload(fio, met_params%precstratiaccumv, precip_units, field3(:, :), nt=timepos, nr=nr, ierror=ierror)
      if (ierror /= 0) then
        !> There was an error, try reading using a fallback precip unit
        precip_units = precip_units_fallback
        call fi_checkload(fio, met_params%precstratiaccumv, precip_units, field3(:, :), nt=timepos, nr=nr)
      endif
      call fi_checkload(fio, met_params%precconaccumv, precip_units, field4(:, :), nt=timepos, nr=nr)

      if (timepos /= 1) then
        call fi_checkload(fio, met_params%precstratiaccumv, precip_units, field1(:, :), nt=timeposm1, nr=nr)
        call fi_checkload(fio, met_params%precconaccumv, precip_units, field2(:, :), nt=timeposm1, nr=nr)
      else
        field1 = 0.0
        field2 = 0.0

        totalprec = sum(field3) + sum(field4)
        if (totalprec > 1e-5) then
          write (iulog, *) "found precip in first timestep, assuming ", &
            "empty 0 timestep to deaccumulate precip"
        endif
      endif

      precip(:,:) = ((field3 + field4) - (field1 + field2))/nhdiff

    else if (met_params%total_column_rain /= '') then
      call fi_checkload(fio, met_params%total_column_rain, precip_units, field3(:, :), nt=timepos, nr=nr, nz=1)
      precip(:,:) = field3
      write (error_unit, *) "Check precipation correctness"
    else
      !..non-accumulated emissions in stratiform an convective
      call fi_checkload(fio, met_params%precstrativrt, precip_rate_units, field1(:, :), nt=timepos, nr=nr, nz=1)
      if (met_params%precconvrt /= '') then
        call fi_checkload(fio, met_params%precstrativrt, precip_rate_units, field2(:, :), nt=timepos, nr=nr, nz=1)
      else
        field2 = 0.
      endif

      precip(:,:) = field1 + field2
    end if

    where (precip < 0.0)
      precip = 0.0
    end where
  end subroutine read_precipitation

  subroutine check(status, errmsg)
    integer, intent(in) :: status
    character(len=*), intent(in), optional :: errmsg

    if (status /= 0) then
      if (present(errmsg)) then
        print *, "fimex-io error: ", status, ": ", trim(errmsg)
      else
        print *, "error reading fimex-file:", status
      endif
      error stop 1
    endif
  end subroutine check

  subroutine fi_checkload1d(fio, varname, units, field, nt, nz, nr, ierror)
    !> the fimex io-object
    TYPE(FimexIO), intent(inout) :: fio
    !> variable name in file
    character(len=*), intent(in) :: varname
    !> the requested units, see snapdimML.f90
    character(len=*), intent(in) :: units
    !> optional position on t-axis, default all (1 is first element)
    integer, intent(in), optional :: nz
    !> optional position on z-axis, default all (1 is first element)
    integer, intent(in), optional :: nt
    !> optional position on realization/ensemble axis, default 1
    integer, intent(in), optional :: nr
    !> error code, 0 on success
    integer, intent(out), optional :: ierror
    !> field to read
    real(real32), intent(out) :: field(:)

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr, ierror)

    field(:) = REAL(reshape(zfield, shape(field)), KIND=real32)
    deallocate(zfield)
  end subroutine fi_checkload1d

  subroutine fi_checkload2d(fio, varname, units, field, nt, nz, nr, ierror)
    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(real32), intent(out) :: field(:, :)
    integer, intent(out), optional :: ierror

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr, ierror)

    field(:,:) = REAL(reshape(zfield, shape(field)), KIND=real32)
    deallocate(zfield)
  end subroutine fi_checkload2d

  subroutine fi_checkload2d_64(fio, varname, units, field, nt, nz, nr, ierror)
    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(real64), intent(out) :: field(:, :)
    integer, intent(out), optional :: ierror

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr, ierror)

    field(:,:) = reshape(zfield, shape(field))
    deallocate(zfield)
  end subroutine fi_checkload2d_64

  subroutine fi_checkload3d(fio, varname, units, field, nt, nz, nr, ierror)
    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(real32), intent(out) :: field(:, :, :)
    integer, intent(out), optional :: ierror

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr, ierror)

    field(:,:,:) = REAL(reshape(zfield, shape(field)), KIND=real32)
    deallocate(zfield)
  end subroutine fi_checkload3d

  subroutine fi_checkload3d_64(fio, varname, units, field, nt, nz, nr, ierror)
    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(real64), intent(out) :: field(:, :, :)
    integer, intent(out), optional :: ierror

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr, ierror)

    field(:,:,:) = reshape(zfield, shape(field))
    deallocate(zfield)
  end subroutine fi_checkload3d_64

  !> internal implementation, allocating the zfield
  subroutine fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr, ierror)
    USE Fimex, ONLY: FimexIO, AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat, AXIS_GeoZ, &
                     AXIS_Pressure, AXIS_Height, AXIS_Realization, AXIS_Time
    USE utils, ONLY: itoa
    USE iso_fortran_env, only: int32

    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(kind=real64), dimension(:), allocatable, target, intent(out) :: zfield
    !> Error code, 0 for no error
    integer, intent(out), optional :: ierror

    integer(int32), dimension(:), allocatable :: start, length, atypes
    integer(int32) :: tlength, i, ndims
    integer :: ierr

    if (present(ierror)) then
      ierror = 0
    endif

! Get dimensions
    ndims = fio%get_dimensions(varname)
    if (ndims <= 0) &
      call check(ndims, "can't make slicebuilder for "//TRIM(varname))
    !WRITE(0,*) "get_dimensions: ", ndims

    ALLOCATE (start(ndims))
    ALLOCATE (length(ndims))
    ALLOCATE (atypes(ndims))

    call check(fio%get_dimension_start_size(start, length), "reading dim-sizes for "//TRIM(varname))
    ierr = fio%get_axistypes(atypes)
    ! no axistypes for scalars, so ierr okay
    if (ierr /= 0 .and. ndims > 1) write (error_unit, *) "no dim-types for "//TRIM(varname)

    tlength = 1
    do i = 1, ndims
      !WRITE (*,*) i, " axistype: ", atypes(i)
      !WRITE (*,*) AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat
      SELECT CASE (atypes(i))
      CASE (AXIS_GeoX, AXIS_Lon) ! full x-range
      CASE (AXIS_GeoY, AXIS_Lat) ! full y-range
      CASE (AXIS_Time)
        if (present(nt)) &
          call check(fio%reduce_dimension(fio%get_dimname(i), nt - 1, 1), &
                     "reducing "//TRIM(fio%get_dimname(i))//" to "//itoa(nt)//" for "//TRIM(varname))
      CASE (AXIS_GeoZ, AXIS_Pressure, AXIS_Height)
        if (present(nz)) &
          call check(fio%reduce_dimension(fio%get_dimname(i), nz - 1, 1), &
                     "reducing "//TRIM(fio%get_dimname(i))//" to "//itoa(nz)//" for "//TRIM(varname))
      CASE (AXIS_Realization)
        if (present(nt)) then
          call check(fio%reduce_dimension(fio%get_dimname(i), nr - 1, 1), &
                     "reducing "//TRIM(fio%get_dimname(i))//" to "//itoa(nr)//" for "//TRIM(varname))
        else
          call check(fio%reduce_dimension(fio%get_dimname(i), 0, 1), &
                     "reducing "//TRIM(fio%get_dimname(i))//" to 0 for "//TRIM(varname))
        end if
      CASE DEFAULT
        call check(fio%reduce_dimension(fio%get_dimname(i), 0, 1), &
                   "reducing "//TRIM(fio%get_dimname(i))//" to 0 for "//TRIM(varname))
      END SELECT
    END DO
    call check(fio%get_dimension_start_size(start, length), "reading dim-sizes for "//TRIM(varname))
    tlength = PRODUCT(length)
    allocate (zfield(tlength))
    write (iulog, *) "reading "//trim(varname)//", dims: ", "start(0):", start, " size:", length, " total-size:", tlength
    if (units /= "") then
      ierr = fio%read(varName, zfield, units)
      if (.not.present(ierror)) then
        call check(ierr, "reading '"//varname//"' in unit '"//units//"'")
      else
        ierror = ierr
        return
      endif
    else
      call check(fio%read (varName, zfield), "reading '"//varname//"'")
    end if
    write (iulog, *) "reading "//trim(varname)//", min, max: ", minval(zfield), maxval(zfield)
  end subroutine fi_checkload_intern

  subroutine read_drydep(fio, timepos, timeposm1, nr)
    USE ieee_arithmetic, only: ieee_is_nan
    USE iso_fortran_env, only: real64
    USE snapmetML, only: met_params, &
      temp_units, downward_momentum_flux_units, surface_roughness_length_units, &
      surface_heat_flux_units, leaf_area_index_units
    use drydep, only: drydep_scheme, DRYDEP_SCHEME_EMEP, drydep_emep_vd, &
      DRYDEP_SCHEME_EMERSON, DRYDEP_SCHEME_ZHANG, drydep_zhang_emerson_vd, &
      classnr
    use snapparML, only: ncomp, run_comp, def_comp
    use snapfldML, only: ps2, vd_dep, xflux, yflux, hflux, z0, leaf_area_index, t2m, &
      roa, ustar, monin_l, raero, vs, rs
    type(FimexIO), intent(inout) :: fio
    integer, intent(in) :: timepos, timeposm1
    integer, intent(in) :: nr

    real, allocatable :: tmp1(:, :), tmp2(:, :)
    integer :: i, mm
    real(real64) :: diam, dens

    if (.not.(drydep_scheme == DRYDEP_SCHEME_EMEP .or. &
              drydep_scheme == DRYDEP_SCHEME_EMERSON .or. &
              drydep_scheme == DRYDEP_SCHEME_ZHANG)) then
      return
    endif

    allocate(tmp1, tmp2, MOLD=ps2)


    ! Fluxes are integrated: Deaccumulate
    if (timepos == 1) then
      call fi_checkload(fio, met_params%xflux, downward_momentum_flux_units, xflux(:, :), nt=timepos, nr=nr)
      call fi_checkload(fio, met_params%yflux, downward_momentum_flux_units, yflux(:, :), nt=timepos, nr=nr)
    else
      call fi_checkload(fio, met_params%xflux, downward_momentum_flux_units, tmp1(:, :), nt=timeposm1, nr=nr)
      call fi_checkload(fio, met_params%xflux, downward_momentum_flux_units, tmp2(:, :), nt=timepos, nr=nr)
      xflux(:,:) = tmp2 - tmp1

      call fi_checkload(fio, met_params%yflux, downward_momentum_flux_units, tmp1(:, :), nt=timeposm1, nr=nr)
      call fi_checkload(fio, met_params%yflux, downward_momentum_flux_units, tmp2(:, :), nt=timepos, nr=nr)
      yflux(:,:) = tmp2 - tmp1
    endif
    ! TODO: Normalise by difference between intervals
    xflux(:,:) =  xflux / 3600
    yflux(:,:) =  yflux / 3600

    if (timepos == 1) then
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, hflux(:, :), nt=timepos, nr=nr)
    else if (timepos == 13) then
      ! Weird AROME data is invalid at t=12
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, tmp1(:, :), nt=timeposm1, nr=nr)
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, tmp2(:, :), nt=timepos+1, nr=nr)
      hflux(:,:) = (tmp2 - tmp1)/2
    else if (timepos == 14) then
      ! Weird AROME data is invalid at t=13
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, tmp1(:, :), nt=timeposm1-1, nr=nr)
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, tmp2(:, :), nt=timepos, nr=nr)
      hflux(:,:) = (tmp2 - tmp1)/2
    else
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, tmp1(:, :), nt=timeposm1, nr=nr)
      call fi_checkload(fio, met_params%hflux, surface_heat_flux_units, tmp2(:, :), nt=timepos, nr=nr)
      hflux(:,:) = tmp2 - tmp1
    endif
    ! TODO: Normalise by difference between intervals
    hflux(:,:) = -hflux / 3600 ! Follow conventions for up/down


    call fi_checkload(fio, met_params%z0, surface_roughness_length_units, z0(:, :), nt=timepos, nr=nr)
    call fi_checkload(fio, met_params%leaf_area_index, leaf_area_index_units, leaf_area_index(:, :), nt=timepos, nr=nr)
    where (ieee_is_nan(leaf_area_index))
      leaf_area_index = 0.0
    endwhere
    call fi_checkload(fio, met_params%t2m, temp_units, t2m(:, :), nt=timepos, nr=nr)

    if (drydep_scheme == DRYDEP_SCHEME_EMEP .or. &
        drydep_scheme == DRYDEP_SCHEME_EMERSON .or. &
        drydep_scheme == DRYDEP_SCHEME_ZHANG) then
      do i=1,ncomp
        mm = run_comp(i)%to_defined

        if (def_comp(mm)%kdrydep == 1) then
          diam = 2*def_comp(mm)%radiusmym*1e-6
          dens = def_comp(mm)%densitygcm3*1e3
          select case(drydep_scheme)
          case (DRYDEP_SCHEME_EMEP)
            call drydep_emep_vd(ps2*100, t2m, yflux, xflux, z0, &
              hflux, leaf_area_index, real(diam), real(dens), classnr, vd_dep(:, :, i), &
              roa, ustar, monin_l, raero, vs, rs)
          case (DRYDEP_SCHEME_ZHANG)
            call drydep_zhang_emerson_vd(ps2*100, t2m, yflux, xflux, z0, &
              hflux, leaf_area_index, diam, dens, classnr, vd_dep(:, :, i), .false., &
              roa, ustar, monin_l, raero, vs, rs)
          case (DRYDEP_SCHEME_EMERSON)
            call drydep_zhang_emerson_vd(ps2*100, t2m, yflux, xflux, z0, &
              hflux, leaf_area_index, diam, dens, classnr, vd_dep(:, :, i), .true., &
              roa, ustar, monin_l, raero, vs, rs)
          case default
            error stop "Unreachable"
          end select
        endif
      end do
    endif
  end subroutine

  subroutine read_landfractions(inputfile)
    use snapdimML, only: nx, ny
    use drydep, only: preprocess_landfraction
    use fimex, only: INTERPOL_NEAREST_NEIGHBOR
    use ISO_C_BINDING, only: C_INT
    character(len=*), intent(in) :: inputfile

    type(fimexIO) :: fio, fio_intern

    real(kind=real32), allocatable :: arr(:,:)

    if (fint%method >= 0) then
      call check(fio_intern%open(inputfile, "", "nc4"), &
        "can't open landclass file")
      call check(fio%interpolate(fio_intern, INTERPOL_NEAREST_NEIGHBOR, fint%proj, fint%x_axis, &
                                 fint%y_axis, fint%unit_is_degree), &
                 "Can't interpolate landclass file")
    else
      call check(fio%open(inputfile, "", "nc4"), "Can't open landclass file")
    endif

    allocate(arr(nx, ny))
    call fi_checkload(fio, "Main_Nature_Cover", "1", arr)

    call preprocess_landfraction(arr)
  end subroutine

end module readfield_fiML
