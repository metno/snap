! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2020   Norwegian Meteorological Institute

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
  use iso_fortran_env, only: real32, real64
  USE ftestML, only: ftest
  USE om2edotML, only: om2edot
  USE milibML, only: mapfield, hrdiff
  USE snaptabML, only: t2thetafac
  USE snapdebug, only: iulog, idebug

  implicit none
  private

  !> fimex filetype, e.g. netcdf, grib, ncml for all files. All files have same type
  character(len=1024), private, save :: file_type = ""
  !> config file, applied to all files
  character(len=1024), private, save :: conf_file = ""

  public fi_init, readfield_fi, check

  !> @brief load and check an array from a source
  interface fi_checkload
    module procedure fi_checkload1d, fi_checkload2d, fi_checkload3d
  end interface

contains

!> Initialize readfield module
  subroutine fi_init(filetype, configfile)
!> filetype of all input-files, e.g. ncml, netcdf, grib
    character(len=1024), intent(in) :: filetype
!> optional config-file
    character(len=1024), optional, intent(in) :: configfile
    file_type = filetype
    if (.not. PRESENT(configfile)) then
      conf_file = ""
    else
      conf_file = configfile
    end if
  end subroutine fi_init

!> Read fields from fimex files. (see fimex.F90)
  subroutine readfield_fi(istep, nhleft, itimei, ihr1, ihr2, itimefi, ierror)
    USE iso_fortran_env, only: error_unit
    USE snapfilML, only: nctype, itimer, kavail, iavail, filef
    USE snapfldML, only: &
      xm, ym, u1, u2, v1, v2, w1, w2, t1, t2, ps1, ps2, pmsl1, pmsl2, &
      hbl1, hbl2, hlayer1, hlayer2, garea, dgarea, hlevel1, hlevel2, &
      hlayer1, hlayer2, bl1, bl2, enspos, nprecip, precip
    USE snapgrdML, only: alevel, blevel, vlevel, ahalf, bhalf, vhalf, &
                         gparam, kadd, klevel, ivlevel, imslp, igtype, ivlayer, ivcoor
    USE snapmetML, only: met_params, xy_wind_units, pressure_units, omega_units, &
                         precip_rate_units, precip_units, temp_units
    USE snapdimML, only: nx, ny, nk
!> current timestep (always positive), negative istep means reset
    integer, intent(in) :: istep
!> remaining run-hours (negative for backward-calculations)
    integer, intent(inout) :: nhleft
!> minimal time-offset?
    integer, intent(inout) :: ihr1
!> maximal time-offset?
    integer, intent(inout) :: ihr2
!> initial time
    integer, intent(inout) :: itimei(5)
!> final time (output)
    integer, intent(out) :: itimefi(5)
!> error (output)
    integer, intent(out) :: ierror

! local variables
    TYPE(FimexIO) :: fio
    integer, save :: ntav1, ntav2 = 0
    character(len=1024), save :: file_name = ""

    integer :: i, j, k, n, ilevel, ierr1, ierr2, i1, i2
    integer :: itime(5, 4), ihours(4)
    integer :: ihdif1, ihdif2, nhdiff
    real :: alev(nk), blev(nk), db, dxgrid, dygrid
    integer :: kk, ifb, kfb
    real :: dred, red, p, px, dp, p1, p2, ptop
    real :: ptoptmp(1)
    real, parameter :: mean_surface_air_pressure = 1013.26

    integer :: timepos, timeposm1, nr

    if (istep < 0) then
      ! set all 'save' variables to default values,
      ntav1 = 0
      ntav2 = 0
      return
    end if

!.. get the correct ensemble/realization position, nr starting with 1, enspos starting with 0
    nr = enspos + 1
    if (enspos <= 0) nr = 1

!..get time offset in hours (as iavail(n)%oHour)
    if (nhleft < 0) then
      ihr1 = -ihr1
      ihr2 = -ihr2
    end if
    ihours = [ihr1, ihr2, 0, nhleft]
    do n = 1, 4
      itime(:, n) = itimei
      itime(5, n) = itime(5, n) + ihours(n)
      call hrdiff(0, 1, itimer(1, 1), itime(1, n), ihours(n), ierr1, ierr2)
    end do
    ihdif1 = ihours(1)
    ihdif2 = ihours(2)

    write (iulog, *) '*READFIELD* Requested time: ', (itime(i, 1), i=1, 4)
    write (iulog, *) '                Time limit: ', (itime(i, 2), i=1, 4)
    write (iulog, *) '                 ihr1,ihr2: ', ihr1, ihr2

!..search in list of available timesteps with model level data
    if (ihdif1 > ihdif2) then
      !..using the backward list
      i = ihdif1
      ihdif1 = ihdif2
      ihdif2 = i
      kfb = 2
      ifb = 10
    else
      kfb = 1
      ifb = 9
    end if

    ntav1 = ntav2
    ntav2 = 0
    n = kavail(kfb)
    do while ((ntav2 == 0) .AND. (n > 0))
      if (iavail(n)%oHour >= ihdif1 .AND. &
          iavail(n)%oHour <= ihdif2) then
        ntav2 = n
      end if
      !..pointer to next timestep (possibly same time)
      if (ifb == 9) then
        n = iavail(n)%nAvail
      else
        n = iavail(n)%pAvail
      end if
    end do

    if (ntav2 < 1) then
      write (iulog, *) '*READFIELD* No model level data available'
      write (error_unit, *) '*READFIELD* No model level data available'
      ierror = 1
      return
    end if

    if (idebug == 1) then
      write (iulog, *) 'MODEL LEVEL SEARCH LIST.   ntav2=', ntav2
      write (iulog, *) 'nx,ny,nk: ', nx, ny, nk
      write (iulog, *) 'istep,nhleft: ', istep, nhleft
      write (iulog, *) 'itimei(5), ihr1, ihr2:', (itimei(i), i=1, 5), ihr1, ihr2
      write (iulog, *) 'kfb,ifb,ihdif1,ihdif2:', kfb, ifb, ihdif1, ihdif2
      write (iulog, fmt='(7(1x,i4),1x,i6,2i5)') (iavail(ntav2))
      flush (iulog)
    end if

! time between two inputs
! open the correct file, if required
    if (file_name /= filef(iavail(ntav2)%fileNo)) then
      call check(fio%close (), "close fio")
    end if
    file_name = filef(iavail(ntav2)%fileNo)
    call check(fio%open (file_name, conf_file, file_type), &
               "Can't make io-object with file:"//trim(file_name)//" config: "//conf_file)

!     set timepos and nhdiff
    nhdiff = 3
    if (ntav1 /= 0) &
      nhdiff = abs(iavail(ntav2)%oHour - iavail(ntav1)%oHour)
    nprecip = nhdiff

    timepos = iavail(ntav2)%timePos
    timeposm1 = timepos ! Default: No deaccumulation possible
    if (ntav1 /= 0) then
      if (iavail(ntav2)%fileNo .eq. iavail(ntav1)%fileNo) then
        ! previous timestep in same file for deaccumulation
        timeposm1 = iavail(ntav1)%timePos
      endif
    endif
    itimefi(1) = iavail(ntav2)%aYear
    itimefi(2) = iavail(ntav2)%aMonth
    itimefi(3) = iavail(ntav2)%aDay
    itimefi(4) = iavail(ntav2)%aHour
    itimefi(5) = iavail(ntav2)%fcHour

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
      hlevel1(:, :, :) = hlevel2
      hlayer1(:, :, :) = hlayer2

      ps1(:, :) = ps2
      bl1(:, :) = bl2
      hbl1(:, :) = hbl2

      if (imslp /= 0) then
        pmsl1(:, :) = pmsl2
      end if

    end if

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
      end if
      !..alevel (here) only for eta levels
      if (.NOT. met_params%apv == '') then
        call fi_checkload(fio, met_params%apv, pressure_units, alev(k:k), nz=ilevel)
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
        call fi_checkload(fio, met_params%sigmadotv, omega_units, w2(:, :, k), nt=timepos, nz=ilevel, nr=nr)
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
      call fi_checkload(fio, met_params%xwindv, xy_wind_units, u2(:, :, 1), nt=timepos, nr=nr, nz=1)
      call fi_checkload(fio, met_params%ywindv, xy_wind_units, v2(:, :, 1), nt=timepos, nr=nr, nz=1)
    endif

!..mean sea level pressure, not used in computations,
!..(only for output to results file)
    if (imslp /= 0) then
      if (.NOT. met_params%mslpv == '') then
        write (iulog, *) 'Mslp not found. Not important.'
        imslp = 0
      else
        call fi_checkload(fio, met_params%mslpv, pressure_units, pmsl2(:, :), nt=timepos, nr=nr, nz=1)
      end if
    end if

    if (met_params%need_precipitation) then
      call read_precipitation(fio, nhdiff, timepos, timeposm1)
    else
      precip = 0.0
    endif

! first time initialized data
    if (istep == 0) then

      do k = 2, nk - kadd
        alevel(k) = alev(k)
        blevel(k) = blev(k)
      end do

      if (kadd > 0) then
        if (ivcoor == 2) then
          !..sigma levels ... blevel=sigma
          db = blevel(nk - kadd - 1) - blevel(nk - kadd)
          db = max(db, blevel(nk - kadd)/float(kadd))
          do k = nk - kadd + 1, nk
            blevel(k) = max(blevel(k - 1) - db, 0.)
          end do
        elseif (ivcoor == 10) then
          !..eta (hybrid) levels
          p1 = alevel(nk - kadd) + blevel(nk - kadd)*1000.
          p2 = alevel(nk - kadd - 1) + blevel(nk - kadd - 1)*1000.
          dp = p2 - p1
          if (p1 - dp*kadd < 10.) dp = (p1 - 10.)/kadd
          db = blevel(nk - kadd - 1) - blevel(nk - kadd)
          db = max(db, blevel(nk - kadd)/float(kadd))
          do k = nk - kadd + 1, nk
            p1 = p1 - dp
            blevel(k) = max(blevel(k - 1) - db, 0.)
            alevel(k) = p1 - blevel(k)*1000.
          end do
        else
          write (error_unit, *) 'PROGRAM ERROR.  ivcoor= ', ivcoor
          error stop 255
        end if
      end if

      if (ivcoor == 2) then
        !..sigma levels (norlam)
        do k = 2, nk
          alevel(k) = ptop*(1.-blevel(k))
        end do
      end if

      !..surface
      alevel(1) = 0.
      blevel(1) = 1.

      if (ivcoor == 2) then
        !..sigma levels ... vlevel=sigma
        vlevel(:) = blevel
      elseif (ivcoor == 10) then
        !..eta (hybrid) levels ... vlevel=eta (eta as defined in Hirlam)
        vlevel(:) = alevel/mean_surface_air_pressure + blevel
      else
        write (error_unit, *) 'PROGRAM ERROR.  ivcoor= ', ivcoor
        error stop 255
      end if

      !..half levels where height is found,
      !..alevel and blevel are in the middle of each layer
      ahalf(1) = alevel(1)
      bhalf(1) = blevel(1)
      vhalf(1) = vlevel(1)
      !..check if subselection of levels
      do k = 2, nk - 1
        if (klevel(k + 1) /= klevel(k) - 1) then
          met_params%manual_level_selection = .TRUE.
        endif
      end do
      do k = 2, nk - 1
        if (.NOT. met_params%manual_level_selection) then
          ahalf(k) = alevel(k) + (alevel(k) - ahalf(k - 1))
          bhalf(k) = blevel(k) + (blevel(k) - bhalf(k - 1))
          vhalf(k) = ahalf(k)/mean_surface_air_pressure + bhalf(k)
        else
          ahalf(k) = (alevel(k) + alevel(k + 1))*0.5
          bhalf(k) = (blevel(k) + blevel(k + 1))*0.5
          vhalf(k) = ahalf(k)/mean_surface_air_pressure + bhalf(k)
        end if
      end do
      ahalf(nk) = alevel(nk)
      bhalf(nk) = blevel(nk)
      vhalf(nk) = vlevel(nk)

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
      !..size of each grid square (m**2)
      garea(:, :) = abs((dxgrid/xm)*(dygrid/ym))
      dgarea(:, :) = garea

      ! end initialization
    end if

    if (met_params%temp_is_abs) then
      !..abs.temp. -> pot.temp.
      do k = 2, nk - kadd
        associate(p => alevel(k) + blevel(k)*ps2(:,:))
          t2(:,:,k) = t2(:,:,k)*t2thetafac(p)
        end associate
      end do
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

    if (nhleft < 0) then
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
        call ftest('pre', precip(:, :, 1:nprecip))
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

    return
  end subroutine readfield_fi

!> read precipitation
  subroutine read_precipitation(fio, nhdiff, timepos, timeposm1)
    use iso_fortran_env, only: error_unit
    use snapdebug, only: iulog
    use snapmetML, only: met_params, xy_wind_units, pressure_units, omega_units, &
                         precip_rate_units, precip_units, temp_units
    use snapfldML, only: field1, field2, field3, field4, precip, &
                         enspos, precip, nprecip
    use snapdimML, only: nx, ny
    USE snapfilML, only: nctype

!> open netcdf file
    TYPE(FimexIO), intent(inout) :: fio
!> time difference in hours between two precip fields
    integer, intent(in) :: nhdiff
!> timestep in file
    integer, intent(in) :: timepos
!> previous timestep
    integer, intent(in) :: timeposm1

    integer :: start3d(7), count3d(7)
    integer :: i, j, nr
    real :: precip1
    real :: unitScale
    real :: totalprec

!.. get the correct ensemble/realization position, nr starting with 1, enspos starting with 0
    nr = enspos + 1
    if (enspos <= 0) nr = 1

    if (met_params%precaccumv /= '') then
      !..precipitation between input time 't1' and 't2'
      if (timepos /= 1) then
        call fi_checkload(fio, met_params%precaccumv, precip_units, field1(:, :), nt=timeposm1, nr=nr, nz=1)
        call fi_checkload(fio, met_params%precaccumv, precip_units, field2(:, :), nt=timepos, nr=nr, nz=1)

        !..the difference below may get negative due to different scaling
        do j = 1, ny
          do i = 1, nx
            precip1 = max(field2(i, j) - field1(i, j), 0.)/nhdiff
            precip(i, j, 1:nprecip) = precip1
          end do
        end do
      end if
    else if (met_params%precstratiaccumv /= '') then
      ! accumulated stratiform and convective precipitation
      !..precipitation between input time 't1' and 't2'
      if (timepos /= 1) then
        call fi_checkload(fio, met_params%precstratiaccumv, precip_units, field1(:, :), nt=timeposm1, nr=nr, nz=1)
        call fi_checkload(fio, met_params%precconaccumv, precip_units, field2(:, :), nt=timeposm1, nr=nr, nz=1)
        call fi_checkload(fio, met_params%precstratiaccumv, precip_units, field3(:, :), nt=timepos, nr=nr, nz=1)
        call fi_checkload(fio, met_params%precconaccumv, precip_units, field4(:, :), nt=timepos, nr=nr, nz=1)

        do j = 1, ny
          do i = 1, nx
            precip1 = max(field3(i, j) + field4(i, j) - &
                          (field1(i, j) + field2(i, j)), 0.)/nhdiff
            precip(i, j, :) = precip1
          end do
        end do
      else
        ! timepos eq 1, check if precipitation already present / assume dummy step 0
        call fi_checkload(fio, met_params%precstratiaccumv, precip_units, field3(:, :), nt=timepos, nr=nr, nz=1)
        call fi_checkload(fio, met_params%precconaccumv, precip_units, field4(:, :), nt=timepos, nr=nr, nz=1)

        field1 = 0.0
        field2 = 0.0
        totalprec = sum(field3) + sum(field4)

        if (totalprec > 1e-5) then
          !..the difference below may get negative due to different scaling
          write (iulog, *) "found precip in first timestep, assuming ", &
            "empty 0 timestep to deaccumulate precip"
          unitScale = 1.
          do j = 1, ny
            do i = 1, nx
              precip1 = max(field3(i, j) + field4(i, j) - &
                            (field1(i, j) + field2(i, j)), 0.)/nhdiff
              precip(i, j, :) = precip1*unitScale
            end do
          end do
        endif
      end if
    else if (met_params%total_column_rain /= '') then
      call fi_checkload(fio, met_params%total_column_rain, precip_units, field3(:, :), nt=timepos, nr=nr, nz=1)
      do j = 1, ny
        do i = 1, nx
          precip(i, j, 1:nprecip) = field3(i, j)
        enddo
      enddo
      write (error_unit, *) "Check precipation correctness"
    else
      !..non-accumulated emissions in stratiform an convective
      call fi_checkload(fio, met_params%precstrativrt, precip_rate_units, field1(:, :), nt=timepos, nr=nr, nz=1)
      if (met_params%precconvrt /= '') then
        call fi_checkload(fio, met_params%precstrativrt, precip_rate_units, field2(:, :), nt=timepos, nr=nr, nz=1)
      else
        field2 = 0.
      endif

      do j = 1, ny
        do i = 1, nx
          !..precipitation must be larger 0
          precip1 = max(field1(i, j) + field2(i, j), 0.)
          precip(i, j, :) = precip1
        end do
      end do

    end if
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

  subroutine fi_checkload1d(fio, varname, units, field, nt, nz, nr)
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
    !> field to read
    real(real32), intent(out) :: field(:)

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr)

    field(:) = reshape(zfield, shape(field))
    deallocate(zfield)
  end subroutine fi_checkload1d

  subroutine fi_checkload2d(fio, varname, units, field, nt, nz, nr)
    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(real32), intent(out) :: field(:, :)

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr)

    field(:,:) = reshape(zfield, shape(field))
    deallocate(zfield)
  end subroutine fi_checkload2d

  subroutine fi_checkload3d(fio, varname, units, field, nt, nz, nr)
    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(real32), intent(out) :: field(:, :, :)

    real(kind=real64), dimension(:), allocatable, target :: zfield

    call fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr)

    field(:,:,:) = reshape(zfield, shape(field))
    deallocate(zfield)
  end subroutine fi_checkload3d

  !> internal implementation, allocating the zfield
  subroutine fi_checkload_intern(fio, varname, units, zfield, nt, nz, nr)
    USE Fimex, ONLY: FimexIO, AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat, AXIS_GeoZ, &
                     AXIS_Pressure, AXIS_Height, AXIS_Realization, AXIS_Time
    USE utils, ONLY: itoa
    USE iso_fortran_env, only: int32

    TYPE(FimexIO), intent(inout) :: fio
    character(len=*), intent(in) :: varname, units
    integer, intent(in), optional :: nt, nz, nr
    real(kind=real64), dimension(:), allocatable, target, intent(out) :: zfield

    integer(int32), dimension(:), allocatable :: start, length, atypes
    integer(int32) :: tlength, i, ndims

! Get dimensions
    ndims = fio%get_dimensions(varname)
    if (ndims <= 0) &
      call check(ndims, "can't make slicebuilder for "//TRIM(varname))
    !WRITE(0,*) "get_dimensions: ", ndims

    ALLOCATE (start(ndims))
    ALLOCATE (length(ndims))
    ALLOCATE (atypes(ndims))

    call check(fio%get_dimension_start_size(start, length), "reading dim-sizes for "//TRIM(varname))
    call check(fio%get_axistypes(atypes), "reading dim-types for "//TRIM(varname))

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
      call check(fio%read (varName, zfield, units), "reading '"//varname//"' in unit '"//units//"'")
    else
      call check(fio%read (varName, zfield), "reading '"//varname//"'")
    end if
    write (iulog, *) "reading "//trim(varname)//", min, max: ", minval(zfield), maxval(zfield)
  end subroutine fi_checkload_intern

end module readfield_fiML
