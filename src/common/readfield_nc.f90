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

module readfield_ncML
  USE ftestML, only: ftest
  USE om2edotML, only: om2edot
  USE milibML, only: mapfield
  USE snaptabML, only: t2thetafac
  USE netcdf
  USE snapdebug, only: iulog, idebug

  implicit none
  private

  public :: readfield_nc, check, nfcheckload, calc_2d_start_length, find_index
  public :: compute_vertical_coords, read_largest_landfraction

  interface nfcheckload
    module procedure nfcheckload1d, nfcheckload2d, nfcheckload3d
  end interface

  contains

!..search in list of available timesteps with model level data
!> Next index to read from, ntav == 0 if no available meteo
integer function find_index(first, backward, itimei, ihr1, ihr2) result(ntav)
  USE snapfilML, only: kavail, iavail
  USE datetime, only: datetime_t, duration_t

  !> Whether this is the first input (which ignores ihr1)
  logical, intent(in) :: first
  !> Calculate time backwards
  logical, value :: backward
  !> Last time of reading
  type(datetime_t), intent(in) :: itimei
  !> bounds in the age of the next time
  integer, value :: ihr1, ihr2

  integer :: current_index
  type(datetime_t) :: test_date, itime(2)

  if (first) then
    ihr1 = 0
    ! Search opposite direction to find suitable initial fields
    backward = .not.backward
  endif
  ntav = 0

!..search in list of available timesteps with model level data
  if(.not.backward) then
    current_index = kavail(1)
    itime(1) = itimei + duration_t(ihr1)
    itime(2) = itimei + duration_t(ihr2)
  else
  !..using the backward list
    current_index = kavail(2)
    itime(1) = itimei - duration_t(ihr1)
    itime(2) = itimei - duration_t(ihr2)
  end if

  if (.not.backward) then
    write(iulog,*) '*READFIELD* Requested time: ', itimei
    write(iulog,*) '                Time limit: ', itime(1), itime(2)
  else
    write(iulog,*) '*READFIELD* Requested time: ', itimei
    write(iulog,*) '                Time limit: ', itime(2), itime(1)
  endif

  do while (current_index > 0)
    test_date = datetime_t(iavail(current_index)%aYear, &
                           iavail(current_index)%aMonth, &
                           iavail(current_index)%aDay, &
                           iavail(current_index)%aHour) + &
                duration_t(iavail(current_index)%fcHour)

    !..pointer to next timestep (possibly same time)
    if (.not.backward) then
      if (test_date >= itime(1) .and. test_date <= itime(2)) then
        ntav = current_index
        exit
      endif
      current_index = iavail(current_index)%nAvail
    else
      if (test_date <= itime(1) .and. test_date >= itime(2)) then
        ntav = current_index
        exit
      endif
      current_index = iavail(current_index)%pAvail
    end if
  end do
end function

!> Read fields from NetCDF files
subroutine readfield_nc(istep, backward, itimei, ihr1, ihr2, &
    itimefi,ierror)
  USE iso_fortran_env, only: error_unit
  USE snapfilML, only: iavail, filef
  USE snapfldML, only: &
      xm, ym, u1, u2, v1, v2, w1, w2, t1, t2, ps1, ps2, pmsl1, pmsl2, &
      hbl1, hbl2, hlayer1, hlayer2, garea, hlevel1, hlevel2, &
      hlayer1, hlayer2, bl1, bl2, enspos, precip, &
      t1_abs, t2_abs, field1
  USE snapgrdML, only: alevel, blevel, vlevel, ahalf, bhalf, vhalf, &
      gparam, klevel, ivlevel, imslp, igtype, ivlayer
  USE snapmetML, only: met_params, requires_precip_deaccumulation, &
      pressure_units, xy_wind_units, temp_units
  USE snapdimML, only: nx, ny, nk, output_resolution_factor, hres_field, surface_index
  USE datetime, only: datetime_t, duration_t
!> current timestep (always positive), negative istep means reset
  integer, intent(in) :: istep
!> whether meteorology should be read backwards
  logical, intent(in) :: backward
!> minimal time-offset after itimei
  integer, intent(in) :: ihr1
!> maximal time-offset after itimei
  integer, intent(in) :: ihr2
!> time since last file input
  type(datetime_t), intent(in) :: itimei
!> final time (output)
  type(datetime_t), intent(out) :: itimefi
!> error (output)
  integer, intent(out) :: ierror

! local variables
  integer, save :: ncid = 0
  integer, save :: ntav1, ntav2 = 0
  character(len=1024), save :: file_name = ""
  logical, save :: first_time_read = .true.

  integer :: i, j, k, ilevel, i1, i2
  integer :: nhdiff, nhdiff_precip
  real :: alev(nk), blev(nk), dxgrid, dygrid
  real :: p, px, ptop, p0
  real :: ptoptmp(1), p0tmp(1)
  integer :: prev_tstep_same_file

  integer :: timepos, timeposm1
  integer :: start3d(7), start4d(7), count3d(7), count4d(7)

  ierror = 0

  if (istep < 0) then
  ! set 'save' variables to default values,
  ! ncid not needed, will close automatically
    ntav1 = 0
    ntav2 = 0
  end if

!..get time offset in hours (as iavail(n)%oHour)
  ntav1 = ntav2
  ntav2 = find_index(istep < 0, backward, itimei, ihr1, ihr2)

  if(ntav2 == 0) then
    write(iulog,*) '*READFIELD* No model level data available'
    write(error_unit,*) '*READFIELD* No model level data available'
    ierror=1
    return
  end if


  if(idebug == 1) then
    write(iulog,*) 'MODEL LEVEL SEARCH LIST.   ntav2=',ntav2
    write(iulog,*) 'nx,ny,nk: ',nx,ny,nk
    write(iulog,*) 'istep: ',istep
    write(iulog,*) 'itimei(5), ihr1, ihr2:',itimei,ihr1,ihr2
    write(iulog,fmt='(7(1x,i4),1x,i6,2i5)') (iavail(ntav2))
    flush(iulog)
  end if

! time between two inputs
! open the correct file, if required
  if (file_name /= filef(iavail(ntav2)%fileNo)) then
    if (ncid /= 0) then
      call check(nf90_close(ncid), "close ncid")
    end if
    file_name = filef(iavail(ntav2)%fileNo)
    call check(nf90_open(file_name, NF90_NOWRITE, ncid), file_name)
  end if

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

  if(idebug == 1) then
    write(iulog,*) 'READING DATA FROM file=',trim(file_name)
    write(iulog,*) 'READING DATA FROM position=',timepos, ' for ', &
      itimefi, ', prev. position=',timeposm1,', hours:',nhdiff
  end if




  if( .TRUE. ) then
  !..move data from input time step 2 to 1

    u1(:,:,:) = u2
    v1(:,:,:) = v2
    w1(:,:,:) = w2
    t1(:,:,:) = t2
    if (allocated(t2_abs)) t1_abs(:,:,:) = t2_abs
    hlevel1(:,:,:) = hlevel2
    hlayer1(:,:,:) = hlayer2

    ps1(:,:) = ps2
    bl1(:,:) = bl2
    hbl1(:,:) = hbl2

    if(imslp /= 0) then
      pmsl1(:,:) = pmsl2
    end if

  end if

  if (met_params%ptopv /= '') then
    call nfcheckload(ncid, met_params%ptopv, (/0/), (/1/), ptoptmp, units=pressure_units)
    ptop = ptoptmp(1)
  else
    ptop= 100.0
  end if

  if (met_params%p0 /= '') then
    call nfcheckload(ncid, met_params%p0, (/0/), (/1/), p0tmp, units=pressure_units)
    p0 = p0tmp(1)
  else
    p0 = 1.0
  end if

  do k=nk,2,-1

  !..input model level no.
    ilevel=klevel(k)

    ! dummy-dim only on 2d
    call calc_2d_start_length(start4d, count4d, nx, ny, ilevel, &
        enspos, timepos, has_2d_dummy_height=.false.)

  !..u
  !     Get the varid of the data variable, based on its name.
    call nfcheckload(ncid, met_params%xwindv, start4d, count4d, u2(:,:,k), units=xy_wind_units)

  !..v
    call nfcheckload(ncid, met_params%ywindv, start4d, count4d, v2(:,:,k), units=xy_wind_units)
  ! bug in chernobyl borders from destaggering
    where (v2 >= 1e+30)
      v2 = 0.0
    end where

  !..pot.temp. or abs.temp.
    call nfcheckload(ncid, met_params%pottempv, start4d, count4d, t2(:,:,k), units=temp_units)

    if (met_params%apv /= '') then
      if (met_params%p0 /= '') then
        call nfcheckload(ncid, met_params%apv, [ilevel], [1], alev(k:k), units="1")
        alev(k) = alev(k) * ptoptmp(1)
      else
        call nfcheckload(ncid, met_params%apv, [ilevel], [1], alev(k:k), units=pressure_units)
      end if
    end if
    if (met_params%bv /= '') then
      call nfcheckload(ncid, met_params%bv, [ilevel], [1], blev(k:k), units="1")
    end if
    if (met_params%sigmav /= '') then
    ! reusing blev(k) for sigma(k) later
      call nfcheckload(ncid, met_params%sigmav, [ilevel], [1], blev(k:k))
    end if

  !..sigma_dot/eta_dot (0 at surface)
  !..eta: eta_dot (or omega) stored in the same levels as u,v,th.
    if (met_params%sigmadotv == '') then
      w2 = 0
    else
      call nfcheckload(ncid, met_params%sigmadotv, &
          start4d, count4d, w2(:,:,k))
    end if

  end do ! k=nk,2,-1


!..surface pressure, 10m wind and possibly mean sea level pressure,
!..precipitation
  call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
      enspos, timepos, met_params%has_dummy_dim)


! ps
  call nfcheckload(ncid, met_params%psv, start3d, count3d, ps2(:,:), units=pressure_units)

! u10m
! v10m
  if (.not.met_params%use_model_wind_for_10m) then
    call nfcheckload(ncid, met_params%xwind10mv, start3d, count3d, u2(:,:,1), units=xy_wind_units)
    call nfcheckload(ncid, met_params%ywind10mv, start3d, count3d, v2(:,:,1), units=xy_wind_units)
  else
    if (enspos >= 0) then
      call nfcheckload(ncid, met_params%xwindv, [1, 1, enspos+1, surface_index, timepos], &
          [nx, ny, 1, 1, 1], u2(:,:,1), units=xy_wind_units)
      call nfcheckload(ncid, met_params%ywindv, [1, 1, enspos+1, surface_index, timepos], &
          [nx, ny, 1, 1, 1], v2(:,:,1), units=xy_wind_units)
    else
      call nfcheckload(ncid, met_params%xwindv, [1, 1, surface_index, timepos], &
          [nx, ny, 1, 1], u2(:,:,1), units=xy_wind_units)
      call nfcheckload(ncid, met_params%ywindv, [1, 1, surface_index, timepos], &
          [nx, ny, 1, 1], v2(:,:,1), units=xy_wind_units)
    endif
  endif

!..mean sea level pressure, not used in computations,
!..(only for output to results file)
  if(imslp /= 0) then
    if (met_params%mslpv /= '') then
      call nfcheckload(ncid, met_params%mslpv, start3d, count3d, pmsl2(:,:), units=pressure_units)
    else if (met_params%psv /= '') then
      call nfcheckload(ncid, met_params%psv, start3d, count3d, pmsl2(:,:), units=pressure_units)
    else
      write(iulog,*) 'Mslp not found. Not important.'
      imslp=0
    end if
  end if

  if (met_params%need_precipitation) then
    call read_precipitation(ncid, nhdiff_precip, timepos, timeposm1)
  else
    precip = 0.0
  endif

  call read_drydep_required_fields(ncid, timepos, timeposm1, itimefi)

! first time initialized data
  if (first_time_read) then
    first_time_read = .false.

    call compute_vertical_coords(alev, blev, ptop)

  !..compute map ratio
    call mapfield(1,0,igtype,gparam,nx,ny,xm,ym,&
        xm, & ! Ignored when icori = 0
        dxgrid,dygrid,ierror)
    if(ierror /= 0) then
      write(iulog,*) 'MAPFIELD ERROR. ierror= ',ierror
      write(error_unit,*) 'MAPFIELD ERROR. ierror= ',ierror
      error stop 255
    end if
    gparam(7)=dxgrid
    gparam(8)=dygrid
      !..set garea size of each grid square (m**2) in output grid-size
    field1 = abs((dxgrid/xm)*(dygrid/ym)) / (output_resolution_factor*output_resolution_factor)
    call hres_field(field1, garea, .true.)

  ! end initialization
  end if

  if (met_params%temp_is_abs) then
    if (allocated(t2_abs)) t2_abs(:,:,:) = t2
  !..abs.temp. -> pot.temp.
    do k=2,nk
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
      do k=2,nk
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
  w2(:,:,1) = 0.0

!..no temperature at or near surface (not used, yet)
  t2(:,:,1) = -999.0

  if(backward) then
  ! backward-calculation, switch sign of winds
    u2 = -u2
    v2 = -v2
    w2 = -w2
  end if


! test---------------------------------------------------------------
  write(iulog,*) 'k,k_model,alevel,blevel,vlevel,p,dp:'
  px=alevel(nk)+blevel(nk)*1000.
  do k=nk,1,-1
    p=alevel(k)+blevel(k)*1000.
    write(iulog,fmt='(1x,2i5,f9.2,2f9.5,f8.0,f6.0)') &
        k,klevel(k),alevel(k),blevel(k),vlevel(k),p,p-px
    px=p
  end do

! test---------------------------------------------------------------

  if(idebug == 1) then
    call ftest('u  ', u2, reverse_third_dim=.true.)
    call ftest('v  ', v2, reverse_third_dim=.true.)
    call ftest('w  ', w2, reverse_third_dim=.true.)
    call ftest('t  ', t2, reverse_third_dim=.true.)
    call ftest('ps ', ps2)
    if (istep > 0) &
      call ftest('pre', precip(:,:))
  end if


  if (istep == 0) then
  ! test---------------------------------------------------------------
    write(iulog,*) 'k,ahalf,bhalf,vhalf,p,dp:'
    px=ahalf(nk)+bhalf(nk)*1000.
    do k=nk,1,-1
      p=ahalf(k)+bhalf(k)*1000.
      write(iulog,fmt='(1x,i5,f9.2,2f9.5,f8.0,f6.0)') &
          k,ahalf(k),bhalf(k),vhalf(k),p,p-px
      px=p
    end do
  ! test---------------------------------------------------------------

  !..level table for (vertical) interpolation
  !..(remember that fields are stored bottom to top
  !.. and that all parameters now are in the same levels)
    write(iulog,*) 'ivlevel:'
    write(iulog,*) 'k,i1,i2,vlevel(k+1),vlevel(k)'
    i2=-1
    do k=nk-1,1,-1
      i1=i2+1
      i2=vlevel(k)*10000.
      if(k == 1) i2=10000
      do i=i1,i2
        ivlevel(i)=k
      end do
      write(iulog,*) k,i1,i2,vlevel(k+1),vlevel(k)
    end do

  !..level table for concentration in each sigma/eta layer
  !..(layers here as in the input model, no '10m' layer,
  !.. but ordering bottom to top, reorder at time of output)
    write(iulog,*) 'ivlayer:'
    write(iulog,*) 'k,i1,i2,vhalf(k+1),vhalf(k)'
    i2=-1
    do k=nk-1,1,-1
      i1=i2+1
      i2=nint(vhalf(k)*10000.)
      if(k == 1) i2=10000
      do i=i1,i2
        ivlayer(i)=k
      end do
      write(iulog,*) k,i1,i2,vhalf(k+1),vhalf(k)
    end do
  end if

end subroutine readfield_nc

!> Reads `units` attribute of the precipitation variable
subroutine read_precip_unit_scale(ncid, varname, unitScale)
  use snapmetML, only: precip_units, precip_units_fallback
  integer, intent(in) :: ncid
  character(len=*), intent(in) :: varname
  !> Scaling factor to multiply input value to obtain the value
  !> in model units
  !> Defaults to 1.0 on error on reading the netCDF file
  real, intent(out) :: unitScale

  integer :: attr_len
  character(len=:), allocatable :: attr

  integer :: varid
  integer :: nferr

  unitScale = 1.0

  nferr = nf90_inq_varid(ncid, varname, varid)
  if (nferr /= NF90_NOERR) return

  nferr = nf90_inquire_attribute(ncid, varid, "units", len=attr_len)
  if (nferr /= NF90_NOERR) return
  allocate(character(len=attr_len) :: attr)
  nferr = nf90_get_att(ncid, varid, "units", attr)
  if (nferr /= NF90_NOERR) return

  select case (attr)
    case ("m")
      unitScale = 1000.0
    case (precip_units, precip_units_fallback)
      unitScale = 1.0
  end select
end subroutine

!> read precipitation
subroutine read_precipitation(ncid, nhdiff, timepos, timeposm1)
  use iso_fortran_env, only: error_unit
  use snapdebug, only: iulog
  use snapmetML, only: met_params
  use snapfldML, only: field1, field2, field3, field4, precip, &
      enspos
  use snapdimML, only: nx, ny
  USE snapfilML, only: nctype
  use wetdepML, only: requires_extra_precip_fields, wetdep_precompute

!> open netcdf file
  integer, intent(in) :: ncid
!> time difference in hours between two precip fields
  integer, intent(in) :: nhdiff
!> timestep in file
  integer, intent(in) :: timepos
!> previous timestep
  integer, intent(in) :: timeposm1


  integer :: start3d(7), count3d(7)
  real :: unitScale
  real :: totalprec

  if (met_params%precaccumv /= '') then
  !..precipitation between input time 't1' and 't2'
    if (timepos == 1) then
      ! Can not deaccumulate using this file only,
      ! assume first timestep is accumulated from start of run
      field1 = 0.0
    else
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
          enspos, timeposm1, met_params%has_dummy_dim)
      call nfcheckload(ncid, met_params%precaccumv, &
          start3d, count3d, field1(:,:))
    endif

    call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
          enspos, timepos, met_params%has_dummy_dim)
    call nfcheckload(ncid, met_params%precaccumv, &
          start3d, count3d, field2(:,:))

    precip(:, :) = (field2 - field1)/nhdiff
  else if (met_params%precstratiaccumv /= '') then
    ! accumulated stratiform and convective precipitation
    !..precipitation between input time 't1' and 't2'
    call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
          enspos, timepos, met_params%has_dummy_dim)
    call nfcheckload(ncid, met_params%precstratiaccumv, &
          start3d, count3d, field3)
    call nfcheckload(ncid, met_params%precconaccumv, &
          start3d, count3d, field4)

    if (timepos /= 1) then
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
          enspos, timeposm1, met_params%has_dummy_dim)
      call nfcheckload(ncid, met_params%precstratiaccumv, &
          start3d, count3d, field1)
      call nfcheckload(ncid, met_params%precconaccumv, &
          start3d, count3d, field2)
    else
      ! assuming empty 0 timestep to deaccumulate precip"
      field1 = 0.0
      field2 = 0.0
      totalprec = sum(field3) + sum(field4)

      if (totalprec > 1e-5) then
        write(iulog,*) "found precip in first timestep, assuming ", &
            "empty 0 timestep to deaccumulate precip"
      endif
    endif

    call read_precip_unit_scale(ncid, met_params%precstratiaccumv, unitScale)

    precip(:,:) = (field3 + field4) - (field1 + field2)
    precip(:,:) = precip/nhdiff*unitScale
  else if (met_params%total_column_rain /= '') then
      call calc_2d_start_length(start3d, count3d, nx, ny, 1, &
          enspos, timepos, met_params%has_dummy_dim)
      call nfcheckload(ncid, met_params%total_column_rain, &
          start3d, count3d, field3)
      precip(:,:) = field3
      write(error_unit, *) "Check precipation correctness"
  else
  !..non-accumulated emissions in stratiform and convective
    call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
              enspos, timepos, met_params%has_dummy_dim)
    call nfcheckload(ncid, met_params%precstrativrt, &
        start3d, count3d, field1(:,:))
    if (met_params%precconvrt /= '') then
      call nfcheckload(ncid, met_params%precconvrt, &
          start3d, count3d, field2(:,:))
    else
      field2 = 0.
    endif

    unitScale = 3600.*1000.0 ! m/s -> mm/h
    if (nctype == 'gfs_grib_filter_fimex') unitScale = 3600. !kg/m3/s -> mm/h
    precip(:,:) = (field1 + field2)*unitScale
  end if

  where (precip < 0.0)
    precip = 0.0
  end where

  if (requires_extra_precip_fields()) then
    if ((met_params%mass_fraction_rain_in_air /= "") .and. &
        (met_params%mass_fraction_cloud_condensed_water_in_air /= "")) then
      call read_extra_precipitation_fields(ncid, timepos)
    else if (met_params%mass_fraction_cloud_condensed_water_in_air /= "") then
      call read_extra_precipitation_fields_infer_3d_precip(ncid, timepos)
    else
      error stop "Can not read extra 3D precipition for this meteorology"
    endif
  endif

  call wetdep_precompute()
end subroutine


  subroutine read_extra_precipitation_fields(ncid, timepos)
    use iso_fortran_env, only: error_unit, real32
    use snaptabML, only: g
    use snapfldML, only: ps2, precip3d, cw3d, cloud_cover, enspos
    use snapgrdML, only: ahalf, bhalf, klevel
    use snapdimML, only: nx, ny, nk
    use snapmetML, only: met_params
!> open netcdf file
    integer, intent(in) :: ncid
!> timestep in file
    integer, intent(in) :: timepos
    integer :: start(7), count(7)

    real(real32), allocatable :: rain_in_air(:,:), graupel_in_air(:,:), snow_in_air(:,:)
    real(real32), allocatable :: cloud_water(:,:), cloud_ice(:,:)
    real(real32), allocatable :: pdiff(:,:)

    integer :: ilevel, k, nr

!.. get the correct ensemble/realization position, nr starting with 1, enspos starting with 0
    nr = enspos + 1
    if (enspos <= 0) nr = 1

    allocate(rain_in_air(nx,ny),graupel_in_air(nx,ny),snow_in_air(nx,ny),pdiff(nx,ny))
    allocate(cloud_water(nx,ny),cloud_ice(nx,ny))

    precip3d(:,:,:) = 0.0
    cw3d(:,:,:) = 0.0

    do k=nk,2,-1
      ilevel = klevel(k)
      call calc_2d_start_length(start, count, nx, ny, ilevel, &
          enspos, timepos, met_params%has_dummy_dim)
      call nfcheckload(ncid, met_params%mass_fraction_rain_in_air, start, count, rain_in_air)
      if (met_params%mass_fraction_graupel_in_air /= "") then
        call nfcheckload(ncid, met_params%mass_fraction_graupel_in_air, start, count, graupel_in_air)
      else
        graupel_in_air(:,:) = 0.0
      endif
        call nfcheckload(ncid, met_params%mass_fraction_snow_in_air, start, count, snow_in_air)

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

      call nfcheckload(ncid, met_params%mass_fraction_cloud_condensed_water_in_air, start, count, cloud_water)
      call nfcheckload(ncid, met_params%mass_fraction_cloud_ice_in_air, start, count, cloud_ice)

      where (cloud_water < 0.0)
        cloud_water = 0.0
      end where
      where (cloud_ice < 0.0)
        cloud_ice = 0.0
      end where
      cw3d(:,:,k) = cloud_water + cloud_ice
      cw3d(:,:,k) = cw3d(:,:,k) * pdiff / g

      call nfcheckload(ncid, met_params%cloud_fraction, start, count, cloud_cover(:,:,k))
    enddo
  end subroutine

  !> Read and convert fields from ecemep input to what we need for 3D precip
  subroutine read_extra_precipitation_fields_infer_3d_precip(ncid, timepos)
    use iso_fortran_env, only: error_unit, real32
    use snaptabML, only: g
    use snapfldML, only: ps2, precip3d, cw3d, cloud_cover, enspos, precip
    use snapgrdML, only: ahalf, bhalf, klevel
    use snapdimML, only: nx, ny, nk
    use snapmetML, only: met_params
!> open netcdf file
    integer, intent(in) :: ncid
!> timestep in file
    integer, intent(in) :: timepos
    integer :: start(7), count(7)

    real(real32), allocatable :: normaliser(:,:)
    real(real32), allocatable :: pdiff(:,:)
    real(real32), allocatable :: cloud_water(:,:), cloud_ice(:,:)

    integer :: ilevel, k, nr, i, j

!.. get the correct ensemble/realization position, nr starting with 1, enspos starting with 0
    nr = enspos + 1
    if (enspos <= 0) nr = 1

    allocate(pdiff(nx,ny))
    allocate(normaliser(nx,ny))
    allocate(cloud_water(nx,ny), cloud_ice(nx,ny))

    precip3d(:,:,:) = 0.0
    cw3d(:,:,:) = 0.0

    normaliser(:,:) = 0.0

    do k=nk,2,-1
      ilevel = klevel(k)
      call calc_2d_start_length(start, count, nx, ny, ilevel, &
          enspos, timepos, met_params%has_dummy_dim)

      pdiff(:,:) = 100*( (ahalf(k-1) - ahalf(k)) + (bhalf(k-1) - bhalf(k))*ps2 )
      call nfcheckload(ncid, met_params%mass_fraction_cloud_condensed_water_in_air, start, count, cloud_water(:,:))
      call nfcheckload(ncid, met_params%mass_fraction_cloud_ice_in_air, start, count, cloud_ice(:,:))

      cw3d(:,:,k) = (abs(cloud_water(:,:)) + abs(cloud_ice(:,:))) * pdiff / g

      ! Use cloud water to assign precipitation at model levels
      normaliser(:,:) = normaliser + cw3d(:,:,k)
      precip3d(:,:,k) = precip * cw3d(:,:,k)

      call nfcheckload(ncid, met_params%cloud_fraction, start, count, cloud_cover(:,:,k))
    enddo

    block
    use snapgrdML, only: ivlevel
    integer :: klimit
    klimit = ivlevel(nint(0.67*10000.0))
    do k=nk,2,-1
      do j=1,ny
        do i=1,nx
          if (normaliser(i,j) > 0.0) then
            precip3d(i,j,k) = precip3d(i,j,k) / normaliser(i,j)
          elseif (k == klimit) then
            ! Put all precip at level closest to 0.67, analoguous
            ! with the old formulation of the precipitation
            precip3d(i,j,k) = precip(i,j)
          endif
        enddo
      enddo
    enddo
    end block
  end subroutine
  


!> calculate the start and length paramters for slicing
!> a 2d field from a 3-5d dataset
subroutine calc_2d_start_length(start, length, nx, ny, zpos, &
    enspos, tpos, has_2d_dummy_height)
  !> Offset into field
  integer, intent (out) :: start(7)
  !> Length of each field
  integer, intent(out) :: length(7)
  !> size (x) of field
  integer, intent (in) :: nx
  !> size (y) of field
  integer, intent(in) :: ny
  !> hybrid position
  integer, intent(in) :: zpos
  !> ensemble member, should be given in C notation,
  !> with 0 being the first member. Setting to a negative
  !> value disables this.
  integer, intent(in) :: enspos
  !> time on which to compute
  integer, intent(in) :: tpos
  !> if set to true, an empty dimension (size 1) has been used
  !> for height. Ignored if \p zpos >= 1
  logical, intent (in) :: has_2d_dummy_height

  integer :: pos

  start = 1
  length = 1

  length(1) = nx
  length(2) = ny

  pos = 2
! ensemble enspos given in C notation, e.g. first member = 0
  if (enspos >= 0) then
    pos = pos + 1
    start(pos) = enspos+1
  end if
! z
  if (zpos >= 1) then
    pos = pos + 1
    start(pos) = zpos
  elseif (has_2d_dummy_height) then
    pos = pos + 1
  end if
! time
  pos = pos + 1
  start(pos) = tpos

end subroutine calc_2d_start_length


subroutine check(status, errmsg)
  use ISO_FORTRAN_ENV, only: OUTPUT_UNIT, ERROR_UNIT
  use snapdebug, only: iulog, idebug
  integer, intent ( in) :: status
  character(len=*), intent(in), optional :: errmsg

  if(status /= NF90_NOERR) then
    if (present(errmsg)) then
      write(OUTPUT_UNIT,*) trim(nf90_strerror(status)), ": ", trim(errmsg)
      write(ERROR_UNIT,*) trim(nf90_strerror(status)), ": ", trim(errmsg)
      if (idebug == 1) write(iulog,*) trim(nf90_strerror(status)), ": ", trim(errmsg)
    else
      write(OUTPUT_UNIT, *) trim(nf90_strerror(status))
      write(ERROR_UNIT, *) trim(nf90_strerror(status))
      if (idebug == 1) write(iulog, *) trim(nf90_strerror(status))
    endif
    error stop 1
  endif
end subroutine check

subroutine fillscaleoffset(ncid, varid, fillvalue, scalefactor, offset, status)
  use iso_fortran_env, only: real32

  integer, intent(in) :: ncid, varid
  real(kind=real32), intent(out) :: fillvalue, scalefactor, offset
  integer, intent(out) :: status

  status = nf90_get_att(ncid, varid, "_FillValue", fillvalue)
  if (status == NF90_ENOTATT) then
    fillvalue = NF90_FILL_FLOAT
  else if (status /= NF90_NOERR) then
    return
  endif

  status = nf90_get_att(ncid, varid, "scale_factor", scalefactor)
  if (status == NF90_ENOTATT) then
    scalefactor = 1
  else if (status /= NF90_NOERR) then
    return
  endif

  status = nf90_get_att(ncid, varid, "add_offset", offset)
  if (status == NF90_ENOTATT) then
    offset = 0
    status = NF90_NOERR
  else if (status /= NF90_NOERR) then
    return
  endif
end subroutine fillscaleoffset

real function conversion_factor(current_units, target_units)
  USE iso_fortran_env, only: error_unit
  character(len=*), intent(in) :: current_units
  character(len=*), intent(in) :: target_units

  if (current_units == target_units) then
    conversion_factor = 1.0
  else if (current_units == "Pa" .and. target_units == "hPa") then
    conversion_factor = 0.01
  else
    write(error_unit, *) "Conversion ", current_units, " to ", target_units
    write(error_unit, *) "Conversion is not supported"
    write(error_unit, *) "Continuing as if the units are the same..."
    conversion_factor = 1.0
  endif
end function

subroutine get_conversion_factor(ncid, varid, target_units, factor)
  integer, intent(in) :: ncid
  integer, intent(in) :: varid
  character(len=*), intent(in) :: target_units
  real, intent(out) :: factor

  character(len=:), allocatable :: current_units
  integer :: attr_len
  integer :: nferr

  factor = 1.0

  nferr = nf90_inquire_attribute(ncid, varid, "units", len=attr_len)
  if (nferr /= NF90_NOERR) return
  allocate(character(len=attr_len) :: current_units)
  nferr = nf90_get_att(ncid, varid, "units", current_units)
  if (nferr /= NF90_NOERR) return

  factor = conversion_factor(current_units, target_units)
end subroutine

subroutine nfcheckload1d(ncid, varname, start, length, field, return_status, units)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  use iso_fortran_env, only: real32

  integer, intent(in) :: ncid, start(:), length(:)
  character(len=*), intent(in) :: varname
  real(real32), intent(out) :: field(:)
  !> Return status instead of panic
  integer, intent(out), optional :: return_status
  character(len=*), intent(in), optional :: units

  real(real32) :: factor, offset, fillvalue
  integer :: varid, status

  if (present(return_status)) return_status = NF90_NOERR

  status = nf90_inq_varid(ncid, varname, varid)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status, varname)

  write (iulog,*) "reading "//trim(varname)//", dims: ", "start(1):",start, " size:",length
  status = nf90_get_var(ncid, varid, field, start=start, count=length)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status, varname)

  call fillscaleoffset(ncid, varid, fillvalue, factor, offset, status)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status)

  where (field == fillvalue)
    field = IEEE_VALUE(fillvalue, IEEE_QUIET_NAN)
  end where

  if (factor /= 1. .OR. offset /= 0.) then
    field = field*factor + offset
  end if

  if (present(units)) then
    call get_conversion_factor(ncid, varid, units, factor)
    field = field*factor
  endif
end subroutine nfcheckload1d

subroutine nfcheckload2d(ncid, varname, start, length, field, return_status, units)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  use iso_fortran_env, only: real32

  integer, intent(in) :: ncid, start(:), length(:)
  character(len=*), intent(in) :: varname
  real(real32), intent(out) :: field(:,:)
  !> Return status instead of panic
  integer, intent(out), optional :: return_status
  character(len=*), intent(in), optional :: units

  real(real32) :: factor, offset, fillvalue
  integer :: varid, status

  if (present(return_status)) return_status = NF90_NOERR

  status = nf90_inq_varid(ncid, varname, varid)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status, varname)

  write (iulog,*) "reading "//trim(varname)//", dims: ", "start(1):",start, " size:",length
  status = nf90_get_var(ncid, varid, field, start=start, count=length)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status, varname)

  call fillscaleoffset(ncid, varid, fillvalue, factor, offset, status)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status)

  where (field == fillvalue)
    field = IEEE_VALUE(fillvalue, IEEE_QUIET_NAN)
  end where

  if (factor /= 1. .OR. offset /= 0.) then
    field = field*factor + offset
  end if

  if (present(units)) then
    call get_conversion_factor(ncid, varid, units, factor)
    field = field*factor
  endif
end subroutine nfcheckload2d


subroutine nfcheckload3d(ncid, varname, start, length, field, return_status, units)
  use ieee_arithmetic, only: ieee_value, IEEE_QUIET_NAN
  use iso_fortran_env, only: real32

  integer, intent(in) :: ncid, start(:), length(:)
  character(len=*), intent(in) :: varname
  real(real32), intent(out) :: field(:,:,:)
  !> Return status instead of panic
  integer, intent(out), optional :: return_status
  character(len=*), intent(in), optional :: units

  real(real32) :: factor, offset, fillvalue
  integer :: varid, status

  if (present(return_status)) return_status = NF90_NOERR

  status = nf90_inq_varid(ncid, varname, varid)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status, varname)

  write (iulog,*) "reading "//trim(varname)//", dims: ", "start(1):",start, " size:",length
  status = nf90_get_var(ncid, varid, field, start=start, count=length)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status, varname)

  call fillscaleoffset(ncid, varid, fillvalue, factor, offset, status)
  if (status /= NF90_NOERR .and. present(return_status)) then
    return_status = status
    return
  endif
  call check(status)

  where (field == fillvalue)
    field = IEEE_VALUE(fillvalue, IEEE_QUIET_NAN)
  end where

  if (factor /= 1. .OR. offset /= 0.) then
    field = field*factor + offset
  end if

  if (present(units)) then
    call get_conversion_factor(ncid, varid, units, factor)
    field = field*factor
  endif
end subroutine nfcheckload3d

subroutine compute_vertical_coords(alev, blev, ptop)
  use iso_fortran_env, only: error_unit
  use snapgrdML, only: alevel, blevel, vlevel, klevel, &
                       ahalf, bhalf, vhalf
  use snapmetML, only: met_params
  use snapdimML, only: nk
  use snaptabML, only: standard_atmosphere

  real, intent(in) :: alev(:)
  real, intent(in) :: blev(:)
  real, intent(in) :: ptop

  integer :: k

  do k = 2, nk
    alevel(k) = alev(k)
    blevel(k) = blev(k)
  end do

  !..surface
  alevel(1) = 0.0
  blevel(1) = 1.0

  !..eta (hybrid) levels ... vlevel=eta (eta as defined in Hirlam)
  vlevel(:) = alevel/standard_atmosphere + blevel

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
      vhalf(k) = ahalf(k)/standard_atmosphere + bhalf(k)
    else
      ahalf(k) = (alevel(k) + alevel(k + 1))*0.5
      bhalf(k) = (blevel(k) + blevel(k + 1))*0.5
      vhalf(k) = ahalf(k)/standard_atmosphere + bhalf(k)
    end if
  end do
  ahalf(nk) = alevel(nk)
  bhalf(nk) = blevel(nk)
  vhalf(nk) = vlevel(nk)
end subroutine

  subroutine read_drydep_required_fields(ncid, timepos, timeposm1, itimefi)
    USE ieee_arithmetic, only: ieee_is_nan
    USE iso_fortran_env, only: real64
    use datetime, only: datetime_t
    use snapmetML, only: met_params
    use snapfldML, only: xflux, yflux, hflux, z0, leaf_area_index, t2m, vd_dep, roa, ustar, monin_l, &
      ps2, rs, raero, vs, enspos
    use drydepml, only: classnr, requires_extra_fields_to_be_read, drydep_precompute
    use snapdimML, only: nx, ny
    use snapparML, only: ncomp, run_comp, def_comp
    
    integer, intent(in) :: ncid
    integer, intent(in) :: timepos
    integer, intent(in) :: timeposm1
    type(datetime_t), intent(in) :: itimefi

    integer :: start(7), startm1(7)
    integer :: count(7)
    integer :: i, mm
    real(real64) :: diam, dens

    real, allocatable :: tmp1(:, :), tmp2(:, :)

    if (.not.requires_extra_fields_to_be_read()) then
      return
    endif

    call calc_2d_start_length(start, count, nx, ny, -1, &
          enspos, timepos, met_params%has_dummy_dim)
    call calc_2d_start_length(startm1, count, nx, ny, -1, &
          enspos, timeposm1, met_params%has_dummy_dim)

    allocate(tmp1(nx,ny), tmp2(nx,ny))

    ! Fluxes are integrated: Deaccumulate
    if (timepos == 1) then
      call nfcheckload(ncid, met_params%xflux, start, count, xflux(:,:))
      call nfcheckload(ncid, met_params%yflux, start, count, yflux(:,:))
    else
      call nfcheckload(ncid, met_params%xflux, start, count, tmp1(:,:))
      call nfcheckload(ncid, met_params%xflux, startm1, count, tmp2(:,:))
      xflux(:,:) = tmp2 - tmp1
      call nfcheckload(ncid, met_params%yflux, start, count, tmp1(:,:))
      call nfcheckload(ncid, met_params%yflux, startm1, count, tmp2(:,:))
      yflux(:,:) = tmp2 - tmp1
    endif
    ! TODO: Normalise by difference between intervals
    xflux(:,:) =  xflux / 3600
    yflux(:,:) =  yflux / 3600

    if (timepos == 1) then
      call nfcheckload(ncid, met_params%hflux, start, count, hflux(:,:))
    else
      call nfcheckload(ncid, met_params%hflux, start, count, tmp1(:,:))
      call nfcheckload(ncid, met_params%hflux, start, count, tmp2(:,:))
      hflux(:,:) = tmp2 - tmp1
    endif
    ! TODO: Normalise by difference between intervals
    hflux(:,:) = -hflux / 3600 ! Follow conventions for up/down

    call nfcheckload(ncid, met_params%z0, start, count, z0(:, :))

    if (met_params%leaf_area_index /= "") then
      call nfcheckload(ncid, met_params%leaf_area_index, start, count, leaf_area_index(:,:))
    else ! Leaf area index may be split into patches which must be combined
      block
        real, allocatable :: leaf_area_index_p1(:,:), leaf_area_index_p2(:,:)
        allocate(leaf_area_index_p1, leaf_area_index_p2, mold=leaf_area_index)
        call nfcheckload(ncid, met_params%leaf_area_index_p1, start, count, leaf_area_index_p1(:,:))
        call nfcheckload(ncid, met_params%leaf_area_index_p2, start, count, leaf_area_index_p2(:,:))

        where (.not.ieee_is_nan(leaf_area_index_p1) .and. .not.ieee_is_nan(leaf_area_index_p2))
          leaf_area_index = max(leaf_area_index_p1, leaf_area_index_p2)
        elsewhere (.not.ieee_is_nan(leaf_area_index_p1))
          leaf_area_index = leaf_area_index_p1
        elsewhere (.not.ieee_is_nan(leaf_area_index_p2))
          leaf_area_index = leaf_area_index_p2
        elsewhere
          leaf_area_index = 0.0
        endwhere

      end block
    endif
    where (ieee_is_nan(leaf_area_index))
      leaf_area_index = 0.0
    endwhere

    call nfcheckload(ncid, met_params%t2m, start, count, t2m(:, :))

    do i=1,ncomp
      mm = run_comp(i)%to_defined

      if (def_comp(mm)%kdrydep == 1) then
        diam = 2*def_comp(mm)%radiusmym*1e-6
        dens = def_comp(mm)%densitygcm3*1e3
        call drydep_precompute(ps2*100, t2m, yflux, xflux, z0, &
            hflux, leaf_area_index, real(diam), real(dens), classnr, vd_dep(:, :, i), &
            roa, ustar, monin_l, raero, vs, rs, itimefi)
      endif
    end do
  end subroutine

  subroutine read_largest_landfraction(inputfile)
    use ieee_arithmetic, only: ieee_is_nan
    use iso_fortran_env, only: real32
    use snapdimML, only: nx, ny
    use drydepml, only: preprocess_landfraction
    use ISO_C_BINDING, only: C_INT
    character(len=*), intent(in) :: inputfile

    ! type(fimexIO) :: fio, fio_intern
    !
    ! Assert some properties, e.g. nx and ny are matching the current grid
    integer :: nx_i, ny_i, attlen
    integer :: ncid, varid, dimids(2), ndims
    character(len=:), allocatable :: flag_attr
    character(len=*), parameter :: flag_attr_expected = "11: Sea, 12: Inland water, 13: Tundra/desert, &
      &14: Ice and ice sheets, 15: Urban, 16: Crops, 17: Grass, 18: Wetlands, &
      &19: Evergreen needleleaf, 20: Deciduous broadleaf, 21: Mixed forest, &
      &22: Shrubs and interrupted woodlands"

    real(kind=real32), allocatable :: arr(:,:)

    call check(nf90_open(inputfile, NF90_NOWRITE, ncid), inputfile)
    call check(nf90_inq_varid(ncid, "Main_Nature_Cover", varid), "Reading variable Main_Nature_cover")
    call check(nf90_inquire_variable(ncid, varid, ndims=ndims))
    if (ndims /= 2) then
      error stop "read_largest_landfraction: Main_Nature_Cover must have two dimensions"
    endif
    call check(nf90_inquire_variable(ncid, varid, dimids=dimids(:)))
    call check(nf90_inquire_dimension(ncid, dimids(1), len=nx_i))
    if (nx_i /= nx) then
      error stop "read_largest_landfraction: Mismatch in nx"
    endif
    call check(nf90_inquire_dimension(ncid, dimids(2), len=ny_i))
    if (ny_i /= ny) then
      error stop "read_largest_landfraction: Mismatch in ny"
    endif

    call check(nf90_inquire_attribute(ncid, varid, "comment", len=attlen), "length of 'comment' attribute")
    allocate(character(len=attlen)::flag_attr)
    call check(nf90_get_att(ncid, varid, "comment", flag_attr))
    if (flag_attr /= flag_attr_expected) then
      error stop "Expected flags are not the same as actual"
    endif

    allocate(arr(nx, ny))
    call check(nf90_get_var(ncid, varid, arr, start=[1, 1], count=[nx, ny]), "read_var")

    where (ieee_is_nan(arr))
      arr = 11  ! Assume water where not defined
    endwhere

    call preprocess_landfraction(arr)
  end subroutine
end module readfield_ncML
