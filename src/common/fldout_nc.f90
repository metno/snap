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

module fldout_ncML
  USE iso_fortran_env, only: real32, real64, error_unit
  USE readfield_ncML, only: check
  USE milibML, only: xyconvert
  USE snapdimML, only: mcomp
  USE netcdf
  USE Utils, only: itoa
  USE datetime
  implicit none
  private

  public :: fldout_nc, initialize_output, accumulate_fields, unload

!> fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
  real, parameter :: cscale = 1.0
!> fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m3)
  real, parameter :: dscale = 1.0


!> Computation mode for computing the surface concentration
  logical, save, public :: surface_layer_is_lowest_level = .true.
!> Height of surface layer for concentrations
  real, save, public :: surface_height_m = 30.0

!> Variables for each component
  type :: component_var
    integer :: icbl
    integer :: acbl
    integer :: idd
    integer :: iwd
    integer :: accdd
    integer :: accwd
    integer :: ac
    integer :: ic
    integer :: icml
    integer :: conc_column = -1
    !> Dry deposition velocity
    integer :: vd = -1
    !> Wet scavenging rate
    integer :: wetscavrate = -1
  end type

!> Variables in a file
  type :: common_var
    integer :: accum_prc
    integer :: instant_prc = -1
    integer :: prc
    integer :: mslp
    integer :: icblt
    integer :: acblt
    integer :: act
    integer :: iddt
    integer :: iwdt
    integer :: accddt
    integer :: accwdt
    integer :: ihbl
    integer :: ahbl
    integer :: ps
    integer :: t
    integer :: k
    integer :: ap
    integer :: b
    integer :: column_max_conc
    integer :: aircraft_doserate
    integer :: aircraft_doserate_threshold_height
    integer :: components
    type(component_var) :: comp(mcomp)
    integer :: xflux = -1
    integer :: yflux = -1
    integer :: hflux = -1
    integer :: z0 = -1
    integer :: t2m = -1
    integer :: lai = -1
    integer :: roa = -1
    integer :: ustar = -1
    integer :: monin_l = -1
    integer :: raero = -1
    integer :: vs = -1
    integer :: rs = -1
    integer :: ps_vd = -1
    integer :: landfraction = -1
    integer :: wetdeprate = -1
  end type

!> dimensions used in a file
  type :: common_dim
    integer :: x
    integer :: y
    integer :: k
    integer :: t
    integer :: nocomp
    integer :: maxcompname
  end type

  !> file base time
  type(datetime_t), save :: iftime

  !> Number of times fields has been accumulated before
  !> being flushed to file
  integer, save :: naverage = 0

  !> Massbalance
  character(len=256), save, public :: massbalance_filename = ""
  integer, save, allocatable :: massbalance_file

  logical, save, public :: output_wetdeprate = .false.

  contains

subroutine fldout_nc(filename, itime,tf1,tf2,tnow, &
    ierror)
  USE iso_fortran_env, only: int16
  USE array_utils, only: is_in_array
  USE snapgrdML, only: imodlevel, modlevel_is_average, imslp, precipitation_in_output, &
      itotcomp, compute_column_max_conc, compute_aircraft_doserate, &
      aircraft_doserate_threshold, output_column, &
      output_column, output_vd, output_vd_debug
  USE snapfldML, only: field_hr1, field_hr2, field_hr3, hbl_hr
  USE snapfldML, only: field1, &
      depdry, depwet, &
      avgbq1, avgbq2, garea, pmsl1, pmsl2, hbl1, hbl2, &
      accdry, accwet, avgprec, concen, ps1, ps2, avghbl, &
      concacc, accprec, max_column_concentration, aircraft_doserate, &
      aircraft_doserate_threshold_height, &
      total_activity_released, total_activity_lost_domain, total_activity_lost_other, &
      vd_dep, precip3d
  USE snapparML, only: output_component, def_comp, nocomp
  USE snapdebug, only: iulog, idebug
  USE ftestML, only: ftest
  USE snapdimML, only: nx, ny, output_resolution_factor, hres_field, hres_pos
  USE releaseML, only: npart
  USE particleML, only: pdata, Particle

  !> File which have been prefilled by initialize_output
  !> This file will be opened and closed on each iteration to
  !> allow e.g. diana to extract information while running snap
  character(len=*), intent(in) :: filename
  type(datetime_t), intent(in) :: itime
  real, intent(in) :: tf1
  real, intent(in) :: tf2
  real, intent(in) :: tnow
  integer, intent(out) :: ierror

  integer :: iunit
  integer :: ipos(3), isize(3)
  integer, save :: ihrs, ihrs_pos

  integer :: nptot1,nptot2
  real(real64) :: bqtot1,bqtot2
  real(real64) :: dblscale

  integer :: i,j,m,n
  integer(int16), pointer, dimension(:) :: mm
  logical :: compute_total_dry_deposition
  logical :: compute_total_wet_deposition
  real :: rt1,rt2,scale,average
  type(common_var) :: varid

  real, parameter :: undef = NF90_FILL_FLOAT

  type(Particle) :: part
  type(duration_t) :: dur

  compute_total_dry_deposition = any(def_comp%kdrydep == 1) .and. nocomp > 1
  compute_total_wet_deposition = any(def_comp%kwetdep == 1) .and. nocomp > 1

  ierror=0

!..output...............................................................
  call check(nf90_open(filename, NF90_WRITE, iunit), filename)

  call get_varids(iunit, varid, ierror)
  call check(ierror, "Obtaining variable ids")

! set the runtime
  ihrs_pos = ihrs_pos + 1
  dur = itime - iftime
  ihrs = dur%hours
  call check(nf90_put_var(iunit, varid%t, start=[ihrs_pos], values=FLOAT(ihrs)), &
      "set time")

  ipos = [1, 1, ihrs_pos]
  isize = [nx*output_resolution_factor, ny*output_resolution_factor, 1]

  average=float(naverage)
  naverage=0

!..for linear interpolation in time
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)

!..surface pressure (if model level output, for vertical crossections)
  if(imodlevel) then
    field1(:,:) = rt1*ps1 + rt2*ps2
    if(idebug == 1) call ftest('ps', field1)
    call hres_field(field1, field_hr1)
    call check(nf90_put_var(iunit, varid%ps, start=ipos, count=isize, &
        values=field_hr1), "set_ps")
  end if

!..total accumulated precipitation from start of run
  if(precipitation_in_output) then
    accprec(:,:) = accprec + avgprec
    field1(:,:) = accprec
    if(idebug == 1) call ftest('accprec', field1)
    call hres_field(field1, field_hr1)
    call check(nf90_put_var(iunit, varid%accum_prc, start=ipos, count=isize, &
        values=field_hr1), "set_accum_prc")

    if (allocated(precip3d)) then
      field1(:,:) = precip3d(:,:,2)
      call check(nf90_put_var(iunit, varid%instant_prc, start=ipos, count=isize, &
          values=field1), "set_accum_prc")
    endif
  end if

!..mslp (if switched on)
  if(imslp == 1) then
    field1(:,:) = rt1*pmsl1 + rt2*pmsl2
    if(idebug == 1) call ftest('mslp', field1)
    call hres_field(field1, field_hr1)
    call check(nf90_put_var(iunit, varid%mslp, start=ipos, count=isize, &
        values=field_hr1), "set_mslp")
  end if

!..instant height of boundary layer
  field1(:,:) = rt1*hbl1 + rt2*hbl2
  if(idebug == 1) call ftest('hbl', field1)
  call hres_field(field1, hbl_hr)
  call check(nf90_put_var(iunit, varid%ihbl, start=ipos, count=isize, &
      values=hbl_hr), "set_ihbl")


!..average height of boundary layer
  field1(:,:) = avghbl / average
  if(idebug == 1) call ftest('avghbl', field1)
  call hres_field(field1, field_hr1)
  call check(nf90_put_var(iunit, varid%ahbl, start=ipos, count=isize, &
      values=field_hr1), "set_ahbl")

!..precipitation accummulated between field output
  if(precipitation_in_output) then
    field1(:,:) = avgprec
    if(idebug == 1) call ftest('prec', field1)
    call hres_field(field1, field_hr1)
    call check(nf90_put_var(iunit, varid%prc, start=ipos, count=isize, &
        values=field_hr1), "set_prc")
  end if

  if (compute_column_max_conc) then
    call check(nf90_put_var(iunit, varid%column_max_conc, &
      start=ipos, count=isize, &
      values=max_column_concentration(:,:)), "column_max_concentration")
  endif
  if (compute_aircraft_doserate) then
    call check(nf90_put_var(iunit, varid%aircraft_doserate, &
      start=ipos, count=isize, &
      values=aircraft_doserate(:,:)), "aircraft_doserate")
    if (aircraft_doserate_threshold > 0.0) then
      call check(nf90_put_var(iunit, varid%aircraft_doserate_threshold_height, &
        start=ipos, count=isize, &
        values=aircraft_doserate_threshold_height(:,:)), "aircraft threshold height")
    endif
  endif

!..parameters for each component......................................

  all_components: do m=1,nocomp

    mm => output_component(m)%to_defined
    write(iulog,*) ' component: ', output_component(m)%name

  !..instant Bq in and above boundary layer
    field_hr1 = 0.0
    field_hr2 = 0.0
    bqtot1 = 0.0
    bqtot2 = 0.0
    nptot1 = 0
    nptot2 = 0

    do n=1,npart
      part = pdata(n)
      if (is_in_array(mm , part%icomp)) then
        i = hres_pos(part%x)
        j = hres_pos(part%y)
        if(part%z >= part%tbl) then
          field_hr1(i,j) = field_hr1(i,j) + part%rad()
          bqtot1 = bqtot1 + dble(part%rad())
          nptot1 = nptot1 + 1
        else
          field_hr2(i,j) = field_hr2(i,j) + part%rad()
          bqtot2 = bqtot2 + dble(part%rad())
          nptot2 = nptot2 + 1
        end if
      end if
    end do

    if (output_column) then
      field_hr3 = field_hr1 + field_hr2
      field_hr3 = field_hr3 / garea
      call check(nf90_put_var(iunit, varid%comp(m)%conc_column, start=ipos, count=isize, &
        values=field_hr3), "output_column")
    endif

  !..instant part of Bq in boundary layer
    scale = 100.
    where (field_hr1 + field_hr2 > 0.0)
      field_hr3 = scale*field_hr1 / (field_hr1 + field_hr2)
    elsewhere
      field_hr3 = undef
    endwhere
    if(idebug == 1) call ftest('pbq', field_hr3, contains_undef=.true.)

  !..instant concentration in boundary layer
    field_hr2(:,:) = cscale*field_hr1 / (hbl_hr*garea)
    if(idebug == 1) call ftest('conc', field_hr2)

    call check(nf90_put_var(iunit, varid%comp(m)%icbl, start=ipos, count=isize, &
        values=field_hr2), "set_icbl")

  !..average concentration in boundary layer
    call hres_field(real(avghbl, kind=real32), field_hr2)
    field_hr1(:,:) = cscale*avgbq1(:,:,m)/(garea*field_hr2)
    if(idebug == 1) call ftest('avgconc', field_hr1)

    call check(nf90_put_var(iunit, varid%comp(m)%acbl, start=ipos, count=isize, &
        values=field_hr1), "set_acbl")

  !..dry deposition
    if (output_component(m)%has_drydep) then
      field_hr1(:,:) = dscale*sngl(depdry(:,:,m)) / garea
      if(idebug == 1) call ftest('dry', field_hr1)

      call check(nf90_put_var(iunit, varid%comp(m)%idd, start=ipos, count=isize, &
          values=field_hr1), "set_idd(m)")

      if (output_vd) then
        call hres_field(vd_dep(:,:,m), field_hr2)
        call check(nf90_put_var(iunit, varid%comp(m)%vd, start=ipos, count=shape(field_hr2), &
            values=field_hr2), "dry_deposition_velocity(m)")
      endif
    end if

  !..accumulated dry deposition
    if (output_component(m)%has_drydep) then
      accdry(:,:,m) = accdry(:,:,m) + depdry(:,:,m)
      field_hr1(:,:) = dscale*sngl(accdry(:,:,m))/garea
      if(idebug == 1) call ftest('adry', field_hr1)

      call check(nf90_put_var(iunit, varid%comp(m)%accdd, start=ipos, count=isize, &
          values=field_hr1), "set_accdd(m)")
    end if

  !..wet deposition
    if (output_component(m)%has_wetdep) then
      field_hr1(:,:) = dscale*sngl(depwet(:,:,m))/garea
      if(idebug == 1) call ftest('wet', field_hr1)

      call check(nf90_put_var(iunit, varid%comp(m)%iwd, start=ipos, count=isize, &
          values=field_hr1), "set_iwd(m)")
    end if

  !..accumulated wet deposition
    if (output_component(m)%has_wetdep) then
      accwet(:,:,m) = accwet(:,:,m) + depwet(:,:,m)
      field_hr1(:,:) = dscale*sngl(accwet(:,:,m))/garea
      if(idebug == 1) call ftest('awet', field_hr1)

      call check(nf90_put_var(iunit, varid%comp(m)%accwd, start=ipos, count=isize, &
          values=field_hr1), "set_accwd(m)")
    end if

    if (output_component(m)%has_wetdep) then
      block
        use snapfldML, only: wscav
        use snapdimML, only: nk
        integer :: k
        if (output_wetdeprate) then
          do k=2,nk
            call hres_field(wscav(:,:,k,m), field_hr1)
            call check(nf90_put_var(iunit, varid%comp(m)%wetscavrate, start=[1,1,k-1,ihrs_pos], &
              count=[nx*output_resolution_factor,ny*output_resolution_factor,1,1], values=field_hr1), "wscavrate")
          end do
        endif
      end block
    endif

  !..instant part of Bq in boundary layer
    if(idebug == 1) call ftest('pbq', field_hr3, contains_undef=.true.)

  !..average part of Bq in boundary layer
    scale=100.

    do j=1,ny
      do i=1,nx
        if (avgbq1(i,j,m) + avgbq2(i,j,m) > 0.0) then
          field_hr3(i,j) = scale*avgbq1(i,j,m) / (avgbq1(i,j,m) + avgbq2(i,j,m))
        else
          field_hr3(i,j) = undef
        endif
      end do
    end do
    if(idebug == 1) call ftest('apbq', field_hr3, contains_undef=.true.)

  !..instant concentration in surface layer
    field_hr3(:,:) = concen(:,:,m)
    if(idebug == 1) call ftest('concen', field_hr3, contains_undef=.true.)
    call check(nf90_put_var(iunit, varid%comp(m)%ic, start=ipos, count=isize, &
        values=field_hr3), "set_ic(m)")

  !..accumulated/integrated concentration surface = dose
    field_hr3(:,:) = concacc(:,:,m)
    if(idebug == 1) call ftest('concac', field_hr3, contains_undef=.true.)

    call check(nf90_put_var(iunit, varid%comp(m)%ac, start=ipos, count=isize, &
        values=field_hr3), "set_ac(m)")

    write(iulog,*) '   Bq,particles in    abl  : ',bqtot1,nptot1
    write(iulog,*) '   Bq,particles above abl  : ',bqtot2,nptot2
    write(iulog,*) '   Bq,particles            : ',bqtot1+bqtot2, &
        nptot1+nptot2
    write(iulog,*) '   Bq,particles added      : ', total_activity_released(m)
    write(iulog,*) '   Bq,particles (domain)   : ', total_activity_lost_domain(m)
    write(iulog,*) '   Bq,particles lost (misc): ', total_activity_lost_other(m)
    if (output_component(m)%has_drydep) then
    write(iulog,*) '   Bq,particles dry dep    : ', sum(accdry(:,:,m))
    endif
    if (output_component(m)%has_wetdep) then
    write(iulog,*) '   Bq,particles wet dep    : ', sum(accwet(:,:,m))
    endif
    if (allocated(massbalance_file)) then
      write(massbalance_file,*) ' component: ', output_component(m)%name
      write(massbalance_file,*) '   Bq,particles in    abl  : ',bqtot1,nptot1
      write(massbalance_file,*) '   Bq,particles above abl  : ',bqtot2,nptot2
      write(massbalance_file,*) '   Bq,particles            : ',bqtot1+bqtot2, &
          nptot1+nptot2
      write(massbalance_file,*) '   Bq,particles added      : ', total_activity_released(m)
      write(massbalance_file,*) '   Bq,particles (domain)   : ', total_activity_lost_domain(m)
      write(massbalance_file,*) '   Bq,particles lost (misc): ', total_activity_lost_other(m)
      if (output_component(m)%has_drydep) then
      write(massbalance_file,*) '   Bq,particles dry dep    : ', sum(accdry(:,:,m))
      endif
      if (output_component(m)%has_wetdep) then
      write(massbalance_file,*) '   Bq,particles wet dep    : ', sum(accwet(:,:,m))
      endif
    endif

  end do all_components


!..total parameters (sum of all components).............................

  if(nocomp > 1 .AND. itotcomp == 1) then

  !..total instant Bq in and above boundary layer
    field_hr1(:,:) = 0.0
    field_hr2(:,:) = 0.0

    do n=1,npart
      i=hres_pos(pdata(n)%x)
      j=hres_pos(pdata(n)%y)
      if(pdata(n)%z >= pdata(n)%tbl) then
        field_hr1(i,j)=field_hr1(i,j)+pdata(n)%rad()
      else
        field_hr2(i,j)=field_hr2(i,j)+pdata(n)%rad()
      end if
    end do

  !..total instant part of Bq in boundary layer
    scale=100.
    where (field_hr1 + field_hr2 > 0.0)
      field_hr3 = scale*field_hr1 / (field_hr1 + field_hr2)
    elsewhere
      field_hr3 = undef
    endwhere

  !..total instant concentration in boundary layer
    field_hr2(:,:) = cscale*field_hr1/(hbl_hr*garea)
    if(idebug == 1) call ftest('tconc', field_hr2)
    call check(nf90_put_var(iunit, varid%icblt, start=ipos, count=isize, &
        values=field_hr2), "set_icblt(m)")

  !..total average concentration in boundary layer
    field_hr1(:,:) = sum(avgbq1, dim=3)
    call hres_field(real(avghbl, kind=real32), field_hr2)
    field_hr1(:,:) = cscale*field_hr1/(garea*field_hr2)
    if(idebug == 1) call ftest('tavgconc', field_hr1)
    call check(nf90_put_var(iunit, varid%acblt, start=ipos, count=isize, &
        values=field_hr1), "set_acblt")

  !..total dry deposition
    if(compute_total_dry_deposition) then
      field_hr1 = 0.0
      do m=1,nocomp
        if (output_component(m)%has_drydep) then
          field_hr1(:,:) = field_hr1 + depdry(:,:,m)
        end if
      end do
      field_hr1(:,:) = dscale*field_hr1/garea
      if(idebug == 1) call ftest('tdry', field_hr1)
      call check(nf90_put_var(iunit, varid%iddt, start=ipos, count=isize, &
          values=field_hr1), "set_iddt")
    end if

  !..total wet deposition
    if(compute_total_wet_deposition) then
      field_hr1 = 0.0
      do m=1,nocomp
        if (output_component(m)%has_wetdep) then
          field_hr1(:,:) = field_hr1 + depwet(:,:,m)
        end if
      end do
      field_hr1(:,:) = dscale*field_hr1/garea
      if(idebug == 1) call ftest('twet', field_hr1)
      call check(nf90_put_var(iunit, varid%iwdt, start=ipos, count=isize, &
          values=field_hr1), "set_iwdt")
    end if

  !..total accumulated dry deposition
    if(compute_total_dry_deposition) then
      field_hr1 = 0.0
      do m=1,nocomp
        if (output_component(m)%has_drydep) then
          field_hr1(:,:) = field_hr1 + accdry(:,:,m)
        end if
      end do
      field_hr1(:,:) = dscale*field_hr1/garea
      if(idebug == 1) call ftest('tadry', field_hr1)
      call check(nf90_put_var(iunit, varid%accddt, start=ipos, count=isize, &
          values=field_hr1), "set_accddt")
    end if

  !..total accumulated wet deposition
    if(compute_total_wet_deposition) then
      field_hr1 = 0.0
      do m=1,nocomp
        if (output_component(m)%has_wetdep) then
          field_hr1(:,:) = field_hr1 + accwet(:,:,m)
        end if
      end do
      field_hr1(:,:) = dscale*field_hr1/garea
      if(idebug == 1) call ftest('tawet', field_hr1)
      call check(nf90_put_var(iunit, varid%accwdt, start=ipos, count=isize, &
          values=field_hr1), "set_accwdt")
    end if

  !..total instant part of Bq in boundary layer
    if(idebug == 1) call ftest('tpbq', field_hr3, contains_undef=.true.)


  !..total average part of Bq in boundary layer
    scale=100.
    field_hr1(:,:) = sum(avgbq1, dim=3)
    field_hr2(:,:) = sum(avgbq2, dim=3)
    where (field_hr1 + field_hr2 > 0.0)
      field_hr3 = scale*field_hr1 / (field_hr1 + field_hr2)
    elsewhere
      field_hr3 = undef
    endwhere
    if(idebug == 1) call ftest('tapbq', field_hr3, contains_undef=.true.)

  !..total accumulated/integrated concentration
    field_hr3(:,:) = sum(concacc, dim=3)
    if(idebug == 1) call ftest('concac', field_hr3, contains_undef=.true.)

    call check(nf90_put_var(iunit, varid%act, start=ipos, count=isize, &
        values=field_hr3), "set_act")

  !.....end if(ncomp.gt.1 .and. itotcomp.eq.1) then

  end if

!..model level fields...................................................
  if (imodlevel) then
    call write_ml_fields(iunit, varid, average, [1, 1, -1, ihrs_pos], &
        [nx*output_resolution_factor, ny*output_resolution_factor, 1, 1], rt1, rt2)
  endif

  if (output_vd_debug) then
    block
      use snapfldml, only: t2m, xflux, yflux, z0, hflux, leaf_area_index, &
        roa, ustar, monin_l, raero, vs, rs, ps2
      call hres_field(ps2, field_hr1)
      call check(nf90_put_var(iunit, varid%ps_vd, start=ipos, count=isize, values=field_hr1))
      call hres_field(t2m, field_hr1)
      call check(nf90_put_var(iunit, varid%t2m, start=ipos, count=isize, values=field_hr1))
      call hres_field(xflux, field_hr1)
      call check(nf90_put_var(iunit, varid%xflux, start=ipos, count=isize, values=field_hr1))
      call hres_field(yflux, field_hr1)
      call check(nf90_put_var(iunit, varid%yflux, start=ipos, count=isize, values=field_hr1))
      call hres_field(z0, field_hr1)
      call check(nf90_put_var(iunit, varid%z0, start=ipos, count=isize, values=field_hr1))
      call hres_field(hflux, field_hr1)
      call check(nf90_put_var(iunit, varid%hflux, start=ipos, count=isize, values=field_hr1))
      call hres_field(leaf_area_index, field_hr1)
      call check(nf90_put_var(iunit, varid%lai, start=ipos, count=isize, values=field_hr1))
      call hres_field(roa, field_hr1)
      call check(nf90_put_var(iunit, varid%roa, start=ipos, count=isize, values=field_hr1))
      call hres_field(ustar, field_hr1)
      call check(nf90_put_var(iunit, varid%ustar, start=ipos, count=isize, values=field_hr1))
      call hres_field(monin_l, field_hr1)
      call check(nf90_put_var(iunit, varid%monin_l, start=ipos, count=isize, values=field_hr1))
      call hres_field(raero, field_hr1)
      call check(nf90_put_var(iunit, varid%raero, start=ipos, count=isize, values=field_hr1))
      call hres_field(vs, field_hr1)
      call check(nf90_put_var(iunit, varid%vs, start=ipos, count=isize, values=field_hr1))
      call hres_field(rs, field_hr1)
      call check(nf90_put_var(iunit, varid%rs, start=ipos, count=isize, values=field_hr1))
    end block
  endif

! reset fields
  do m=1,nocomp
    if (output_component(m)%has_drydep) then
      depdry(:,:,m) = 0.0
    end if
    if (output_component(m)%has_wetdep) then
      depwet(:,:,m) = 0.0
    end if
  end do

  call check(nf90_sync(iunit))
  call check(nf90_close(iunit))
end subroutine fldout_nc


subroutine write_ml_fields(iunit, varid, average, ipos_in, isize, rt1, rt2)
  USE releaseML, only: nplume, iplume
  USE particleML, only: pdata, Particle
  USE snapparML, only: def_comp, nocomp
  USE snapfldML, only: field_hr1, field_hr2, &
      hlayer1, hlayer2, garea, ml_bq
  USE ftestML, only: ftest
  USE snapdebug, only: idebug
  USE snapgrdML, only: itotcomp, modlevel_is_average, modleveldump, ivlayer
  USE snapdimML, only: nx,ny,nk,output_resolution_factor, hres_pos

  integer, intent(in) :: iunit
  type(common_var), intent(in) :: varid
  real, intent(in) :: average
  integer, intent(in) :: ipos_in(4)
  integer, intent(in) :: isize(4)
  real, intent(in) :: rt1, rt2

  type(Particle) :: part
  real :: avg, total, dh
  integer :: ivlvl
  integer :: i, j, k, loop, m, maxage, n, npl
  integer :: ipos(4)
  logical :: inactivated_ ! dummy param

!..concentration in each layer
!..(height only computed at time of output)

  ipos(:) = ipos_in

!..loop for 1=average and 2=instant concentration
!..(now computing average first, then using the same arrays for instant)
  total=0.
  maxage=0

  if (modlevel_is_average) then
    avg = average
  else
    ml_bq = 0.0
    avg = 1.0
    if (modleveldump > 0) then
      do npl = 1, nplume
        do n = iplume(npl)%start, iplume(npl)%end
          part = pdata(n)
          i = hres_pos(part%x)
          j = hres_pos(part%y)
          ivlvl = part%z*10000.
          k = ivlayer(ivlvl)
          m = def_comp(part%icomp)%to_output
          !..in each sigma/eta (input model) layer
          !.. dump and remove old particles, don't touch  new ones
          if (iplume(npl)%ageInSteps >= nint(modleveldump)) then
            maxage = max(maxage, int(iplume(npl)%ageInSteps, kind(maxage)))
            ml_bq(i,j,k,m) = ml_bq(i,j,k,m) + part%rad()
            total = total + part%rad()

            inactivated_ =  part%inactivate()
            pdata(n) = part
          end if
        end do
      end do
      write (error_unit,*) "dumped; maxage, total", maxage, total
    else
      ! instant, accumulate once
      call accumulate_ml_field()
    end if

  endif

  do k=1,nk-1
    do j=1,ny*output_resolution_factor
      do i=1,nx*output_resolution_factor
        dh = rt1*hlayer1(i,j,k) + rt2*hlayer2(i,j,k)
        field_hr2(i,j) = dh*garea(i,j)
      end do
    end do
    do m=1,nocomp
      ml_bq(:,:,k,m) = ml_bq(:,:,k,m)/(field_hr2*avg)
    end do
  end do

!..concentration in each layer for each type
  do m=1,nocomp
    do k=1,nk-1
      field_hr1(:,:) = cscale*ml_bq(:,:,k,m)
      if (modlevel_is_average) then
        if(idebug == 1) call ftest('tinstconcml', field_hr1)
      else
        if(idebug == 1) call ftest('tavgconcml', field_hr1)
      end if

      ipos(3) = k
      call check(nf90_put_var(iunit, varid%comp(m)%icml, start=ipos, &
          count=isize, values=field_hr1), "icml(m)")
    end do
  end do

!..total concentration in each layer
  if(nocomp > 1 .AND. itotcomp == 1) then
    do m=2,nocomp
      do k=1,nk-1
        ml_bq(:,:,k,1) = ml_bq(:,:,k,1) + ml_bq(:,:,k,m)
      end do
    end do
    do k=1,nk-1
      field_hr1(:,:) = cscale*ml_bq(:,:,k,1)
      if (modlevel_is_average) then
        if(idebug == 1) call ftest('tinstconcml', field_hr1)
      else
        if(idebug == 1) call ftest('tavgconcml', field_hr1)
      end if
    end do
  end if
end subroutine


subroutine nc_declare(iunit, dimids, varid, varname, units, stdname, chunksize, datatype)
  USE snapdebug, only: iulog
  integer, intent(in) :: iunit
  integer, intent(out)   :: varid
  integer, intent(in)    :: dimids(:)

  character(len=*), intent(in) :: units
  character(len=*), intent(in) :: varname
  character(len=*), intent(in), optional :: stdname
  integer, intent(in), optional :: chunksize(:)
  integer, intent(in), optional :: datatype

  integer :: datatype_internal

  write(iulog,"('declaring ' (a) ' ' (a) )",advance="NO") varname, units
  if (present(stdname)) write(iulog, "(' ' (a))",advance="NO") trim(stdname)
  write(iulog,'()',advance="YES")

  if (present(datatype)) then
    datatype_internal = datatype
  else
    datatype_internal = NF90_FLOAT
  endif
  call check(nf90_def_var(iunit, TRIM(varname), &
      datatype_internal, dimids, varid), "def_"//varname)
  if (present(chunksize)) then
    call check(nf90_def_var_chunking(iunit, varid, NF90_CHUNKED, chunksize))
    call check(nf90_def_var_deflate(iunit, varid, &
      shuffle=1, deflate=1, deflate_level=1))
  endif
  call check(nf90_put_att(iunit,varid, "units", TRIM(units)))
  if (present(stdname)) then
    call check(nf90_put_att(iunit,varid,"standard_name", TRIM(stdname)))
  endif

  call check(nf90_put_att(iunit,varid,"coordinates", "longitude latitude"))
  call check(nf90_put_att(iunit,varid,"grid_mapping", "projection"))
end subroutine

subroutine nc_set_vtrans(iunit, kdimid,k_varid,ap_varid,b_varid)
  use snapgrdML, only: vlevel, alevel, blevel
  INTEGER, INTENT(IN) :: iunit, kdimid
  INTEGER, INTENT(OUT) :: k_varid, ap_varid, b_varid

  call check(nf90_def_var(iunit, "k", &
      NF90_FLOAT, kdimid, k_varid), "def_k")
  call check(nf90_put_att(iunit,k_varid, "standard_name", &
      TRIM("atmosphere_hybrid_sigma_pressure_coordinate")))
  call check(nf90_put_att(iunit,k_varid, "formula", &
      TRIM("p(n,k,j,i) = ap(k) + b(k)*ps(n,j,i)")))
  call check(nf90_put_att(iunit,k_varid, "formula_terms", &
      TRIM("ap: ap b: b ps: surface_air_pressure")))
  call check(nf90_put_att(iunit,k_varid, "positive", TRIM("down")))
  call check(nf90_put_var(iunit, k_varid, vlevel(2:)))

  call check(nf90_def_var(iunit, "ap", &
      NF90_FLOAT, kdimid, ap_varid), "def_ap")
  call check(nf90_put_att(iunit,ap_varid, "units", TRIM("hPa")))
  call check(nf90_put_var(iunit, ap_varid, alevel(2:)))

  call check(nf90_def_var(iunit, "b", &
      NF90_FLOAT, kdimid, b_varid), "def_b")
  call check(nf90_put_var(iunit, b_varid, blevel(2:)))

end subroutine nc_set_vtrans

subroutine nc_set_projection(iunit, xdimid, ydimid, &
    igtype, gparam, garea, xm, ym, &
    simulation_start)
  USE snapdimML, only : nx, ny, output_resolution_factor, hres_field
  USE snapfldML, only : field_hr1, field_hr2
  INTEGER, INTENT(IN) :: iunit, xdimid, ydimid, igtype
  REAL(real32), INTENT(IN):: gparam(8)
  REAL(real32), INTENT(IN), DIMENSION(nx*output_resolution_factor,ny*output_resolution_factor) :: garea
  REAL(real32), INTENT(IN), DIMENSION(nx,ny) :: xm
  REAL(real32), INTENT(IN), DIMENSION(nx,ny) :: ym
  CHARACTER(LEN=19), INTENT(IN)  :: simulation_start

  INTEGER :: i, j, ierror, x_varid, y_varid, proj_varid, &
  lon_varid, lat_varid, carea_varid, mapx_varid, mapy_varid, &
  dimids(2)
  REAL(KIND=real32) :: xvals(nx*output_resolution_factor), &
  yvals(ny*output_resolution_factor), &
  lon(nx,ny), &
  lat(nx,ny), &
  val, gparam2(6), gparam_hres(8)
  real(kind=real32) :: llparam(6) = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0]

  call check(nf90_def_var(iunit, "x", &
      NF90_FLOAT, xdimid, x_varid))
  call check(nf90_def_var(iunit, "y", &
      NF90_FLOAT, ydimid, y_varid))
  dimids = [xdimid, ydimid]

  call check(nf90_def_var(iunit, "projection", &
      NF90_SHORT, varid=proj_varid))

  call check(nf90_put_att(iunit, NF90_GLOBAL, "Conventions", &
      "CF-1.0"))

! a reference-time, same as in WRF
  call check(nf90_put_att(iunit, NF90_GLOBAL, &
      "SIMULATION_START_DATE", trim(simulation_start)))

  gparam_hres(:) = gparam(:)
  select case(igtype)
  case(2) !..geographic
    call check(nf90_put_att(iunit,x_varid, "units", &
        TRIM("degrees_east")))
    call check(nf90_put_att(iunit,y_varid, "units", &
        TRIM("degrees_north")))
    call check(nf90_put_att(iunit,proj_varid, &
        "grid_mapping_name", TRIM("latitude_longitude")))
    gparam_hres(3) = gparam(3)/output_resolution_factor
    gparam_hres(4) = gparam(4)/output_resolution_factor
    ! first cell center, not left edge. must be moved
    gparam_hres(1) = gparam(1) - .5 * (output_resolution_factor-1) * gparam_hres(3)
    gparam_hres(2) = gparam(2) - .5 * (output_resolution_factor-1) * gparam_hres(4)

    do i=1,nx*output_resolution_factor
      xvals(i) = gparam_hres(1) + (i-1)*gparam_hres(3)
    end do
    do i=1,ny*output_resolution_factor
      yvals(i) = gparam_hres(2) + (i-1)*gparam_hres(4)
    end do
  case(3) !..rot_geographic
    call check(nf90_put_att(iunit,x_varid, "units", &
        TRIM("degrees")))
    call check(nf90_put_att(iunit,y_varid, "units", &
        TRIM("degrees")))
    call check(nf90_put_att(iunit,x_varid, "standard_name", &
        TRIM("grid_longitude")))
    call check(nf90_put_att(iunit,y_varid, "standard_name", &
        TRIM("grid_latitude")))
    call check(nf90_put_att(iunit,proj_varid, &
        "grid_mapping_name", TRIM("rotated_latitude_longitude")))
    val = 180+gparam(5)
    if (val > 360) val = val - 360
    call check(nf90_put_att(iunit,proj_varid, &
        "grid_north_pole_longitude", val))
    call check(nf90_put_att(iunit,proj_varid, &
        "grid_north_pole_latitude", 90.0-gparam(6)))
    call check(nf90_sync(iunit))

    do i=1,nx*output_resolution_factor
      xvals(i) = gparam_hres(1) + (i-1)*gparam_hres(3)
    end do
    do i=1,ny*output_resolution_factor
      yvals(i) = gparam_hres(2) + (i-1)*gparam_hres(4)
    end do
  case(1,4) !..polar_stereographic
    call check(nf90_put_att(iunit,x_varid, "units", TRIM("m")))
    call check(nf90_put_att(iunit,y_varid, "units", TRIM("m")))
    call check(nf90_put_att(iunit,x_varid, "standard_name", &
        TRIM("projection_x_coordinate")))
    call check(nf90_put_att(iunit,y_varid, "standard_name", &
        TRIM("projection_y_coordinate")))
    call check(nf90_put_att(iunit,proj_varid, &
        "grid_mapping_name", TRIM("polar_stereographic")))
    val = 180+gparam(5)
    if (val > 360) val = val - 360
    call check(nf90_put_att(iunit,proj_varid, &
        "straight_vertical_longitude_from_pole", gparam(4)))
    call check(nf90_put_att(iunit,proj_varid, &
        "standard_parallel", gparam(5)))
    call check(nf90_put_att(iunit,proj_varid, &
        "latitude_of_projection_origin", 90))
  !..increment
    gparam_hres(7) = gparam(7)/output_resolution_factor
    gparam_hres(8) = gparam(8)/output_resolution_factor
    ! first cell center, not left edge. must be moved
    gparam_hres(1) = gparam(1) + .5 * (output_resolution_factor-1)
    gparam_hres(2) = gparam(2) + .5 * (output_resolution_factor-1)
    do i=1,nx*output_resolution_factor
      xvals(i) = (i-gparam(1))*gparam_hres(7)
    end do
    do i=1,ny*output_resolution_factor
      yvals(i) = (i-gparam(2))*gparam(8)
    end do
  case(6) !..lcc
    call check(nf90_put_att(iunit,x_varid, "units", TRIM("m")))
    call check(nf90_put_att(iunit,y_varid, "units", TRIM("m")))
    call check(nf90_put_att(iunit,x_varid, "standard_name", &
        TRIM("projection_x_coordinate")))
    call check(nf90_put_att(iunit,y_varid, "standard_name", &
        TRIM("projection_y_coordinate")))
    call check(nf90_put_att(iunit,proj_varid, "grid_mapping_name", &
        TRIM("lambert_conformal_conic")))
    call check(nf90_put_att(iunit,proj_varid, &
        "longitude_of_central_meridian", gparam(5)))
    call check(nf90_put_att(iunit,proj_varid, &
        "standard_parallel", gparam(6)))
    call check(nf90_put_att(iunit,proj_varid, &
        "latitude_of_projection_origin", gparam(6)))

    xvals(1) = gparam(1)
    yvals(1) = gparam(2)
  ! gparam(5) and gparam(6) are the reference-point of the projection
    gparam2(1) = gparam(5)
    gparam2(2) = gparam(6)
    gparam2(3:6) = gparam(3:6)

    ierror = 0
    call xyconvert(1, xvals(1), yvals(1), 2, llparam, &
    &                                          6, gparam2, ierror)
    if (ierror /= 0) then
      write (error_unit,*) "error converting lcc to ll"
      error stop 1
    end if
    gparam_hres(2) = gparam(2) / output_resolution_factor
    gparam_hres(3) = gparam(3) / output_resolution_factor
    gparam_hres(7) = gparam(7) / output_resolution_factor
    gparam_hres(8) = gparam(8) / output_resolution_factor

    xvals(1) = (xvals(1)-1)*gparam(7)
    yvals(1) = (yvals(1)-1)*gparam(8)
    ! xvals is currently the lowerd left corner in plane-coordinates
    ! but must be in m from center
    ! first cell center, not left edge. must be moved
    xvals(1) = xvals(1) - .5 * (output_resolution_factor-1) * gparam_hres(7)
    yvals(2) = yvals(2) - .5 * (output_resolution_factor-1) * gparam_hres(8)

    do i=2,nx*output_resolution_factor
      xvals(i) = xvals(1) + (i-1)*gparam_hres(7)
    end do
    do i=2,ny*output_resolution_factor
      yvals(i) = yvals(1) + (i-1)*gparam_hres(8)
    end do
  case default
    write (error_unit,*) "unkown grid-type:", igtype
    error stop 1
  end select

  call check(nf90_put_var(iunit, x_varid, xvals))
  call check(nf90_put_var(iunit, y_varid, yvals))
  call check(nf90_sync(iunit))

  call check(nf90_def_var(iunit, "longitude", &
      NF90_FLOAT, dimids, lon_varid))
  call check(NF90_DEF_VAR_DEFLATE(iunit, lon_varid, 1,1,1))
  call check(nf90_sync(iunit))
  call check(nf90_def_var(iunit, "latitude", &
      NF90_FLOAT, dimids, lat_varid))
  call check(nf90_sync(iunit))
  call check(nf90_put_att(iunit,lon_varid, "units", &
      TRIM("degrees_east")))
  call check(nf90_put_att(iunit,lat_varid, "units", &
      TRIM("degrees_north")))

  call check(nf90_sync(iunit))

!.... create latitude/longitude variable-values
  if (igtype .eq. 2) then
    ! exact
    do j=1,ny*output_resolution_factor
      do i=1,nx*output_resolution_factor
        ! longitude field
        field_hr1(i,j) = xvals(i)
        ! latitude field
        field_hr2(i,j) = yvals(j)
      enddo
    enddo
  else
    ! interpolated
    do j=1,ny
      do i=1,nx
        lon(i,j) = i
        lat(i,j) = j
      end do
    end do
    call xyconvert(nx*ny, lon, lat,igtype, gparam, &
        2, llparam, ierror)
    if (ierror /= 0) then
      write (error_unit,*) "error converting pos to latlon-projection"
      error stop 1
    end if
    call hres_field(lon, field_hr1, .true.)
    call hres_field(lat, field_hr2, .true.)
  endif
  call check(nf90_put_var(iunit, lon_varid, field_hr1))
  call check(nf90_put_var(iunit, lat_varid, field_hr2))

!.... create cell_area
  call check(nf90_def_var(iunit, "cell_area", &
      NF90_FLOAT, dimids, carea_varid))
  call check(nf90_put_att(iunit,carea_varid, "units", &
      TRIM("m2")))
  call check(nf90_put_att(iunit,carea_varid, "grid_mapping", &
      TRIM("projection")))
  call check(nf90_put_att(iunit,carea_varid, "coordinates", &
      TRIM("longitude latitude")))

  call check(nf90_put_var(iunit, carea_varid, garea))

!.... add map_factor_x and map_factor_y
  call check(nf90_def_var(iunit, "map_factor_x", &
      NF90_FLOAT, dimids, mapx_varid))
  call check(nf90_put_att(iunit,mapx_varid, "units", "1"))
  call check(nf90_put_att(iunit,mapx_varid, "grid_mapping", &
      TRIM("projection")))
  call check(nf90_put_att(iunit,mapx_varid, "coordinates", &
      TRIM("longitude latitude")))

  call hres_field(xm, field_hr1, .true.)
  call check(nf90_put_var(iunit, mapx_varid, field_hr1))

  call check(nf90_def_var(iunit, "map_factor_y", &
      NF90_FLOAT, dimids, mapy_varid))
  call check(nf90_put_att(iunit,mapy_varid, "units", "1"))
  call check(nf90_put_att(iunit,mapy_varid, "grid_mapping", &
      TRIM("projection")))
  call check(nf90_put_att(iunit,mapy_varid, "coordinates", &
      TRIM("longitude latitude")))

  call hres_field(ym, field_hr1, .true.)
  call check(nf90_put_var(iunit, mapy_varid, field_hr1))

  call check(nf90_sync(iunit))
end subroutine nc_set_projection

subroutine initialize_output(filename, itime, ierror)
  USE snapfilML, only: ncsummary, nctitle, simulation_start
  USE snapgrdML, only: gparam, igtype, imodlevel, modlevel_is_average, imslp, precipitation_in_output, &
      itotcomp, modleveldump, compute_column_max_conc, compute_aircraft_doserate, &
      aircraft_doserate_threshold, &
      output_vd, output_column, output_vd_debug
  USE snapfldML, only:  &
      garea, &
      xm, ym, &
      nhfout
  USE snapparML, only: nocomp, output_component, def_comp
  USE ftestML, only: ftest
  USE snapdimML, only: nx, ny, nk, output_resolution_factor, hres_field
  USE particleML, only: Particle
  use snapmetML, only: met_params

  character(len=*), intent(in) :: filename
  type(datetime_t), intent(in) :: itime
  integer, intent(out) :: ierror

  integer :: iunit
  integer :: m, mm
  character(len=256) :: string
  integer :: dimids2d(2), dimids3d(3), dimids4d(4)
  integer :: chksz3d(3), chksz4d(4)
  logical :: compute_total_dry_deposition
  logical :: compute_total_wet_deposition
  type(common_dim) :: dimid
  type(common_var) :: varid

  ierror = 0

  compute_total_dry_deposition = any(def_comp%kdrydep == 1) .and. nocomp > 1
  compute_total_wet_deposition = any(def_comp%kwetdep == 1) .and. nocomp > 1


    call check(nf90_create(filename, ior(NF90_NETCDF4, NF90_CLOBBER), iunit), filename)
    call check(nf90_def_dim(iunit, "time", NF90_UNLIMITED, dimid%t), &
        "t-dim")
    call check(nf90_def_dim(iunit, "x", nx*output_resolution_factor, dimid%x), "x-dim")
    call check(nf90_def_dim(iunit, "y", ny*output_resolution_factor, dimid%y), "y-dim")
    call check(nf90_def_dim(iunit, "k", nk-1, dimid%k), "k-dim")
    call check(nf90_def_dim(iunit, "nocomp", nocomp, dimid%nocomp), "nocomp-dim")
    call check(nf90_def_dim(iunit, "compnamelenmax", len(def_comp(1)%compname), dimid%maxcompname), "maxcompname-dim")

    if (allocated(nctitle)) then
      call check(nf90_put_att(iunit, NF90_GLOBAL, &
          "title", trim(nctitle)))
    endif
    call check(nf90_put_att(iunit, NF90_GLOBAL, &
        "summary", trim(ncsummary)))

    call nc_set_projection(iunit, dimid%x, dimid%y, &
        igtype, gparam, garea, xm, ym, simulation_start)
    if (imodlevel .or. met_params%use_3d_precip) then
      call nc_set_vtrans(iunit, dimid%k, varid%k, varid%ap, varid%b)
    endif

    call check(nf90_def_var(iunit, "time", NF90_FLOAT, dimid%t, varid%t))
    write(string,'(A12,I4,A1,I0.2,A1,I0.2,A1,I0.2,A12)') &
        "hours since ",itime%year,"-",itime%month,"-",itime%day," ", &
        itime%hour,":00:00 +0000"
    call check(nf90_put_att(iunit, varid%t, "units", &
        trim(string)))

  !..store the files base-time
    iftime = itime

    dimids2d = [dimid%x, dimid%y]
    dimids3d = [dimid%x, dimid%y, dimid%t]
    dimids4d = [dimid%x, dimid%y, dimid%k, dimid%t]

    chksz3d = [nx, ny, 1]
    chksz4d = [nx, ny, 1, 1]

    if (imodlevel) then
      call nc_declare(iunit, dimids3d, varid%ps, &
          "surface_air_pressure", units="hPa", &
          stdname="surface_air_pressure", chunksize=chksz3d)
    endif
    if (imslp == 1) then
      call nc_declare(iunit, dimids3d, varid%mslp, &
        "air_pressure_at_sea_level", units="hPa", &
        stdname="air_pressure_at_sea_level", chunksize=chksz3d)
    endif
    if (precipitation_in_output) then
      call nc_declare(iunit, dimids3d, varid%accum_prc, &
        "precipitation_amount_acc", units="kg/m2", &
        stdname="precipitation_amount", chunksize=chksz3d)

      call nc_declare(iunit, dimids3d, varid%prc, &
        "lwe_precipitation_rate", units="mm/("//itoa(nhfout)//"hr)", &
        stdname="lwe_precipitation_rate", chunksize=chksz3d)

      if (met_params%use_3d_precip) then
        call nc_declare(iunit, dimids3d, varid%instant_prc, &
          "precipitation_amount_instant", units="mm/h", &
          stdname="precipitation", chunksize=chksz3d)
      endif
    endif

    call nc_declare(iunit, dimids3d, varid%ihbl, &
        "instant_height_boundary_layer", units="m", &
        stdname="height", chunksize=chksz3d)

    call nc_declare(iunit, dimids3d, varid%ahbl, &
      "average_height_boundary_layer", units="m", &
      stdname="height", chunksize=chksz3d)

    if (compute_column_max_conc) then
      call nc_declare(iunit, dimids3d, varid%column_max_conc, &
       "max_column_concentration", units="Bq/m3", &
       chunksize=chksz3d)
    endif
    if (compute_aircraft_doserate) then
      call nc_declare(iunit, dimids3d, varid%aircraft_doserate, &
        "aircraft_doserate", units="Sv/h", &
        chunksize=chksz3d)
      if (aircraft_doserate_threshold > 0.0) then
        call nc_declare(iunit, dimids3d, varid%aircraft_doserate_threshold_height, &
          "aircraft_doserate_threshold_height", units="m", &
          chunksize=chksz3d)
      endif
    endif

    if (output_wetdeprate) then
      call nc_declare(iunit, dimids3d, varid%wetdeprate, &
       "wetdeprate", units="m/s", chunksize=chksz3d)
    endif

    call check(nf90_def_var(iunit, "components", NF90_CHAR, [dimid%maxcompname, dimid%nocomp], varid%components))

    do m=1,nocomp
      call check(nf90_put_var(iunit, varid%components, &
        start=[1, m], count=[len_trim(output_component(m)%name), 1], &
        values=trim(output_component(m)%name)))
      call nc_declare(iunit, dimids3d, varid%comp(m)%ic, &
        trim(output_component(m)%name)//"_concentration", &
        units="Bq/m3", chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%comp(m)%icbl, &
        trim(output_component(m)%name)//"_concentration_bl", &
        units="Bq/m3", chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%comp(m)%ac, &
        trim(output_component(m)%name)//"_acc_concentration", &
        units="Bq*hr/m3", chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%comp(m)%acbl, &
        trim(output_component(m)%name)//"_avg_concentration_bl", &
        "Bq/m3", chunksize=chksz3d)
      if (output_component(m)%has_drydep) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%idd, &
          trim(output_component(m)%name)//"_dry_deposition", &
          units="Bq/m2", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%comp(m)%accdd, &
          trim(output_component(m)%name)//"_acc_dry_deposition", &
          units="Bq/m2", chunksize=chksz3d)
      end if

      if (output_component(m)%has_wetdep) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%iwd, &
          trim(output_component(m)%name)//"_wet_deposition", &
            units="Bq/m2", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%comp(m)%accwd, &
          trim(output_component(m)%name)//"_acc_wet_deposition", &
          units="Bq/m2", chunksize=chksz3d)
        if (output_wetdeprate) then
          call nc_declare(iunit, dimids4d, varid%comp(m)%wetscavrate, &
             trim(output_component(m)%name)//"_wetdeprate", units="1/s", chunksize=chksz4d)
        endif
      end if

      if (imodlevel) then
        if (modleveldump > 0.) then
          string = trim(output_component(m)%name)//"_concentration_dump_ml"
        else
          string = trim(output_component(m)%name)//"_concentration_ml"
        endif
        call nc_declare(iunit, dimids4d, varid%comp(m)%icml, &
          string, units="Bq/m3", chunksize=chksz4d)
        if (modlevel_is_average) then
          call check(nf90_put_att(iunit, varid%comp(m)%icml, "cell_methods", "time: mean"))
        else
          call check(nf90_put_att(iunit, varid%comp(m)%icml, "cell_methods", "time: point"))
        end if
      end if
      if (output_column) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%conc_column, &
          trim(output_component(m)%name)//"_column_concentration", &
          units="Bq/m2", chunksize=chksz3d)
      endif

      if (def_comp(mm)%kdrydep > 0 .and. output_vd) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%vd, &
          trim(def_comp(mm)%output_name)//"_dry_deposition_velocity", &
          units="m/s", chunksize=chksz3d)
      endif

      if (output_vd_debug) then
        block
          use snapmetml, only: downward_momentum_flux_units, surface_heat_flux_units, &
            leaf_area_index_units, surface_roughness_length_units, temp_units
        call nc_declare(iunit, dimids3d, varid%xflux, &
          "xflux", units=downward_momentum_flux_units, &
          chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%yflux, &
          "yflux", units=downward_momentum_flux_units, &
          chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%hflux, &
          "hflux", units=surface_heat_flux_units, &
          chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%z0, &
          "z0", units=surface_roughness_length_units, &
          chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%lai, &
          "lai", units=leaf_area_index_units, &
          chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%t2m, &
          "t2m", units=temp_units, &
          chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%roa, &
          "roa", units="??", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%ustar, &
          "ustar", units="??", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%monin_l, &
          "monin_l", units="??", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%raero, &
          "raero", units="??", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%vs, &
          "vs", units="??", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%rs, &
          "rs", units="??", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%ps_vd, &
          "ps_vd", units="hPa", chunksize=chksz3d)
        end block
      endif

      block
        use iso_fortran_env, only: int8
        use drydep, only: largest_landfraction_file, classnr
        integer(kind=int8), allocatable :: classnr_hr(:,:)
        if (largest_landfraction_file /= "not set") then
            call nc_declare(iunit, dimids2d, varid%landfraction, &
              "land_fraction", units="1", datatype=NF90_BYTE)
            call hres_field(classnr, classnr_hr)
            call check(nf90_put_var(iunit, varid%landfraction, start=[1, 1], count=shape(classnr_hr), &
              values=classnr_hr), "Put landfraction")
            call check(nf90_put_att(iunit, varid%landfraction, "11", "Sea"))
            call check(nf90_put_att(iunit, varid%landfraction, "12", "Inland water"))
            call check(nf90_put_att(iunit, varid%landfraction, "13", "Tundra/desert"))
            call check(nf90_put_att(iunit, varid%landfraction, "14", "Ice and ice sheets"))
            call check(nf90_put_att(iunit, varid%landfraction, "15", "Urban"))
            call check(nf90_put_att(iunit, varid%landfraction, "16", "Crops"))
            call check(nf90_put_att(iunit, varid%landfraction, "17", "Grass"))
            call check(nf90_put_att(iunit, varid%landfraction, "18", "Wetlands"))
            call check(nf90_put_att(iunit, varid%landfraction, "19", "Evergreen needleleaf"))
            call check(nf90_put_att(iunit, varid%landfraction, "20", "Deciduous broadleaf"))
            call check(nf90_put_att(iunit, varid%landfraction, "21", "Mixed forest"))
            call check(nf90_put_att(iunit, varid%landfraction, "22", "Shrubs and interrupted woodlands"))
        endif
    end block

    end do
    if (itotcomp == 1) then
      call nc_declare(iunit, dimids3d, varid%icblt, &
        "total_concentration_bl", units="Bq/m3", &
          chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%acblt, &
        "total_avg_concentration_bl", units="Bq/m3", &
        chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%act, &
        "total_acc_concentration", units="Bq/m3", &
          chunksize=chksz3d)
      if (compute_total_dry_deposition) then
        call nc_declare(iunit, dimids3d, varid%iddt, &
          "total_dry_deposition", units="Bq/m2", &
            chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%accddt, &
          "total_acc_dry_deposition", units="Bq/m2", &
          chunksize=chksz3d)
      end if
      if (compute_total_wet_deposition) then
        call nc_declare(iunit, dimids3d, varid%iwdt, &
          "total_wet_deposition", units="Bq/m2", &
            chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%accwdt, &
          "total_acc_wet_deposition", units="Bq/m2", &
          chunksize=chksz3d)
      end if
    end if
    call check(nf90_enddef(iunit))
    call check(nf90_close(iunit))

    if (massbalance_filename /= "") then
      call open_massbalance_file()
    endif
end subroutine

subroutine open_massbalance_file()
   allocate(massbalance_file)
   open(NEWUNIT=massbalance_file, FILE=massbalance_filename, ACTION="WRITE", ENCODING="UTF-8")
end subroutine

subroutine close_massbalance_file()
  if (.not.allocated(massbalance_file)) return
  close(massbalance_file)
  deallocate(massbalance_file)
end subroutine

subroutine unload()
  call close_massbalance_file()
end subroutine

subroutine get_varids(iunit, varid, ierror)
  USE snapparML, only: nocomp, output_component, def_comp, nocomp
  USE snapgrdML, only: imodlevel, modleveldump
  integer, intent(in) :: iunit
  type(common_var), intent(out) :: varid
  integer, intent(out) :: ierror

  integer :: m
  character(len=64) :: varname

  ierror = 0

  ! Required
  ierror = nf90_inq_varid(iunit, "instant_height_boundary_layer", varid%ihbl)
  if (ierror /= NF90_NOERR) return
  ierror = nf90_inq_varid(iunit, "average_height_boundary_layer", varid%ahbl)
  if (ierror /= NF90_NOERR) return
  ierror = nf90_inq_varid(iunit, "time", varid%t)
  if (ierror /= NF90_NOERR) return

  ! Optional
  ierror = nf90_inq_varid(iunit, "precipitation_amount_acc", varid%accum_prc)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "precipitation_amount_instant", varid%instant_prc)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "lwe_precipitation_rate", varid%prc)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "air_pressure_at_sea_level", varid%mslp)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_concentration_bl", varid%icblt)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_avg_concentration_bl", varid%acblt)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_acc_concentration", varid%act)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_dry_deposition", varid%iddt)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_acc_dry_deposition", varid%accddt)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_wet_deposition", varid%iwdt)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "total_acc_wet_deposition", varid%accwdt)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "surface_air_pressure", varid%ps)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "k", varid%k)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "ap", varid%ap)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "b", varid%b)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "max_column_concentration", varid%column_max_conc)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "aircraft_doserate", varid%aircraft_doserate)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "aircraft_doserate_threshold_height", varid%aircraft_doserate_threshold_height)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

  ierror = nf90_inq_varid(iunit, "xflux", varid%xflux)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "yflux", varid%yflux)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "hflux", varid%hflux)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "lai", varid%lai)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "z0", varid%z0)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "t2m", varid%t2m)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "roa", varid%roa)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "ustar", varid%ustar)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "monin_l", varid%monin_l)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "raero", varid%raero)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "vs", varid%vs)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "rs", varid%rs)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  ierror = nf90_inq_varid(iunit, "ps_vd", varid%ps_vd)
  if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

  do m=1,nocomp
    varname = trim(output_component(m)%name) // "_concentration"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%ic)
    if (ierror /= NF90_NOERR) return

    varname = trim(output_component(m)%name) // "_concentration_bl"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%icbl)
    if (ierror /= NF90_NOERR) return

    varname = trim(output_component(m)%name) // "_acc_concentration"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%ac)
    if (ierror /= NF90_NOERR) return

    varname = trim(output_component(m)%name) // "_avg_concentration_bl"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%acbl)
    if (ierror /= NF90_NOERR) return

    varname = trim(output_component(m)%name) // "_dry_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%idd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_acc_dry_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%accdd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_wet_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%iwd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_acc_wet_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%accwd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_dry_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%idd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_acc_dry_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%accdd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_dry_deposition_velocity"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%vd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_wet_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%iwd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    varname = trim(output_component(m)%name) // "_acc_wet_deposition"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%accwd)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    ierror = nf90_inq_varid(iunit, trim(output_component(m)%name)//"_wetdeprate", varid%comp(m)%wetscavrate)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

    if (imodlevel) then
      if (modleveldump > 0.) then
        varname = trim(output_component(m)%name) // "_concentration_dump_ml"
        ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%icml)
        if (ierror /= NF90_NOERR) return
      else
        varname = trim(output_component(m)%name) // "_concentration_ml"
        ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%icml)
        if (ierror /= NF90_NOERR) return
      endif
    endif

    varname = trim(output_component(m)%name) // "_column_concentration"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%conc_column)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  enddo

  ierror = NF90_NOERR
end subroutine

!> accumulate model level fields only
subroutine accumulate_ml_field()
  USE snapgrdML, only: imodlevel, modlevel_is_average, &
      ivlayer
  USE snapfldML, only: ml_bq
  USE snapparML, only: def_comp
  USE snapdimml, only: hres_pos
  USE releaseML, only: npart
  USE particleML, only: pdata
  integer :: i, j, m, n, k
  integer :: ivlvl

  if(imodlevel) then
    do n=1,npart
      i = hres_pos(pdata(n)%x)
      j = hres_pos(pdata(n)%y)
      ivlvl = pdata(n)%z*10000.
      k = ivlayer(ivlvl)
      m = def_comp(pdata(n)%icomp)%to_output
    !..in each sigma/eta (input model) layer
      ml_bq(i,j,k,m) = ml_bq(i,j,k,m) + pdata(n)%rad()
    end do
  end if
end subroutine accumulate_ml_field

!> accumulation for average fields
subroutine accumulate_fields(tf1, tf2, tnow, tstep, nsteph)
  USE snapgrdML, only: imodlevel, modlevel_is_average, &
      ivlayer, compute_column_max_conc, compute_aircraft_doserate, &
      alevel, blevel, aircraft_doserate_threshold
  USE snapfldML, only:  &
      avgbq1, avgbq2, hlayer1, hlayer2, hbl1, hbl2, &
      avgprec, concen, avghbl, &
      concacc, precip, ml_bq, &
      max_column_scratch, max_column_concentration, garea, &
      ps1, ps2, t1_abs, t2_abs, aircraft_doserate_scratch, aircraft_doserate, &
      aircraft_doserate_threshold_height
  USE snapdimml, only: nx, ny, nk, output_resolution_factor, hres_pos, lres_pos
  USE snapparML, only: nocomp, def_comp, output_component
  USE ftestML, only: ftest
  USE releaseML, only: npart
  USE particleML, only: pdata, Particle

  real, intent(in) :: tf1, tf2, tnow, tstep
  integer, intent(in) :: nsteph

  real :: rt1, rt2, dh
  real :: outside_pressure, outside_temperature, inside_pressure
  real :: pressure, pressure_altitude, doserate
  real :: scale
  integer :: i, j, m, n, k, ii, ji
  integer :: ivlvl
  type(Particle) :: part
  real :: hrstep

  if(naverage == 0) then
    avghbl = 0.0
    avgprec = 0.0

    avgbq1 = 0.0
    avgbq2 = 0.0

    if (imodlevel .and. modlevel_is_average) then
      ml_bq = 0.0
    end if

    if (compute_column_max_conc) then
      max_column_concentration = 0.0
    endif
    if (compute_aircraft_doserate) then
      aircraft_doserate = 0.0
      if (aircraft_doserate_threshold > 0.0) then
        aircraft_doserate_threshold_height = 0
      endif
    endif
  end if

  naverage=naverage+1

!..for time interpolation
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)
  hrstep=1./float(nsteph)

!..height of boundary layer
  avghbl(:,:) = avghbl + (rt1*hbl1 + rt2*hbl2)

!..precipitation (no time interpolation, but hourly time intervals)
  scale=tstep/3600.
  avgprec(:,:) = avgprec + scale*precip(:,:)

  do n=1,npart
    part = pdata(n)
    i = hres_pos(part%x)
    j = hres_pos(part%y)
  ! c     ivlvl=pdata(n)%z*10000.
  ! c     k=ivlevel(ivlvl)
    m = def_comp(part%icomp)%to_output
    if(part%z >= part%tbl) then
    !..in boundary layer
      avgbq1(i,j,m) = avgbq1(i,j,m) + part%rad()
    else
    !..above boundary layer
      avgbq2(i,j,m) = avgbq2(i,j,m) + part%rad()
    end if
  end do

!..accumulated/integrated concentration
  block
  use snapfldML, only: sigma_level_of_surface_h => field1, surface_temp => field2, &
                       surface_pressure =>field3, dh=>field_hr3, t1, t2
  use snaptabML, only: hypsometric_eq_inv
  integer :: ilvl

  if (surface_layer_is_lowest_level) then
    do n=1,npart
      part = pdata(n)
      ilvl = part%z*10000.0
      k = ivlayer(ilvl)
      if (k==1) then
        i = hres_pos(part%x)
        j = hres_pos(part%y)
        m = def_comp(part%icomp)%to_output
        concen(i,j,m) = concen(i,j,m) + dble(part%rad())
      endif
    enddo
    dh(:,:) = rt1*hlayer1(:,:,1) + rt2*hlayer2(:,:,1)
  else
    ! Use a predetermined height as the surface layer
    ! Approximate temperature by using first model layer
    surface_temp(:,:) = rt1*t1(:,:,2) + rt2*t2(:,:,2)
    surface_pressure(:,:) = rt1*ps1 + rt2*ps2
    ! pressure at 30m height (negative depth)
    sigma_level_of_surface_h(:,:) = hypsometric_eq_inv(-surface_height_m, surface_pressure, surface_temp)
    sigma_level_of_surface_h(:,:) = sigma_level_of_surface_h / surface_pressure

    concen = 0.0
    do n=1,npart
      part = pdata(n)

      i = hres_pos(part%x)
      j = hres_pos(part%y)
      if(part%z > sigma_level_of_surface_h(i,j)) then
        m = def_comp(part%icomp)%to_output
        concen(i,j,m) = concen(i,j,m) + dble(part%rad())
      end if
    end do
    dh(:,:) = surface_height_m
  end if

  do m=1,nocomp
    concen(:,:,m) = concen(:,:,m) / (dh * garea)
    concacc(:,:,m) = concacc(:,:,m) + concen(:,:,m)*hrstep
  end do

  end block

  if(imodlevel .and. modlevel_is_average) then
    call accumulate_ml_field()
  end if

  if (compute_column_max_conc) then
    max_column_scratch = 0.0
    do n=1,npart
      part = pdata(n)
      i = hres_pos(part%x)
      j = hres_pos(part%y)
      ivlvl = part%z*10000.
      k = ivlayer(ivlvl)
    !..in each sigma/eta (input model) layer
      max_column_scratch(i,j,k) = max_column_scratch(i,j,k) + part%rad()
    end do

    do k=1,nk-1
      do j = 1, ny*output_resolution_factor
        do i = 1, nx*output_resolution_factor
          dh = rt1*hlayer1(i,j,k) + rt2*hlayer2(i,j,k)
          max_column_scratch(i,j,k) = max_column_scratch(i,j,k)/(dh*garea(i,j))
        end do
      end do
    end do

    max_column_concentration(:,:) = max( &
              max_column_concentration(:,:), &
              maxval(max_column_scratch(:,:,:), dim=3))
  endif

  ! Compute inhalation dose for aircraft equivalent following
  ! "Dose calculations in aircrafts after Fukushima nuclear power
  !  plant accident - Preliminary study for aviation operations",
  ! Vargas et. al. 2019, JER
  ! Doses due to cloud immersion and deposition is neglected
  if (compute_aircraft_doserate) then
    block
    ! use all running components separately since doses might be different
    USE snapparML, only: ncomp, run_comp

    real, parameter :: regulatory_minimum_pressure = 750 ! hPa
    real, parameter :: inside_temperature = 20 + 275.15

    aircraft_doserate_scratch = 0.0
    ! Flatten particles to grid
    do n=1,npart
      part = pdata(n)
      i = hres_pos(part%x)
      j = hres_pos(part%y)
      ivlvl = part%z*10000.
      k = ivlayer(ivlvl)
      m = def_comp(part%icomp)%to_running
    !..in each sigma/eta (input model) layer
      aircraft_doserate_scratch(i,j,k,m) = aircraft_doserate_scratch(i,j,k,m) + part%rad()
    enddo

    ! Normalise by volume to obtain concentration
    do n=1,ncomp
      do k=2,nk-1
        do j = 1, ny*output_resolution_factor
          do i = 1, nx*output_resolution_factor
            dh = rt1*hlayer1(i,j,k) + rt2*hlayer2(i,j,k)
            aircraft_doserate_scratch(i,j,k,n) = aircraft_doserate_scratch(i,j,k,n)/(dh*garea(i,j))
          enddo
        enddo
      enddo
    enddo

    ! Correct for aircraft compressing outside air to +20 C, 750hPa
    do k=2,nk-1
      do j = 1, ny*output_resolution_factor
        do i = 1, nx*output_resolution_factor
          ii = lres_pos(i)
          ji = lres_pos(j)
          outside_pressure = rt1*(alevel(k) + blevel(k)*ps1(ii,ji)) + rt2*(alevel(k)+blevel(k)*ps2(ii,ji))
          outside_temperature = rt1*t1_abs(ii,ji,k) + rt2*t2_abs(ii,ji,k)
          inside_pressure = max(outside_pressure, regulatory_minimum_pressure)

          do n=1,ncomp
            aircraft_doserate_scratch(i,j,k,n) = aircraft_doserate_scratch(i,j,k,n) * &
              inside_pressure / outside_pressure * &
              outside_temperature / inside_temperature
          enddo
        end do
      end do
    enddo

    ! Weight the dose contributions from each isotope
    do n=1,ncomp
      if (run_comp(n)%defined%DPUI <= 0) cycle
      aircraft_doserate_scratch(:,:,:,n) = aircraft_doserate_scratch(:,:,:,n) * run_comp(n)%defined%DPUI
      ! Sum dose contributions
      aircraft_doserate_scratch(:,:,:,ncomp+1) = aircraft_doserate_scratch(:,:,:,ncomp+1) + &
        aircraft_doserate_scratch(:,:,:,n)
    enddo

    if (aircraft_doserate_threshold > 0.0) then
      do k=2,nk-2
        do j = 1, ny*output_resolution_factor
          do i = 1, nx*output_resolution_factor
            ii = lres_pos(i)
            ji = lres_pos(j)
              doserate = aircraft_doserate_scratch(i,j,k,ncomp+1)
            pressure = rt1*(alevel(k+1) + blevel(k+1)*ps1(ii,ji)) + rt2*(alevel(k+1)+blevel(k+1)*ps2(ii,ji))
            ! NOAA conversion formula www.weather.gov/media/epz/wxcalc/pressureConversion.pdf
            pressure_altitude = 0.3048 * (1 - (pressure / 1013.25) ** 0.190284) * 145366.45
            if (doserate > aircraft_doserate_threshold) then
              aircraft_doserate_threshold_height(i,j) = max(aircraft_doserate_threshold_height(i,j), pressure_altitude)
            end if
          end do
        end do
      end do
    endif

    ! Take max over column, skip k=1 since this we do not have temp here
    aircraft_doserate_scratch(:,:,1,ncomp+1) = 0.0
    do k=2,nk
      aircraft_doserate_scratch(:,:,1,ncomp+1) = &
          max(aircraft_doserate_scratch(:,:,1,ncomp+1), &
              aircraft_doserate_scratch(:,:,k,ncomp+1))
    end do

    aircraft_doserate(:,:) = max(aircraft_doserate, &
      aircraft_doserate_scratch(:,:,1,ncomp+1))
    end block
  endif
end subroutine

end module fldout_ncML
