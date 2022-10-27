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

  public fldout_nc, initialize_output, accumulate_fields

!> fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
  real, parameter :: cscale = 1.0
!> fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m3)
  real, parameter :: dscale = 1.0

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
    integer :: vd
  end type

!> Variables in a file
  type :: common_var
    integer :: accum_prc
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
    integer :: garea
    type(component_var) :: comp(mcomp)
  end type

!> dimensions used in a file
  type :: common_dim
    integer :: x
    integer :: y
    integer :: k
    integer :: t
    integer :: ncomp
    integer :: maxcompname
  end type

  !> file base time
  type(datetime_t), save :: iftime

  !> Number of times fields has been accumulated before
  !> being flushed to file
  integer, save :: naverage = 0

  contains

subroutine fldout_nc(filename, itime,tf1,tf2,tnow, &
    ierror)
  USE iso_fortran_env, only: int16
  USE snapgrdML, only: imodlevel, imslp, precipitation_in_output, &
      itotcomp, compute_column_max_conc, compute_aircraft_doserate, &
      aircraft_doserate_threshold, &
      output_column, output_vd
  USE snapfldML, only: field1, field2, field3, field4, &
      depdry, depwet, &
      avgbq1, avgbq2, garea, pmsl1, pmsl2, hbl1, hbl2, &
      accdry, accwet, avgprec, concen, ps1, ps2, avghbl, &
      concacc, accprec, max_column_concentration, aircraft_doserate, &
      aircraft_doserate_threshold_height, &
      vd_dep
  USE snapparML, only: time_profile, ncomp, run_comp, def_comp, &
    TIME_PROFILE_BOMB
  USE snapdebug, only: iulog, idebug
  USE ftestML, only: ftest
  USE snapdimML, only: nx, ny
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

  integer :: i,j,m,mm,n
  logical :: compute_total_dry_deposition
  logical :: compute_total_wet_deposition
  real :: rt1,rt2,scale,average
  type(common_var) :: varid

  real, parameter :: undef = NF90_FILL_FLOAT

  type(Particle) :: part
  type(duration_t) :: dur

  compute_total_dry_deposition = any(def_comp%kdrydep == 1) .and. ncomp > 1
  compute_total_wet_deposition = any(def_comp%kwetdep == 1) .and. ncomp > 1

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
  isize = [nx, ny, 1]

  average=float(naverage)
  naverage=0

!..for linear interpolation in time
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)

!..surface pressure (if model level output, for vertical crossections)
  if(imodlevel) then
    field1(:,:) = rt1*ps1 + rt2*ps2
    if(idebug == 1) call ftest('ps', field1)
    call check(nf90_put_var(iunit, varid%ps, start=ipos, count=isize, &
        values=field1), "set_ps")
  end if

!..total accumulated precipitation from start of run
  if(precipitation_in_output) then
    accprec(:,:) = accprec + avgprec
    field1(:,:) = accprec
    if(idebug == 1) call ftest('accprec', field1)

    call check(nf90_put_var(iunit, varid%accum_prc, start=ipos, count=isize, &
        values=field1), "set_accum_prc")
  end if

!..mslp (if switched on)
  if(imslp == 1) then
    field1(:,:) = rt1*pmsl1 + rt2*pmsl2
    if(idebug == 1) call ftest('mslp', field1)

    call check(nf90_put_var(iunit, varid%mslp, start=ipos, count=isize, &
        values=field1), "set_mslp")
  end if

!..instant height of boundary layer
  field4(:,:) = rt1*hbl1 + rt2*hbl2
  if(idebug == 1) call ftest('hbl', field4)

  call check(nf90_put_var(iunit, varid%ihbl, start=ipos, count=isize, &
      values=field4), "set_ihbl")

!..average height of boundary layer
  field1(:,:) = avghbl / average
  if(idebug == 1) call ftest('avghbl', field1)

  call check(nf90_put_var(iunit, varid%ahbl, start=ipos, count=isize, &
      values=field1), "set_ahbl")

!..precipitation accummulated between field output
  if(precipitation_in_output) then
    field1(:,:) = avgprec
    if(idebug == 1) call ftest('prec', field1)

    call check(nf90_put_var(iunit, varid%prc, start=ipos, count=isize, &
        values=field1), "set_prc")
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

  do m=1,ncomp

    mm = run_comp(m)%to_defined

  !..instant Bq in and above boundary layer
    field1 = 0.0
    field2 = 0.0
    bqtot1 = 0.0
    bqtot2 = 0.0
    nptot1 = 0
    nptot2 = 0

    do n=1,npart
      part = pdata(n)
      if(part%icomp == mm) then
        i = nint(part%x)
        j = nint(part%y)
        if(part%z >= part%tbl) then
          field1(i,j) = field1(i,j) + part%rad
          bqtot1 = bqtot1 + dble(part%rad)
          nptot1 = nptot1 + 1
        else
          field2(i,j) = field2(i,j) + part%rad
          bqtot2 = bqtot2 + dble(part%rad)
          nptot2 = nptot2 + 1
        end if
      end if
    end do

    write(iulog,*) ' component: ', def_comp(mm)%compname
    write(iulog,*) '   Bq,particles in    abl: ',bqtot1,nptot1
    write(iulog,*) '   Bq,particles above abl: ',bqtot2,nptot2
    write(iulog,*) '   Bq,particles          : ',bqtot1+bqtot2, &
        nptot1+nptot2


    if (output_column) then
      field3 = field1 + field2
      field3 = field3 / garea
      call check(nf90_put_var(iunit, varid%comp(m)%conc_column, start=ipos, count=isize, &
        values=field3), "output_column")
    endif

  !..instant part of Bq in boundary layer
    scale = 100.
    where (field1 + field2 > 0.0)
      field3 = scale*field1 / (field1 + field2)
    elsewhere
      field3 = undef
    endwhere

  !..instant concentration in boundary layer; field4 = hbl
    field2(:,:) = cscale*field1 / (field4*garea)
    if(idebug == 1) call ftest('conc', field2)

    call check(nf90_put_var(iunit, varid%comp(m)%icbl, start=ipos, count=isize, &
        values=field2), "set_icbl")

  !..average concentration in boundary layer
    field1(:,:) = cscale*avgbq1(:,:,m)/(garea*avghbl)
    if(idebug == 1) call ftest('avgconc', field1)

    call check(nf90_put_var(iunit, varid%comp(m)%acbl, start=ipos, count=isize, &
        values=field1), "set_acbl")

  !..dry deposition
    if (def_comp(mm)%kdrydep == 1) then
      field1(:,:) = dscale*sngl(depdry(:,:,m)) / garea
      accdry(:,:,m) = accdry(:,:,m) + depdry(:,:,m)
      if(idebug == 1) call ftest('dry', field1)

      call check(nf90_put_var(iunit, varid%comp(m)%idd, start=ipos, count=isize, &
          values=field1), "set_idd(m)")

      if (output_vd) then
        call check(nf90_put_var(iunit, varid%comp(m)%vd, start=[ipos], count=[isize], &
            values=vd_dep(:, :, m)), "dry_deposition_velocity(m)")
      endif
    end if

  !..wet deposition
    if (def_comp(mm)%kwetdep == 1) then
      field1(:,:) = dscale*sngl(depwet(:,:,m))/garea
      accwet(:,:,m) = accwet(:,:,m) + depwet(:,:,m)
      if(idebug == 1) call ftest('wet', field1)

      call check(nf90_put_var(iunit, varid%comp(m)%iwd, start=ipos, count=isize, &
          values=field1), "set_iwd(m)")
    end if

  !..accumulated dry deposition
    if (def_comp(mm)%kdrydep == 1) then
      field1(:,:) = dscale*sngl(accdry(:,:,m))/garea
      if(idebug == 1) call ftest('adry', field1)

      call check(nf90_put_var(iunit, varid%comp(m)%accdd, start=ipos, count=isize, &
          values=field1), "set_accdd(m)")
    end if

  !..accumulated wet deposition
    if (def_comp(mm)%kwetdep == 1) then
      field1(:,:) = dscale*sngl(accwet(:,:,m))/garea
      if(idebug == 1) call ftest('awet', field1)

      call check(nf90_put_var(iunit, varid%comp(m)%accwd, start=ipos, count=isize, &
          values=field1), "set_accwd(m)")
    end if

  !..instant part of Bq in boundary layer
    if(idebug == 1) call ftest('pbq', field3, contains_undef=.true.)

  !..average part of Bq in boundary layer
    scale=100.

    do j=1,ny
      do i=1,nx
        if (avgbq1(i,j,m) + avgbq2(i,j,m) > 0.0) then
          field3(i,j) = scale*avgbq1(i,j,m) / (avgbq1(i,j,m) + avgbq2(i,j,m))
        else
          field3(i,j) = undef
        endif
      end do
    end do
    if(idebug == 1) call ftest('apbq', field3, contains_undef=.true.)

  !..instant concentration on surface (not in felt-format)
    field3(:,:) = concen(:,:,m)
    if(idebug == 1) call ftest('concen', field3, contains_undef=.true.)
    call check(nf90_put_var(iunit, varid%comp(m)%ic, start=ipos, count=isize, &
        values=field3), "set_ic(m)")

  !..accumulated/integrated concentration surface = dose
    field3(:,:) = concacc(:,:,m)
    if(idebug == 1) call ftest('concac', field3, contains_undef=.true.)

    call check(nf90_put_var(iunit, varid%comp(m)%ac, start=ipos, count=isize, &
        values=field3), "set_ac(m)")
  !.....end do m=1,ncomp

  end do


!..total parameters (sum of all components).............................

  if(ncomp > 1 .AND. itotcomp == 1) then

  !..total instant Bq in and above boundary layer
    field1(:,:) = 0.0
    field2(:,:) = 0.0

    do n=1,npart
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      if(pdata(n)%z >= pdata(n)%tbl) then
        field1(i,j)=field1(i,j)+pdata(n)%rad
      else
        field2(i,j)=field2(i,j)+pdata(n)%rad
      end if
    end do

  !..total instant part of Bq in boundary layer
    scale=100.
    where (field1 + field2 > 0.0)
      field3 = scale*field1 / (field1 + field2)
    elsewhere
      field3 = undef
    endwhere

  !..total instant concentration in boundary layer
  ! field4 : hbl
    field2(:,:) = cscale*field1/(field4*garea)
    if(idebug == 1) call ftest('tconc', field2)
    call check(nf90_put_var(iunit, varid%icblt, start=ipos, count=isize, &
        values=field2), "set_icblt(m)")

  !..total average concentration in boundary layer
    field1(:,:) = sum(avgbq1, dim=3)
    field1(:,:) = cscale*field1/(garea*avghbl)
    if(idebug == 1) call ftest('tavgconc', field1)
    call check(nf90_put_var(iunit, varid%acblt, start=ipos, count=isize, &
        values=field1), "set_acblt")

  !..total dry deposition
    if(compute_total_dry_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm = run_comp(m)%to_defined
        if (def_comp(mm)%kdrydep == 1) then
          field1(:,:) = field1 + depdry(:,:,m)
        end if
      end do
      field1(:,:) = dscale*field1/garea
      if(idebug == 1) call ftest('tdry', field1)
      call check(nf90_put_var(iunit, varid%iddt, start=ipos, count=isize, &
          values=field1), "set_iddt")
    end if

  !..total wet deposition
    if(compute_total_wet_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm = run_comp(m)%to_defined
        if (def_comp(mm)%kwetdep == 1) then
          field1(:,:) = field1 + depwet(:,:,m)
        end if
      end do
      field1(:,:) = dscale*field1/garea
      if(idebug == 1) call ftest('twet', field1)
      call check(nf90_put_var(iunit, varid%iwdt, start=ipos, count=isize, &
          values=field1), "set_iwdt")
    end if

  !..total accumulated dry deposition
    if(compute_total_dry_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm = run_comp(m)%to_defined
        if (def_comp(mm)%kdrydep == 1) then
          field1(:,:) = field1 + accdry(:,:,m)
        end if
      end do
      field1(:,:) = dscale*field1/garea
      if(idebug == 1) call ftest('tadry', field1)
      call check(nf90_put_var(iunit, varid%accddt, start=ipos, count=isize, &
          values=field1), "set_accddt")
    end if

  !..total accumulated wet deposition
    if(compute_total_wet_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm = run_comp(m)%to_defined
        if (def_comp(mm)%kwetdep == 1) then
          field1(:,:) = field1 + accwet(:,:,m)
        end if
      end do
      field1(:,:) = dscale*field1/garea
      if(idebug == 1) call ftest('tawet', field1)
      call check(nf90_put_var(iunit, varid%accwdt, start=ipos, count=isize, &
          values=field1), "set_accwdt")
    end if

  !..total instant part of Bq in boundary layer
    if(idebug == 1) call ftest('tpbq', field3, contains_undef=.true.)


  !..total average part of Bq in boundary layer
    scale=100.
    field1(:,:) = sum(avgbq1, dim=3)
    field2(:,:) = sum(avgbq2, dim=3)
    where (field1 + field2 > 0.0)
      field3 = scale*field1 / (field1 + field2)
    elsewhere
      field3 = undef
    endwhere
    if(idebug == 1) call ftest('tapbq', field3, contains_undef=.true.)

  !..total accumulated/integrated concentration
    field3(:,:) = sum(concacc, dim=3)
    if(idebug == 1) call ftest('concac', field3, contains_undef=.true.)

    call check(nf90_put_var(iunit, varid%act, start=ipos, count=isize, &
        values=field3), "set_act")

  !.....end if(ncomp.gt.1 .and. itotcomp.eq.1) then

  end if


!..BOMB fields..........................................................

  if (time_profile == TIME_PROFILE_BOMB) then

  !..bomb parameters for each component.........

    do m=1,ncomp

      mm = run_comp(m)%to_defined

      if(idebug == 1) write(iulog,*) ' component: ', def_comp(mm)%compname

    !..scale to % of total released Bq (in a single bomb)
      dblscale= 100.0d0/dble(run_comp(m)%totalbq)

    !..dry deposition
      if (def_comp(mm)%kdrydep == 1) then
        field1(:,:) = dblscale*depdry(:,:,m)
        if(idebug == 1) call ftest('dry%', field1)
      end if

    !..wet deposition
      if (def_comp(mm)%kwetdep == 1) then
        field1(:,:) = dblscale*depwet(:,:,m)
        if(idebug == 1) call ftest('wet%', field1)
      end if

    !..accumulated dry deposition
      if (def_comp(mm)%kdrydep == 1) then
        field1(:,:) = dblscale*accdry(:,:,m)
        if(idebug == 1) call ftest('adry%', field1)
      end if

    !..accumulated wet deposition
      if (def_comp(mm)%kwetdep == 1) then
        field1(:,:) = dblscale*accwet(:,:,m)
        if(idebug == 1) call ftest('awet%', field1)
      end if

    !.......end do m=1,ncomp
    end do

  end if


!..model level fields...................................................
  if (imodlevel) then
    call write_ml_fields(iunit, varid, average, [1, 1, -1, ihrs_pos], [nx, ny, 1, 1], rt1, rt2)
  endif

! reset fields
  do m=1,ncomp
    mm = run_comp(m)%to_defined
    if (def_comp(mm)%kdrydep == 1) then
      depdry(:,:,m) = 0.0
    end if
    if (def_comp(mm)%kwetdep == 1) then
      depwet(:,:,m) = 0.0
    end if
  end do

  call check(nf90_sync(iunit))
  call check(nf90_close(iunit))
end subroutine fldout_nc


subroutine write_ml_fields(iunit, varid, average, ipos_in, isize, rt1, rt2)
  USE releaseML, only: nplume, iplume
  USE particleML, only: pdata, Particle
  USE snapparML, only: def_comp, ncomp
  USE snapfldML, only: field1, field4, &
      hlayer1, hlayer2, garea, avgbq
  USE ftestML, only: ftest
  USE snapdebug, only: idebug
  USE snapgrdML, only: itotcomp, modleveldump, ivlayer
  USE snapdimML, only: nx,ny,nk

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

!..concentration in each layer
!..(height only computed at time of output)

  ipos(:) = ipos_in

!..loop for 1=average and 2=instant concentration
!..(now computing average first, then using the same arrays for instant)
  do loop=1,2

    if(loop == 1) then
      avg=average
    else
      avg=1.
      total=0.
      maxage=0

      avgbq = 0.0

      do npl = 1, nplume
        do n = iplume(npl)%start, iplume(npl)%end
          part = pdata(n)
          i = nint(part%x)
          j = nint(part%y)
          ivlvl = part%z*10000.
          k = ivlayer(ivlvl)
          m = def_comp(part%icomp)%to_running
        !..in each sigma/eta (input model) layer
          if (modleveldump > 0) then
          !.. dump and remove old particles, don't touch  new ones
            if (iplume(npl)%ageInSteps >= nint(modleveldump)) then
              maxage = max(maxage, int(iplume(npl)%ageInSteps, kind(maxage)))
              avgbq(i,j,k,m) = avgbq(i,j,k,m) + part%rad
              total = total + part%rad
              part%active = .FALSE.
              part%rad = 0.
              pdata(n) = part
            end if
          else
            avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(n)%rad
          endif
        end do
        end do
      if (modleveldump > 0) then
        write (error_unit,*) "dumped; maxage, total", maxage, total
      endif
    end if

    do k=1,nk-1
      do j=1,ny
        do i=1,nx
          dh = rt1*hlayer1(i,j,k) + rt2*hlayer2(i,j,k)
          field1(:,:) = dh
          field4(:,:) = dh*garea(i,j)*avg
        end do
      end do

      do m=1,ncomp
        avgbq(:,:,k,m) = avgbq(:,:,k,m)/field4
      end do
    end do

  !..average concentration in each layer for each type
    do m=1,ncomp
      do k=1,nk-1
        field1(:,:) = cscale*avgbq(:,:,k,m)
        if(idebug == 1) call ftest('avconcl', field1)

        if (loop == 2) then
          ipos(3) = k
          call check(nf90_put_var(iunit, varid%comp(m)%icml, start=ipos, &
              count=isize, values=field1), "icml(m)")
        endif
      end do
    end do

  !..total average concentration in each layer
    if(ncomp > 1 .AND. itotcomp == 1) then
      do m=2,ncomp
        do k=1,nk-1
          avgbq(:,:,k,1) = avgbq(:,:,k,1) + avgbq(:,:,k,m)
        end do
      end do
      do k=1,nk-1
        field1(:,:) = cscale*avgbq(:,:,k,1)
        if(idebug == 1) call ftest('tavconcl', field1)
      end do
    end if
  end do
end subroutine


subroutine nc_declare(iunit, dimids, varid, varname, units, stdname, chunksize)
  USE snapdebug, only: iulog
  integer, intent(in) :: iunit
  integer, intent(out)   :: varid
  integer, intent(in)    :: dimids(:)

  character(len=*), intent(in) :: units
  character(len=*), intent(in) :: varname
  character(len=*), intent(in), optional :: stdname
  integer, intent(in), optional :: chunksize(:)

  write(iulog,"('declaring ' (a) ' ' (a) )",advance="NO") varname, units
  if (present(stdname)) write(iulog, "(' ' (a))",advance="NO") trim(stdname)
  write(iulog,'()',advance="YES")

  call check(nf90_def_var(iunit, TRIM(varname), &
      NF90_FLOAT, dimids, varid), "def_"//varname)
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

subroutine nc_declare_put(iunit, dimids, varid, varname, units, stdname, chunksize, values)
  integer, intent(in) :: iunit
  integer, intent(out)   :: varid
  integer, intent(in)    :: dimids(:)

  character(len=*), intent(in) :: units
  character(len=*), intent(in) :: varname
  character(len=*), intent(in), optional :: stdname
  integer, intent(in), optional :: chunksize(:)

  real, intent(in) :: values(:, :)

  call nc_declare(iunit, dimids, varid, varname, units, stdname, chunksize)
  call check(nf90_put_var(iunit, varid, values), "Could not put values")
end subroutine

subroutine nc_set_vtrans(iunit, kdimid,k_varid,ap_varid,b_varid)
  use snapgrdML, only: vlevel, alevel, blevel
  INTEGER, INTENT(IN) :: iunit, kdimid
  INTEGER, INTENT(OUT) :: k_varid, ap_varid, b_varid
  INTEGER ::p0_varid

  call check(nf90_def_var(iunit, "k", &
      NF90_FLOAT, kdimid, k_varid), "def_k")
  call check(nf90_put_att(iunit,k_varid, "standard_name", &
      TRIM("atmosphere_hybrid_sigma_pressure_coordinate")))
  call check(nf90_put_att(iunit,k_varid, "formula", &
      TRIM("p(n,k,j,i) = ap(k) + b(k)*ps(n,j,i)")))
  call check(nf90_put_att(iunit,k_varid, "formula_terms", &
      TRIM("ap: ap b: b ps: surface_air_pressure p0: p0")))
  call check(nf90_put_att(iunit,k_varid, "positive", TRIM("down")))
  call check(nf90_put_var(iunit, k_varid, vlevel(2:)))

  call check(nf90_def_var(iunit, "ap", &
      NF90_FLOAT, kdimid, ap_varid), "def_ap")
  call check(nf90_put_att(iunit,ap_varid, "units", TRIM("hPa")))
  call check(nf90_put_var(iunit, ap_varid, alevel(2:)))

  call check(nf90_def_var(iunit, "b", &
      NF90_FLOAT, kdimid, b_varid), "def_b")
  call check(nf90_put_var(iunit, b_varid, blevel(2:)))

  call check(nf90_def_var(iunit, "p0", &
      NF90_FLOAT, varid=p0_varid))
  call check(nf90_put_att(iunit,p0_varid, "units", &
      TRIM("hPa")))
  call check(nf90_put_var(iunit, p0_varid, 100))

end subroutine nc_set_vtrans

subroutine nc_set_projection(iunit, xdimid, ydimid, &
    igtype,nx,ny,gparam,garea, xm, ym, &
    simulation_start)
  INTEGER, INTENT(IN) :: iunit, xdimid, ydimid, igtype, nx, ny
  REAL(real32), INTENT(IN):: gparam(8)
  REAL(real32), INTENT(IN), DIMENSION(nx,ny) :: garea
  REAL(real32), INTENT(IN), DIMENSION(nx,ny) :: xm
  REAL(real32), INTENT(IN), DIMENSION(nx,ny) :: ym
  CHARACTER(LEN=19), INTENT(IN)  :: simulation_start

  INTEGER :: i, j, ierror, x_varid, y_varid, proj_varid, &
  lon_varid, lat_varid, carea_varid, mapx_varid, mapy_varid, &
  dimids(2)
  REAL(KIND=real32) :: xvals(nx), yvals(ny), lon(nx,ny), lat(nx,ny), &
  val, gparam2(6)
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

  select case(igtype)
  case(2) !..geographic
    call check(nf90_put_att(iunit,x_varid, "units", &
        TRIM("degrees_east")))
    call check(nf90_put_att(iunit,y_varid, "units", &
        TRIM("degrees_north")))
    call check(nf90_put_att(iunit,proj_varid, &
        "grid_mapping_name", TRIM("latitude_longitude")))
    do i=1,nx
      xvals(i) = gparam(1) + (i-1)*gparam(3)
    end do
    do i=1,ny
      yvals(i) = gparam(2) + (i-1)*gparam(4)
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

    do i=1,nx
      xvals(i) = gparam(1) + (i-1)*gparam(3)
    end do
    do i=1,ny
      yvals(i) = gparam(2) + (i-1)*gparam(4)
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
    do i=1,nx
      xvals(i) = (i-gparam(1))*gparam(7)
    end do
    do i=1,ny
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
    xvals(1) = (xvals(1)-1)*gparam(7)
    yvals(1) = (yvals(1)-1)*gparam(8)
  ! xvals is currently the lowerd left corner in plane-coordinates
  ! but must be in m from center
    do i=2,nx
      xvals(i) = xvals(1) + (i-1)*gparam(7)
    end do
    do i=2,ny
      yvals(i) = yvals(1) + (i-1)*gparam(8)
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
  call check(nf90_put_var(iunit, lon_varid, lon))
  call check(nf90_put_var(iunit, lat_varid, lat))

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

  call check(nf90_put_var(iunit, mapx_varid, xm))

  call check(nf90_def_var(iunit, "map_factor_y", &
      NF90_FLOAT, dimids, mapy_varid))
  call check(nf90_put_att(iunit,mapy_varid, "units", "1"))
  call check(nf90_put_att(iunit,mapy_varid, "grid_mapping", &
      TRIM("projection")))
  call check(nf90_put_att(iunit,mapy_varid, "coordinates", &
      TRIM("longitude latitude")))

  call check(nf90_put_var(iunit, mapy_varid, ym))

  call check(nf90_sync(iunit))
end subroutine nc_set_projection

subroutine initialize_output(filename, itime, ierror)
  USE snapfilML, only: ncsummary, nctitle, simulation_start
  USE snapgrdML, only: gparam, igtype, imodlevel, imslp, precipitation_in_output, &
      itotcomp, modleveldump, compute_column_max_conc, compute_aircraft_doserate, &
      aircraft_doserate_threshold, &
      output_vd, output_column
  USE snapfldML, only:  &
      garea, &
      xm, ym, &
      nhfout
  USE snapparML, only: ncomp, run_comp, def_comp
  USE ftestML, only: ftest
  USE snapdimML, only: nx, ny, nk
  USE particleML, only: Particle

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

  compute_total_dry_deposition = any(def_comp%kdrydep == 1) .and. ncomp > 1
  compute_total_wet_deposition = any(def_comp%kwetdep == 1) .and. ncomp > 1


    call check(nf90_create(filename, ior(NF90_NETCDF4, NF90_CLOBBER), iunit), filename)
    call check(nf90_def_dim(iunit, "time", NF90_UNLIMITED, dimid%t), &
        "t-dim")
    call check(nf90_def_dim(iunit, "x", nx, dimid%x), "x-dim")
    call check(nf90_def_dim(iunit, "y", ny, dimid%y), "y-dim")
    call check(nf90_def_dim(iunit, "k", nk-1, dimid%k), "k-dim")
    call check(nf90_def_dim(iunit, "ncomp", ncomp, dimid%ncomp), "ncomp-dim")
    call check(nf90_def_dim(iunit, "compnamelenmax", len(def_comp(1)%compname), dimid%maxcompname), "maxcompname-dim")

    if (allocated(nctitle)) then
      call check(nf90_put_att(iunit, NF90_GLOBAL, &
          "title", trim(nctitle)))
    endif
    call check(nf90_put_att(iunit, NF90_GLOBAL, &
        "summary", trim(ncsummary)))

    call nc_set_projection(iunit, dimid%x, dimid%y, &
        igtype,nx,ny,gparam, garea, xm, ym, &
        simulation_start)
    if (imodlevel) then
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

    call nc_declare_put(iunit, dimids2d, varid%garea, "area", units="m2", &
      stdname="area", chunksize=[nx,ny], values=garea)

    call check(nf90_def_var(iunit, "components", NF90_CHAR, [dimid%maxcompname, dimid%ncomp], varid%components))

    do m=1,ncomp
      mm= run_comp(m)%to_defined
      call check(nf90_put_var(iunit, varid%components, &
        start=[1, m], count=[len_trim(def_comp(mm)%compnamemc), 1], &
        values=trim(def_comp(mm)%compnamemc)))
      call nc_declare(iunit, dimids3d, varid%comp(m)%ic, &
        trim(def_comp(mm)%compnamemc)//"_concentration", &
        units="Bq/m3", chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%comp(m)%icbl, &
        trim(def_comp(mm)%compnamemc)//"_concentration_bl", &
        units="Bq/m3", chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%comp(m)%ac, &
        trim(def_comp(mm)%compnamemc)//"_acc_concentration", &
        units="Bq*hr/m3", chunksize=chksz3d)
      call nc_declare(iunit, dimids3d, varid%comp(m)%acbl, &
        trim(def_comp(mm)%compnamemc)//"_avg_concentration_bl", &
        "Bq/m3", chunksize=chksz3d)
      if (def_comp(mm)%kdrydep > 0) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%idd, &
          trim(def_comp(mm)%compnamemc)//"_dry_deposition", &
          units="Bq/m2", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%comp(m)%accdd, &
          trim(def_comp(mm)%compnamemc)//"_acc_dry_deposition", &
          units="Bq/m2", chunksize=chksz3d)
      end if
      if (def_comp(mm)%kwetdep > 0) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%iwd, &
          trim(def_comp(mm)%compnamemc)//"_wet_deposition", &
            units="Bq/m2", chunksize=chksz3d)
        call nc_declare(iunit, dimids3d, varid%comp(m)%accwd, &
          trim(def_comp(mm)%compnamemc)//"_acc_wet_deposition", &
          units="Bq/m2", chunksize=chksz3d)
      end if
      if (imodlevel) then
        if (modleveldump > 0.) then
          string = trim(def_comp(mm)%compnamemc)//"_concentration_dump_ml"
        else
          string = trim(def_comp(mm)%compnamemc)//"_concentration_ml"
        endif
        call nc_declare(iunit, dimids4d, varid%comp(m)%icml, &
          string, units="Bq/m3", chunksize=chksz4d)
      !           call nc_declare(iunit, dimids4d, acml_varid(m), &
      !     +          TRIM(def_comp(mm)%compnamemc)//"_avg_concentration_ml", &
      !     +          units="Bq*hour/m3")
      end if
      if (output_column) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%conc_column, &
          trim(def_comp(mm)%compnamemc)//"_column_concentration", &
          units="Bq/m2", chunksize=chksz3d)
      endif

      if (def_comp(mm)%kdrydep > 0 .and. output_vd) then
        call nc_declare(iunit, dimids3d, varid%comp(m)%vd, &
          trim(def_comp(mm)%compnamemc)//"_dry_deposition_velocity", &
          units="m/s", chunksize=chksz3d)
      endif

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
end subroutine

subroutine get_varids(iunit, varid, ierror)
  USE snapparML, only: ncomp, run_comp, def_comp
  USE snapgrdML, only: imodlevel, modleveldump, output_vd
  integer, intent(in) :: iunit
  type(common_var), intent(out) :: varid
  integer, intent(out) :: ierror

  integer :: m, mm
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
  ierror = nf90_inq_varid(iunit, "p0", varid%ps)
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

  do m=1,ncomp
    mm = run_comp(m)%to_defined
    varname = trim(def_comp(mm)%compnamemc) // "_concentration"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%ic)
    if (ierror /= NF90_NOERR) return

    varname = trim(def_comp(mm)%compnamemc) // "_concentration_bl"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%icbl)
    if (ierror /= NF90_NOERR) return

    varname = trim(def_comp(mm)%compnamemc) // "_acc_concentration"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%ac)
    if (ierror /= NF90_NOERR) return

    varname = trim(def_comp(mm)%compnamemc) // "_avg_concentration_bl"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%acbl)
    if (ierror /= NF90_NOERR) return

    if (def_comp(mm)%kdrydep == 1) then
      varname = trim(def_comp(mm)%compnamemc) // "_dry_deposition"
      ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%idd)
      if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

      varname = trim(def_comp(mm)%compnamemc) // "_acc_dry_deposition"
      ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%accdd)
      if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

      if (output_vd) then
        varname = trim(def_comp(mm)%compnamemc) // "_dry_deposition_velocity"
        ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%vd)
        if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
      endif
    endif

    if (def_comp(mm)%kwetdep == 1) then
      varname = trim(def_comp(mm)%compnamemc) // "_wet_deposition"
      ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%iwd)
      if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return

      varname = trim(def_comp(mm)%compnamemc) // "_acc_wet_deposition"
      ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%accwd)
      if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
    endif

    if (imodlevel) then
      if (modleveldump > 0.) then
        varname = trim(def_comp(mm)%compnamemc) // "_concentration_dump_ml"
        ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%icml)
        if (ierror /= NF90_NOERR) return
      else
        varname = trim(def_comp(mm)%compnamemc) // "_concentration_ml"
        ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%icml)
        if (ierror /= NF90_NOERR) return
      endif
    endif

    varname = trim(def_comp(mm)%compnamemc) // "_column_concentration"
    ierror = nf90_inq_varid(iunit, varname, varid%comp(m)%conc_column)
    if (ierror /= NF90_NOERR .and. .not. ierror == NF90_ENOTVAR) return
  enddo

  ierror = NF90_NOERR
end subroutine

!> accumulation for average fields
subroutine accumulate_fields(tf1, tf2, tnow, tstep, nsteph)
  USE snapgrdML, only: imodlevel, &
      ivlayer, compute_column_max_conc, compute_aircraft_doserate, &
      alevel, blevel, aircraft_doserate_threshold
  USE snapfldML, only:  &
      avgbq1, avgbq2, hlayer1, hlayer2, hbl1, hbl2, &
      avgprec, concen, avghbl, dgarea, &
      avgbq, concacc, precip, &
      max_column_scratch, max_column_concentration, garea, &
      ps1, ps2, t1_abs, t2_abs, aircraft_doserate_scratch, aircraft_doserate, &
      aircraft_doserate_threshold_height
  USE snapdimml, only: nx, ny, nk
  USE snapparML, only: ncomp, def_comp, run_comp
  USE ftestML, only: ftest
  USE releaseML, only: npart
  USE particleML, only: pdata, Particle

  real, intent(in) :: tf1, tf2, tnow, tstep
  integer, intent(in) :: nsteph

  real :: rt1, rt2, dh
  real :: outside_pressure, outside_temperature, inside_pressure
  real :: pressure, pressure_altitude, doserate
  real :: scale
  integer :: i, j, m, n, k
  integer :: ivlvl
  type(Particle) :: part
  real :: hrstep

  if(naverage == 0) then
    avghbl = 0.0
    avgprec = 0.0

    avgbq1 = 0.0
    avgbq2 = 0.0

    if (imodlevel) then
      avgbq = 0.0
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
    i = nint(part%x)
    j = nint(part%y)
  ! c     ivlvl=pdata(n)%z*10000.
  ! c     k=ivlevel(ivlvl)
    m = def_comp(part%icomp)%to_running
    if(part%z >= part%tbl) then
    !..in boundary layer
      avgbq1(i,j,m) = avgbq1(i,j,m) + part%rad
    else
    !..above boundary layer
      avgbq2(i,j,m) = avgbq2(i,j,m) + part%rad
    end if
  end do

!..accumulated/integrated concentration

  concen = 0.0

  do n=1,npart
    part = pdata(n)
    ivlvl=part%z*10000.
    k=ivlayer(ivlvl)
    if(k == 1) then
      i = nint(part%x)
      j = nint(part%y)
      m = def_comp(part%icomp)%to_running
      concen(i,j,m) = concen(i,j,m) + dble(part%rad)
    end if
  end do

  do m=1,ncomp
    do j=1,ny
      do i=1,nx
        if(concen(i,j,m) > 0.0d0) then
          dh= rt1*hlayer1(i,j,1)+rt2*hlayer2(i,j,1)
          concen(i,j,m)= concen(i,j,m)/(dh*dgarea(i,j))
          concacc(i,j,m)= concacc(i,j,m) + concen(i,j,m)*hrstep
        end if
      end do
    end do
  end do

  if(imodlevel) then
    do n=1,npart
      part = pdata(n)
      i = nint(part%x)
      j = nint(part%y)
      ivlvl = part%z*10000.
      k = ivlayer(ivlvl)
      m = def_comp(part%icomp)%to_running
    !..in each sigma/eta (input model) layer
      avgbq(i,j,k,m) = avgbq(i,j,k,m) + part%rad
    end do
  end if

  if (compute_column_max_conc) then
    max_column_scratch = 0.0
    do n=1,npart
      part = pdata(n)
      i = nint(part%x)
      j = nint(part%y)
      ivlvl = part%z*10000.
      k = ivlayer(ivlvl)
    !..in each sigma/eta (input model) layer
      max_column_scratch(i,j,k) = max_column_scratch(i,j,k) + part%rad
    end do

    do k=1,nk-1
      do j = 1, ny
        do i = 1, nx
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
    real, parameter :: regulatory_minimum_pressure = 750 ! hPa
    real, parameter :: inside_temperature = 20 + 275.15

    aircraft_doserate_scratch = 0.0
    ! Flatten particles to grid
    do n=1,npart
      part = pdata(n)
      i = nint(part%x)
      j = nint(part%y)
      ivlvl = part%z*10000.
      k = ivlayer(ivlvl)
      m = def_comp(part%icomp)%to_running
    !..in each sigma/eta (input model) layer
      aircraft_doserate_scratch(i,j,k,m) = aircraft_doserate_scratch(i,j,k,m) + part%rad
    enddo

    ! Normalise by volume to obtain concentration
    do n=1,ncomp
      do k=2,nk-1
        do j = 1, ny
          do i = 1, nx
            dh = rt1*hlayer1(i,j,k) + rt2*hlayer2(i,j,k)
            aircraft_doserate_scratch(i,j,k,n) = aircraft_doserate_scratch(i,j,k,n)/(dh*garea(i,j))
          enddo
        enddo
      enddo
    enddo

    ! Correct for aircraft compressing outside air to +20 C, 750hPa
    do k=2,nk-1
      do j = 1, ny
        do i = 1, nx
          outside_pressure = rt1*(alevel(k) + blevel(k)*ps1(i,j)) + rt2*(alevel(k)+blevel(k)*ps2(i,j))
          outside_temperature = rt1*t1_abs(i,j,k) + rt2*t2_abs(i,j,k)
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
        do j = 1, ny
          do i = 1, nx
            doserate = aircraft_doserate_scratch(i,j,k,ncomp+1)
            pressure = rt1*(alevel(k+1) + blevel(k+1)*ps1(i,j)) + rt2*(alevel(k+1)+blevel(k+1)*ps2(i,j))
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
    aircraft_doserate_scratch(:,:,1,ncomp+1) = maxval(aircraft_doserate_scratch(:,:,2:,ncomp+1), dim=3)

    aircraft_doserate(:,:) = max(aircraft_doserate, &
      aircraft_doserate_scratch(:,:,1,ncomp+1))
    end block
  endif
end subroutine

end module fldout_ncML
