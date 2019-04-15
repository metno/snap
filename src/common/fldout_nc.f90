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
  USE iso_fortran_env, only: real32, real64
  USE readfield_ncML, only: check
  USE milibML, only: xyconvert, gridpar, hrdiff
  implicit none
  private

  public fldout_nc

  contains

subroutine fldout_nc(iwrite,iunit,filnam,itime,tf1,tf2,tnow,tstep, &
  istep,nsteph,ierror)
  USE iso_fortran_env, only: int16
  USE snapfilML
  USE snapgrdML
  USE snapfldML
  USE snapparML
  USE snaptabML
  USE snapargosML
  USE snapdebug, only: iulog, idebug
  USE ftestML, only: ftest
  USE netcdf
  USE snapdimML, only: mcomp, ldata, nx, ny, nk, nxmc, nymc
! netcdf
!  Purpose:  Accumulation for average fields (iwrite=0,1).
!            Make and write output fields (iwrite=1).
!	     Initialization of output accumulation arrays (iwrite=-1).
!            Fields written to a sequential 'model output' file,
!            not opened here.

!---------------------------------------------------------------------
!  Field parameter numbers used here:
!     *   8 - surface pressure (if model level output) (hPa)
!     *  17 - precipitation accummulated from start of run (mm)
!     *  58 - mean sea level pressure, mslp (if switched on) (hPa)
!     * 500 - instant height of boundary layer (m)
!     * 501 - average height of boundary layer (m)
!     * 502 - precipitation accummulated between field output (mm)
!	      (better scaling than param. 17 for long runs)
!     * 510 - instant concentration in boundary layer (Bq/m3)
!     * 511 - average concentration in boundary layer (Bq/m3)
!     * 512 - dry deposition (for one time interval)  (Bq/m2)
!     * 513 - wet deposition (for one time interval)  (Bq/m2)
!     * 514 - dry deposition (accumulated from start) (Bq/m2)
!     * 515 - wet deposition (accumulated from start) (Bq/m2)
!     * 516 - instant part of Bq in the boundary layer  (%)
!     * 517 - average part of Bq in the boundary layer  (%)
!     * 518 - accumulated concentration in the lower layer (Bq*hr/m3)
!     * 519 - instant     concentration in the lower layer (Bq/m3)
!     * 521 - BOMB dry deposition (for one time interval),  % of release
!     * 522 - BOMB wet deposition (for one time interval),  % of release
!     * 523 - BOMB dry deposition (accumulated from start), % of release
!     * 524 - BOMB wet deposition (accumulated from start), % of release
!       540-569 - instant concentration in each layer (Bq/m3)
!       570-599 - average concentration in each layer (Bq/m3)
!     * 901 - geographic latitude  (degrees)
!     * 902 - geographic longitude (degrees)
!     * 903 - grid square area (m2)
!     ^------- current output
!---------------------------------------------------------------------
!  Notes:
!    -  data type (field ident. no. 3) is set to:
!              3 if forecast length (itime(5)) is 0 (valid time, +0)
!              2 if forecast length (itime(5)) is greater than 0
!              4 if geographic latitude, longitude, grid square area
!    -  for parameter 510-517 and 521-524 the component is identified
!	in field ident. no. 7 (usually 'level').
!    -  for parameter 540-569 : 540=total, 540+idcomp for each type
!       for parameter 570-599 : 570=total, 570+idcomp for each type
!	(the idcomp maximum is then 29, i.e. max 29 components)
!    -  parameters 500 - 509 for fields not dependant on component
!	parameters 510 - 539 for fields dependant on component
!	(level = idcomp to identify components,  0 = total)
!    -  possible use of grid square area, garea  (param. 903):
!	    Bq in boundary layer
!		= concentration/(garea*hbl)
!	    Bq above boundary layer
!		= (1.0-part_of_bq_in_bl)*concentration/(garea*hbl)
!---------------------------------------------------------------------

  USE particleML
  USE fileInfoML
  implicit none


!     *   ps 8 - surface pressure (if model level output) (hPa)
!     *  accum_prc 17 - precipitation accummulated from start of run (mm)
!     *  mslp 58 - mean sea level pressure, mslp (if switched on) (hPa)
!     * ihbl 500 - instant height of boundary layer (m)
!     * ahbl 501 - average height of boundary layer (m)
!     * 502 - precipitation accummulated between field output (mm)
!       (better scaling than param. 17 for long runs)
!     * icbl 510 - instant concentration in boundary layer (Bq/m3)
!     * acbl 511 - average concentration in boundary layer (Bq/m3)
!     * idd 512 - dry deposition (for one time interval)  (Bq/m2)
!     * iwd 513 - wet deposition (for one time interval)  (Bq/m2)
!     * accdd 514 - dry deposition (accumulated from start) (Bq/m2)
!     * accwd 515 - wet deposition (accumulated from start) (Bq/m2)
!     * 516 - instant part of Bq in the boundary layer  (%)
!     * 517 - average part of Bq in the boundary layer  (%)
!     * ac 518 - accumulated concentration in the surface layer (Bq*hr/m3)
!     * ic 519 - instant     concentration in the surface layer (Bq/m3)
!     * 521 bdd - BOMB dry deposition (for one time interval),  % of release
!     * 522 bwd - BOMB wet deposition (for one time interval),  % of release
!     * 523 accbdd - BOMB dry deposition (accumulated from start), % of release
!     * 524 accbwd - BOMB wet deposition (accumulated from start), % of release
!       icml 540-569 - instant concentration in each layer (Bq/m3)
!       acml 570-599 - average concentration in each layer (Bq/m3)
!     * 901 - geographic latitude  (degrees)
!     * 902 - geographic longitude (degrees)
!     * 903 - grid square area (m2)
!     ^------- current output

  integer, save :: ps_varid, accum_prc_varid, prc_varid, &
  mslp_varid, &
  icblt_varid,acblt_varid,act_varid,iddt_varid,iwdt_varid, &
  accddt_varid,accwdt_varid, &
  ihbl_varid, ahbl_varid, &
  icbl_varid(mcomp), &
  acbl_varid(mcomp), idd_varid(mcomp), iwd_varid(mcomp), &
  accdd_varid(mcomp), accwd_varid(mcomp), ac_varid(mcomp), &
  ic_varid(mcomp), icml_varid(mcomp)
! intger, save :: acml_varid(mcomp)

  integer ::   iwrite,iunit,istep,nsteph,ierror
  integer ::   itime(5)
  real ::      tf1,tf2,tnow,tstep
  character*(*) filnam

  integer, save :: naverage = 0
  logical, save :: acc_initialized = .false.

  integer :: dimids2d(2),dimids3d(3),dimids4d(4), ipos(4), isize(4), &
             chksz3d(3), chksz4d(4)
  integer, save :: x_dimid, y_dimid, k_dimid, t_dimid
  integer, save :: t_varid, k_varid, ap_varid, b_varid
  integer, save  :: iftime(5), ihrs, ihrs_pos


  integer ::          nptot1,nptot2
  real(real64) :: bqtot1,bqtot2
  real(real64) :: dblscale

  integer :: maxage
  integer :: i,j,k,m,mm,n,ivlvl,idextr,loop
  logical :: is_dry_deposition
  logical :: is_wet_deposition
  integer :: ko,lvla,lvlb
  integer, save :: numfields = 0
  real ::    rt1,rt2,scale,average,averinv,hbl
!> fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
  real, parameter :: cscale= 1.0
!> fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m3)
  real, parameter :: dscale = 1.0

  real, parameter :: undef = NF90_FILL_FLOAT
  real ::    avg,hrstep,dh,total

  integer, save :: itimeargos(5) = [0, 0, 0, 0, 0]

  character(len=256) :: string

  is_dry_deposition = any(kdrydep == 1)
  is_wet_deposition = any(kwetdep == 1)

  ierror=0

!..initialization

  if(imodlevel == 1 .AND. (nxmc /= nx .OR. nymc /= ny)) imodlevel=0

  if(.not.acc_initialized) then
    depdry = 0.0
    depwet = 0.0
    accdry = 0.0
    accwet = 0.0
    concen = 0.0
    concacc = 0.0
    accprec = 0.0

    acc_initialized = .true.
  end if

  if(iwrite == -1) then
  !..initialization of routine (not of file)
  !..iwrite=-1: istep is no. of field outputs
    n=ncomp
    if(ncomp > 1 .AND. itotcomp == 1) n=n+1
    numfields= 2+n*9
    if(itprof == 2) numfields= numfields + ncomp*4
    if(inprecip > 0) numfields=numfields+2
    if(imslp    > 0) numfields=numfields+1
    if(imodlevel > 0) numfields=numfields+n*nk*2+nk+1
    numfields= numfields*istep + 4
    if(numfields > 32767) numfields=32767
    do i=1,5
      itimeargos(i)=itime(i)
    end do
    return
  end if

!..accumulation for average fields......................................

  if(naverage == 0) then
    avghbl = 0.0
    avgprec = 0.0

    avgbq1 = 0.0
    avgbq2 = 0.0

  !..note: model level output on if nxmc=nx, nymc=ny and imodlevel=1
    if(imodlevel == 1) then
      avgbq = 0.0
    end if
  end if

  naverage=naverage+1

!..for time interpolation
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)
  hrstep=1./float(nsteph)

!..height of boundary layer
  do j=1,ny
    do i=1,nx
      avghbl(i,j)=avghbl(i,j)+(rt1*hbl1(i,j)+rt2*hbl2(i,j))
    end do
  end do

!..precipitation (no time interpolation, but hourly time intervals)
  scale=tstep/3600.
  do j=1,ny
    do i=1,nx
      avgprec(i,j)=avgprec(i,j)+scale*precip(i,j,iprecip)
    end do
  end do

  do n=1,npart
    i=nint(pdata(n)%x)
    j=nint(pdata(n)%y)
  ! c     ivlvl=pdata(n)%z*10000.
  ! c     k=ivlevel(ivlvl)
    m=iruncomp(icomp(n))
    if(pdata(n)%z >= pdata(n)%tbl) then
    !..in boundary layer
      avgbq1(i,j,m)=avgbq1(i,j,m)+pdata(n)%rad
    else
    !..above boundary layer
      avgbq2(i,j,m)=avgbq2(i,j,m)+pdata(n)%rad
    end if
  end do

!..accumulated/integrated concentration

  concen = 0.0

  do n=1,npart
    ivlvl=pdata(n)%z*10000.
    k=ivlayer(ivlvl)
    if(k == 1) then
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      m=iruncomp(icomp(n))
      concen(i,j,m)= concen(i,j,m)+dble(pdata(n)%rad)
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

  if(imodlevel == 1) then
    do n=1,npart
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      ivlvl=pdata(n)%z*10000.
      k=ivlayer(ivlvl)
      m=iruncomp(icomp(n))
    !..in each sigma/eta (input model) layer
      avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(n)%rad
    end do
  end if

  if(iwrite == 0) then
    return
  end if


!..output...............................................................

  write(iulog,*) '*FLDOUT_NC*', numfields

  if(numfields > 0) then
  !..initialization of file
  !..remove an existing file and create a completely new one
    if (iunit /= 30) &
    call check(nf90_close(iunit))
    numfields=0
    write(iulog,*) 'creating fldout_nc: ',filnam
    ihrs_pos = 0
    call check(nf90_create(filnam, ior(NF90_NETCDF4, NF90_CLOBBER), iunit), filnam)
    call check(nf90_def_dim(iunit, "time", NF90_UNLIMITED, t_dimid), &
    "t-dim")
    call check(nf90_def_dim(iunit, "x", nx, x_dimid), "x-dim")
    call check(nf90_def_dim(iunit, "y", ny, y_dimid), "y-dim")
    call check(nf90_def_dim(iunit, "k", nk-1, k_dimid), "k-dim")

    if (nctitle /= "")  call check(nf90_put_att(iunit, NF90_GLOBAL, &
    "title", trim(nctitle)))
    call check(nf90_put_att(iunit, NF90_GLOBAL, &
    "summary", trim(ncsummary)))

    call nc_set_projection(iunit, x_dimid, y_dimid, &
    igtype,nx,ny,gparam, garea, xm, ym, &
    simulation_start)
    if (imodlevel == 1) &
    call nc_set_vtrans(iunit, k_dimid, k_varid, ap_varid, b_varid)

    call check(nf90_def_var(iunit, "time",NF90_FLOAT,t_dimid,t_varid))
    write(string,'(A12,I4,A1,I0.2,A1,I0.2,A1,I0.2,A12)') &
    "hours since ",itime(1),"-",itime(2),"-",itime(3)," ", &
    itime(4),":00:00 +0000"
    call check(nf90_put_att(iunit, t_varid, "units", &
    trim(string)))

  !..store the files base-time
    iftime = itime
    iftime(5) = 0

    dimids2d = [x_dimid, y_dimid]
    dimids3d = [x_dimid, y_dimid, t_dimid]
    dimids4d = [x_dimid, y_dimid, k_dimid, t_dimid]

    chksz3d = [nx, ny, 1]
    chksz4d = [nx, ny, 1, 1]

    if (imodlevel == 1) &
    call nc_declare_3d(iunit, dimids3d, ps_varid, &
    chksz3d, "surface_air_pressure", &
    "hPa", "surface_air_pressure", "")
    if (imslp == 1) &
    call nc_declare_3d(iunit, dimids3d, mslp_varid, &
    chksz3d, "air_pressure_at_sea_level", &
    "hPa", "air_pressure_at_sea_level", "")
    if (inprecip == 1) &
    call nc_declare_3d(iunit, dimids3d, accum_prc_varid, &
    chksz3d, "precipitation_amount_acc", &
    "kg/m2", "precipitation_amount", "")

    call nc_declare_3d(iunit, dimids3d, ihbl_varid, &
    chksz3d, "instant_height_boundary_layer", &
    "m", "height", &
    "instant_height_boundary_layer")
    call nc_declare_3d(iunit, dimids3d, ahbl_varid, &
    chksz3d, "average_height_boundary_layer", &
    "m", "height", &
    "average_height_boundary_layer")


    do m=1,ncomp
      mm=idefcomp(m)
      call nc_declare_3d(iunit, dimids3d, ic_varid(m), &
      chksz3d, TRIM(compnamemc(mm))//"_concentration", &
      "Bq/m3","", &
      TRIM(compnamemc(mm))//"_concentration")
      call nc_declare_3d(iunit, dimids3d, icbl_varid(m), &
      chksz3d, TRIM(compnamemc(mm))//"_concentration_bl", &
      "Bq/m3","", &
      TRIM(compnamemc(mm))//"_concentration_boundary_layer")
      call nc_declare_3d(iunit, dimids3d, ac_varid(m), &
      chksz3d, TRIM(compnamemc(mm))//"_acc_concentration", &
      "Bq*hr/m3","", &
      TRIM(compnamemc(mm))//"_accumulated_concentration")
      call nc_declare_3d(iunit, dimids3d, acbl_varid(m), &
      chksz3d, TRIM(compnamemc(mm))//"_avg_concentration_bl", &
      "Bq/m3","", &
      TRIM(compnamemc(mm))//"_average_concentration_bl")
      if (kdrydep(mm) > 0) then
        call nc_declare_3d(iunit, dimids3d, idd_varid(m), &
        chksz3d, TRIM(compnamemc(mm))//"_dry_deposition", &
        "Bq/m2","", &
        TRIM(compnamemc(mm))//"_dry_deposition")
        call nc_declare_3d(iunit, dimids3d, accdd_varid(m), &
        chksz3d, TRIM(compnamemc(mm))//"_acc_dry_deposition", &
        "Bq/m2","", &
        TRIM(compnamemc(mm))//"_accumulated_dry_deposition")
      end if
      if (kwetdep(mm) > 0) then
        call nc_declare_3d(iunit, dimids3d, iwd_varid(m), &
        chksz3d, TRIM(compnamemc(mm))//"_wet_deposition", &
        "Bq/m2","", &
        TRIM(compnamemc(mm))//"_wet_deposition")
        call nc_declare_3d(iunit, dimids3d, accwd_varid(m), &
        chksz3d, TRIM(compnamemc(mm))//"_acc_wet_deposition", &
        "Bq/m2","", &
        TRIM(compnamemc(mm))//"_accumulated_wet_deposition")
      end if
      if (imodlevel == 1) then
        if (modleveldump > 0.) then
          string = TRIM(compnamemc(mm))//"_concentration_dump_ml"
        else
          string = TRIM(compnamemc(mm))//"_concentration_ml"
        endif
        call nc_declare_4d(iunit, dimids4d, icml_varid(m), &
        chksz4d, TRIM(string), &
        "Bq/m3","", &
        TRIM(string))
      !           call nc_declare_4d(iunit, dimids4d, acml_varid(m),
      !     +          chksz4d, TRIM(compnamemc(mm))//"_avg_concentration_ml",
      !     +          "Bq*hour/m3","",
      !     +          TRIM(compnamemc(mm))//"_accumulated_concentration_ml")
      end if
    end do
    if (itotcomp == 1) then
      call nc_declare_3d(iunit, dimids3d, icblt_varid, &
      chksz3d, "total_concentration_bl", &
      "Bq/m3","", &
      "total_concentration_bl")
      call nc_declare_3d(iunit, dimids3d, acblt_varid, &
      chksz3d, "total_avg_concentration_bl", &
      "Bq/m3","", &
      "total_average_concentration_bl")
      call nc_declare_3d(iunit, dimids3d, act_varid, &
      chksz3d, "total_acc_concentration", &
      "Bq/m3","", &
      "total_accumulated_concentration")
      if (kdrydep(mm) > 0) then
        call nc_declare_3d(iunit, dimids3d, iddt_varid, &
        chksz3d, "total_dry_deposition", &
        "Bq/m2","", &
        "total_dry_deposition")
        call nc_declare_3d(iunit, dimids3d, accddt_varid, &
        chksz3d, "total_acc_dry_deposition", &
        "Bq/m2","", &
        "total_accumulated_dry_deposition")
      end if
      if (kwetdep(mm) > 0) then
        call nc_declare_3d(iunit, dimids3d, iwdt_varid, &
        chksz3d, "total_wet_deposition", &
        "Bq/m2","", &
        "total_wet_deposition")
        call nc_declare_3d(iunit, dimids3d, accwdt_varid, &
        chksz3d, "total_acc_wet_deposition", &
        "Bq/m2","", &
        "total_accumulated_wet_deposition")
      end if
    end if
    call check(nf90_enddef(iunit))
  end if

! set the runtime
  ihrs_pos = ihrs_pos + 1
  call hrdiff(0,0,iftime,itime,ihrs,ierror,ierror)
  call check(nf90_put_var(iunit,t_varid,start=[ihrs_pos],values=FLOAT(ihrs)), &
  "set time")

  ipos = [1, 1, ihrs_pos, ihrs_pos]
  isize = [nx, ny, 1, 1]

!..common field identification.............

  idata(1:20) = 0
  idata( 1)=iprodr
  idata( 2)=igridr
  idata( 3)=2
  if(itime(5) == 0) idata(3)=3
  idata( 4)=itime(5)
!.... idata( 5)= .......... vertical coordinate
!.... idata( 6)= .......... parameter no.
!.... idata( 7)= .......... level or level no. or component id
  idata( 8)=0
!.... idata( 9)= .......... set by gridpar
!.... idata(10)= .......... set by gridpar
!.... idata(11)= .......... set by gridpar
  idata(12)=itime(1)
  idata(13)=itime(2)*100+itime(3)
  idata(14)=itime(4)*100
!.... idata(15)= .......... set by gridpar
!.... idata(16)= .......... set by gridpar
!.... idata(17)= .......... set by gridpar
!.... idata(18)= .......... set by gridpar
!.... idata(19)= .......... 0 or sigma*10000 value
!.... idata(20)= .......... field scaling (automatic the best possible)

!..put grid parameters into field identification
!..(into the first 20 words and possibly also after space for data)
  call gridpar(-1,ldata,idata,igtype,nx,ny,gparam,ierror)
  if(ierror /= 0) goto 900


  average=float(naverage)
  averinv=1./float(naverage)
  naverage=0

!..for linear interpolation in time
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)

!..surface pressure (if model level output, for vertical crossections)
  if(imodlevel == 1) then
    do j=1,ny
      do i=1,nx
        field1(i,j)=rt1*ps1(i,j)+rt2*ps2(i,j)
      end do
    end do
    if(idebug == 1) call ftest('ps',1,1,nx,ny,1,field1,0)
    call check(nf90_put_var(iunit, ps_varid,start=[ipos],count=[isize],values=field1), &
    "set_ps")
  end if

!..total accumulated precipitation from start of run
  if(inprecip == 1) then
    do j=1,ny
      do i=1,nx
        accprec(i,j)=accprec(i,j)+avgprec(i,j)
        field1(i,j)=accprec(i,j)
      end do
    end do
    idextr=nint(float(istep)/float(nsteph))
    if(idebug == 1) call ftest('accprec',1,1,nx,ny,1,field1,0)

    call check(nf90_put_var(iunit, accum_prc_varid, start=[ipos], count=[isize], &
    values=field1), "set_accum_prc")
  end if

!..mslp (if switched on)
  if(imslp == 1) then
    do j=1,ny
      do i=1,nx
        field1(i,j)=rt1*pmsl1(i,j)+rt2*pmsl2(i,j)
      end do
    end do
    if(idebug == 1) call ftest('mslp',1,1,nx,ny,1,field1,0)

    call check(nf90_put_var(iunit, mslp_varid, start=[ipos], count=[isize], &
    values=field1), "set_mslp")
  end if

!..instant height of boundary layer
  do j=1,ny
    do i=1,nx
      field4(i,j)=rt1*hbl1(i,j)+rt2*hbl2(i,j)
    end do
  end do
  if(idebug == 1) call ftest('hbl',1,1,nx,ny,1,field4,0)

  call check(nf90_put_var(iunit, ihbl_varid, start=[ipos], count=[isize], &
  values=field4), "set_ihbl")

!..average height of boundary layer
  do j=1,ny
    do i=1,nx
      field1(i,j)=avghbl(i,j)*averinv
    end do
  end do
  if(idebug == 1) call ftest('avghbl',1,1,nx,ny,1,field1,0)

  call check(nf90_put_var(iunit, ahbl_varid, start=[ipos], count=[isize], &
  values=field1), "set_ahbl")

!..precipitation accummulated between field output // currently disable use 1 to write
  if(inprecip == -1) then
    do j=1,ny
      do i=1,nx
        field1(i,j)=avgprec(i,j)
      end do
    end do
    idextr=nint(average*tstep/3600.)
    if(idebug == 1) call ftest('prec',1,1,nx,ny,1,field1,0)

    call check(nf90_put_var(iunit, prc_varid, start=[ipos], count=[isize], &
    values=field1), "set_prc")
  end if

!..parameters for each component......................................

  do m=1,ncomp

    mm=idefcomp(m)

  !..instant Bq in and above boundary layer
    field1 = 0.0
    field2 = 0.0
    bqtot1=0.0d0
    bqtot2=0.0d0
    nptot1=0
    nptot2=0

    do n=1,npart
      if(icomp(n) == mm) then
        i=nint(pdata(n)%x)
        j=nint(pdata(n)%y)
        if(pdata(n)%z >= pdata(n)%tbl) then
          field1(i,j)=field1(i,j)+pdata(n)%rad
          bqtot1=bqtot1+dble(pdata(n)%rad)
          nptot1=nptot1+1
        else
          field2(i,j)=field2(i,j)+pdata(n)%rad
          bqtot2=bqtot2+dble(pdata(n)%rad)
          nptot2=nptot2+1
        end if
      end if
    end do

    write(iulog,*) ' component: ',compname(mm)
    write(iulog,*) '   Bq,particles in    abl: ',bqtot1,nptot1
    write(iulog,*) '   Bq,particles above abl: ',bqtot2,nptot2
    write(iulog,*) '   Bq,particles          : ',bqtot1+bqtot2, &
    nptot1+nptot2

  !..instant part of Bq in boundary layer
    scale=100.
    do j=1,ny
      do i=1,nx
        if(field1(i,j)+field2(i,j) > 0.) then
          field3(i,j)=scale*field1(i,j)/(field1(i,j)+field2(i,j))
        else
          field3(i,j)=undef
        end if
      end do
    end do

  !..instant concentration in boundary layer
    do j=1,ny
      do i=1,nx
      ! c         hbl=rt1*hbl1(i,j)+rt2*hbl2(i,j)
        hbl=field4(i,j)
        field2(i,j)=cscale*field1(i,j)/(hbl*garea(i,j))
      end do
    end do
    if(idebug == 1) call ftest('conc',1,1,nx,ny,1,field2,0)

    call check(nf90_put_var(iunit, icbl_varid(m), start=[ipos], count=[isize], &
    values=field2), "set_icbl")

  !..average concentration in boundary layer
    do j=1,ny
      do i=1,nx
        field1(i,j)=cscale*avgbq1(i,j,m) &
        /(garea(i,j)*avghbl(i,j))
      end do
    end do
    if(idebug == 1) call ftest('avgconc',1,1,nx,ny,1,field1,0)

    call check(nf90_put_var(iunit, acbl_varid(m), start=[ipos], count=[isize], &
    values=field1), "set_acbl")

  !..dry deposition
    if(kdrydep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(depdry(i,j,m))/garea(i,j)
          accdry(i,j,m)=accdry(i,j,m)+depdry(i,j,m)
        end do
      end do
      if(idebug == 1) call ftest('dry',1,1,nx,ny,1,field1,0)

      call check(nf90_put_var(iunit, idd_varid(m), start=[ipos], count=[isize], &
      values=field1), "set_idd(m)")
    end if

  !..wet deposition
    if(kwetdep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(depwet(i,j,m))/garea(i,j)
          accwet(i,j,m)=accwet(i,j,m)+depwet(i,j,m)
        end do
      end do
      if(idebug == 1) call ftest('wet',1,1,nx,ny,1,field1,0)

      call check(nf90_put_var(iunit, iwd_varid(m), start=[ipos], count=[isize], &
      values=field1), "set_iwd(m)")
    end if

  !..accumulated dry deposition
    if(kdrydep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(accdry(i,j,m))/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('adry',1,1,nx,ny,1,field1,0)

      call check(nf90_put_var(iunit, accdd_varid(m),start=[ipos],count=[isize], &
      values=field1), "set_accdd(m)")
    end if

  !..accumulated wet deposition
    if(kwetdep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(accwet(i,j,m))/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('awet',1,1,nx,ny,1,field1,0)

      call check(nf90_put_var(iunit, accwd_varid(m),start=[ipos],count=[isize], &
      values=field1), "set_accwd(m)")
    end if

  !..instant part of Bq in boundary layer
    if(idebug == 1) call ftest('pbq',1,1,nx,ny,1,field3,1)

  !..average part of Bq in boundary layer
    scale=100.
    do j=1,ny
      do i=1,nx
        if(avgbq1(i,j,m)+avgbq2(i,j,m) > 0.) then
          field3(i,j)=scale*avgbq1(i,j,m) &
          /(avgbq1(i,j,m)+avgbq2(i,j,m))
        else
          field3(i,j)=undef
        end if
      end do
    end do
    if(idebug == 1) call ftest('apbq',1,1,nx,ny,1,field3,1)

  !..instant concentration on surface (not in felt-format)
    do j=1,ny
      do i=1,nx
        field3(i,j)= sngl(concen(i,j,m))
      end do
    end do
    if(idebug == 1) call ftest('concen',1,1,nx,ny,1,field3,1)
    call check(nf90_put_var(iunit, ic_varid(m),start=[ipos],count=[isize], &
    values=field3), "set_ic(m)")

  !..accumulated/integrated concentration surface = dose
    do j=1,ny
      do i=1,nx
        field3(i,j)= sngl(concacc(i,j,m))
      end do
    end do
    if(idebug == 1) call ftest('concac',1,1,nx,ny,1,field3,1)

    call check(nf90_put_var(iunit, ac_varid(m),start=[ipos],count=[isize], &
    values=field3), "set_ac(m)")
  !.....end do m=1,ncomp
  end do


!..total parameters (sum of all components).............................

  if(ncomp > 1 .AND. itotcomp == 1) then

  !..total instant Bq in and above boundary layer
    field1 = 0.0
    field2 = 0.0

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
    do j=1,ny
      do i=1,nx
        if(field1(i,j)+field2(i,j) > 0.) then
          field3(i,j)=scale*field1(i,j)/(field1(i,j)+field2(i,j))
        else
          field3(i,j)=undef
        end if
      end do
    end do

  !..total instant concentration in boundary layer
    do j=1,ny
      do i=1,nx
      ! c         hbl=rt1*hbl1(i,j)+rt2*hbl2(i,j)
        hbl=field4(i,j)
        field2(i,j)=cscale*field1(i,j)/(hbl*garea(i,j))
      end do
    end do
    if(idebug == 1) call ftest('tconc',1,1,nx,ny,1,field2,0)
    call check(nf90_put_var(iunit, icblt_varid,start=[ipos],count=[isize], &
    values=field2), "set_icblt(m)")

  !..total average concentration in boundary layer
    field1 = 0.0
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          field1(i,j)=field1(i,j)+avgbq1(i,j,m)
        end do
      end do
    end do
    do j=1,ny
      do i=1,nx
        field1(i,j)=cscale*field1(i,j) &
        /(garea(i,j)*avghbl(i,j))
      end do
    end do
    if(idebug == 1) call ftest('tavgconc',1,1,nx,ny,1,field1,0)
    call check(nf90_put_var(iunit, acblt_varid,start=[ipos],count=[isize], &
    values=field1), "set_acblt")

  !..total dry deposition
    if(is_dry_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm=idefcomp(m)
        if(kdrydep(mm) == 1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=field1(i,j)+sngl(depdry(i,j,m))
            end do
          end do
        end if
      end do
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*field1(i,j)/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('tdry',1,1,nx,ny,1,field1,0)
      call check(nf90_put_var(iunit, iddt_varid,start=[ipos],count=[isize], &
      values=field1), "set_iddt")

    end if

  !..total wet deposition
    if(is_wet_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm=idefcomp(m)
        if(kwetdep(mm) == 1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=field1(i,j)+sngl(depwet(i,j,m))
            end do
          end do
        end if
      end do
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*field1(i,j)/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('twet',1,1,nx,ny,1,field1,0)
      call check(nf90_put_var(iunit, iwdt_varid,start=[ipos],count=[isize], &
      values=field1), "set_iwdt")
    end if

  !..total accumulated dry deposition
    if(is_dry_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm=idefcomp(m)
        if(kdrydep(mm) == 1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=field1(i,j)+sngl(accdry(i,j,m))
            end do
          end do
        end if
      end do
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*field1(i,j)/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('tadry',1,1,nx,ny,1,field1,0)
      call check(nf90_put_var(iunit, accddt_varid,start=[ipos],count=[isize], &
      values=field1), "set_accddt")
    end if

  !..total accumulated wet deposition
    if(is_wet_deposition) then
      field1 = 0.0
      do m=1,ncomp
        mm=idefcomp(m)
        if(kwetdep(mm) == 1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=field1(i,j)+sngl(accwet(i,j,m))
            end do
          end do
        end if
      end do
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*field1(i,j)/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('tawet',1,1,nx,ny,1,field1,0)
      call check(nf90_put_var(iunit, accwdt_varid,start=[ipos],count=[isize], &
      values=field1), "set_accwdt")
    end if

  !..total instant part of Bq in boundary layer
    if(idebug == 1) call ftest('tpbq',1,1,nx,ny,1,field3,1)


  !..total average part of Bq in boundary layer
    scale=100.
    field1 = 0.0
    field2 = 0.0
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          field1(i,j)=field1(i,j)+sngl(avgbq1(i,j,m))
          field2(i,j)=field2(i,j)+sngl(avgbq2(i,j,m))
        end do
      end do
    end do
    do j=1,ny
      do i=1,nx
        if(field1(i,j)+field2(i,j) > 0.) then
          field3(i,j)=scale*field1(i,j) &
          /(field1(i,j)+field2(i,j))
        else
          field3(i,j)=undef
        end if
      end do
    end do
    if(idebug == 1) call ftest('tapbq',1,1,nx,ny,1,field3,1)

  !..total accumulated/integrated concentration
    field3 = 0.0
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          field3(i,j)= field3(i,j) + sngl(concacc(i,j,m))
        end do
      end do
    end do
    if(idebug == 1) call ftest('concac',1,1,nx,ny,1,field3,1)

    call check(nf90_put_var(iunit, act_varid,start=[ipos],count=[isize], &
    values=field3), "set_act")

  !.....end if(ncomp.gt.1 .and. itotcomp.eq.1) then
  end if


!..BOMB fields..........................................................

  if (itprof == 2) then

  !..bomb parameters for each component.........

    do m=1,ncomp

      mm= idefcomp(m)

      if(idebug == 1) write(iulog,*) ' component: ',compname(mm)

    !..scale to % of total released Bq (in a single bomb)
      dblscale= 100.0d0/dble(totalbq(mm))

    !..dry deposition
      if(kdrydep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*depdry(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('dry%',1,1,nx,ny,1,field1,0)
      end if

    !..wet deposition
      if(kwetdep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*depwet(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('wet%',1,1,nx,ny,1,field1,0)
      end if

    !..accumulated dry deposition
      if(kdrydep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*accdry(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('adry%',1,1,nx,ny,1,field1,0)
      end if

    !..accumulated wet deposition
      if(kwetdep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*accwet(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('awet%',1,1,nx,ny,1,field1,0)
      end if

    !.......end do m=1,ncomp
    end do

  end if


!..model level fields...................................................

  if(imodlevel /= 1) goto 800

! write k, ap, b - will be overwritten several times, but not data/timecritical
  call check(nf90_put_var(iunit, k_varid, vlevel(2)), "set_k")
  call check(nf90_put_var(iunit, ap_varid, alevel(2)), "set_ap")
  call check(nf90_put_var(iunit, b_varid, blevel(2)), "set_b")


!..concentration in each layer
!..(height only computed at time of output)

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

      do n=1,npart
        i=nint(pdata(n)%x)
        j=nint(pdata(n)%y)
        ivlvl=pdata(n)%z*10000.
        k=ivlayer(ivlvl)
        m=iruncomp(icomp(n))
      !..in each sigma/eta (input model) layer
        if (modleveldump > 0) then
        !.. dump and remove old particles, don't touch  new ones
          if (pdata(n)%ageInSteps >= nint(modleveldump)) then
            if (pdata(n)%ageInSteps > maxage) &
            maxage=pdata(n)%ageInSteps
            avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(n)%rad
            total = total + pdata(n)%rad
            pdata(n)%active = .FALSE.
            pdata(n)%rad = 0.
          end if
        else
          avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(n)%rad
        endif
      end do
      if (modleveldump > 0) then
        write(*,*) "dumped; maxage, total", maxage, total
      endif
    end if

    do k=1,nk-1
      do j=1,ny
        do i=1,nx
          dh=rt1*hlayer1(i,j,k)+rt2*hlayer2(i,j,k)
          field1(i,j)=dh
          field4(i,j)=dh*garea(i,j)*avg
        end do
      end do
    !.. write out layer-height
      if (loop == 1) then
        ko=klevel(k+1)
        lvla=nint(alevel(k+1)*10.)
        lvlb=nint(blevel(k+1)*10000.)
        if(ivcoor == 2) lvla=0
      end if

      do m=1,ncomp
        do j=1,ny
          do i=1,nx
            avgbq(i,j,k,m)=avgbq(i,j,k,m)/field4(i,j)
          end do
        end do
      end do
    end do

  !..average concentration in each layer for each type
    do m=1,ncomp
      do k=1,nk-1
        do j=1,ny
          do i=1,nx
            field1(i,j)=cscale*sngl(avgbq(i,j,k,m))
          end do
        end do
        if(idebug == 1) call ftest('avconcl',1,1,nx,ny,1,field1,0)
        ko=klevel(k+1)
        lvla=nint(alevel(k+1)*10.)
        lvlb=nint(blevel(k+1)*10000.)
        if(ivcoor == 2) lvla=0

        if (loop == 2) then
          ipos(3) = k
          call check(nf90_put_var(iunit, icml_varid(m), start=ipos, &
          count=isize, values=field1), "icml(m)")
        ! reset ipos(3) for 3d fields to time-pos (=ipos(4))
          ipos(3) = ipos(4)
        endif
      end do
    end do

  !..total average concentration in each layer
    if(ncomp > 1 .AND. itotcomp == 1) then
      do m=2,ncomp
        do k=1,nk-1
          do j=1,ny
            do i=1,nx
              avgbq(i,j,k,1)=avgbq(i,j,k,1)+avgbq(i,j,k,m)
            end do
          end do
        end do
      end do
      do k=1,nk-1
        do j=1,ny
          do i=1,nx
            field1(i,j)=cscale*avgbq(i,j,k,1)
          end do
        end do
        if(idebug == 1) call ftest('tavconcl',1,1,nx,ny,1,field1,0)
        ko=klevel(k+1)
        lvla=nint(alevel(k+1)*10.)
        lvlb=nint(blevel(k+1)*10000.)
        if(ivcoor == 2) lvla=0
      end do
    end if

  !.....end do loop=1,2
  end do

  800 ierror=0

  do m=1,ncomp
    mm=idefcomp(m)
    if(kdrydep(mm) == 1) then
      depdry(:,:,m) = 0.0
    end if
    if(kwetdep(mm) == 1) then
      depwet(:,:,m) = 0.0
    end if
  end do

  call check(nf90_sync(iunit))
  return

  900 ierror=1
  write(iulog,*) '*FLDOUT_NC*  Terminates due to write error.'

  return
end subroutine fldout_nc


subroutine nc_declare_3d(iunit, dimids, varid, &
  chksz, varnm, &
  units, stdnm, metnm)
  USE netcdf
  USE snapdebug, only: iulog
  implicit none
  INTEGER, INTENT(OUT)   :: varid
  INTEGER, INTENT(IN)    :: iunit, dimids(3), chksz(3)
  CHARACTER(LEN=*), INTENT(IN) :: varnm, stdnm, metnm, units

  if (.false.) write(*,*) chksz ! Silence compiler

  write(iulog,*) "declaring ", iunit, TRIM(varnm), TRIM(units) &
  ,TRIM(stdnm),TRIM(metnm)
  call check(nf90_def_var(iunit, TRIM(varnm), &
  NF90_FLOAT, dimids, varid), "def_"//varnm)
!       call check(NF_DEF_VAR_CHUNKING(iunit, varid, NF_CHUNKED, chksz))
  call check(NF90_DEF_VAR_DEFLATE(iunit, varid, 1,1,1))
  call check(nf90_put_att(iunit,varid, "units", TRIM(units)))
  if (LEN_TRIM(stdnm) > 0) &
  call check(nf90_put_att(iunit,varid,"standard_name", TRIM(stdnm)))
!       if (LEN_TRIM(metnm).gt.0)
!     +    call check(nf_put_att_text(iunit,varid,"metno_name",
!     +    LEN_TRIM(metnm), TRIM(metnm)))

  call check(nf90_put_att(iunit,varid,"coordinates", "longitude latitude"))
  call check(nf90_put_att(iunit,varid,"grid_mapping", "projection"))

end subroutine nc_declare_3d

subroutine nc_declare_4d(iunit, dimids, varid, &
  chksz, varnm, &
  units, stdnm, metnm)
  USE netcdf
  USE snapdebug, only: iulog
  implicit none
  INTEGER, INTENT(OUT)   :: varid
  INTEGER, INTENT(IN)    :: iunit, dimids(4), chksz(4)
  CHARACTER(LEN=*), INTENT(IN) :: varnm, stdnm, metnm, units


  write(iulog,*) "declaring ", iunit, TRIM(varnm), TRIM(units) &
  ,TRIM(stdnm),TRIM(metnm)
  call check(nf90_def_var(iunit, TRIM(varnm), &
  NF90_FLOAT, dimids, varid), "def_"//varnm)
  call check(NF90_DEF_VAR_CHUNKING(iunit, varid, NF90_CHUNKED, chksz))
  call check(NF90_DEF_VAR_DEFLATE(iunit, varid, 1,1,1))
  call check(nf90_put_att(iunit,varid, "units", TRIM(units)))
  if (LEN_TRIM(stdnm) > 0) &
  call check(nf90_put_att(iunit,varid,"standard_name", TRIM(stdnm)))
!       if (LEN_TRIM(metnm).gt.0)
!     +    call check(nf_put_att_text(iunit,varid,"metno_name",
!     +    LEN_TRIM(metnm), TRIM(metnm)))

  call check(nf90_put_att(iunit,varid,"coordinates", "x y"))
  call check(nf90_put_att(iunit,varid,"grid_mapping", "projection"))

end subroutine nc_declare_4d

subroutine nc_set_vtrans(iunit, kdimid,k_varid,ap_varid,b_varid)
  USE netcdf
  implicit none
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
!       call check(nf_put_var_real(iunit, k_varid, vlevel))

  call check(nf90_def_var(iunit, "ap", &
  NF90_FLOAT, kdimid, ap_varid), "def_ap")
  call check(nf90_put_att(iunit,ap_varid, "units", TRIM("hPa")))
!       call check(nf_put_var_real(iunit, ap_varid, alevel))

  call check(nf90_def_var(iunit, "b", &
  NF90_FLOAT, kdimid, b_varid), "def_b")
!       call check(nf_put_var_real(iunit, ap_varid, blevel))

  call check(nf90_def_var(iunit, "p0", &
  NF90_FLOAT, 0, p0_varid))
  call check(nf90_put_att(iunit,p0_varid, "units", &
  TRIM("hPa")))
  call check(nf90_put_var(iunit, p0_varid, 100))

end subroutine nc_set_vtrans

subroutine nc_set_projection(iunit, xdimid, ydimid, &
  igtype,nx,ny,gparam,garea, xm, ym, &
  simulation_start)
  USE netcdf
  implicit none
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

  if (igtype == 2) then
  !..geographic
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

  elseif (igtype == 3) then
  !..rot_geographic
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


  elseif (igtype == 1 .OR. igtype == 4) then
  !..polar_stereographic
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

  else if (igtype == 6) then
  !..lcc
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
      write(*,*) "error converting lcc to ll"
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
  else
    write(*,*) "unkown grid-type:", igtype
    error stop 1
  end if

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
  &                          2, llparam, ierror)
  if (ierror /= 0) then
    write(*,*) "error converting pos to latlon-projection"
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
  call check(nf90_put_att(iunit,mapx_varid, "units", &
  TRIM("1")))
  call check(nf90_put_att(iunit,mapx_varid, "grid_mapping", &
  TRIM("projection")))
  call check(nf90_put_att(iunit,mapx_varid, "coordinates", &
  TRIM("longitude latitude")))

  call check(nf90_put_var(iunit, mapx_varid, xm))

  call check(nf90_def_var(iunit, "map_factor_y", &
  NF90_FLOAT, dimids, mapy_varid))
  call check(nf90_put_att(iunit,mapy_varid, "units", &
  TRIM("1")))
  call check(nf90_put_att(iunit,mapy_varid, "grid_mapping", &
  TRIM("projection")))
  call check(nf90_put_att(iunit,mapy_varid, "coordinates", &
  TRIM("longitude latitude")))

  call check(nf90_put_var(iunit, mapy_varid, ym))


  call check(nf90_sync(iunit))

end subroutine nc_set_projection
end module fldout_ncML
