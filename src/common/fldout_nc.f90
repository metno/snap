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

subroutine fldout_nc(iwrite,iunit,filnam,itime,tf1,tf2,tnow,tstep, &
  istep,nsteph,ierror)
  USE snapfilML
  USE snapgrdML
  USE snapfldML
  USE snapparML
  USE snaptabML
  USE snapargosML
  USE snapdebugML
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

#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  USE particleML
  USE fileInfoML
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

        

  include 'netcdf.inc'

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
  ic_varid(mcomp), icml_varid(mcomp), acml_varid(mcomp), &
  bdd_varid(mcomp), bwd_varid(mcomp), accbdd_varid(mcomp), &
  accbwd_varid(mcomp)

  integer ::   iwrite,iunit,istep,nsteph,ierror
  integer ::   itime(5)
  real ::      tf1,tf2,tnow,tstep
  character*(*) filnam

  integer ::   igeofield,naverage,initacc,initargos,ihr
  real ::      geoparam(6)


  integer :: dimids2d(2),dimids3d(3),dimids4d(4), ipos(4), isize(4), &
  varid, chksz3d(3), chksz4d(4)
  integer, save :: x_dimid, y_dimid, k_dimid, t_dimid
  integer, save :: t_varid, k_varid, ap_varid, b_varid
  integer, save  :: iftime(5), ihrs, ihrs_pos


  integer ::          nptot1,nptot2
  double precision :: bqtot1,bqtot2
  double precision :: dblscale

  integer :: iyear,month,iday,ihour,minute,isecond,maxage
  integer :: i,j,k,m,mm,n,id03,id04,ivlvl,idextr,idry,iwet,loop,iparx
  integer :: itab,ko,lvla,lvlb,ipar,ierr,numfields,ios,iu,i1,i2
  real ::    rt1,rt2,scale,undef,average,averinv,cscale,dscale,hbl
  real ::    avg,hrstep,dh,splon,splat,total

  integer ::   itypef,ltimef,itimef(5),icodef,lspecf,loptf,ioptf
  integer ::   itimeargos(5)
  integer*2 :: ispecf(3)

  character(256) :: filename

  character(256) :: string
  integer :: lenstr


!..used in xyconvert (x,y -> longitude,latitude)
  data geoparam/1.,1.,1.,1.,0.,0./

  data initacc,initargos,igeofield,naverage/0,0,0,0/
  data numfields/0/

  data itimeargos/5*0/

#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('FLDOUT_NC',0,ZHOOK_HANDLE)
#endif

  idry=0
  iwet=0
  do m=1,ncomp
    mm=idefcomp(m)
    if(kdrydep(mm) == 1) idry=1
    if(kwetdep(mm) == 1) iwet=1
  end do

  ierror=0

!..initialization

  if(imodlevel == 1 .AND. (nxmc /= nx .OR. nymc /= ny)) imodlevel=0

  if(initacc == 0) then
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          depdry(i,j,m)=0.0d0
          depwet(i,j,m)=0.0d0
          accdry(i,j,m)=0.0d0
          accwet(i,j,m)=0.0d0
          concen(i,j,m)=0.0d0
          concacc(i,j,m)=0.0d0
        end do
      end do
    end do
    do j=1,ny
      do i=1,nx
        accprec(i,j)=0.0d0
      end do
    end do
    initacc=1
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
#if defined(DRHOOK)
  !     before the return statement
    IF (LHOOK) CALL DR_HOOK('FLDOUT_NC',1,ZHOOK_HANDLE)
#endif
    return
  end if

!..accumulation for average fields......................................

  if(naverage == 0) then
  
    do j=1,ny
      do i=1,nx
        avghbl(i,j)=0.0d0
        avgprec(i,j)=0.0d0
      end do
    end do
  
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          avgbq1(i,j,m)=0.0d0
          avgbq2(i,j,m)=0.0d0
        end do
      end do
    end do
  
  !..note: model level output on if nxmc=nx, nymc=ny and imodlevel=1
    if(imodlevel == 1) then
      do m=1,ncomp
        do k=1,nk-1
          do j=1,nymc
            do i=1,nxmc
              avgbq(i,j,k,m)=0.0d0
            end do
          end do
        end do
      end do
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

  do m=1,ncomp
    do j=1,ny
      do i=1,nx
        concen(i,j,m)=0.0d0
      end do
    end do
  end do

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
#if defined(DRHOOK)
  !     before the return statement
    IF (LHOOK) CALL DR_HOOK('FLDOUT_NC',1,ZHOOK_HANDLE)
#endif
    return
  end if


!..output...............................................................

  write(9,*) '*FLDOUT_NC*', numfields

  if(numfields > 0) then
  !..initialization of file
  !..remove an existing file and create a completely new one
    if (iunit /= 30) &
    call check(nf_close(iunit))
    numfields=0
    call rmfile(filnam,0,ierror)
    write(9,*) 'creating fldout_nc: ',filnam
    ihrs_pos = 0
    call check(nf_create(filnam, NF_NETCDF4, iunit), filnam)
    call check(nf_def_dim(iunit, "time", NF_UNLIMITED, t_dimid), &
    "t-dim")
    call check(nf_def_dim(iunit, "x", nx, x_dimid), "x-dim")
    call check(nf_def_dim(iunit, "y", ny, y_dimid), "y-dim")
    call check(nf_def_dim(iunit, "k", nk-1, k_dimid), "k-dim")

    if (nctitle /= "")  call check(nf_put_att_text(iunit, nf_global, &
    "title", len_trim(nctitle), trim(nctitle)))
    call check(nf_put_att_text(iunit, nf_global, &
    "summary", len_trim(ncsummary), trim(ncsummary)))

    call nc_set_projection(iunit, x_dimid, y_dimid, &
    igtype,nx,ny,gparam, garea, &
    simulation_start)
    if (imodlevel == 1) &
    call nc_set_vtrans(iunit, k_dimid, k_varid, ap_varid, b_varid)

    call check(nf_def_var(iunit, "time",NF_FLOAT,1,t_dimid,t_varid))
    write(string,'(A12,I4,A1,I0.2,A1,I0.2,A1,I0.2,A12)') &
    "hours since ",itime(1),"-",itime(2),"-",itime(3)," ", &
    itime(4),":00:00 +0000"
    call check(nf_put_att_text(iunit, t_varid, "units", &
    len_trim(string), trim(string)))
  
  !..store the files base-time
    do i=1,4
      iftime(i) = itime(i)
    end do
    iftime(5) = 0

    dimids2d(1) = x_dimid
    dimids3d(1) = x_dimid
    dimids4d(1) = x_dimid
    dimids2d(2) = y_dimid
    dimids3d(2) = y_dimid
    dimids4d(2) = y_dimid
    dimids3d(3) = t_dimid
    dimids4d(3) = k_dimid
    dimids4d(4) = t_dimid

    chksz3d(1) = nx
    chksz3d(2) = ny
    chksz3d(3) = 1
    chksz4d(1) = nx
    chksz4d(2) = ny
    chksz4d(3) = 1
    chksz4d(4) = 1

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
    call check(nf_enddef(iunit))
  end if

! set the runtime
  ihrs_pos = ihrs_pos + 1
  call hrdiff(0,0,iftime,itime,ihrs,ierror,ierror)
  call check(NF_PUT_VAR1_REAL(iunit,t_varid,ihrs_pos,FLOAT(ihrs)), &
  "set time")
  ipos(1) = 1
  ipos(2) = 1
  ipos(3) = ihrs_pos
  ipos(4) = ihrs_pos
  isize(1) = nx
  isize(2) = ny
  isize(3) = 1
  isize(4) = 1


!..open output felt (field) file
!      call mwfelt(1,filnam,iunit,1,nx*ny,field1,1.0,
!     +            ldata,idata,ierror)
!      if(ierror.ne.0) goto 920
!      call check(nf_open(filnam, NF_WRITE, iunit))

!..common field identification.............

  do i=1,20
    idata(i)=0
  end do
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

!..geographic coordinates etc.
  if(igeofield == 0) then
    do j=1,ny
      do i=1,nx
        field1(i,j)=float(i)
        field2(i,j)=float(j)
      end do
    end do
    id03=idata(3)
    id04=idata(4)
    idata( 3)=4
    idata( 4)=0
    idata( 5)=0
    idata( 7)=0
    idata( 8)=0
    idata(19)=0
    call xyconvert(nx*ny,field1,field2, &
    igtype,gparam,2,geoparam,ierror)
    if(idebug == 1) call ftest('lat',1,1,nx,ny,1,field2,0)
    idata( 6)=901
    idata(20)=-32767
  !      call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0,
  !    +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
    if(idebug == 1) call ftest('long',1,1,nx,ny,1,field1,0)
    idata( 6)=902
    idata(20)=-32767
  !       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
    if(idebug == 1) call ftest('area',1,1,nx,ny,1,garea,0)
    idata( 6)=903
    idata(20)=-32767
  !       call mwfelt(2,filnam,iunit,1,nx*ny,garea,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
    idata(3)=id03
    idata(4)=id04
    igeofield=1
  end if

  idata( 5)=2
  idata( 7)=1000
  idata( 8)=0
  idata(19)=0

  undef=+1.e+35

  average=float(naverage)
  averinv=1./float(naverage)
  naverage=0

!..fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
! c   cscale=10.**12

!..fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m3)
! c   dscale=10.**9

  cscale= 1.0
  dscale= 1.0

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
    idata( 6)=8
    idata(20)=-32767
    call check(NF_PUT_VARA_REAL(iunit, ps_varid,ipos,isize,field1), &
    "set_ps")
  !       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
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
    idata( 6)=17
    idata(19)=idextr
    idata(20)=-32767
  !       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, accum_prc_varid, ipos, isize, &
    field1), "set_accum_prc")
    if(ierror /= 0) goto 900
    idata(19)=0
  end if

!..mslp (if switched on)
  if(imslp == 1) then
    do j=1,ny
      do i=1,nx
        field1(i,j)=rt1*pmsl1(i,j)+rt2*pmsl2(i,j)
      end do
    end do
    if(idebug == 1) call ftest('mslp',1,1,nx,ny,1,field1,0)
  !        idata( 6)=58
  !        idata(20)=-32767
  !       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, mslp_varid, ipos, isize, &
    field1), "set_mslp")
  end if

!..instant height of boundary layer
  do j=1,ny
    do i=1,nx
      field4(i,j)=rt1*hbl1(i,j)+rt2*hbl2(i,j)
    end do
  end do
  if(idebug == 1) call ftest('hbl',1,1,nx,ny,1,field4,0)
  idata( 6)=500
  idata(20)=-32767
!      call mwfelt(2,filnam,iunit,1,nx*ny,field4,1.0,
!     +            ldata,idata,ierror)
  call check(NF_PUT_VARA_REAL(iunit, ihbl_varid, ipos, isize, &
  field1), "set_ihbl")
  if(ierror /= 0) goto 900

!..average height of boundary layer
  do j=1,ny
    do i=1,nx
      field1(i,j)=avghbl(i,j)*averinv
    end do
  end do
  if(idebug == 1) call ftest('avghbl',1,1,nx,ny,1,field1,0)
  idata( 6)=501
  idata(20)=-32767
!      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
!     +            ldata,idata,ierror)
  call check(NF_PUT_VARA_REAL(iunit, ahbl_varid, ipos, isize, &
  field1), "set_ahbl")
  if(ierror /= 0) goto 900

!..precipitation accummulated between field output // currently disable use 1 to write
  if(inprecip == -1) then
    do j=1,ny
      do i=1,nx
        field1(i,j)=avgprec(i,j)
      end do
    end do
    idextr=nint(average*tstep/3600.)
    if(idebug == 1) call ftest('prec',1,1,nx,ny,1,field1,0)
    idata( 6)=502
    idata(19)=idextr
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, prc_varid, ipos, isize, &
    field1), "set_prc")
    if(ierror /= 0) goto 900
    idata(19)=0
  end if

!..parameters for each component......................................

! c   idata( 5)=3
  idata( 5)=0
  idata( 8)=0
  idata(19)=0

  do m=1,ncomp
  
    mm=idefcomp(m)
  
  !..using the field level identifier to identify the component
    idata(7)=idcomp(mm)
  
  !..instant Bq in and above boundary layer
    do j=1,ny
      do i=1,nx
        field1(i,j)=0.
        field2(i,j)=0.
      end do
    end do
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
  
    write(9,*) ' component: ',compname(mm)
    write(9,*) '   Bq,particles in    abl: ',bqtot1,nptot1
    write(9,*) '   Bq,particles above abl: ',bqtot2,nptot2
    write(9,*) '   Bq,particles          : ',bqtot1+bqtot2, &
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
    idata( 6)=510
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, icbl_varid(m), ipos, isize, &
    field2), "set_icbl")
    if(ierror /= 0) goto 900
  
  !..average concentration in boundary layer
    do j=1,ny
      do i=1,nx
        field1(i,j)=cscale*avgbq1(i,j,m) &
        /(garea(i,j)*avghbl(i,j))
      end do
    end do
    if(idebug == 1) call ftest('avgconc',1,1,nx,ny,1,field1,0)
    idata( 6)=511
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, acbl_varid(m), ipos, isize, &
    field1), "set_acbl")
    if(ierror /= 0) goto 900
  
  !..dry deposition
    if(kdrydep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(depdry(i,j,m))/garea(i,j)
          accdry(i,j,m)=accdry(i,j,m)+depdry(i,j,m)
        end do
      end do
      if(idebug == 1) call ftest('dry',1,1,nx,ny,1,field1,0)
      idata( 6)=512
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, idd_varid(m), ipos, isize, &
      field1), "set_idd(m)")
      if(ierror /= 0) goto 900
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
      idata( 6)=513
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, iwd_varid(m), ipos, isize, &
      field1), "set_iwd(m)")
      if(ierror /= 0) goto 900
    end if
  
  !..accumulated dry deposition
    if(kdrydep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(accdry(i,j,m))/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('adry',1,1,nx,ny,1,field1,0)
      idata( 6)=514
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, accdd_varid(m),ipos,isize, &
      field1), "set_accdd(m)")

      if(ierror /= 0) goto 900
    end if
  
  !..accumulated wet deposition
    if(kwetdep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=dscale*sngl(accwet(i,j,m))/garea(i,j)
        end do
      end do
      if(idebug == 1) call ftest('awet',1,1,nx,ny,1,field1,0)
      idata( 6)=515
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, accwd_varid(m),ipos,isize, &
      field1), "set_accwd(m)")
      if(ierror /= 0) goto 900
    end if
  
  !..instant part of Bq in boundary layer
    if(idebug == 1) call ftest('pbq',1,1,nx,ny,1,field3,1)
    idata( 6)=516
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
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
    idata( 6)=517
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
  !..instant concentration on surface (not in felt-format)
    do j=1,ny
      do i=1,nx
        field3(i,j)= sngl(concen(i,j,m))
      end do
    end do
    if(idebug == 1) call ftest('concen',1,1,nx,ny,1,field3,1)
    call check(NF_PUT_VARA_REAL(iunit, ic_varid(m),ipos,isize, &
    field3), "set_ic(m)")

  !..accumulated/integrated concentration surface = dose
    do j=1,ny
      do i=1,nx
        field3(i,j)= sngl(concacc(i,j,m))
      end do
    end do
    if(idebug == 1) call ftest('concac',1,1,nx,ny,1,field3,1)
    idata( 6)=518
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, ac_varid(m),ipos,isize, &
    field3), "set_ac(m)")
    if(ierror /= 0) goto 900
  
  !.....end do m=1,ncomp
  end do


!..total parameters (sum of all components).............................

  if(ncomp > 1 .AND. itotcomp == 1) then
  
  !..using the field level identifier to identify component, 0=total
    idata(7)=0
  
  !..total instant Bq in and above boundary layer
    do j=1,ny
      do i=1,nx
        field1(i,j)=0.
        field2(i,j)=0.
      end do
    end do
  
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
    idata( 6)=510
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, icblt_varid,ipos,isize, &
    field2), "set_icblt(m)")
    if(ierror /= 0) goto 900
  
  !..total average concentration in boundary layer
    do j=1,ny
      do i=1,nx
        field1(i,j)=0.
      end do
    end do
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
    idata( 6)=511
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, acblt_varid,ipos,isize, &
    field1), "set_acblt")
    if(ierror /= 0) goto 900
  
  !..total dry deposition
    if(idry == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=0.
        end do
      end do
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
      idata( 6)=512
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, iddt_varid,ipos,isize, &
      field1), "set_iddt")

      if(ierror /= 0) goto 900
    end if
  
  !..total wet deposition
    if(iwet == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=0.
        end do
      end do
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
      idata( 6)=513
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, iwdt_varid,ipos,isize, &
      field1), "set_iwdt")
      if(ierror /= 0) goto 900
    end if
  
  !..total accumulated dry deposition
    if(idry == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=0.
        end do
      end do
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
      idata( 6)=514
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, accddt_varid,ipos,isize, &
      field1), "set_accddt")
      if(ierror /= 0) goto 900
    end if
  
  !..total accumulated wet deposition
    if(iwet == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=0.
        end do
      end do
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
      idata( 6)=515
      idata(20)=-32767
    !          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
    !     +                ldata,idata,ierror)
      call check(NF_PUT_VARA_REAL(iunit, accwdt_varid,ipos,isize, &
      field1), "set_accwdt")
      if(ierror /= 0) goto 900
    end if
  
  !..total instant part of Bq in boundary layer
    if(idebug == 1) call ftest('tpbq',1,1,nx,ny,1,field3,1)
    idata( 6)=516
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
  !..total average part of Bq in boundary layer
    scale=100.
    do j=1,ny
      do i=1,nx
        field1(i,j)=0.
        field2(i,j)=0.
      end do
    end do
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
    idata( 6)=517
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
  !     +              ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
  !..total accumulated/integrated concentration
    do j=1,ny
      do i=1,nx
        field3(i,j)=0.
      end do
    end do
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          field3(i,j)= field3(i,j) + sngl(concacc(i,j,m))
        end do
      end do
    end do
    if(idebug == 1) call ftest('concac',1,1,nx,ny,1,field3,1)
    idata( 6)=518
    idata(20)=-32767
  !        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
  !     +              ldata,idata,ierror)
    call check(NF_PUT_VARA_REAL(iunit, act_varid,ipos,isize, &
    field3), "set_act")

    if(ierror /= 0) goto 900
  
  !.....end if(ncomp.gt.1 .and. itotcomp.eq.1) then
  end if


!..BOMB fields..........................................................

  if (itprof == 2) then
  
  !..bomb parameters for each component.........
  
  ! c     idata( 5)=3
    idata( 5)=0
    idata( 8)=0
    idata(19)=0
  
    do m=1,ncomp
    
      mm= idefcomp(m)
    
      if(idebug == 1) write(9,*) ' component: ',compname(mm)
    
    !..using the field level identifier to identify the component
      idata(7)=idcomp(mm)
    
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
        idata( 6)=521
        idata(20)=-32767
      !            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +                  ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end if
    
    !..wet deposition
      if(kwetdep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*depwet(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('wet%',1,1,nx,ny,1,field1,0)
        idata( 6)=522
        idata(20)=-32767
      !            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +                  ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end if
    
    !..accumulated dry deposition
      if(kdrydep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*accdry(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('adry%',1,1,nx,ny,1,field1,0)
        idata( 6)=523
        idata(20)=-32767
      !            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +                  ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end if
    
    !..accumulated wet deposition
      if(kwetdep(mm) == 1) then
        do j=1,ny
          do i=1,nx
            field1(i,j)=sngl(dblscale*accwet(i,j,m))
          end do
        end do
        if(idebug == 1) call ftest('awet%',1,1,nx,ny,1,field1,0)
        idata( 6)=524
        idata(20)=-32767
      !            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +                  ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end if
    
    !.......end do m=1,ncomp
    end do
  
  end if


!..model level fields...................................................

  if(imodlevel /= 1) goto 800

! write k, ap, b - will be overwritten several times, but not data/timecritical
  call check(nf_put_var_real(iunit, k_varid, vlevel(2)), "set_k")
  call check(nf_put_var_real(iunit, ap_varid, alevel(2)), "set_ap")
  call check(nf_put_var_real(iunit, b_varid, blevel(2)), "set_b")


!..concentration in each layer
!..(height only computed at time of output)

  idata( 5)=ivcoor

!..loop for 1=average and 2=instant concentration
!..(now computing average first, then using the same arrays for instant)

  do loop=1,2
  
    if(loop == 1) then
    
      avg=average
      iparx=570
    
    else
    
      avg=1.
      iparx=540
      total=0.
      maxage=0
    
      do m=1,ncomp
        do k=1,nk-1
          do j=1,nymc
            do i=1,nxmc
              avgbq(i,j,k,m)=0.0d0
            end do
          end do
        end do
      end do
    
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
      ! use parameter z (1)
        idata( 6)=1
        idata( 7)=ko
        idata( 8)=lvla
        idata(19)=lvlb
        idata(20)=-32767
      !           call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +               ldata,idata,ierror)
        if(ierror /= 0) goto 900
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
        ipar=iparx+idcomp(m)
        idata( 6)=ipar
        idata( 7)=ko
        idata( 8)=lvla
        idata(19)=lvlb
        idata(20)=-32767
      ! don't write average currently, only instant (loop = 2)
      !        if (loop .eq. 1) then
      !           ipos(3) = k
      !           call check(NF_PUT_VARA_REAL(iunit, acml_varid(m),ipos,isize,
      !     +            field1))
      ! reset ipos(3) for 3d fields to time-pos (=ipos(4))
      !           ipos(3) = ipos(4)
      !     +       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +                  ldata,idata,ierror)
      
      !        end if
        if (loop == 2) then
          ipos(3) = k
          call check(NF_PUT_VARA_REAL(iunit, icml_varid(m),ipos, &
          isize,field1), "icml(m)")
        ! reset ipos(3) for 3d fields to time-pos (=ipos(4))
          ipos(3) = ipos(4)
          if(ierror /= 0) goto 900
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
        ipar=iparx+0
        idata( 6)=ipar
        idata( 7)=ko
        idata( 8)=lvla
        idata(19)=lvlb
        idata(20)=-32767
      !            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
      !     +                  ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end do
    end if
  
  !.....end do loop=1,2
  end do

  800 ierror=0

  do m=1,ncomp
    mm=idefcomp(m)
    if(kdrydep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          depdry(i,j,m)=0.0d0
        end do
      end do
    end if
    if(kwetdep(mm) == 1) then
      do j=1,ny
        do i=1,nx
          depwet(i,j,m)=0.0d0
        end do
      end do
    end if
  end do

!..close output felt (field) file
!      call mwfelt(13,filnam,iunit,1,nx*ny,field1,1.0,
!     +            ldata,idata,ierror)
  call check(nf_sync(iunit))
#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('FLDOUT_NC',1,ZHOOK_HANDLE)
#endif
  return

  900 ierror=1
!..close output felt (field) file
!      call mwfelt(13,filnam,iunit,1,nx*ny,field1,1.0,
!     +            ldata,idata,ierr)
  920 write(9,*) '*FLDOUT_NC*  Terminates due to write error.'

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('FLDOUT_NC',1,ZHOOK_HANDLE)
#endif
  return
end subroutine fldout_nc


subroutine nc_declare_3d(iunit, dimids, varid, &
  chksz, varnm, &
  units, stdnm, metnm)
  implicit none
  include 'netcdf.inc'
  INTEGER, INTENT(OUT)   :: varid
  INTEGER, INTENT(IN)    :: iunit, dimids(3), chksz(3)
  CHARACTER(LEN=*), INTENT(IN) :: varnm, stdnm, metnm, units

  write(9,*) "declaring ", iunit, TRIM(varnm), TRIM(units) &
  ,TRIM(stdnm),TRIM(metnm)
  call check(nf_def_var(iunit, TRIM(varnm), &
  NF_FLOAT, 3, dimids, varid), "def_"//varnm)
!       call check(NF_DEF_VAR_CHUNKING(iunit, varid, NF_CHUNKED, chksz))
  call check(NF_DEF_VAR_DEFLATE(iunit, varid, 1,1,1))
  call check(nf_put_att_text(iunit,varid, "units", &
  LEN_TRIM(units), TRIM(units)))
  if (LEN_TRIM(stdnm) > 0) &
  call check(nf_put_att_text(iunit,varid,"standard_name", &
  LEN_TRIM(stdnm), TRIM(stdnm)))
!       if (LEN_TRIM(metnm).gt.0)
!     +    call check(nf_put_att_text(iunit,varid,"metno_name",
!     +    LEN_TRIM(metnm), TRIM(metnm)))

  call check(nf_put_att_text(iunit,varid,"coordinates", &
  LEN_TRIM("longitude latitude"), "longitude latitude"))
  call check(nf_put_att_text(iunit,varid,"grid_mapping", &
  LEN_TRIM("projection"), "projection"))

end subroutine nc_declare_3d

subroutine nc_declare_4d(iunit, dimids, varid, &
  chksz, varnm, &
  units, stdnm, metnm)
  implicit none
  include 'netcdf.inc'
  INTEGER, INTENT(OUT)   :: varid
  INTEGER, INTENT(IN)    :: iunit, dimids(4), chksz(4)
  CHARACTER(LEN=*), INTENT(IN) :: varnm, stdnm, metnm, units


  write(9,*) "declaring ", iunit, TRIM(varnm), TRIM(units) &
  ,TRIM(stdnm),TRIM(metnm)
  call check(nf_def_var(iunit, TRIM(varnm), &
  NF_FLOAT, 4, dimids, varid), "def_"//varnm)
  call check(NF_DEF_VAR_CHUNKING(iunit, varid, NF_CHUNKED, chksz))
  call check(NF_DEF_VAR_DEFLATE(iunit, varid, 1,1,1))
  call check(nf_put_att_text(iunit,varid, "units", &
  LEN_TRIM(units), TRIM(units)))
  if (LEN_TRIM(stdnm) > 0) &
  call check(nf_put_att_text(iunit,varid,"standard_name", &
  LEN_TRIM(stdnm), TRIM(stdnm)))
!       if (LEN_TRIM(metnm).gt.0)
!     +    call check(nf_put_att_text(iunit,varid,"metno_name",
!     +    LEN_TRIM(metnm), TRIM(metnm)))

  call check(nf_put_att_text(iunit,varid,"coordinates", &
  LEN_TRIM("x y"), "x y"))
  call check(nf_put_att_text(iunit,varid,"grid_mapping", &
  LEN_TRIM("projection"), "projection"))

end subroutine nc_declare_4d

subroutine nc_set_vtrans(iunit, kdimid,k_varid,ap_varid,b_varid)
  implicit none
  include 'netcdf.inc'
  INTEGER, INTENT(IN) :: iunit, kdimid
  INTEGER, INTENT(OUT) :: k_varid, ap_varid, b_varid
  INTEGER ::p0_varid

  call check(nf_def_var(iunit, "k", &
  NF_FLOAT, 1, kdimid, k_varid), "def_k")
  call check(nf_put_att_text(iunit,k_varid, "standard_name", &
  LEN_TRIM("atmosphere_hybrid_sigma_pressure_coordinate"), &
  TRIM("atmosphere_hybrid_sigma_pressure_coordinate")))
  call check(nf_put_att_text(iunit,k_varid, "formula", &
  LEN_TRIM("p(n,k,j,i) = ap(k) + b(k)*ps(n,j,i)"), &
  TRIM("p(n,k,j,i) = ap(k) + b(k)*ps(n,j,i)")))
  call check(nf_put_att_text(iunit,k_varid, "formula_terms", &
  LEN_TRIM("ap: ap b: b ps: surface_air_pressure p0: p0"), &
  TRIM("ap: ap b: b ps: surface_air_pressure p0: p0")))
  call check(nf_put_att_text(iunit,k_varid, "positive", &
  LEN_TRIM("down"), &
  TRIM("down")))
!       call check(nf_put_var_real(iunit, k_varid, vlevel))

  call check(nf_def_var(iunit, "ap", &
  NF_FLOAT, 1, kdimid, ap_varid), "def_ap")
  call check(nf_put_att_text(iunit,ap_varid, "units", &
  LEN_TRIM("hPa"), TRIM("hPa")))
!       call check(nf_put_var_real(iunit, ap_varid, alevel))

  call check(nf_def_var(iunit, "b", &
  NF_FLOAT, 1, kdimid, b_varid), "def_b")
!       call check(nf_put_var_real(iunit, ap_varid, blevel))

  call check(nf_def_var(iunit, "p0", &
  NF_FLOAT, 0, 0, p0_varid))
  call check(nf_put_att_text(iunit,p0_varid, "units", &
  LEN_TRIM("hPa"), TRIM("hPa")))
  call check(nf_put_var_real(iunit, p0_varid, 100))

end subroutine nc_set_vtrans

subroutine nc_set_projection(iunit, xdimid, ydimid, &
  igtype,nx,ny,gparam,garea, &
  simulation_start)
  implicit none
  include 'netcdf.inc'
  INTEGER, INTENT(IN) :: iunit, xdimid, ydimid, igtype, nx, ny
  REAL(KIND=4), INTENT(IN):: gparam(8)
  REAL(kind=4), INTENT(IN), DIMENSION(nx,ny) :: garea
  CHARACTER(LEN=19), INTENT(IN)  :: simulation_start


  INTEGER :: i, j, ierror, x_varid, y_varid, proj_varid, &
  lon_varid, lat_varid, carea_varid, dimids(2)
  REAL(KIND=4) :: xvals(nx), yvals(ny), lon(nx,ny), lat(nx,ny), &
  val, pi, deg2rad, incr, llparam(6), gparam2(6)

  DATA llparam /1., 1., 1., 1., 0., 0./
  pi = 4.D0*DATAN(1.D0)
  deg2rad = pi/180.

  call check(nf_def_var(iunit, "x", &
  NF_FLOAT, 1, xdimid, x_varid))
  call check(nf_def_var(iunit, "y", &
  NF_FLOAT, 1, ydimid, y_varid))
  dimids(1)=xdimid
  dimids(2)=ydimid

  call check(nf_def_var(iunit, "projection", &
  NF_SHORT, 0, 0, proj_varid))

  call check(nf_put_att_text(iunit,NF_GLOBAL, "Conventions", &
  LEN_TRIM("CF-1.0"), "CF-1.0"))

! a reference-time, same as in WRF
  call check(nf_put_att_text(iunit, NF_GLOBAL, &
  "SIMULATION_START_DATE", LEN_TRIM("0000-00-00_00:00:00"), &
  simulation_start))

  if (igtype == 2) then
  !..geographic
    call check(nf_put_att_text(iunit,x_varid, "units", &
    LEN_TRIM("degrees_east"), TRIM("degrees_east")))
    call check(nf_put_att_text(iunit,y_varid, "units", &
    LEN_TRIM("degrees_north"), TRIM("degrees_north")))
    call check(nf_put_att_text(iunit,proj_varid, &
    "grid_mapping_name", LEN_TRIM("latitude_longitude"), &
    TRIM("latitude_longitude")))
    do i=1,nx
      xvals(i) = gparam(1) + (i-1)*gparam(3)
    end do
    do i=1,ny
      yvals(i) = gparam(2) + (i-1)*gparam(4)
    end do

  elseif (igtype == 3) then
  !..rot_geographic
    call check(nf_put_att_text(iunit,x_varid, "units", &
    LEN_TRIM("degrees"), TRIM("degrees")))
    call check(nf_put_att_text(iunit,y_varid, "units", &
    LEN_TRIM("degrees"), TRIM("degrees")))
    call check(nf_put_att_text(iunit,x_varid, "standard_name", &
    LEN_TRIM("grid_longitude"), TRIM("grid_longitude")))
    call check(nf_put_att_text(iunit,y_varid, "standard_name", &
    LEN_TRIM("grid_latitude"), TRIM("grid_latitude")))
    call check(nf_put_att_text(iunit,proj_varid, &
    "grid_mapping_name", LEN_TRIM("rotated_latitude_longitude"), &
    TRIM("rotated_latitude_longitude")))
    val = 180+gparam(5)
    if (val > 360) val = val - 360
    call check(nf_put_att_real(iunit,proj_varid, &
    "grid_north_pole_longitude", NF_FLOAT, 1, val))
    call check(nf_put_att_real(iunit,proj_varid, &
    "grid_north_pole_latitude", NF_FLOAT, 1,90-gparam(6)))
    call check(nf_sync(iunit))

    do i=1,nx
      xvals(i) = gparam(1) + (i-1)*gparam(3)
    end do
    do i=1,ny
      yvals(i) = gparam(2) + (i-1)*gparam(4)
    end do


  elseif (igtype == 1 .OR. igtype == 4) then
  !..polar_stereographic
    call check(nf_put_att_text(iunit,x_varid, "units", &
    LEN_TRIM("m"), TRIM("m")))
    call check(nf_put_att_text(iunit,y_varid, "units", &
    LEN_TRIM("m"), TRIM("m")))
    call check(nf_put_att_text(iunit,x_varid, "standard_name", &
    LEN_TRIM("projection_x_coordinate"), &
    TRIM("projection_x_coordinate")))
    call check(nf_put_att_text(iunit,y_varid, "standard_name", &
    LEN_TRIM("projection_y_coordinate"), &
    TRIM("projection_y_coordinate")))
    call check(nf_put_att_text(iunit,proj_varid, &
    "grid_mapping_name", LEN_TRIM("polar_stereographic"), &
    TRIM("polar_stereographic")))
    val = 180+gparam(5)
    if (val > 360) val = val - 360
    call check(nf_put_att_real(iunit,proj_varid, &
    "straight_vertical_longitude_from_pole",NF_FLOAT,1,gparam(4)))
    call check(nf_put_att_real(iunit,proj_varid, &
    "standard_parallel", NF_FLOAT, 1,gparam(5)))
    call check(nf_put_att_int(iunit,proj_varid, &
    "latitude_of_projection_origin", NF_INT, 1, 90))
  !..increment
    do i=1,nx
      xvals(i) = (i-gparam(1))*gparam(7)
    end do
    do i=1,ny
      yvals(i) = (i-gparam(2))*gparam(8)
    end do

  else if (igtype == 6) then
  !..lcc
    call check(nf_put_att_text(iunit,x_varid, "units", &
    LEN_TRIM("m"), TRIM("m")))
    call check(nf_put_att_text(iunit,y_varid, "units", &
    LEN_TRIM("m"), TRIM("m")))
    call check(nf_put_att_text(iunit,x_varid, "standard_name", &
    LEN_TRIM("projection_x_coordinate"), &
    TRIM("projection_x_coordinate")))
    call check(nf_put_att_text(iunit,y_varid, "standard_name", &
    LEN_TRIM("projection_y_coordinate"), &
    TRIM("projection_y_coordinate")))
    call check(nf_put_att_text(iunit,proj_varid, &
    "grid_mapping_name", LEN_TRIM("lambert_conformal_conic"), &
    TRIM("lambert_conformal_conic")))
    call check(nf_put_att_real(iunit,proj_varid, &
    "longitude_of_central_meridian", NF_FLOAT, 1, gparam(5)))
    call check(nf_put_att_real(iunit,proj_varid, &
    "standard_parallel", NF_FLOAT, 1,gparam(6)))
    call check(nf_put_att_real(iunit,proj_varid, &
    "latitude_of_projection_origin", NF_FLOAT, 1, gparam(6)))

    xvals(1) = gparam(1)
    yvals(1) = gparam(2)
  ! gparam(5) and gparam(6) are the reference-point of the projection
    gparam2(1) = gparam(5)
    gparam2(2) = gparam(6)
    do i=3,6
      gparam2(i) = gparam(i)
    end do
    ierror = 0
    call xyconvert(1, xvals(1), yvals(1), 2, llparam, &
    &                                          6, gparam2, ierror)
    if (ierror /= 0) then
      write(*,*) "error converting lcc to ll"
      call exit(1)
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
    call exit(1)
  end if

  call check(nf_put_var_real(iunit, x_varid, xvals))
  call check(nf_put_var_real(iunit, y_varid, yvals))
  call check(nf_sync(iunit))

  call check(nf_def_var(iunit, "longitude", &
  NF_FLOAT, 2, dimids, lon_varid))
  call check(NF_DEF_VAR_DEFLATE(iunit, lon_varid, 1,1,1))
  call check(nf_sync(iunit))
  call check(nf_def_var(iunit, "latitude", &
  NF_FLOAT, 2, dimids, lat_varid))
  call check(nf_sync(iunit))
  call check(nf_put_att_text(iunit,lon_varid, "units", &
  LEN_TRIM("degrees_east"), TRIM("degrees_east")))
  call check(nf_put_att_text(iunit,lat_varid, "units", &
  LEN_TRIM("degrees_north"), TRIM("degrees_north")))

  call check(nf_sync(iunit))

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
    call exit(1)
  end if
  call check(nf_put_var_real(iunit, lon_varid, lon))
  call check(nf_put_var_real(iunit, lat_varid, lat))

!.... create cell_area
  call check(nf_def_var(iunit, "cell_area", &
  NF_FLOAT, 2, dimids, carea_varid))
  call check(nf_put_att_text(iunit,carea_varid, "units", &
  LEN_TRIM("m2"), TRIM("m2")))
  call check(nf_put_att_text(iunit,carea_varid, "grid_mapping", &
  LEN_TRIM("projection"), TRIM("projection")))
  call check(nf_put_att_text(iunit,carea_varid, "coordinates", &
  LEN_TRIM("longitude latitude"), TRIM("longitude latitude")))

  call check(nf_put_var_real(iunit, carea_varid, garea))


  call check(nf_sync(iunit))

end subroutine nc_set_projection
