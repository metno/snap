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

module fldoutML
  implicit none
  private

  public fldout

  contains

subroutine fldout(iwrite,iunit,filnam,itime,tf1,tf2,tnow,tstep, &
  istep,nsteph,ierror)

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
  USE iso_fortran_env, only: int16, error_unit
  USE particleML
  USE snapfilML
  USE snapgrdML
  USE snapfldML
  USE snapparML
  USE snaptabML
  USE snapargosML
  USE snapdebug, only: iulog, idebug
  USE argoswriteML, only: argoswrite
  USE ftestML, only: ftest
  USE snapdimML, only: nx, ny, nk, nxmc, nymc, ldata
  USE milibML, only: xyconvert, gridpar, rmfile, vtime
  USE releaseML, only: npart
  USE drydep, only: kdrydep
  USE wetdep, only: kwetdep
  implicit none

  integer ::   iwrite,iunit,istep,nsteph,ierror
  integer ::   itime(5)
  real ::      tf1,tf2,tnow,tstep
  character*(*) filnam

  integer, save ::   igeofield=0,naverage=0,initacc=0,initargos=0
  real, save :: geoparam(6) = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0]

  integer ::          nptot1,nptot2
  double precision :: bqtot1,bqtot2
  double precision :: dblscale
  double precision :: dblfield(nx,ny)

  integer :: iyear,month,iday,ihour,minute,isecond
  integer :: i,j,k,m,mm,n,id03,id04,ivlvl,idextr,idry,iwet,loop,iparx
  integer :: itab,ko,lvla,lvlb,ipar,ierr,ios,iu,i1,i2
  integer, save :: numfields=0
  real ::    rt1,rt2,scale,undef,average,averinv,cscale,dscale,hbl
  real ::    avg,hrstep,dh,splon,splat

  integer ::   itypef,ltimef,itimef(5),icodef,lspecf,loptf,ioptf
  integer, save :: itimeargos(5) = [0, 0, 0, 0, 0]
  integer(int16) :: ispecf(3)

  character(len=256) :: filename

  real :: field1print(nx*ny),field2print(nx*ny)

!      equivalence (field1(1,1),field1print(1))
!      equivalence (field2(1,1),field2print(1))



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

  if(iargos == 1 .AND. initargos == 0) then
  
  !..open output files
    open(91,file=argosdepofile, &
    access='sequential',form='formatted',status='unknown')
    open(92,file=argosconcfile, &
    access='sequential',form='formatted',status='unknown')
    open(93,file=argosdosefile, &
    access='sequential',form='formatted',status='unknown')
  
    read(argosdepofile,fmt='(i4,5i2)',iostat=ios) &
    iyear,month,iday,ihour,minute,isecond
  !..if not correct "Run time ID", use machine time
    if(ios /= 0) &
    call daytim(iyear,month,iday,ihour,minute,isecond)
  
    iyear=mod(iyear,100)
  
    call vtime(itimeargos,ierror)
    itimeargos(1)=mod(itimeargos(1),100)
  
    do i=1,nargos
      call vtime(argostime(1,i),ierror)
      argostime(1,i)=mod(argostime(1,i),100)
    end do
  
  !..from (x,y) to (longitude,latitude)
    do j=1,ny
      do i=1,nx
        field1(i,j)=float(i)
        field2(i,j)=float(j)
      end do
    end do
  
    call xyconvert(nx*ny,field1,field2,igtype,gparam, &
    & 		       2,geoparam,ierror)
    if(ierror /= 0) then
      write(error_unit,*) 'XYCONVERT ERROR (ARGOS) !!!!!!!!!!!!!!!!!!'
      write(iulog,*) 'XYCONVERT ERROR (ARGOS) !!!!!!!!!!!!!!!!!!'
    end if
  
    if(igtype == 3) then
      splon=gparam(5)
      splat=gparam(6)-90.
    else
      splon=0.
      splat=0.
    end if
  
  !..same information in all headers
  
    do iu=91,93
    
      write(iu,fmt='(a)',err=900) 'HEADER'
    !..case or machine date/time
      write(iu,fmt='(4(i2.2,1x),i2.2)',err=900) &
      iyear,month,iday,ihour,minute
    !..release/run start
      write(iu,fmt='(4(i2.2,1x),i2.2)',err=900) &
      (itimeargos(i),i=1,5)
    
      write(iu,fmt='(2i5)',err=900) nx,ny
    
      write(iu,fmt='(i5,100(1x,5i2.2))',err=900) &
      nargos,((argostime(i,j),i=1,5),j=1,nargos)
    
      write(iu,fmt='(a)',err=900) '1 centered'
      write(iu,fmt='(2f8.2)',err=900) splat,splon
      write(iu,fmt='(a)',err=900) 'SINGLE-LEVEL FIELDS'
    
      write(iu,fmt='(a)',err=900) 'longitude (decimal deg.)'
      write(iu,fmt='(1pe8.2e2)',err=900) 1.0
      do i1=1,nx*ny,10
        i2=min(i1+9,nx*ny)
      !           write(iu,1001) (field1print(i),i=i1,i2)
        write(iu,1001) (field1(modulo(i-1,nx)+1,int((i-1)/nx)+1), &
        i=i1,i2)
      end do
    
      write(iu,fmt='(a)',err=900) 'latitude (decimal deg.)'
      write(iu,fmt='(1pe8.2e2)',err=900) 1.0
      do i1=1,nx*ny,10
        i2=min(i1+9,nx*ny)
      !           write(iu,1001) (field2print(i),i=i1,i2)
        write(iu,1001) (field2(modulo(i-1,nx)+1,int((i-1)/nx)+1), &
        i=i1,i2)
      end do
    
      1001 format(10(1pe14.6e2))
    
      write(iu,fmt='(a)',err=900) 'MULTI-LEVEL FIELDS'
    
    end do
  
    initargos=1
  
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
    m = def_comp(icomp(n))%to_running
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
      m = def_comp(icomp(n))%to_running
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
      m = def_comp(icomp(n))%to_running
    !..in each sigma/eta (input model) layer
      avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(n)%rad
    end do
  
  end if

  if(iwrite == 0) then
    return
  end if

  if(iargos == 1) then
    do i=1,5
      itimeargos(i)=itime(i)
    end do
    call vtime(itimeargos,ierror)
    itimeargos(1)=mod(itimeargos(1),100)
  end if

!..output...............................................................

  write(iulog,*) '*FLDOUT*'

  if(numfields > 0) then
  !..remove an existing file
    call rmfile(filnam,0,ierror)
  !..create DNMI felt file
    itypef=999
    ltimef=5
    itimef(1)=itime(1)
    itimef(2)=itime(2)
    itimef(3)=itime(3)
    itimef(4)=itime(4)
    itimef(5)=0
    icodef=0
    lspecf=3
    ispecf(1)=iprodr
    ispecf(2)=igridr
    ispecf(3)=numfields
    loptf=1
    ioptf=0
    call crefelt(filnam,iunit,itypef,ltimef,itimef, &
    icodef,lspecf,ispecf,loptf,ioptf,ierror)
    write(iulog,*) 'creating fldout: ',filnam
    if(ierror /= 0) write(error_unit,*) 'fldout: crefelt ERROR'
    if(ierror /= 0) then
      return
    end if
    numfields=0
  
    filename=trim(filnam)//'_level_names'
    open (90,file=filename,access='sequential',form='formatted')
    write(90,1090) 0,'Total'
    do m=1,ncomp
      mm = run_comp(m)%to_defined
      write(90,1090) idcomp(mm),trim(compnamemc(mm))
    end do
    1090 format(1x,i5,1x,'"',a,'"')
    close(90)
  end if

!..open output felt (field) file
  call mwfelt(1,filnam,iunit,1,nx*ny,field1,1.0, &
  ldata,idata,ierror)
  if(ierror /= 0) goto 920

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
    call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
    if(idebug == 1) call ftest('long',1,1,nx,ny,1,field1,0)
    idata( 6)=902
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
    if(idebug == 1) call ftest('area',1,1,nx,ny,1,garea,0)
    idata( 6)=903
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,1,nx*ny,garea,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
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
    idata( 6)=58
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
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
  call mwfelt(2,filnam,iunit,1,nx*ny,field4,1.0, &
  ldata,idata,ierror)
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
  call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
  ldata,idata,ierror)
  if(ierror /= 0) goto 900

!..precipitation accummulated between field output
  if(inprecip == 1) then
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
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
    idata(19)=0
  end if

!..parameters for each component......................................

! c   idata( 5)=3
  idata( 5)=0
  idata( 8)=0
  idata(19)=0

  do m=1,ncomp
  
    mm = run_comp(m)%to_defined
  
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
    idata( 6)=510
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
      if(ierror /= 0) goto 900
    end if
  
  !..instant part of Bq in boundary layer
    if(idebug == 1) call ftest('pbq',1,1,nx,ny,1,field3,1)
    idata( 6)=516
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
  !..accumulated/integrated concentration
    do j=1,ny
      do i=1,nx
        field3(i,j)= sngl(concacc(i,j,m))
      end do
    end do
    if(idebug == 1) call ftest('concac',1,1,nx,ny,1,field3,1)
    idata( 6)=518
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
  !.....end do m=1,ncomp
  end do

!..ARGOS output.........................................................

  if(iargos == 1) then
  
  !..argos "depo" output
    do m=1,ncomp
      do j=1,ny
        do i=1,nx
          dblfield(i,j)=(accdry(i,j,m)+accwet(i,j,m))/dgarea(i,j)
        end do
      end do
      call argoswrite(91,'depo',idcomp(run_comp(m)%to_defined), &
      itimeargos,nx,ny,dblfield)
    end do
  
  !..argos "conc" output
    do m=1,ncomp
      call argoswrite(92,'conc',idcomp(run_comp(m)%to_defined), &
      itimeargos,nx,ny,concen(:,:,m))
    end do
  
  !..argos "dose" output
    do m=1,ncomp
      call argoswrite(93,'dose',idcomp(run_comp(m)%to_defined), &
      itimeargos,nx,ny,concacc(:,:,m))
    end do
  
  end if


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
    call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
    ldata,idata,ierror)
    if(ierror /= 0) goto 900
  
    idry=0
    iwet=0
    do m=1,ncomp
      mm = run_comp(m)%to_defined
      if(kdrydep(mm) == 1) idry=1
      if(kwetdep(mm) == 1) iwet=1
    end do
  
  !..total dry deposition
    if(idry == 1) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=0.
        end do
      end do
      do m=1,ncomp
        mm = run_comp(m)%to_defined
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
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
        mm = run_comp(m)%to_defined
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
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
        mm = run_comp(m)%to_defined
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
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
        mm = run_comp(m)%to_defined
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
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
      ldata,idata,ierror)
      if(ierror /= 0) goto 900
    end if
  
  !..total instant part of Bq in boundary layer
    if(idebug == 1) call ftest('tpbq',1,1,nx,ny,1,field3,1)
    idata( 6)=516
    idata(20)=-32767
    call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0, &
    ldata,idata,ierror)
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
    call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0, &
    ldata,idata,ierror)
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
    
      mm = run_comp(m)%to_defined
    
      if(idebug == 1) write(iulog,*) ' component: ',compname(mm)
    
    !..using the field level identifier to identify the component
      idata(7)=idcomp(mm)
    
    !..scale to % of total released Bq (in a single bomb)
      dblscale= 100.0d0/dble(run_comp(m)%totalbq)
    
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
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
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
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
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
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
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
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end if
    
    !.......end do m=1,ncomp
    end do
  
  end if


!..model level fields...................................................

  if(imodlevel /= 1) goto 800

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
        m = def_comp(icomp(n))%to_running
      !..in each sigma/eta (input model) layer
        avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(n)%rad
      end do

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
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
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
        if (loop == 2) &
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
      
        if(ierror /= 0) goto 900
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
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0, &
        ldata,idata,ierror)
        if(ierror /= 0) goto 900
      end do
    end if
  
  !.....end do loop=1,2
  end do

  800 ierror=0

  do m=1,ncomp
    mm = run_comp(m)%to_defined
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
  call mwfelt(13,filnam,iunit,1,nx*ny,field1,1.0, &
  ldata,idata,ierror)
  return

  900 ierror=1
!..close output felt (field) file
  call mwfelt(13,filnam,iunit,1,nx*ny,field1,1.0, &
  ldata,idata,ierr)
  920 write(iulog,*) '*FLDOUT*  Terminates due to write error.'

  return
end subroutine fldout
end module fldoutML
