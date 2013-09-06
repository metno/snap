      subroutine fldout(iwrite,iunit,filnam,itime,tf1,tf2,tnow,tstep,
     *                  istep,nsteph,ierror)
c
c  Purpose:  Accumulation for average fields (iwrite=0,1).
c            Make and write output fields (iwrite=1).
c	     Initialization of output accumulation arrays (iwrite=-1).
c            Fields written to a sequential 'model output' file,
c            not opened here.
c
c---------------------------------------------------------------------
c  Field parameter numbers used here:
c     *   8 - surface pressure (if model level output) (hPa)
c     *  17 - precipitation accummulated from start of run (mm)
c     *  58 - mean sea level pressure, mslp (if switched on) (hPa)
c     * 500 - instant height of boundary layer (m)
c     * 501 - average height of boundary layer (m)
c     * 502 - precipitation accummulated between field output (mm)
c	      (better scaling than param. 17 for long runs)
c     * 510 - instant concentration in boundary layer (Bq/m3)
c     * 511 - average concentration in boundary layer (Bq/m3)
c     * 512 - dry deposition (for one time interval)  (Bq/m2)
c     * 513 - wet deposition (for one time interval)  (Bq/m2)
c     * 514 - dry deposition (accumulated from start) (Bq/m2)
c     * 515 - wet deposition (accumulated from start) (Bq/m2)
c     * 516 - instant part of Bq in the boundary layer  (%)
c     * 517 - average part of Bq in the boundary layer  (%)
c     * 518 - accumulated concentration in the lower layer (Bq*hr/m3)
c     * 519 - instant     concentration in the lower layer (Bq/m3)
c     * 521 - BOMB dry deposition (for one time interval),  % of release
c     * 522 - BOMB wet deposition (for one time interval),  % of release
c     * 523 - BOMB dry deposition (accumulated from start), % of release
c     * 524 - BOMB wet deposition (accumulated from start), % of release
c       540-569 - instant concentration in each layer (Bq/m3)
c       570-599 - average concentration in each layer (Bq/m3)
c     * 901 - geographic latitude  (degrees)
c     * 902 - geographic longitude (degrees)
c     * 903 - grid square area (m2)
c     ^------- current output
c---------------------------------------------------------------------
c  Notes:
c    -  data type (field ident. no. 3) is set to:
c              3 if forecast length (itime(5)) is 0 (valid time, +0)
c              2 if forecast length (itime(5)) is greater than 0
c              4 if geographic latitude, longitude, grid square area
c    -  for parameter 510-517 and 521-524 the component is identified
c	in field ident. no. 7 (usually 'level').
c    -  for parameter 540-569 : 540=total, 540+idcomp for each type
c       for parameter 570-599 : 570=total, 570+idcomp for each type
c	(the idcomp maximum is then 29, i.e. max 29 components)
c    -  parameters 500 - 509 for fields not dependant on component
c	parameters 510 - 539 for fields dependant on component
c	(level = idcomp to identify components,  0 = total)
c    -  possible use of grid square area, garea  (param. 903):
c	    Bq in boundary layer
c		= concentration/(garea*hbl)
c	    Bq above boundary layer
c		= (1.0-part_of_bq_in_bl)*concentration/(garea*hbl)
c---------------------------------------------------------------------
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
      include 'snaptab.inc'
      include 'snapargos.inc'
      include 'snapdebug.inc'
c
      integer   iwrite,iunit,istep,nsteph,ierror
      integer   itime(5)
      real      tf1,tf2,tnow,tstep
      character*(*) filnam
c
      integer   igeofield,naverage,initacc,initargos
      real      geoparam(6)
c
      integer          nptot1,nptot2
      double precision bqtot1,bqtot2
      double precision dblscale
      double precision dblfield(nx,ny)
c
      integer iyear,month,iday,ihour,minute,isecond
      integer i,j,k,m,mm,n,id03,id04,ivlvl,idextr,idry,iwet,loop,iparx
      integer itab,ko,lvla,lvlb,ipar,ierr,numfields,ios,iu,i1,i2
      real    rt1,rt2,scale,undef,average,averinv,cscale,dscale,hbl
      real    avg,hrstep,dh,splon,splat
c
      integer   itypef,ltimef,itimef(5),icodef,lspecf,loptf,ioptf
      integer   itimeargos(5)
      integer*2 ispecf(3)
c
      character*256 filename
c
      integer lenstr
c
      real field1print(nx*ny),field2print(nx*ny)
c
c      equivalence (field1(1,1),field1print(1))
c      equivalence (field2(1,1),field2print(1))
c
c..used in xyconvert (x,y -> longitude,latitude)
      data geoparam/1.,1.,1.,1.,0.,0./
c
      data initacc,initargos,igeofield,naverage/0,0,0,0/
      data numfields/0/
c
      data itimeargos/5*0/
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('FLDOUT',0,ZHOOK_HANDLE)
#endif
c
      ierror=0
c
c..initialization
c
      if(imodlevel.eq.1 .and. (nxmc.ne.nx .or. nymc.ne.ny)) imodlevel=0
c
      if(initacc.eq.0) then
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
c
      if(iwrite.eq.-1) then
c..iwrite=-1: istep is no. of field outputs
       n=ncomp
       if(ncomp.gt.1 .and. itotcomp.eq.1) n=n+1
       numfields= 2+n*9
       if(itprof.eq.2) numfields= numfields + ncomp*4
       if(inprecip .gt.0) numfields=numfields+2
       if(imslp    .gt.0) numfields=numfields+1
       if(imodlevel.gt.0) numfields=numfields+n*nk*2+1
        numfields= numfields*istep + 4
       if(numfields.gt.32767) numfields=32767
       do i=1,5
         itimeargos(i)=itime(i)
       end do
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FLDOUT',1,ZHOOK_HANDLE)
#endif
       return
      end if
c
      if(iargos.eq.1 .and. initargos.eq.0) then
c
c..open output files
       open(91,file=argosdepofile,
     +		access='sequential',form='formatted',status='unknown')
       open(92,file=argosconcfile,
     +		access='sequential',form='formatted',status='unknown')
       open(93,file=argosdosefile,
     +		access='sequential',form='formatted',status='unknown')
c
       read(argosdepofile,fmt='(i4,5i2)',iostat=ios)
     +				iyear,month,iday,ihour,minute,isecond
c..if not correct "Run time ID", use machine time
        if(ios.ne.0)
     +	   call daytim(iyear,month,iday,ihour,minute,isecond)
c
       iyear=mod(iyear,100)
c
       call vtime(itimeargos,ierror)
       itimeargos(1)=mod(itimeargos(1),100)
c
       do i=1,nargos
         call vtime(argostime(1,i),ierror)
         argostime(1,i)=mod(argostime(1,i),100)
       end do
c
c..from (x,y) to (longitude,latitude)
       do j=1,ny
         do i=1,nx
           field1(i,j)=float(i)
           field2(i,j)=float(j)
         end do
       end do
c
       call xyconvert(nx*ny,field1,field2,igtype,gparam,
     +		       2,geoparam,ierror)
        if(ierror.ne.0) then
         write(6,*) 'XYCONVERT ERROR (ARGOS) !!!!!!!!!!!!!!!!!!'
         write(9,*) 'XYCONVERT ERROR (ARGOS) !!!!!!!!!!!!!!!!!!'
       end if
c
       if(igtype.eq.3) then
         splon=gparam(5)
         splat=gparam(6)-90.
       else
         splon=0.
         splat=0.
       end if
c
c..same information in all headers
c
       do iu=91,93
c
         write(iu,fmt='(a)',err=900) 'HEADER'
c..case or machine date/time
         write(iu,fmt='(4(i2.2,1x),i2.2)',err=900)
     +		    iyear,month,iday,ihour,minute
c..release/run start
         write(iu,fmt='(4(i2.2,1x),i2.2)',err=900)
     +				(itimeargos(i),i=1,5)
c
         write(iu,fmt='(2i5)',err=900) nx,ny
c
         write(iu,fmt='(i5,100(1x,5i2.2))',err=900)
     +			nargos,((argostime(i,j),i=1,5),j=1,nargos)
c
         write(iu,fmt='(a)',err=900) '1 centered'
         write(iu,fmt='(2f8.2)',err=900) splat,splon
         write(iu,fmt='(a)',err=900) 'SINGLE-LEVEL FIELDS'
c
         write(iu,fmt='(a)',err=900) 'longitude (decimal deg.)'
         write(iu,fmt='(1pe8.2e2)',err=900) 1.0
         do i1=1,nx*ny,10
           i2=min(i1+9,nx*ny)
c           write(iu,1001) (field1print(i),i=i1,i2)
           write(iu,1001) (field1(modulo(i,nx),int(i/nx)),i=i1,i2)
         end do
c
         write(iu,fmt='(a)',err=900) 'latitude (decimal deg.)'
         write(iu,fmt='(1pe8.2e2)',err=900) 1.0
         do i1=1,nx*ny,10
           i2=min(i1+9,nx*ny)
c           write(iu,1001) (field2print(i),i=i1,i2)
           write(iu,1001) (field2(modulo(i,nx),int(i/nx)),i=i1,i2)
         end do
c
 1001	  format(10(1pe14.6e2))
c
         write(iu,fmt='(a)',err=900) 'MULTI-LEVEL FIELDS'
c
       end do
c
       initargos=1
c
      end if
c
c..accumulation for average fields......................................
c
      if(naverage.eq.0) then
c
        do j=1,ny
          do i=1,nx
             avghbl(i,j)=0.0d0
            avgprec(i,j)=0.0d0
         end do
        end do
c
       do m=1,ncomp
          do j=1,ny
            do i=1,nx
              avgbq1(i,j,m)=0.0d0
              avgbq2(i,j,m)=0.0d0
           end do
          end do
        end do
c
c..note: model level output on if nxmc=nx, nymc=ny and imodlevel=1
       if(imodlevel.eq.1) then
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
c
      end if
c
      naverage=naverage+1
c
c..for time interpolation
      rt1=(tf2-tnow)/(tf2-tf1)
      rt2=(tnow-tf1)/(tf2-tf1)
      hrstep=1./float(nsteph)
c
c..height of boundary layer
      do j=1,ny
        do i=1,nx
          avghbl(i,j)=avghbl(i,j)+(rt1*hbl1(i,j)+rt2*hbl2(i,j))
        end do
      end do
c
c..precipitation (no time interpolation, but hourly time intervals)
      scale=tstep/3600.
      do j=1,ny
        do i=1,nx
          avgprec(i,j)=avgprec(i,j)+scale*precip(i,j,iprecip)
        end do
      end do
c
      do n=1,npart
        i=nint(pdata(1,n))
        j=nint(pdata(2,n))
ccc     ivlvl=pdata(3,n)*10000.
ccc     k=ivlevel(ivlvl)
       m=iruncomp(icomp(n))
        if(pdata(3,n).ge.pdata(4,n)) then
c..in boundary layer
          avgbq1(i,j,m)=avgbq1(i,j,m)+pdata(9,n)
        else
c..above boundary layer
          avgbq2(i,j,m)=avgbq2(i,j,m)+pdata(9,n)
        end if
      end do
c
c..accumulated/integrated concentration
c
      do m=1,ncomp
       do j=1,ny
         do i=1,nx
           concen(i,j,m)=0.0d0
         end do
       end do
      end do
c
      do n=1,npart
        ivlvl=pdata(3,n)*10000.
        k=ivlayer(ivlvl)
        if(k.eq.1) then
          i=nint(pdata(1,n))
          j=nint(pdata(2,n))
         m=iruncomp(icomp(n))
          concen(i,j,m)= concen(i,j,m)+dble(pdata(9,n))
       end if
      end do
c
      do m=1,ncomp
       do j=1,ny
         do i=1,nx
           if(concen(i,j,m).gt.0.0d0) then
             dh= rt1*hlayer1(i,j,1)+rt2*hlayer2(i,j,1)
             concen(i,j,m)= concen(i,j,m)/(dh*dgarea(i,j))
             concacc(i,j,m)= concacc(i,j,m) + concen(i,j,m)*hrstep
           end if
         end do
       end do
      end do
c
      if(imodlevel.eq.1) then
c
        do n=1,npart
          i=nint(pdata(1,n))
          j=nint(pdata(2,n))
          ivlvl=pdata(3,n)*10000.
          k=ivlayer(ivlvl)
         m=iruncomp(icomp(n))
c..in each sigma/eta (input model) layer
          avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(9,n)
       end do
c
      end if
c
      if(iwrite.eq.0) then
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FLDOUT',1,ZHOOK_HANDLE)
#endif
        return
      end if
c
      if(iargos.eq.1) then
        do i=1,5
         itimeargos(i)=itime(i)
       end do
       call vtime(itimeargos,ierror)
       itimeargos(1)=mod(itimeargos(1),100)
      end if
c
c..output...............................................................
c
      write(9,*) '*FLDOUT*'
c
      if(numfields.gt.0) then
c..remove an existing file
       call rmfile(filnam,0,ierror)
c..create DNMI felt file
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
       call crefelt(filnam,iunit,itypef,ltimef,itimef,
     +               icodef,lspecf,ispecf,loptf,ioptf,ierror)
       if(ierror.ne.0) write(6,*) 'fldout: crefelt ERROR'
       if(ierror.ne.0) then
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FLDOUT',1,ZHOOK_HANDLE)
#endif
         return
       end if
       numfields=0
c
       filename=filnam(1:lenstr(filnam,1))//'_level_names'
       open (90,file=filename,access='sequential',form='formatted')
       write(90,1090) 0,'Total'
       do m=1,ncomp
         mm=idefcomp(m)
         k=lenstr(compnamemc(mm),1)
         write(90,1090) idcomp(mm),compnamemc(mm)(1:k)
       end do
 1090   format(1x,i5,1x,'"',a,'"')
       close(90)
      end if
c
c..open output felt (field) file
      call mwfelt(1,filnam,iunit,1,nx*ny,field1,1.0,
     +            ldata,idata,ierror)
      if(ierror.ne.0) goto 920
c
c..common field identification.............
c
      do i=1,20
        idata(i)=0
      end do
      idata( 1)=iprodr
      idata( 2)=igridr
      idata( 3)=2
      if(itime(5).eq.0) idata(3)=3
      idata( 4)=itime(5)
c.... idata( 5)= .......... vertical coordinate
c.... idata( 6)= .......... parameter no.
c.... idata( 7)= .......... level or level no. or component id
      idata( 8)=0
c.... idata( 9)= .......... set by gridpar
c.... idata(10)= .......... set by gridpar
c.... idata(11)= .......... set by gridpar
      idata(12)=itime(1)
      idata(13)=itime(2)*100+itime(3)
      idata(14)=itime(4)*100
c.... idata(15)= .......... set by gridpar
c.... idata(16)= .......... set by gridpar
c.... idata(17)= .......... set by gridpar
c.... idata(18)= .......... set by gridpar
c.... idata(19)= .......... 0 or sigma*10000 value
c.... idata(20)= .......... field scaling (automatic the best possible)
c
c..put grid parameters into field identification
c..(into the first 20 words and possibly also after space for data)
      call gridpar(-1,ldata,idata,igtype,nx,ny,gparam,ierror)
c
c..geographic coordinates etc.
      if(igeofield.eq.0) then
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
       call xyconvert(nx*ny,field1,field2,
     +		       igtype,gparam,2,geoparam,ierror)
        if(idebug.eq.1) call ftest('lat',1,1,nx,ny,1,field2,0)
        idata( 6)=901
        idata(20)=-32767
       call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
        if(idebug.eq.1) call ftest('long',1,1,nx,ny,1,field1,0)
        idata( 6)=902
        idata(20)=-32767
       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
        if(idebug.eq.1) call ftest('area',1,1,nx,ny,1,garea,0)
        idata( 6)=903
        idata(20)=-32767
       call mwfelt(2,filnam,iunit,1,nx*ny,garea,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
        idata(3)=id03
        idata(4)=id04
        igeofield=1
      end if
c
      idata( 5)=2
      idata( 7)=1000
      idata( 8)=0
      idata(19)=0
c
      undef=+1.e+35
c
      average=float(naverage)
      averinv=1./float(naverage)
      naverage=0
c
c..fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
ccc   cscale=10.**12
c
c..fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m3)
ccc   dscale=10.**9
c
      cscale= 1.0
      dscale= 1.0
c
c..for linear interpolation in time
      rt1=(tf2-tnow)/(tf2-tf1)
      rt2=(tnow-tf1)/(tf2-tf1)
c
c..surface pressure (if model level output, for vertical crossections)
      if(imodlevel.eq.1) then
       do j=1,ny
         do i=1,nx
           field1(i,j)=rt1*ps1(i,j)+rt2*ps2(i,j)
         end do
       end do
        if(idebug.eq.1) call ftest('ps',1,1,nx,ny,1,field1,0)
        idata( 6)=8
        idata(20)=-32767
       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
      end if
c
c..total accumulated precipitation from start of run
      if(inprecip.eq.1) then
       do j=1,ny
         do i=1,nx
           accprec(i,j)=accprec(i,j)+avgprec(i,j)
           field1(i,j)=accprec(i,j)
         end do
       end do
       idextr=nint(float(istep)/float(nsteph))
        if(idebug.eq.1) call ftest('accprec',1,1,nx,ny,1,field1,0)
        idata( 6)=17
        idata(19)=idextr
        idata(20)=-32767
       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
        idata(19)=0
      end if
c
c..mslp (if switched on)
      if(imslp.eq.1) then
       do j=1,ny
         do i=1,nx
           field1(i,j)=rt1*pmsl1(i,j)+rt2*pmsl2(i,j)
         end do
       end do
        if(idebug.eq.1) call ftest('mslp',1,1,nx,ny,1,field1,0)
        idata( 6)=58
        idata(20)=-32767
       call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
      end if
c
c..instant height of boundary layer
      do j=1,ny
        do i=1,nx
          field4(i,j)=rt1*hbl1(i,j)+rt2*hbl2(i,j)
        end do
      end do
      if(idebug.eq.1) call ftest('hbl',1,1,nx,ny,1,field4,0)
      idata( 6)=500
      idata(20)=-32767
      call mwfelt(2,filnam,iunit,1,nx*ny,field4,1.0,
     +            ldata,idata,ierror)
      if(ierror.ne.0) goto 900
c
c..average height of boundary layer
      do j=1,ny
        do i=1,nx
          field1(i,j)=avghbl(i,j)*averinv
        end do
      end do
      if(idebug.eq.1) call ftest('avghbl',1,1,nx,ny,1,field1,0)
      idata( 6)=501
      idata(20)=-32767
      call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +            ldata,idata,ierror)
      if(ierror.ne.0) goto 900
c
c..precipitation accummulated between field output
      if(inprecip.eq.1) then
       do j=1,ny
         do i=1,nx
           field1(i,j)=avgprec(i,j)
         end do
       end do
       idextr=nint(average*tstep/3600.)
        if(idebug.eq.1) call ftest('prec',1,1,nx,ny,1,field1,0)
        idata( 6)=502
        idata(19)=idextr
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
        idata(19)=0
      end if
c
c..parameters for each component......................................
c
ccc   idata( 5)=3
      idata( 5)=0
      idata( 8)=0
      idata(19)=0
c
      do m=1,ncomp
c
       mm=idefcomp(m)
c
c..using the field level identifier to identify the component
        idata(7)=idcomp(mm)
c
c..instant Bq in and above boundary layer
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
c
        do n=1,npart
         if(icomp(n).eq.mm) then
            i=nint(pdata(1,n))
            j=nint(pdata(2,n))
            if(pdata(3,n).ge.pdata(4,n)) then
              field1(i,j)=field1(i,j)+pdata(9,n)
             bqtot1=bqtot1+dble(pdata(9,n))
             nptot1=nptot1+1
            else
              field2(i,j)=field2(i,j)+pdata(9,n)
             bqtot2=bqtot2+dble(pdata(9,n))
             nptot2=nptot2+1
            end if
          end if
        end do
c
       write(9,*) ' component: ',compname(mm)
       write(9,*) '   Bq,particles in    abl: ',bqtot1,nptot1
       write(9,*) '   Bq,particles above abl: ',bqtot2,nptot2
       write(9,*) '   Bq,particles          : ',bqtot1+bqtot2,
     +						 nptot1+nptot2
c
c..instant part of Bq in boundary layer
       scale=100.
       do j=1,ny
         do i=1,nx
           if(field1(i,j)+field2(i,j).gt.0.) then
             field3(i,j)=scale*field1(i,j)/(field1(i,j)+field2(i,j))
           else
             field3(i,j)=undef
           end if
         end do
       end do
c
c..instant concentration in boundary layer
        do j=1,ny
          do i=1,nx
ccc         hbl=rt1*hbl1(i,j)+rt2*hbl2(i,j)
            hbl=field4(i,j)
            field2(i,j)=cscale*field1(i,j)/(hbl*garea(i,j))
          end do
        end do
        if(idebug.eq.1) call ftest('conc',1,1,nx,ny,1,field2,0)
        idata( 6)=510
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..average concentration in boundary layer
        do j=1,ny
         do i=1,nx
            field1(i,j)=cscale*avgbq1(i,j,m)
     +			      /(garea(i,j)*avghbl(i,j))
         end do
        end do
        if(idebug.eq.1) call ftest('avgconc',1,1,nx,ny,1,field1,0)
        idata( 6)=511
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..dry deposition
        if(kdrydep(mm).eq.1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=dscale*sngl(depdry(i,j,m))/garea(i,j)
              accdry(i,j,m)=accdry(i,j,m)+depdry(i,j,m)
            end do
          end do
         if(idebug.eq.1) call ftest('dry',1,1,nx,ny,1,field1,0)
          idata( 6)=512
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..wet deposition
        if(kwetdep(mm).eq.1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=dscale*sngl(depwet(i,j,m))/garea(i,j)
              accwet(i,j,m)=accwet(i,j,m)+depwet(i,j,m)
            end do
          end do
          if(idebug.eq.1) call ftest('wet',1,1,nx,ny,1,field1,0)
          idata( 6)=513
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..accumulated dry deposition
        if(kdrydep(mm).eq.1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=dscale*sngl(accdry(i,j,m))/garea(i,j)
            end do
          end do
          if(idebug.eq.1) call ftest('adry',1,1,nx,ny,1,field1,0)
          idata( 6)=514
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..accumulated wet deposition
        if(kwetdep(mm).eq.1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=dscale*sngl(accwet(i,j,m))/garea(i,j)
            end do
          end do
          if(idebug.eq.1) call ftest('awet',1,1,nx,ny,1,field1,0)
          idata( 6)=515
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..instant part of Bq in boundary layer
        if(idebug.eq.1) call ftest('pbq',1,1,nx,ny,1,field3,1)
        idata( 6)=516
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..average part of Bq in boundary layer
       scale=100.
       do j=1,ny
         do i=1,nx
           if(avgbq1(i,j,m)+avgbq2(i,j,m).gt.0.) then
             field3(i,j)=scale*avgbq1(i,j,m)
     +			       /(avgbq1(i,j,m)+avgbq2(i,j,m))
           else
             field3(i,j)=undef
           end if
         end do
       end do
        if(idebug.eq.1) call ftest('apbq',1,1,nx,ny,1,field3,1)
        idata( 6)=517
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..accumulated/integrated concentration
       do j=1,ny
         do i=1,nx
           field3(i,j)= sngl(concacc(i,j,m))
         end do
       end do
        if(idebug.eq.1) call ftest('concac',1,1,nx,ny,1,field3,1)
        idata( 6)=518
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c.....end do m=1,ncomp
      end do
c
c..ARGOS output.........................................................
c
      if(iargos.eq.1) then
c
c..argos "depo" output
        do m=1,ncomp
          do j=1,ny
            do i=1,nx
              dblfield(i,j)=(accdry(i,j,m)+accwet(i,j,m))/dgarea(i,j)
            end do
          end do
         call argoswrite(91,'depo',idcomp(idefcomp(m)),
     +			  itimeargos,nx,ny,dblfield)
       end do
c
c..argos "conc" output
        do m=1,ncomp
         call argoswrite(92,'conc',idcomp(idefcomp(m)),
     +			  itimeargos,nx,ny,concen(1,1,m))
       end do
c
c..argos "dose" output
        do m=1,ncomp
         call argoswrite(93,'dose',idcomp(idefcomp(m)),
     +			  itimeargos,nx,ny,concacc(1,1,m))
       end do
c
      end if
c
c
c..total parameters (sum of all components).............................
c
      if(ncomp.gt.1 .and. itotcomp.eq.1) then
c
c..using the field level identifier to identify component, 0=total
        idata(7)=0
c
c..total instant Bq in and above boundary layer
        do j=1,ny
          do i=1,nx
            field1(i,j)=0.
            field2(i,j)=0.
          end do
        end do
c
        do n=1,npart
          i=nint(pdata(1,n))
          j=nint(pdata(2,n))
          if(pdata(3,n).ge.pdata(4,n)) then
            field1(i,j)=field1(i,j)+pdata(9,n)
          else
            field2(i,j)=field2(i,j)+pdata(9,n)
          end if
        end do
c
c..total instant part of Bq in boundary layer
       scale=100.
       do j=1,ny
         do i=1,nx
           if(field1(i,j)+field2(i,j).gt.0.) then
             field3(i,j)=scale*field1(i,j)/(field1(i,j)+field2(i,j))
           else
             field3(i,j)=undef
           end if
         end do
       end do
c
c..total instant concentration in boundary layer
        do j=1,ny
          do i=1,nx
ccc         hbl=rt1*hbl1(i,j)+rt2*hbl2(i,j)
            hbl=field4(i,j)
            field2(i,j)=cscale*field1(i,j)/(hbl*garea(i,j))
          end do
        end do
        if(idebug.eq.1) call ftest('tconc',1,1,nx,ny,1,field2,0)
        idata( 6)=510
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,1,nx*ny,field2,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..total average concentration in boundary layer
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
            field1(i,j)=cscale*field1(i,j)
     +			      /(garea(i,j)*avghbl(i,j))
         end do
        end do
        if(idebug.eq.1) call ftest('tavgconc',1,1,nx,ny,1,field1,0)
        idata( 6)=511
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
        idry=0
        iwet=0
       do m=1,ncomp
         mm=idefcomp(m)
         if(kdrydep(mm).eq.1) idry=1
         if(kwetdep(mm).eq.1) iwet=1
       end do
c
c..total dry deposition
       if(idry.eq.1) then
         do j=1,ny
           do i=1,nx
             field1(i,j)=0.
           end do
         end do
         do m=1,ncomp
           mm=idefcomp(m)
            if(kdrydep(mm).eq.1) then
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
          if(idebug.eq.1) call ftest('tdry',1,1,nx,ny,1,field1,0)
          idata( 6)=512
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..total wet deposition
        if(iwet.eq.1) then
         do j=1,ny
           do i=1,nx
             field1(i,j)=0.
           end do
         end do
         do m=1,ncomp
           mm=idefcomp(m)
            if(kwetdep(mm).eq.1) then
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
          if(idebug.eq.1) call ftest('twet',1,1,nx,ny,1,field1,0)
          idata( 6)=513
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..total accumulated dry deposition
       if(idry.eq.1) then
         do j=1,ny
           do i=1,nx
             field1(i,j)=0.
           end do
         end do
         do m=1,ncomp
           mm=idefcomp(m)
            if(kdrydep(mm).eq.1) then
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
          if(idebug.eq.1) call ftest('tadry',1,1,nx,ny,1,field1,0)
          idata( 6)=514
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..total accumulated wet deposition
        if(iwet.eq.1) then
         do j=1,ny
           do i=1,nx
             field1(i,j)=0.
           end do
         end do
         do m=1,ncomp
           mm=idefcomp(m)
            if(kwetdep(mm).eq.1) then
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
          if(idebug.eq.1) call ftest('tawet',1,1,nx,ny,1,field1,0)
          idata( 6)=515
          idata(20)=-32767
          call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                ldata,idata,ierror)
          if(ierror.ne.0) goto 900
        end if
c
c..total instant part of Bq in boundary layer
        if(idebug.eq.1) call ftest('tpbq',1,1,nx,ny,1,field3,1)
        idata( 6)=516
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..total average part of Bq in boundary layer
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
           if(field1(i,j)+field2(i,j).gt.0.) then
             field3(i,j)=scale*field1(i,j)
     +			       /(field1(i,j)+field2(i,j))
           else
             field3(i,j)=undef
           end if
         end do
       end do
        if(idebug.eq.1) call ftest('tapbq',1,1,nx,ny,1,field3,1)
        idata( 6)=517
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c..total accumulated/integrated concentration
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
        if(idebug.eq.1) call ftest('concac',1,1,nx,ny,1,field3,1)
        idata( 6)=518
        idata(20)=-32767
        call mwfelt(2,filnam,iunit,2,nx*ny,field3,1.0,
     +              ldata,idata,ierror)
        if(ierror.ne.0) goto 900
c
c.....end if(ncomp.gt.1 .and. itotcomp.eq.1) then
      end if
c
c
c..BOMB fields..........................................................
c
      if (itprof.eq.2) then
c
c..bomb parameters for each component.........
c
ccc     idata( 5)=3
        idata( 5)=0
        idata( 8)=0
        idata(19)=0
c
        do m=1,ncomp
c
         mm= idefcomp(m)
c
         if(idebug.eq.1) write(9,*) ' component: ',compname(mm)
c
c..using the field level identifier to identify the component
          idata(7)=idcomp(mm)
c
c..scale to % of total released Bq (in a single bomb)
         dblscale= 100.0d0/dble(totalbq(mm))
c
c..dry deposition
          if(kdrydep(mm).eq.1) then
            do j=1,ny
              do i=1,nx
                field1(i,j)=sngl(dblscale*depdry(i,j,m))
              end do
            end do
            if(idebug.eq.1) call ftest('dry%',1,1,nx,ny,1,field1,0)
            idata( 6)=521
            idata(20)=-32767
            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                  ldata,idata,ierror)
            if(ierror.ne.0) goto 900
          end if
c
c..wet deposition
          if(kwetdep(mm).eq.1) then
            do j=1,ny
              do i=1,nx
                field1(i,j)=sngl(dblscale*depwet(i,j,m))
              end do
            end do
            if(idebug.eq.1) call ftest('wet%',1,1,nx,ny,1,field1,0)
            idata( 6)=522
            idata(20)=-32767
            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                  ldata,idata,ierror)
            if(ierror.ne.0) goto 900
          end if
c
c..accumulated dry deposition
          if(kdrydep(mm).eq.1) then
            do j=1,ny
              do i=1,nx
                field1(i,j)=sngl(dblscale*accdry(i,j,m))
              end do
            end do
            if(idebug.eq.1) call ftest('adry%',1,1,nx,ny,1,field1,0)
            idata( 6)=523
            idata(20)=-32767
            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                  ldata,idata,ierror)
            if(ierror.ne.0) goto 900
          end if
c
c..accumulated wet deposition
          if(kwetdep(mm).eq.1) then
            do j=1,ny
              do i=1,nx
                field1(i,j)=sngl(dblscale*accwet(i,j,m))
              end do
            end do
            if(idebug.eq.1) call ftest('awet%',1,1,nx,ny,1,field1,0)
            idata( 6)=524
            idata(20)=-32767
            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                  ldata,idata,ierror)
            if(ierror.ne.0) goto 900
          end if
c
c.......end do m=1,ncomp
        end do
c
      end if
c
c
c..model level fields...................................................
c
      if(imodlevel.ne.1) goto 800
c
c..concentration in each layer
c..(height only computed at time of output)
c
      idata( 5)=ivcoor
c
c..loop for 1=average and 2=instant concentration
c..(now computing average first, then using the same arrays for instant)
c
      do loop=1,2
c
        if(loop.eq.1) then
c
         avg=average
         iparx=570
c
        else
c
         avg=1.
         iparx=540
c
         do m=1,ncomp
            do k=1,nk-1
              do j=1,nymc
                do i=1,nxmc
                  avgbq(i,j,k,m)=0.0d0
               end do
              end do
            end do
          end do
c
          do n=1,npart
            i=nint(pdata(1,n))
            j=nint(pdata(2,n))
            ivlvl=pdata(3,n)*10000.
            k=ivlayer(ivlvl)
           m=iruncomp(icomp(n))
c..in each sigma/eta (input model) layer
            avgbq(i,j,k,m)=avgbq(i,j,k,m)+pdata(9,n)
         end do

        end if
c
        do k=1,nk-1
         do j=1,ny
           do i=1,nx
              dh=rt1*hlayer1(i,j,k)+rt2*hlayer2(i,j,k)
             field4(i,j)=dh*garea(i,j)*avg
           end do
         end do
         do m=1,ncomp
           do j=1,ny
             do i=1,nx
       	avgbq(i,j,k,m)=avgbq(i,j,k,m)/field4(i,j)
             end do
           end do
         end do
        end do
c
c..average concentration in each layer for each type
       do m=1,ncomp
          do k=1,nk-1
            do j=1,ny
              do i=1,nx
                field1(i,j)=cscale*sngl(avgbq(i,j,k,m))
              end do
            end do
            if(idebug.eq.1) call ftest('avconcl',1,1,nx,ny,1,field1,0)
           ko=klevel(k+1)
           lvla=nint(alevel(k+1)*10.)
           lvlb=nint(blevel(k+1)*10000.)
           if(ivcoor.eq.2) lvla=0
           ipar=iparx+idcomp(m)
            idata( 6)=ipar
            idata( 7)=ko
            idata( 8)=lvla
            idata(19)=lvlb
            idata(20)=-32767
            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                  ldata,idata,ierror)
            if(ierror.ne.0) goto 900
          end do
       end do
c
c..total average concentration in each layer
       if(ncomp.gt.1 .and. itotcomp.eq.1) then
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
            if(idebug.eq.1) call ftest('tavconcl',1,1,nx,ny,1,field1,0)
           ko=klevel(k+1)
           lvla=nint(alevel(k+1)*10.)
           lvlb=nint(blevel(k+1)*10000.)
           if(ivcoor.eq.2) lvla=0
           ipar=iparx+0
            idata( 6)=ipar
            idata( 7)=ko
            idata( 8)=lvla
            idata(19)=lvlb
            idata(20)=-32767
            call mwfelt(2,filnam,iunit,1,nx*ny,field1,1.0,
     +                  ldata,idata,ierror)
            if(ierror.ne.0) goto 900
          end do
       end if
c
c.....end do loop=1,2
      end do
c
  800 ierror=0
c
      do m=1,ncomp
       mm=idefcomp(m)
        if(kdrydep(mm).eq.1) then
          do j=1,ny
            do i=1,nx
              depdry(i,j,m)=0.0d0
            end do
          end do
       end if
        if(kwetdep(mm).eq.1) then
          do j=1,ny
            do i=1,nx
              depwet(i,j,m)=0.0d0
            end do
          end do
        end if
      end do
c
c..close output felt (field) file
      call mwfelt(13,filnam,iunit,1,nx*ny,field1,1.0,
     +            ldata,idata,ierror)
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FLDOUT',1,ZHOOK_HANDLE)
#endif
      return
c
  900 ierror=1
c..close output felt (field) file
      call mwfelt(13,filnam,iunit,1,nx*ny,field1,1.0,
     +            ldata,idata,ierr)
  920 write(9,*) '*FLDOUT*  Terminates due to write error.'
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FLDOUT',1,ZHOOK_HANDLE)
#endif
      return
      end
