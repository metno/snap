c-----------------------------------------------------------------------
c snap.F
c
c SNAP - Severe Nuclear Accident Program
c
c-----------------------
c Options in snap.input:
c=======================================================================
c * comment
c GRAPHICS.ON
c GRAPHICS.OFF
c GRAPHICS.PRECIP.ON
c GRAPHICS.PRECIP.OFF
c GRAPHICS.DRY.DEP.ON
c GRAPHICS.DRY.DEP.OFF
c GRAPHICS.WET.DEP.ON
c GRAPHICS.WET.DEP.OFF
c GRAPHICS.TOTAL.DEP.ON
c GRAPHICS.TOTAL.DEP.OFF
c GRAPHICS.MSLP.ON
c GRAPHICS.MSLP.OFF
c GRAPHICS.COMPONENT= Aerosol
c GRAPHICS.COMPONENTS.TOTAL
c GRAPHICS.PARTICLES.KEEP.MASS
c GRAPHICS.PARTICLES.KEEP.POSITIONS
c GRAPHICS.PARTICLES.IN.BOUNDARY.LAYER.ON
c GRAPHICS.PARTICLES.IN.BOUNDARY.LAYER.OFF
c GRAPHICS.PARTICLES.ABOVE.BOUNDARY.LAYER.ON
c GRAPHICS.PARTICLES.ABOVE.BOUNDARY.LAYER.OFF
c GRAPHICS.FREEZE.OFF
c GRAPHICS.FREEZE.AT.START
c GRAPHICS.FREEZE.AT.START.AND.END
c GRAPHICS.FREEZE.AT.END
c GRAPHICS.FREEZE.AT.EVERY.TIMESTEP
c GRAPHICS.FREEZE.AT.TIME.OF.FIELD.OUTPUT
c GRAPHICS.BACKGROUND.BLACK
c GRAPHICS.BACKGROUND.WHITE
c GRAPHICS.AREA.PERCENT.X1.X2.Y1.Y2= 0.,100.,0.,100.
c GRAPHICS.SCREEN.PERCENT.X1.X2.Y1.Y2= 5.,95.,5.,95.
c GRAPHICS.LAND= /usr/local/gsetup/land4.dat
c GRAPHICS.LAND= /usr/local/gsetup/land5.dat
c SAVE.PNG.STEP= 1
c SAVE.XPM.STEP= 0
c SAVE.BMP.STEP= 0
c VIDEO.SAVE.ON
c VIDEO.SAVE.OFF
c POSITIONS.DECIMAL
c POSITIONS.DEGREE+MINUTE
c TIME.START=        1993,1,25,12
c TIME.RUN  =        5d
c TIME.RELEASE=      12h
c SET_RELEASE.POS=   P.1
c RANDOM.WALK.ON
c RANDOM.WALK.OFF
c BOUNDARY.LAYER.FULL.MIX.OFF
c BOUNDARY.LAYER.FULL.MIX.ON
c DRY.DEPOSITION.OLD .......................... (default)
c DRY.DEPOSITION.NEW
c WET.DEPOSITION.OLD .......................... (default)
c WET.DEPOSITION.NEW
c TIME.STEP= 900.
c TIME.RELEASE.PROFILE.CONSTANT
c TIME.RELEASE.PROFILE.BOMB
c TIME.RELEASE.PROFILE.LINEAR
c TIME.RELEASE.PROFILE.STEPS
c * RELEASE.SECOND=
c * RELEASE.MINUTE=
c * RELEASE.DAY=                    0,       1,       2, ....
c RELEASE.HOUR=                     0,      24,      48, ....
c RELEASE.RADIUS.M=             10000,   10000,   10000, ....
c RELEASE.UPPER.M=              10000,   10000,   10000, ....
c RELEASE.LOWER.M=               5000,    5000,    5000, ....
c RELEASE.MUSHROOM.STEM.RADIUS.M= 500,     500,     500, ....
c RELEASE.BQ/HOUR.COMP=   2.e+15,  1.e+15, 0.5e+15, ..., 'Aerosol.large'
c RELEASE.BQ/HOUR.COMP=   1.e+15,  1.e+15, 0.5e+15, ..., 'Aerosol.medium'
c RELEASE.BQ/HOUR.COMP=       0.,  1.e+15, 0.5e+15, ..., 'Aerosol.small'
c * RELEASE.BQ/SEC.COMP=
c * RELEASE.BQ/DAY.COMP=
c * RELEASE.BQ/STEP.COMP=
c MAX.PARTICLES.PER.RELEASE= 2000
c COMPONENT= Aerosol
c DRY.DEP.ON
c DRY.DEP.OFF
c WET.DEP.ON
c WET.DEP.OFF
c DRY.DEP.HEIGHT= 44.
c DRY.DEP.RATIO=  0.1
c WET.DEP.RATIO=  0.2
c RADIOACTIVE.DECAY.ON
c RADIOACTIVE.DECAY.OFF
c HALF.LIFETIME.MINUTES= 45.5
c HALF.LIFETIME.HOURS= ...
c HALF.LIFETIME.DAYS= ....
c HALF.LIFETIME.YEARS= ....
c * GRAVITY.OFF
c * GRAVITY.FIXED.M/S= 0.01
c * GRAVITY.FIXED.CM/S= 1.0
c RADIUS.MICROMETER= 50.
c DENSITY.G/CM3= 19.
c FIELD.IDENTIFICATION= 1
c TOTAL.COMPONENTS.OFF
c TOTAL.COMPONENTS.ON
c PRECIP(MM/H).PROBAB= 0.0,0.00, 0.5,0.31, 1.0,0.48, 1.5,0.60, 2.0,0.66
c PRECIP(MM/H).PROBAB= 3.3,0.72, 8.3,0.80, 15.,0.85, 25.,0.91
c REMOVE.RELATIVE.MASS.LIMIT= 0.02
c STEP.HOUR.INPUT.MIN=  6
c STEP.HOUR.INPUT.MAX= 12
c STEP.HOUR.OUTPUT.FIELDS=  3
c SYNOPTIC.OUTPUT
c ASYNOPTIC.OUTPUT
c MSLP.ON
c MSLP.OFF
c PRECIPITATION.ON
c PRECIPITATION.OFF
c MODEL.LEVEL.FIELDS.ON
c MODEL.LEVEL.FIELDS.OFF
c RELEASE.POS= 'P.1', 58.50, -4.00
c GRID.INPUT= 88,1814
c GRID.SIZE= 360,180
c GRID.RUN=   88,1814, 1,1,1
c * Norlam (sigma levels)
c DATA.SIGMA.LEVELS
c * Hirlam (eta levels)
c DATA.ETA.LEVELS
c * wind.surface = wind.10m (one 0 level first in list)
c LEVELS.INPUT= 14, 0,31,30,29,28,27,26,25,23,21,17,13,7,1
c * forecast.hour.min/max for following files (may change between files)
c FORECAST.HOUR.MIN= +6 ......................... (default is +6)
c FORECAST.HOUR.MAX= +32767 ..................... (default is +32767)
c FIELD.TYPE=felt|netcdf
c FIELD.INPUT= arklam.dat
c FIELD.INPUT= feltlam.dat
c FIELD_TIME.FORECAST
c FIELD_TIME.VALID
c FIELD.OUTPUT= snap.felt
c LOG.FILE=     snap.log
c DEBUG.OFF ..................................... (default)
c DEBUG.ON
c ENSEMBLE.PROJECT.OUTPUT.OFF ................... (default)
c ENSEMBLE.PROJECT.OUTPUT.ON
c ENSEMBLE.PROJECT.OUTPUT.FILE= ensemble.list
c ENSEMBLE.PROJECT.PARTICIPANT= 09
c ENSEMBLE.PROJECT.RANDOM.KEY=  RL52S3U
c ENSEMBLE.PROJECT.OUTPUT.STEP.HOUR= 3
c ARGOS.OUTPUT.OFF
c ARGOS.OUTPUT.ON
c ARGOS.OUTPUT.DEPOSITION.FILE=    runident_MLDP0_depo
c ARGOS.OUTPUT.CONCENTRATION.FILE= runident_MLDP0_conc
c ARGOS.OUTPUT.TOTALDOSE.FILE=     runident_MLDP0_dose
c ARGOS.OUTPUT.TIMESTEP.HOUR= 3
c END
c=======================================================================
c
c-----------------------------------------------------------------------
c DNMI library subroutines : rfelt
c                            rlunit
c                            chcase
c                            termchar
c                            getvar
c                            keywrd
c                            hrdiff
c                            vtime
c                            gridpar
c                            mapfield
c                            xyconvert
c                            daytim
c                            mwfelt
c
c-----------------------------------------------------------------------
c  DNMI/FoU  18.09.1992  Anstein Foss
c  DNMI/FoU  19.02.1993  Anstein Foss
c  DNMI/FoU  18.11.1993  Anstein Foss
c  DNMI/FoU  08.04.1994  Anstein Foss
c  DNMI/FoU  24.10.1994  Anstein Foss ... GL graphics
c  DNMI/FoU  26.10.1994  Anstein Foss ... rmpart(...)
c  DNMI/FoU  05.12.1994  Anstein Foss ... status file for normem etc.
c  DNMI/FoU  06.01.1995  Anstein Foss
c  DNMI/FoU  21.03.1995  Anstein Foss
c  DNMI/FoU  21.04.1995  Anstein Foss
c  DNMI/FoU  05.06.1995  Anstein Foss ... Norlam+Hirlam
c  DNMI/FoU  16.08.1995  Anstein Foss
c  DNMI/FoU  06.10.1995  Anstein Foss ... no receive positions
c  DNMI/FoU  24.10.1995  Anstein Foss ... aerosol+gas+noble.gas+...
c  DNMI/FoU  23.11.1995  Anstein Foss
c  DNMI/FoU  11.03.1996  Anstein Foss ... video.save
c  DNMI/FoU  13.09.1996  Anstein Foss ... ECMWF model level data
c  DNMI/FoU  22.11.1996  Anstein Foss ... mass in graphics +++
c  DNMI/FoU  06.02.2001  Anstein Foss ... Ensemble project output
c  DNMI/FoU  17.02.2001  Anstein Foss ... Qt/OpenGL graphics
c  DNMI/FoU  24.08.2001  Anstein Foss ... Radioactive decay
c  DNMI/FoU  29.10.2001  Anstein Foss ... Nuclear bomb, gravity +++
c  DNMI/FoU  10.12.2001  Anstein Foss ... NKS, 'singlecomponent' +++++
c  DNMI/FoU  01.12.2002  Anstein Foss ... Argos output for SSV
c  DNMI/FoU  04.12.2002  Anstein Foss ... (Argos) no mass, only Bq
c  DNMI/FoU  09.04.2003  Anstein Foss ... remove 10m level option (k10m)
c  DNMI/FoU  15.08.2003  Anstein Foss ... ARGOS output in the model grid
c  DNMI/FoU  23.11.2003  Anstein Foss ... BOMB version drydep2,wetdep2
c  DNMI/FoU  16.01.2004  Anstein Foss ... mushroom shape release
c  DNMI/FoU  04.11.2004  Anstein Foss ... bugfix (pselect called too late)
c  DNMI/FoU  08.02.2005  Anstein Foss ... gravity(m/s) in pdata(10,n)
c met.no/FoU 20.06.2006  Anstein Foss ... Ensemble update
c met.no/FoU 22.03.2013  Heiko Klein  ... Fix output-format for argos/nrpa
c-----------------------------------------------------------------------
c
c
      program bsnap
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif

c
c SNAP - Severe Nuclear Accident Program
c
c..BATCH input:
c	iaction=0 : initialize
c	iaction=1 : run one timestep
c	iaction=2 : finish
c..BATCH output:
c	iexit=0   : ok
c	iexit=1   : end of run
c	iexit=2   : some error, exit
c
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
      include 'snappos.inc'
      include 'snaptab.inc'
      include 'snapeps.inc'
      include 'snapdebug.inc'
      include 'snapargos.inc'
      include 'snapgrp.inc'
c
      integer   iaction,iexit,allocatestatus
c
      integer   itime1(5),itime2(5),itime(5),itimei(5),itimeo(5)
      integer   itimefi(5),itimefa(5),itimefo(5,2)
c
      real      geoparam(6)
c
      integer maxkey
      parameter (maxkey=10)
      integer   kwhere(5,maxkey)
c
      integer iargc,c2fiargc
      integer narg,iuinp,ios,iprhlp,nlines,nhrun,nhrel,irwalk,nhfout
      integer isynoptic,isvideo,m,nlevel,minhfc,maxhfc,ifltim
      integer ipostyp,lcinp,iend,ks,nkey,k,ierror,mkey
      integer ikey,k1,k2,kv1,kv2,nkv,i,kh,kd,ig,igd,igm,i1,i2,l,n,idmin
      integer iscale
      integer idrydep,iwetdep,idecay
      integer iulog,ntimefo,iunitf,iunitx,nh1,nh2
      integer ierr1,ierr2,nsteph,nstep,nstepr,iunito
      integer nxtinf,ihread,isteph,lstepr,iendrel,istep,ihr1,ihr2,nhleft
      integer ierr,ihdiff,ihr,ifldout,ihour,istop
      real    tstep,rmlimit,rnhrun,rnhrel,glat,glong,tf1,tf2,tnow,tnext
      real    x,y
      real    rscale,actweight
c ipcount(mdefcomp, nk)
      integer, dimension(:,:), allocatable:: ipcount
c npcount(nk)
      integer, dimension(:), allocatable:: npcount
      integer ilvl
      real    vlvl
cjb_start
       real mhmin, mhmax	! minimum and maximum of mixing height
cjb_end
c
      logical blfullmix
c
      character*72  finput,fldfil,logfile,ftype
      character*256 cinput,ciname,cipart,cipart2
      character*8   cpos1,cpos2
      character*1   tchar
      character*32  bqcomponent
c
      integer lenstr
c
      logical pendingOutput
#if defined(VOLCANO) || defined(TRAJ)
cjb 02.05
c
c... matrix for calculating volcanic ash concentrations + file name
c
       real vcon(nx,ny,3)
       character*60 cfile
cjb 19.05
       integer itimev(5),j
#if defined(TRAJ)
       real zzz
       character*60 tr_file
       integer ntraj,itraj
       real tlevel(100)
       character*80 tname(10)	! name for the trajectory
       integer tyear, tmon, tday, thour, tmin
       real distance, speed
       common /speed/ speed
#endif
#endif
c
c
c..used in xyconvert (longitude,latitude -> x,y)
      data geoparam/1.,1.,1.,1.,0.,0./
c
      data pendingOutput/.false./
c
      data nx/nxpre/
      data ny/nypre/

      iaction=0
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('SNAP',0,ZHOOK_HANDLE)
#endif
c initialize random number generator for rwalk
      CALL rwalk_init_random_seed()
      iexit=0
c
c
c-------------------------------------------------------------------
      narg=iargc()
      if(narg.lt.1) then
        write(6,*)
        write(6,*) '  usage: snap <snap.input>'
        write(6,*) '     or: snap <snap.input> <arguments>'
        write(6,*) '     or: snap <snap.input> ?     (to get help)'
        write(6,*)
        stop 1
      endif
      call getarg(1,finput)
c
      iuinp=8
      open(iuinp,file=finput,
     *           access='sequential',form='formatted',
     *           status='old',iostat=ios)
      if(ios.ne.0) then
        write(6,*) 'Open Error: ',finput(1:lenstr(finput,1))
        stop 1
      endif
c
      if(narg.eq.2) then
        call getarg(2,cinput)
        iprhlp=1
        if(cinput(1:1).eq.'?') goto 14
        iprhlp=0
      endif
c
c
      write(6,*) 'Reading input file:'
      write(6,*)  finput
c
      nlines=0
c
c..termination character (machine dependant)
      call termchar(tchar)
c
c..set release position as not chosen
      irelpos=0
c
      itime1(1)=-32767
      itime1(2)=-32767
      itime1(3)=-32767
      itime1(4)=-32767
      itime1(5)=-32767
      nhrun  =0
      nhrel  =0
      srelnam='*'
c
c..default values
      irwalk =1
      tstep  =900.
      mprel  =200
      nhfmin =6
      nhfmax =12
      nhfout =3
      isynoptic=0
      igraphics=0
      igrspec( 1)=0
      igrspec( 2)=1
      igrspec( 3)=1
      igrspec( 4)=0
      igrspec( 5)=1
      igrspec( 6)=1
      igrspec( 7)=1
      igrspec( 8)=0
      igrspec( 9)=0
      igrspec(10)=-1
      igrspec(11)=-1
      cgrspec='*'
      rgrspec(1)=  0.
      rgrspec(2)=100.
      rgrspec(3)=  0.
      rgrspec(4)=100.
      rgrspec(5)=  5.
      rgrspec(6)= 95.
      rgrspec(7)=  5.
      rgrspec(8)= 95.
      nlandfile=0
c
      savePNGstep= 0
      saveXPMstep= 0
      saveBMPstep= 0
c
      isvideo=0
c
      do m=1,mdefcomp
        compname(m)   ='Unknown'
        compnamemc(m) ='Unknown'
        kdrydep(m)    =-1
        kwetdep(m)    =-1
        kdecay(m)     =-1
        drydephgt(m)  =-1.
        drydeprat(m)  =-1.
        wetdeprat(m)  =-1.
        halftime(m)   =-1.
        kgravity(m)   =-1
        gravityms(m)  = 0.
       radiusmym(m)  = 0.
       densitygcm3(m)= 0.
        idcomp(m)     =-1
       iruncomp(m)   = 0
        totalbq(m)    = 0.
       numtotal(m)   = 0
      end do
      ncomp=0
      ndefcomp=0
      itotcomp=0
      rmlimit=-1.
      nprepro=0
      itprof=0
c
      do i=1,mtprof
       relradius(i)= -1.
       relupper(i)=  -1.
       rellower(i)=  -1.
       relstemradius(i)= -1.
      end do
c
      do m=1,mcomp
       relbqsec(1,m)= -1.
      end do
c
      nrelpos=0
      iprod  =0
      igrid  =0
      iprodr =0
      igridr =0
      ixbase =0
      iybase =0
      ixystp =0
      ivcoor =0
      nlevel =0
      minhfc =+6
      maxhfc =+32767
      nfilef =0
      ifltim =0
      blfullmix= .true.
      idrydep=0
      iwetdep=0
c
      inprecip =1
      imslp    =0
      imodlevel=0
c
      idebug=0
c
      ftype='felt'
      fldfil= 'snap.dat'
      logfile='snap.log'
c
      iensemble=0
      ensemblefile='ensemble.list'
      ensembleparticipant=09
      ensembleRandomKey='*******'
      ensembleStepHours=3
c
      iargos=0
      argoshourstep= 6
      argosdepofile= 'xxx_MLDP0_depo'
      argosconcfile= 'xxx_MLDP0_conc'
      argosdosefile= 'xxx_MLDP0_dose'
c
c..ipostyp=1 : latitude,longitude as decimal numbers (real)
c..ipostyp=2 : latitude,longitude as degree*100+minute (integer)
c
      ipostyp=1
c
c
      lcinp=len(cinput)
      iend=0
c
      do while (iend.eq.0)
c
        nlines=nlines+1
        read(iuinp,fmt='(a)',err=11) cinput
c
        ks=index(cinput,'*')
        if(ks.lt.1) ks=lcinp+1
c
        if(ks.eq.1) then
          nkey=0
        else
          do k=ks,lcinp
            cinput(k:k)=' '
          end do
c..check if input as environment variables or command line arguments
          call getvar(1,cinput,1,1,1,ierror)
          if(ierror.ne.0) goto 14
c..find keywords and values
          mkey=maxkey
          call keywrd(1,cinput,'=',';',mkey,kwhere,nkey,ierror)
          if(ierror.ne.0) goto 12
        end if
c
        do ikey=1,nkey
c
ccc         l=kwhere(1,ikey)
           k1=kwhere(2,ikey)
           k2=kwhere(3,ikey)
          kv1=kwhere(4,ikey)
          kv2=kwhere(5,ikey)
c
          if(kv1.gt.0) then
            nkv=kv2-kv1+1
            ciname=cinput(kv1:kv2)
            cipart=cinput(kv1:kv2)//tchar
          end if
c
c=======================================================================
c
          if(cinput(k1:k2).eq.'graphics.on') then
c..graphics.on
            igraphics=1
          elseif(cinput(k1:k2).eq.'graphics.off') then
c..graphics.off
            igraphics=0
          elseif(cinput(k1:k2).eq.'graphics.precip.on') then
c..graphics.precip.on
            igrspec(1)=1
          elseif(cinput(k1:k2).eq.'graphics.precip.off') then
c..graphics.precip.off
            igrspec(1)=0
          elseif(cinput(k1:k2).eq.'graphics.dry.dep.on') then
c..graphics.dry.dep.on
            igrspec(2)=1
          elseif(cinput(k1:k2).eq.'graphics.dry.dep.off') then
c..graphics.dry.dep.off
            igrspec(2)=0
          elseif(cinput(k1:k2).eq.'graphics.wet.dep.on') then
c..graphics.wet.dep.on
            igrspec(3)=1
          elseif(cinput(k1:k2).eq.'graphics.wet.dep.off') then
c..graphics.wet.dep.off
            igrspec(3)=0
          elseif(cinput(k1:k2).eq.'graphics.total.dep.on') then
c..graphics.total.dep.on
            igrspec(4)=1
          elseif(cinput(k1:k2).eq.'graphics.total.dep.off') then
c..graphics.total.dep.off
            igrspec(4)=0
          elseif(cinput(k1:k2).eq.'graphics.mslp.on') then
c..graphics.mslp.on
            igrspec(9)=1
          elseif(cinput(k1:k2).eq.'graphics.mslp.off') then
c..graphics.mslp.off
            igrspec(9)=0
          elseif(cinput(k1:k2).eq.'graphics.component') then
c..graphics.component= name
            if(kv1.lt.1) goto 12
            if(cgrspec.ne.'*' .or. igrspec(10).ne.-1) goto 12
            cgrspec=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'graphics.components.total') then
c..graphics.components.total
            if(cgrspec.ne.'*' .or. igrspec(10).ne.-1) goto 12
            igrspec(10)=0
          elseif(cinput(k1:k2).eq.'graphics.particles.keep.mass') then
c..graphics.particles.keep.mass
            if(igrspec(11).ne.-1) goto 12
            igrspec(11)=1
          elseif(cinput(k1:k2).eq.
     +          'graphics.particles.keep.positions') then
c..graphics.particles.keep.positions
            if(igrspec(11).ne.-1) goto 12
            igrspec(11)=0
          elseif(cinput(k1:k2).eq.
     +           'graphics.particles.in.boundary.layer.on') then
c..graphics.particles.in.boundary.layer.on
            igrspec(5)=1
          elseif(cinput(k1:k2).eq.
     +           'graphics.particles.in.boundary.layer.off') then
c..graphics.particles.in.boundary.layer.off
            igrspec(5)=0
          elseif(cinput(k1:k2).eq.
     +           'graphics.particles.above.boundary.layer.on') then
c..graphics.particles.above.boundary.layer.on
            igrspec(6)=1
          elseif(cinput(k1:k2).eq.
     +           'graphics.particles.above.boundary.layer.off') then
c..graphics.particles.above.boundary.layer.off
            igrspec(6)=0
          elseif(cinput(k1:k2).eq.'graphics.freeze.off') then
c..graphics.freeze.off
            igrspec(7)=0
          elseif(cinput(k1:k2).eq.'graphics.freeze.at.start') then
c..graphics.freeze.at.start
            igrspec(7)=1
          elseif(cinput(k1:k2).eq.
     +           'graphics.freeze.at.start.and.end') then
c..graphics.freeze.at.start.and.end
            igrspec(7)=2
          elseif(cinput(k1:k2).eq.'graphics.freeze.at.end') then
c..graphics.freeze.at.end
            igrspec(7)=3
          elseif(cinput(k1:k2).eq.
     +           'graphics.freeze.at.every.timestep') then
c..graphics.freeze.at.every.timestep
            igrspec(7)=4
          elseif(cinput(k1:k2).eq.
     +           'graphics.freeze.at.time.of.field.output') then
c..graphics.freeze.at.time.of.field.output
            igrspec(7)=5
          elseif(cinput(k1:k2).eq.'graphics.background.black') then
c..graphics.background.black
            igrspec(8)=0
          elseif(cinput(k1:k2).eq.'graphics.background.white') then
c..graphics.background.white
            igrspec(8)=1
          elseif(cinput(k1:k2).eq.
     +           'graphics.area.percent.x1.x2.y1.y2') then
c..graphics.area.percent.x1.x2.y1.y2=
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) (rgrspec(i),i=1,4)
            if(rgrspec(1).lt.0. .or. rgrspec(1).ge.rgrspec(2) .or.
     +         rgrspec(2).gt.100. .or.
     +         rgrspec(3).lt.0. .or. rgrspec(3).ge.rgrspec(4) .or.
     +         rgrspec(4).gt.100.) goto 12
          elseif(cinput(k1:k2).eq.
     +           'graphics.screen.percent.x1.x2.y1.y2') then
c..graphics.screen.percent.x1.x2.y1.y2=
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) (rgrspec(i),i=5,8)
            if(rgrspec(5).lt.0. .or. rgrspec(5).ge.rgrspec(6) .or.
     +         rgrspec(6).gt.100. .or.
     +         rgrspec(7).lt.0. .or. rgrspec(7).ge.rgrspec(8) .or.
     +         rgrspec(8).gt.100.) goto 12
          elseif(cinput(k1:k2).eq.'graphics.land') then
c..graphics.land= file_name
            if(nlandfile.lt.mlandfile) then
              if(kv1.lt.1) goto 12
              nlandfile=nlandfile+1
              landfile(nlandfile)=ciname(1:nkv)
            end if
          elseif(cinput(k1:k2).eq.'save.png.step') then
c..save.png.step= 1
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) savePNGstep
          elseif(cinput(k1:k2).eq.'save.xpm.step') then
c..save.xpm.step= 0
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) saveXPMstep
          elseif(cinput(k1:k2).eq.'save.bmp.step') then
c..save.bmp.step= 0
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) saveBMPstep
          elseif(cinput(k1:k2).eq.'video.save.on') then
c..video.save.on
            isvideo=1
          elseif(cinput(k1:k2).eq.'video.save.off') then
c..video.save.off
            isvideo=0
          elseif(cinput(k1:k2).eq.'positions.decimal') then
c..positions.decimal
            ipostyp=1
          elseif(cinput(k1:k2).eq.'positions.degree_minute') then
c..positions.degree_minute
            ipostyp=2
          elseif(cinput(k1:k2).eq.'time.start') then
c..time.start=<year,month,day,hour>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) (itime1(i),i=1,4)
            itime1(5)=0
          elseif(cinput(k1:k2).eq.'time.run') then
c..time.run=<hours'h'> or <days'd'>
            if(kv1.lt.1) goto 12
            kh=index(cipart,'h')
            kd=index(cipart,'d')
            if(kh.gt.0 .and. kd.eq.0) then
              cipart(kh:kh)=' '
              read(cipart,*,err=12) rnhrun
              nhrun=nint(rnhrun)
            elseif(kd.gt.0 .and. kh.eq.0) then
              cipart(kd:kd)=' '
              read(cipart,*,err=12) rnhrun
              nhrun=nint(rnhrun*24.)
            else
              goto 12
            end if
          elseif(cinput(k1:k2).eq.'time.release') then
c..time.release=<hours'h'> or <days'd'>
            if(kv1.lt.1) goto 12
            kh=index(cipart,'h')
            kd=index(cipart,'d')
            if(kh.gt.0 .and. kd.eq.0) then
              cipart(kh:kh)=' '
              read(cipart,*,err=12) rnhrel
              nhrel=nint(rnhrel)
            elseif(kd.gt.0 .and. kh.eq.0) then
              cipart(kd:kd)=' '
              read(cipart,*,err=12) rnhrel
              nhrel=nint(rnhrel*24.)
            else
              goto 12
            end if
          elseif(cinput(k1:k2).eq.'set_release.pos') then
c..set_release.pos=<name>   or <p=lat,long>
            if(kv1.lt.1) goto 12
            srelnam=ciname
            if(srelnam(1:2).eq.'p=' .or. srelnam(1:2).eq.'P=') then
              cipart(1:2)='  '
              read(cipart,*,err=12) glat,glong
              if(ipostyp.eq.2) then
                ig=nint(glat)
                igd=ig/100
                igm=ig-igd*100
                glat=float(igd)+float(igm)/60.
                ig=nint(glong)
                igd=ig/100
                igm=ig-igd*100
                glong=float(igd)+float(igm)/60.
              end if
              srelnam=' '
              write(cpos1,fmt='(sp,f7.2,ss)') glat
              write(cpos2,fmt='(sp,f7.2,ss)') glong
              k1=index(cpos1,'+')
              if(k1.gt.0) then
                cpos1(8:8)='N'
              else
                k1=index(cpos1,'-')
                cpos1(8:8)='S'
              end if
              k2=index(cpos2,'+')
              if(k2.gt.0) then
                cpos2(8:8)='E'
              else
                k2=index(cpos2,'-')
                cpos2(8:8)='W'
              end if
              srelnam=cpos1(k1+1:8)//' '//cpos2(k2+1:8)
              if(nrelpos.lt.mrelpos) nrelpos=nrelpos+1
              relnam(nrelpos)=srelnam
              relpos(1,nrelpos)=glat
              relpos(2,nrelpos)=glong
            end if
          elseif(cinput(k1:k2).eq.'random.walk.on') then
c..random.walk.on
            irwalk=1
          elseif(cinput(k1:k2).eq.'random.walk.off') then
c..random.walk.off
            irwalk=0
          elseif(cinput(k1:k2).eq.'boundary.layer.full.mix.off') then
c..boundary.layer.full.mix.off
            blfullmix= .false.
          elseif(cinput(k1:k2).eq.'boundary.layer.full.mix.on') then
c..boundary.layer.full.mix.on
            blfullmix= .true.
          elseif(cinput(k1:k2).eq.'dry.deposition.old') then
c..dry.deposition.old
           if(idrydep.ne.0 .and. idrydep.ne.1) goto 12
           idrydep=1
          elseif(cinput(k1:k2).eq.'dry.deposition.new') then
c..dry.deposition.new
           if(idrydep.ne.0 .and. idrydep.ne.2) goto 12
           idrydep=2
          elseif(cinput(k1:k2).eq.'wet.deposition.old') then
c..wet.deposition.old
           if(iwetdep.ne.0 .and. iwetdep.ne.1) goto 12
           iwetdep=1
          elseif(cinput(k1:k2).eq.'wet.deposition.new') then
c..wet.deposition.new
           if(iwetdep.ne.0 .and. iwetdep.ne.2) goto 12
           iwetdep=2
          elseif(cinput(k1:k2).eq.'time.step') then
c..time.step=<seconds>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) tstep
            if(tstep.lt.0.9999) goto 12
          elseif(cinput(k1:k2).eq.'time.release.profile.constant') then
c..time.release.profile.constant
           if(itprof.ne.0 .and. itprof.ne.1) goto 12
           itprof=1
          elseif(cinput(k1:k2).eq.'time.release.profile.bomb') then
c..time.release.profile.bomb
           if(itprof.ne.0 .and. itprof.ne.2) goto 12
           itprof=2
          elseif(cinput(k1:k2).eq.'time.release.profile.linear') then
c..time.release.profile.linear
           if(itprof.ne.0 .and. itprof.ne.3) goto 12
           itprof=3
          elseif(cinput(k1:k2).eq.'time.release.profile.steps') then
c..time.release.profile.steps
           if(itprof.ne.0 .and. itprof.ne.4) goto 12
           itprof=4
          elseif(cinput(k1:k2).eq.'release.day'    .or.
     +		 cinput(k1:k2).eq.'release.hour'   .or.
     +		 cinput(k1:k2).eq.'release.minute' .or.
     +		 cinput(k1:k2).eq.'release.second') then
           rscale=1.
           if(cinput(k1:k2).eq.'release.day')    rscale=24.
           if(cinput(k1:k2).eq.'release.minute') rscale=1./60.
           if(cinput(k1:k2).eq.'release.second') rscale=1./3600.
c..release.day
c..release.hour
c..release.minute
c..release.second
           if(kv1.lt.1 .or. ntprof.gt.0) goto 12
            i1=ntprof+1
            i2=ntprof
            ios=0
            do while (ios.eq.0)
              if(i2.gt.mtprof) goto 13
              i2=i2+1
              read(cipart,*,iostat=ios) (irelhour(i),i=i1,i2)
            end do
            i2=i2-1
            if(i2.lt.i1) goto 12
           do i=i1,i2
             irelhour(i)= nint(irelhour(i)*rscale)
           end do
            ntprof=i2
           if(irelhour(1).ne.0) goto 12
           do i=2,ntprof
             if(irelhour(i-1).ge.irelhour(i)) goto 12
           end do
          elseif(cinput(k1:k2).eq.'release.radius.m') then
c..release.radius.m
           if(kv1.lt.1 .or. ntprof.lt.1) goto 12
            read(cipart,*,err=12) (relradius(i),i=1,ntprof)
           do i=1,ntprof
             if(relradius(i).lt.0.) goto 12
           end do
          elseif(cinput(k1:k2).eq.'release.upper.m') then
c..release.upper.m
           if(kv1.lt.1 .or. ntprof.lt.1) goto 12
            read(cipart,*,err=12) (relupper(i),i=1,ntprof)
           do i=1,ntprof
             if(relupper(i).lt.0.) goto 12
           end do
          elseif(cinput(k1:k2).eq.'release.lower.m') then
c..release.lower.m
           if(kv1.lt.1 .or. ntprof.lt.1) goto 12
            read(cipart,*,err=12) (rellower(i),i=1,ntprof)
           do i=1,ntprof
             if(rellower(i).lt.0.) goto 12
           end do
          elseif(cinput(k1:k2).eq.'release.mushroom.stem.radius.m') then
c..release.mushroom.stem.radius.m
           if(kv1.lt.1 .or. ntprof.lt.1) goto 12
            read(cipart,*,err=12) (relstemradius(i),i=1,ntprof)
          elseif(cinput(k1:k2).eq.'release.bq/hour.comp' .or.
     +           cinput(k1:k2).eq.'release.bq/sec.comp'  .or.
     +           cinput(k1:k2).eq.'release.bq/day.comp' .or.
     +           cinput(k1:k2).eq.'release.bq/step.comp') then
c..release.bq/hour.comp
c..release.bq/sec.comp
c..release.bq/day.comp
c..release.bq/step.comp
           if(cinput(k1:k2).eq.'release.bq/hour.comp') then
             rscale=1./3600.
           elseif(cinput(k1:k2).eq.'release.bq/day.comp') then
             rscale=1./(3600.*24.)
           elseif(cinput(k1:k2).eq.'release.bq/step.comp') then
             rscale=-1.
           else
             rscale=1.
           end if
           if(kv1.lt.1 .or. ntprof.lt.1) goto 12
           ncomp= ncomp+1
           if(ncomp.gt.mcomp) goto 13
            read(cipart,*,err=12) (relbqsec(i,ncomp),i=1,ntprof),
     +				   component(ncomp)
           do i=1,ntprof
             if(relbqsec(i,ncomp).lt.0.) goto 12
           end do
           if(rscale.gt.0.) then
             do i=1,ntprof
               relbqsec(i,ncomp)= relbqsec(i,ncomp)*rscale
             end do
           elseif(ntprof.gt.1 .and. relbqsec(ntprof,ncomp).ne.0.) then
             goto 12
           elseif(ntprof.gt.1) then
             do i=1,ntprof-1
       	rscale=1./(3600.*(irelhour(i+1)-irelhour(i)))
               relbqsec(i,ncomp)= relbqsec(i,ncomp)*rscale
             end do
           end if
          elseif(cinput(k1:k2).eq.'max.particles.per.release') then
c..max.particles.per.release
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) mprel
            if(mprel.lt.1) goto 12
          elseif(cinput(k1:k2).eq.'component') then
c..component= name
            if(kv1.lt.1) goto 12
            ndefcomp=ndefcomp+1
            if(ndefcomp.gt.mdefcomp) goto 12
            compname(ndefcomp)=ciname(1:nkv)
            compnamemc(ndefcomp)=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'dry.dep.on') then
c..dry.dep.on
            if(ndefcomp.lt.1 .or. kdrydep(ndefcomp).ne.-1) goto 12
            kdrydep(ndefcomp)=1
          elseif(cinput(k1:k2).eq.'dry.dep.off') then
c..dry.dep.off
            if(ndefcomp.lt.1 .or. kdrydep(ndefcomp).ne.-1) goto 12
            kdrydep(ndefcomp)=0
          elseif(cinput(k1:k2).eq.'wet.dep.on') then
c..wet.dep.on
            if(ndefcomp.lt.1 .or. kwetdep(ndefcomp).ne.-1) goto 12
            kwetdep(ndefcomp)=1
          elseif(cinput(k1:k2).eq.'wet.dep.off') then
c..wet.dep.off
            if(ndefcomp.lt.1 .or. kwetdep(ndefcomp).ne.-1) goto 12
            kwetdep(ndefcomp)=0
          elseif(cinput(k1:k2).eq.'dry.dep.height') then
c..dry.dep.height=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. drydephgt(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) drydephgt(ndefcomp)
          elseif(cinput(k1:k2).eq.'dry.dep.ratio') then
c..dry.dep.ratio=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. drydeprat(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) drydeprat(ndefcomp)
          elseif(cinput(k1:k2).eq.'wet.dep.ratio') then
c..wet.dep.ratio=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. wetdeprat(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) wetdeprat(ndefcomp)
          elseif(cinput(k1:k2).eq.'radioactive.decay.on') then
c..radioactive.decay.on
            if(ndefcomp.lt.1 .or. kdecay(ndefcomp).ne.-1) goto 12
            kdecay(ndefcomp)=1
          elseif(cinput(k1:k2).eq.'radioactive.decay.off') then
c..radioactive.decay.off
            if(ndefcomp.lt.1 .or. kdecay(ndefcomp).ne.-1) goto 12
            kdecay(ndefcomp)=0
          elseif(cinput(k1:k2).eq.'half.lifetime.minutes') then
c..half.lifetime.minutes=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. halftime(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) halftime(ndefcomp)
           halftime(ndefcomp)=halftime(ndefcomp)/60.
          elseif(cinput(k1:k2).eq.'half.lifetime.hours') then
c..half.lifetime.hours=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. halftime(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) halftime(ndefcomp)
          elseif(cinput(k1:k2).eq.'half.lifetime.days') then
c..half.lifetime.days=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. halftime(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) halftime(ndefcomp)
           halftime(ndefcomp)=halftime(ndefcomp)*24.
          elseif(cinput(k1:k2).eq.'half.lifetime.years') then
c..half.lifetime.years=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. halftime(ndefcomp).ge.0.) goto 12
            read(cipart,*,err=12) halftime(ndefcomp)
           halftime(ndefcomp)=halftime(ndefcomp)*24.*365.25
          elseif(cinput(k1:k2).eq.'gravity.off') then
c..gravity.off
            if(ndefcomp.lt.1 .or. kgravity(ndefcomp).ne.-1) goto 12
           kgravity(ndefcomp)= 0
          elseif(cinput(k1:k2).eq.'gravity.fixed.m/s') then
c..gravity.fixed.m/s
            if(ndefcomp.lt.1 .or. kgravity(ndefcomp).ne.-1) goto 12
           kgravity(ndefcomp)= 1
           if(kv1.lt.1) goto 12
            read(cipart,*,err=12) gravityms(ndefcomp)
           if (gravityms(ndefcomp).le.0.) goto 12
          elseif(cinput(k1:k2).eq.'gravity.fixed.cm/s') then
c..gravity.fixed.cm/s
            if(ndefcomp.lt.1 .or. kgravity(ndefcomp).ne.-1) goto 12
           kgravity(ndefcomp)= 1
           if(kv1.lt.1) goto 12
            read(cipart,*,err=12) gravityms(ndefcomp)
           if (gravityms(ndefcomp).le.0.) goto 12
           gravityms(ndefcomp)=gravityms(ndefcomp)*0.01
          elseif(cinput(k1:k2).eq.'radius.micrometer') then
c..radius.micrometer  (for gravity computation)
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. radiusmym(ndefcomp).gt.0.) goto 12
            read(cipart,*,err=12) radiusmym(ndefcomp)
           if(radiusmym(ndefcomp).le.0.) goto 12
          elseif(cinput(k1:k2).eq.'density.g/cm3') then
c..density.g/cm3  (for gravity computation)
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. densitygcm3(ndefcomp).gt.0.) goto 12
            read(cipart,*,err=12) densitygcm3(ndefcomp)
           if(densitygcm3(ndefcomp).le.0.) goto 12
          elseif(cinput(k1:k2).eq.'field.identification') then
c..field.identification=
            if(kv1.lt.1) goto 12
            if(ndefcomp.lt.1 .or. idcomp(ndefcomp).ge.0) goto 12
            read(cipart,*,err=12) idcomp(ndefcomp)
           if(idcomp(ndefcomp).lt.1) goto 12
           do i=1,ndefcomp-1
             if(idcomp(i).eq.idcomp(ndefcomp)) goto 12
            end do
          elseif(cinput(k1:k2).eq.'precip(mm/h).probab') then
c..precip(mm/h).probab=<precip_intensity,probability, ...>
            if(kv1.lt.1) goto 12
            i1=nprepro+1
            i2=nprepro
            ios=0
            do while (ios.eq.0)
              if(i2.gt.mprepro) goto 12
              i2=i2+1
              read(cipart,*,iostat=ios)
     +                    ((prepro(k,i),k=1,2),i=i1,i2)
            end do
            i2=i2-1
            if(i2.lt.i1) goto 12
            nprepro=i2
          elseif(cinput(k1:k2).eq.'remove.relative.mass.limit') then
c..remove.relative.mass.limit=
            if(kv1.lt.1) goto 12
            if(rmlimit.ge.0.00) goto 12
            read(cipart,*,err=12) rmlimit
            if(rmlimit.lt.0.0 .or. rmlimit.gt.0.5) goto 12
          elseif(cinput(k1:k2).eq.'step.hour.input.min') then
c..step.hour.input.min=<hours>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) nhfmin
          elseif(cinput(k1:k2).eq.'step.hour.input.max') then
c..step.hour.input.max=<hours>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) nhfmax
          elseif(cinput(k1:k2).eq.'step.hour.output.fields') then
c..step.hour.output.fields=<hours>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) nhfout
          elseif(cinput(k1:k2).eq.'synoptic.output') then
c..synoptic.output ... output at synoptic hours
            isynoptic=1
          elseif(cinput(k1:k2).eq.'asynoptic.output') then
c..asynoptic.output ... output at fixed intervals after start
            isynoptic=0
          elseif(cinput(k1:k2).eq.'total.components.off') then
c..total.components.off
            itotcomp=0
          elseif(cinput(k1:k2).eq.'total.components.on') then
c..total.components.on
            itotcomp=1
          elseif(cinput(k1:k2).eq.'mslp.on') then
c..mslp.on
            imslp=1
          elseif(cinput(k1:k2).eq.'mslp.off') then
c..mslp.off
            imslp=0
          elseif(cinput(k1:k2).eq.'precipitation.on') then
c..precipitation.on
            inprecip=1
          elseif(cinput(k1:k2).eq.'precipitation.off') then
c..precipitation.off
            inprecip=0
          elseif(cinput(k1:k2).eq.'model.level.fields.on') then
c..model.level.fields.on
            imodlevel=1
          elseif(cinput(k1:k2).eq.'model.level.fields.off') then
c..model.level.fields.off
            imodlevel=0
          elseif(cinput(k1:k2).eq.'release.pos') then
c..release.pos=<'name',latitude,longitude>
            if(kv1.lt.1) goto 12
            if(nrelpos.lt.mrelpos) then
              nrelpos=nrelpos+1
              read(cipart,*,err=12) relnam(nrelpos),
     +                              (relpos(i,nrelpos),i=1,2)
              if(ipostyp.eq.2) then
                ig=nint(relpos(1,nrelpos))
                igd=ig/100
                igm=ig-igd*100
                relpos(1,nrelpos)=float(igd)+float(igm)/60.
                ig=nint(relpos(2,nrelpos))
                igd=ig/100
                igm=ig-igd*100
                relpos(2,nrelpos)=float(igd)+float(igm)/60.
              end if
            else
              write(6,*) 'WARNING. Too many RELEASE POSITIONS'
              write(6,*) '  ==> ',cinput(kv1:kv2)
            end if
          elseif(cinput(k1:k2).eq.'grid.input') then
c..grid.input=<producer,grid>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) iprod,igrid
          elseif(cinput(k1:k2).eq.'grid.size') then
c..grid.size=<nx,ny>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) nx,ny
          elseif(cinput(k1:k2).eq.'grid.run') then
c..grid.run=<producer,grid,ixbase,iybase,ixystp>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) iprodr,igridr,ixbase,iybase,ixystp
          elseif(cinput(k1:k2).eq.'data.sigma.levels') then
c..data.sigma.levels
            ivcoor=2
          elseif(cinput(k1:k2).eq.'data.eta.levels') then
c..data.eta.levels
            ivcoor=10
          elseif(cinput(k1:k2).eq.'levels.input') then
c..levels.input=<num_levels, 0,kk,k,k,k,....,1>
c..levels.input=<num_levels, 0,kk,k,k,k,....,18,0,0,...>
            if(nlevel.ne.0) goto 12
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) nlevel
            nk = nlevel
            ALLOCATE ( klevel(nk), STAT = AllocateStatus)
            IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
            ALLOCATE ( ipcount(mdefcomp, nk), STAT = AllocateStatus)
            IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
            ALLOCATE ( npcount(nk), STAT = AllocateStatus)
            IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

            read(cipart,*,err=12) nlevel,(klevel(i),i=1,nlevel)
            if(klevel(1).ne.0 .or. klevel(2).eq.0) goto 12
            kadd=0
            do i=nk,2,-1
              if(klevel(i).eq.0) kadd=kadd+1
            end do
            do i=nk-kadd-1,2,-1
              if(klevel(i).le.klevel(i+1)) goto 12
            end do
          elseif(cinput(k1:k2).eq.'forecast.hour.min') then
c..forecast.hour.min= +6
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) minhfc
          elseif(cinput(k1:k2).eq.'forecast.hour.max') then
c..forecast.hour.max= +32767
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) maxhfc
          elseif(cinput(k1:k2).eq.'field.type') then
c..field.type felt or netcdf
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) ftype
          elseif(cinput(k1:k2).eq.'field.input') then
c..field.input=  felt_file_name
            if(kv1.lt.1) goto 12
            if(nfilef.lt.mfilef) then
              nfilef=nfilef+1
              limfcf(1,nfilef)=minhfc
              limfcf(2,nfilef)=maxhfc
              if(ciname(1:1).eq.'''' .or. ciname(1:1).eq.'"') then
                read(cipart,*,err=12) filef(nfilef)
              else
                filef(nfilef)=ciname(1:nkv)
              end if
            else
              write(6,*) 'WARNING. Too many FIELD INPUT files'
              write(6,*) '  ==> ',cinput(kv1:kv2)
            end if
          elseif(cinput(k1:k2).eq.'field_time.forecast') then
c..field_time.forecast ... use forecast length in output field ident.
            ifltim=0
          elseif(cinput(k1:k2).eq.'field_time.valid') then
c..field_time.valid ...... use valid time in output field identification
            ifltim=1
          elseif(cinput(k1:k2).eq.'field.output') then
c..field.output= <'file_name'>
            if(kv1.lt.1) goto 12
            fldfil=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'log.file') then
c..log.file= <'log_file_name'>
            if(kv1.lt.1) goto 12
            logfile=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'debug.off') then
c..debug.off
            idebug=0
          elseif(cinput(k1:k2).eq.'debug.on') then
c..debug.on
            idebug=1
          elseif(cinput(k1:k2).eq.'ensemble.project.output.off') then
c..ensemble.project.output.off
            iensemble=0
          elseif(cinput(k1:k2).eq.'ensemble.project.output.on') then
c..ensemble.project.output.on
            iensemble=1
          elseif(cinput(k1:k2).eq.'ensemble.project.output.file') then
c..ensemble.project.output.file= <ensemble.list>
            if(kv1.lt.1) goto 12
            ensemblefile=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'ensemble.project.participant') then
c..ensemble.project.participant= 09
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) ensembleparticipant
           if(ensembleparticipant.lt.0 .or.
     +	       ensembleparticipant.gt.99) goto 12
          elseif(cinput(k1:k2).eq.'ensemble.project.random.key') then
c..ensemble.project.random.key= <rl52s3u>
            if(kv1.lt.1) goto 12
            ensembleRandomKey=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.
     +			'ensemble.project.output.step.hour') then
c..ensemble.project.output.step.hour= 3
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) ensembleStepHours
          elseif(cinput(k1:k2).eq.'argos.output.off') then
c..argos.output.off
            iargos=0
          elseif(cinput(k1:k2).eq.'argos.output.on') then
c..argos.output.on
            iargos=1
          elseif(cinput(k1:k2).eq.'argos.output.deposition.file') then
c..argos.output.deposition.file= runident_MLDP0_depo
            if(kv1.lt.1) goto 12
            argosdepofile=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.
     +		 	'argos.output.concentration.file') then
c..argos.output.concentration.file= runident_MLDP0_conc
            if(kv1.lt.1) goto 12
            argosconcfile=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'argos.output.totaldose.file') then
c..argos.output.totaldose.file= runident_MLDP0_dose
            if(kv1.lt.1) goto 12
            argosdosefile=ciname(1:nkv)
          elseif(cinput(k1:k2).eq.'argos.output.timestep.hour') then
c..argos.output.timestep.hour= <3>
            if(kv1.lt.1) goto 12
            read(cipart,*,err=12) argoshourstep
           if(argoshourstep.le.0 .or.
     +	       argoshourstep.gt.240) goto 12
          elseif(cinput(k1:k2).eq.'end') then
c..end
#if defined(TRAJ)
       read(iuinp,*) ntraj
       write(*,*) 'ntraj=', ntraj
       do i=1,ntraj
          read(iuinp,*) tlevel(i)
          write(*,*) tlevel(i)
       enddo
       do i=1,ntraj
          read(iuinp,'(a80)') tname(i)
          write(*,'(i4,1x,a80)') i,tname(i)
       enddo
#endif
            iend=1
          else
            write(6,*) 'ERROR.  Unknown input:'
            write(6,*) cinput
            goto 12
          end if
c
        end do
c
      end do
c
      goto 18
c
   11 write(6,*) 'ERROR reading file: ',finput(1:lenstr(finput,1))
      write(6,*) 'At line no. ',nlines
      iexit=2
      goto 18
c
   12 write(6,*) 'ERROR reading file: ',finput(1:lenstr(finput,1))
      write(6,*) 'At line no. ',nlines,' :'
      write(6,*)  cinput(1:lenstr(cinput,1))
      iexit=2
      goto 18
c
   13 write(6,*) 'ERROR reading file:'
      write(6,*)  finput
      write(6,*) 'At line no. ',nlines,' :'
      write(6,*)  cinput(1:lenstr(cinput,1))
      write(6,*) 'SOME LIMIT WAS EXCEEDED !!!!!!!!!!!!!!!!!'
      iexit=2
      goto 18
c
   14 l=index(finput,' ')-1
      if(l.lt.1) l=len(finput)
      write(6,*) 'Help from ',finput(1:l),' :'
      call prhelp(iuinp,'*=>')
      iexit=2
      goto 18
c
   18 close(iuinp)
c
      if (iexit.ne.0) then
        call exit(1)
      end if
c

c initialize all arrays after reading input
      maxsiz = nx*ny
      ldata=20+maxsiz+50
      CALL allocateFields()


c..convert names to uppercase letters
      call chcase(2,1,srelnam)
      if(nrelpos.gt.0) call chcase(2,nrelpos,relnam)
      if(ndefcomp.gt.0) call chcase(2,ndefcomp,compname)
      if(ncomp.gt.0)    call chcase(2,ncomp,component)
c
      ierror=0
      do n=1,nrelpos
        if(srelnam.eq.relnam(n)) irelpos=n
      end do
      if(irelpos.eq.0 .and. nrelpos.eq.1) irelpos=1
      if(irelpos.eq.0) then
        write(6,*) 'No (known) release position selected'
        ierror=1
      end if
c
      if(ivcoor.eq.0) then
        write(6,*) 'Input model level type (sigma,eta) not specified'
        ierror=1
      end if
      if(nlevel.eq.0) then
        write(6,*) 'Input model levels not specified'
        ierror=1
      end if
      if(ftype .ne. "felt" .and. ftype .ne. "netcdf") then
        write(6,*) 'Input type not felt or netcdf:', ftype
        ierror=1
      end if
      if(nfilef.eq.0) then
        write(6,*) 'No input field files specified'
        ierror=1
      end if
c
      if(nhrel.eq.0 .and. ntprof.gt.0) nhrel=irelhour(ntprof)
c
c..check if compiled without ENSEMBLE PROJECT arrays
      if(nxep.lt.2 .or. nyep.lt.2) iensemble=0
c
      if(ntprof.lt.1) then
       write(6,*) 'No time profile(s) specified'
       ierror=1
      end if
      if(itprof.lt.1) then
       write(6,*) 'No time profile type specified'
       ierror=1
      end if
      k=0
      do i=1,ntprof
       if(relradius(i).lt.0. .or.
     +     relupper(i) .lt.0. .or.
     +     rellower(i) .lt.0. .or.
     +     relupper(i).lt.rellower(i)) k=1
       if(relupper(i).lt.rellower(i)+1.) relupper(i)=rellower(i)+1.
      end do
      if(k.eq.1) then
       write(6,*) 'ERROR in relase profiles ',
     +			'of upper,lower and/or radius'
       ierror=1
      end if
c
      if(ncomp.lt.0) then
        write(6,*) 'No (release) components specified for run'
        ierror=1
      end if
      if(ndefcomp.lt.0) then
        write(6,*) 'No (release) components defined'
        ierror=1
      end if
c
      do m=1,ndefcomp-1
       if(idcomp(m).lt.1) then
         write(6,*) 'Component has no field identification: ',
     +		      compname(m)(1:lenstr(compname(m),1))
       end if
       do i=m+1,ndefcomp
         if(compname(m).eq.compname(i)) then
           write(6,*) 'Component defined more than once: ',
     +			compname(m)(1:lenstr(compname(m),1))
           ierror=1
         end if
       end do
      end do
c
      do m=1,ncomp-1
       do i=m+1,ncomp
         if(component(m).eq.component(i)) then
           write(6,*) 'Released component defined more than once: ',
     +			component(m)(1:lenstr(component(m),1))
           ierror=1
         end if
       end do
      end do
c
c..match used components with defined components
      do m=1,ncomp
       k=0
       do i=1,ndefcomp
          if(component(m).eq.compname(i)) k=i
       end do
       if(k.gt.0) then
         idefcomp(m)= k
         iruncomp(k)= m
       else
         write(6,*) 'Released component ',
     +		      component(m)(1:lenstr(component(m),1)),
     +               ' is not defined'
         ierror=1
       end if
      end do
c
c..gravity
      do n=1,ncomp
        m=idefcomp(n)
       if(kgravity(m).lt.0) kgravity(m)= 2
       if(kgravity(m).eq.2 .and.
     +     (radiusmym(m).le.0. .or. densitygcm3(m).le.0.)) then
         write(6,*) 'Gravity error. radius,density: ',
     +			radiusmym(m),densitygcm3(m)
         ierror=1
        end if
      end do
c
      if(idrydep.eq.0) idrydep=1
      if(iwetdep.eq.0) iwetdep=1
      i1=0
      i2=0
      idecay=0
c
      do n=1,ncomp
        m=idefcomp(n)
        if(idrydep.eq.1 .and. kdrydep(m).eq.1) then
          if(drydeprat(m).gt.0. .and. drydephgt(m).gt.0.) then
            i1=i1+1
          else
            write(6,*) 'Dry deposition error. rate,height: ',
     +                    drydeprat(m),drydephgt(m)
            ierror=1
          end if
        elseif(idrydep.eq.2 .and. kdrydep(m).eq.1) then
          if(kgravity(m).eq.1 .and. gravityms(m).gt.0.) then
            i1=i1+1
          elseif(kgravity(m).eq.2) then
            i1=i1+1
          else
            write(6,*) 'Dry deposition error. gravity: ',
     +                    gravityms(m)
            ierror=1
          end if
        end if
c
        if(iwetdep.eq.1 .and. kwetdep(m).eq.1) then
          if(wetdeprat(m).gt.0.) then
            i2=i2+1
          else
            write(6,*) 'Wet deposition error. rate: ',
     +                    wetdeprat(m)
            ierror=1
          end if
        elseif(iwetdep.eq.2 .and. kwetdep(m).eq.1) then
          if(radiusmym(m).gt.0.) then
            i2=i2+1
          else
            write(6,*) 'Wet deposition error. radius: ',
     +                    radiusmym(m)
            ierror=1
          end if
        end if
c
        if(kdecay(m).eq.1 .and. halftime(m).gt.0.) then
          idecay=1
        else
          kdecay(m)=0
          halftime(m)=0.
        end if
      end do
c
      if(i1.eq.0) idrydep=0
      if(i2.eq.0) iwetdep=0
c
      if(ierror.ne.0) then
        call exit(1)
      end if
c
      if(itotcomp.eq.1 .and. ncomp.eq.1) itotcomp=0
c
      if(rmlimit.lt.0.0) rmlimit=0.0001
c
      if(iprodr.eq.0) iprodr=iprod
      if(igridr.eq.0) igridr=igrid
c
      if(idrydep.ne.1) igrspec(2)=0
      if(iwetdep.ne.1) igrspec(3)=0
c
      if(igrspec(4).eq.1) igrspec(2)=0
      if(igrspec(4).eq.1) igrspec(3)=0
c
c..possibly only read mslp
      if(igrspec(9).eq.1 .and. imslp.eq.0) imslp=-1
c
c..default graphics.component is the first
      if(igrspec(10).eq.-1 .and. cgrspec.ne.'*') then
        do m=1,ncomp
          if(cgrspec.eq.compname(m)) igrspec(10)=m
        end do
      end if
      if(igrspec(10).eq.0 .and. ncomp.eq.1) igrspec(10)=1
      if(igrspec(10).lt.0) igrspec(10)=1
c
c..default graphics.particles.keep.mass
      if(igrspec(11).lt.0) igrspec(11)=1
c
c..precipitation is needed for wet depositions and possibly graphics
      if(iwetdep.eq.1) inprecip=1
      if(igrspec(1).eq.1 .and. inprecip.eq.0) inprecip=-1
c
      write(6,*) 'Input o.k.'
c-------------------------------------------------------------------
c
c..log file
      iulog=9
      open(iulog,file=logfile,
     *           access='sequential',form='formatted',
     *           status='unknown')
c
      ntimefo=0
c
c..define fixed tables and constants (independant of input data)
      call tabcon
c
c..file unit for all input field files
      iunitf=20
c
c..file unit for temporary open files (graphics etc.)
      iunitx=90
c
c..check input FELT files and make sorted lists of available data
c..make main list based on x wind comp. (u) in upper used level
      if (ftype .eq. "netcdf") then
        call filesort_nc(iunitf, ierror)
      else
        call filesort(iunitf,ierror)
      end if
      if(ierror.ne.0) goto 910
c
c..itime: itime(1) - year
c         itime(2) - month
c         itime(3) - day
c         itime(4) - hour
c         itime(5) - forecast time in hours (added to date/time above)
c
c..itime1: start time
c..itime2: stop  time
c
      call vtime(itime1,ierror)
      if(ierror.ne.0) then
        write(9,*) 'Requested start time is wrong:'
        write(9,*) (itime1(i),i=1,4)
        write(6,*) 'Requested start time is wrong:'
        write(6,*) (itime1(i),i=1,4)
        goto 910
      end if
c
      do i=1,5
        itime2(i)=itime1(i)
      end do
      itime2(5)=itime2(5)+nhrun
      call vtime(itime2,ierror)
c
      call hrdiff(0,0,itimer(1,1),itime1,nh1,ierr1,ierr2)
      call hrdiff(0,0,itime2,itimer(1,2),nh2,ierr1,ierr2)
      if(nh1.lt.0 .or. nh2.lt.0) then
        write(9,*) 'Not able to run requested time periode.'
        write(9,*) 'Start:        ',(itime1(i),i=1,4)
        write(9,*) 'End:          ',(itime2(i),i=1,4)
        write(9,*) 'First fields: ',(itimer(i,1),i=1,4)
        write(9,*) 'Last  fields: ',(itimer(i,2),i=1,4)
        write(6,*) 'Not able to run requested time periode.'
        write(6,*) 'Start:        ',(itime1(i),i=1,4)
        write(6,*) 'End:          ',(itime2(i),i=1,4)
        write(6,*) 'First fields: ',(itimer(i,1),i=1,4)
        write(6,*) 'Last  fields: ',(itimer(i,2),i=1,4)
        if(nh1.lt.0) then
          write(9,*) 'NO DATA AT START OF RUN'
          write(6,*) 'NO DATA AT START OF RUN'
          goto 910
        end if
        write(9,*) 'Running until end of data'
        write(6,*) 'Running until end of data'
        do i=1,5
          itime2(i)=itimer(i,2)
        end do
        call hrdiff(0,0,itime1,itime2,nhrun,ierr1,ierr2)
      end if
c
      if(nhrel.gt.nhrun) nhrel=nhrun
c
      if(iargos.eq.1) then
       argoshoursrelease= nhrel
       argoshoursrun=     nhrun
c..the following done to avoid updateing subr. fldout............
       nhfout= argoshourstep
       isynoptic= 0
c................................................................
      end if
#if defined(TRAJ)
       do 201 itraj=1,ntraj
       rellower(1)=tlevel(itraj)
c	relupper(1)=rellower(1)+1
       relupper(1)=rellower(1)
       tyear=itime1(1)
       tmon=itime1(2)
       tday=itime1(3)
       thour=itime1(4)
       tmin=0.0
       write(*,*) 'lower, upper',rellower(1),relupper(1)
       write(*,*) 'tyear, tmon, tday, thour, tmin',
     &  tyear, tmon, tday, thour, tmin
       distance=0.0
       speed=0.0
#endif
c
c..initial no. of plumes and particles
      nplume=0
      npart=0
      nparnum=0
c
c..no. of timesteps per hour (adjust the timestep)
      nsteph=nint(3600./tstep)
      tstep=3600./float(nsteph)
c
c..total no. of timesteps to run (nhrun is no. of hours to run)
      nstep=nsteph*nhrun
c
c..total no. of timesteps to release particles
      nstepr=nsteph*nhrel
c
c..nuclear bomb case
      if(itprof.eq.2) nstepr=1
c
c..field output file unit
      iunito=30
c
c..information to log file
      write(9,*) 'nx,ny,nk:  ',nx,ny,nk
c      write(9,*) 'nxad,nyad: ',nxad,nyad
      write(9,*) 'nxmc,nymc: ',nxmc,nymc
      write(9,*) 'kadd:      ',kadd
      write(9,*) 'klevel:'
      write(9,*) (klevel(i),i=1,nk)
      write(9,*) 'imslp:     ',imslp
      write(9,*) 'inprecip:  ',inprecip
      write(9,*) 'imodlevel: ',imodlevel
      write(9,*) 'itime1:  ',(itime1(i),i=1,5)
      write(9,*) 'itime2:  ',(itime2(i),i=1,5)
      write(9,*) 'itimer1: ',(itimer(i,1),i=1,5)
      write(9,*) 'itimer2: ',(itimer(i,2),i=1,5)
      write(9,*) 'nhfmin:  ',nhfmin
      write(9,*) 'nhfmax:  ',nhfmax
      write(9,*) 'nhrun:   ',nhrun
      write(9,*) 'nhrel:   ',nhrel
      write(9,*) 'tstep:   ',tstep
      write(9,*) 'nsteph:  ',nsteph
      write(9,*) 'nstep:   ',nstep
      write(9,*) 'nstepr:  ',nstepr
      write(9,*) 'mprel:   ',mprel
      write(9,*) 'ifltim:  ',ifltim
      write(9,*) 'irwalk:  ',irwalk
      write(9,*) 'idrydep: ',idrydep
      write(9,*) 'iwetdep: ',iwetdep
      write(9,*) 'idecay:  ',idecay
      write(9,*) 'rmlimit: ',rmlimit
      write(9,*) 'ndefcomp:',ndefcomp
      write(9,*) 'ncomp:   ',ncomp
      write(9,fmt='(1x,a,40(1x,i2))') 'idefcomp: ',
     +				      (idefcomp(i),i=1,ncomp)
      write(9,fmt='(1x,a,40(1x,i2))') 'iruncomp: ',
     +				      (iruncomp(i),i=1,ndefcomp)
      do n=1,ncomp
       m=idefcomp(n)
        write(9,*) 'component no:  ',n
        write(9,*) 'compname:   ',compname(m)
        write(9,*) '  field id:   ',idcomp(m)
        write(9,*) '  kdrydep:    ',kdrydep(m)
        write(9,*) '  drydephgt:  ',drydephgt(m)
        write(9,*) '  drydeprat:  ',drydeprat(m)
        write(9,*) '  kwetdep:    ',kwetdep(m)
        write(9,*) '  wetdeprat:  ',wetdeprat(m)
        write(9,*) '  kdecay:     ',kdecay(m)
        write(9,*) '  halftime:   ',halftime(m)
        write(9,*) '  decayrate:  ',decayrate(m)
        write(9,*) '  kgravity:   ',kgravity(m)
        write(9,*) '  gravityms:  ',gravityms(m)
        write(9,*) '  radiusmym:  ',radiusmym(m)
        write(9,*) '  densitygcm3:',densitygcm3(m)
        write(9,*) '  Relase time profile:   ntprof: ',ntprof
       do i=1,ntprof
          write(9,*) '  hour,Bq/hour: ',
     +			irelhour(i),relbqsec(i,n)*3600.
       end do
      end do
      write(9,*) 'itotcomp:   ',itotcomp
      write(9,*) 'nprepro:    ',nprepro
      write(9,*) 'iensemble:  ',iensemble
      write(9,*) 'iargos:     ',iargos
      write(9,*) 'blfulmix:   ',blfullmix
c
c..initialize files, deposition fields etc.
      m=0
      nargos=0
      do n=1,nhrun
        do i=1,4
         itime(i)=itime1(i)
       end do
       itime(5)=n
       if(isynoptic.eq.0) then
c..asynoptic output (use forecast length in hours to test if output)
         ihour=itime(5)
       else
c..synoptic output  (use valid hour to test if output)
         call vtime(itime,ierror)
         ihour=itime(4)
       end if
       if(mod(ihour,nhfout).eq.0) m=m+1
       if(iargos.eq.1) then
         if(mod(n,argoshourstep).eq.0) then
           if(nargos.lt.margos) then
             nargos=nargos+1
             do i=1,4
               argostime(i,nargos)=itime1(i)
             end do
             argostime(5,nargos)=n
           end if
         end if
       end if
      end do
      call fldout(-1,iunito,fldfil,itime1,0.,0.,0.,tstep,
     *            m,nsteph,ierror)
      if(ierror.ne.0) goto 910
c
      do i=1,5
        itime(i)=itime1(i)
        itimefi(i)=0
        itimefa(i)=0
        itimefo(i,1)=0
        itimefo(i,2)=0
      end do
c
      nxtinf=0
      ihread=0
      isteph=0
      lstepr=0
      iendrel=0
c
      istep=-1
c
#if defined(TRAJ)
c	write(*,*) (itime(i),i=1,5)
       do i=1,5
          itimev(i)=itime(i)
       enddo
c	write(*,*) (itimev(i),i=1,5)
1110	continue
c	write(tr_file,'(''Trajectory_'',i3.3,
c     &  ''_'',i4,3i2.2,''0000.DAT'')') itraj,(itime(i),i=1,4)
c	open(13,file=tr_file)
       open(13,file=tname(itraj))
       rewind 13
c	write(*,*) tr_file
       write(*,*) tname(itraj)

c	write(13,'(i6,3f12.3)') nstep
c
#endif

 1111 continue
cjb_start
       mhmin=10000.0
       mhmax=-10.0
cjb_end

#if defined(VOLCANO)
cjb 01.05 initialize concentration (mass) matrix
c
       do i=1,nx
       do j=1,ny
       do k=1,3
          vcon(i,j,k)=0.0
       enddo
       enddo
       enddo
cjb END
#endif

c
      do 200 istep=0,nstep
c
        write(9,*) 'istep,nplume,npart: ',istep,nplume,npart
        flush(9)
        if(mod(istep,nsteph).eq.0) then
          write(6,*) 'istep,nplume,npart: ',istep,nplume,npart
          flush(6)
        end if
c
c#######################################################################
c..test print: printing all particles in plume 'jpl'
c       write(88,*) 'step,plume,part: ',istep,nplume,npart
c       jpl=1
c       if(jpl.le.nplume .and. iplume(1,jpl).gt.0) then
c         do n=iplume(1,jpl),iplume(2,jpl)
c           write(88,fmt='(1x,i6,2f7.2,2f7.4,f6.0,f7.3,i4,f8.3)')
c    +                  iparnum(n),(pdata(i,n),i=1,5),pdata(8,n),
c    +                  icomp(n),pdata(9,n)
c         end do
c       end if
c#######################################################################
c
        if(istep.eq.nxtinf) then
c
c..read fields
          if(istep.eq.0) then
            do i=1,5
              itimei(i)=itime1(i)
            end do
            ihr1=-0
            ihr2=-nhfmax
            nhleft=nhrun
          else
            do i=1,5
              itimei(i)=itimefi(i)
            end do
            ihr1=+nhfmin
            ihr2=+nhfmax
            nhleft=(nstep-istep+1)/nsteph
          end if
c          write (*,*) "readfield(", iunitf, istep, nhleft, itimei, ihr1
c     +          ,ihr2, itimefi, ierror, ")"
          if (ftype .eq. "netcdf") then
            call readfield_nc(iunitf,istep,nhleft,itimei,ihr1,ihr2,
     +                   itimefi,ierror)
          else
            call readfield(iunitf,istep,nhleft,itimei,ihr1,ihr2,
     +                   itimefi,ierror)
          end if
c          write (*,*) "readfield(", iunitf, istep, nhleft, itimei, ihr1
c     +          ,ihr2, itimefi, ierror, ")"
          if(ierror.ne.0) goto 910
c
c..analysis time of input model
          if(itimefi(5).le.+6) then
            do i=1,4
              itimefa(i)=itimefi(i)
            end do
            itimefa(5)=0
          end if
c
          n=itimefi(5)
          call vtime(itimefi,ierr)
          write(6,fmt='(''input data: '',i4,3i3.2,''  prog='',i4)')
     +                             (itimefi(i),i=1,4),n
c
c..compute model level heights
          call compheight
c
c..calculate boundary layer (top and height)
          call bldp
c
          if(istep.eq.0) then
c
c..release position from geographic to polarstereographic coordinates
            y=relpos(1,irelpos)
            x=relpos(2,irelpos)
            write(9,*) 'release lat,long: ',y,x
#if defined(TRAJ)
c	write(*,*) istep,x,y,rellower(1)
c	write(13,'(i6,3f12.3)') istep,x,y,rellower(1)
c
       write(13,'(''RIMPUFF'')')
       write(13,'(i2)') ntraj
       write(13,'(1x,i4,4i2.2,''00'',
     &  2f9.3,f12.3,f15.2,f10.2)')
     &  (itime(i),i=1,4),mod(istep,12)*5,y,x,rellower(1),distance,speed
       write(*,'(i4,1x,i4,i2,i2,2i2.2,''00'',
     &  2f9.3,f12.3,f15.2,f10.2)') istep,
     &  (itime(i),i=1,4),mod(istep,12)*5,y,x,rellower(1),distance,speed
#endif
            call xyconvert(1,x,y,2,geoparam,igtype,gparam,ierror)
            if(ierror.ne.0) then
              write(9,*) 'ERROR: xyconvert'
              write(9,*) '   igtype: ',igtype
              write(9,*) '   gparam: ',gparam
              write(6,*) 'ERROR: xyconvert'
              write(6,*) '   igtype: ',igtype
              write(6,*) '   gparam: ',gparam
              goto 910
            end if
            write(9,*) 'release   x,y:    ',x,y
            if(x.lt.1.01 .or. x.gt.nx-0.01 .or.
     -         y.lt.1.01 .or. y.gt.ny-0.01) then
              write(9,*) 'ERROR: Release position outside field area'
              write(6,*) 'ERROR: Release position outside field area'
              goto 910
            end if
            relpos(3,irelpos)=x
            relpos(4,irelpos)=y
c
            if(iensemble.eq.1)
     +        call ensemble(0,itime1,tf1,tf2,tnow,istep,nstep,nsteph)
c
            nxtinf=1
            ifldout=0
c continue istep loop after initialization
            goto 200
          end if
c
         if(iensemble.eq.1)
     +      call ensemble(1,itime,tf1,tf2,tnow,istep,nstep,nsteph)
c
          call hrdiff(0,0,itimei,itimefi,ihdiff,ierr1,ierr2)
          tf1=0.
          tf2=3600.*ihdiff
          if(istep.eq.1) then
            call hrdiff(0,0,itimei,itime1,ihr,ierr1,ierr2)
            tnow=3600.*ihr
            nxtinf=istep+nsteph*(ihdiff-ihr)
           iprecip=1+ihr
          else
            tnow=0.
            nxtinf=istep+nsteph*ihdiff
           iprecip=1
          end if
c
        else
c
          tnow=tnow+tstep
c
        end if
c
        tnext=tnow+tstep
c
        if(iendrel.eq.0 .and. istep.le.nstepr) then
c
c..release one plume of particles
c
          call release(istep-1,nsteph,tf1,tf2,tnow,ierror)
c
          if(ierror.eq.0) then
c..interpolation of boundary layer top, height, precipitation etc.
            call posint(1,tf1,tf2,tnow)
            lstepr=istep
          else
            write(9,*) 'WARNING. Out of space for plumes/particles'
            write(9,*) 'WARNING. End release, continue running'
            write(6,*) 'WARNING. Out of space for plumes/particles'
            write(6,*) 'WARNING. End release, continue running'
            iendrel=1
          end if
c
        end if
c
c#############################################################
c     write(6,*) 'tf1,tf2,tnow,tnext,tstep,ipr: ',
c    +		  tf1,tf2,tnow,tnext,tstep,iprecip
c     write(9,*) 'tf1,tf2,tnow,tnext,tstep,ipr: ',
c    +		  tf1,tf2,tnow,tnext,tstep,iprecip
c#############################################################
c
c#######################################################################
c	if(npart.gt.0) then
c          write(88,*) 'istep,nplume,npart,nk: ',istep,nplume,npart,nk
c          do k=1,nk
c            npcount(k)=0
c            do i=1,mdefcomp
c              ipcount(i,k)=0
c            end do
c          end do
c          do n=1,npart
c            vlvl=pdata(3,n)
c            ilvl=vlvl*10000.
c            k=ivlevel(ilvl)
c            npcount(k)  =npcount(k)+1
c            m=icomp(n)
c            ipcount(m,k)=ipcount(m,k)+1
c          end do
c          do k=nk,1,-1
c            if(npcount(k).gt.0) then
c              write(88,8800) k,npcount(k),(ipcount(i,k),i=1,ndefcomp)
c 8800	      format(1x,i2,':',i7,2x,12(1x,i5))
c            end if
c          end do
c          write(88,*) '----------------------------------------------'
c        end if
c#######################################################################
c
c..radioactive decay
c
       if(idecay.eq.1) call decay(tstep)
c
       if(iensemble.eq.1)
     +    call ensemble(2,itime,tf1,tf2,tnow,istep,nstep,nsteph)
c
c..dry deposition (1=old, 2=new version)
c
        if(idrydep.eq.1) call drydep1
        if(idrydep.eq.2) call drydep2(tstep)
c
       if(iensemble.eq.1)
     +    call ensemble(3,itime,tf1,tf2,tnow,istep,nstep,nsteph)
c
c..wet deposition (1=old, 2=new version)
c
        if(iwetdep.eq.1) call wetdep1
        if(iwetdep.eq.2) call wetdep2(tstep)
c
       if(iensemble.eq.1)
     +    call ensemble(4,itime,tf1,tf2,tnow,istep,nstep,nsteph)
c
c..move all particles forward
c
        call forwrd(tf1,tf2,tnow,tstep)
c
c..apply the random walk method (diffusion)
c
        if(irwalk.ne.0) call rwalk(tstep,blfullmix)
c
c..remove particles outside grid domain or without any mass left
c
        call rmpart(rmlimit)
c
c..interpolation of boundary layer top, height, precipitation etc.
c
        call posint(0,tf1,tf2,tnext)
c
       if(iensemble.eq.1)
     +    call ensemble(5,itime,tf1,tf2,tnext,istep,nstep,nsteph)
cjb_start
       do n=1,npart
          if(pdata(5,n) .gt. mhmax) mhmax=pdata(5,n)
          if(pdata(5,n) .lt. mhmin) mhmin=pdata(5,n)
       enddo
cjb_end
c
c###################################################################
c	write(6,
c    +  fmt='(''istep,nstep,isteph,nsteph,iprecip,nprecip: '',6i4)')
c    +          istep,nstep,isteph,nsteph,iprecip,nprecip
c	write(9,
c    +  fmt='(''istep,nstep,isteph,nsteph,iprecip,nprecip: '',6i4)')
c    +          istep,nstep,isteph,nsteph,iprecip,nprecip
c###################################################################
c
c..output...................................................
#if defined(VOLCANO)
       do k=1,npart
          x=pdata(1,k)
          y=pdata(2,k)
          i=nint(pdata(1,k))
          j=nint(pdata(2,k))
          if(pdata(3,k) .gt. 0.43)
     &     vcon(i,j,1)=vcon(i,j,1)+pdata(9,k)/120.0				! level 1
          if(pdata(3,k) .gt. 0.23 .and. pdata(3,k) .le. 0.43)
     &     vcon(i,j,2)=vcon(i,j,2)+pdata(9,k)/120.0				! level 2
          if(pdata(3,k) .gt. 0.03 .and. pdata(3,k) .le. 0.216)
     &     vcon(i,j,3)=vcon(i,j,3)+pdata(9,k)/120.0				! level 3
       enddo
cccc
c	if(mod(istep,nsteph).eq.0) then
c     +    write(6,*) 'istep,nplume,npart: ',istep,nplume,npart
cjb... START
cjb... output with concentrations after 6 hours
       if(istep .gt. 1 .and. mod(istep,72).eq.0) then
       write(*,*) (itime(i),i=1,5)
       do i=1,5
          itimev(i)=itime(i)
       enddo
       itimev(5)= itime(5)+1
       call vtime(itimev,ierror)
       write(*,*) (itimev(i),i=1,5)
c     +    write(6,*) 'istep,nplume,npart: ',istep,nplume,npart
c	write(6,*)
c	write(6,*) 'istep,hour,npart=',istep,istep/72,npart
c
c... calculate number of non zero model grids
c
       m=0
       do i=1,nx
       do j=1,ny
       do k=1,3
          if(vcon(i,j,k) .gt. 0.0) m=m+1
       enddo
       enddo
       enddo
c
c... write non zero model grids, their gegraphical coordinates and mass to output file
c
cjb 19.05 start
       write(cfile,'(''concentrations-'',i2.2)') istep/72
       open(12,file=cfile)
       rewind 12
       write(12,'(i4,3i2.2)') (itimev(i),i=1,4)
       write(12,'(i6,'' - non zero grids'')') m
       write(*,*)
       write(*,*) 'Output no.:',istep/72
       write(*,*) 'Time (hrs): ',istep/12
c
       m=0
       do i=1,nx
       do j=1,ny
       do k=1,3
          if(vcon(i,j,k) .gt. 0.0) then
             m=m+1
             x=real(i)+0.5
             y=real(j)+0.5
             call xyconvert(1,x,y,igtype,gparam,2,geoparam,ierror)
             write(12,'(i6,2x,3i5,2x,2f10.3,2x,e12.3)')
     &        m,i,j,k,x,y,vcon(i,j,k)/72.0
c	      write(*,'(i6,2x,3i5,2x,2f10.3,2x,e12.3)')
c     &        m,i,j,k,x,y,vcon(i,j,k)/72.0
          endif
       enddo
       enddo
       enddo
c
       do i=1,nx
       do j=1,ny
       do k=1,3
          vcon(i,j,k)=0.0
       enddo
       enddo
       enddo
c
       write(6,*) 'npart all=',npart
       write(6,*) 'ngrid all=',m
       endif
       close (12)
cjb... END
#endif
c
c..fields
      ifldout=0
      isteph=isteph+1
      if(isteph.eq.nsteph) then
        isteph=0
        itime(5)=itime(5)+1
        do i=1,5
          itimeo(i)=itime(i)
        end do
        call vtime(itimeo,ierror)
        if(isynoptic.eq.0) then
c..asynoptic output (use forecast length in hours to test if output)
          ihour=itime(5)
        else
c..synoptic output  (use valid hour to test if output)
          ihour=itimeo(4)
        end if
        if(mod(ihour,nhfout).eq.0) then
          ifldout=1
          if(ifltim.eq.0) then
c..identify fields with forecast length (hours after start)
            do i=1,5
              itimeo(i)=itime(i)
            end do
          end if
c..save first and last output time
          ntimefo=ntimefo+1
          if(ntimefo.eq.1) then
            do i=1,5
              itimefo(i,1)=itimeo(i)
            end do
          end if
          do i=1,5
            itimefo(i,2)=itimeo(i)
          end do
          write(9,*) 'fldout. ',itimeo
        end if
      end if
c
c
 3333 continue
c
      if(iensemble.eq.1 .and. isteph.eq.0)
     +  call ensemble(6,itime,tf1,tf2,tnext,istep,nstep,nsteph)
c
c..field output if ifldout=1, always accumulation for average fields
      call fldout(ifldout,iunito,fldfil,itimeo,tf1,tf2,tnext,tstep,
     +            istep,nsteph,ierror)
      if(ierror.ne.0) goto 910
c
c###c..select particles to be displayed (graphics) or saved in video files.
c###      if(igraphics.eq.1 .or. isvideo.eq.1)
c###     +  call pselect(igrspec,idrydep,iwetdep)
c
c..store data for 'video replay'
      if(isvideo.eq.1)
     +   call videosave(iunitx,istep,nstep,itime,isteph,nsteph,itime1,
     +                  igrspec)
c
      if(isteph.eq.0 .and. iprecip.lt.nprecip) iprecip=iprecip+1
c
#if defined(TRAJ)
cjb
       tmin=tmin+tstep/60.0
       if(tmin .eq. 60) then
          thour=thour+1
          tmin=0.0
       endif
       if(thour .eq. 24) then
          tday=tday+1
          thour=0
       endif
       distance=distance+speed*tstep
       if(istep .gt. 0 .and. mod(istep,12).eq.0) then
c	if(istep .gt. -1) then
       call vtime(itimev,ierror)
c	write(*,*) (itime(i),i=1,5), ierror
c	write(*,*) (itimev(i),i=1,5)
c	write(*,*) 'istep=',istep, 'npart=',npart
c	do k=1,npart
       do k=1,1
c	write(*,*) istep,pdata(1,k),pdata(2,k),pdata(3,k)
             x=pdata(1,k)
             y=pdata(2,k)
             i=int(x)
             j=int(y)
             call xyconvert(1,x,y,igtype,gparam,2,geoparam,ierror)
             vlvl=pdata(3,k)
             ilvl=vlvl*10000.
             k1=ivlevel(ilvl)
             k2=k1+1
       zzz=hlevel2(i,j,k2)+(hlevel2(i,j,k1)-hlevel2(i,j,k2))*
     &  (pdata(3,k)-vlevel(k2))/(vlevel(k1)-vlevel(k2))
c	write(*,*) istep,x,y,pdata(3,k),k1,k2,
c     &  vlevel(k1),vlevel(k2),hlevel2(i,j,k1),hlevel2(i,j,k2),zzz
c	write(*,*)
c	write(*,*) istep,k,x,y,zzz
c	write(*,'(1x,i4,i2,i2,2i2.2,''00'')')
c     &(itime(i),i=1,4),mod(istep,12)*5
       write(13,'(1x,i4,4i2.2,''00'',
     &  2f9.3,f12.3,f15.2,f10.2)')
     &  tyear,tmon,tday,thour,mod(istep,12)*5,
     &  y,x,zzz,distance, speed
       write(*,'(i4,1x,i4,i2,i2,2i2.2,''00'',
     &  2f9.3,f12.3,f15.2,f10.2)') istep,
     &  tyear,tmon,tday,thour,mod(istep,12)*5,
     &  y,x,zzz,distance, speed
cjb-2701
       enddo
       endif
#endif
  200 continue
#if defined(TRAJ)
       close (13)
201	continue
#endif
c
c
 2222 continue
c
      istop=0
      goto 990
c
  910 istop=2
      goto 990
c
  990 continue
c
      if(iensemble.eq.1)
     +  call ensemble(7,itimefa,tf1,tf2,tnext,istep,nstep,nsteph)
c
      if(iargos.eq.1) then
        close(91)
        close(92)
        close(93)
      end if
c
      if(istop.eq.0 .and. lstepr.lt.nstep .and. lstepr.lt.nstepr) then
        write(9,*) 'ERROR: Due to space problems the release period was'
        write(9,*) '       shorter than requested.'
        write(9,*) '   No. of requested release timesteps: ',nstepr
        write(9,*) '   No. of simulated release timesteps: ',lstepr
        write(6,*) 'ERROR: Due to space problems the release period was'
        write(6,*) '       shorter than requested.'
        write(6,*) '   No. of requested release timesteps: ',nstepr
        write(6,*) '   No. of simulated release timesteps: ',lstepr
        istop=1
      end if
c
      if(istop.eq.0) then
cjb_240311
       write(*,*)
       write(*,'(''mhmax='',f10.2)') mhmax
       write(*,'(''mhmin='',f10.2)') mhmin
       write(*,*)
cjb_end
        write(9,*) ' SNAP run finished'
        write(6,*) ' SNAP run finished'
      else
        write(9,*) ' ------- SNAP ERROR EXIT -------'
        write(6,*) ' ------- SNAP ERROR EXIT -------'
      end if
c
      close(iulog)
c
c deallocate all fields
      CALL deAllocateFields()
      if(istop.gt.0) call exit(istop)
#if defined(DRHOOK)
      ! Before the very last statement
      IF (LHOOK) CALL DR_HOOK('SNAP',1,ZHOOK_HANDLE)
#endif
      stop
      end
