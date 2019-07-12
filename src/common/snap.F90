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


! SNAP - Severe Nuclear Accident Program

!-----------------------
! Options in snap.input:
!=======================================================================
! * comment
! POSITIONS.DECIMAL
! POSITIONS.DEGREE+MINUTE
! TIME.START=        1993,1,25,12
! TIME.RUN  =        5d
! TIME.RELEASE=      12h
! TITLE=  This is a optional title
! SET_RELEASE.POS=   P.1
! RANDOM.WALK.ON
! RANDOM.WALK.OFF
! BOUNDARY.LAYER.FULL.MIX.OFF
! BOUNDARY.LAYER.FULL.MIX.ON
! DRY.DEPOSITION.OLD .......................... (default)
! DRY.DEPOSITION.NEW
! WET.DEPOSITION.OLD .......................... (default)
! WET.DEPOSITION.NEW
! TIME.STEP= 900.
! TIME.RELEASE.PROFILE.CONSTANT
! TIME.RELEASE.PROFILE.BOMB
! TIME.RELEASE.PROFILE.LINEAR
! TIME.RELEASE.PROFILE.STEPS
! * RELEASE.SECOND=
! * RELEASE.MINUTE=
! * RELEASE.DAY=                    0,       1,       2, ....
! RELEASE.HOUR=                     0,      24,      48, ....
! RELEASE.RADIUS.M=             10000,   10000,   10000, ....
! RELEASE.UPPER.M=              10000,   10000,   10000, ....
! RELEASE.LOWER.M=               5000,    5000,    5000, ....
! RELEASE.MUSHROOM.STEM.RADIUS.M= 500,     500,     500, ....
! RELEASE.BQ/HOUR.COMP=   2.e+15,  1.e+15, 0.5e+15, ..., 'Aerosol.large'
! RELEASE.BQ/HOUR.COMP=   1.e+15,  1.e+15, 0.5e+15, ..., 'Aerosol.medium'
! RELEASE.BQ/HOUR.COMP=       0.,  1.e+15, 0.5e+15, ..., 'Aerosol.small'
! * RELEASE.BQ/SEC.COMP=
! * RELEASE.BQ/DAY.COMP=
! * RELEASE.BQ/STEP.COMP=
! MAX.PARTICLES.PER.RELEASE= 2000
! MAX.TOTALPARTICLES=2000000
! * Input for multi-timesteps, multi-height releases
! RELEASE.FILE=release.txt
! RELEASE.COMPONENTS= 'CS137', 'XE133', ...
! RELEASE.HEIGHTLOWER.M= 0  ,626,
! RELEASE.HEIGHTUPPER.M= 625,1275
! RELEASE.HEIGHTRADIUS.M= 0, 0
! COMPONENT= Aerosol
! DRY.DEP.ON
! DRY.DEP.OFF
! WET.DEP.ON
! WET.DEP.OFF
! DRY.DEP.HEIGHT= 44.
! DRY.DEP.RATIO=  0.1
! WET.DEP.RATIO=  0.2
! RADIOACTIVE.DECAY.ON
! RADIOACTIVE.DECAY.OFF
! HALF.LIFETIME.MINUTES= 45.5
! HALF.LIFETIME.HOURS= ...
! HALF.LIFETIME.DAYS= ....
! HALF.LIFETIME.YEARS= ....
! * GRAVITY.OFF
! * GRAVITY.FIXED.M/S= 0.01
! * GRAVITY.FIXED.CM/S= 1.0
! RADIUS.MICROMETER= 50.
! DENSITY.G/CM3= 19.
! FIELD.IDENTIFICATION= 1
! TOTAL.COMPONENTS.OFF
! TOTAL.COMPONENTS.ON
! PRECIP(MM/H).PROBAB= 0.0,0.00, 0.5,0.31, 1.0,0.48, 1.5,0.60, 2.0,0.66
! PRECIP(MM/H).PROBAB= 3.3,0.72, 8.3,0.80, 15.,0.85, 25.,0.91
! REMOVE.RELATIVE.MASS.LIMIT= 0.02
! STEP.HOUR.INPUT.MIN=  6
! STEP.HOUR.INPUT.MAX= 12
! STEP.HOUR.OUTPUT.FIELDS=  3
! SYNOPTIC.OUTPUT
! ASYNOPTIC.OUTPUT
! MSLP.ON
! MSLP.OFF
! PRECIPITATION.ON
! PRECIPITATION.OFF
! MODEL.LEVEL.FIELDS.ON
! MODEL.LEVEL.FIELDS.OFF
! * only write particles to model-level, which are at least DUMPTIME in h
! * old. If DUMPTIME > 0, the dumped particles will be removed from model
! MODEL.LEVEL.FIELDS.DUMPTIME= 4.0
! RELEASE.POS= 'P.1', 58.50, -4.00
! GRID.INPUT= 88,1814
! GRID.SIZE= 360,180
! * gtype and gparam(6) according to felt.txt (required only for nc-files)
! * rotated latlon, hirlam 12
! GRID.GPARAM = 3,-46.400002,-36.400002,0.10800000,0.10800000, 0.0000000, 65.000000
! * emep 1x1 deg lat lon
! * GRID.GPARAM = 2, -179.,-89.5,1.,1., 0., 0.
! GRID.RUN=   88,1814, 1,1,1
! * Norlam (sigma levels)
! DATA.SIGMA.LEVELS
! * Hirlam (eta levels)
! DATA.ETA.LEVELS
! * select the ensemble member (0=first, -1=no ensemble-input)
! ENSEMBLE_MEMBER.INPUT = 0
! * wind.surface = wind.10m (one 0 level first in list)
! LEVELS.INPUT= 14, 0,31,30,29,28,27,26,25,23,21,17,13,7,1
! * forecast.hour.min/max for following files (may change between files)
! FORECAST.HOUR.MIN= +6 ......................... (default is +6)
! FORECAST.HOUR.MAX= +32767 ..................... (default is +32767)
! FIELD.TYPE=felt|netcdf
! FIELD.INPUT= arklam.dat
! FIELD.INPUT= feltlam.dat
! FIELD_TIME.FORECAST
! FIELD_TIME.VALID
! FIELD.OUTPUT= snap.felt
! FIELD.OUTTYPE=netcdf/felt
! FIELD.DAILY.OUTPUT.ON
! FIELD.USE_MODEL_WIND_INSTEAD_OF_10M= [.false.]/.true
! * timestamp which will also be written to netcdf-files, default: now
! SIMULATION.START.DATE=2010-01-01_10:00:00
! LOG.FILE=     snap.log
! DEBUG.OFF ..................................... (default)
! DEBUG.ON
! ENSEMBLE.PROJECT.OUTPUT.OFF ................... (default)
! ENSEMBLE.PROJECT.OUTPUT.ON
! ENSEMBLE.PROJECT.OUTPUT.FILE= ensemble.list
! ENSEMBLE.PROJECT.PARTICIPANT= 09
! ENSEMBLE.PROJECT.RANDOM.KEY=  RL52S3U
! ENSEMBLE.PROJECT.OUTPUT.STEP.HOUR= 3
! ARGOS.OUTPUT.OFF
! ARGOS.OUTPUT.ON
! ARGOS.OUTPUT.DEPOSITION.FILE=    runident_MLDP0_depo
! ARGOS.OUTPUT.CONCENTRATION.FILE= runident_MLDP0_conc
! ARGOS.OUTPUT.TOTALDOSE.FILE=     runident_MLDP0_dose
! ARGOS.OUTPUT.TIMESTEP.HOUR= 3
! END
!=======================================================================

!-----------------------------------------------------------------------
! DNMI library subroutines : rfelt
!                            rlunit
!                            chcase
!                            termchar
!                            getvar
!                            keywrd
!                            hrdiff
!                            vtime
!                            gridpar
!                            mapfield
!                            xyconvert
!                            daytim
!                            mwfelt

!-----------------------------------------------------------------------
!  DNMI/FoU  18.09.1992  Anstein Foss
!  DNMI/FoU  19.02.1993  Anstein Foss
!  DNMI/FoU  18.11.1993  Anstein Foss
!  DNMI/FoU  08.04.1994  Anstein Foss
!  DNMI/FoU  24.10.1994  Anstein Foss ... GL graphics
!  DNMI/FoU  26.10.1994  Anstein Foss ... rmpart(...)
!  DNMI/FoU  05.12.1994  Anstein Foss ... status file for normem etc.
!  DNMI/FoU  06.01.1995  Anstein Foss
!  DNMI/FoU  21.03.1995  Anstein Foss
!  DNMI/FoU  21.04.1995  Anstein Foss
!  DNMI/FoU  05.06.1995  Anstein Foss ... Norlam+Hirlam
!  DNMI/FoU  16.08.1995  Anstein Foss
!  DNMI/FoU  06.10.1995  Anstein Foss ... no receive positions
!  DNMI/FoU  24.10.1995  Anstein Foss ... aerosol+gas+noble.gas+...
!  DNMI/FoU  23.11.1995  Anstein Foss
!  DNMI/FoU  11.03.1996  Anstein Foss ... video.save
!  DNMI/FoU  13.09.1996  Anstein Foss ... ECMWF model level data
!  DNMI/FoU  22.11.1996  Anstein Foss ... mass in graphics +++
!  DNMI/FoU  06.02.2001  Anstein Foss ... Ensemble project output
!  DNMI/FoU  17.02.2001  Anstein Foss ... Qt/OpenGL graphics
!  DNMI/FoU  24.08.2001  Anstein Foss ... Radioactive decay
!  DNMI/FoU  29.10.2001  Anstein Foss ... Nuclear bomb, gravity +++
!  DNMI/FoU  10.12.2001  Anstein Foss ... NKS, 'singlecomponent' +++++
!  DNMI/FoU  01.12.2002  Anstein Foss ... Argos output for SSV
!  DNMI/FoU  04.12.2002  Anstein Foss ... (Argos) no mass, only Bq
!  DNMI/FoU  09.04.2003  Anstein Foss ... remove 10m level option (k10m)
!  DNMI/FoU  15.08.2003  Anstein Foss ... ARGOS output in the model grid
!  DNMI/FoU  23.11.2003  Anstein Foss ... BOMB version drydep2,wetdep2
!  DNMI/FoU  16.01.2004  Anstein Foss ... mushroom shape release
!  DNMI/FoU  04.11.2004  Anstein Foss ... bugfix (pselect called too late)
!  DNMI/FoU  08.02.2005  Anstein Foss ... gravity(m/s) in pdata(n)%grv
! met.no/FoU 20.06.2006  Anstein Foss ... Ensemble update
! met.no/FoU 22.03.2013  Heiko Klein  ... Fix output-format for argos/nrpa
!-----------------------------------------------------------------------



!> SNAP - Severe Nuclear Accident Program
PROGRAM bsnap
  USE iso_fortran_env, only: real64, output_unit, error_unit, IOSTAT_END
  USE DateCalc, only : epochToDate, timeGM
  USE snapdebug, only: iulog, idebug
  USE snapargosML, only: argosdepofile, argosdosefile, argoshoursrelease, &
      argoshourstep, argoshoursrun, iargos, margos, argosconcfile, nargos, &
      argostime
  USE snapepsML, only: ensemblefile, ensembleparticipant, ensemblerandomkey, &
      ensemblestephours, iensemble
  USE snapdimML, only: nx, ny, nk, nxmc, nymc, ldata, maxsiz, mcomp, mdefcomp
  USE snapfilML, only: filef, itimer, limfcf, ncsummary, nctitle, nhfmax, nhfmin, &
      nctype, nfilef, simulation_start
  USE snapfldML, only: enspos, iprecip, nprecip
  USE snapmetML, only: init_meteo_params, use_model_wind_for_10m
  USE snapparML, only: compname, kgravity, component, running_to_defined_comp, itprof, &
      ncomp, totalbq, compnamemc, gravityms, iruncomp, idcomp, nparnum
  USE snapposML, only: irelpos, nrelpos, release_positions
  USE snapgrdML, only: modleveldump, ivcoor, ixbase, iybase, ixystp, kadd, &
      klevel, imslp, inprecip, iprod, iprodr, itotcomp, gparam, igrid, igridr, &
      igtype, imodlevel
  USE snaptabML, only: tabcon, nprepro, prepro
  USE particleML, only: pdata, extraParticle
  USE allocateFieldsML, only: allocateFields, deallocateFields
  USE fldout_ncML, only: fldout_nc
  USE rmpartML, only: rmpart
  USE checkdomainML, only: checkdomain
  USE rwalkML, only: rwalk, rwalk_init
  USE vgravtablesML, only: radiusmym, densitygcm3
  USE ensembleML, only: nxep, nyep
  USE milibML, only: xyconvert, keywrd, chcase, getvar, hrdiff, prhelp, vtime, termchar
#if defined(TRAJ)
  USE snapfldML, only: hlevel2
  USE forwrdML, only: forwrd, forwrd_init, speed
  USE snapgrdML, only: vlevel, ivlevel
#else
  USE forwrdML, only: forwrd, forwrd_init
#endif
  USE wetdep, only: wetdep1, wetdep2, wetdep2_init, wetdeprat, kwetdep
  USE drydep, only: drydep1, drydep2, drydeprat, drydephgt, kdrydep
  USE decayML, only: decay, decayDeps, kdecay, halftime, decayrate
  USE posintML, only: posint, posint_init
  USE bldpML, only: bldp
  USE releaseML, only: release, frelhour, relradius, rellower, relupper, &
                       relstemradius, relbqsec, nrelheight, ntprof, mprel, &
                       mplume, nplume, npart, mpart, numtotal
  USE init_random_seedML, only: init_random_seed
  USE compheightML, only: compheight
  USE readfield_ncML, only: readfield_nc
  USE releasefileML, only: releasefile
  USE filesort_ncML, only: filesort_nc
#if defined(MILIB)
  use filesortML, only: filesort
  use fldoutML, only: fldout
  use readfieldML, only: readfield
#else
  USE feltio_dummy, only: readfd, readfield, filesort, fldout
#endif
  USE find_parameter, only: detect_gridparams, get_klevel

  implicit none

!..BATCH output:
!	iexit=0   : ok
!	iexit=1   : end of run
!	iexit=2   : some error, exit
  integer :: iexit

  integer :: allocatestatus
  character(len=*), parameter :: allocateErrorMessage = "*** Not enough memory ***"

  integer ::   itime1(5),itime2(5),itime(5),itimei(5),itimeo(5)
  integer ::   itimefi(5),itimefa(5),itimefo(5,2)

!..used in xyconvert (longitude,latitude -> x,y)
  real, save :: geoparam(6) = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0]

  integer, parameter :: maxkey = 10
  integer ::   kwhere(5,maxkey)

  integer :: narg,iuinp,ios,iprhlp,nlines,nhrun,nhrel,irwalk,nhfout
  integer :: isynoptic,m,np,nlevel,minhfc,maxhfc,ifltim
  integer :: ipostyp,lcinp,iend,ks,nkey,k,ierror,mkey
  integer :: ikey,k1,k2,kv1,kv2,nkv,i,kh,kd,ig,igd,igm,i1,i2,l,n
  integer :: ih
  integer :: idrydep,iwetdep,idecay
  integer :: ntimefo,iunitf,nh1,nh2
  integer :: ierr1,ierr2,nsteph,nstep,nstepr,iunito
  integer :: nxtinf,ihread,isteph,lstepr,iendrel,istep,ihr1,ihr2,nhleft
  integer :: ierr,ihdiff,ihr,ifldout,idailyout,ihour
  integer :: date_time(8)
  real ::    tstep,rmlimit,rnhrun,rnhrel,glat,glong,tf1,tf2,tnow,tnext
  real ::    x(1),y(1)
  type(extraParticle) :: pextra
  real ::    rscale
! ipcount(mdefcomp, nk)
! integer, dimension(:,:), allocatable:: ipcount
! npcount(nk)
! integer, dimension(:), allocatable:: npcount
! b_start
  real :: mhmin, mhmax  ! minimum and maximum of mixing height
! b_end
  integer :: ndefcomp

  logical :: blfullmix
  logical :: init = .TRUE.

  character(len=1024) ::  finput,fldfil,fldfilX,fldfilN,logfile,ftype, &
      fldtype, relfile
  character(len=1024*8) :: cinput,ciname,cipart
  character(len=8) ::   cpos1,cpos2
  character(len=1) ::   tchar
  character(len=1024) :: tempstr

!> name of selected release position
  character(len=40), save :: srelnam

  integer :: experimental_stat

#if defined(TRAJ)
  integer :: timeStart(6), timeCurrent(6)
  integer :: ilvl
  real ::    vlvl

  integer(kind=real64) :: epochSecs
  real :: zzz
! character(len=60) :: tr_file
  integer :: ntraj,itraj
  real :: tlevel(100)
  character(len=80) :: tname(10) ! name for the trajectory
  integer :: tyear, tmon, tday, thour, tmin
  real :: distance
#endif

#if defined(VOLCANO)
!... matrix for calculating volcanic ash concentrations + file name
!   vcon(nx,ny,3)
  real, allocatable :: vcon(:,:,:)
  character(len=60) :: cfile
#endif

#if defined(VOLCANO) || defined(TRAJ)
  integer :: itimev(5),j
#endif


! initialize random number generator for rwalk and release
  CALL init_random_seed()
  iexit=0


!-------------------------------------------------------------------
  narg = command_argument_count()
  if(narg < 1) then
    write(error_unit,*)
    write(error_unit,*) '  usage: snap <snap.input>'
    write(error_unit,*) '     or: snap <snap.input> <arguments>'
    write(error_unit,*) '     or: snap <snap.input> ?     (to get help)'
    write(error_unit,*)
    stop 1
  endif
  call get_command_argument(1, finput)
  if (finput == "--version") then
#if defined(VERSION)
    write(output_unit,*) "snap version: ", VERSION
#else
    write(output_unit,*) "snap version: UNVERSIONED"
#endif
    stop
  endif

  open(newunit=iuinp,file=finput, &
      access='sequential',form='formatted', &
      status='old',iostat=ios,action='read')
  if(ios /= 0) then
    write(error_unit,*) 'Open Error: ', trim(finput)
    error stop 1
  endif

  if(narg == 2) then
    call get_command_argument(2, cinput)
    iprhlp=1
    if(cinput(1:1) == '?') goto 14
    iprhlp=0
  endif


  write(output_unit,*) 'Reading input file:'
  write(output_unit,*)  TRIM(finput)

  nlines=0

!..termination character (machine dependant)
  call termchar(tchar)

!..set release position as not chosen
  irelpos=0

  itime1 = -huge(itime1)
  nhrun  =0
  nhrel  =0
  srelnam='*'

!..default values
  irwalk =1
  tstep  =900.
  mprel  =200
  nhfmin =6
  nhfmax =12
  nhfout =3
  isynoptic=0
  nrelheight=1

  compname = 'Unknown'
  compnamemc = 'Unknown'

  kdrydep = -1
  kwetdep = -1
  kdecay = -1
  drydephgt = -1.0
  drydeprat = -1.0
  wetdeprat = -1.0
  halftime = -1.0
  kgravity = -1
  gravityms = 0.0
  radiusmym = 0.0
  densitygcm3 = 0.0
  idcomp = -1
  iruncomp = 0
  totalbq = 0.0
  numtotal = 0
  ncomp = 0
  ndefcomp = 0
  itotcomp = 0
  rmlimit = -1.0
  nprepro = 0
  itprof = 0

  relradius = -1.0
  relupper = -1.0
  rellower = -1.0
  relstemradius = -1.0
  relbqsec(1,:,:) = -1.0

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
  maxhfc =+huge(maxhfc)
  nfilef =0
  ifltim =0
  blfullmix= .TRUE.
  idrydep=0
  iwetdep=0

  inprecip =1
  imslp    =0
  imodlevel=0
  modleveldump=0.0

  idebug=0
! input type
  ftype='felt'
! output type
  fldtype='felt'
  fldfil= 'snap.dat'
  logfile='snap.log'
  nctitle=''
  ncsummary=''
  relfile='*'
! timestamp of the form 0000-00-00_00:00:00
  call DATE_AND_TIME(VALUES=date_time)
  write (simulation_start, 9999) (date_time(i),i=1,3), &
      (date_time(i),i=5,7)
  9999 FORMAT(I4.4,'-',I2.2,'-',I2.2,'_',I2.2,':',I2.2,':',I2.2)
! input ensemble member, default to no ensembles
  enspos = -1
! ensemble output
  iensemble=0
  ensemblefile='ensemble.list'
  ensembleparticipant=09
  ensembleRandomKey='*******'
  ensembleStepHours=3

  idailyout=0

  iargos=0
  argoshourstep= 6
  argosdepofile= 'xxx_MLDP0_depo'
  argosconcfile= 'xxx_MLDP0_conc'
  argosdosefile= 'xxx_MLDP0_dose'

!..ipostyp=1 : latitude,longitude as decimal numbers (real)
!..ipostyp=2 : latitude,longitude as degree*100+minute (integer)

  ipostyp=1


  lcinp=len(cinput)
  iend=0

  do while (iend == 0)

    nlines=nlines+1
    read(iuinp,fmt='(a)',iostat=ierror) cinput
    if (ierror == IOSTAT_END) goto 18
    if (ierror /= 0) goto 11

    ks=index(cinput,'*')
    if(ks < 1) ks=lcinp+1

    if(ks == 1) then
      nkey=0
    else
      do k=ks,lcinp
        cinput(k:k)=' '
      end do
    !..check if input as environment variables or command line arguments
      call getvar(1,cinput,1,[1],1,ierror)
      if(ierror /= 0) goto 14
    !..find keywords and values
      mkey=maxkey
      call keywrd(1,cinput,'=',';',mkey,kwhere,nkey,ierror)
      if(ierror /= 0) goto 12
    end if

    nkv = 0
    do ikey=1,nkey

    ! c         l=kwhere(1,ikey)
      k1=kwhere(2,ikey)
      k2=kwhere(3,ikey)
      kv1=kwhere(4,ikey)
      kv2=kwhere(5,ikey)

      if(kv1 > 0) then
        nkv=kv2-kv1+1
        ciname=cinput(kv1:kv2)
        cipart=cinput(kv1:kv2)//tchar
      end if

    !=======================================================================

      if(cinput(k1:k2) == 'positions.decimal') then
      !..positions.decimal
        ipostyp=1
      elseif(cinput(k1:k2) == 'positions.degree_minute') then
      !..positions.degree_minute
        ipostyp=2
      elseif(cinput(k1:k2) == 'time.start') then
      !..time.start=<year,month,day,hour>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) (itime1(i),i=1,4)
        itime1(5)=0
      elseif(cinput(k1:k2) == 'time.run') then
      !..time.run=<hours'h'> or <days'd'>
        if(kv1 < 1) goto 12
        kh=index(cipart,'h')
        kd=index(cipart,'d')
        if(kh > 0 .AND. kd == 0) then
          cipart(kh:kh)=' '
          read(cipart,*,err=12) rnhrun
          nhrun=nint(rnhrun)
        elseif(kd > 0 .AND. kh == 0) then
          cipart(kd:kd)=' '
          read(cipart,*,err=12) rnhrun
          nhrun=nint(rnhrun*24.)
        else
          goto 12
        end if
      elseif(cinput(k1:k2) == 'time.release') then
      !..time.release=<hours'h'> or <days'd'>
        if(kv1 < 1) goto 12
        kh=index(cipart,'h')
        kd=index(cipart,'d')
        if(kh > 0 .AND. kd == 0) then
          cipart(kh:kh)=' '
          read(cipart,*,err=12) rnhrel
          nhrel=nint(rnhrel)
        elseif(kd > 0 .AND. kh == 0) then
          cipart(kd:kd)=' '
          read(cipart,*,err=12) rnhrel
          nhrel=nint(rnhrel*24.)
        else
          goto 12
        end if
      elseif(cinput(k1:k2) == 'set_release.pos') then
      !..set_release.pos=<name>   or <p=lat,long>
        if(kv1 < 1) goto 12
        srelnam=ciname
        if(srelnam(1:2) == 'p=' .OR. srelnam(1:2) == 'P=') then
          cipart(1:2)='  '
          read(cipart,*,err=12) glat,glong
          if(ipostyp == 2) then
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
          if(k1 > 0) then
            cpos1(8:8)='N'
          else
            k1=index(cpos1,'-')
            cpos1(8:8)='S'
          end if
          k2=index(cpos2,'+')
          if(k2 > 0) then
            cpos2(8:8)='E'
          else
            k2=index(cpos2,'-')
            cpos2(8:8)='W'
          end if
          srelnam=cpos1(k1+1:8)//' '//cpos2(k2+1:8)
          if(nrelpos < size(release_positions)) nrelpos=nrelpos+1
          release_positions(nrelpos)%name = srelnam
          release_positions(nrelpos)%geo_latitude = glat
          release_positions(nrelpos)%geo_longitude = glong
        end if
      elseif(cinput(k1:k2) == 'random.walk.on') then
      !..random.walk.on
        irwalk=1
      elseif(cinput(k1:k2) == 'random.walk.off') then
      !..random.walk.off
        irwalk=0
      elseif(cinput(k1:k2) == 'boundary.layer.full.mix.off') then
      !..boundary.layer.full.mix.off
        blfullmix= .FALSE.
      elseif(cinput(k1:k2) == 'boundary.layer.full.mix.on') then
      !..boundary.layer.full.mix.on
        blfullmix= .TRUE.
      elseif(cinput(k1:k2) == 'dry.deposition.old') then
      !..dry.deposition.old
        if(idrydep /= 0 .AND. idrydep /= 1) goto 12
        idrydep=1
      elseif(cinput(k1:k2) == 'dry.deposition.new') then
      !..dry.deposition.new
        if(idrydep /= 0 .AND. idrydep /= 2) goto 12
        idrydep=2
      elseif(cinput(k1:k2) == 'wet.deposition.old') then
      !..wet.deposition.old
        if(iwetdep /= 0 .AND. iwetdep /= 1) goto 12
        iwetdep=1
      elseif(cinput(k1:k2) == 'wet.deposition.new') then
      !..wet.deposition.new
        if(iwetdep /= 0 .AND. iwetdep /= 2) goto 12
        iwetdep=2
      elseif(cinput(k1:k2) == 'time.step') then
      !..time.step=<seconds>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) tstep
        if(tstep < 0.9999) goto 12
      elseif(cinput(k1:k2) == 'time.release.profile.constant') then
      !..time.release.profile.constant
        if(itprof /= 0 .AND. itprof /= 1) goto 12
        itprof=1
      elseif(cinput(k1:k2) == 'time.release.profile.bomb') then
      !..time.release.profile.bomb
        if(itprof /= 0 .AND. itprof /= 2) goto 12
        itprof=2
      elseif(cinput(k1:k2) == 'time.release.profile.linear') then
      !..time.release.profile.linear
        if(itprof /= 0 .AND. itprof /= 3) goto 12
        itprof=3
      elseif(cinput(k1:k2) == 'time.release.profile.steps') then
      !..time.release.profile.steps
        if(itprof /= 0 .AND. itprof /= 4) goto 12
        itprof=4
      elseif(cinput(k1:k2) == 'release.day'    .OR. &
          cinput(k1:k2) == 'release.hour'   .OR. &
          cinput(k1:k2) == 'release.minute' .OR. &
          cinput(k1:k2) == 'release.second') then
        rscale=1.
        if(cinput(k1:k2) == 'release.day')    rscale=24.
        if(cinput(k1:k2) == 'release.minute') rscale=1./60.
        if(cinput(k1:k2) == 'release.second') rscale=1./3600.
      !..release.day
      !..release.hour
      !..release.minute
      !..release.second
        if(kv1 < 1 .OR. ntprof > 0) goto 12
        i1=ntprof+1
        i2=ntprof
        ios=0
        do while (ios == 0)
          if(i2 > size(frelhour)) goto 13
          i2=i2+1
          read(cipart,*,iostat=ios) (frelhour(i),i=i1,i2)
        end do
        i2=i2-1
        if(i2 < i1) goto 12
        do i=i1,i2
          frelhour(i)= frelhour(i)*rscale
        end do
        ntprof=i2
        if(frelhour(1) /= 0) goto 12
        do i=2,ntprof
          if(frelhour(i-1) >= frelhour(i)) then
              write(error_unit, *) 'ERROR: Relase hours must be monotonically increasing'
              goto 12
          endif
        end do
      elseif(cinput(k1:k2) == 'release.radius.m') then
      !..release.radius.m
        if(kv1 < 1 .OR. ntprof < 1) goto 12
        read(cipart,*,err=12) (relradius(i,1),i=1,ntprof)
        do i=1,ntprof
          if(relradius(i,1) < 0.) goto 12
        end do
      elseif(cinput(k1:k2) == 'release.upper.m') then
      !..release.upper.m
        if(kv1 < 1 .OR. ntprof < 1) goto 12
        read(cipart,*,err=12) (relupper(i,1),i=1,ntprof)
        do i=1,ntprof
          if(relupper(i,1) < 0.) goto 12
        end do
      elseif(cinput(k1:k2) == 'release.lower.m') then
      !..release.lower.m
        if(kv1 < 1 .OR. ntprof < 1) goto 12
        read(cipart,*,err=12) (rellower(i,1),i=1,ntprof)
        do i=1,ntprof
          if(rellower(i,1) < 0.) goto 12
        end do
      elseif(cinput(k1:k2) == 'release.mushroom.stem.radius.m') then
      !..release.mushroom.stem.radius.m
        if(kv1 < 1 .OR. ntprof < 1) goto 12
        read(cipart,*,err=12) (relstemradius(i),i=1,ntprof)
      elseif(cinput(k1:k2) == 'release.bq/hour.comp' .OR. &
          cinput(k1:k2) == 'release.bq/sec.comp'  .OR. &
          cinput(k1:k2) == 'release.bq/day.comp' .OR. &
          cinput(k1:k2) == 'release.bq/step.comp') then
      !..release.bq/hour.comp
      !..release.bq/sec.comp
      !..release.bq/day.comp
      !..release.bq/step.comp
        if(cinput(k1:k2) == 'release.bq/hour.comp') then
          rscale=1./3600.
        elseif(cinput(k1:k2) == 'release.bq/day.comp') then
          rscale=1./(3600.*24.)
        elseif(cinput(k1:k2) == 'release.bq/step.comp') then
          rscale=-1.
        else
          rscale=1.
        end if
        if(kv1 < 1 .OR. ntprof < 1) goto 12
        ncomp= ncomp+1
        if(ncomp > mcomp) goto 13
        read(cipart,*,err=12) (relbqsec(i,ncomp,1),i=1,ntprof), &
            component(ncomp)
        do i=1,ntprof
          if(relbqsec(i,ncomp,1) < 0.) goto 12
        end do
        if(rscale > 0.) then
          do i=1,ntprof
            relbqsec(i,ncomp,1)= relbqsec(i,ncomp,1)*rscale
          end do
        elseif(ntprof > 1) then
          if(cinput(k1:k2) == 'release.bq/step.comp') then
            if (relbqsec(ntprof,ncomp,1) /= 0) then
              write(error_unit,*) "Last element at line ", nlines, " is not zero"
              write(error_unit,*) "this release will not be included when computing"
              write(error_unit,*) "Consider appending a zero for correct handling of"
              write(error_unit,*) "releases close to the end of the run"
            endif
            do i=1,ntprof-1
              rscale=1./(3600.*(frelhour(i+1)-frelhour(i)))
              relbqsec(i,ncomp,1)= relbqsec(i,ncomp,1)*rscale
            end do
          end if
        end if

      !..releases with different height classes
      !..  height-classes defined here, time-profile defined outside
      !..release.heightlower.m
      elseif(cinput(k1:k2) == 'release.heightlower.m') then
        nrelheight=0
        if(kv1 < 1 .OR. nrelheight > 0) goto 12
        i1=nrelheight+1
        i2=nrelheight
        ios=0
        do while (ios == 0)
          if(i2 > size(rellower, 2)) goto 13
          i2=i2+1
          read(cipart,*,iostat=ios) (rellower(1,ih),ih=i1,i2)
        end do
        i2=i2-1
        if(i2 < i1) goto 12
        nrelheight=i2
      elseif(cinput(k1:k2) == 'release.heightradius.m') then
      !..release.heightradius.m
        if(kv1 < 1 .OR. nrelheight < 1) goto 12
        read(cipart,*,err=12) (relradius(1,ih),ih=1,nrelheight)
        do ih=1,nrelheight
          if(relradius(1,ih) < 0.) goto 12
        end do
      elseif(cinput(k1:k2) == 'release.heightupper.m') then
      !..release.heightupper.m
        if(kv1 < 1 .OR. nrelheight < 1) goto 12
        read(cipart,*,err=12) (relupper(1,ih),ih=1,nrelheight)
        do ih=1,nrelheight
          if(relupper(1,ih) < rellower(1,ih)) goto 12
        end do
      elseif(cinput(k1:k2) == 'release.file') then
      !..release.file
        relfile=ciname(1:nkv)
      !..release.component
      elseif(cinput(k1:k2) == 'release.components') then
        ncomp=0
        if(kv1 < 1 .OR. ncomp > 0) goto 12
        i1=ncomp+1
        i2=ncomp
        ios=0
        do while (ios == 0)
          if(i2 > mcomp) goto 13
          i2=i2+1
          read(cipart,*,iostat=ios) (component(i),i=i1,i2)
        end do
        i2=i2-1
        if(i2 < i1) goto 12
        ncomp=i2

      elseif(cinput(k1:k2) == 'max.totalparticles') then
      !..max.totalparticles
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) mpart
        if(mpart < 1) goto 12
      elseif(cinput(k1:k2) == 'max.totalplumes') then
      !..max.totalplumes
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) mplume
        if(mplume < 1) goto 12
      elseif(cinput(k1:k2) == 'max.particles.per.release') then
      !..max.particles.per.release
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) mprel
        if(mprel < 1) goto 12
      elseif(cinput(k1:k2) == 'component') then
      !..component= name
        if(kv1 < 1) goto 12
        ndefcomp=ndefcomp+1
        if(ndefcomp > mdefcomp) goto 12
        compname(ndefcomp)=ciname(1:nkv)
        compnamemc(ndefcomp)=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'dry.dep.on') then
      !..dry.dep.on
        if(ndefcomp < 1 .OR. kdrydep(ndefcomp) /= -1) goto 12
        kdrydep(ndefcomp)=1
      elseif(cinput(k1:k2) == 'dry.dep.off') then
      !..dry.dep.off
        if(ndefcomp < 1 .OR. kdrydep(ndefcomp) /= -1) goto 12
        kdrydep(ndefcomp)=0
      elseif(cinput(k1:k2) == 'wet.dep.on') then
      !..wet.dep.on
        if(ndefcomp < 1 .OR. kwetdep(ndefcomp) /= -1) goto 12
        kwetdep(ndefcomp)=1
      elseif(cinput(k1:k2) == 'wet.dep.off') then
      !..wet.dep.off
        if(ndefcomp < 1 .OR. kwetdep(ndefcomp) /= -1) goto 12
        kwetdep(ndefcomp)=0
      elseif(cinput(k1:k2) == 'dry.dep.height') then
      !..dry.dep.height=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. drydephgt(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) drydephgt(ndefcomp)
      elseif(cinput(k1:k2) == 'dry.dep.ratio') then
      !..dry.dep.ratio=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. drydeprat(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) drydeprat(ndefcomp)
      elseif(cinput(k1:k2) == 'wet.dep.ratio') then
      !..wet.dep.ratio=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. wetdeprat(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) wetdeprat(ndefcomp)
      elseif(cinput(k1:k2) == 'radioactive.decay.on') then
      !..radioactive.decay.on
        if(ndefcomp < 1 .OR. kdecay(ndefcomp) /= -1) goto 12
        kdecay(ndefcomp)=1
      elseif(cinput(k1:k2) == 'radioactive.decay.off') then
      !..radioactive.decay.off
        if(ndefcomp < 1 .OR. kdecay(ndefcomp) /= -1) goto 12
        kdecay(ndefcomp)=0
      elseif(cinput(k1:k2) == 'half.lifetime.minutes') then
      !..half.lifetime.minutes=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. halftime(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) halftime(ndefcomp)
        halftime(ndefcomp)=halftime(ndefcomp)/60.
      elseif(cinput(k1:k2) == 'half.lifetime.hours') then
      !..half.lifetime.hours=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. halftime(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) halftime(ndefcomp)
      elseif(cinput(k1:k2) == 'half.lifetime.days') then
      !..half.lifetime.days=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. halftime(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) halftime(ndefcomp)
        halftime(ndefcomp)=halftime(ndefcomp)*24.
      elseif(cinput(k1:k2) == 'half.lifetime.years') then
      !..half.lifetime.years=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. halftime(ndefcomp) >= 0.) goto 12
        read(cipart,*,err=12) halftime(ndefcomp)
        halftime(ndefcomp)=halftime(ndefcomp)*24.*365.25
      elseif(cinput(k1:k2) == 'gravity.off') then
      !..gravity.off
        if(ndefcomp < 1 .OR. kgravity(ndefcomp) /= -1) goto 12
        kgravity(ndefcomp)= 0
      elseif(cinput(k1:k2) == 'gravity.fixed.m/s') then
      !..gravity.fixed.m/s
        if(ndefcomp < 1 .OR. kgravity(ndefcomp) /= -1) goto 12
        kgravity(ndefcomp)= 1
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) gravityms(ndefcomp)
        if (gravityms(ndefcomp) <= 0.) goto 12
      elseif(cinput(k1:k2) == 'gravity.fixed.cm/s') then
      !..gravity.fixed.cm/s
        if(ndefcomp < 1 .OR. kgravity(ndefcomp) /= -1) goto 12
        kgravity(ndefcomp)= 1
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) gravityms(ndefcomp)
        if (gravityms(ndefcomp) <= 0.) goto 12
        gravityms(ndefcomp)=gravityms(ndefcomp)*0.01
      elseif(cinput(k1:k2) == 'radius.micrometer') then
      !..radius.micrometer  (for gravity computation)
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. radiusmym(ndefcomp) > 0.) goto 12
        read(cipart,*,err=12) radiusmym(ndefcomp)
        if(radiusmym(ndefcomp) <= 0.) goto 12
      elseif(cinput(k1:k2) == 'density.g/cm3') then
      !..density.g/cm3  (for gravity computation)
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. densitygcm3(ndefcomp) > 0.) goto 12
        read(cipart,*,err=12) densitygcm3(ndefcomp)
        if(densitygcm3(ndefcomp) <= 0.) goto 12
      elseif(cinput(k1:k2) == 'field.identification') then
      !..field.identification=
        if(kv1 < 1) goto 12
        if(ndefcomp < 1 .OR. idcomp(ndefcomp) >= 0) goto 12
        read(cipart,*,err=12) idcomp(ndefcomp)
        if(idcomp(ndefcomp) < 1) goto 12
        do i=1,ndefcomp-1
          if(idcomp(i) == idcomp(ndefcomp)) goto 12
        end do
      elseif (cinput(k1:k2) == 'field.use_model_wind_instead_of_10m') then
        if (kv1 < 1) goto 12
        read(cipart,*,err=12) use_model_wind_for_10m
! FIELD.USE_MODEL_WIND_INSTEAD_OF_10M= [.false.]/.true
      elseif(cinput(k1:k2) == 'precip(mm/h).probab') then
      !..precip(mm/h).probab=<precip_intensity,probability, ...>
        if(kv1 < 1) goto 12
        i1=nprepro+1
        i2=nprepro
        ios=0
        do while (ios == 0)
          if(i2 > size(prepro,2)) goto 12
          i2=i2+1
          read(cipart,*,iostat=ios) &
              ((prepro(k,i),k=1,2),i=i1,i2)
        end do
        i2=i2-1
        if(i2 < i1) goto 12
        nprepro=i2
      elseif(cinput(k1:k2) == 'remove.relative.mass.limit') then
      !..remove.relative.mass.limit=
        if(kv1 < 1) goto 12
        if(rmlimit >= 0.00) goto 12
        read(cipart,*,err=12) rmlimit
        if(rmlimit < 0.0 .OR. rmlimit > 0.5) goto 12
      elseif(cinput(k1:k2) == 'step.hour.input.min') then
      !..step.hour.input.min=<hours>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) nhfmin
      elseif(cinput(k1:k2) == 'step.hour.input.max') then
      !..step.hour.input.max=<hours>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) nhfmax
      elseif(cinput(k1:k2) == 'step.hour.output.fields') then
      !..step.hour.output.fields=<hours>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) nhfout
      elseif(cinput(k1:k2) == 'synoptic.output') then
      !..synoptic.output ... output at synoptic hours
        isynoptic=1
      elseif(cinput(k1:k2) == 'asynoptic.output') then
      !..asynoptic.output ... output at fixed intervals after start
        isynoptic=0
      elseif(cinput(k1:k2) == 'total.components.off') then
      !..total.components.off
        itotcomp=0
      elseif(cinput(k1:k2) == 'total.components.on') then
      !..total.components.on
        itotcomp=1
      elseif(cinput(k1:k2) == 'mslp.on') then
      !..mslp.on
        imslp=1
      elseif(cinput(k1:k2) == 'mslp.off') then
      !..mslp.off
        imslp=0
      elseif(cinput(k1:k2) == 'precipitation.on') then
      !..precipitation.on
        inprecip=1
      elseif(cinput(k1:k2) == 'precipitation.off') then
      !..precipitation.off
        inprecip=0
      elseif(cinput(k1:k2) == 'model.level.fields.on') then
      !..model.level.fields.on
        imodlevel=1
      elseif(cinput(k1:k2) == 'model.level.fields.off') then
      !..model.level.fields.off
        imodlevel=0
      elseif(cinput(k1:k2) == 'model.level.fields.dumptime') then
      !..levelfields are dump-data
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) modleveldump
      elseif(cinput(k1:k2) == 'release.pos') then
      !..release.pos=<'name',latitude,longitude>
        if(kv1 < 1) goto 12
        if(nrelpos < size(release_positions)) then
          nrelpos=nrelpos+1
          read(cipart,*,err=12) release_positions(nrelpos)%name, &
              release_positions(nrelpos)%geo_latitude, &
              release_positions(nrelpos)%geo_longitude
          if(ipostyp == 2) then
            ig = nint(release_positions(nrelpos)%geo_latitude)
            igd = ig/100
            igm = ig - igd*100
            release_positions(nrelpos)%geo_latitude = &
                float(igd) + float(igm)/60.
            ig = nint(release_positions(nrelpos)%geo_longitude)
            igd = ig/100
            igm = ig - igd*100
            release_positions(nrelpos)%geo_longitude = &
                float(igd) + float(igm)/60.
          end if
        else
          write(error_unit,*) 'WARNING. Too many RELEASE POSITIONS'
          write(error_unit,*) '  ==> ',cinput(kv1:kv2)
        end if
      elseif(cinput(k1:k2) == 'grid.input') then
      !..grid.input=<producer,grid>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) iprod,igrid
      elseif(cinput(k1:k2) == 'grid.nctype') then
      !..grid.nctype=<emep/hirlam12>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) nctype
      elseif(cinput(k1:k2) == 'grid.size') then
      !..grid.size=<nx,ny>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) nx,ny
      elseif(cinput(k1:k2) == 'grid.gparam') then
      !..grid.gparam=<igtype,gparam(6)>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) igtype,(gparam(i),i=1,6)
      elseif(cinput(k1:k2) == 'grid.run') then
      !..grid.run=<producer,grid,ixbase,iybase,ixystp>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) iprodr,igridr,ixbase,iybase,ixystp
      elseif(cinput(k1:k2) == 'ensemble_member.input') then
        read(cipart,*,err=12) enspos
      elseif(cinput(k1:k2) == 'data.sigma.levels') then
      !..data.sigma.levels
        ivcoor=2
      elseif(cinput(k1:k2) == 'data.eta.levels') then
      !..data.eta.levels
        ivcoor=10
      elseif(cinput(k1:k2) == 'levels.input') then
      !..levels.input=<num_levels, 0,kk,k,k,k,....,1>
      !..levels.input=<num_levels, 0,kk,k,k,k,....,18,0,0,...>
        if(nlevel /= 0) goto 12
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) nlevel
        nk = nlevel
        ALLOCATE ( klevel(nk), STAT = AllocateStatus)
        IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage
!       ALLOCATE ( ipcount(mdefcomp, nk), STAT = AllocateStatus)
!       IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage
!       ALLOCATE ( npcount(nk), STAT = AllocateStatus)
!       IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage

        read(cipart,*,err=12) nlevel,(klevel(i),i=1,nlevel)
        if(klevel(1) /= 0 .OR. klevel(2) == 0) goto 12
        kadd = count(klevel(2:nk) == 0)
        do i=nk-kadd-1,2,-1
          if(klevel(i) <= klevel(i+1)) goto 12
        end do
      elseif(cinput(k1:k2) == 'forecast.hour.min') then
      !..forecast.hour.min= +6
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) minhfc
      elseif(cinput(k1:k2) == 'forecast.hour.max') then
      !..forecast.hour.max= +32767
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) maxhfc
      elseif(cinput(k1:k2) == 'field.type') then
      !..field.type felt or netcdf
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) ftype
      elseif(cinput(k1:k2) == 'field.input') then
      !..field.input=  felt_file_name
        if(kv1 < 1) goto 12
        if(nfilef < size(limfcf,2)) then
          nfilef=nfilef+1
          limfcf(1,nfilef)=minhfc
          limfcf(2,nfilef)=maxhfc
          if(ciname(1:1) == '''' .OR. ciname(1:1) == '"') then
            read(cipart,*,err=12) filef(nfilef)
          else
            filef(nfilef)=ciname(1:nkv)
          end if
        else
          write(error_unit,*) 'WARNING. Too many FIELD INPUT files'
          write(error_unit,*) '  ==> ',cinput(kv1:kv2)
        end if
      elseif(cinput(k1:k2) == 'field_time.forecast') then
      !..field_time.forecast ... use forecast length in output field ident.
        ifltim=0
      elseif(cinput(k1:k2) == 'field_time.valid') then
      !..field_time.valid ...... use valid time in output field identification
        ifltim=1
      elseif(cinput(k1:k2) == 'field.output') then
      !..field.output= <'file_name'>
        if(kv1 < 1) goto 12
        fldfil=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'field.outtype') then
      !..field.outtype= <'felt|netcdf'>
        if(kv1 < 1) goto 12
        fldtype=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'title') then
        nctitle=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'field.daily.output.on') then
        idailyout = 1
      elseif(cinput(k1:k2) == 'field.daily.output.off') then
        idailyout = 0
      elseif(cinput(k1:k2) == 'simulation.start.date') then
        simulation_start = ciname(1:nkv)
      elseif(cinput(k1:k2) == 'log.file') then
      !..log.file= <'log_file_name'>
        if(kv1 < 1) goto 12
        logfile=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'debug.off') then
      !..debug.off
        idebug=0
      elseif(cinput(k1:k2) == 'debug.on') then
      !..debug.on
        idebug=1
      elseif(cinput(k1:k2) == 'ensemble.project.output.off') then
      !..ensemble.project.output.off
        iensemble=0
      elseif(cinput(k1:k2) == 'ensemble.project.output.on') then
      !..ensemble.project.output.on
        iensemble=1
      elseif(cinput(k1:k2) == 'ensemble.project.output.file') then
      !..ensemble.project.output.file= <ensemble.list>
        if(kv1 < 1) goto 12
        ensemblefile=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'ensemble.project.participant') then
      !..ensemble.project.participant= 09
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) ensembleparticipant
        if(ensembleparticipant < 0 .OR. &
            ensembleparticipant > 99) goto 12
      elseif(cinput(k1:k2) == 'ensemble.project.random.key') then
      !..ensemble.project.random.key= <rl52s3u>
        if(kv1 < 1) goto 12
        ensembleRandomKey=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'ensemble.project.output.step.hour') then
      !..ensemble.project.output.step.hour= 3
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) ensembleStepHours
      elseif(cinput(k1:k2) == 'argos.output.off') then
      !..argos.output.off
        iargos=0
      elseif(cinput(k1:k2) == 'argos.output.on') then
      !..argos.output.on
        iargos=1
      elseif(cinput(k1:k2) == 'argos.output.deposition.file') then
      !..argos.output.deposition.file= runident_MLDP0_depo
        if(kv1 < 1) goto 12
        argosdepofile=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'argos.output.concentration.file') then
      !..argos.output.concentration.file= runident_MLDP0_conc
        if(kv1 < 1) goto 12
        argosconcfile=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'argos.output.totaldose.file') then
      !..argos.output.totaldose.file= runident_MLDP0_dose
        if(kv1 < 1) goto 12
        argosdosefile=ciname(1:nkv)
      elseif(cinput(k1:k2) == 'argos.output.timestep.hour') then
      !..argos.output.timestep.hour= <3>
        if(kv1 < 1) goto 12
        read(cipart,*,err=12) argoshourstep
        if(argoshourstep <= 0 .OR. argoshourstep > 240) goto 12
      elseif(cinput(k1:k2) == 'experimental.autodetect.from_input') then
        if (nfilef < 1) then
          error stop "experimental.autodetect requires at least one field.input to be set"
        endif
        call detect_gridparams(filef(1), nx, ny, igtype, gparam, experimental_stat)
        if (experimental_stat /= 0) then
          write(error_unit, *) "Autodetection did not work, continuing"
          write(iulog, *) "Autodetection did not work, continuing"
        endif
        call get_klevel(filef(1), klevel, experimental_stat)
        if (experimental_stat /= 0) then
          write(error_unit, *) "Autodetection did not work, continuing"
          write(iulog, *) "Autodetection did not work, continuing"
        endif
        nlevel = size(klevel)
        nk = nlevel
      elseif(cinput(k1:k2) == 'end') then
      !..end
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
        write(error_unit,*) 'ERROR.  Unknown input:'
        write(error_unit,*) cinput
        goto 12
      end if

    end do

  end do

  goto 18

  11 write(error_unit,*) 'ERROR reading file: ', trim(finput)
  write(error_unit,*) 'At line no. ',nlines
  iexit=2
  goto 18

  12 write(error_unit,*) 'ERROR reading file: ', trim(finput)
  write(error_unit,*) 'At line no. ',nlines,' :'
  write(error_unit,*)  trim(cinput)
  iexit=2
  goto 18

  13 write(error_unit,*) 'ERROR reading file:'
  write(error_unit,*)  finput
  write(error_unit,*) 'At line no. ',nlines,' :'
  write(error_unit,*)  trim(cinput)
  write(error_unit,*) 'SOME LIMIT WAS EXCEEDED !!!!!!!!!!!!!!!!!'
  iexit=2
  goto 18

  14 l=index(finput,' ')-1
  if(l < 1) l=len(finput)
  write(error_unit,*) 'Help from ',finput(1:l),' :'
  call prhelp(iuinp,'*=>')
  iexit=2
  goto 18

  18 close(iuinp)

  if (iexit /= 0) then
    error stop 1
  end if

  write(*,*) "SIMULATION_START_DATE: ", simulation_start


  if (relfile /= '*') then
    call releasefile(relfile)
  end if
! initialize all arrays after reading input
  if (imodlevel == 1) then
    nxmc = nx
    nymc = ny
  else
    nxmc = 1
    nymc = 1
  end if
  maxsiz = nx*ny
  ldata=20+maxsiz+50
  CALL allocateFields()


!..convert names to uppercase letters
  call chcase(2,1,srelnam)
  do n=1,nrelpos
    call chcase(2, 1, release_positions(n)%name)
  end do
  if(ndefcomp > 0) call chcase(2,ndefcomp,compname)
  if(ncomp > 0)    call chcase(2,ncomp,component)

  ierror=0
  do n=1,nrelpos
    if(srelnam == release_positions(n)%name) irelpos=n
  end do
  if(irelpos == 0 .AND. nrelpos == 1) irelpos=1
  if(irelpos == 0) then
    write(error_unit,*) 'No (known) release position selected'
    ierror=1
  end if

  if(ivcoor == 0) then
    write(error_unit,*) 'Input model level type (sigma,eta) not specified'
    ierror=1
  end if
  if(nlevel == 0) then
    write(error_unit,*) 'Input model levels not specified'
    ierror=1
  end if
  if(ftype /= "felt" .AND. ftype /= "netcdf") then
    write(error_unit,*) 'Input type not felt or netcdf:', ftype
    ierror=1
  end if
  if(ftype /= "felt" .AND. ftype /= "netcdf") then
    write(error_unit,*) 'Output type not felt or netcdf:', ftype
    ierror=1
  end if
  if(nfilef == 0) then
    write(error_unit,*) 'No input field files specified'
    ierror=1
  end if

  if(nhrel == 0 .AND. ntprof > 0) nhrel=frelhour(ntprof)

!..check if compiled without ENSEMBLE PROJECT arrays
  if(nxep < 2 .OR. nyep < 2) iensemble=0

  if(ntprof < 1) then
    write(error_unit,*) 'No time profile(s) specified'
    ierror=1
  end if
  if(itprof < 1) then
    write(error_unit,*) 'No time profile type specified'
    ierror=1
  end if
  k=0
  do i=1,ntprof
    if(relradius(i,1) < 0. .OR. &
        relupper(i,1) < 0. .OR. &
        rellower(i,1) < 0. .OR. &
        relupper(i,1) < rellower(i,1)) then
      k=1
    endif
    if(relupper(i,1) < rellower(i,1)+1.) then
      relupper(i,1)=rellower(i,1)+1.
    endif
  end do
  if(k == 1) then
    write(error_unit,*) 'ERROR in relase profiles ', &
        &'of upper,lower and/or radius'
    ierror=1
  end if

  if(ncomp < 0) then
    write(error_unit,*) 'No (release) components specified for run'
    ierror=1
  end if
  if(ndefcomp < 0) then
    write(error_unit,*) 'No (release) components defined'
    ierror=1
  end if
  if (ncomp > ndefcomp) then
    write(error_unit,*) "Number of RELEASE.BQ components is higher than the"
    write(error_unit,*) "number of defined components"
    ierror = 1
  end if
  if (maxval(running_to_defined_comp) > ncomp) then
    write(error_unit,*) "Field identification is higher than total number of fields"
    ierror = 1
  end if

  do m=1,ndefcomp-1
    if(idcomp(m) < 1) then
      write(error_unit,*) 'Component has no field identification: ', &
          trim(compname(m))
    end if
    do i=m+1,ndefcomp
      if(compname(m) == compname(i)) then
        write(error_unit,*) 'Component defined more than once: ', &
            trim(compname(m))
        ierror=1
      end if
    end do
  end do

  do m=1,ncomp-1
    do i=m+1,ncomp
      if(component(m) == component(i)) then
        write(error_unit,*) 'Released component defined more than once: ', &
            trim(component(m))
        ierror=1
      end if
    end do
  end do

!..match used components with defined components
  do m=1,ncomp
    k=0
    do i=1,ndefcomp
      if(component(m) == compname(i)) k=i
    end do
    if(k > 0) then
      running_to_defined_comp(m)= k
      iruncomp(k)= m
    else
      write(error_unit,*) 'Released component ', &
          trim(component(m)), ' is not defined'
      ierror=1
    end if
  end do

!..gravity
  do n=1,ncomp
    m=running_to_defined_comp(n)
    if(kgravity(m) < 0) kgravity(m)= 2
    if(kgravity(m) == 2 .AND. &
        (radiusmym(m) <= 0. .OR. densitygcm3(m) <= 0.)) then
      write(error_unit,*) 'Gravity error. radius,density: ', &
          radiusmym(m),densitygcm3(m)
      ierror=1
    end if
  end do

  if(idrydep == 0) idrydep=1
  if(iwetdep == 0) iwetdep=1
  i1=0
  i2=0
  idecay=0

  do n=1,ncomp
    m=running_to_defined_comp(n)
    if(idrydep == 1 .AND. kdrydep(m) == 1) then
      if(drydeprat(m) > 0. .AND. drydephgt(m) > 0.) then
        i1=i1+1
      else
        write(error_unit,*) 'Dry deposition error. rate,height: ', &
            drydeprat(m),drydephgt(m)
        ierror=1
      end if
    elseif(idrydep == 2 .AND. kdrydep(m) == 1) then
      if(kgravity(m) == 1 .AND. gravityms(m) > 0.) then
        i1=i1+1
      elseif(kgravity(m) == 2) then
        i1=i1+1
      else
        write(error_unit,*) 'Dry deposition error. gravity: ', &
            gravityms(m)
        ierror=1
      end if
    end if

    if(iwetdep == 1 .AND. kwetdep(m) == 1) then
      if(wetdeprat(m) > 0.) then
        i2=i2+1
      else
        write(error_unit,*) 'Wet deposition error. rate: ', &
            wetdeprat(m)
        ierror=1
      end if
    elseif(iwetdep == 2 .AND. kwetdep(m) == 1) then
      if(radiusmym(m) > 0.) then
        i2=i2+1
      else
        write(error_unit,*) 'Wet deposition error. radius: ', &
            radiusmym(m)
        ierror=1
      end if
    end if

    if(kdecay(m) == 1 .AND. halftime(m) > 0.) then
      idecay=1
    else
      kdecay(m)=0
      halftime(m)=0.
    end if
  end do

  if(i1 == 0) idrydep=0
  if(i2 == 0) iwetdep=0

  if(ierror /= 0) then
    error stop 1
  end if

  if(itotcomp == 1 .AND. ncomp == 1) itotcomp=0

  if(rmlimit < 0.0) rmlimit=0.0001

  if(iprodr == 0) iprodr=iprod
  if(igridr == 0) igridr=igrid

  call init_meteo_params()



  write(output_unit,*) 'Input o.k.'
!-------------------------------------------------------------------

!..log file
  open(newunit=iulog, file=logfile, &
      access='sequential', form='formatted', &
      status='replace', action='write')

  ntimefo=0

!..define fixed tables and constants (independant of input data)
  call tabcon

!..file unit for all input field files
  iunitf=20

!..check input FELT files and make sorted lists of available data
!..make main list based on x wind comp. (u) in upper used level
  if (ftype == "netcdf") then
    call filesort_nc ! (iunitf, ierror)
  else
    call filesort(iunitf,ierror)
  end if
  if(ierror /= 0) call snap_error_exit()

!..itime: itime(1) - year
!         itime(2) - month
!         itime(3) - day
!         itime(4) - hour
!         itime(5) - forecast time in hours (added to date/time above)

!..itime1: start time
!..itime2: stop  time

  call vtime(itime1,ierror)
  if(ierror /= 0) then
    write(iulog,*) 'Requested start time is wrong:'
    write(iulog,*) (itime1(i),i=1,4)
    write(error_unit,*) 'Requested start time is wrong:'
    write(error_unit,*) (itime1(i),i=1,4)
    call snap_error_exit()
  end if

  itime2 = itime1
  itime2(5)=itime2(5)+nhrun
  call vtime(itime2,ierror)

  call hrdiff(0,0,itimer(1,1),itime1,nh1,ierr1,ierr2)
  call hrdiff(0,0,itime2,itimer(1,2),nh2,ierr1,ierr2)
  if(nh1 < 0 .OR. nh2 < 0) then
    write(iulog,*) 'Not able to run requested time periode.'
    write(iulog,*) 'Start:        ',(itime1(i),i=1,4)
    write(iulog,*) 'End:          ',(itime2(i),i=1,4)
    write(iulog,*) 'First fields: ',(itimer(i,1),i=1,4)
    write(iulog,*) 'Last  fields: ',(itimer(i,2),i=1,4)
    write(error_unit,*) 'Not able to run requested time periode.'
    write(error_unit,*) 'Start:        ',(itime1(i),i=1,4)
    write(error_unit,*) 'End:          ',(itime2(i),i=1,4)
    write(error_unit,*) 'First fields: ',(itimer(i,1),i=1,4)
    write(error_unit,*) 'Last  fields: ',(itimer(i,2),i=1,4)
    if(nh1 < 0) then
      write(iulog,*) 'NO DATA AT START OF RUN'
      write(error_unit,*) 'NO DATA AT START OF RUN'
      call snap_error_exit()
    end if
    write(iulog,*) 'Running until end of data'
    write(error_unit,*) 'Running until end of data'
    do i=1,5
      itime2(i)=itimer(i,2)
    end do
    call hrdiff(0,0,itime1,itime2,nhrun,ierr1,ierr2)
  end if

  if(nhrel > abs(nhrun)) nhrel=abs(nhrun)

  if(iargos == 1) then
    argoshoursrelease= nhrel
    argoshoursrun=     nhrun
  !..the following done to avoid updateing subr. fldout............
    nhfout= argoshourstep
    isynoptic= 0
  !................................................................
  end if
#if defined(TRAJ)
  do itraj=1,ntraj
    rellower(1,1)=tlevel(itraj)
  !	relupper(1)=rellower(1)+1
    relupper(1,1)=rellower(1,1)
    tyear=itime1(1)
    tmon=itime1(2)
    tday=itime1(3)
    thour=itime1(4)
    tmin=0.0
    write(*,*) 'lower, upper',rellower(1,1),relupper(1,1)
    write(*,*) 'tyear, tmon, tday, thour, tmin', &
        tyear, tmon, tday, thour, tmin
    distance=0.0
    speed=0.0
    iprecip=1
#endif

  !..initial no. of plumes and particles
    nplume=0
    npart=0
    nparnum=0

  !..no. of timesteps per hour (adjust the timestep)
    nsteph=nint(3600./tstep)
    tstep=3600./float(nsteph)
  !..convert modleveldump from hours to steps
    modleveldump=modleveldump * nsteph

  !..total no. of timesteps to run (nhrun is no. of hours to run)
    nstep=nsteph*nhrun
    if (nstep < 0) nstep = -nstep

  !..total no. of timesteps to release particles
    nstepr=nsteph*nhrel

  !..nuclear bomb case
    if(itprof == 2) nstepr=1

  !..field output file unit
    iunito=30

  !..information to log file
    write(iulog,*) 'nx,ny,nk:  ',nx,ny,nk
  !      write(iulog,*) 'nxad,nyad: ',nxad,nyad
    write(iulog,*) 'nxmc,nymc: ',nxmc,nymc
    write(iulog,*) 'kadd:      ',kadd
    write(iulog,*) 'klevel:'
    write(iulog,*) (klevel(i),i=1,nk)
    write(iulog,*) 'imslp:     ',imslp
    write(iulog,*) 'inprecip:  ',inprecip
    write(iulog,*) 'imodlevel: ',imodlevel
    write(iulog,*) 'modleveldump (h), steps:', modleveldump/nsteph, &
        modleveldump
    write(iulog,*) 'itime1:  ',(itime1(i),i=1,5)
    write(tempstr, '("Starttime: ",I4,"-",I2.2,"-",I2.2,"T",I2.2 &
        &,":",I2.2)') (itime1(i),i=1,5)
    ncsummary = trim(ncsummary) // " " // trim(tempstr)
    do n=1,nrelpos
      write(tempstr, '("Release Pos (lat, lon): (", F5.1, ",", F6.1 &
          &,")")') &
          release_positions(n)%geo_latitude, &
          release_positions(n)%geo_longitude
      ncsummary = trim(ncsummary) // ". " // trim(tempstr)
    end do

    write(iulog,*) 'itime2:  ',(itime2(i),i=1,5)
    write(iulog,*) 'itimer1: ',(itimer(i,1),i=1,5)
    write(iulog,*) 'itimer2: ',(itimer(i,2),i=1,5)
    write(iulog,*) 'nhfmin:  ',nhfmin
    write(iulog,*) 'nhfmax:  ',nhfmax
    write(iulog,*) 'nhrun:   ',nhrun
    write(iulog,*) 'nhrel:   ',nhrel
    write(iulog,*) 'tstep:   ',tstep
    write(iulog,*) 'nsteph:  ',nsteph
    write(iulog,*) 'nstep:   ',nstep
    write(iulog,*) 'nstepr:  ',nstepr
    write(iulog,*) 'mprel:   ',mprel
    write(iulog,*) 'ifltim:  ',ifltim
    write(iulog,*) 'irwalk:  ',irwalk
    write(iulog,*) 'idrydep: ',idrydep
    write(iulog,*) 'iwetdep: ',iwetdep
    write(iulog,*) 'idecay:  ',idecay
    write(iulog,*) 'rmlimit: ',rmlimit
    write(iulog,*) 'ndefcomp:',ndefcomp
    write(iulog,*) 'ncomp:   ',ncomp
    write(iulog,fmt='(1x,a,40(1x,i2))') 'running_to_defined_comp: ', &
        (running_to_defined_comp(i),i=1,ncomp)
    write(iulog,fmt='(1x,a,40(1x,i2))') 'iruncomp: ', &
        (iruncomp(i),i=1,ndefcomp)
    do n=1,ncomp
      m=running_to_defined_comp(n)
      write(iulog,*) 'component no:  ',n
      write(iulog,*) 'compname:   ',compname(m)
      write(iulog,*) '  field id:   ',idcomp(m)
      write(iulog,*) '  kdrydep:    ',kdrydep(m)
      write(iulog,*) '  drydephgt:  ',drydephgt(m)
      write(iulog,*) '  drydeprat:  ',drydeprat(m)
      write(iulog,*) '  kwetdep:    ',kwetdep(m)
      write(iulog,*) '  wetdeprat:  ',wetdeprat(m)
      write(iulog,*) '  kdecay:     ',kdecay(m)
      write(iulog,*) '  halftime:   ',halftime(m)
      write(iulog,*) '  decayrate:  ',decayrate(m)
      write(iulog,*) '  kgravity:   ',kgravity(m)
      write(iulog,*) '  gravityms:  ',gravityms(m)
      write(iulog,*) '  radiusmym:  ',radiusmym(m)
      write(iulog,*) '  densitygcm3:',densitygcm3(m)
      write(iulog,*) '  Relase time profile:   ntprof: ',ntprof
      ncsummary = trim(ncsummary) // ". Release " // trim(compname(m)) &
          // " (hour, Bq/s): "
      do i=1,ntprof
        write(iulog,*) '  hour,Bq/hour: ', &
            frelhour(i),(relbqsec(i,n,ih)*3600.,ih=1,nrelheight)
        write(tempstr, '("(",f5.1,",",ES9.2,")")') &
            frelhour(i), relbqsec(i,n,1)
        ncsummary = trim(ncsummary) // " " // trim(tempstr)
      end do
    end do
    write(iulog,*) 'itotcomp:   ',itotcomp
    write(iulog,*) 'nprepro:    ',nprepro
    write(iulog,*) 'iensemble:  ',iensemble
    write(iulog,*) 'iargos:     ',iargos
    write(iulog,*) 'blfulmix:   ',blfullmix
    write(*,*) 'Title:      ', trim(nctitle)
    write(*,*) 'Summary:    ', trim(ncsummary)


  !..initialize files, deposition fields etc.
    m=0
    nargos=0
    do n=1,abs(nhrun)
      do i=1,4
        itime(i)=itime1(i)
      end do
      if (nhrun > 0) then
        itime(5)=n
      else
        itime(5)=-n
      endif
      if(isynoptic == 0) then
      !..asynoptic output (use forecast length in hours to test if output)
        ihour=itime(5)
      else
      !..synoptic output  (use valid hour to test if output)
        call vtime(itime,ierror)
        ihour=itime(4)
      end if
      if(mod(ihour,nhfout) == 0) m=m+1
      if(iargos == 1) then
        if(mod(n,argoshourstep) == 0) then
          if(nargos < margos) then
            nargos=nargos+1
            do i=1,4
              argostime(i,nargos)=itime1(i)
            end do
            argostime(5,nargos)=n
          end if
        end if
      end if
    end do
    if (idailyout == 1) then
    !       daily output, append +x for each day, but initialize later
      write(fldfilX,'(a9,a1,I3.3)') fldfil, '+', -1
    end if
  ! standard output needs to be initialized, even for daily
    if (fldtype == "netcdf") then
      call fldout_nc(-1,iunito,fldfil,itime1,0.,0.,0.,tstep, &
          m,nsteph,ierror)
    else
      call fldout(-1,iunito,fldfil,itime1,0.,0.,0.,tstep, &
          m,nsteph,ierror)
    endif
    if(ierror /= 0) call snap_error_exit()

    itime = itime1
    itimefi = 0
    itimefa = 0
    itimefo = 0

    nxtinf=0
    ihread=0
    isteph=0
    lstepr=0
    iendrel=0

    istep=-1

#if defined(TRAJ)
  !	write(*,*) (itime(i),i=1,5)
    itimev = itime
  !	write(*,*) (itimev(i),i=1,5)
  ! 1110 continue
  !	write(tr_file,'(''Trajectory_'',i3.3,
  !     &  ''_'',i4,3i2.2,''0000.DAT'')') itraj,(itime(i),i=1,4)
  !	open(13,file=tr_file)
    open(13,file=tname(itraj))
    rewind 13
  !	write(*,*) tr_file
    write(*,*) tname(itraj)

  !	write(13,'(i6,3f12.3)') nstep

#endif

  ! b_start
    mhmin=10000.0
    mhmax=-10.0
  ! b_end

#if defined(VOLCANO)
  ! b 01.05 initialize concentration (mass) matrix

    ALLOCATE( vcon(nx,ny,3), STAT = AllocateStatus )
    IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage
    vcon = 0.0
  ! b END
#endif


  ! reset readfield_nc (eventually, traj will rerun this loop)
    if (ftype == "netcdf") &
        call readfield_nc(iunitf,-1,nhleft,itimei,ihr1,ihr2, &
            itimefi,ierror)
  ! start time loop
    time_loop: do istep=0,nstep

      write(iulog,*) 'istep,nplume,npart: ',istep,nplume,npart
      flush(iulog)
      if(mod(istep,nsteph) == 0) then
        write(error_unit,*) 'istep,nplume,npart: ',istep,nplume,npart
        flush(error_unit)
      end if

    !#######################################################################
    !..test print: printing all particles in plume 'jpl'
    !       write(88,*) 'step,plume,part: ',istep,nplume,npart
    !       jpl=1
    !       if(jpl.le.nplume .and. iplume(1,jpl).gt.0) then
    !         do n=iplume(1,jpl),iplume(2,jpl)
    !           write(88,fmt='(1x,i6,2f7.2,2f7.4,f6.0,f7.3,i4,f8.3)')
    !    +                  iparnum(n),(pdata(i,n),i=1,5),pdata(n)%prc,
    !    +                  icomp(n),pdata(n)%rad
    !         end do
    !       end if
    !#######################################################################

      if(istep == nxtinf) then

      !..read fields
        if(istep == 0) then
          itimei = itime1
          ihr1=-0
          ihr2=-nhfmax
          nhleft=nhrun
        else
          itimei = itimefi
          ihr1=+nhfmin
          ihr2=+nhfmax
          nhleft=(nstep-istep+1)/nsteph
          if (nhrun < 0) nhleft=-nhleft
        end if
      !          write (*,*) "readfield(", iunitf, istep, nhleft, itimei, ihr1
      !     +          ,ihr2, itimefi, ierror, ")"
        if (ftype == "netcdf") then
          call readfield_nc(iunitf,istep,nhleft,itimei,ihr1,ihr2, &
              itimefi,ierror)
        else
          call readfield(iunitf,istep,nhleft,itimei,ihr1,ihr2, &
              itimefi,ierror)
        end if
        if (idebug >= 1) then
          write(iulog,*) "igtype, gparam(8): ", igtype, gparam
        end if
      !          write (*,*) "readfield(", iunitf, istep, nhleft, itimei, ihr1
      !     +          ,ihr2, itimefi, ierror, ")"
        if(ierror /= 0) call snap_error_exit()

      !..analysis time of input model
        if(itimefi(5) <= +6) then
          do i=1,4
            itimefa(i)=itimefi(i)
          end do
          itimefa(5)=0
        end if

        n=itimefi(5)
        call vtime(itimefi,ierr)
        write(error_unit,fmt='(''input data: '',i4,3i3.2,''  prog='',i4)') &
            (itimefi(i),i=1,4),n

      !..compute model level heights
        call compheight

      !..calculate boundary layer (top and height)
        call bldp

        if(istep == 0) then

        !..release position from geographic to polarstereographic coordinates
          y = release_positions(irelpos)%geo_latitude
          x = release_positions(irelpos)%geo_longitude
          write(iulog,*) 'release lat,long: ',y,x
#if defined(TRAJ)
        !	write(*,*) istep,x,y,rellower(1)
        !	write(13,'(i6,3f12.3)') istep,x,y,rellower(1)

          write(13,'(''RIMPUFF'')')
          write(13,'(i2)') ntraj
          write(13,'(1x,i4,4i2.2,''00'', &
              &   2f9.3,f12.3,f15.2,f10.2)') &
              (itime(i),i=1,4),0,y,x,rellower(1,1), &
              distance,speed
          write(*,'(i4,1x,i4,i2,i2,2i2.2,''00'', &
              &   2f9.3,f12.3,f15.2,f10.2)') istep, &
              (itime(i),i=1,4),0,y,x,rellower(1,1), &
              distance,speed
#endif
          call xyconvert(1,x,y,2,geoparam,igtype,gparam,ierror)
          if(ierror /= 0) then
            write(iulog,*) 'ERROR: xyconvert'
            write(iulog,*) '   igtype: ',igtype
            write(iulog,*) '   gparam: ',gparam
            write(error_unit,*) 'ERROR: xyconvert'
            write(error_unit,*) '   igtype: ',igtype
            write(error_unit,*) '   gparam: ',gparam
            call snap_error_exit()
          end if
          write(iulog,*) 'release   x,y:    ',x,y
          if(x(1) < 1.01 .OR. x(1) > nx-0.01 .OR. &
              y(1) < 1.01 .OR. y(1) > ny-0.01) then
            write(iulog,*) 'ERROR: Release position outside field area'
            write(error_unit,*) 'ERROR: Release position outside field area'
            call snap_error_exit()
          end if
          release_positions(irelpos)%grid_x = x(1)
          release_positions(irelpos)%grid_y = y(1)

        !            if(iensemble.eq.1)
        !     +        call ensemble(0,itime1,tf1,tf2,tnow,istep,nstep,nsteph,0)

          nxtinf=1
          ifldout=0
        ! continue istep loop after initialization
          cycle time_loop
        end if

      !          if(iensemble.eq.1)
      !     +      call ensemble(1,itime,tf1,tf2,tnow,istep,nstep,nsteph,0)

        call hrdiff(0,0,itimei,itimefi,ihdiff,ierr1,ierr2)
        tf1=0.
        tf2=3600.*ihdiff
        if (nhrun < 0) tf2=-tf2
        if(istep == 1) then
          call hrdiff(0,0,itimei,itime1,ihr,ierr1,ierr2)
          tnow=3600.*ihr
          nxtinf=istep+nsteph*abs(ihdiff-ihr)
          iprecip=1+ihr
        !              backward calculations difficult, but precip does not matter there
          if (ihr < 0) iprecip = 1
        else
          tnow=0.
          nxtinf=istep+nsteph*abs(ihdiff)
          iprecip=1
        end if

      else

        tnow=tnow+tstep

      end if

      tnext=tnow+tstep

      if(iendrel == 0 .AND. istep <= nstepr) then

      !..release one plume of particles

        call release(istep-1,nsteph,tf1,tf2,tnow,ierror)

        if(ierror == 0) then
          lstepr=istep
        else
          write(iulog,*) 'WARNING. Out of space for plumes/particles'
          write(iulog,*) 'WARNING. End release, continue running'
          write(error_unit,*) 'WARNING. Out of space for plumes/particles'
          write(error_unit,*) 'WARNING. End release, continue running'
          iendrel=1
        end if

      end if

    !#############################################################
    !     write(error_unit,*) 'tf1,tf2,tnow,tnext,tstep,ipr: ',
    !    +		  tf1,tf2,tnow,tnext,tstep,iprecip
    !     write(iulog,*) 'tf1,tf2,tnow,tnext,tstep,ipr: ',
    !    +		  tf1,tf2,tnow,tnext,tstep,iprecip
    !#############################################################

    !#######################################################################
    !	if(npart.gt.0) then
    !          write(88,*) 'istep,nplume,npart,nk: ',istep,nplume,npart,nk
    !          do k=1,nk
    !            npcount(k)=0
    !            do i=1,mdefcomp
    !              ipcount(i,k)=0
    !            end do
    !          end do
    !          do n=1,npart
    !            vlvl=pdata(n)%z
    !            ilvl=vlvl*10000.
    !            k=ivlevel(ilvl)
    !            npcount(k)  =npcount(k)+1
    !            m=icomp(n)
    !            ipcount(m,k)=ipcount(m,k)+1
    !          end do
    !          do k=nk,1,-1
    !            if(npcount(k).gt.0) then
    !              write(88,8800) k,npcount(k),(ipcount(i,k),i=1,ndefcomp)
    ! 8800	      format(1x,i2,':',i7,2x,12(1x,i5))
    !            end if
    !          end do
    !          write(88,*) '----------------------------------------------'
    !        end if
    !#######################################################################

    !..radioactive decay for depositions
    !.. and initialization of decay-parameters
      if (idecay == 1) call decayDeps(tstep)
    ! prepare particle functions once before loop
      if (init) then
      ! setting particle-number to 0 means init
        call posint_init()
        if(iwetdep == 2) call wetdep2_init(tstep)
        call forwrd_init()
        if(irwalk /= 0) call rwalk_init(tstep)
        init = .FALSE.
      end if

    ! particle loop
    ! OMP PARALLEL DO PRIVATE(pextra) SCHEDULE(guided) !np is private by default
      part_do: do np=1,npart
        if (.not.pdata(np)%active) cycle part_do

        pdata(np)%ageInSteps = pdata(np)%ageInSteps + 1

        !..interpolation of boundary layer top, height, precipitation etc.
        !  creates and save temporary data to pextra%prc, pextra%
        call posint(pdata(np), tf1, tf2, tnow, pextra)

        !..radioactive decay
        if(idecay == 1) call decay(pdata(np))

        !         if(iensemble.eq.1)
        !     +      call ensemble(2,itime,tf1,tf2,tnow,istep,nstep,nsteph,np)

        !..dry deposition (1=old, 2=new version)
        if(idrydep == 1) call drydep1(pdata(np))
        if(idrydep == 2) call drydep2(tstep, pdata(np))

        !          if(iensemble.eq.1)
        !     +      call ensemble(3,itime,tf1,tf2,tnow,istep,nstep,nsteph,np)

        !..wet deposition (1=old, 2=new version)
        if(iwetdep == 1) call wetdep1(pdata(np), pextra)
        if(iwetdep == 2) call wetdep2(tstep, pdata(np), pextra)

        !          if(iensemble.eq.1)
        !     +      call ensemble(4,itime,tf1,tf2,tnow,istep,nstep,nsteph,np)

        !..move all particles forward, save u and v to pextra
        call forwrd(tf1, tf2, tnow, tstep, pdata(np), pextra)

        !..apply the random walk method (diffusion)
        if(irwalk /= 0) call rwalk(blfullmix, pdata(np), pextra)

        !.. check domain (%active) after moving particle
        call checkDomain(pdata(np))
      end do part_do
    ! OMP END PARALLEL DO

    !..remove inactive particles or without any mass left
      call rmpart(rmlimit)

    !       if(iensemble.eq.1)
    !     +    call ensemble(5,itime,tf1,tf2,tnext,istep,nstep,nsteph,0)

    ! OMP PARALLEL DO REDUCTION(max : mhmax) REDUCTION(min : mhmin)
      do n=1,npart
        if(pdata(n)%hbl > mhmax) mhmax=pdata(n)%hbl
        if(pdata(n)%hbl < mhmin) mhmin=pdata(n)%hbl
      enddo
    ! OMP END PARALLEL DO


    !###################################################################
    !	write(error_unit,
    !    +  fmt='(''istep,nstep,isteph,nsteph,iprecip,nprecip: '',6i4)')
    !    +          istep,nstep,isteph,nsteph,iprecip,nprecip
    !	write(iulog,
    !    +  fmt='(''istep,nstep,isteph,nsteph,iprecip,nprecip: '',6i4)')
    !    +          istep,nstep,isteph,nsteph,iprecip,nprecip
    !###################################################################

    !..output...................................................
#if defined(VOLCANO)
      do k=1,npart
        x=pdata(k)%x
        y=pdata(k)%y
        i=nint(pdata(k)%x)
        j=nint(pdata(k)%y)
        if(pdata(k)%z > 0.43) &
            vcon(i,j,1)=vcon(i,j,1)+pdata(k)%rad/120.0        ! level 1
        if(pdata(k)%z > 0.23 .AND. pdata(k)%z <= 0.43) &
            vcon(i,j,2)=vcon(i,j,2)+pdata(k)%rad/120.0        ! level 2
        if(pdata(k)%z > 0.03 .AND. pdata(k)%z <= 0.216) &
            vcon(i,j,3)=vcon(i,j,3)+pdata(k)%rad/120.0        ! level 3
      enddo
    ! cc
    !	if(mod(istep,nsteph).eq.0) then
    !     +    write(error_unit,*) 'istep,nplume,npart: ',istep,nplume,npart
    ! b... START
    ! b... output with concentrations after 6 hours
      if(istep > 1 .AND. mod(istep,72) == 0) then
        write(*,*) (itime(i),i=1,5)
        itimev = itime
        itimev(5)= itime(5)+1
        call vtime(itimev,ierror)
        write(*,*) (itimev(i),i=1,5)
      !     +    write(error_unit,*) 'istep,nplume,npart: ',istep,nplume,npart
      !	write(error_unit,*)
      !	write(error_unit,*) 'istep,hour,npart=',istep,istep/72,npart

      !... calculate number of non zero model grids

        m=count(vcon > 0.0)

      !... write non zero model grids, their gegraphical coordinates and mass to output file

      ! b 19.05 start
        write(cfile,'(''concentrations-'',i2.2)') istep/72
        open(12,file=cfile)
        rewind 12
        write(12,'(i4,3i2.2)') (itimev(i),i=1,4)
        write(12,'(i6,'' - non zero grids'')') m
        write(*,*)
        write(*,*) 'Output no.:',istep/72
        write(*,*) 'Time (hrs): ',istep/12

        m=0
        do i=1,nx
          do j=1,ny
            do k=1,3
              if(vcon(i,j,k) > 0.0) then
                m=m+1
                x=real(i)+0.5
                y=real(j)+0.5
                call xyconvert(1,x,y,igtype,gparam,2,geoparam,ierror)
                write(12,'(i6,2x,3i5,2x,2f10.3,2x,e12.3)') &
                    m,i,j,k,x,y,vcon(i,j,k)/72.0
              !	      write(*,'(i6,2x,3i5,2x,2f10.3,2x,e12.3)')
              !     &        m,i,j,k,x,y,vcon(i,j,k)/72.0
              endif
            enddo
          enddo
        enddo

        vcon =  0.0

        write(error_unit,*) 'npart all=',npart
        write(error_unit,*) 'ngrid all=',m
      endif
      close (12)
    ! b... END
#endif

    !..fields
      ifldout=0
      isteph=isteph+1
      if(isteph == nsteph) then
        isteph=0
        if (nhrun > 0) then
          itime(5)=itime(5)+1
        else
          itime(5)=itime(5)-1
        end if
        do i=1,5
          itimeo(i)=itime(i)
        end do
        call vtime(itimeo,ierror)
        if(isynoptic == 0) then
        !..asynoptic output (use forecast length in hours to test if output)
          ihour=itime(5)
        else
        !..synoptic output  (use valid hour to test if output)
          ihour=itimeo(4)
        end if
        if(mod(ihour,nhfout) == 0) then
          ifldout=1
          if(ifltim == 0) then
          !..identify fields with forecast length (hours after start)
            itimeo = itime
          end if
        !..save first and last output time
          ntimefo=ntimefo+1
          if(ntimefo == 1) then
            itimefo(:,1) = itimeo
          end if
          itimefo(:,2) = itimeo
          write(iulog,*) 'fldout. ',itimeo
        end if
      end if

    !      if(iensemble.eq.1 .and. isteph.eq.0)
    !     +  call ensemble(6,itime,tf1,tf2,tnext,istep,nstep,nsteph,0)

    !..field output if ifldout=1, always accumulation for average fields
      if (idailyout == 1) then
      !       daily output, append +x for each day
      ! istep/nsteph = hour  -> /24 =day
        write(fldfilN,'(a9,a1,I3.3)') fldfil, '+', istep/nsteph/24
        if (fldfilX /= fldfilN) then
          fldfilX = fldfilN
          if (fldtype == "netcdf") then
            call fldout_nc(-1,iunito,fldfilX,itime1,0.,0.,0.,tstep, &
                (24/nhfout)+1,nsteph,ierror)
          else
            call fldout(-1,iunito,fldfilX,itime1,0.,0.,0.,tstep, &
                (24/nhfout)+1,nsteph,ierror)
          endif
          if(ierror /= 0) call snap_error_exit()
        end if
        if (fldtype == "netcdf") then
          call fldout_nc(ifldout,iunito,fldfilX,itimeo,tf1,tf2,tnext, &
              tstep,istep,nsteph,ierror)
        else
          call fldout(ifldout,iunito,fldfilX,itimeo,tf1,tf2,tnext,tstep, &
              istep,nsteph,ierror)
        endif
        if(ierror /= 0) call snap_error_exit()
      else
        if (fldtype == "netcdf") then
          call fldout_nc(ifldout,iunito,fldfil,itimeo,tf1,tf2,tnext, &
              tstep,istep,nsteph,ierror)
        else
          call fldout(ifldout,iunito,fldfil,itimeo,tf1,tf2,tnext,tstep, &
              istep,nsteph,ierror)
        endif
        if(ierror /= 0) call snap_error_exit()
      end if

      if(isteph == 0 .AND. iprecip < nprecip) iprecip=iprecip+1

#if defined(TRAJ)
    ! b

      distance=distance+speed*tstep
      if(istep > 0 .AND. mod(istep,nsteph) == 0) then
        timeStart = [0, 0, itime1(4), itime1(3), itime1(2), itime1(1)]
        epochSecs = timeGm(timeStart)
        if (nhrun >= 0) then
          epochSecs = epochSecs + nint(istep*tstep)
        else
          epochSecs = epochSecs - nint(istep*tstep)
        endif
        timeCurrent = epochToDate(epochSecs)

      !	if(istep .gt. -1) then
        call vtime(itimev,ierror)
      !	write(*,*) (itime(i),i=1,5), ierror
      !	write(*,*) (itimev(i),i=1,5)
      !	write(*,*) 'istep=',istep, 'npart=',npart
      !	do k=1,npart
        do k=1,1
        !	write(*,*) istep,pdata(k)%x,pdata(k)%y,pdata(k)%z
          x=pdata(k)%x
          y=pdata(k)%y
          i=int(x(1))
          j=int(y(1))
          call xyconvert(1,x,y,igtype,gparam,2,geoparam,ierror)
          vlvl=pdata(k)%z
          ilvl=vlvl*10000.
          k1=ivlevel(ilvl)
          k2=k1+1
          zzz=hlevel2(i,j,k2)+(hlevel2(i,j,k1)-hlevel2(i,j,k2))* &
          (pdata(k)%z-vlevel(k2))/(vlevel(k1)-vlevel(k2))
        !	write(*,*) istep,x,y,pdata(k)%z,k1,k2,
        !     &  vlevel(k1),vlevel(k2),hlevel2(i,j,k1),hlevel2(i,j,k2),zzz
        !	write(*,*)
        !	write(*,*) istep,k,x,y,zzz
        !	write(*,'(1x,i4,i2,i2,2i2.2,''00'')')
        !     &(itime(i),i=1,4),mod(istep,12)*5
          write(13,'(1x,i4,4i2.2,''00'', &
              &                  2f9.3,f12.3,f15.2,f10.2)') &
              timeCurrent(6),timeCurrent(5),timeCurrent(4), &
              timeCurrent(3),timeCurrent(2), &
              y,x,zzz,distance, speed
          write(*,'(i4,1x,i4,i2,i2,2i2.2,''00'', &
              &                 2f9.3,f12.3,f15.2,f10.2)') istep, &
              timeCurrent(6),timeCurrent(5),timeCurrent(4), &
              timeCurrent(3),timeCurrent(2), &
              y,x,zzz,distance, speed
        ! b-2701
          flush(13)
        enddo
      endif
#endif
    end do time_loop
#if defined(TRAJ)
    close (13)

  end do
#endif

!      if(iensemble.eq.1)
!     +  call ensemble(7,itimefa,tf1,tf2,tnext,istep,nstep,nsteph,0)

  if(lstepr < nstep .AND. lstepr < nstepr) then
    write(iulog,*) 'ERROR: Due to space problems the release period was'
    write(iulog,*) '       shorter than requested.'
    write(iulog,*) '   No. of requested release timesteps: ',nstepr
    write(iulog,*) '   No. of simulated release timesteps: ',lstepr
    write(error_unit,*) 'ERROR: Due to space problems the release period was'
    write(error_unit,*) '       shorter than requested.'
    write(error_unit,*) '   No. of requested release timesteps: ',nstepr
    write(error_unit,*) '   No. of simulated release timesteps: ',lstepr

    call snap_error_exit()
  end if

  ! b_240311
  write(*,*)
  write(*,'(''mhmax='',f10.2)') mhmax
  write(*,'(''mhmin='',f10.2)') mhmin
  write(*,*)
  ! b_end
  write(iulog,*) ' SNAP run finished'
  write(output_unit,*) ' SNAP run finished'

  if(iargos == 1) then
    close(91)
    close(92)
    close(93)
  end if

! deallocate all fields
  CALL deAllocateFields()

  close(iulog)

  contains

  subroutine snap_error_exit()
    character(len=*), parameter :: ERROR_MSG = ' ------- SNAP ERROR EXIT -------'

    call deAllocateFields()
    if(iargos == 1) then
      close(91)
      close(92)
      close(93)
    end if

    write(iulog,*) ERROR_MSG
    write(error_unit,*) ERROR_MSG
    close(iulog)

    error stop ERROR_MSG
  end subroutine
END PROGRAM
