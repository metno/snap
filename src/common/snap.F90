! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2021   Norwegian Meteorological Institute

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
! REMOVE.RELATIVE.MASS.LIMIT= 0.02
! PARTICLES.SPLIT.HOURS= 24
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
! * number of steps to skip in the beginning of a file
! FIELD.SPINUPSTEPS= 1
! FIELD.INPUT= arklam.dat
! FIELD.INPUT= feltlam.dat
! FIELD_TIME.FORECAST
! FIELD_TIME.VALID
! FIELD.OUTPUT= snap.felt
! FIELD.OUTTYPE=netcdf
! FIELD.DAILY.OUTPUT.ON
! FIELD.USE_MODEL_WIND_INSTEAD_OF_10M= [.false.]/.true
! * timestamp which will also be written to netcdf-files, default: now
! SIMULATION.START.DATE=2010-01-01_10:00:00
! LOG.FILE=     snap.log
! DEBUG.OFF ..................................... (default)
! DEBUG.ON
! ARGOS.OUTPUT.OFF
! ARGOS.OUTPUT.ON
! ARGOS.OUTPUT.DEPOSITION.FILE=    runident_MLDP0_depo
! ARGOS.OUTPUT.CONCENTRATION.FILE= runident_MLDP0_conc
! ARGOS.OUTPUT.TOTALDOSE.FILE=     runident_MLDP0_dose
! ARGOS.OUTPUT.TIMESTEP.HOUR= 3
! END
!=======================================================================

!> SNAP - Severe Nuclear Accident Program
PROGRAM bsnap
  USE iso_fortran_env, only: real64, output_unit, error_unit, IOSTAT_END
  USE DateCalc, only: epochToDate, timeGM
  USE snapdebug, only: iulog, idebug, acc_timer => prefixed_accumulating_timer
  USE snapargosML, only: argosdepofile, argosdosefile, argoshoursrelease, &
                         argoshourstep, argoshoursrun, iargos, margos, argosconcfile, nargos, &
                         argostime
  USE snapdimML, only: nx, ny, nk, ldata, maxsiz, mcomp
  USE snapfilML, only: filef, itimer, limfcf, ncsummary, nctitle, nhfmax, nhfmin, &
                       nctype, nfilef, simulation_start, spinup_steps
  USE snapfldML, only: nhfout, enspos, iprecip, nprecip
  USE snapmetML, only: init_meteo_params, met_params
  USE snapparML, only: component, run_comp, &
                       ncomp, def_comp, nparnum, &
                       time_profile, TIME_PROFILE_BOMB
  USE snapposML, only: irelpos, nrelpos, release_positions
  USE snapgrdML, only: modleveldump, ivcoor, ixbase, iybase, ixystp, kadd, &
                       klevel, imslp, inprecip, iprod, iprodr, itotcomp, gparam, igrid, igridr, &
                       igtype, imodlevel
  USE snaptabML, only: tabcon
  USE particleML, only: pdata, extraParticle
  USE allocateFieldsML, only: allocateFields, deallocateFields
  USE fldout_ncML, only: fldout_nc
  USE rmpartML, only: rmpart
  USE split_particlesML, only: split_particles
  USE checkdomainML, only: checkdomain
  USE rwalkML, only: rwalk, rwalk_init
  USE milibML, only: xyconvert, chcase, hrdiff, vtime
  use snapfldML, only: depwet
#if defined(TRAJ)
  USE snapfldML, only: hlevel2
  USE forwrdML, only: forwrd, forwrd_init, speed
  USE snapgrdML, only: vlevel, ivlevel
#else
  USE forwrdML, only: forwrd, forwrd_init
#endif
  USE wetdep, only: wetdep2, wetdep2_init
  USE drydep, only: drydep1, drydep2
  USE decayML, only: decay, decayDeps
  USE posintML, only: posint, posint_init
  USE bldpML, only: bldp
  USE releaseML, only: release, releases, nrelheight, mprel, &
                       mplume, nplume, iplume, npart, mpart, release_t
  USE init_random_seedML, only: init_random_seed
  USE compheightML, only: compheight
  USE readfield_ncML, only: readfield_nc
  USE snapfimexML, only: fimex_type => file_type, fimex_config => conf_file, fimex_interpolation => interpolation, fint
#if defined(FIMEX)
  USE readfield_fiML, only: readfield_fi
  USE filesort_fiML, only: filesort_fi
#endif
  USE releasefileML, only: releasefile
  USE filesort_ncML, only: filesort_nc

  implicit none

  integer :: allocatestatus
  character(len=*), parameter :: allocateErrorMessage = "*** Not enough memory ***"

! Format of time
!..itime: itime(1) - year
!         itime(2) - month
!         itime(3) - day
!         itime(4) - hour
!         itime(5) - forecast time in hours (added to date/time above)

!> start time
  integer :: itime1(5) = -huge(itime1)
!> stop  time
  integer :: itime2(5)
  integer :: itime(5), itimei(5), itimeo(5)
  integer :: time_file(5)

!..used in xyconvert (longitude,latitude -> x,y)
  real, save :: geoparam(6) = [1.0, 1.0, 1.0, 1.0, 0.0, 0.0]

  integer :: snapinput_unit, ios
  integer :: nhrun = 0, nhrel = 0
  logical :: use_random_walk = .true.
  integer :: isynoptic = 0, m, np, npl, nlevel = 0, minhfc = +6, maxhfc = +huge(maxhfc), ifltim = 0
  integer :: k, ierror, i, n
  integer :: ih
  integer :: idrydep = 0, wetdep_version = 0, idecay
  integer :: ntimefo, iunitf, nh1, nh2
  integer :: ierr1, ierr2, nsteph, nstep, nstepr, iunito
  integer :: nxtinf, ihread, isteph, lstepr, iendrel, istep, ihr1, ihr2, nhleft
  integer :: ierr, ihdiff, ihr, ifldout, idailyout = 0, ihour, split_particle_after_step, split_particle_hours
  integer :: date_time(8)
  logical :: warning = .false.
  real :: tstep = 900, rmlimit = -1.0, rnhrun, rnhrel, tf1, tf2, tnow, tnext
  real ::    x(1), y(1)
  type(extraParticle) :: pextra
  real ::    rscale
  integer :: ntprof
! ipcount(mdefcomp, nk)
! integer, dimension(:,:), allocatable:: ipcount
! npcount(nk)
! integer, dimension(:), allocatable:: npcount
! b_start
  real :: mhmin, mhmax  ! minimum and maximum of mixing height
! b_end
!> Information for reading from a releasefile
  type(release_t) :: release1

  logical :: blfullmix = .true.
  logical :: init = .TRUE.

  character(len=1024) ::  finput, fldfil = "snap.dat", fldfilX, fldfilN, logfile = "snap.log", ftype = "netcdf", &
                         fldtype = "netcdf", relfile = "*"
  character(len=1024) :: tempstr

!> name of selected release position
  character(len=40), save :: srelnam = "*"

  !> Time spent in various components of SNAP
  type(acc_timer) :: timeloop_timer
  type(acc_timer) :: output_timer
  type(acc_timer) :: input_timer
  type(acc_timer) :: particleloop_timer

#if defined(TRAJ)
  integer :: timeStart(6), timeCurrent(6)
  integer :: ilvl, k1, k2
  real ::    vlvl

  integer(kind=real64) :: epochSecs
  real :: zzz
! character(len=60) :: tr_file
  integer :: ntraj, itraj
  real :: tlevel(100)
  character(len=80) :: tname(10) ! name for the trajectory
  integer :: tyear, tmon, tday, thour, tmin
  real :: distance
#endif

#if defined(VOLCANO)
!... matrix for calculating volcanic ash concentrations + file name
!   vcon(nx,ny,3)
  real, allocatable :: vcon(:, :, :)
  character(len=60) :: cfile
#endif

#if defined(VOLCANO) || defined(TRAJ)
  integer :: itimev(5), j
#endif

#if defined(VERSION)
  character(len=*), parameter :: VERSION_ = VERSION
#else
  character(len=*), parameter :: VERSION_ = "UNVERSIONED"
#endif

  if (command_argument_count() < 1) then
    write (error_unit, *)
    write (error_unit, *) '  usage: snap <snap.input>'
    write (error_unit, *) '     or: snap --version'
    write (error_unit, *)
    stop 1
  endif
  call get_command_argument(1, finput)
  if (finput == "--version") then
    write (output_unit, *) "snap version: ", VERSION_
    stop
  endif

  open (newunit=snapinput_unit, file=finput, &
        access='stream', form='unformatted', &
        status='old', iostat=ios, action='read', &
        position='rewind')
  if (ios /= 0) then
    write (error_unit, *) 'Open Error: ', trim(finput)
    call snap_error_exit()
  endif

  call DATE_AND_TIME(VALUES=date_time)
  write (simulation_start, 9999) (date_time(i), i=1, 3), &
    (date_time(i), i=5, 7)
9999 FORMAT(I4.4, '-', I2.2, '-', I2.2, '_', I2.2, ':', I2.2, ':', I2.2)
  write (error_unit, *) 'Reading input file:'
  write (error_unit, *) TRIM(finput)

  call read_inputfile(snapinput_unit)

  close (snapinput_unit)

  write (error_unit, *) "SIMULATION_START_DATE: ", simulation_start

  if (relfile /= '*') then
    call releasefile(relfile, release1)
    ntprof = size(releases)
  end if

!..convert names to uppercase letters
  call chcase(2, 1, srelnam)
  do n = 1, nrelpos
    call chcase(2, 1, release_positions(n)%name)
  end do
  if (ncomp > 0) call chcase(2, ncomp, component)

  allocate (run_comp(ncomp))
  run_comp%totalbq = 0
  run_comp%numtotal = 0
  run_comp%to_defined = 0

  call conform_input(ierror)
  if (ierror /= 0) then
    write (error_unit, *) "Input '", trim(finput), "' contains some invalid input"
    stop 1
  end if

  maxsiz = nx*ny
  ldata = 20 + maxsiz + 50
  CALL allocateFields()

  if (warning) then
    write (error_unit, *) 'Input o.k. (with warnings)'
  else
    write (error_unit, *) 'Input o.k.'
  endif
!-------------------------------------------------------------------

!..log file
  open (newunit=iulog, file=logfile, &
        access='sequential', form='formatted', &
        status='replace', action='write')

  ntimefo = 0

!..define fixed tables and constants (independant of input data)
  call tabcon

! initialize random number generator for rwalk and release
  CALL init_random_seed()

!..file unit for all input field files
  iunitf = 20

!..check input FELT files and make sorted lists of available data
!..make main list based on x wind comp. (u) in upper used level
  if (ftype == "netcdf") then
    call filesort_nc ! (iunitf, ierror)
  else if (ftype == "fimex") then
#if defined(FIMEX)
    call filesort_fi()
#endif
  else
    ierror = 1
  end if
  if (ierror /= 0) call snap_error_exit(iulog)

  call vtime(itime1, ierror)
  if (ierror /= 0) then
    write (iulog, *) 'Requested start time is wrong:'
    write (iulog, *) (itime1(i), i=1, 4)
    write (error_unit, *) 'Requested start time is wrong:'
    write (error_unit, *) (itime1(i), i=1, 4)
    call snap_error_exit(iulog)
  end if

  itime2 = itime1
  itime2(5) = itime2(5) + nhrun
  call vtime(itime2, ierror)

  call hrdiff(0, 0, itimer(1, 1), itime1, nh1, ierr1, ierr2)
  call hrdiff(0, 0, itime2, itimer(1, 2), nh2, ierr1, ierr2)
  if (nh1 < 0 .OR. nh2 < 0) then
    write (iulog, *) 'Not able to run requested time periode.'
    write (iulog, *) 'Start:        ', (itime1(i), i=1, 4)
    write (iulog, *) 'End:          ', (itime2(i), i=1, 4)
    write (iulog, *) 'First fields: ', (itimer(i, 1), i=1, 4)
    write (iulog, *) 'Last  fields: ', (itimer(i, 2), i=1, 4)
    write (error_unit, *) 'Not able to run requested time periode.'
    write (error_unit, *) 'Start:        ', (itime1(i), i=1, 4)
    write (error_unit, *) 'End:          ', (itime2(i), i=1, 4)
    write (error_unit, *) 'First fields: ', (itimer(i, 1), i=1, 4)
    write (error_unit, *) 'Last  fields: ', (itimer(i, 2), i=1, 4)
    if (nh1 < 0) then
      write (iulog, *) 'NO DATA AT START OF RUN'
      write (error_unit, *) 'NO DATA AT START OF RUN'
      call snap_error_exit(iulog)
    end if
    write (iulog, *) 'Running until end of data'
    write (error_unit, *) 'Running until end of data'
    do i = 1, 5
      itime2(i) = itimer(i, 2)
    end do
    call hrdiff(0, 0, itime1, itime2, nhrun, ierr1, ierr2)
  end if

  if (nhrel > abs(nhrun)) nhrel = abs(nhrun)

  if (iargos == 1) then
    argoshoursrelease = nhrel
    argoshoursrun = nhrun
    !..the following done to avoid updateing subr. fldout............
    nhfout = argoshourstep
    isynoptic = 0
    !................................................................
  end if
#if defined(TRAJ)
  do itraj = 1, ntraj
    releases(1)%rellower(1) = tlevel(itraj)
    !        relupper(1)=rellower(1)+1
    releases(1)%relupper(1) = releases(1)%rellower(1)
    tyear = itime1(1)
    tmon = itime1(2)
    tday = itime1(3)
    thour = itime1(4)
    tmin = 0.0
    write (error_unit, *) 'lower, upper', releases(1)%rellower(1), releases(1)%relupper(1)
    write (error_unit, *) 'tyear, tmon, tday, thour, tmin', &
      tyear, tmon, tday, thour, tmin
    distance = 0.0
    speed = 0.0
    iprecip = 1
#endif

    !..initial no. of plumes and particles
    nplume = 0
    npart = 0
    nparnum = 0

    !..no. of timesteps per hour (adjust the timestep)
    nsteph = nint(3600./tstep)
    tstep = 3600./float(nsteph)
    split_particle_after_step = split_particle_hours * nsteph

    !..convert modleveldump from hours to steps
    modleveldump = modleveldump*nsteph

    !..total no. of timesteps to run (nhrun is no. of hours to run)
    nstep = nsteph*nhrun
    if (nstep < 0) nstep = -nstep

    !..total no. of timesteps to release particles
    nstepr = nsteph*nhrel

    !..nuclear bomb case
    if (time_profile == TIME_PROFILE_BOMB) nstepr = 1

    !..field output file unit
    iunito = 30

    !..information to log file
    write (iulog, *) 'nx,ny,nk:  ', nx, ny, nk
    write (iulog, *) 'kadd:      ', kadd
    write (iulog, *) 'klevel:'
    write (iulog, *) (klevel(i), i=1, nk)
    write (iulog, *) 'imslp:     ', imslp
    write (iulog, *) 'inprecip:  ', inprecip
    write (iulog, *) 'imodlevel: ', imodlevel
    write (iulog, *) 'modleveldump (h), steps:', modleveldump/nsteph, &
      modleveldump
    write (iulog, *) 'itime1:  ', (itime1(i), i=1, 5)
    write (tempstr, '("Starttime: ",I4,"-",I2.2,"-",I2.2,"T",I2.2 &
        &,":",I2.2)') (itime1(i), i=1, 5)
    ncsummary = trim(ncsummary)//" "//trim(tempstr)
    do n = 1, nrelpos
      write (tempstr, '("Release Pos (lat, lon): (", F5.1, ",", F6.1 &
          &,")")') &
          release_positions(n)%geo_latitude, &
          release_positions(n)%geo_longitude
      ncsummary = trim(ncsummary)//". "//trim(tempstr)
    end do

    write (iulog, *) 'itime2:  ', (itime2(i), i=1, 5)
    write (iulog, *) 'itimer1: ', (itimer(i, 1), i=1, 5)
    write (iulog, *) 'itimer2: ', (itimer(i, 2), i=1, 5)
    write (iulog, *) 'nhfmin:  ', nhfmin
    write (iulog, *) 'nhfmax:  ', nhfmax
    write (iulog, *) 'nhrun:   ', nhrun
    write (iulog, *) 'nhrel:   ', nhrel
    write (iulog, *) 'tstep:   ', tstep
    write (iulog, *) 'nsteph:  ', nsteph
    write (iulog, *) 'nstep:   ', nstep
    write (iulog, *) 'nstepr:  ', nstepr
    write (iulog, *) 'mprel:   ', mprel
    write (iulog, *) 'ifltim:  ', ifltim
    write (iulog, *) 'irwalk:  ', use_random_walk
    write (iulog, *) 'idrydep: ', idrydep
    write (iulog, *) 'wetdep_version: ', wetdep_version
    write (iulog, *) 'idecay:  ', idecay
    write (iulog, *) 'rmlimit: ', rmlimit
    write (iulog, *) 'ndefcomp:', size(def_comp)
    write (iulog, *) 'ncomp:   ', ncomp
    write (iulog, fmt='(1x,a,40(1x,i2))') 'running_to_defined_comp: ', &
      (run_comp(i)%to_defined, i=1, ncomp)
    write (iulog, fmt='(1x,a,40(1x,i2))') 'defined_to_running_comp: ', &
      (def_comp(i)%to_running, i=1, size(def_comp))
    do n = 1, ncomp
      m = run_comp(n)%to_defined
      write (iulog, *) 'component no:  ', n
      write (iulog, *) 'compname:   ', def_comp(m)%compname
      write (iulog, *) '  field id:   ', def_comp(m)%idcomp
      write (iulog, *) '  kdrydep:    ', def_comp(m)%kdrydep
      write (iulog, *) '  drydephgt:  ', def_comp(m)%drydephgt
      write (iulog, *) '  drydeprat:  ', def_comp(m)%drydeprat
      write (iulog, *) '  kwetdep:    ', def_comp(m)%kwetdep
      write (iulog, *) '  wetdeprat:  ', def_comp(m)%wetdeprat
      write (iulog, *) '  kdecay:     ', def_comp(m)%kdecay
      write (iulog, *) '  halftime:   ', def_comp(m)%halftime
      write (iulog, *) '  decayrate:  ', def_comp(m)%decayrate
      write (iulog, *) '  kgravity:   ', def_comp(m)%grav_type
      write (iulog, *) '  gravityms:  ', def_comp(m)%gravityms
      write (iulog, *) '  radiusmym:  ', def_comp(m)%radiusmym
      write (iulog, *) '  densitygcm3:', def_comp(m)%densitygcm3
      write (iulog, *) '  Relase time profile:   ntprof: ', ntprof
      ncsummary = trim(ncsummary)//". Release "//trim(def_comp(m)%compname) &
                  //" (hour, Bq/s): "
      do i = 1, ntprof
        write (iulog, *) '  hour,Bq/hour: ', &
          releases(i)%frelhour, (releases(i)%relbqsec(n, ih)*3600., ih=1, nrelheight)
        write (tempstr, '("(",f5.1,",",ES9.2,")")') &
          releases(i)%frelhour, releases(i)%relbqsec(n, 1)
        ncsummary = trim(ncsummary)//" "//trim(tempstr)
      end do
    end do
    write (iulog, *) 'itotcomp:   ', itotcomp
    write (iulog, *) 'iargos:     ', iargos
    write (iulog, *) 'blfulmix:   ', blfullmix
    write (error_unit, *) 'Title:      ', trim(nctitle)
    write (error_unit, *) 'Summary:    ', trim(ncsummary)

    !..initialize files, deposition fields etc.
    m = 0
    nargos = 0
    do n = 1, abs(nhrun)
      do i = 1, 4
        itime(i) = itime1(i)
      end do
      if (nhrun > 0) then
        itime(5) = n
      else
        itime(5) = -n
      endif
      if (isynoptic == 0) then
        !..asynoptic output (use forecast length in hours to test if output)
        ihour = itime(5)
      else
        !..synoptic output  (use valid hour to test if output)
        call vtime(itime, ierror)
        ihour = itime(4)
      end if
      if (mod(ihour, nhfout) == 0) m = m + 1
      if (iargos == 1) then
        if (mod(n, argoshourstep) == 0) then
          if (nargos < margos) then
            nargos = nargos + 1
            do i = 1, 4
              argostime(i, nargos) = itime1(i)
            end do
            argostime(5, nargos) = n
          end if
        end if
      end if
    end do
    if (idailyout == 1) then
      !       daily output, append +x for each day, but initialize later
      write (fldfilX, '(a9,a1,I3.3)') fldfil, '+', -1
    end if
    ! standard output needs to be initialized, even for daily
    if (fldtype == "netcdf") then
      call fldout_nc(-1, iunito, fldfil, itime1, 0., 0., 0., tstep, &
                     m, nsteph, ierror)
    else
      write (iulog, *) "only FIELD.OUTTYPE=netcdf supported, got: ", fldtype
      ierror = 1
    endif
    if (ierror /= 0) call snap_error_exit(iulog)

    itime = itime1
    time_file = 0

    nxtinf = 0
    ihread = 0
    isteph = 0
    lstepr = 0
    iendrel = 0

    istep = -1

#if defined(TRAJ)
    !        write (error_unit,*) (itime(i),i=1,5)
    itimev = itime
    !        write (error_unit,*) (itimev(i),i=1,5)
    ! 1110 continue
    !        write (tr_file,'(''Trajectory_'',i3.3,
    !     &  ''_'',i4,3i2.2,''0000.DAT'')') itraj,(itime(i),i=1,4)
    !        open(13,file=tr_file)
    open (13, file=tname(itraj))
    rewind 13
    !        write (error_unit,*) tr_file
    write (error_unit, *) tname(itraj)

    !        write (13,'(i6,3f12.3)') nstep

#endif

    ! b_start
    mhmin = 10000.0
    mhmax = -10.0
    ! b_end

#if defined(VOLCANO)
    ! b 01.05 initialize concentration (mass) matrix

    ALLOCATE (vcon(nx, ny, 3), STAT=AllocateStatus)
    IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage
    vcon = 0.0
    ! b END
#endif

! reset readfield_nc (eventually, traj will rerun this loop)
    if (ftype == "netcdf") then
      call readfield_nc(-1, nhleft, itimei, ihr1, ihr2, &
                        time_file, ierror)
    else if (ftype == "fimex") then
#if defined(FIMEX)
      call readfield_fi(-1, nhleft, itimei, ihr1, ihr2, &
                        time_file, ierror)
#else
      error stop "A fimex read was requested, but fimex support is not included"// &
        " in this build"
#endif
    end if

    timeloop_timer = acc_timer("time_loop:")
    output_timer = acc_timer("output/accumulation:")
    input_timer = acc_timer("Reading MET input:")
    particleloop_timer = acc_timer("Particle loop:")

    ! start time loop
    time_loop: do istep = 0, nstep
      call timeloop_timer%start()
      write (iulog, *) 'istep,nplume,npart: ', istep, nplume, npart
      flush (iulog)
      if (mod(istep, nsteph) == 0) then
        write (error_unit, *) 'istep,nplume,npart: ', istep, nplume, npart
        flush (error_unit)
      end if

      !#######################################################################
      !..test print: printing all particles in plume 'jpl'
      !       write (88,*) 'step,plume,part: ',istep,nplume,npart
      !       jpl=1
      !       if(jpl.le.nplume .and. iplume(jpl)%start.gt.0) then
      !         do n=iplume(jpl)%start,iplume(jpl)%end
      !           write (88,fmt='(1x,i6,2f7.2,2f7.4,f6.0,f7.3,i4,f8.3)')
      !    +                  iparnum(n),(pdata(i,n),i=1,5),pdata(n)%prc,
      !    +                  icomp(n),pdata(n)%rad
      !         end do
      !       end if
      !#######################################################################

      if (istep == nxtinf) then

        !..read fields
        if (istep == 0) then
          itimei = itime1
          ihr1 = -0
          ihr2 = -nhfmax
          nhleft = nhrun
        else
          itimei = time_file
          ihr1 = +nhfmin
          ihr2 = +nhfmax
          nhleft = (nstep - istep + 1)/nsteph
          if (nhrun < 0) nhleft = -nhleft
        end if
        call input_timer%start()
        if (ftype == "netcdf") then
          call readfield_nc(istep, nhleft, itimei, ihr1, ihr2, &
                            time_file, ierror)
#if defined(FIMEX)
        elseif (ftype == "fimex") then
          call readfield_fi(istep, nhleft, itimei, ihr1, ihr2, &
                            time_file, ierror)
#endif
        end if
        if (idebug >= 1) then
          write (iulog, *) "igtype, gparam(8): ", igtype, gparam
        end if
        if (ierror /= 0) call snap_error_exit(iulog)
        call input_timer%stop_and_log()

        n = time_file(5)
        call vtime(time_file, ierr)
        write (error_unit, fmt='(''input data: '',i4,3i3.2,''  prog='',i4)') &
          (time_file(i), i=1, 4), n

        !..compute model level heights
        call compheight

        !..calculate boundary layer (top and height)
        call bldp

        if (istep == 0) then

          !..release position from geographic to polarstereographic coordinates
          y = release_positions(irelpos)%geo_latitude
          x = release_positions(irelpos)%geo_longitude
          write (iulog, *) 'release lat,long: ', y, x
#if defined(TRAJ)
          !        write (error_unit,*) istep,x,y,rellower(1)
          !        write (13,'(i6,3f12.3)') istep,x,y,rellower(1)

          write (13, '(''RIMPUFF'')')
          write (13, '(i2)') ntraj
          write (13, '(1x,i4,4i2.2,''00'', &
              &   2f9.3,f12.3,f15.2,f10.2)') &
              (itime(i), i=1, 4), 0, y, x, releases(1)%rellower(1), &
              distance, speed
          write (error_unit, '(i4,1x,i4,i2,i2,2i2.2,''00'', &
              &   2f9.3,f12.3,f15.2,f10.2)') istep, &
              (itime(i), i=1, 4), 0, y, x, releases(1)%rellower(1), &
              distance, speed
#endif
          call xyconvert(1, x, y, 2, geoparam, igtype, gparam, ierror)
          if (ierror /= 0) then
            write (iulog, *) 'ERROR: xyconvert'
            write (iulog, *) '   igtype: ', igtype
            write (iulog, *) '   gparam: ', gparam
            write (error_unit, *) 'ERROR: xyconvert'
            write (error_unit, *) '   igtype: ', igtype
            write (error_unit, *) '   gparam: ', gparam
            call snap_error_exit(iulog)
          end if
          write (iulog, *) 'release   x,y:    ', x, y
          if (x(1) < 1.01 .OR. x(1) > nx - 0.01 .OR. &
              y(1) < 1.01 .OR. y(1) > ny - 0.01) then
            write (iulog, *) 'ERROR: Release position outside field area'
            write (error_unit, *) 'ERROR: Release position outside field area'
            call snap_error_exit(iulog)
          end if
          release_positions(irelpos)%grid_x = x(1)
          release_positions(irelpos)%grid_y = y(1)

          nxtinf = 1
          ifldout = 0
          ! continue istep loop after initialization
          call timeloop_timer%stop()
          cycle time_loop
        end if

        call hrdiff(0, 0, itimei, time_file, ihdiff, ierr1, ierr2)
        tf1 = 0.
        tf2 = 3600.*ihdiff
        if (nhrun < 0) tf2 = -tf2
        if (istep == 1) then
          call hrdiff(0, 0, itimei, itime1, ihr, ierr1, ierr2)
          tnow = 3600.*ihr
          nxtinf = istep + nsteph*abs(ihdiff - ihr)
          iprecip = 1 + ihr
          !              backward calculations difficult, but precip does not matter there
          if (ihr < 0) iprecip = 1
        else
          tnow = 0.
          nxtinf = istep + nsteph*abs(ihdiff)
          iprecip = 1
        end if

      else

        tnow = tnow + tstep

      end if

      tnext = tnow + tstep

      if (iendrel == 0 .AND. istep <= nstepr) then

        !..release one plume of particles

        call release(istep - 1, nsteph, tf1, tf2, tnow, ierror)

        if (ierror == 0) then
          lstepr = istep
        else
          write (iulog, *) 'WARNING. Out of space for plumes/particles'
          write (iulog, *) 'WARNING. End release, continue running'
          write (error_unit, *) 'WARNING. Out of space for plumes/particles'
          write (error_unit, *) 'WARNING. End release, continue running'
          iendrel = 1
        end if

      end if

      !#############################################################
      !     write (error_unit,*) 'tf1,tf2,tnow,tnext,tstep,ipr: ',
      !    +                  tf1,tf2,tnow,tnext,tstep,iprecip
      !     write (iulog,*) 'tf1,tf2,tnow,tnext,tstep,ipr: ',
      !    +                  tf1,tf2,tnow,tnext,tstep,iprecip
      !#############################################################

      !#######################################################################
      !        if(npart.gt.0) then
      !          write (88,*) 'istep,nplume,npart,nk: ',istep,nplume,npart,nk
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
      !              write (88,8800) k,npcount(k),(ipcount(i,k),i=1,ndefcomp)
      ! 8800              format(1x,i2,':',i7,2x,12(1x,i5))
      !            end if
      !          end do
      !          write (88,*) '----------------------------------------------'
      !        end if
      !#######################################################################

      !..radioactive decay for depositions
      !.. and initialization of decay-parameters
      if (idecay == 1) call decayDeps(tstep)
      ! prepare particle functions once before loop
      if (init) then
        ! setting particle-number to 0 means init
        call posint_init()
        if (wetdep_version == 2) call wetdep2_init(tstep)
        call forwrd_init()
        if (use_random_walk) call rwalk_init(tstep)
        init = .FALSE.
      end if

      ! plume loop, increase age of all plumes/particles
      !$OMP PARALLEL DO SCHEDULE(guided) !npl is private by default
      do npl = 1, nplume
        iplume(npl)%ageInSteps = iplume(npl)%ageInSteps + 1
      end do
      !$OMP END PARALLEL DO

      call particleloop_timer%start()
      ! particle loop
      !$OMP PARALLEL DO PRIVATE(pextra) SCHEDULE(guided) !np is private by default
      part_do: do np = 1, npart
        if (.not. pdata(np)%active) cycle part_do

        !..interpolation of boundary layer top, height, precipitation etc.
        !  creates and save temporary data to pextra%prc, pextra%
        call posint(pdata(np), tf1, tf2, tnow, pextra)

        !..radioactive decay
        if (idecay == 1) call decay(pdata(np))

        !..dry deposition (1=old, 2=new version)
        if (idrydep == 1) call drydep1(pdata(np))
        if (idrydep == 2) call drydep2(tstep, pdata(np))

        !..wet deposition (1=old, 2=new version)
        if (wetdep_version == 2) call wetdep2(depwet, tstep, pdata(np), pextra)

        !..move all particles forward, save u and v to pextra
        call forwrd(tf1, tf2, tnow, tstep, pdata(np), pextra)

        !..apply the random walk method (diffusion)
        if (use_random_walk) call rwalk(blfullmix, pdata(np), pextra)

        !.. check domain (%active) after moving particle
        call checkDomain(pdata(np))
      end do part_do
      !$OMP END PARALLEL DO
      call particleloop_timer%stop_and_log()

      !..remove inactive particles or without any mass left
      call rmpart(rmlimit)

      !..split particles after some time of transport
      if (split_particle_after_step > 0) then
        call split_particles(split_particle_after_step)
      end if

      !$OMP PARALLEL DO REDUCTION(max : mhmax) REDUCTION(min : mhmin)
      do n = 1, npart
        if (pdata(n)%hbl > mhmax) mhmax = pdata(n)%hbl
        if (pdata(n)%hbl < mhmin) mhmin = pdata(n)%hbl
      enddo
      !$OMP END PARALLEL DO

      !###################################################################
      !        write (error_unit,
      !    +  fmt='(''istep,nstep,isteph,nsteph,iprecip,nprecip: '',6i4)')
      !    +          istep,nstep,isteph,nsteph,iprecip,nprecip
      !        write (iulog,
      !    +  fmt='(''istep,nstep,isteph,nsteph,iprecip,nprecip: '',6i4)')
      !    +          istep,nstep,isteph,nsteph,iprecip,nprecip
      !###################################################################

      !..output...................................................
#if defined(VOLCANO)
      do k = 1, npart
        x = pdata(k)%x
        y = pdata(k)%y
        i = nint(pdata(k)%x)
        j = nint(pdata(k)%y)
        if (pdata(k)%z > 0.43) &
          vcon(i, j, 1) = vcon(i, j, 1) + pdata(k)%rad/120.0        ! level 1
        if (pdata(k)%z > 0.23 .AND. pdata(k)%z <= 0.43) &
          vcon(i, j, 2) = vcon(i, j, 2) + pdata(k)%rad/120.0        ! level 2
        if (pdata(k)%z > 0.03 .AND. pdata(k)%z <= 0.216) &
          vcon(i, j, 3) = vcon(i, j, 3) + pdata(k)%rad/120.0        ! level 3
      enddo
      ! cc
      !        if(mod(istep,nsteph).eq.0) then
      !     +    write (error_unit,*) 'istep,nplume,npart: ',istep,nplume,npart
      ! b... START
      ! b... output with concentrations after 6 hours
      if (istep > 1 .AND. mod(istep, 72) == 0) then
        write (error_unit, *) (itime(i), i=1, 5)
        itimev = itime
        itimev(5) = itime(5) + 1
        call vtime(itimev, ierror)
        write (error_unit, *) (itimev(i), i=1, 5)
        !     +    write (error_unit,*) 'istep,nplume,npart: ',istep,nplume,npart
        !        write (error_unit,*)
        !        write (error_unit,*) 'istep,hour,npart=',istep,istep/72,npart

        !... calculate number of non zero model grids

        m = count(vcon > 0.0)

        !... write non zero model grids, their gegraphical coordinates and mass to output file

        ! b 19.05 start
        write (cfile, '(''concentrations-'',i2.2)') istep/72
        open (12, file=cfile)
        rewind 12
        write (12, '(i4,3i2.2)') (itimev(i), i=1, 4)
        write (12, '(i6,'' - non zero grids'')') m
        write (error_unit, *)
        write (error_unit, *) 'Output no.:', istep/72
        write (error_unit, *) 'Time (hrs): ', istep/12

        m = 0
        do i = 1, nx
          do j = 1, ny
            do k = 1, 3
              if (vcon(i, j, k) > 0.0) then
                m = m + 1
                x = real(i) + 0.5
                y = real(j) + 0.5
                call xyconvert(1, x, y, igtype, gparam, 2, geoparam, ierror)
                write (12, '(i6,2x,3i5,2x,2f10.3,2x,e12.3)') &
                  m, i, j, k, x, y, vcon(i, j, k)/72.0
                !              write (error_unit,'(i6,2x,3i5,2x,2f10.3,2x,e12.3)')
                !     &        m,i,j,k,x,y,vcon(i,j,k)/72.0
              endif
            enddo
          enddo
        enddo

        vcon = 0.0

        write (error_unit, *) 'npart all=', npart
        write (error_unit, *) 'ngrid all=', m
      endif
      close (12)
      ! b... END
#endif

      !..fields
      ifldout = 0
      isteph = isteph + 1
      if (isteph == nsteph) then
        isteph = 0
        if (nhrun > 0) then
          itime(5) = itime(5) + 1
        else
          itime(5) = itime(5) - 1
        end if
        do i = 1, 5
          itimeo(i) = itime(i)
        end do
        call vtime(itimeo, ierror)
        if (isynoptic == 0) then
          !..asynoptic output (use forecast length in hours to test if output)
          ihour = itime(5)
        else
          !..synoptic output  (use valid hour to test if output)
          ihour = itimeo(4)
        end if
        if (mod(ihour, nhfout) == 0) then
          ifldout = 1
          if (ifltim == 0) then
            !..identify fields with forecast length (hours after start)
            itimeo = itime
          end if
          !..save first and last output time
          ntimefo = ntimefo + 1
          write (iulog, *) 'fldout. ', itimeo
        end if
      end if

      !..field output if ifldout=1, always accumulation for average fields
      call output_timer%start()
      if (idailyout == 1) then
        !       daily output, append +x for each day
        ! istep/nsteph = hour  -> /24 =day
        write (fldfilN, '(a9,a1,I3.3)') fldfil, '+', istep/nsteph/24
        if (fldfilX /= fldfilN) then
          fldfilX = fldfilN
          if (fldtype == "netcdf") then
            call fldout_nc(-1, iunito, fldfilX, itime1, 0., 0., 0., tstep, &
                           (24/nhfout) + 1, nsteph, ierror)
          endif
          if (ierror /= 0) call snap_error_exit(iulog)
        end if
        if (fldtype == "netcdf") then
          call fldout_nc(ifldout, iunito, fldfilX, itimeo, tf1, tf2, tnext, &
                         tstep, istep, nsteph, ierror)
        endif
        if (ierror /= 0) call snap_error_exit(iulog)
      else
        if (fldtype == "netcdf") then
          call fldout_nc(ifldout, iunito, fldfil, itimeo, tf1, tf2, tnext, &
                         tstep, istep, nsteph, ierror)
        endif
        if (ierror /= 0) call snap_error_exit(iulog)
      end if
      call output_timer%stop_and_log()

      if (isteph == 0 .AND. iprecip < nprecip) iprecip = iprecip + 1

#if defined(TRAJ)
      ! b

      distance = distance + speed*tstep
      if (istep > 0 .AND. mod(istep, nsteph) == 0) then
        timeStart = [0, 0, itime1(4), itime1(3), itime1(2), itime1(1)]
        epochSecs = timeGm(timeStart)
        if (nhrun >= 0) then
          epochSecs = epochSecs + nint(istep*tstep)
        else
          epochSecs = epochSecs - nint(istep*tstep)
        endif
        timeCurrent = epochToDate(epochSecs)

        !        if(istep .gt. -1) then
        call vtime(itimev, ierror)
        !        write (error_unit,*) (itime(i),i=1,5), ierror
        !        write (error_unit,*) (itimev(i),i=1,5)
        !        write (error_unit,*) 'istep=',istep, 'npart=',npart
        !        do k=1,npart
        do k = 1, 1
          !        write (error_unit,*) istep,pdata(k)%x,pdata(k)%y,pdata(k)%z
          x = pdata(k)%x
          y = pdata(k)%y
          i = int(x(1))
          j = int(y(1))
          call xyconvert(1, x, y, igtype, gparam, 2, geoparam, ierror)
          vlvl = pdata(k)%z
          ilvl = vlvl*10000.
          k1 = ivlevel(ilvl)
          k2 = k1 + 1
          zzz = hlevel2(i, j, k2) + (hlevel2(i, j, k1) - hlevel2(i, j, k2))* &
                (pdata(k)%z - vlevel(k2))/(vlevel(k1) - vlevel(k2))
          !        write (error_unit,*) istep,x,y,pdata(k)%z,k1,k2,
          !     &  vlevel(k1),vlevel(k2),hlevel2(i,j,k1),hlevel2(i,j,k2),zzz
          !        write (error_unit,*)
          !        write (error_unit,*) istep,k,x,y,zzz
          !        write (error_unit,'(1x,i4,i2,i2,2i2.2,''00'')')
          !     &(itime(i),i=1,4),mod(istep,12)*5
          write (13, '(1x,i4,4i2.2,''00'', &
              &                  2f9.3,f12.3,f15.2,f10.2)') &
              timeCurrent(6), timeCurrent(5), timeCurrent(4), &
              timeCurrent(3), timeCurrent(2), &
              y, x, zzz, distance, speed
          write (error_unit, '(i4,1x,i4,i2,i2,2i2.2,''00'', &
              &                 2f9.3,f12.3,f15.2,f10.2)') istep, &
              timeCurrent(6), timeCurrent(5), timeCurrent(4), &
              timeCurrent(3), timeCurrent(2), &
              y, x, zzz, distance, speed
          ! b-2701
          flush (13)
        enddo
      endif
#endif
      call timeloop_timer%stop_and_log()
    end do time_loop
#if defined(TRAJ)
    close (13)

  end do
#endif

  if (lstepr < nstep .AND. lstepr < nstepr) then
    write (iulog, *) 'ERROR: Due to space problems the release period was'
    write (iulog, *) '       shorter than requested.'
    write (iulog, *) '   No. of requested release timesteps: ', nstepr
    write (iulog, *) '   No. of simulated release timesteps: ', lstepr
    write (error_unit, *) 'ERROR: Due to space problems the release period was'
    write (error_unit, *) '       shorter than requested.'
    write (error_unit, *) '   No. of requested release timesteps: ', nstepr
    write (error_unit, *) '   No. of simulated release timesteps: ', lstepr

    call snap_error_exit(iulog)
  end if

  ! b_240311
  write (error_unit, *)
  write (error_unit, '(''mhmax='',f10.2)') mhmax
  write (error_unit, '(''mhmin='',f10.2)') mhmin
  write (error_unit, *)

  call timeloop_timer%print_accumulated()
  call output_timer%print_accumulated()
  call input_timer%print_accumulated()
  call particleloop_timer%print_accumulated()
  ! b_end
  write (iulog, *) ' SNAP run finished'
  write (error_unit, *) ' SNAP run finished'

  if (iargos == 1) then
    close (91)
    close (92)
    close (93)
  end if

! deallocate all fields
  CALL deAllocateFields()

  close (iulog)

contains

  !> Exits snap ungracefully, leaving cleanup to the OS
  !>
  !> Additionally leaves the error msg on iulog if supplied
  subroutine snap_error_exit(iulog)
    !> Open file unit
    integer, intent(in), optional :: iulog

    character(len=*), parameter :: ERROR_MSG = '------- SNAP ERROR EXIT -------'

    write (error_unit, *) ERROR_MSG
    if (present(iulog)) then
      write (iulog, *) ERROR_MSG
      close (iulog)
    endif

    error stop ERROR_MSG
  end subroutine

  subroutine first_nonblank(str, n)
    character(len=*), intent(in) :: str
    integer, intent(out) :: n

    do n = 1, len(str)
      if (str(n:n) /= " ") then
        return
      end if
    end do

  end subroutine

  !> reads information from an inputfile and loads into the program
  subroutine read_inputfile(snapinput_unit)
    use snapparML, only: push_down_dcomp, defined_component, &
                         TIME_PROFILE_CONSTANT, TIME_PROFILE_LINEAR, TIME_PROFILE_LINEAR, TIME_PROFILE_STEPS, &
                         TIME_PROFILE_UNDEFINED
    use find_parameter, only: detect_gridparams, get_klevel
    use snapfimexML, only: parse_interpolator
#if defined(FIMEX)
    use find_parameters_fi, only: detect_gridparams_fi
#endif

    !> Open file unit
    integer, intent(in) :: snapinput_unit

    integer :: nlines
    logical :: end_loop
    integer :: ipostyp, k1, k2, ierror
    integer :: i, kh, kd, ig, igd, igm, i1, i2
    integer :: comment_start
    real :: glat, glong
    character(len=8) :: cpos1, cpos2
    type(defined_component), pointer :: d_comp
    integer :: kname_start, kname_end ! keyword
    integer :: pname_start, pname_end ! value
    integer :: equal_sign_position
    logical :: has_value

    character(len=:), allocatable :: cinput
    character(len=:), allocatable :: char_tmp
    character(len=256) :: keyword

    !..ipostyp=1 : latitude,longitude as decimal numbers (real)
    !..ipostyp=2 : latitude,longitude as degree*100+minute (integer)
    ipostyp = 1

    end_loop = .false.
    nlines = 0
    allocate (character(len=256)::cinput)

    do while (.not. end_loop)
      nlines = nlines + 1
      cinput(:) = ""
      i = 0
      read_line: do ! Read a line into cinput
        i = i + 1
        if (len(cinput) < i) then
          call move_alloc(from=cinput, to=char_tmp)
          allocate (character(len=len(char_tmp) + 256)::cinput)
          cinput(:) = ""
          cinput(:len(char_tmp)) = char_tmp(:)
          deallocate (char_tmp)
        endif
        read (snapinput_unit, iostat=ierror) cinput(i:i)
        if (ierror == IOSTAT_END) then
          end_loop = .true.
          exit read_line
        endif
        if (ierror /= 0) goto 11
        if (cinput(i:i) == new_line(cinput(i:i))) then
          cinput(:) = cinput(1:i - 1)
          exit read_line
        endif
      enddo read_line

      comment_start = index(cinput, '*')
      if (comment_start /= 0) then
        if (len_trim(cinput(:comment_start - 1)) == 0) then
          ! Empty line
          cycle
        end if
        kname_end = comment_start
      else
        if (len_trim(cinput) == 0) cycle
        kname_end = len_trim(cinput)
      end if
      call first_nonblank(cinput(:kname_end), kname_start)

      equal_sign_position = index(cinput(:kname_end), "=")
      if (equal_sign_position /= 0) then
        has_value = .true.
        pname_end = kname_end ! Only extending up to comment
        kname_end = equal_sign_position - 1
        pname_start = equal_sign_position + 1
        ! Can safely skip to the first nonblank
        call first_nonblank(cinput(pname_start:pname_end), k1)
        pname_start = pname_start + k1 - 1
      else
        has_value = .false.
        pname_start = 1
        pname_end = 1
      end if

      if ((kname_end - kname_start) > len(keyword)) then
        write (error_unit, *) "Keyword is too long"
        goto 12
      end if

      keyword(:) = trim(cinput(kname_start:kname_end))
      call chcase(1, 1, keyword) ! Lowercase

      ! write (error_unit,*) "keyword: ", trim(keyword)
      ! if (has_value) write (error_unit,*) "value: ", trim(cinput(pname_start:pname_end))

      select case (trim(keyword))
      case ('positions.decimal')
        !..positions.decimal
        ipostyp = 1
      case ('positions.degree_minute')
        !..positions.degree_minute
        ipostyp = 2
      case ('time.start')
        !..time.start=<year,month,day,hour>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) (itime1(i), i=1, 4)
        itime1(5) = 0
      case ('time.run')
        !..time.run=<hours'h'> or <days'd'>
        if (.not. has_value) goto 12
        kh = index(cinput(pname_start:pname_end), 'h')
        kd = index(cinput(pname_start:pname_end), 'd')
        if (kh > 0 .AND. kd == 0) then
          read (cinput(pname_start:pname_start + kh - 2), *, err=12) rnhrun
          nhrun = nint(rnhrun)
        elseif (kd > 0 .AND. kh == 0) then
          read (cinput(pname_start:pname_start + kd - 2), *, err=12) rnhrun
          nhrun = nint(rnhrun*24.)
        else
          goto 12
        end if
      case ('time.release')
        !..time.release=<hours'h'> or <days'd'>
        if (.not. has_value) goto 12
        kh = index(cinput(pname_start:pname_end), 'h')
        kd = index(cinput(pname_start:pname_end), 'd')
        if (kh > 0 .AND. kd == 0) then
          read (cinput(pname_start:pname_start + kh - 2), *, err=12) rnhrel
          nhrel = ceiling(rnhrel)
        elseif (kd > 0 .AND. kh == 0) then
          read (cinput(pname_start:pname_start + kd - 2), *, err=12) rnhrel
          nhrel = ceiling(rnhrel*24.)
        else
          goto 12
        end if
      case ('set_release.pos')
        !..set_release.pos=<name>   or <p=lat,long>
        if (.not. has_value) goto 12

        srelnam = trim(cinput(pname_start:pname_end))
        if (srelnam(1:2) == 'p=' .OR. srelnam(1:2) == 'P=') then
          pname_start = pname_start + 2
          read (cinput(pname_start:pname_end), *, err=12) glat, glong
          if (ipostyp == 2) then
            ig = nint(glat)
            igd = ig/100
            igm = ig - igd*100
            glat = float(igd) + float(igm)/60.
            ig = nint(glong)
            igd = ig/100
            igm = ig - igd*100
            glong = float(igd) + float(igm)/60.
          end if
          srelnam = ' '
          write (cpos1, fmt='(sp,f7.2,ss)') glat
          write (cpos2, fmt='(sp,f7.2,ss)') glong
          k1 = index(cpos1, '+')
          if (k1 > 0) then
            cpos1(8:8) = 'N'
          else
            k1 = index(cpos1, '-')
            cpos1(8:8) = 'S'
          end if
          k2 = index(cpos2, '+')
          if (k2 > 0) then
            cpos2(8:8) = 'E'
          else
            k2 = index(cpos2, '-')
            cpos2(8:8) = 'W'
          end if
          srelnam = cpos1(k1 + 1:8)//' '//cpos2(k2 + 1:8)
          if (nrelpos < size(release_positions)) nrelpos = nrelpos + 1
          release_positions(nrelpos)%name = srelnam
          release_positions(nrelpos)%geo_latitude = glat
          release_positions(nrelpos)%geo_longitude = glong
        end if
      case ('random.walk.on')
        !..random.walk.on
        use_random_walk = .true.
      case ('random.walk.off')
        !..random.walk.off
        use_random_walk = .false.
      case ('boundary.layer.full.mix.off')
        !..boundary.layer.full.mix.off
        blfullmix = .FALSE.
      case ('boundary.layer.full.mix.on')
        !..boundary.layer.full.mix.on
        blfullmix = .TRUE.
      case ('dry.deposition.old')
        !..dry.deposition.old
        if (idrydep /= 0 .AND. idrydep /= 1) goto 12
        idrydep = 1
      case ('dry.deposition.new')
        !..dry.deposition.new
        if (idrydep /= 0 .AND. idrydep /= 2) goto 12
        idrydep = 2
      case ('wet.deposition.old')
        write (error_unit, *) "This option is deprecated and removed"
        goto 12
      case ('wet.deposition.new')
        !..wet.deposition.new
        write (error_unit, *) "Deprecated, please use wet.deposition.version = 2"
        warning = .true.
        if (wetdep_version /= 0) then
          write (error_unit, *) "already set"
          goto 12
        endif
        wetdep_version = 2
      case ('wet.deposition.version')
        if (wetdep_version /= 0) then
          write (error_unit, *) "already set"
          goto 12
        endif
        if (.not. has_value) then
          write (error_unit, *) "expected a keyword"
          goto 12
        endif
        read (cinput(pname_start:pname_end), *, err=12) wetdep_version
      case ('time.step')
        !..time.step=<seconds>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) tstep
        if (tstep < 0.9999) goto 12

      case ('time.release.profile.constant')
        if (time_profile /= TIME_PROFILE_UNDEFINED) goto 12
        time_profile = TIME_PROFILE_CONSTANT
      case ('time.release.profile.bomb')
        if (time_profile /= TIME_PROFILE_UNDEFINED) goto 12
        time_profile = TIME_PROFILE_BOMB
      case ('time.release.profile.linear')
        if (time_profile /= TIME_PROFILE_UNDEFINED) goto 12
        time_profile = TIME_PROFILE_LINEAR
      case ('time.release.profile.steps')
        if (time_profile /= TIME_PROFILE_UNDEFINED) goto 12
        time_profile = TIME_PROFILE_STEPS

      case ('release.day', 'release.hour', 'release.minute', 'release.second')
        rscale = 1.
        if (keyword == 'release.day') rscale = 24.
        if (keyword == 'release.minute') rscale = 1./60.
        if (keyword == 'release.second') rscale = 1./3600.
        !..release.day
        !..release.hour
        !..release.minute
        !..release.second
        if (.not. has_value) goto 12
        if (.not. allocated(releases)) call allocate_releases(cinput(pname_start:pname_end), ntprof)

        read (cinput(pname_start:pname_end), *, err=12) releases%frelhour

        releases%frelhour = releases%frelhour*rscale

        do i = 2, ntprof
          if (releases(i - 1)%frelhour >= releases(i)%frelhour) then
            write (error_unit, *) 'ERROR: Release hours must be monotonically increasing'
            goto 12
          endif
        end do
      case ('release.radius.m')
        !..release.radius.m
        if (.not. has_value) goto 12
        if (.not. allocated(releases)) call allocate_releases(cinput(pname_start:pname_end), ntprof)

        call read_array_or_scalar(cinput(pname_start:pname_end), releases%relradius(1), ierror)
        if (ierror /= 0) goto 12
        if (any(releases%relradius(1) < 0.0)) goto 12

      case ('release.upper.m')
        !..release.upper.m
        if (.not. has_value) goto 12
        if (.not. allocated(releases)) call allocate_releases(cinput(pname_start:pname_end), ntprof)
        call read_array_or_scalar(cinput(pname_start:pname_end), releases%relupper(1), ierror)
        if (ierror /= 0) goto 12
        if (any(releases%relupper(1) < 0.0)) goto 12

      case ('release.lower.m')
        !..release.lower.m
        if (.not. has_value) goto 12
        if (.not. allocated(releases)) call allocate_releases(cinput(pname_start:pname_end), ntprof)
        call read_array_or_scalar(cinput(pname_start:pname_end), releases%rellower(1), ierror)
        if (ierror /= 0) goto 12
        if (any(releases%rellower(1) < 0.0)) goto 12

      case ('release.mushroom.stem.radius.m')
        !..release.mushroom.stem.radius.m
        if (.not. has_value) goto 12
        if (.not. allocated(releases)) call allocate_releases(cinput(pname_start:pname_end), ntprof)
        call read_array_or_scalar(cinput(pname_start:pname_end), releases%relstemradius, ierror)
        if (ierror /= 0) goto 12

      case ('release.bq/hour.comp', 'release.bq/sec.comp', 'release.bq/day.comp', 'release.bq/step.comp')
        !..release.bq/hour.comp
        !..release.bq/sec.comp
        !..release.bq/day.comp
        !..release.bq/step.comp
        if (keyword == 'release.bq/hour.comp') then
          rscale = 1./3600.
        elseif (keyword == 'release.bq/day.comp') then
          rscale = 1./(3600.*24.)
        elseif (keyword == 'release.bq/step.comp') then
          rscale = -1.
        else
          rscale = 1.
        end if
        if (.not. has_value) goto 12
        if (.not. allocated(releases)) call allocate_releases(cinput(pname_start:pname_end), ntprof)
        ncomp = ncomp + 1
        if (ncomp > mcomp) goto 13
        read (cinput(pname_start:pname_end), *, err=12) releases%relbqsec(ncomp, 1), &
          component(ncomp)
        if (any(releases%relbqsec(ncomp, 1) < 0.0)) goto 12
        if (rscale > 0.) then
          releases%relbqsec(ncomp, 1) = releases%relbqsec(ncomp, 1)*rscale
        elseif (ntprof > 1) then
          if (keyword == 'release.bq/step.comp') then
            do i = 1, ntprof - 1
              rscale = 1./(3600.*(releases(i + 1)%frelhour - releases(i)%frelhour))
              releases(i)%relbqsec(ncomp, 1) = releases(i)%relbqsec(ncomp, 1)*rscale
            end do
          end if
        end if

        !..releases with different height classes
        !..  height-classes defined here, time-profile defined outside
        !..release.heightlower.m
      case ('release.heightlower.m')
        nrelheight = 0
        if (.not. has_value .OR. nrelheight > 0) goto 12
        i1 = nrelheight + 1
        i2 = nrelheight
        ios = 0
        do while (ios == 0)
          if (i2 > size(release1%rellower)) goto 13
          i2 = i2 + 1
          read (cinput(pname_start:pname_end), *, iostat=ios) (release1%rellower(ih), ih=i1, i2)
        end do
        i2 = i2 - 1
        if (i2 < i1) goto 12
        nrelheight = i2
      case ('release.heightradius.m')
        !..release.heightradius.m
        if (.not. has_value .OR. nrelheight < 1) goto 12
        read (cinput(pname_start:pname_end), *, err=12) (release1%relradius(ih), ih=1, nrelheight)
        do ih = 1, nrelheight
          if (release1%relradius(ih) < 0.) goto 12
        end do
      case ('release.heightupper.m')
        !..release.heightupper.m
        if (.not. has_value .OR. nrelheight < 1) goto 12
        read (cinput(pname_start:pname_end), *, err=12) (release1%relupper(ih), ih=1, nrelheight)
        do ih = 1, nrelheight
          if (release1%relupper(ih) < release1%rellower(ih)) goto 12
        end do
      case ('release.file')
        if (.not. has_value) goto 12
        !..release.file
        relfile = trim(cinput(pname_start:pname_end))
        !..release.component
      case ('release.components')
        ncomp = 0
        if (.not. has_value .OR. ncomp > 0) goto 12
        i1 = ncomp + 1
        i2 = ncomp
        ios = 0
        do while (ios == 0)
          if (i2 > mcomp) goto 13
          i2 = i2 + 1
          read (cinput(pname_start:pname_end), *, iostat=ios) (component(i), i=i1, i2)
        end do
        i2 = i2 - 1
        if (i2 < i1) goto 12
        ncomp = i2

      case ('max.totalparticles')
        !..max.totalparticles
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) mpart
        if (mpart < 1) goto 12
      case ('max.totalplumes')
        !..max.totalplumes
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) mplume
        if (mplume < 1) goto 12
      case ('max.particles.per.release')
        !..max.particles.per.release
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) mprel
        if (mprel < 1) goto 12
      case ('component')
        !..component= name
        if (.not. has_value) goto 12
        call push_down_dcomp(def_comp, top=d_comp)

        d_comp%compname = cinput(pname_start:pname_end)
        d_comp%compnamemc = cinput(pname_start:pname_end)
        call chcase(2, 1, d_comp%compname)
      case ('dry.dep.on')
        !..dry.dep.on
        if (.not. associated(d_comp)) goto 12
        if (d_comp%kdrydep /= -1) goto 12
        d_comp%kdrydep = 1
      case ('dry.dep.off')
        !..dry.dep.off
        if (.not. associated(d_comp)) goto 12
        if (d_comp%kdrydep /= -1) goto 12
        d_comp%kdrydep = 0
      case ('wet.dep.on')
        !..wet.dep.on
        if (.not. associated(d_comp)) goto 12
        if (d_comp%kwetdep /= -1) goto 12
        d_comp%kwetdep = 1
      case ('wet.dep.off')
        !..wet.dep.off
        if (.not. associated(d_comp)) goto 12
        if (d_comp%kwetdep /= -1) goto 12
        d_comp%kwetdep = 0
      case ('dry.dep.height')
        !..dry.dep.height=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%drydephgt >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%drydephgt
      case ('dry.dep.ratio')
        !..dry.dep.ratio=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%drydeprat >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%drydeprat
      case ('wet.dep.ratio')
        !..wet.dep.ratio=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%wetdeprat >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%wetdeprat
      case ('radioactive.decay.on')
        !..radioactive.decay.on
        if (.not. associated(d_comp)) goto 12
        if (d_comp%kdecay /= -1) goto 12
        d_comp%kdecay = 1
      case ('radioactive.decay.off')
        !..radioactive.decay.off
        if (.not. associated(d_comp)) goto 12
        if (d_comp%kdecay /= -1) goto 12
        d_comp%kdecay = 0
      case ('half.lifetime.minutes')
        !..half.lifetime.minutes=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%halftime >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%halftime
        d_comp%halftime = d_comp%halftime/60.
      case ('half.lifetime.hours')
        !..half.lifetime.hours=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%halftime >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%halftime
      case ('half.lifetime.days')
        !..half.lifetime.days=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%halftime >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%halftime
        d_comp%halftime = d_comp%halftime*24.
      case ('half.lifetime.years')
        !..half.lifetime.years=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%halftime >= 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%halftime
        d_comp%halftime = d_comp%halftime*24.*365.25
      case ('gravity.off')
        !..gravity.off
        if (.not. associated(d_comp)) goto 12
        if (d_comp%grav_type /= -1) goto 12
        d_comp%grav_type = 0
      case ('gravity.fixed.m/s')
        !..gravity.fixed.m/s
        if (.not. associated(d_comp)) goto 12
        if (d_comp%grav_type /= -1) goto 12
        d_comp%grav_type = 1
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%gravityms
        if (d_comp%gravityms <= 0.) goto 12
      case ('gravity.fixed.cm/s')
        !..gravity.fixed.cm/s
        if (.not. associated(d_comp)) goto 12
        if (d_comp%grav_type /= -1) goto 12
        d_comp%grav_type = 1
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%gravityms
        if (d_comp%gravityms <= 0.) goto 12
        d_comp%gravityms = d_comp%gravityms*0.01
      case ('radius.micrometer')
        !..radius.micrometer  (for gravity computation)
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%radiusmym > 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%radiusmym
        if (d_comp%radiusmym <= 0.) goto 12
      case ('density.g/cm3')
        !..density.g/cm3  (for gravity computation)
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%densitygcm3 > 0.) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%densitygcm3
        if (d_comp%densitygcm3 <= 0.) goto 12
      case ('field.identification')
        !..field.identification=
        if (.not. has_value) goto 12
        if (.not. associated(d_comp)) goto 12
        if (d_comp%idcomp >= 0) goto 12
        read (cinput(pname_start:pname_end), *, err=12) d_comp%idcomp
        if (d_comp%idcomp < 1) goto 12
        do i = 1, size(def_comp) - 1
          if (def_comp(i)%idcomp == d_comp%idcomp) goto 12
        end do
      case ('field.use_model_wind_instead_of_10m')
        ! FIELD.USE_MODEL_WIND_INSTEAD_OF_10M= [.false.]/.true
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) met_params%use_model_wind_for_10m
      case ('precip(mm/h).probab')
        !..precip(mm/h).probab=<precip_intensity,probability, ...>
        write (error_unit, *) "precip(mm/h).probab is no longer used"
        warning = .true.
      case ('remove.relative.mass.limit')
        !..remove.relative.mass.limit=
        if (.not. has_value) goto 12
        if (rmlimit >= 0.00) goto 12
        read (cinput(pname_start:pname_end), *, err=12) rmlimit
        if (rmlimit < 0.0 .OR. rmlimit > 0.5) goto 12
      case ('particles.split.hours')
        !..split particles after hours
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) split_particle_hours
      case ('step.hour.input.min')
        !..step.hour.input.min=<hours>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) nhfmin
      case ('step.hour.input.max')
        !..step.hour.input.max=<hours>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) nhfmax
      case ('step.hour.output.fields')
        !..step.hour.output.fields=<hours>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) nhfout
      case ('synoptic.output')
        !..synoptic.output ... output at synoptic hours
        isynoptic = 1
      case ('asynoptic.output')
        !..asynoptic.output ... output at fixed intervals after start
        isynoptic = 0
      case ('total.components.off')
        !..total.components.off
        itotcomp = 0
      case ('total.components.on')
        !..total.components.on
        itotcomp = 1
      case ('mslp.on')
        !..mslp.on
        imslp = 1
      case ('mslp.off')
        !..mslp.off
        imslp = 0
      case ('precipitation.on')
        !..precipitation.on
        inprecip = 1
        met_params%need_precipitation = .true.
      case ('precipitation.off')
        !..precipitation.off
        inprecip = 0
        met_params%need_precipitation = .false.
      case ('model.level.fields.on')
        !..model.level.fields.on
        imodlevel = .true.
      case ('model.level.fields.off')
        !..model.level.fields.off
        imodlevel = .false.
      case ('model.level.fields.dumptime')
        !..levelfields are dump-data
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) modleveldump
      case ('release.pos')
        !..release.pos=<'name',latitude,longitude>
        if (.not. has_value) goto 12
        if (nrelpos < size(release_positions)) then
          nrelpos = nrelpos + 1
          read (cinput(pname_start:pname_end), *, err=12) release_positions(nrelpos)%name, &
            release_positions(nrelpos)%geo_latitude, &
            release_positions(nrelpos)%geo_longitude
          if (ipostyp == 2) then
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
          warning = .true.
          write (error_unit, *) 'WARNING. Too many RELEASE POSITIONS'
          write (error_unit, *) '  ==> ', cinput(pname_start:pname_end)
        end if
      case ('grid.input')
        !..grid.input=<producer,grid>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) iprod, igrid
      case ('grid.nctype')
        !..grid.nctype=<emep/hirlam12>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) nctype

        call init_meteo_params(nctype, ierror)
        if (ierror /= 0) goto 12
      case ('grid.size')
        !..grid.size=<nx,ny>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) nx, ny
      case ('grid.gparam')
        !..grid.gparam=<igtype,gparam(6)>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) igtype, (gparam(i), i=1, 6)
      case ('grid.run')
        !..grid.run=<producer,grid,ixbase,iybase,ixystp>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) iprodr, igridr, ixbase, iybase, ixystp
      case ('ensemble_member.input')
        read (cinput(pname_start:pname_end), *, err=12) enspos
      case ('data.sigma.levels')
        !..data.sigma.levels
        ivcoor = 2
      case ('data.eta.levels')
        !..data.eta.levels
        ivcoor = 10
      case ('levels.input')
        !..levels.input=<num_levels, 0,kk,k,k,k,....,1>
        !..levels.input=<num_levels, 0,kk,k,k,k,....,18,0,0,...>
        if (.not. has_value) goto 12
        if (nlevel /= 0) then
          write (error_unit, *) "re-assigning levels"
          DEALLOCATE(klevel, STAT=AllocateStatus)
        end if
        read (cinput(pname_start:pname_end), *, err=12) nlevel
        nk = nlevel
        ALLOCATE (klevel(nk), STAT=AllocateStatus)
        IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage
!         ALLOCATE ( ipcount(mdefcomp, nk), STAT = AllocateStatus)
!         IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage
!         ALLOCATE ( npcount(nk), STAT = AllocateStatus)
!         IF (AllocateStatus /= 0) ERROR STOP AllocateErrorMessage

        read (cinput(pname_start:pname_end), *, err=12) nlevel, (klevel(i), i=1, nlevel)
        if (klevel(1) /= 0 .OR. klevel(2) == 0) goto 12
        kadd = count(klevel(2:nk) == 0)
        do i = nk - kadd - 1, 2, -1
          if (klevel(i) <= klevel(i + 1)) goto 12
        end do
      case ('forecast.hour.min')
        !..forecast.hour.min= +6
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) minhfc
      case ('forecast.hour.max')
        !..forecast.hour.max= +32767
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) maxhfc
      case ('field.type')
        !..field.type felt, netcdf or fimex
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) ftype
      case ('fimex.config')
#if !defined(FIMEX)
        write (error_unit, *) "fimex.config is only used when compiled with fimex support"
        warning = .true.
#endif
        !..fimex.config config filename, only used when ftype=fimex
        if (.not. has_value) then
          fimex_config = ""
        else
          fimex_config = cinput(pname_start:pname_end)
        endif
      case ('fimex.file_type')
        !..fimex.file_type grib,netcdf,felt,ncml (known fimex filetypes), only used when ftype=fimex
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) fimex_type
      case ('fimex.interpolation')
        !..fimex.interpolation e.g. bilinear|+proj=stere +R=6371000 +lat_ts=60 +lat_0=0 +no_defs|0,1000,...,50000|0,1000,...,50000|m
        if (.not. has_value) goto 12
        fimex_interpolation = cinput(pname_start:pname_end)
        if (ftype == "fimex") then
          if (parse_interpolator() == 1) then
            write (error_unit, *) 'interpolation input wrong:', trim(fimex_interpolation)
            ierror = 1
          else
            write(error_unit,*) "interpolation enabled:", fint%method, trim(fint%proj), trim(fint%x_axis), &
              trim(fint%y_axis), fint%unit_is_degree
          end if
        else
          write(error_unit,*) "fimex.interpolation ignored when field.type not fimex: ", trim(ftype)
        end if
      case ('field.input')
        !..field.input=  felt_file_name
        if (.not. has_value) goto 12
        if (nfilef < size(limfcf, 2)) then
          nfilef = nfilef + 1
          limfcf(1, nfilef) = minhfc
          limfcf(2, nfilef) = maxhfc
          if (cinput(pname_start:pname_start) == '''' .OR. &
              cinput(pname_start:pname_start) == '"') then
            read (cinput(pname_start:pname_end), *, err=12) filef(nfilef)
          else
            filef(nfilef) = cinput(pname_start:pname_end)
          endif
        else
          warning = .true.
          write (error_unit, *) 'WARNING. Too many FIELD INPUT files'
          write (error_unit, *) '  ==> ', cinput(pname_start:pname_end)
        end if
      case ('field.spinupsteps')
        !..field.spinupsteps= 1
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) spinup_steps
        if (spinup_steps < 0) goto 12
      case ('field_time.forecast')
        !..field_time.forecast ... use forecast length in output field ident.
        ifltim = 0
      case ('field_time.valid')
        !..field_time.valid ...... use valid time in output field identification
        ifltim = 1
      case ('field.output')
        !..field.output= <'file_name'>
        if (.not. has_value) goto 12
        fldfil = cinput(pname_start:pname_end)
      case ('field.outtype')
        !..field.outtype= <'felt|netcdf'>
        if (.not. has_value) goto 12
        fldtype = cinput(pname_start:pname_end)
      case ('title')
        allocate (character(len=len_trim(cinput(pname_start:pname_end))) :: nctitle)
        nctitle(:) = trim(cinput(pname_start:pname_end))
      case ('field.daily.output.on')
        idailyout = 1
      case ('field.daily.output.off')
        idailyout = 0
      case ('simulation.start.date')
        if (.not. has_value) goto 12
        simulation_start = cinput(pname_start:pname_end)
      case ('log.file')
        !..log.file= <'log_file_name'>
        if (.not. has_value) goto 12
        logfile = cinput(pname_start:pname_end)
      case ('debug.off')
        !..debug.off
        idebug = 0
      case ('debug.on')
        !..debug.on
        idebug = 1
      case ('ensemble.project.output.off')
        write (error_unit, *) "ensemble.project is deprecated, key is not used"
        warning = .true.
      case ('argos.output.off')
        !..argos.output.off
        iargos = 0
      case ('argos.output.on')
        !..argos.output.on
        iargos = 1
      case ('argos.output.deposition.file')
        !..argos.output.deposition.file= runident_MLDP0_depo
        if (.not. has_value) goto 12
        argosdepofile = cinput(pname_start:pname_end)
      case ('argos.output.concentration.file')
        !..argos.output.concentration.file= runident_MLDP0_conc
        if (.not. has_value) goto 12
        argosconcfile = cinput(pname_start:pname_end)
      case ('argos.output.totaldose.file')
        !..argos.output.totaldose.file= runident_MLDP0_dose
        if (.not. has_value) goto 12
        argosdosefile = cinput(pname_start:pname_end)
      case ('argos.output.timestep.hour')
        !..argos.output.timestep.hour= <3>
        if (.not. has_value) goto 12
        read (cinput(pname_start:pname_end), *, err=12) argoshourstep
        if (argoshourstep <= 0 .OR. argoshourstep > 240) goto 12
      case ('grid.autodetect.from_input')
        if (nfilef < 1) then
          write (error_unit, *) "grid.autodetect requires at least one field.input to be set"
          goto 12
        endif
        if (nctype == "*") then
          write (error_unit, *) "grid.autodetect requires an nctype to be set"
          goto 12
        endif
        if (ftype .eq. "fimex") then
#ifdef FIMEX
          call detect_gridparams_fi(filef(1), met_params%xwindv, nx, ny, igtype, gparam, klevel, ierror)
          if (ierror /= 0) then
            write (error_unit, *) "Could not detect gridparams"
            goto 12
          endif
#else
          write (error_unit, *) "fimex required, but not compiled"
          goto 12
#endif
        else
          call detect_gridparams(filef(1), nx, ny, igtype, gparam, ierror)
          if (ierror /= 0) then
            write (error_unit, *) "Autodetection did not work"
            goto 12
          endif
          call get_klevel(filef(1), klevel, ierror)
          if (ierror /= 0) then
            write (error_unit, *) "Autodetection did not work"
            goto 12
          endif
        end if
        nlevel = size(klevel)
        nk = nlevel
        write (error_unit, *) "autodetection of grid-param: ", gparam
      case ('end')
        !..end
#if defined(TRAJ)
        allocate (character(len=1024)::char_tmp)
        call read_line_no_realloc(snapinput_unit, char_tmp, ierror)
        if (ierror /= 0) goto 12

        read (char_tmp, *) ntraj
        write (error_unit, *) 'ntraj=', ntraj
        do i = 1, ntraj
          call read_line_no_realloc(snapinput_unit, char_tmp, ierror)
          if (ierror /= 0) goto 12
          read (char_tmp, *) tlevel(i)
          write (error_unit, *) tlevel(i)
        enddo
        do i = 1, ntraj
          call read_line_no_realloc(snapinput_unit, char_tmp, ierror)
          if (ierror /= 0) goto 12
          read (char_tmp, '(a80)') tname(i)
          write (error_unit, '(i4,1x,a80)') i, tname(i)
        enddo
#endif
        end_loop = .true.
      case default
        write (error_unit, *) 'ERROR.  Unknown input:'
        write (error_unit, *) cinput
        goto 12
      end select
    end do

    return

11  write (error_unit, *) 'ERROR reading file: ', trim(finput)
    write (error_unit, *) 'At line no. ', nlines
    call snap_error_exit()

12  write (error_unit, *) 'ERROR reading file: ', trim(finput)
    write (error_unit, *) 'At line no. ', nlines, ' :'
    write (error_unit, *) trim(cinput)
    call snap_error_exit()

13  write (error_unit, *) 'ERROR reading file:'
    write (error_unit, *) trim(finput)
    write (error_unit, *) 'At line no. ', nlines, ' :'
    write (error_unit, *) trim(cinput)
    write (error_unit, *) 'SOME LIMIT WAS EXCEEDED !!!!!!!!!!!!!!!!!'
    call snap_error_exit()

  end subroutine

  subroutine allocate_releases(string_with_commas, nelems)
    !> Some comma separated values
    character(len=*), intent(in) :: string_with_commas
    !> Number of elements
    integer, intent(out) :: nelems

    nelems = 0
    do i = 1, len_trim(string_with_commas)
      if (string_with_commas(i:i) == ',') nelems = nelems + 1
    enddo
    nelems = nelems + 1 ! Fencepost

    allocate (releases(nelems))
    do i = 1, nelems
      releases(i)%frelhour = -1.0
      releases(i)%relradius = -1.0
      releases(i)%relupper = -1.0
      releases(i)%rellower = -1.0
      releases(i)%relstemradius = -1.0
      releases(i)%relbqsec = -1.0
    enddo
  end subroutine

#ifdef TRAJ
  !> Reads into a string from a file, returning if
  !> reaching newline/end of file/len(str)
  subroutine read_line_no_realloc(snapinput_unit, str, ierror)
    !> Open file unit
    integer, intent(in) :: snapinput_unit
    !> String read into
    character(len=*), intent(out) :: str
    !> 0 if no error
    integer, intent(out) :: ierror

    integer :: i

    ierror = 0
    str(:) = ""
    i = 0
    do
      i = i + 1
      if (i > len(str)) then
        ierror = 1
        return
      endif
      read (snapinput_unit, iostat=ierror) str(i:i)
      if (ierror == IOSTAT_END) then
        ierror = 0
        return
      endif
      if (ierror /= 0) return
      if (str(i:i) == new_line(str(i:i))) then
        str(:) = str(1:i - 1)
        return
      endif
    enddo
  end subroutine
#endif

!> checks the data from the input for errors or missing information,
!> and copies information to structures used when running the program.
  subroutine conform_input(ierror)
    use snapparML, only: TIME_PROFILE_UNDEFINED
    use snapfimexML, only: parse_interpolator
    integer, intent(out) :: ierror

    integer :: i1, i2

    logical :: error_release_profile

    ierror = 0
    do n = 1, nrelpos
      if (srelnam == release_positions(n)%name) irelpos = n
    end do
    if (irelpos == 0 .AND. nrelpos == 1) irelpos = 1
    if (irelpos == 0) then
      write (error_unit, *) 'No (known) release position selected'
      ierror = 1
    end if

    if (ivcoor == 0) then
      write (error_unit, *) 'Input model level type (sigma,eta) not specified'
      ierror = 1
    end if
    if (nlevel == 0) then
      write (error_unit, *) 'Input model levels not specified'
      ierror = 1
    end if
    if (ftype /= "netcdf" .AND. ftype /= "fimex") then
      write (error_unit, *) 'Input type not netcdf or fimex:', trim(ftype)
      ierror = 1
    end if
    if (fldtype /= "netcdf") then
      write (error_unit, *) 'Output type not netcdf:', trim(fldtype)
      ierror = 1
    end if
    if (nfilef == 0) then
      write (error_unit, *) 'No input field files specified'
      ierror = 1
    end if

    if (.not. allocated(releases)) then
      write (error_unit, *) 'No time profile(s) specified'
      ierror = 1
      ntprof = 0
    end if
    if (nhrel == 0 .AND. ntprof > 0) then
      rnhrel = releases(ntprof)%frelhour
      nhrel = ceiling(rnhrel)
    endif

    if (time_profile == TIME_PROFILE_UNDEFINED) then
      write (error_unit, *) 'No time profile type specified'
      ierror = 1
    end if

    error_release_profile = .false.
    do i = 1, ntprof
      if (releases(i)%relradius(1) < 0. .OR. &
          releases(i)%relupper(1) < 0. .OR. &
          releases(i)%rellower(1) < 0. .OR. &
          releases(i)%relupper(1) < releases(i)%rellower(1)) then
        error_release_profile = .true.
      endif
      if (releases(i)%relupper(1) < releases(i)%rellower(1) + 1.) then
        releases(i)%relupper(1) = releases(i)%rellower(1) + 1.
      endif
    end do
    if (error_release_profile) then
      write (error_unit, *) 'ERROR in relase profiles ', &
          &'of upper,lower and/or radius'
      ierror = 1
    end if

    do i = 1, ntprof - 1
      if ((releases(i + 1)%frelhour - releases(i)%frelhour)*3600 < tstep) then
        warning = .true.
        write (error_unit, *) "WARNING: Release interval is shorter than timestep; ", &
          "some releases may be skipped"
        exit
      endif
    enddo

    if (ncomp < 0) then
      write (error_unit, *) 'No (release) components specified for run'
      ierror = 1
    end if
    if (.not. allocated(def_comp)) then
      write (error_unit, *) 'No (release) components defined'
      ierror = 1
    else
      if (ncomp > size(def_comp)) then
        write (error_unit, *) "Number of RELEASE.BQ components is higher than the"
        write (error_unit, *) "number of defined components"
        ierror = 1
      end if
      if (maxval(def_comp%idcomp) > ncomp) then
        write (error_unit, *) "Warn: Field identification is higher than total number of fields: ", &
          maxval(def_comp%idcomp), " > ", ncomp
      end if

      do m = 1, size(def_comp) - 1
        if (def_comp(m)%idcomp < 1) then
          write (error_unit, *) 'Component has no field identification: ', &
            trim(def_comp(m)%compname)
        end if
        do i = m + 1, size(def_comp)
          if (def_comp(m)%compname == def_comp(i)%compname) then
            write (error_unit, *) 'Component defined more than once: ', &
              trim(def_comp(m)%compname)
            ierror = 1
          end if
        end do
      end do
    endif

    do m = 1, ncomp - 1
      do i = m + 1, ncomp
        if (component(m) == component(i)) then
          write (error_unit, *) 'Released component defined more than once: ', &
            trim(component(m))
          ierror = 1
        end if
      end do
    end do

!..match used components with defined components

    do m = 1, ncomp
      k = 0
      do i = 1, size(def_comp)
        if (component(m) == def_comp(i)%compname) k = i
      end do
      if (k > 0) then
        run_comp(m)%to_defined = k
        run_comp(m)%defined => def_comp(k)
        def_comp(k)%to_running = m
      else
        write (error_unit, *) 'Released component ', &
          trim(component(m)), ' is not defined'
        ierror = 1
      end if
    end do

!..gravity
    do n = 1, ncomp
      m = run_comp(n)%to_defined
      if (m == 0) cycle
      if (def_comp(m)%grav_type < 0) def_comp(m)%grav_type = 2
      if (def_comp(m)%grav_type == 2 .AND. &
          (def_comp(m)%radiusmym <= 0. .OR. def_comp(m)%densitygcm3 <= 0.)) then
        write (error_unit, *) 'Gravity error. radius,density: ', &
          def_comp(m)%radiusmym, def_comp(m)%densitygcm3
        ierror = 1
      end if
    end do

    if (idrydep == 0) idrydep = 1
    if (wetdep_version == 0) then ! Set default wetdep version
      wetdep_version = 2
    endif
    if (wetdep_version /= 2) then
      write (error_unit, *) "Unknown wet deposition version"
      ierror = 1
    endif
    i1 = 0
    i2 = 0
    idecay = 0

    do n = 1, ncomp
      m = run_comp(n)%to_defined
      if (m == 0) cycle
      if (idrydep == 1 .AND. def_comp(m)%kdrydep == 1) then
        if (def_comp(m)%drydeprat > 0. .AND. def_comp(m)%drydephgt > 0.) then
          i1 = i1 + 1
        else
          write (error_unit, *) 'Dry deposition error. rate,height: ', &
            def_comp(m)%drydeprat, def_comp(m)%drydephgt
          ierror = 1
        end if
      elseif (idrydep == 2 .AND. def_comp(m)%kdrydep == 1) then
        if (def_comp(m)%grav_type == 1 .AND. def_comp(m)%gravityms > 0.) then
          i1 = i1 + 1
        elseif (def_comp(m)%grav_type == 2) then
          i1 = i1 + 1
        else
          write (error_unit, *) 'Dry deposition error. gravity: ', &
            def_comp(m)%gravityms
          ierror = 1
        end if
      end if

      if (wetdep_version == 2 .AND. def_comp(m)%kwetdep == 1) then
        if (def_comp(m)%radiusmym > 0.) then
          i2 = i2 + 1
        else
          write (error_unit, *) 'Wet deposition error. radius: ', &
            def_comp(m)%radiusmym
          ierror = 1
        end if
      end if

      if (def_comp(m)%kdecay == 1 .AND. def_comp(m)%halftime > 0.) then
        idecay = 1
      else
        def_comp(m)%kdecay = 0
        def_comp(m)%halftime = 0.0
      end if
    end do

    if (i1 == 0) idrydep = 0

    if (itotcomp == 1 .AND. ncomp == 1) itotcomp = 0

    if (rmlimit < 0.0) rmlimit = 0.0001

    if (iprodr == 0) iprodr = iprod
    if (igridr == 0) igridr = igrid

  end subroutine

  ! Reads an array from str to arr, or broadcast a scalar
  ! from str to arr
  pure subroutine read_array_or_scalar(str, arr, stat)
    character(len=*), intent(in) :: str
    real, intent(out) :: arr(:)
    integer, intent(out) :: stat

    read (str, *, iostat=stat) arr
    if (stat == 0) return
    ! The IO error might be due to size mismatch, try reading a scalar
    read (str, *, iostat=stat) arr(1)
    if (stat /= 0) return

    ! Broadcast the scalar to arr
    arr(2:) = arr(1)
  end subroutine
END PROGRAM
