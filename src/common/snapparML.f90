! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
! 
! This file is part of SNAP. SNAP is free software: you can 
! redistribute it and/or modify it under the terms of the 
! GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!

module snapparML
  use snapdimML
  implicit none

  private

!-----------------------------------------------------------------------
! snappar.inc
!
!..include file  -  common for particles
!
!
!..ndefcomp:  no. of components defined (aerosol,gas,noble.gas,...)
!..nplume:    no. of released plumes
!..npart:     total no. of particles (in all released plumes)
!..nparnum:   counter for unique particle identifier
!..iplume:    pointers to first and last particle in each plume
!.            (0,-1 means no particles left in the grid domain)
!..kdrydep:   for each component: 0=dry deposition off  1=dry dep. on
!..kwetdep:   for each component: 0=wet deposition off  1=wet dep. on
!..kdecay:    for each component: 0=radioactive decay off  1=decay on
!..kgravity:  gravity type: 0=off
!                           1=fixed
!                           2=computed
!..drydephgt: max height above ground for dry deposition
!..drydeprat: dry deposition rate
!..wetdeprat: wet deposition rate
!..halftime:  radioactive half lifetime (hours)
!..decayrate: radioactive decay (rate)
!..gravityms:   fixed gravity in unit m/s
!..radiusmym:   radius in unit micrometer (for gravity computation)
!..densitygcm3: density in unit g/cm3     (for gravity computation)
!..vgtable:     table of gravity in m/s
!		(temperature as first index, pressure second)
!..idcomp:    an identifier used in the field identification
!	      (stored as level_1 for single level fields,
!	       model level fields adds this number to a 'basic'
!	       parameter no., 0 is used for the total if more than
!	       one component present)
!..iruncomp:  run comp. (1,...ncomp) or 0 if not used
!..totalbq:   total release in unit Bq, accumulated during run
!..numtotal:  total no. of particles released, accumulated during run
!..compname:  a component name (not much used, really)
!
      integer, save, public :: ndefcomp,nplume,npart,nparnum, mpart, mplume
!      integer      iplume(2,mplume)
      INTEGER, DIMENSION(:,:), allocatable, save, public :: iplume
      integer, save, public :: kdrydep(mdefcomp),kwetdep(mdefcomp),kdecay(mdefcomp)
      integer, save, public :: kgravity(mdefcomp)
      real, save, public :: drydephgt(mdefcomp) &
                 ,drydeprat(mdefcomp),wetdeprat(mdefcomp) &
                 ,halftime(mdefcomp),decayrate(mdefcomp) &
                 ,gravityms(mdefcomp) &
                 ,radiusmym(mdefcomp),densitygcm3(mdefcomp) &
                 ,vgtable(numtempvg,numpresvg,mdefcomp) &
                 ,tbasevg,tincrvg,pbasevg,pincrvg &
                 ,totalbq(mdefcomp)
      integer, save, public :: numtotal(mdefcomp)
      integer, save, public :: idcomp(mdefcomp),iruncomp(mdefcomp)
      character(len=32), save, public :: compname(mdefcomp),compnamemc(mdefcomp)
!
!
!
!..itprof:  time profile type:
!             1= constant
!             2= bomb (only one initial release)
!             3= linear (between specified timesteps)
!             4= steps  (constant between the specified timesteps)
!..ntprof:  no. of timesteps in the release profiles
!..ncomp:   no. of components used in the run
!..nrelheight: no. of height classes in the run
!..mprel:   max no. of particles released in each plume
!              (scaled according to max mass released
!               and divided between components according to mass)
!..component(n):    component name
!..idefcomp(n):     component no. as defined in input file sequence
!..frelhour(i):     specified release hours in time profile
!..relradius(i,h):    release radius in unit meter
!..relupper(i,h):     release upper height in unit meter
!..rellower(i,h):     release lower height in unit meter
!..relstemradius(i): release radius in unit meter for a mushroom stem
!..relbqsec(i,n,h):   radioactive release in unit Bq/sec
!..      i=1,ntprof
!..      n=1,ncomp
!..      h=1,nrelheight
!
      integer, save, public :: itprof,ntprof,ncomp,nrelheight,mprel
      integer, save, public :: idefcomp(mcomp)
      real, save, public :: frelhour(mtprof)
      real, save, public :: relradius(mtprof,mrelheight)
      real, save, public :: relupper(mtprof,mrelheight)
      real, save, public :: rellower(mtprof,mrelheight)
      real, save, public :: relstemradius(mtprof)
      real, save, public :: relbqsec(mtprof,mcomp,mrelheight)
      character(len=32), save, public :: component(mcomp)
!
!
!..pdata:   now defined in particleML.f90, no more npdata
!..pwork:   work array, currently also used to transfer wind components
!                       (u,v) for 'forwrd' to 'rwalk'
!..icomp:   component no. in particle
!..iparnum: unique identifier for each particle
!..ipwork:  work array
!
!      REAL, DIMENSION(:,:), POINTER :: pwork
      INTEGER, DIMENSION(:), allocatable, save, public :: icomp, iparnum
!
!     -  pwork, ipwork
!
end module snapparML
