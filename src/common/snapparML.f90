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

!> Common for particles
module snapparML
  use snapdimML
  implicit none

  private

!> an identifier used in the field identification
!>      (stored as level_1 for single level fields,
!>       model level fields adds this number to a 'basic'
!>       parameter no., 0 is used for the total if more than
!>       one component present)
      integer, save, public :: idcomp(mdefcomp)
!>  run comp. (1,...ncomp) or 0 if not used
      integer, save, public :: iruncomp(mdefcomp)

!..nparnum:   counter for unique particle identifier
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

!..totalbq:   total release in unit Bq, accumulated during run
!..numtotal:  total no. of particles released, accumulated during run
!..compname:  a component name (not much used, really)
!
      integer, save, public :: nparnum
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
      character(len=32), save, public :: compname(mdefcomp),compnamemc(mdefcomp)


!>   no. of components used in the run
  integer, save, public :: ncomp

!..itprof:  time profile type:
!             1= constant
!             2= bomb (only one initial release)
!             3= linear (between specified timesteps)
!             4= steps  (constant between the specified timesteps)
!..component(n):    component name
!..idefcomp(n):     component no. as defined in input file sequence

      integer, save, public :: itprof
      integer, save, public :: idefcomp(mcomp)
      character(len=32), save, public :: component(mcomp)


!..icomp:   component no. in particle
!..iparnum: unique identifier for each particle
      INTEGER, DIMENSION(:), allocatable, save, public :: icomp, iparnum

end module snapparML
