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
  use snapdimML, only: mdefcomp, mcomp
  implicit none

  private

!> an identifier used in the field identification
!>
!> (stored as level_1 for single level fields,
!> model level fields adds this number to a 'basic'
!> parameter no., 0 is used for the total if more than
!> one component present)
  integer, save, public :: idcomp(mdefcomp)

!> mapping from defined component to the running component
!>
!> used when mapping from component given by e.g. ::particle\%icomp
!> to defined components in e.g. ::kgravity
!>
!> should be 0 for components that are not running
  integer, save, public :: defined_to_running_comp(mdefcomp)

!> counter for unique particle identifier
  integer, save, public :: nparnum
!> gravity type:
!>
!> * 0=off
!> * 1=fixed
!> * 2=computed
  integer, save, public :: kgravity(mdefcomp)
!> fixed gravity in unit m/s
  real, save, public :: gravityms(mdefcomp)

!> total release in unit Bq, accumulated during run
  real, save, public :: totalbq(mdefcomp)
!> a component name (not much used, really)
  character(len=32), save, public :: compname(mdefcomp)
  character(len=32), save, public :: compnamemc(mdefcomp)


!>   no. of components used in the run
  integer, save, public :: ncomp

!>  time profile type:
!> * 1= constant
!> * 2= bomb (only one initial release)
!> * 3= linear (between specified timesteps)
!> * 4= steps  (constant between the specified timesteps)
  integer, save, public :: itprof
!>  component name
  character(len=32), save, public :: component(mcomp)


!> Mapping from running components to the defined compont
!>
!> Used when looping over all running components to pick
!> the corresponding defined component
  integer, save, public :: running_to_defined_comp(mcomp)


!> component no. in particle
  integer, dimension(:), allocatable, save, public :: icomp
!> unique identifier for each particle
  integer, dimension(:), allocatable, save, public :: iparnum

end module snapparML
