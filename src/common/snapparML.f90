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
  use snapdimML, only: mcomp
  implicit none

  private

  public push_down_dcomp


!> Information defining a component
  type, public :: defined_component
!> mapping from defined component to the running component
!>
!> used when mapping from component given by e.g. particleml::particle::icomp
!> to the currenctly running component in ::run_comp
!>
!> should be 0 for components that are not running
    integer :: to_running = 0
!> gravity type:
!>
!> * 0=off
!> * 1=fixed
!> * 2=computed
    integer :: grav_type = -1

!> fixed gravity in unit m/s, used when \ref grav_type != 2
    real :: gravityms = 0.0
!> radius in unit micrometer
    real :: radiusmym = 0.0
!> density in unit g/cm3
    real :: densitygcm3 = 0.0

!> for each component: 0=radioactive decay off  1=decay on
    integer :: kdecay = -1
!> radioactive half lifetime (hours)
    real :: halftime = -1.0
!> radioactive decay (rate)
    real :: decayrate = -1.0

!> wet deposition rate
    real :: wetdeprat = -1.0
!> for each component: 0=wet deposition off  1=wet dep. on
    integer :: kwetdep = -1

!> dry deposition rate (used in ::drydep::drydep1)
    real :: drydeprat = -1.0
!> max height above ground for dry deposition (used in ::drydep::drydep1)
    real :: drydephgt = -1.0
!> for each component: 0=dry deposition off  1=dry dep. on
    integer :: kdrydep = -1


!> a component name
    character(len=32) :: compname = "Unknown"
!> a component name (mixed case)
    character(len=32) :: compnamemc = "Unknown"

!> an identifier used in the field identification
!>
!> (stored as level_1 for single level fields,
!> model level fields adds this number to a 'basic'
!> parameter no., 0 is used for the total if more than
!> one component present)
    integer :: idcomp = -1

!> Commited dose per unit intake
!>
!> Unit of Sv/Bq
!> See e.g. ICRP Publication 119
    real :: dpui = -1.0
  end type
  type(defined_component), save, allocatable, public, target :: def_comp(:)

!> counter for unique particle identifier
  integer, save, public :: nparnum



!>   no. of components used in the run
  integer, save, public :: ncomp = 0

  integer, parameter, public :: TIME_PROFILE_CONSTANT = 1, &
                                !> bomb (only one initial release)
                                TIME_PROFILE_BOMB = 2, &
                                !> linear (between specified timesteps)
                                TIME_PROFILE_LINEAR = 3, &
                                !> constant between the specified timesteps
                                TIME_PROFILE_STEPS = 4, &
                                TIME_PROFILE_UNDEFINED = -1
!>  time profile of release
  integer, save, public :: time_profile = TIME_PROFILE_UNDEFINED

!>  component name
  character(len=32), save, public :: component(mcomp)

!> Contains information which are applicable to active (running)
!> components
  type, public :: running_component
!> Mapping from running components to the defined compont
!>
!> Used when looping over all running components to pick
!> the corresponding defined component
    integer :: to_defined

!> Pointer to the definition of the component
    type(defined_component), pointer :: defined

!> total release in unit Bq, accumulated during run
    real :: totalbq

!>  total no. of particles released, accumulated during run
    integer :: numtotal

!> deposition constant for use in wetdep
    real :: depconst
  end type

  type(running_component), save, allocatable, public :: run_comp(:)


!> unique identifier for each particle
  integer, dimension(:), allocatable, save, public :: iparnum

  contains
    !> Pushes a default component onto the array
    subroutine push_down_dcomp(arr, top)
      !> Growable array, increases by one every call
      type(defined_component), intent(inout), allocatable, target :: arr(:)
      !> pointer to the top/last element
      type(defined_component), pointer, intent(out), optional :: top

      type(defined_component), allocatable :: old_arr(:)
      integer :: s

      if (.not.allocated(arr)) then
        allocate(arr(1))
        arr(1) = defined_component()
        if (present(top)) then
          top => arr(1)
        endif
        return
      endif

      s = size(arr)
      call move_alloc(from=arr, to=old_arr)
      allocate(arr(s+1))

      arr(:s) = old_arr
      arr(s+1) = defined_component()
      if (present(top)) then
        top => arr(s+1)
      endif
    end subroutine

end module snapparML
