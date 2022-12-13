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

module particleML
    USE iso_fortran_env, only: real64, int16
    implicit none
    private

    !> numerical limit for particle-rad contents so that common computations
    !> like decay or deposition don't become subnormal
    !> (smallest possible normal float: ~1e-38)
    real, parameter :: numeric_limit_rad = 1e-35

!> a simple particle to be stored
    type, public :: particle
        !> x position in grid
        real(real64)   :: x
        !> y position in grid
        real(real64)   :: y
        !> sigma/eta position (vertical)
        real(real64)   :: z
        !> sigma/eta at top of boundary layer
        real           :: tbl
        !> radioactive content (Bq)
        real, private  :: rad_
        !> height of boundary layer
        real           :: hbl
        !> gravity in m/s (fixed or computed)
        real           :: grv = 0.
        !> index to the defined component
        integer(int16) :: icomp
        contains
          procedure :: scale_rad, set_rad, rad => get_rad, add_rad, get_set_rad
          procedure :: is_inactive, is_active, inactivate
          procedure, private :: flush_away_denormal
    end type particle

!> storage for extra particle data
    type, public :: extraParticle
        sequence
        !> u-speed
        real         :: u
        !> v-speed
        real         :: v
        !> map ratio in x direction
        real(real64) :: rmx
        !> map ration in y direction
        real(real64) :: rmy
        !> precipition intensity (mm/hour)
        real         :: prc
    end type extraParticle

!> the actual particle storage
!>
!> will be allocated in allocatefieldsml::allocatefields
    type(particle), allocatable, public, save :: pdata(:)

    contains
      !> Scale activity of the particle,
      !>
      !> Activity remaining in the particle is
      !> factor * original
      !> The "lost" activity is returned
      real function scale_rad(p, factor) result(removed)
        class(particle), intent(inout) :: p
        real, value :: factor
        real :: previous

        if (p%is_inactive()) then
          removed = 0.0
          return
        endif

        factor = min(factor, 1.0)
        factor = max(factor, 0.0)

        previous = p%rad_
        p%rad_ = factor * previous
        call p%flush_away_denormal()

        removed = previous - p%rad_
      end function

      !> Set activity of particle
      subroutine set_rad(p, rad)
        class(particle), intent(inout) :: p
        real, intent(in) :: rad
        p%rad_ = rad
      end subroutine

      !> Get activity of particle
      real pure function get_rad(p)
        class(particle), intent(in) :: p
        get_rad = p%rad_
      end function

      !> Get current activity and set activity
      real function get_set_rad(p, rad) result(previous)
        class(particle), intent(inout) :: p
        real, intent(in) :: rad
        previous = p%rad_
        p%rad_ = rad
      end function

      !> Add activity to the particle
      subroutine add_rad(p, rad)
        class(particle), intent(inout) :: p
        real, intent(in) :: rad
        p%rad_ = p%rad_ + rad
      end subroutine

      !> Deactivate a particle
      !>
      !> Lost activity is stored as negative
      subroutine inactivate(p)
        class(particle), intent(inout) :: p
        if (p%is_inactive()) return
        p%rad_ = -p%rad_
      end subroutine

      !> Get status of particle
      logical pure function is_inactive(p)
        class(particle), intent(in) :: p
          is_inactive = .not. p%is_active()
      end function

      !> Get status of particle
      logical pure function is_active(p)
        class(particle), intent(in) :: p
        is_active = p%rad() > 0
      end function

      !> Ensure there is a sensible lower limit
      !> for the activity (machine precision)
      subroutine flush_away_denormal(p)
        class(particle), intent(inout) :: p

        if (p%is_inactive()) return

        if (p%rad() < numeric_limit_rad) p%rad_ = 0.0
      end subroutine

end module particleML
