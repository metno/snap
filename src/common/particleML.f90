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

!> a simple particle to be stored
    type, public :: particle
        sequence
        !> x position in grid
        real(real64)   :: x
        !> y position in grid
        real(real64)   :: y
        !> sigma/eta position (vertical)
        real(real64)   :: z
        !> sigma/eta at top of boundary layer
        real           :: tbl
        !> radioactive content (Bq)
        real           :: rad
        !> height of boundary layer
        real           :: hbl
        !> gravity in m/s (fixed or computed)
        real           :: grv = 0.
        !> inside/outside domain
        logical        :: active = .false.
        !> index to the defined component
        integer(int16) :: icomp
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

end module particleML
