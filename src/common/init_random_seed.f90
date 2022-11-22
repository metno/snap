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

module init_random_seedML
  implicit none
  private

  integer, public, save :: extra_seed = 0

  public :: init_random_seed

  contains

  SUBROUTINE init_random_seed()
  INTEGER :: i,n
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed

  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))

  seed(:) = 3 + 37 * (/ (i - 1, i = 1, n) /) + extra_seed
  CALL RANDOM_SEED(PUT = seed)

  END SUBROUTINE
end module init_random_seedML
