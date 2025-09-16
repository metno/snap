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

  public :: init_random_seed, generate_normal_randoms

  contains

  SUBROUTINE init_random_seed()
  INTEGER :: i,n
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed

  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))

  seed(:) = 3 + 37 * (/ (i - 1, i = 1, n) /) + extra_seed
  CALL RANDOM_SEED(PUT = seed)

  END SUBROUTINE

  SUBROUTINE generate_normal_randoms(x, n)

  implicit none
  integer, intent(in) :: n
  real, intent(out) :: x(n)
  integer :: i
  real :: u1, u2, z
  real, parameter :: pi = 3.141592653589793

  call random_seed()  ! initialize RNG from system clock

  i = 1
  do while (i <= n)
      call random_number(u1)
      call random_number(u2)

      ! Box–Muller transform for standard normal
      z = sqrt(-2.0*log(u1)) * cos(2.0*pi*u2)

      if (abs(z) <= 3.0) then
        x(i) = z
        i = i + 1
      end if
  end do

  END SUBROUTINE

end module init_random_seedML
