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

!> fixed tables, constants, and functions
!> which are independent of input data
module snaptabML
    implicit none
    private

!> max. no. of steps in precipitation probability table
    integer, parameter :: mpretab = 500

!> multiply pressure by this value to get index in pitab
    real, parameter, public :: pmult = 0.1
!>  Exner function, pitab(0:130) for p=0,10,20,...1300 hPa
    real, save, public :: pitab(0:130)

    real, parameter, public :: g=9.81, r=287.0, cp=1004.0
    real, parameter, public :: rcp = r/cp
!> Units of hPa
    real, parameter, public :: standard_atmosphere = 1013.25

!> Height of surface layer
!>
!> Used for dry deposition
    real, parameter, public :: surface_height_sigma = 0.996

!> create precomputed table for pressures between 0.1 and 1500hPa
    real, save :: t2thetafac_table(15000)

    public :: t2thetafac
    public :: tabcon
    public :: hypsometric_eq, hypsometric_eq_inv

    contains


!> Purpose:  Define fixed tables and constants
!>           that is independent of input data
      subroutine tabcon
        implicit none

        integer :: i
        real :: p

!..Exner function, pitab(0:130) for p=0,10,20,...1300 hPa
        do i=lbound(pitab,1),ubound(pitab,1)
          p = i/pmult
          pitab(i) = 1004.*(p/1000.)**rcp
        end do

        do i=1,size(t2thetafac_table)
          t2thetafac_table(i) = 1.0/((real(i)/10.0*0.001)**rcp)
        enddo
      end subroutine tabcon

      !> t2thetafac 50% faster, and less than 0.5% difference in theta
      pure elemental real function t2thetafac(p)
        real, intent(in) :: p
        t2thetafac = t2thetafac_table(nint(p*10.0 + 0.5))
      end function

  pure elemental real function hypsometric_eq(p1, p2, T) result(h)
    !> Pressure
    real, intent(in) :: p1
    !> Pressure, same unit as p1
    real, intent(in) :: p2
    !> Mean virtual temperatur [K]
    real, intent(in) :: T
    h = R * T / g * log(p1/p2)
  end function

  pure elemental real function hypsometric_eq_inv(h, p, T) result(p2)
    !> Height [m]
    real, intent(in) :: h
    !> Pressure (same unit as output)
    real, intent(in) :: p
    !> Mean virtual temperature [K]
    real, intent(in) :: T
    p2 = p*exp(g*h/(R*T))
  end function
end module snaptabML
