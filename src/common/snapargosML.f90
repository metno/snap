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
module snapargosML
    implicit none
    private
!
!..include file  -  common for SSV ARGOS OUTPUT
!
    integer, parameter, public :: margos=500
!
    integer, save, public :: iargos,nargos
    integer, save, public :: argoshourstep,argoshoursrelease,argoshoursrun
    integer, save, public :: argostime(5,margos)
    character(len=100), save, public :: argosdepofile,argosconcfile,argosdosefile
!
end module snapargosML
