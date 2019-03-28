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
module particleML
    USE iso_fortran_env, only: real64, int16
    implicit none
    private
    ! a simple particle to be stored
    TYPE, PUBLIC :: particle
! old definition
!..pdata:   pdata(1,n) - x position in grid
!           pdata(2,n) - y position in grid
!           pdata(3,n) - sigma/eta position (vertical)
!           pdata(4,n) - sigma/eta at top of boundary layer
!           pdata(5,n) - height of boundary layer
!           pdata(6,n) - map ratio in x direction
!           pdata(7,n) - map ratio in y direction
!           pdata(8,n) - precipitation intensity (mm/hour)
!           pdata(9,n) - radioactive content (Bq)
!           pdata(10,n) - gravity in m/s (fixed or computed)
       SEQUENCE
       REAL(real64)   :: x     !- x position in grid
       REAL(real64)   :: y     !- y position in grid
       REAL(real64)   :: z     !- sigma/eta position (vertical)
       REAL           :: tbl   !- sigma/eta at top of boundary layer
       REAL           :: rad   !- radioactive content (Bq)
       REAL           :: hbl
       REAL           :: grv = 0. ! gravity in m/s (fixed or computed)
       INTEGER(int16) :: ageInSteps = 0 ! age of particle since construction
       LOGICAL        :: active = .false. ! inside/outside domain
    END TYPE particle

! storage for extra particle data
    TYPE, PUBLIC :: extraParticle
       SEQUENCE
       REAL         :: u ! u-speed
       REAL         :: v ! v-speed
       REAL(real64) :: rmx ! map ratio in x direction
       REAL(real64) :: rmy ! map ration in y direction
       REAL         :: prc ! precipition intensity (mm/hour)
    END TYPE extraParticle

! the actual particle storage, will be allocated in allocateFields.F
    TYPE(particle), DIMENSION(:), POINTER, PUBLIC :: pdata

end module particleML
