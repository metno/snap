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
module snapdimML
  implicit none
  private

!-----------------------------------------------------------------------
!
!..include file  - all parameter statements (model dimensions etc.)
!
!
!..nx,ny:  horizontal field dimensions (for computations)
!..nxpre,nypre: nx and ny if not in input-file
!..nk:     number of levels
!
      integer, parameter :: nxpre=864, nypre=698, nkpre=61
      integer, save, public :: nx = nxpre,ny=nypre,nk=nkpre
      integer, save, public :: maxsiz,ldata
!cc   parameter (nx=121,ny=97,nk=14)
!cc   parameter (nx=127,ny=103,nk=19)
!cc   parameter (nx=188,ny=152,nk=32)
!cc      parameter (nx=468,ny=378,nk=41)
!cc h12ml in archive (which is 24km)
!cc   parameter (nx=432,ny=349,nk=61)
!cc h12sf and h12snap (12km)
!c default parameters, if input.gridsize and input.levels not in snap.input


!
!..mprecip: max no. og hourly precipitation fields
!           (and then the maximum hours between field input...)
!
      integer, parameter, public :: mprecip=12
!
!..maxsiz: max. input field size (possibly larger than nx*ny)
!..ldata:  length of buffer for field input/output
!
!      integer maxsiz,ldata
!      parameter (maxsiz=nx*ny)
!      parameter (ldata=20+maxsiz+50)
!
!
!
!
!..SSV ARGOS............................................
      integer, parameter, public :: mxyargos=1
!..disable SSV ARGOS output
!..enable SSV ARGOS output, set maximum grid sizes
!cc   parameter (mxyargos=nx*ny)
!.......................................................


!..mdefcomp: max. no. of components defined in input file (aerosol,gas,....)
!..mcomp:    max no. of components used in one run
!            (keep as small as "possible", it dimensions 2d/3d output fields)
!
      integer, parameter, public :: mcomp=51
      integer, parameter, public :: mdefcomp=51
!
!
!..mfilef: max. no. of input FELT files
!..mavail: max. no. of available timesteps with data
!
      integer, parameter, public :: mfilef=1024,mavail=8192
!
      integer, save, public :: nxmc,nymc
!..use 1 or 2 below:
!..1: keep large arrays for concentration of each component in each layer
!c    parameter (nxmc=nx,nymc=ny)
!..2: remove large arrays for concentration
!      parameter (nxmc=1,nymc=1)
!
!
!..mbuffr:  max. length of buffer (for misc. reading from 'snap.input')
!..mrelpos: max. no. of release positions available (in list)
!..mprepro: max. no. of steps in input precipitation probability table
!..mpretab: max. no. of steps in precipitation probability table
!
      integer, parameter, public :: mbuffr=60
      integer, parameter, public :: mrelpos=30
      integer, parameter, public :: mprepro=40,mpretab=500
!
!-----------------------------------------------------------------------
end module snapdimML
