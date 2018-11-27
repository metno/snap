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
  public

!-----------------------------------------------------------------------
!
!..include file  - all parameter statements (model dimensions etc.)
!
!
!..nx,ny:  horizontal field dimensions (for computations)
!..nxpre,nypre: nx and ny if not in input-file
!..nk:     number of levels
!
      integer nx,ny,nk, maxsiz,ldata
      integer nxpre, nypre, nkpre
!cc   parameter (nx=121,ny=97,nk=14)
!cc   parameter (nx=127,ny=103,nk=19)
!cc   parameter (nx=188,ny=152,nk=32)
!cc      parameter (nx=468,ny=378,nk=41)
!cc h12ml in archive (which is 24km)
!cc   parameter (nx=432,ny=349,nk=61)
!cc h12sf and h12snap (12km)
!c default parameters, if input.gridsize and input.levels not in snap.input
      parameter (nxpre=864,nypre=698,nkpre=61)
      data nx/nxpre/
      data ny/nypre/


!cc emep latlon 1x1,
!c      parameter (nxpre=360,nypre=180)

!
!..mprecip: max no. og hourly precipitation fields
!           (and then the maximum hours between field input...)
!
      integer mprecip
      parameter (mprecip=12)
!
!
!..maxsiz: max. input field size (possibly larger than nx*ny)
!..ldata:  length of buffer for field input/output
!
!      integer maxsiz,ldata
!      parameter (maxsiz=nx*ny)
!      parameter (ldata=20+maxsiz+50)
!
!
!..ENSEMBLE PROJECT.....................................
      integer nxep,nyep
!..disable ENSEMBLE PROJECT computation and output
!cc   parameter (nxep=1,nyep=1)
!..enable ENSEMBLE PROJECT computation and output
      parameter (nxep=151,nyep=91)
!.......................................................
!
!
!..SSV ARGOS............................................
      integer mxyargos
!..disable SSV ARGOS output
      parameter (mxyargos=1)
!..enable SSV ARGOS output, set maximum grid sizes
!cc   parameter (mxyargos=nx*ny)
!.......................................................
!
!
!..mplumepre:  max. no. of plume releases, preset, can be configured in snap.input
!..mpartpre:   max. no. of particles, total in all plumes, preset, can be configured in snap.input
!
      integer mplumepre,mpartpre
! mplume should be timesteps (model) * release-heights
      parameter (mplumepre=50000)
      parameter (mpartpre =10000000)
!cc   parameter (mpartpre =300000)
!
!..mdefcomp: max. no. of components defined in input file (aerosol,gas,....)
!..mcomp:    max no. of components used in one run
!            (keep as small as "possible", it dimensions 2d/3d output fields)
!
      integer mcomp
      integer, parameter :: mdefcomp=51
      parameter (mcomp=51)
!
!
!..mfilef: max. no. of input FELT files
!..mavail: max. no. of available timesteps with data
!
      integer mfilef,mavail
      parameter (mfilef=1024)
      parameter (mavail=8192)
!
      integer nxmc,nymc
!..use 1 or 2 below:
!..1: keep large arrays for concentration of each component in each layer
!c    parameter (nxmc=nx,nymc=ny)
!..2: remove large arrays for concentration
!      parameter (nxmc=1,nymc=1)
!
!      integer nxad,nyad
!..use 1 or 2 below:
!..1: keep additional fields (for output/graphics, not used in comp.)
!      parameter (nxad=nx,nyad=ny)
!..2: remove additional fields
!c    parameter (nxad=1,nyad=1)
!
!
!..mbuffr:  max. length of buffer (for misc. reading from 'snap.input')
!..mrelpos: max. no. of release positions available (in list)
!..mtprof:  max. no. of timesteps in release profiles
!..mrelheight: max. no. of height classes for releases
!..mprepro: max. no. of steps in input precipitation probability table
!..mpretab: max. no. of steps in precipitation probability table
!
      integer mbuffr
      integer mrelpos
      integer mtprof, mrelheight
      integer mprepro,mpretab
      parameter (mbuffr=60)
      parameter (mrelpos=30)
      parameter (mtprof=500)
      parameter (mrelheight=20)
      parameter (mprepro=40,mpretab=500)
!
!
!..for gravity tables
!
      integer numtempvg,numpresvg
      parameter (numtempvg=  41)
      parameter (numpresvg=  25)
!
!
!..for graphics:
!..mlandfile: max. no. of landcontour (etc.) files
!
      integer mlandfile
      parameter (mlandfile=6)
!
!-----------------------------------------------------------------------
      common /gridsize/nx,ny,nk, nxmc, nymc, maxsiz,ldata
end module snapdimML
