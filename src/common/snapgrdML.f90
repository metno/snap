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
module snapgrdML
    implicit none
    private
!
!..include file  -  common for grid specifications etc.
!
!
!..iprod:         input producer no.
!..igrid:         input grid no.
!..klevel(nk):    model level no.
!                 sequence: bottom to top (kk,kk-1,....1)
!                 level no.  1 should be 0    (surface fields here)
!                 level no.  2 should be 'kk' (no. of levels)
!                 level no. nk should be 1    (current SNAP version)
!..iprodr:        output producer no. (usually as input)
!..igridr:        output grid no. (may be different from input)
!..ixbase,iybase: lower left corner in input fields
!                 (size is set by nx,ny at compilation time)
!..ixystp:        step in x and y direction
!                 (ixystp>1 means lower resolution fields than input,
!                  to decrease memory size)
!..inprecip:      0=not read precipitation  1=read precipitation
!..		  -1=read precipitation (but there is no wet deposition)
!..imslp:         0=not read mslp  -1=read mslp  1=read and write mslp
!		  (mslp not used in computations)
!..imodlevel:     0=not output of concentrations in model levels
!		  1=output (possible if nxmc=nx and nymc=ny)
!..modleveldump: remove particles from model after at least that many steps
!..itotcomp:      0=not output of total of all components
!		  (e.g. when each component is released with a mass unit)
!                 1=output of total of all components
!
    integer, save, public :: iprod,igrid
! klevel needs to get allocated in main program while reading parameters
    INTEGER, DIMENSION(:), allocatable, save, public ::  klevel
    integer, save, public :: iprodr,igridr,ixbase,iybase,ixystp
    integer, save, public :: inprecip,imslp,imodlevel,itotcomp
    real, save, public :: modleveldump
!
!
!
!..gparam:  grid parameters (depending on the grid type)
!..  polarstereographic (igtype=1,4):
!..    gparam(1): xp     - x position of north pole
!..    gparam(2): yp     - y position of north pole
!..    gparam(3): an     - no. grid units between the pole and equator
!..    gparam(4): fi     - grid rotation angle
!                          (y axis compared to longitude 0, +/- = e/w)
!..    gparam(5): 60.    - projection latitude (usually 60., i.e. 60N)
!..    gparam(6): 0.     - (not used)
!..    gparam(7): dxgrid - grid resolution in meters, x direction
!..    gparam(8): dygrid - grid resolution in meters, y direction
!..  geographic and spherical rotated (igtype=2,3):
!..    gparam(1): blon   - west  longitude (x)
!..    gparam(2): blat   - south latitude  (y)
!..    gparam(3): dlon   - longitude resolution
!..    gparam(4): dlat   - latitude  resolution
!..    gparam(5): clon   - centre longitude (0 if not rotated)
!..    gparam(6): clat   - centre latitude  (0 if not rotated)
!..    gparam(7): dxgrid - grid resolution in meters in x direction
!                          (longitude)
!..    gparam(8): dygrid - grid resolution in meters in y direction
!                          (latitude)
!..alevel:    eta a_level (sigma levels: alevel=ptop*(1.-sigma))
!..blevel:    eta b_level (sigma levels: blevel=sigma)
!..vlevel:    vertical level (sigma or eta)
!..igtype:    grid type, 1=polarstereographic
!..                      2=geographic
!..                      3=spherical (rotated)
!..ivcoor:    vertical coordinate,  2=sigma (Norlam)
!..                                10=eta   (Hirlam,...))
!..kadd:      levels added at the top (when missing upper model levels)
!..	      (u,v copied up, w reduced, pot.temp. const.)
!..ivlevel:   tabel of level numbers for interpolation
!..	      (key is vlevel*10000)
!..ivlayer:   tabel of layer numbers for concentration in model levels
!..	      (key is vlevel*10000)
!
    real, save, public :: gparam(8)
! nk dependent arrays
    REAL, DIMENSION(:), allocatable, save, public :: alevel,blevel,vlevel
    REAL, DIMENSION(:), allocatable, save, public :: ahalf,bhalf,vhalf
    integer, save, public :: igtype,ivcoor,kadd
    integer, save, public :: ivlevel(0:10000),ivlayer(0:10000)

end module snapgrdML
