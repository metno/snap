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

!> common for grid specifications etc.
module snapgrdML
    implicit none
    private

!> mapping from internal vertical coordinate to MET vertical coord
!>
!> sequence: bottom to top (kk,kk-1,....1)
!>
!> level no.  1 should be 0    (surface fields here)
!>
!> level no.  2 should be 'kk' (no. of levels)
!>
!> level no. nk should be 1    (current SNAP version)
!>
!> klevel needs to get allocated in main program while reading parameters
    integer, dimension(:), allocatable, save, public ::  klevel

    logical, save, public :: precipitation_in_output = .true.
!> * 0=not read mslp
!> * -1=read mslp
!> * 1=read and write mslp (mslp not used in computations)
    integer, save, public :: imslp = 0
!> * output of concentrations in model levels
    logical, save, public :: imodlevel = .false.
!> remove particles from model after at least that many steps
    real, save, public :: modleveldump = 0.0
!> * 0=not output of total of all components (e.g. when each component is released with a mass unit)
!> 1=output of total of all components
    integer, save, public :: itotcomp = 0

!> Compute max concentration in a column (accumulated per timestep)
    logical, save, public :: compute_column_max_conc = .false.

!> Compute max doserate in an aircraft equivalent
    logical, save, public :: compute_aircraft_doserate = .false.
!> Threshold limit in [Sv/h] used for determining minimum flight altitude
    real, save, public :: aircraft_doserate_threshold = -1.0

!> grid parameters (depending on the grid type)
!>
!> polarstereographic (igtype=1,4):
!> * gparam(1): xp     - x position of north pole
!> * gparam(2): yp     - y position of north pole
!> * gparam(3): an     - no. grid units between the pole and equator
!> * gparam(4): fi     - grid rotation angle
!> (y axis compared to longitude 0, +/- = e/w)
!> * gparam(5): 60.    - projection latitude (usually 60., i.e. 60N)
!> * gparam(6): 0.     - (not used)
!> * gparam(7): dxgrid - grid resolution in meters, x direction
!> * gparam(8): dygrid - grid resolution in meters, y direction
!>
!> geographic and spherical rotated (igtype=2,3):
!> * gparam(1): blon   - west  longitude (x)
!> * gparam(2): blat   - south latitude  (y)
!> * gparam(3): dlon   - longitude resolution
!> * gparam(4): dlat   - latitude  resolution
!> * gparam(5): clon   - centre longitude (0 if not rotated)
!> * gparam(6): clat   - centre latitude  (0 if not rotated)
!> * gparam(7): dxgrid - grid resolution in meters in x direction
!>                         (longitude)
!> * gparam(8): dygrid - grid resolution in meters in y direction
!>                         (latitude)
    real, save, public :: gparam(8)

!> eta a_level (sigma levels: alevel=ptop*(1.-sigma))
!> For hybrid levels p = alevel(k) + blevel(k) * ps
!> alevel(1) is ground level (=0.0)
    real, allocatable, save, public :: alevel(:)
!> Halfway levels
!> ahalf(1) is ground level (=0.0)
!> ahalf(2) is between alevel(2) and alevel(3)
!> ahalf(nk) is top of atmosphere (p=0.0,a=0.0)
    real, allocatable, save, public :: ahalf(:)

!> eta b_level (sigma levels: blevel=sigma)
!> blevel(1) is ground level (=1.0)
    real, allocatable, save, public :: blevel(:)
!> Halfway levels
!> bhalf(1) is ground level (=1.0)
!> bhalf(2) is between blevel(2) and blevel(3)
!> bhalf(nk) is top of atmosphere (p=0.0,b=0.0)
    real, allocatable, save, public :: bhalf(:)

!> vertical level (sigma / eta / hybrid)
!> sigma = p / p_sfc
    real, allocatable, save, public :: vlevel(:)
    real, allocatable, save, public :: vhalf(:)

!> grid type
!> * 1=polarstereographic
!> * 2=geographic
!> * 3=spherical (rotated)
    integer, save, public :: igtype
! vertical coordinate
!> * 2=sigma (Norlam)
!> * 10=eta/hybrid   (Hirlam,...))
    integer, save, public :: ivcoor = 0
!> levels added at the top (when missing upper model levels)
!>
!> (u,v copied up, w reduced, pot.temp. const.)
    integer, save, public :: kadd
!> table of level numbers for interpolation
!>
!> (key is vlevel*10000)
    integer, save, public :: ivlevel(0:10000)
!> table of layer numbers for concentration in model levels
!>
!> (key is vlevel*10000)
    integer, save, public :: ivlayer(0:10000)

!> Output concentration in each column
    logical, save, public :: output_column = .false.

!> Save dry deposition velocity in output file
    logical, save, public :: output_vd = .false.
!> Extra debug flags for dry deposition
    logical, save, public :: output_vd_debug = .false.

end module snapgrdML
