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
module snapfldML
    implicit none
    public
!
!..include file  -  common for fields
!
!
!..1:      input time step 1
!..2:      input time step 2
!..u,v:    horizontal wind components
!..w:      sigma_dot
!..t:      potential temperature (level 1, surface: abs. temp. 2m)
!..ps:     surface pressure
!..bl:     boudary layer top in sigma coordinate
!..hbl:    boudary layer depth in meters
!..xm:     map ratio in x direction
!..ym:     map ratio in y direction
!..garea:  grid square area (m**2)
!..field*: work arrays
!..pmsl:   mean sea level pressure (mslp, not used in computations)
!	   for graphics and/or output (possible if nxad=nx and nyad=ny)
!..precip: hourly precipitation intensity (mm/hour)
!..nprecip: no. of steps stored
!..iprecip: the current precipitation field
!..enspos: the ensemble-member to read met-data from
!
      REAL(kind=4), DIMENSION(:,:,:), POINTER :: &
        u1, v1, w1, t1, hlevel1, hlayer1, &
        u2, v2, w2, t2, hlevel2, hlayer2, &
        precip

      REAL(kind=4), DIMENSION(:,:), POINTER :: &
        ps1, bl1, hbl1, &
        ps2, bl2, hbl2, &
        xm, ym, garea, field1, field2, field3, &
        field4, pmsl1, pmsl2

      integer nprecip,iprecip,enspos
!
      common/fieldi/u1,v1,w1,t1,ps1,bl1,hbl1,hlevel1,hlayer1 &
                  ,u2,v2,w2,t2,ps2,bl2,hbl2,hlevel2,hlayer2 &
                  ,xm,ym,garea &
                  ,field1,field2,field3,field4 &
                  ,pmsl1,pmsl2 &
                  ,precip,nprecip,iprecip,enspos
!
!
!..dgarea:  grid square area (m**2) ... double precision
!..depdry:  dry deposition
!..depwet:  wet deposition
!..accdry:  accumulated dry deposition
!..accwet:  accumulated wet deposition
!..concacc: accumulated/integrated concentration
!..avghbl:  average height of boundary layer (accumulation)
!..avgprec: precipitation  (accumulation)
!..accprec: accumulation of precipitation from start of run
!..avgbq1:  average Bq (per square area) in boundary layer (accum.)
!..avgbq2:  average Bq (per square area) above boundary layer (accum.)
!..avgbq:   average Bq (per square area) in each layer (accum.)
!..	    only used if (nxmc=nx, nymc=ny and imodlevel=1)
!
      REAL(kind=8), DIMENSION(:,:), POINTER :: &
        dgarea, avghbl, avgprec, accprec
      REAL(kind=8), DIMENSION(:,:,:), POINTER :: &
        depdry, depwet, accdry, accwet, &
        concen, concacc, avgbq1, avgbq2
      REAL(kind=8), DIMENSION(:,:,:,:), POINTER :: &
        avgbq
!
      common/fieldo/dgarea &
              ,depdry,depwet,accdry,accwet &
              ,concen,concacc &
              ,avghbl,avgprec,accprec &
              ,avgbq1,avgbq2 &
              ,avgbq
end module snapfldML
