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
c-----------------------------------------------------------------------
c snaptab.inc
c
c..include file  - fixed tables and constants
c                  (independant of input data)
c
c
c..pmult:  multiply pressure by this value to get index in pitab
c..pitab:  Exner function, pitab(0:130) for p=0,10,20,...1300 hPa
c..g    :  ...
c..r    :  ...
c..cp   :  ...
c
      real    pmult,pitab(0:130),g,r,cp
c
      common/table1/pitab
      parameter (g=9.81, r=287., cp=1004., pmult=0.1)
c
c
c..premult: multiply precipitation intensity (mm/hour) by this value
c	    to get index in pretab
c..pretab:  precipitation parobability table (for wet depositions)
c..nprepro:     no. of steps in input precipitation probability table
c..prepro(1,n): precipitation intensity (mm/hour)
c..prepro(2,n): probability for precipitation (0. - 1.)
c
      real    premult,pretab(0:mpretab)
      integer nprepro
      real    prepro(2,mprepro+1)
c
      common/table2/premult,pretab
     -             ,nprepro,prepro
c
c-----------------------------------------------------------------------
