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
c snappos.inc
c
c..include file  -  common for release positions and profiles
c
c
c..irelpos:     the chosen release position
c..nrelpos:     no. of release positions (in list)
c..relpos(1,n): geographic latitude  for release positions
c..relpos(2,n): geographic longitude for release positions
c..relpos(3,n): grid x coordinat for release positions
c..relpos(4,n): grid y coordinat for release positions
c..relnam(n)  : name of release positions
c..srelnam    : name of selected release position
c
      integer      irelpos,nrelpos
      real         relpos(4,mrelpos)
      character*40 relnam(mrelpos),srelnam
c
      common/poscom/irelpos,nrelpos
     -             ,relpos
     -             ,relnam,srelnam
c
c-----------------------------------------------------------------------
