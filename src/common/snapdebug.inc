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
c snapdebug.inc
c
c..include file  -  common for debugging.....
c
c..idebug - 0 = debug off
c	    1 = debug on, note that mrfturbo/mrfelt only
c		          prints to standard out (not the log file)
c
      integer idebug
c
      common/debugcom/idebug
c
c-----------------------------------------------------------------------
