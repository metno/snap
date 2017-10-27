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
c snapeps.inc
c
c..include file  -  common for ENSEMBLE PROJECT OUTPUT
c
c
c..ensemblefile
c..ensembleStepHours
c..ensembleparticipant
c..ensembleRandomKey
c
      integer iensemble
      integer ensembleStepHours
      integer ensembleparticipant
      character*128 ensemblefile
      character*7   ensembleRandomKey
c
      common/ensemb/iensemble,
     +              ensembleStepHours,
     +              ensembleparticipant,
     +              ensemblefile,ensembleRandomKey
c
c-----------------------------------------------------------------------
