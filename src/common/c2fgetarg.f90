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
      subroutine c2fgetarg(iarg,carg)
c
c	SET arguments from C/C++ (main) program (iarg=-1,-2,-3,...)
c	GET arguments from Fortran subroutines  (iarg=+1,+2,+3,...)
c
c
      implicit none
c
      integer iarg
      character*(*) carg
c
      integer maxargs
      parameter (maxargs=40)
c
      integer nargs,lallargs,iallargs(2,maxargs)
c
      character*512 allargs
c
      integer lenstr
      integer la,lc,k1,k2
c
      save iallargs,allargs
c
      data nargs,lallargs/0,0/
c
      if(iarg.lt.0) then
c
c..set (from C/C++)
c
       la=len(allargs)
       lc=lenstr(carg,0)
       if(lallargs+lc.le.la .and. nargs.lt.maxargs) then
         nargs=nargs+1
         k1=lallargs+1
         k2=lallargs+lc
         iallargs(1,nargs)=k1
         iallargs(2,nargs)=k2
         allargs(k1:k2)=carg(1:lc)
         lallargs=k2
       end if
c
      elseif(iarg.eq.0) then
c
c..only called from iargc below !!!!!
c..return no. of arguments set
c
       iarg=nargs
c
      elseif(iarg.gt.0) then
c
c..get (from fortran subroutine)
c
       if(iarg.le.nargs) then
         k1=iallargs(1,iarg)
         k2=iallargs(2,iarg)
         lc=len(carg)
         lc=min(k2-k1+1,lc)
         carg=' '
         carg(1:lc)=allargs(k1:k1+lc-1)
       else
         carg=' '
       end if
c
      end if
c
      return
      end
c
c***********************************************************************
c
      integer function c2fiargc()
c
c..only called from fortran subroutine
c
      implicit none
c
      integer nargs
      character*2 cdummy
c
      nargs=0
      call c2fgetarg(nargs,cdummy)
c
      c2fiargc=nargs
c
      return
      end
