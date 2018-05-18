! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

subroutine c2fgetarg(iarg,carg)

!	SET arguments from C/C++ (main) program (iarg=-1,-2,-3,...)
!	GET arguments from Fortran subroutines  (iarg=+1,+2,+3,...)


  implicit none

  integer :: iarg
  character*(*) carg

  integer :: maxargs
  parameter (maxargs=40)

  integer :: nargs,lallargs,iallargs(2,maxargs)

  character(512) :: allargs

  integer :: lenstr
  integer :: la,lc,k1,k2

  save iallargs,allargs

  data nargs,lallargs/0,0/

  if(iarg < 0) then
  
  !..set (from C/C++)
  
    la=len(allargs)
    lc=lenstr(carg,0)
    if(lallargs+lc <= la .AND. nargs < maxargs) then
      nargs=nargs+1
      k1=lallargs+1
      k2=lallargs+lc
      iallargs(1,nargs)=k1
      iallargs(2,nargs)=k2
      allargs(k1:k2)=carg(1:lc)
      lallargs=k2
    end if
  
  elseif(iarg == 0) then
  
  !..only called from iargc below !!!!!
  !..return no. of arguments set
  
    iarg=nargs
  
  elseif(iarg > 0) then
  
  !..get (from fortran subroutine)
  
    if(iarg <= nargs) then
      k1=iallargs(1,iarg)
      k2=iallargs(2,iarg)
      lc=len(carg)
      lc=min(k2-k1+1,lc)
      carg=' '
      carg(1:lc)=allargs(k1:k1+lc-1)
    else
      carg=' '
    end if
  
  end if

  return
end subroutine c2fgetarg

!***********************************************************************

  integer function c2fiargc()

!..only called from fortran subroutine

  implicit none

  integer :: nargs
  character(2) :: cdummy

  nargs=0
  call c2fgetarg(nargs,cdummy)

  c2fiargc=nargs

  return
  end function c2fiargc
