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
      subroutine checkDomain(np)
c Purpose:
c    check if particle is inside domain (set active = .false. if outside)
c    move particle to lowest (highest) level if below lowest (highest) level

      use particleML
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
      include 'snapdim.inc'
      include 'snapgrd.inc'
      INTEGER, INTENT(IN) :: np
      REAL vmin, vmax

#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('CHECKDOMAIN',0,ZHOOK_HANDLE)
#endif

      if (pdata(np)%x.lt.1. .or. pdata(np)%y.lt.1. .or.
     +    pdata(np)%x.gt.nx .or. pdata(np)%y.gt.ny) then
         pdata(np)%active = .false.
      else
        vmin=vlevel(nk)
        vmax=vlevel( 1)
        if (pdata(np)%z .gt. vmax) then
          pdata(np)%z = vmax
        else if (pdata(np)%z .lt. vmin) then
          pdata(np)%z = vmin
        end if
      end if
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('CHECKDOMAIN',1,ZHOOK_HANDLE)
#endif
      return

      end subroutine checkDomain
