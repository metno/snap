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

subroutine decay(n)
  USE snapparML
!  Purpose:  Decrease radioactive contents due to decay
!    WARNING:   make sure decayDeps is run once before running decay

#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  use particleML
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

        
  integer, INTENT(IN) :: n
  integer :: m



#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('DECAY',0,ZHOOK_HANDLE)
#endif


!      do n=1,npart loop outside this function
  m= icomp(n)
  if(kdecay(m) == 1) then
    pdata(n)%rad= pdata(n)%rad * decayrate(m)
  end if
!      end do


#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('DECAY',1,ZHOOK_HANDLE)
#endif
  return
end subroutine decay
