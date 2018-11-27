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

subroutine allocateFields
  USE particleML
  USE fileInfoML
  USE snapdimML, only: nx, ny, nk
  USE snapparML
  USE snapfldML
  USE snapfilML
  USE snapgrdML
  implicit none

  TYPE(particle), DIMENSION(:), POINTER :: x
  logical, save :: FirstCall = .TRUE. 
  integer :: AllocateStatus
        
  if ( .NOT. FirstCall) return
  FirstCall = .FALSE. 

  ALLOCATE ( alevel(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( blevel(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( vlevel(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( ahalf(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( bhalf(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( vhalf(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"


  ALLOCATE ( u1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( v1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( w1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( t1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( ps1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( bl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( hbl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( hlevel1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( hlayer1(nx,ny,nk), STAT = AllocateStatus)

  ALLOCATE ( u2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( v2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( w2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( t2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( ps2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( bl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( hbl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( hlevel2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( hlayer2(nx,ny,nk), STAT = AllocateStatus)

  ALLOCATE ( idata(ldata), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( fdata(maxsiz), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

  ALLOCATE ( xm(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( ym(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( garea(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( field1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( field2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( field3(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( field4(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

  ALLOCATE ( pmsl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( pmsl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

  ALLOCATE ( precip(nx,ny,mprecip), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

! the calculation-fields
  ALLOCATE ( dgarea(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( avghbl(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( avgprec(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( accprec(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

  ALLOCATE ( depdry(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( depwet(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( accdry(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( accwet(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( concen(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( concacc(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( avgbq1(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( avgbq2(nx,ny,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

  ALLOCATE ( avgbq(nxmc,nymc,nk-1,mcomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

! the part particles fields
  ALLOCATE ( pdata(mpart), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( icomp(mpart), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"
  ALLOCATE ( iparnum(mpart), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

! the plumes
  ALLOCATE ( iplume(2,mplume), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) STOP "*** Not enough memory ***"

end subroutine allocateFields


subroutine deAllocateFields
  USE particleML
  USE fileInfoML
  USE snapparML
  USE snapdimML, only: nx, ny, nk
  USE snapfldML
  USE snapfilML
  USE snapgrdML
  use snapdimML
  implicit none
        
  DEALLOCATE ( alevel )
  DEALLOCATE ( blevel )
  DEALLOCATE ( vlevel )
  DEALLOCATE ( ahalf )
  DEALLOCATE ( bhalf )
  DEALLOCATE ( vhalf )

  DEALLOCATE ( u1)
  DEALLOCATE ( v1)
  DEALLOCATE ( w1)
  DEALLOCATE ( t1)
  DEALLOCATE ( ps1)
  DEALLOCATE ( bl1)
  DEALLOCATE ( hbl1)
  DEALLOCATE ( hlevel1)
  DEALLOCATE ( hlayer1)

  DEALLOCATE ( u2)
  DEALLOCATE ( v2)
  DEALLOCATE ( w2)
  DEALLOCATE ( t2)
  DEALLOCATE ( ps2)
  DEALLOCATE ( bl2)
  DEALLOCATE ( hbl2)
  DEALLOCATE ( hlevel2)
  DEALLOCATE ( hlayer2)

  DEALLOCATE ( idata )
  DEALLOCATE ( fdata )

  DEALLOCATE ( xm)
  DEALLOCATE ( ym)
  DEALLOCATE ( garea)
  DEALLOCATE ( field1)
  DEALLOCATE ( field2)
  DEALLOCATE ( field3)
  DEALLOCATE ( field4)

  DEALLOCATE ( pmsl1)
  DEALLOCATE ( pmsl2)

  DEALLOCATE ( precip)

  DEALLOCATE ( dgarea )
  DEALLOCATE ( avghbl )
  DEALLOCATE ( avgprec )
  DEALLOCATE ( accprec )

  DEALLOCATE ( depdry )
  DEALLOCATE ( depwet )
  DEALLOCATE ( accdry )
  DEALLOCATE ( accwet )
  DEALLOCATE ( concen )
  DEALLOCATE ( concacc )
  DEALLOCATE ( avgbq1 )
  DEALLOCATE ( avgbq2 )

  DEALLOCATE ( avgbq )

  DEALLOCATE ( pdata )
  DEALLOCATE ( icomp )
  DEALLOCATE ( iparnum )

  DEALLOCATE ( iplume )

end subroutine deAllocateFields
