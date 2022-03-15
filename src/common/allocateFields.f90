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

module allocateFieldsML
  USE particleML, only: pdata
  USE snapparML, only: ncomp, iparnum
  USE snapfldML, only: u1, u2, v1, v2, w1, w2, bl1, bl2, t1, t2, &
      ps1, ps2, hbl1, hbl2, hlevel1, hlevel2, hlayer1, hlayer2, &
      concacc, avgbq1, avgbq2, avgbq, accwet, accdry, concen, &
      depdry, depwet, accprec, avgprec, avghbl, precip, &
      pmsl1, pmsl2, field1, field2, field3, field4, xm, ym, &
      garea, dgarea, &
      max_column_scratch, max_column_concentration, &
      max_aircraft_doserate, max_aircraft_doserate_scratch, t1_abs, t2_abs
  USE snapfilML, only: idata, fdata
  USE snapgrdML, only: ahalf, bhalf, vhalf, alevel, blevel, vlevel, imodlevel, &
      compute_column_max_conc, compute_max_aircraft_doserate
  USE releaseML, only: mplume, iplume, mpart
  implicit none
  private

  public allocateFields, deAllocateFields

  contains

subroutine allocateFields
  USE particleML, only: pdata
  USE snapdimML, only: nx, ny, nk, ldata, maxsiz
  USE snapparML, only: ncomp, iparnum
  USE releaseML, only: mplume, iplume, plume_release, mpart

  logical, save :: FirstCall = .TRUE.
  integer :: AllocateStatus
  character(len=*), parameter :: errmsg = "*** Not enough memory ***"

  if ( .NOT. FirstCall) return
  FirstCall = .FALSE.

  ALLOCATE ( alevel(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( blevel(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( vlevel(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( ahalf(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( bhalf(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( vhalf(nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg


  ALLOCATE ( u1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( v1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( w1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( t1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( ps1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( bl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hbl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hlevel1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hlayer1(nx,ny,nk), STAT = AllocateStatus)

  ALLOCATE ( u2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( v2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( w2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( t2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( ps2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( bl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hbl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hlevel2(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hlayer2(nx,ny,nk), STAT = AllocateStatus)

  ALLOCATE ( idata(ldata), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( fdata(maxsiz), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  ALLOCATE ( xm(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( ym(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( garea(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field3(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field4(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  ALLOCATE ( pmsl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( pmsl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  ALLOCATE ( precip(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

! the calculation-fields
  ALLOCATE ( dgarea(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( avghbl(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( avgprec(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( accprec(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  accprec = 0.0

  ALLOCATE ( depdry(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  depdry = 0.0
  ALLOCATE ( depwet(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  depwet = 0.0
  ALLOCATE ( accdry(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  accdry = 0.0
  ALLOCATE ( accwet(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  accwet = 0.0
  ALLOCATE ( concen(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  concen = 0.0
  ALLOCATE ( concacc(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  concacc = 0.0
  ALLOCATE ( avgbq1(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( avgbq2(nx,ny,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  if (imodlevel) then
    ALLOCATE ( avgbq(nx,ny,nk-1,ncomp), STAT = AllocateStatus)
    IF (AllocateStatus /= 0) ERROR STOP errmsg
  endif

! the part particles fields
  ALLOCATE ( pdata(mpart), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( iparnum(mpart), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

! the plumes
  ALLOCATE ( iplume(mplume), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( plume_release(mplume, ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  if (compute_column_max_conc) then
    allocate(max_column_scratch(nx,ny,nk), &
      max_column_concentration(nx,ny), &
      STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
  endif

  if (compute_max_aircraft_doserate) then
    allocate(max_aircraft_doserate_scratch(nx,ny,nk,ncomp+1), &
      max_aircraft_doserate(nx,ny), &
      t1_abs(nx,ny,nk), t2_abs(nx,ny,nk), &
      STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
  endif

end subroutine allocateFields


subroutine deAllocateFields
  USE releaseML, only: iplume, plume_release

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

  if (allocated(avgbq)) then
    deallocate(avgbq)
  endif

  if (allocated(max_column_concentration)) then
    deallocate(max_column_scratch)
    deallocate(max_column_concentration)
  endif

  if (allocated(max_aircraft_doserate)) then
    deallocate(max_aircraft_doserate, max_aircraft_doserate_scratch)
  endif

  if (allocated(t1_abs)) then
    deallocate(t1_abs, t2_abs)
  endif

  DEALLOCATE ( pdata )
  DEALLOCATE ( iparnum )

  DEALLOCATE ( iplume )
  DEALLOCATE ( plume_release )

end subroutine deAllocateFields
end module allocateFieldsML
