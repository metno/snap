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
      pmsl1, pmsl2, field1, field2, field3, field4, field3d1, xm, ym, &
      garea, field_hr1, field_hr2, field_hr3, hbl_hr, &
      precip3d, cw3d, &
      max_column_scratch, max_column_concentration, &
      aircraft_doserate, aircraft_doserate_scratch, t1_abs, t2_abs, &
      aircraft_doserate_threshold_height, vd_dep, &
      xflux, yflux, hflux, t2m, z0, leaf_area_index, &
      roa, ustar, monin_l, raero, vs, rs, &
      total_activity_released, total_activity_lost_domain, total_activity_lost_other, &
      wscav, cloud_cover
  USE snapfilML, only: idata, fdata
  USE snapgrdML, only: ahalf, bhalf, vhalf, alevel, blevel, vlevel, imodlevel, &
      compute_column_max_conc, compute_aircraft_doserate, aircraft_doserate_threshold
  USE releaseML, only: mplume, iplume, mpart
  implicit none
  private

  public allocateFields, deAllocateFields

  contains

subroutine allocateFields
  USE particleML, only: pdata
  USE snapdimML, only: nx, ny, nk, output_resolution_factor, ldata, maxsiz
  USE snapparML, only: ncomp, iparnum
  USE releaseML, only: mplume, iplume, plume_release, mpart
  USE drydep, only: drydep_scheme, DRYDEP_SCHEME_EMEP, DRYDEP_SCHEME_EMERSON, &
    DRYDEP_SCHEME_ZHANG
  USE snapmetML, only: met_params

  logical, save :: FirstCall = .TRUE.
  integer :: AllocateStatus
  integer :: nxhr, nyhr
  character(len=*), parameter :: errmsg = "*** Not enough memory ***"

  if ( .NOT. FirstCall) return
  FirstCall = .FALSE.

  ! high resolution dimensions/arrays (output)
  nxhr = nx * output_resolution_factor
  nyhr = ny * output_resolution_factor

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
  ! hlayer calculated and only needed for output
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hlayer1(nxhr,nyhr,nk), STAT = AllocateStatus)

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
  ! hlayer calculated and only needed for output
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hlayer2(nxhr,nyhr,nk), STAT = AllocateStatus)

  ALLOCATE ( idata(ldata), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( fdata(maxsiz), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  ALLOCATE ( xm(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( ym(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( garea(nxhr,nyhr), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field_hr1(nxhr,nyhr), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field_hr2(nxhr,nyhr), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field_hr3(nxhr,nyhr), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( hbl_hr(nxhr,nyhr), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field3(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field4(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( field3d1(nx,ny,nk), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg


  ALLOCATE ( pmsl1(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( pmsl2(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  ALLOCATE ( precip(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  if (met_params%use_3d_precip) then
    ALLOCATE(precip3d(nx,ny,nk), cw3d(nx,ny,nk), STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
    ALLOCATE(wscav(nx,ny,nk,ncomp),STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
    wscav(:,:,:,:) = 0.0
    allocate(cloud_cover(nx,ny,nk))
  endif

! the calculation-fields
  ALLOCATE ( avghbl(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( avgprec(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( accprec(nx,ny), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  accprec = 0.0

  ALLOCATE ( depdry(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  depdry = 0.0
  ALLOCATE ( depwet(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  depwet = 0.0
  ALLOCATE ( accdry(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  accdry = 0.0
  ALLOCATE ( accwet(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  accwet = 0.0
  ALLOCATE ( concen(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  concen = 0.0
  ALLOCATE ( concacc(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  concacc = 0.0
  ALLOCATE ( avgbq1(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  ALLOCATE ( avgbq2(nxhr,nyhr,ncomp), STAT = AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg

  if (imodlevel) then
    ALLOCATE ( avgbq(nxhr,nyhr,nk-1,ncomp), STAT = AllocateStatus)
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
    allocate(max_column_scratch(nxhr,nyhr,nk), &
      max_column_concentration(nxhr,nyhr), &
      STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
  endif

  if (compute_aircraft_doserate) then
    allocate(aircraft_doserate_scratch(nxhr,nyhr,nk,ncomp+1), &
      aircraft_doserate(nxhr,nyhr), &
      t1_abs(nxhr,nyhr,nk), t2_abs(nxhr,nyhr,nk), &
      STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
    if (aircraft_doserate_threshold > 0.0) then
      allocate(aircraft_doserate_threshold_height(nxhr,nyhr), STAT=allocatestatus)
      if (AllocateStatus /= 0) ERROR STOP errmsg
    endif
  endif

  allocate(total_activity_released(ncomp), total_activity_lost_domain(ncomp), &
    total_activity_lost_other(ncomp), stat=AllocateStatus)
  IF (AllocateStatus /= 0) ERROR STOP errmsg
  total_activity_released(:) = 0.0
  total_activity_lost_domain(:) = 0.0
  total_activity_lost_other(:) = 0.0
  if (drydep_scheme == DRYDEP_SCHEME_EMEP .or. &
      drydep_scheme == DRYDEP_SCHEME_ZHANG .or. &
      drydep_scheme == DRYDEP_SCHEME_EMERSON) then
    allocate(vd_dep(nx,ny,ncomp), STAT=AllocateStatus)
    if (AllocateStatus /= 0) ERROR STOP errmsg
    allocate(xflux, yflux, hflux, t2m, z0, leaf_area_index, mold=ps2)
    allocate(roa(nx, ny))
    allocate(ustar, monin_l, raero, vs, rs, mold=roa)
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
  DEALLOCATE ( field3d1)
  DEALLOCATE ( field_hr1)
  DEALLOCATE ( field_hr2)
  DEALLOCATE ( field_hr3)
  DEALLOCATE ( hbl_hr)

  DEALLOCATE ( pmsl1)
  DEALLOCATE ( pmsl2)

  DEALLOCATE ( precip)
  if (allocated(precip3d)) deallocate(precip3d)
  if (allocated(cw3d)) deallocate(cw3d)
  if (allocated(wscav)) deallocate(wscav)
  if (allocated(cloud_cover)) deallocate(cloud_cover)

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

  if (allocated(aircraft_doserate)) then
    deallocate(aircraft_doserate, aircraft_doserate_scratch)
  endif

  if (allocated(aircraft_doserate_threshold_height)) deallocate(aircraft_doserate_threshold_height)

  if (allocated(t1_abs)) then
    deallocate(t1_abs, t2_abs)
  endif

  DEALLOCATE ( pdata )
  DEALLOCATE ( iparnum )

  DEALLOCATE ( iplume )
  DEALLOCATE ( plume_release )

  DEALLOCATE( total_activity_released, total_activity_lost_domain, total_activity_lost_other )
  if (allocated(vd_dep)) then
    deallocate(vd_dep)
    deallocate(xflux, yflux, hflux, t2m, z0, leaf_area_index)
    deallocate(roa, ustar, monin_l, raero, vs, rs)
  endif

end subroutine deAllocateFields
end module allocateFieldsML
