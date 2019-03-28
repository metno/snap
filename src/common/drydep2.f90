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

module drydep2ML
  implicit none
  private

  public drydep2

  contains

subroutine drydep2(tstep,n)
  USE particleML
  USE snapfldML
  USE snapparML
  USE snapgrdML, only: vlevel
  USE snapdimML, only: nx,ny,nk

!  Purpose:  Compute dry deposition for each particle and each component
!            and store depositions in nearest gridpoint in a field
!  Method:   J.Bartnicki 2003

! ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|


  implicit none

  real, INTENT(IN) ::    tstep

! particle loop index, n = 0 means init
  INTEGER, INTENT(IN) :: n
  integer :: m,i,j,mm
  real ::    h,deprate,dep
!################################################################
  integer :: numdep
  real :: depmin,depmax,ratmin,ratmax,hblmin,hblmax
  double precision :: totinp,depsum,totsum


  numdep=0
  hblmin=+1.e+38
  hblmax=-1.e+38
  ratmin=+1.e+38
  ratmax=-1.e+38
  depmin=+1.e+38
  depmax=-1.e+38
  totinp=0.0d0
  depsum=0.0d0
  totsum=0.0d0
!################################################################

!     do n=1,npart // particle loop outside
!################################################################
  totinp=totinp+dble(pdata(n)%rad)
!################################################################
  m= icomp(n)
!#### 30m = surface-layer (deposition-layer); sigma(hybrid)=0.996 ~ 30m
  if(kdrydep(m) == 1 .AND. pdata(n)%z > 0.996) then
    h=30.0
  ! b...23.04.12... difference between particle and gas
  
    if(radiusmym(m) <= 0.05) then
    ! gas
      deprate= 1.0-exp(-tstep*(0.008)/h)
    else if (radiusmym(m) <= 10.0) then
    ! particle 0.05<r<10
      deprate= 1.0-exp(-tstep*(0.002)/h)
    else
    ! particle r>=10
      deprate= 1.0-exp(-tstep*(0.002+pdata(n)%grv)/h)
    ! complete deposition when particle hits ground
      if (pdata(n)%z == vlevel(1)) deprate = 1.
    endif
    dep=deprate*pdata(n)%rad
    pdata(n)%rad=pdata(n)%rad-dep
    i=nint(pdata(n)%x)
    j=nint(pdata(n)%y)
    mm=iruncomp(m)
  ! omp atomic
    depdry(i,j,mm)=depdry(i,j,mm)+dble(dep)
  !################################################################
    if(hblmin > h) hblmin=h
    if(hblmax < h) hblmax=h
    if(ratmin > deprate) ratmin=deprate
    if(ratmax < deprate) ratmax=deprate
    if(depmin > dep) depmin=dep
    if(depmax < dep) depmax=dep
    depsum=depsum+dble(dep)
    numdep=numdep+1
  !################################################################
  end if
!################################################################
  totsum=totsum+dble(pdata(n)%rad)
!################################################################
!      end do

!################################################################
!      write(88,*) 'DRYDEP2 numdep,npart:  ',numdep,npart
!      write(88,*) 'DRYDEP2 totinp:        ',totinp
!      write(88,*) 'DRYDEP2 totsum,depsum: ',totsum,depsum
!      if(hblmin.le.hblmax)
!     +   write(88,*) 'DRYDEP2 hblmin,hblmax: ',hblmin,hblmax
!      if(ratmin.le.ratmax)
!     +   write(88,*) 'DRYDEP2 ratmin,ratmax: ',ratmin,ratmax
!      if(depmin.le.depmax)
!     +   write(88,*) 'DRYDEP2 depmin,depmax: ',depmin,depmax
!      write(88,*) '--------'
!################################################################
  return
end subroutine drydep2
end module drydep2ML
