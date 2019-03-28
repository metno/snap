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

module ftestML
  implicit none
  private

  public ftest

  contains

subroutine ftest(name,k1,k2,nx,ny,nk,field,iundef)

!  Purpose: Test field, print min,mean,max values.


#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

  integer ::       k1,k2,nx,ny,nk,iundef
  real ::          field(nx,ny,nk)
  character*(*) name

  integer :: kstep,i,j,k,ndef
  real ::    undef,ud,f,fmin,fmax,fmean

  double precision :: fsum

#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('FTEST',0,ZHOOK_HANDLE)
#endif
  if(k1 < 1 .OR. k1 > nk) k1=1
  if(k2 < 1 .OR. k2 > nk) k2=nk
  kstep=+1
  if(k1 > k2) kstep=-1

  undef=+1.e+35
  ud=undef*0.9

  do k=k1,k2,kstep
    fmin=+undef
    fmax=-undef
    fsum=0.
    if(iundef == 0) then

    ! OMP PARALLEL DO PRIVATE(j,i) REDUCTION(max : fmax)
    ! OMP&            REDUCTION(min : fmin) REDUCTION( + : fsum)
    ! OMP&            COLLAPSE(2)
      do j=1,ny
        do i=1,nx
          fmin=min(fmin,field(i,j,k))
          fmax=max(fmax,field(i,j,k))
          fsum=fsum+field(i,j,k)
        end do
      end do
    ! OMP END PARALLEL DO
      ndef=nx*ny
    else
      ndef=0
    ! OMP PARALLEL DO PRIVATE(j,i) REDUCTION(max : fmax)
    ! OMP&            REDUCTION(min : fmin) REDUCTION( + : fsum, ndef)
    ! OMP&            COLLAPSE(2)
      do j=1,ny
        do i=1,nx
          if(field(i,j,k) < ud) then
            fmin=min(fmin,field(i,j,k))
            fmax=max(fmax,field(i,j,k))
            fsum=fsum+field(i,j,k)
            ndef=ndef+1
          end if
        end do
      end do
    ! OMP END PARALLEL DO
    end if
    if(ndef > 0) then
      fmean=fsum/dble(ndef)
    else
      fmin=0.
      fmax=0.
      fmean=0.
    end if
    if(k1 /= k2) then
      write(9,fmt='(5x,a8,1x,i3,3(1x,e13.5))') &
      name,k,fmin,fmean,fmax
    else
      write(9,fmt='(5x,a8,1x,3x,3(1x,e13.5))') &
      name,  fmin,fmean,fmax
    end if
  end do
  flush(9)

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('FTEST',1,ZHOOK_HANDLE)
#endif
  return
end subroutine ftest
end module ftestML
