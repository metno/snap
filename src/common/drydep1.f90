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

module drydep1ML
  implicit none
  private

  public drydep1

  contains

subroutine drydep1(n)
  USE particleML
  USE snapfldML
  USE snapparML
  USE snapdimML, only: nx,ny,nk


!  Purpose:  Compute dry deposition for each particle and each component
!            and store depositions in nearest gridpoint in a field
!  Method:   J.Saltbones 1994


  implicit none

! particle loop index, n=0 means init
  INTEGER, INTENT(IN) :: n
  integer :: m,i,j,mm
  real ::    h,dep


!      do n=1,npart // particle loop now outside
  m= icomp(n)
  if(kdrydep(m) == 1) then
  !..very rough eastimate of height,
  !..using boundary layer height, then just linear in sigma/eta !!! ????
    h=pdata(n)%hbl*(1.-pdata(n)%z)/(1.-pdata(n)%tbl)
    if(h < drydephgt(m)) then
      dep=drydeprat(m)*pdata(n)%rad
      pdata(n)%rad=pdata(n)%rad-dep
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      mm=iruncomp(m)
    ! omp atomic
      depdry(i,j,mm)=depdry(i,j,mm)+dble(dep)
    end if
  end if
!      end do

  return
end subroutine drydep1
end module drydep1ML
