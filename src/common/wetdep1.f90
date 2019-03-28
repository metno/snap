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

module wetdep1ML
  implicit none
  private

  public wetdep1

  contains

subroutine wetdep1(n,pextra)
  USE particleML
  USE snapgrdML
  USE snapfldML
  USE snapparML
  USE snaptabML
  USE snapdimML, only: mpretab

!  Purpose:  Compute wet deposition for each particle and each component
!            and store depositions in nearest gridpoint in a field
!  Method:   J.Saltbones 1994


  implicit none


! particle loop index, n = 0 means init
  INTEGER, INTENT(IN) :: n
  TYPE(extraParticle), INTENT(INOUT) :: pextra
  integer :: m,itab,i,j,mm
  real ::    precint,probab,prand,dep


!      do n=1,npart // particle loop moved outside subroutine
  m= icomp(n)
  if(kwetdep(m) == 1 .AND. pextra%prc > 0.0) then
  !..find particles with wet deposition and
  !..reset precipitation to zero if not wet deposition
    precint=pextra%prc
    itab=nint(precint*premult)
    itab=min(itab,mpretab)
    probab=pretab(itab)
  !..the rand function returns random real numbers between 0.0 and 1.0
    call random_number(prand)
    if(prand > probab) then
      pextra%prc=0.0
    else
      dep=wetdeprat(m)*pdata(n)%rad
      pdata(n)%rad=pdata(n)%rad-dep
      i=nint(pdata(n)%x)
      j=nint(pdata(n)%y)
      mm=iruncomp(m)
    ! omp atomic
      depwet(i,j,mm)=depwet(i,j,mm)+dble(dep)
    end if
  end if
!      end do

  return
end subroutine wetdep1
end module wetdep1ML
