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

module decayML
  implicit none
  private

  public decay, decayDeps

  contains

subroutine decay(n)
  USE snapparML
!  Purpose:  Decrease radioactive contents due to decay
!    WARNING:   make sure decayDeps is run once before running decay

  use particleML
  implicit none

  integer, INTENT(IN) :: n
  integer :: m


!      do n=1,npart loop outside this function
  m= icomp(n)
  if(kdecay(m) == 1) then
    pdata(n)%rad= pdata(n)%rad * decayrate(m)
  end if
!      end do

  return
end subroutine decay

subroutine decayDeps(tstep)
  USE snapfldML
  USE snapparML
  USE snapdimML, only: nx,ny
!  Purpose:  Decrease radioactive contents of deposition fields
!            due to decay
!     NEEDS TO BE RUN BEFORE 1 decay

  implicit none

  real :: tstep

  integer :: i,j,m

  logical, save :: prepare = .TRUE. 



  if(prepare) then
  
  !..radioactive decay rate
    do m=1,ncomp
      if (kdecay(m) == 1) then
        decayrate(m)= exp(-log(2.0)*tstep/(halftime(m)*3600.))
      else
        decayrate(m)=1.0
      end if
    end do
  
    prepare= .FALSE. 
  end if

  do m=1,ncomp
    if(kdecay(m) == 1) then
      do j=1,ny
        do i=1,nx
          depdry(i,j,m)=depdry(i,j,m)*decayrate(m)
          depwet(i,j,m)=depwet(i,j,m)*decayrate(m)
          accdry(i,j,m)=accdry(i,j,m)*decayrate(m)
          accwet(i,j,m)=accwet(i,j,m)*decayrate(m)
        enddo
      enddo
    endif
  enddo
  return
end subroutine decayDeps
end module decayML
