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
  use snapdimML, only: mdefcomp
  implicit none
  private

  public decay, decayDeps

!> for each component: 0=radioactive decay off  1=decay on
  integer, save, public :: kdecay(mdefcomp)
!> radioactive half lifetime (hours)
  real, save, public :: halftime(mdefcomp)
!> radioactive decay (rate)
  real, save, public :: decayrate(mdefcomp)

  contains

!>  Purpose:  Decrease radioactive contents due to decay
!>
!>  WARNING:   make sure ::decayDeps is run once before running decay
subroutine decay(part)
  use particleML, only: Particle

  type(Particle), intent(inout) :: part
  integer :: m

  m = part%icomp
  if(kdecay(m) == 1) then
    part%rad = part%rad * decayrate(m)
  end if

  return
end subroutine decay

!>  Purpose:  Decrease radioactive contents of deposition fields
!>            due to decay
!>
!>     NEEDS TO BE RUN BEFORE ::decay
subroutine decayDeps(tstep)
  USE snapfldML, only: depdry, depwet, accdry, accwet
  USE snapparML, only: ncomp

  real, intent(in) :: tstep

  integer :: m
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
      depdry(:,:,m) = depdry(:,:,m)*decayrate(m)
      depwet(:,:,m) = depwet(:,:,m)*decayrate(m)
      accdry(:,:,m) = accdry(:,:,m)*decayrate(m)
      accwet(:,:,m) = accwet(:,:,m)*decayrate(m)
    endif
  enddo
  return
end subroutine decayDeps
end module decayML
