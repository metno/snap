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

!>  Purpose:  Decrease radioactive contents due to decay
!>
!>  WARNING:   make sure ::decaydeps is run once before running decay
subroutine decay(part)
  use particleML, only: Particle
  use snapparML, only: def_comp

  type(Particle), intent(inout) :: part
  integer :: m
  real :: removed

  m = part%icomp
  if(def_comp(m)%kdecay == 1) then
    removed = part%scale_rad(def_comp(m)%decayrate)
  end if

  return
end subroutine decay

!>  Purpose:  Decrease radioactive contents of deposition fields
!>            due to decay
!>
!>     NEEDS TO BE RUN BEFORE ::decay
subroutine decayDeps(tstep)
  USE snapfldML, only: depdry, depwet, accdry, accwet, &
    total_activity_released, total_activity_lost_domain, total_activity_lost_other
  USE snapparML, only: ncomp, run_comp, def_comp

  real, intent(in) :: tstep

  integer :: m, mm
  logical, save :: prepare = .TRUE.

  if(prepare) then

  !..radioactive decay rate
    do m=1,ncomp
      mm = run_comp(m)%to_defined
      if (def_comp(mm)%kdecay == 1) then
        def_comp(mm)%decayrate = exp(-log(2.0)*tstep/(def_comp(mm)%halftime*3600.))
      else
        def_comp(mm)%decayrate = 1.0
      end if
    end do

    prepare= .FALSE.
  end if

  do m=1,ncomp
    mm = run_comp(m)%to_defined
    if(def_comp(mm)%kdecay == 1) then
      depdry(:,:,m) = depdry(:,:,m)*def_comp(mm)%decayrate
      depwet(:,:,m) = depwet(:,:,m)*def_comp(mm)%decayrate
      accdry(:,:,m) = accdry(:,:,m)*def_comp(mm)%decayrate
      accwet(:,:,m) = accwet(:,:,m)*def_comp(mm)%decayrate

      total_activity_released(m) = total_activity_released(m)*def_comp(mm)%decayrate
      total_activity_lost_domain(m) = total_activity_lost_domain(m)*def_comp(mm)%decayrate
      total_activity_lost_other(m) = total_activity_lost_other(m)*def_comp(mm)%decayrate
    endif
  enddo
  return
end subroutine decayDeps
end module decayML
