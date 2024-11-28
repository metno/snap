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
  if(def_comp(m)%kdecay >= 1) then
    removed = part%scale_rad(def_comp(m)%decayrate)
  end if

  return
end subroutine decay

!>  Purpose:  Decrease radioactive contents of deposition fields
!>            due to decay
!>
!>     param: tstep model timestep in seconds
!>     NEEDS TO BE RUN BEFORE ::decay
subroutine decayDeps(tstep)
  USE snapfldML, only: depdry, depwet, accdry, accwet, &
    total_activity_released, total_activity_lost_domain, total_activity_lost_other
  USE snapparML, only: ncomp, run_comp, def_comp
  USE iso_fortran_env, only: real64
  USE releaseML, only: tpos_bomb

  real, intent(in) :: tstep

  integer :: m, mm
  real :: bomb_decay_rate, current_state, next_state
  logical, save :: prepare = .TRUE.
  logical, save :: has_bomb_decay = .FALSE.
  !> totalstep in seconds run in decay, assumed to be H+1 after release (stable cloud)
  !> start at 1h to satisfy C(t) = C_0 * t^-1.2 (t in [hrs])
  real(real64), save :: total_steps = 0.

  if(prepare) then
  !..radioactive decay rate
    do m=1,ncomp
      mm = run_comp(m)%to_defined
      if (def_comp(mm)%kdecay == 1) then
        def_comp(mm)%decayrate = exp(-log(2.0)*tstep/(def_comp(mm)%halftime*3600.))
      elseif (def_comp(mm)%kdecay == 2) then
        has_bomb_decay = .TRUE.
      else
        def_comp(mm)%decayrate = 1.
      end if
    end do

    prepare= .FALSE.
  end if

  ! bomb t[h]^-1.2 power-function,
  ! see glassstone/dolan: effects of nuclear weapons
  ! converted  to decay-rate factor per tstep
  ! initial release to be defined at H+1
  if (has_bomb_decay) then
    if (total_steps >= (3600+tpos_bomb)) then
      ! start decay after 1h after explosion
      current_state = ((total_steps-tpos_bomb)/3600.)**(-1.2)
      next_state = ((total_steps+tstep-tpos_bomb)/3600.)**(-1.2)
      bomb_decay_rate = next_state/current_state
    else
      ! no decay before release+1h
      bomb_decay_rate = 1.
    end if
    do m=1,ncomp
      mm = run_comp(m)%to_defined
      if (def_comp(mm)%kdecay == 2) then
        def_comp(mm)%decayrate = bomb_decay_rate
      end if
    end do
    total_steps = total_steps + tstep
  end if

  do m=1,ncomp
    mm = run_comp(m)%to_defined
    if(def_comp(mm)%kdecay >= 1) then
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
