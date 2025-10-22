! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2021   Norwegian Meteorological Institute
!
! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

!> Common container for global timers
module snaptimers
  USE snapdebug, only: acc_timer => prefixed_accumulating_timer
  implicit none
  public
  !> timers used within snap
  type(acc_timer), save :: &
      timeloop_timer, &
      output_timer, &
      input_timer, &
      metcalc_timer, &
      other_timer, &
      release_timer, &
      particleloop_timer

  ! type(acc_timer), save :: &
  contains
    subroutine initialize_timers()
      timeloop_timer = acc_timer("time_loop:")
      output_timer = acc_timer("output/accumulation:")
      input_timer = acc_timer("Reading MET input:")
      metcalc_timer = acc_timer("Calc MET input:")
      other_timer = acc_timer("Others:")
      release_timer = acc_timer("release prep:")
      particleloop_timer = acc_timer("Particle loop:")
    end subroutine


end module snaptimers
