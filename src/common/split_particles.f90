! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2020   Norwegian Meteorological Institute

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

module split_particlesML
  implicit none
  private

  public split_particles

  contains

!> split particles after configurable number of steps to
!> to be able to start with few particles close to the
!> the source and double number after some transport to
!> cover enough cells
!> content of particle is split between the old and new
!> particle, but the new particle is allocated to a new
!> plume
subroutine split_particles(split_particle_after_step)
  USE iso_fortran_env, only: error_unit
  USE snapdebug, only: iulog, idebug
  USE snapparML, only: ncomp
  USE particleML, only: pdata
  USE releaseML, only: iplume, plume_release, nplume, mplume, npart, mpart

  !> number of timesteps before a particle will be split
  integer, intent(IN) :: split_particle_after_step
  integer :: n, np, npl, nplume_old
  real :: removed

  nplume_old = nplume

  do npl = 1, nplume_old
    if (modulo(iplume(npl)%ageInSteps, split_particle_after_step) /= 0) then
      cycle ! no split needed yet
    end if
    if (nplume >= mplume) then
      if (idebug >= 1) write(iulog,*) 'too many plumes, stop splitting'
      return
    end if
    nplume = nplume + 1
    iplume(nplume)%start = 0
    iplume(nplume)%end = -1
    iplume(nplume)%ageInSteps = iplume(npl)%ageInSteps
    do n = 1, ncomp
      plume_release(npl, n) = plume_release(npl, n) / 2. ! plume split 
      plume_release(nplume, n) = plume_release(npl, n)
    end do
    do np = iplume(npl)%start, iplume(npl)%end
      if (npart >= mpart) then
        if (idebug >= 1) write(iulog,*) 'too many particles, stop splitting'
        return
      end if
      ! split particles, add one to the new plume at the end
      npart = npart + 1
      iplume(nplume)%end = npart
      if (iplume(nplume)%start .eq. 0) then
        iplume(nplume)%start = npart
      end if

      removed = pdata(np)%scale_rad(0.5)
      pdata(npart) = pdata(np)
    end do
  end do

end subroutine split_particles
end module split_particlesML
