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

module rwalkML
  USE iso_fortran_env, only: real64

  implicit none
  private

  real(real64), save :: vrdbla ! l-eta above mixing height
  real(real64), save :: tfactor ! tfactor=tstep/tmix
  real(real64), save :: tstep

  real(real64), parameter :: hmax = 2500.0 ! maximum mixing height
  real(real64), parameter :: tmix = 15.0*60.0 ! Characteristic mixing time = 15 min
  real(real64), parameter :: lmax = 0.28 ! Maximum l-eta in the mixing layer
  real(real64), parameter :: labove = 0.03 ! Standard l-eta above the mixing layer

  public rwalk, rwalk_init

  contains

!> Initialise constants needed for rwalk
subroutine rwalk_init(timestep)
!> time step in seconds (trajectory calculations)
  real, intent(in) :: timestep

  tfactor = timestep/tmix
  tstep = timestep

  ! l-eta above mixing height
  vrdbla = labove*tfactor
end subroutine

!>  Purpose:  Diffusion, in and above boudary layer.
!>
!>  Method:   Random walk.
!>
!> ::rwalk_init must be run before rwalk
subroutine rwalk(blfullmix,part,pextra)
!   24.04.2009 Jerzy Bartnicki: Model particle which goes below the
!   ground or above the top boundary in the random walk is reflected
!   26.03.2011 Jerzy Bartnicki: New parameterization of vertical diffusion in the
!   mixing layer. l-eta proportional to mixing height and the time step.
!   For mixing height = 2500 m and time step = 15 min:
!   In ABL: l-eta=0.28
!   Above ABL: l-eta=0.003
!   For 200< mixing height<2500 and arbitrary time step:
!   In ABL: l-eta=0.28*(mh/2500m)*(tstep/tstep-mix)
!   Above ABL: l-eta=0.003*(tstep/tstep-mix)
!   Entrainment zone = 10%*h
  USE particleML, only: extraParticle, Particle
!> full mixing in boundarylayer (true=old,false=new)
  logical, intent(in) :: blfullmix
!> particle with information
  type(Particle), intent(inout)  :: part
!> extra information regarding the particle (u, v, rmx, rmy)
  type(extraParticle), intent(in) :: pextra

  real(real64) :: rnd(3), rl, vabs
  real(real64) :: hfactor, rv, rvmax

  real(real64) :: a
  real(real64), parameter :: b = 0.875


! the random_number function returns 3 (x,y,z) random real numbers between 0.0 and 1.0
  call random_number(rnd)
  rnd = rnd - 0.5

! horizontal diffusion
  if (part%z > part%tbl) then ! in boundary layer
    a = 0.5
  else ! above boundary layer
    a = 0.25
  endif

  vabs = hypot(pextra%u, pextra%v)
  rl = 2*a*((vabs*tstep)**b)
  part%x = part%x + rl*rnd(1)*pextra%rmx
  part%y = part%y + rl*rnd(2)*pextra%rmy


! vertical diffusion
  if (part%z <= part%tbl) then ! Above boundary layer
      part%z = part%z + vrdbla*rnd(3)
  else ! In boundary layer
    if (blfullmix) then
      part%z = 1.0 - (1.0 - part%tbl)*1.1*(rnd(3)+0.5)
    else ! not full mixing
      hfactor = part%hbl/hmax
      rv = lmax*hfactor*tfactor
      rvmax = 1.0 - part%tbl

      rv = min(rv, rvmax)
      part%z = part%z + rv*rnd(3)

    !... reflection from the ABL top
      if(part%z < part%tbl) then
        part%z = 2.0*part%tbl - part%z
      endif

    !... reflection from the bottom
      if(part%z > 1.0) then
        part%z = 2.0 - part%z
      endif

    !..vertical limits
      part%z = min(part%z, 1.0d0)
      part%z = max(part%z, real(part%tbl, kind=kind(part%z)))
    end if
  end if
end subroutine rwalk
end module rwalkML
