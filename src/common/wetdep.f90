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

module wetdep
  use snapdimML, only: mdefcomp
  implicit none
  private

  public wetdep1, wetdep2, wetdep2_init

!> wet deposition rate
  real, save, public :: wetdeprat(mdefcomp)

!> for each component: 0=wet deposition off  1=wet dep. on
  integer, save, public :: kwetdep(mdefcomp)

  real, save :: depconst(mdefcomp)

  contains

!> Purpose:  Compute wet deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> Method:   J.Saltbones 1994
subroutine wetdep1(part, pextra)
  USE particleML, only: Particle, extraParticle
  USE snapfldML, only: depwet
  USE snapparML, only: def_comp
  USE snaptabML, only: mpretab, premult, pretab

!> particle
  type(Particle), intent(inout) :: part
!> reading and possibly resetting prc
  type(extraParticle), intent(inout) :: pextra

  integer :: m,itab,i,j,mm
  real :: precint,probab,prand,dep

  m = part%icomp
  if(kwetdep(m) == 1 .AND. pextra%prc > 0.0) then
  !..find particles with wet deposition and
  !..reset precipitation to zero if not wet deposition
    precint = pextra%prc
    itab = nint(precint*premult)
    itab = min(itab, mpretab)
    probab = pretab(itab)
  !..the rand function returns random real numbers between 0.0 and 1.0
    call random_number(prand)
    if(prand > probab) then
      pextra%prc = 0.0
    else
      dep = wetdeprat(m)*part%rad
      part%rad = part%rad - dep
      i = nint(part%x)
      j = nint(part%y)
      mm = def_comp(m)%to_running
    ! omp atomic
      depwet(i,j,mm) = depwet(i,j,mm) + dble(dep)
    end if
  end if
end subroutine wetdep1


!> Purpose:  Compute wet deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> NOTE: ::wetdep2_init must be run first
!>
!> Method:   J.Bartnicki 2003
subroutine wetdep2(tstep, part, pextra)
  USE particleML, only: Particle, extraParticle
  USE snapfldML, only: depwet
  USE snapparML, only: def_comp
  USE vgravtablesML, only: radiusmym

  real, intent(in) ::    tstep
!> particle
  type(Particle), intent(inout) :: part
  type(extraParticle), intent(in) :: pextra

  integer :: m,i,j,mm
  real :: precint,deprate,dep,q


  m = part%icomp
  if(kwetdep(m) == 1 .AND. pextra%prc > 0.0 &
      .AND. part%z > 0.67) then
  !..find particles with wet deposition and
  !..reset precipitation to zero if not wet deposition
    precint = pextra%prc
    q = precint

    deprate = wet_deposition_rate(radiusmym(m), q, depconst(m), tstep)

  ! b... 25.04.12 wet deposition for convective and gases
    dep = deprate*part%rad
    if(dep > part%rad) dep = part%rad
    part%rad = part%rad-dep
    i = nint(part%x)
    j = nint(part%y)
    mm = def_comp(m)%to_running

  ! omp atomic
    depwet(i,j,mm) = depwet(i,j,mm) + dble(dep)
  end if
end subroutine wetdep2

!> Initialisation routine for ::wetdep2
subroutine wetdep2_init(tstep)
! initalization
  USE snapdebug, only: iulog
  USE particleML, only: Particle, extraParticle
  USE snapparML, only: ncomp
  USE snapdimML, only: mdefcomp
  USE vgravtablesML, only: radiusmym

!> Timestep in seconds
  real, intent(in) :: tstep

  integer :: m,n
  real :: q

  real :: ratdep(mdefcomp)
  real :: rm

  do m=1,ncomp
    rm = radiusmym(m)
    depconst(m) = wet_deposition_constant(rm)
    write(iulog,*) 'WETDEP2 m,r,depconst(m): ',m,rm,depconst(m)
  end do

  write(iulog,*) '-------------------------------------------------'
  write(iulog,*) 'WETDEP2 PREPARE .... q,deprate(1:ncomp):'

  do n=1,200
    q=float(n)*0.1
    do m=1,ncomp
      ratdep(m) = wet_deposition_rate(radiusmym(m), q, depconst(m), tstep)
    end do
    write(iulog,1010) q,(ratdep(m),m=1,ncomp)
1010 format(1x,f5.1,':',12f7.4)
  end do
  write(iulog,*) '-------------------------------------------------'
end subroutine

pure real function wet_deposition_constant(rm) result(depconst)
!> Radius in micrometer
  real, intent(in) :: rm

  real, parameter :: b0 = -0.1483
  real, parameter :: b1 = 0.3220133
  real, parameter :: b2 = -3.0062e-2
  real, parameter :: b3 = 9.34458e-4

  depconst = b0 + b1*rm + b2*rm*rm + b3*rm*rm*rm
end function

pure real function wet_deposition_rate(radius, q, depconst, tstep) result(deprate)
! ... 23.04.12 - gas, particle 0.1<d<10, particle d>10 - J. Bartnicki|
! ... 12.12.13 - gas 'particle size' changed to 0.05um - H. Klein
  real, intent(in) :: radius
  real, intent(in) :: q
  real, intent(in) :: depconst
  real, intent(in) :: tstep

  real, parameter :: a0 = 8.4e-5
  real, parameter :: a1 = 2.7e-4
  real, parameter :: a2 = -3.618e-6

  real :: rkw

  if(radius > 0.05 .AND. radius <= 1.4) then
    rkw = a0*q**0.79
  endif
  if(radius > 1.4 .AND. radius <= 10.0) then
    rkw = depconst*(a1*q + a2*q*q)
  endif
  if(radius > 10.0) then
    rkw = a1*q + a2*q*q
  endif
  if(q > 7.0) then ! convective
    rkw = 3.36e-4*q**0.79
  endif
  if(radius <= 0.1) then ! gas
    rkw = 1.12e-4*q**0.79
  endif
  deprate = 1.0 - exp(-tstep*rkw)
end function
end module wetdep
