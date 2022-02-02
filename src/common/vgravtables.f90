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

module vgravtablesML
  use drydep, only: drydep_scheme, DRYDEP_SCHEME_EMEP, DRYDEP_SCHEME_ZHANG, DRYDEP_SCHEME_EMERSON
  implicit none
  private

!> number of temperature levels in ::vgtable
  integer, parameter, public :: numtempvg = 41
!> number of pressure levels in ::vgtable
  integer, parameter, public :: numpresvg = 25

!> table of gravity in m/s
!>
!> vgtable(temperature,pressure,component)
  real, save, allocatable, public :: vgtable(:,:,:)

  real, parameter, public :: tincrvg = 200.0/float(numtempvg - 1)
  real, parameter, public :: tbasevg = 273. - 120. - tincrvg
  real, parameter, public :: pincrvg = 1200./float(numpresvg-1)
  real, parameter, public :: pbasevg = 0. - pincrvg

  public :: vgravtables

  contains

!>  program for calculating the gravitational settling velocities
!>  for small and large particles (outside the Stokes law)
subroutine vgravtables
  USE ISO_FORTRAN_ENV, only: real64
  USE snapparML, only: ncomp, run_comp, def_comp
  USE drydep, only: gravitational_settling

  real, parameter :: R = 287.05
  !> absolute temperature (K)
  real :: t
  !> particle size
  real :: diam_part
  !> particle density
  real :: rho_part
  !> gravitational setling
  real :: vg
  !> gravitational setling after iteration
  real :: vgmod
  !> atmospheric pressure
  real :: p
  real(real64) :: roa

  integer :: n,m,ip,it
!---------------------------------------

  if (.not.allocated(vgtable)) allocate(vgtable(numtempvg,numpresvg,ncomp))


do_comp: do n=1,ncomp

    m= run_comp(n)%to_defined
    if (def_comp(m)%grav_type /= 2) then ! Not using gravity table
      cycle do_comp
    endif

    select case(drydep_scheme)
    case (DRYDEP_SCHEME_EMEP,DRYDEP_SCHEME_ZHANG,DRYDEP_SCHEME_EMERSON)
      ! expected kg/m3
      rho_part = def_comp(m)%densitygcm3 / 1000.0
      ! expected m
      diam_part = 2 * def_comp(m)%radiusmym / 1e6

      do ip=1,numpresvg
        ! Expecting pascal
        p = max(1.0, pbasevg + ip*pincrvg) / 100.0
        do it=1,numtempvg
          t = tbasevg + it*tincrvg
          roa = p / (real(t, kind=real64) * R)
          vgtable(it, ip, n) = gravitational_settling(roa, real(diam_part, kind=real64), real(rho_part, kind=real64))
        end do
      end do
    case default
      ! radius to diameter
      diam_part = 2 * def_comp(m)%radiusmym
      rho_part = def_comp(m)%densitygcm3

      do ip=1,numpresvg

        p= pbasevg + ip*pincrvg
        if(p < 1.) p=1.

        do it=1,numtempvg

          t= tbasevg + it*tincrvg

          vg=vgrav(diam_part,rho_part,p,t)
          call iter(vgmod,vg,diam_part,rho_part,p,t)

        !..table in unit m/s (cm/s computed)
          vgtable(it,ip,n)= vgmod*0.01

        end do
      end do
    end select
  end do do_comp
end subroutine vgravtables

!>  function for calculating viscosity of the air depending on
!>  temperature. According to RAFF (1999), Kyle (1991).
!>
!>  etha(T) = 1.72e-2*(393/(T+120))*(T/273)**1.5
!>
!>  etha         - viscosity of the air (g cm-1 s-1)
!>
!>  T             - absolute temperature (K)
  pure real function visc(t)
! c    real etha    ! viscosity of the air (g cm-1 s-1)
    real, intent(in) :: t !< absolute temperature (K)

    visc=1.72e-4*(393.0/(t+120.0))*(t/273.0)**1.5
  end function visc

!>  function for calculating Cunningham factor for the small particles
!>  according to Zannetti (1990).
!>
!>  c = 1 + (2*l/dp)*(a1+a2*exp(-a3*dp/l))
!>
!>  c             - Cunningham factor
!>
!>  l=0.0653 um     - free path of air molecules
!>
!>  dp            - particle size (*10e-4 for dp in micro meters)
!>
!>  a1=1.257        - constant
!>
!>  a2=0.40        - constant
!>
!>  a3=0.55        - constant
!>
  pure real function cun(dp)
    real, intent(in) :: dp !< particle size in micro meters
! c    real c        ! Cunningham factor
    real, parameter :: a1=1.257,a2=0.40,a3=0.55 ! constants
! real :: dp_fac    ! conversion factor micro meters -> cm
    real, parameter :: l=0.0653        ! free path of air molecules

    cun=1.0+(2.0*l/dp)*(a1+a2*exp(-a3*dp/l))
  end function cun

!> function for calculating function for iteration procedure
  pure real function fit(u,dp,rp,p,t)
    real, intent(in) :: u !< vg during the iteration
    real, intent(in) :: dp !< particle size in micro meters
    real, intent(in) :: rp !< particle density in g/cm3
    real, intent(in) :: p !< atmospheric pressure (hPa)
    real, intent(in) :: t !< temperature of the air, absolute temperature (K)

    real :: u0        ! vg according to Stokes low
    real, parameter :: a1=0.15,a2=0.687 ! constants
    real :: etha    ! viscosity of the air (g cm-1 s-1)
    real :: re        ! Reynolds number

    u0=vgrav(dp,rp,p,t)
    etha=visc(t)
    re=u*dp*1.0e-4*roa(p,t)/etha
    fit=u*(1.0+a1*re**a2)-u0
    if(u == 0.0) fit=-u0
  end function fit

!>  program for calculating gravitational setling velocity for particles
!>  according to Zannetti (1990).
!>
!>  vg = g*(rp-ra(p,t))*c(dp)*dp**2/(18*etha(t))
!>
!>  vg=vg(dp,rp,p,t)     - gravitational setling (cm/s)
!>
!>  etha=etha(t)    - viscosity of the air (g cm-1 s-1)
!>  c(dp)        - Cunningham factor for the small particles
  pure real function vgrav(dp,rp,p,t)
    real, intent(in) :: dp !< particle size in micro meters
    real, intent(in) :: rp !< density of particle (g/cm3)
    real, intent(in) :: p !< atmospheric presure (hPa)
    real, intent(in) :: t !< air absolute temperature (K)

    real, parameter :: g = 981.0 ! acceleration of gravity
    real :: ra        ! density of the air
!   real, parameter :: dp_fac = 1.0e-4 ! conversion factor micro meters -> cm
    real :: etha    ! viscosity of the air

    ra=roa(p,t)
    etha=visc(t)
    vgrav=(dp*0.0001)**2*g*(rp-ra)*cun(dp)/(18.0*etha)

  end function vgrav

!>  iteration procedure for calculating vg
subroutine iter(vg,u0,dp,rp,p,t)
  real, intent(out) :: vg !< computed gravitational settling velocity
  real, intent(in) :: u0 !< vg according to Stokes low
  real, intent(in) :: dp !< particle size (diameter) in micro meters
  real, intent(in) :: rp !< particle density in g/cm3
  real, intent(in) :: p !< atmospheric pressure
  real, intent(in) :: t !< temperature of the air

  real :: eps    ! accuracy of computed vg (0.1%)
  real :: x1,x2    ! boundary of the domain for fit
  real :: x        ! value from the fit domain
  real :: y,y1,y2    ! fit values during the iteration
  integer :: it    ! itereation number
!---------------------------------------
  eps=0.001*u0
  x1=0.0
  x2=u0
  x=x2
  y1=fit(x1,dp,rp,p,t)
  y2=fit(x2,dp,rp,p,t)
  y=fit(x,dp,rp,p,t)

  it=0
  do while (it < 100)
    it=it+1
    x=0.5*(x2+x1)
    y=fit(x,dp,rp,p,t)
    if(abs(y) < eps) then
      vg=x
      return
    endif
    if(y1*y < 0.0) then
      x2=x
      y2=fit(x2,dp,rp,p,t)
    else
      x1=x
      y1=fit(x1,dp,rp,p,t)
    endif
  enddo

  error stop "Could not determine vg"

  return
end subroutine iter

!>  function for calculating density of the air depending on
!>  temperature and pressure. According to RAFF (1999).
!>
!>  ro(P,T) = P/(R*T)
!>
!>  ro         - density of the dry air (g/cm3)
  pure real function roa(p,t)
    real, intent(in) :: t !< absolute temperature (K)
    real, intent(in) :: p !< presure (hPa)
    real, parameter :: r = 287.04 ! gas constant

    roa=0.001*p*100.0/(r*t)

  end function roa
end module vgravtablesML
