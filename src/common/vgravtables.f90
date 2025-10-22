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
  implicit none
  private

!> number of temperature levels in ::vgtable
  integer, parameter, public :: numtempvg = 41 ! 5deg incr
!> number of pressure levels in ::vgtable
  integer, parameter, public :: numpresvg = 25 ! 200hPa

!> table of gravity in m/s
!>
!> vgtable(temperature,pressure,component%running)
  real, save, allocatable, private :: vgtable(:,:,:)

  real, parameter, private :: tincrvg = 200.0/float(numtempvg - 1)
  real, parameter, private :: tbasevg = 273. - 120. - tincrvg
  real, parameter, private :: pincrvg = 1200./float(numpresvg-1)
  real, parameter, private :: pbasevg = 0. - pincrvg

  public :: vgravtables_init, vgrav, vgrav_zanetti

  contains

!>  program for calculating the gravitational settling velocities
!>  for small and large particles (outside the Stokes law)
subroutine vgravtables
  USE ISO_FORTRAN_ENV, only: real64
  USE snapparML, only: ncomp, run_comp, def_comp

  !> absolute temperature (K)
  real :: t
  !> particle size (µm)
  real :: diam_part
  !> particle density (g cm-3)
  real :: rho_part
  !> gravitational setling
  real :: vg
  !> gravitational setling after iteration
  real :: vgmod
  !> atmospheric pressure (hPa)
  real :: p

  integer :: n,m,ip,it
!---------------------------------------

  if (.not.allocated(vgtable)) allocate(vgtable(numtempvg,numpresvg,ncomp))


  do_comp: do n=1,ncomp

    m= run_comp(n)%to_defined
    if (def_comp(m)%grav_type /= 2) then ! Not using gravity table
      cycle do_comp
    endif

    ! radius to diameter
    diam_part = 2 * def_comp(m)%radiusmym
    rho_part = def_comp(m)%densitygcm3

    do ip=1,numpresvg

      p= pbasevg + ip*pincrvg
      if(p < 1.) p=1.

      do it=1,numtempvg

        t= tbasevg + it*tincrvg

        vg=vgrav_zanetti(diam_part,rho_part,p,t)
        call iter(vgmod,vg,diam_part,rho_part,p,t)

      !..table in unit m/s (cm/s computed)
        vgtable(it,ip,n)= vgmod*0.01
      end do
    end do
  end do do_comp
end subroutine vgravtables

!> initialize gravitational tables for all components if needed
!> and print surface gravity to log
subroutine vgravtables_init()
  !USE vgravtablesML, only: vgravtables, vgtable, pbasevg, tbasevg, pincrvg, tincrvg
  USE iso_fortran_env, only: real64
  USE snapparML, only: ncomp, run_comp, def_comp
  USE snapdebug, only: iulog

  integer :: i,m,ip,it
  logical :: compute_grav_table

  compute_grav_table = .false.
  grav_do: do i=1,ncomp
    m= run_comp(i)%to_defined
    if(def_comp(m)%grav_type > 1) then
      compute_grav_table = .true.
      exit grav_do
    endif
  end do grav_do
  if (.not.compute_grav_table) then
    write(iulog,*) 'Computation of gravity tables not needed'
    return
  endif

  write(iulog,*) 'Computing gravity tables...'
  call vgravtables()
  write(iulog,*) 'Surface gravity (1013hPa, 288K), direct and interpolated:'
  it = (288-tbasevg)/tincrvg
  ip = (1013-pbasevg)/pincrvg
  do i=1,ncomp
    m = run_comp(i)%to_defined
    write(iulog,*) ' particle ', def_comp(m)%compname, ": ", vgtable(it,ip,i), vgrav(i,1000., 300.)
  end do
end subroutine


!>  function for calculating viscosity of the air depending on
!>  temperature. According to RAFF (1999), Kyle (1991).
!>
!>  etha(T) = 1.72e-2*(393/(T+120))*(T/273)**1.5
!>
!>  etha         - viscosity of the air (g cm-1 s-1)
!>
!>  T             - absolute temperature (K)
  elemental real function visc(t)
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
  pure elemental real function cun(dp)
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

    u0=vgrav_zanetti(dp,rp,p,t)
    etha=visc(t)
    re=u*dp*1.0e-4*roa(p,t)/etha
    fit=u*(1.0+a1*re**a2)-u0
    if(u == 0.0) fit=-u0
  end function fit


!>  access gravitational velocity interpolated from precomputed lookup-tables
!>  vgrav in m/s
  elemental real function vgrav(run_comp, p,t)
    USE iso_fortran_env, only: real64

    integer, intent(in) :: run_comp !< running component number
    real, intent(in) :: p !< atmospheric presure (hPa)
    real, intent(in) :: t !< air absolute temperature (K)

    real(real64) :: grav1, grav2, pvg, tvg
    integer :: ip, it

    ! old       gravity= vgrav(radiusmym(m),densitygcm3(m),p,t)
      ip = (p-pbasevg)/pincrvg
      ip = max(ip, 1)
      ip = min(size(vgtable,2)-1, ip)
      pvg = pbasevg + ip*pincrvg
      it = (t-tbasevg)/tincrvg
      it = max(it, 1)
      it = min(size(vgtable,1)-1, it)
      tvg = tbasevg + it*tincrvg

      grav1 = vgtable(it,ip,run_comp) &
          + (vgtable(it+1,ip,run_comp)-vgtable(it,ip,run_comp)) &
          *(t-tvg)/tincrvg
      ip = ip + 1
      grav2 = vgtable(it,ip,run_comp) &
          + (vgtable(it+1,ip,run_comp)-vgtable(it,ip,run_comp)) &
          *(t-tvg)/tincrvg
      vgrav = grav1 + (grav2-grav1) * (p-pvg)/pincrvg

  end function vgrav

!>  program for calculating gravitational setling velocity for particles
!>  according to Zannetti (1990).
!>
!>  vg = g*(rp-ra(p,t))*c(dp)*dp**2/(18*etha(t))
!>
!>  vg=vg(dp,rp,p,t)     - gravitational setling (cm/s)
!>
!>  etha=etha(t)    - viscosity of the air (g cm-1 s-1)
!>  c(dp)        - Cunningham factor for the small particles
  elemental real function vgrav_zanetti(dp,rp,p,t)
    real, intent(in) :: dp !< particle size in micro meters
    real, intent(in) :: rp !< density of particle (g/cm3)
    real, intent(in) :: p !< atmospheric presure (hPa)
    real, intent(in) :: t !< air absolute temperature (K)

    real, parameter :: g = 981.0 ! acceleration of gravity

    vgrav_zanetti=(dp*0.0001)**2*g*(rp-roa(p,t))*cun(dp)/(18.0*visc(t))

  end function vgrav_zanetti

!>  iteration procedure for calculating vg
subroutine iter(vg,u0,dp,rp,p,t)
  real, intent(out) :: vg !< computed gravitational settling velocity in cm/s
  real, intent(in) :: u0 !< vg according to Stokes law in cm/s
  real, intent(in) :: dp !< particle size (diameter) in micro meters
  real, intent(in) :: rp !< particle density in g/cm3
  real, intent(in) :: p !< atmospheric pressure in hPa
  real, intent(in) :: t !< temperature of the air in K

  real :: eps    ! accuracy of computed vg (0.1%)
  real :: x1,x2    ! boundary of the domain for fit
  real :: x        ! value from the fit domain
  real :: y,y1,y2    ! fit values during the iteration
  integer :: it    ! iteration number
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
  elemental real function roa(p,t)
    real, intent(in) :: t !< absolute temperature (K)
    real, intent(in) :: p !< presure (hPa)
    real, parameter :: r = 287.04 ! Specific gas constant (J kg-1 K-1)

    roa=0.001*p*100.0/(r*t)

  end function roa
end module vgravtablesML
