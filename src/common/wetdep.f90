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
  use iso_fortran_env, only: real64
  implicit none
  private

  real, save :: vminprec = -1.
  real, parameter :: precmin = 0.01

  public :: wetdep2, wetdep2_init, &
      wetdep_conventional, wetdep_conventional_init, wetdep_conventional_deinit, wetdep_conventional_compute
  public :: operator(==), operator(/=)
  public :: wetdep_bartnicki
  public :: prepare_wetdep, wetdep_using_precomputed_wscav

  type, public :: wetdep_scheme_t
    integer, private :: scheme
    character(len=32), public :: description
  end type

  type(wetdep_scheme_t), parameter, public :: WETDEP_SCHEME_UNDEFINED = &
      wetdep_scheme_t(0, "Not defined")
  type(wetdep_scheme_t), parameter, public :: WETDEP_SCHEME_BARTNICKI = &
      wetdep_scheme_t(2, "Bartnicki")
  type(wetdep_scheme_t), parameter, public :: WETDEP_SCHEME_CONVENTIONAL = &
      wetdep_scheme_t(3, "Conventional")

  interface operator (==)
    module procedure :: equal_scheme, equal_incloud_scheme
  end interface

  interface operator (/=)
    module procedure :: not_equal_scheme, not_equal_incloud_scheme
  end interface


  type, public :: wetdep_incloud_scheme_t
    integer, private :: scheme
    character(len=32), public :: description
  end type

  type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_UNDEFINED = &
      wetdep_incloud_scheme_t(0, "Not defined")
  type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_NONE = &
      wetdep_incloud_scheme_t(1, "No scheme (skip)")
  type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_GCM = &
      wetdep_incloud_scheme_t(2, "GCM")
  type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_ROSELLE = &
      wetdep_incloud_scheme_t(3, "Roselle")

  type, public :: conventional_params_t
    real(real64) :: A
    real(real64) :: B
  end type

  type(conventional_params_t), save, public :: conventional_params = conventional_params_t(0.0, 0.0)
  real(real64), allocatable, save, public :: conventional_deprate_m1(:,:)

  type(wetdep_scheme_t), save, public :: wetdep_scheme = WETDEP_SCHEME_UNDEFINED
  type(wetdep_incloud_scheme_t), save, public :: wetdep_incloud_scheme = WETDEP_INCLOUD_SCHEME_UNDEFINED

contains

  logical pure function equal_scheme(this, other) result(eq)
    type(wetdep_scheme_t), intent(in) :: this, other
    eq = this%scheme == other%scheme
  end function

  logical pure function not_equal_scheme(this, other) result(eq)
    type(wetdep_scheme_t), intent(in) :: this, other
    eq = .not. (this == other)
  end function

  logical pure function equal_incloud_scheme(this, other) result(eq)
    type(wetdep_incloud_scheme_t), intent(in) :: this, other
    eq = this%scheme == other%scheme
  end function

  logical pure function not_equal_incloud_scheme(this, other) result(eq)
    type(wetdep_incloud_scheme_t), intent(in) :: this, other
    eq = .not. (this == other)
  end function

  subroutine wetdep_bartnicki(wscav, precip, radius)
    use iso_fortran_env, only: real32
    !> 1/s
    real(real32), intent(out) :: wscav(:,:,:)
    !> mm/hr
    real(real32), intent(in) :: precip(:,:,:)
    !> micrometer
    real(real32), intent(in) :: radius

    real, parameter :: a0 = 8.4e-5
    real, parameter :: a1 = 2.7e-4
    real, parameter :: a2 = -3.618e-6

    if (radius > 10.0) then
      wscav = a1*precip + a2*precip*precip
    elseif (radius > 1.4) then
      wscav = wet_deposition_constant(radius)*(a1*precip + a2*precip*precip)
    elseif (radius > 0.1) then
      wscav = a0*precip**0.79
    else ! gas
      wscav = 1.12e-4*precip**0.79
    endif

    if (radius > 0.1) then
      ! Convective rain
      where(precip > 7.0)
        wscav = 3.36e-4*precip**0.79
      endwhere
    endif
  end subroutine

!> Purpose:  Compute wet deposition for each particle and each component
!>           and store depositions in nearest gridpoint in a field
!>
!> NOTE: ::wetdep2_init must be run first
!>
!> Method:   J.Bartnicki 2003
  subroutine wetdep2(depwet, tstep, part, pextra)
    USE iso_fortran_env, only: real64
    USE particleML, only: Particle, extraParticle
    USE snapparML, only: def_comp, run_comp
    USE snapdimML, only: hres_pos

!> Field which ret deposition gets added to
    real(real64), intent(inout) :: depwet(:, :, :)
!> Timestep of the simulation, affects the deposition rate
    real, intent(in) :: tstep
!> particle
    type(Particle), intent(inout) :: part
!> uses the precipitation at the particle position
    type(extraParticle), intent(in) :: pextra

    integer :: m, i, j, mm, mo
    real :: precint, deprate, dep, q

    m = part%icomp

    !..reset precipitation to zero if pressure less than approx. 550 hPa.
    !..and if less than a minimum precipitation intensity (mm/hour)
    if (def_comp(m)%kwetdep == 1 .AND. pextra%prc > precmin &
        .AND. part%z > 0.67 .AND. part%z > vminprec) then
      !..find particles with wet deposition and
      !..reset precipitation to zero if not wet deposition
      precint = pextra%prc
      q = precint

      mm = def_comp(m)%to_running

      deprate = wet_deposition_rate(def_comp(m)%radiusmym, q, run_comp(mm)%depconst, tstep)

      dep = part%scale_rad(1.0 - deprate)

      i = hres_pos(part%x)
      j = hres_pos(part%y)
      mo = def_comp(m)%to_output

      !$OMP atomic
      depwet(i, j, mo) = depwet(i, j, mo) + dble(dep)
    end if
  end subroutine wetdep2

!> Initialisation routine for ::wetdep2
  subroutine wetdep2_init(tstep)
! initalization
    USE snapdebug, only: iulog
    USE particleML, only: Particle, extraParticle
    USE snapparML, only: ncomp
    USE snapparML, only: run_comp, def_comp

!> Timestep in seconds
    real, intent(in) :: tstep

    integer :: m, n, mm
    real :: q

    real, allocatable :: ratdep(:)
    real :: rm

    allocate (ratdep(ncomp))

    do m = 1, ncomp
      mm = run_comp(m)%to_defined
      rm = def_comp(mm)%radiusmym
      run_comp(m)%depconst = wet_deposition_constant(rm)
      write (iulog, *) 'WETDEP2 m,r,depconst(m): ', m, rm, run_comp(m)%depconst
    end do

    write (iulog, *) '-------------------------------------------------'
    write (iulog, *) 'WETDEP2 PREPARE .... q,deprate(1:ncomp):'

    do n = 1, 200
      q = float(n)*0.1
      do m = 1, ncomp
        mm = run_comp(m)%to_defined
        ratdep(m) = wet_deposition_rate(def_comp(mm)%radiusmym, q, run_comp(m)%depconst, tstep)
      end do
      write (iulog, 1010) q, (ratdep(m), m=1, ncomp)
1010  format(1x, f5.1, ':', 12f7.4)
    end do
    write (iulog, *) '-------------------------------------------------'

    block
      USE snapgrdML, only: alevel, blevel, vlevel
      USE snapdimML, only: nk
      USE snapdebug, only: iulog

      integer :: k
      real :: p1,p2
      real, parameter :: plim = 550.0

      if(vminprec < 0.) then
        p2 = 1000.
        k = 1
        do while (p2 > plim .AND. k < nk)
          k = k+1
          p1 = p2
          p2 = alevel(k) + blevel(k)*1000.
        end do
        if(k > 1) then
          vminprec = vlevel(k-1) &
              + (vlevel(k)-vlevel(k-1))*(p1-plim)/(p1-p2)
        else
          vminprec = vlevel(nk)
        end if
        write(iulog,*) 'POSINT. precmin,vminprec: ',precmin,vminprec
      end if
    end block
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

  pure elemental real function wet_deposition_rate_imm(radius, q, depconst) result(rkw)
    !> radius in micrometer
    real, intent(in) :: radius
    !> precipitation intensity in mm/h
    real, intent(in) :: q
    !> deposition constant
    real, intent(in) :: depconst

    real, parameter :: a0 = 8.4e-5
    real, parameter :: a1 = 2.7e-4
    real, parameter :: a2 = -3.618e-6

    rkw = 0
    if (radius > 0.05 .AND. radius <= 1.4) then
      rkw = a0*q**0.79
    endif
    if (radius > 1.4 .AND. radius <= 10.0) then
      rkw = depconst*(a1*q + a2*q*q)
    endif
    if (radius > 10.0) then
      rkw = a1*q + a2*q*q
    endif
    if (q > 7.0) then ! convective
      rkw = 3.36e-4*q**0.79
    endif
    if (radius <= 0.1) then ! gas
      rkw = 1.12e-4*q**0.79
    endif
  end function

  pure elemental real function wet_deposition_rate(radius, q, depconst, tstep) result(deprate)
    !> radius in micrometer
    real, intent(in) :: radius
    !> precipitation intensity in mm/h
    real, intent(in) :: q
    !> deposition constant
    real, intent(in) :: depconst
    !> length of a timestep in seconds
    real, intent(in) :: tstep

    real :: rkw

    rkw = wet_deposition_rate_imm(radius, q, depconst)

    deprate = 1.0 - exp(-tstep*rkw)
  end function

  subroutine wetdep_conventional_init()
    use snapdimML, only: nx, ny
    allocate(conventional_deprate_m1(nx,ny))
  end subroutine

  subroutine wetdep_conventional_deinit()
    deallocate(conventional_deprate_m1)
  end subroutine

  subroutine wetdep_conventional_compute(precip)
    real, intent(in) :: precip(:,:)

    conventional_deprate_m1(:,:) = conventional_params%A * (precip ** conventional_params%B)
  end subroutine

  !> TODO: Expand to 3D
  subroutine wetdep_conventional(depwet, part, tstep)
    USE iso_fortran_env, only: real64
    USE particleML, only: Particle, extraParticle
    USE snapparML, only: def_comp

!> Field which ret deposition gets added to
    real(real64), intent(inout) :: depwet(:, :, :)
!> particle
    type(Particle), intent(inout) :: part
    real, intent(in) :: tstep

    integer :: m, i, j, mm
    real :: dep, deprate

    m = part%icomp

    i = nint(part%x)
    j = nint(part%y)

    deprate = exp(-tstep * conventional_deprate_m1(i, j))
    dep = part%scale_rad(deprate)

    mm = def_comp(m)%to_running

    !$OMP atomic
    depwet(i, j, mm) = depwet(i, j, mm) + dble(dep)
  end subroutine

  subroutine wetdep_takemura()
    real :: lambda

    !> raindrop radius, in µm
    real, parameter :: r_r = 500
    !> raindrop terminal velocity in m/s
    real, parameter :: v_r = 0.5
    !> Collection efficienty
    real, parameter :: E = 2.05e-4
    real, parameter :: pi = 4.0 * atan(1.0)

    !> Size of aerosol, input
    real, parameter :: r_aer = 500
    !> Density of aerosol, input
    real, parameter :: rho_aer = 1.0

    !> Number density of aerosols
    real, parameter :: N_aer = 1.0
    !> Number density of cloud droplets, input
    real, parameter :: N_r = 1e-6
    !> Air viscosity in kg/(ms)
    real, parameter :: mu = 1.78e-5
    real, parameter :: g = 9.81

    real :: v_aer

    v_aer = 2.0/9.0 * rho_aer * (r_aer ** 2) * g / mu

    ! TODO: How to get number density of hygrometeors?
    ! TODO: Use distribution for raindrop radius?
    ! TODO: Set raindrop radius/velocity based on precip rate?
    ! TODO: Skip N_aer, we can work with the rate instead

    lambda = E * pi * (r_r + r_aer) ** 2 * (v_r - v_aer) * N_r * N_aer

  end subroutine

  !> Aerosol rainout process also known as GCM-type wet deposition process
  subroutine wetdep_incloud_gcm(lambda, q, cloud_water, cloud_fraction)
    real, intent(out) :: lambda(:,:)
    !> vertical flux of hydrometeors
    real, intent(in) :: q(:,:)
    !> Fraction of aerosol mass in cloud water to total aerosol mass in the grid
    !> or the absorbtion coefficient
    !> Usually very high
    real, parameter :: f_inc = 1.0
    !> cloud water
    real, intent(in) :: cloud_water(:,:)
    !> Cloud fraction
    real, intent(in) :: cloud_fraction(:,:)


    lambda(:,:) = q/(q + cloud_water) * f_inc * cloud_fraction / 3600.0
  end subroutine

  !> Aerosol rainout process from Roselle and Binkowski 1999
  subroutine wetdep_incloud_roselle(lambda, q, cloud_water, cloud_fraction)
    real, intent(out) :: lambda(:,:)
    !> vertical flux of hydrometeors
    real, intent(in) :: q(:,:)
    !> Fraction of aerosol mass in cloud water to total aerosol mass in the grid
    !> or the absorbtion coefficient
    !> Usually very high
    real, parameter :: f_inc = 1.0
    !> cloud water
    real, intent(in) :: cloud_water(:,:)
    !> Cloud fraction
    real, intent(in) :: cloud_fraction(:,:)

    !> Characteristic time of cloud (equal to timestep?)
    real, parameter :: tau_cloud = 1.0

    real, allocatable :: tau_washout(:,:)

    allocate(tau_washout, mold=lambda)

    where (q > 0.0)
      tau_washout = cloud_water / q
      lambda = (1.0 - exp(-tau_cloud / tau_washout)) / tau_cloud * f_inc * cloud_fraction / 3600.0
    elsewhere
      lambda = 0.0
    endwhere
  end subroutine

  subroutine wet_deposition_rate_bartnicki(wscav, radius, precip, cw, ccf, use_ccf)
    real, intent(out) :: wscav(:,:,:)
    real, intent(in) :: precip(:,:,:)
    real, intent(in) :: cw(:,:,:)
    real, intent(in) :: radius
    real, intent(in) :: ccf(:,:,:)
    logical, intent(in) :: use_ccf

    real :: depconst
    integer :: nx, ny, nk

      nx = size(precip,1)
      ny = size(precip,2)
      nk = size(precip,3)

    depconst = wet_deposition_constant(radius)
    if (.not.use_ccf) then
      wscav(:,:,:) = wet_deposition_rate_imm(radius, precip, depconst)
    else
      block
        integer :: i, j, k
        real, allocatable :: precip_scaled(:,:)
        real, allocatable :: ccf_max(:,:)


        allocate(precip_scaled(nx,ny), ccf_max(nx,ny))

        ccf_max(:,:) = 0.0

        do k=nk,1,-1
          precip_scaled(:,:) = 0.0
          ccf_max(:,:) = max(ccf_max, ccf(:,:,k))

          do j=1,ny
            do i=1,nx
              if (precip(i,j,k) <= 0.0) cycle

              if (ccf_max(i,j) > 0.0) then
                ! Scales up the precipitation intensity
                precip_scaled(i,j) = precip(i,j,k) / ccf_max(i,j)
              else
                precip_scaled(i,j) = precip(i,j,k)
              endif
            enddo
          enddo
          wscav(:,:,k) = wet_deposition_rate_imm(radius, precip_scaled, depconst)

          do j=1,ny
            do i=1,nx
              if (ccf_max(i,j) > 0.0) then
                ! Only applies to the portion in the cloud
                wscav(i,j,k) = wscav(i,j,k) * ccf_max(i,j)
              endif
            enddo
          enddo
        enddo
      end block
    endif

    block
      integer :: k
      real, allocatable :: incloud_coeff(:,:)

      allocate(incloud_coeff(nx,ny))

      do k=nk,1,-1
        select case(wetdep_incloud_scheme%scheme)
          case (WETDEP_INCLOUD_SCHEME_NONE%scheme)
            incloud_coeff(:,:) = 0.0
          case (WETDEP_INCLOUD_SCHEME_GCM%scheme)
            call wetdep_incloud_gcm(incloud_coeff, precip(:,:,k), cw(:,:,k), ccf(:,:,k))
          case (WETDEP_INCLOUD_SCHEME_ROSELLE%scheme)
            call wetdep_incloud_roselle(incloud_coeff, precip(:,:,k), cw(:,:,k), ccf(:,:,k))
          case default
            error stop "Unknown scheme"
        end select
        ! call incloud_scavenging(incloud_coeff, precip(:,:,k), cw(:,:,k), ccf(:,:,k))
        wscav(:,:,k) = max(wscav(:,:,k), incloud_coeff)
      enddo
    end block
  end subroutine

!  elemental subroutine incloud_scavenging(scav, precip, cw, cc)
!    USE IEEE_ARITHMETIC, only: IEEE_VALUE, IEEE_POSITIVE_INF
!    real, intent(out) :: scav
!    real, intent(in) :: precip
!    real, intent(in) :: cw
!    real, intent(in) :: cc
!
!    real :: drainage_time
!    real, parameter :: MIN_DRAINAGE_TIME = 300
!
!    if (precip == 0.0) then
!      drainage_time = IEEE_VALUE(drainage_time, IEEE_POSITIVE_INF)
!    else
!      drainage_time = cw / precip
!    endif
!
!    drainage_time = max(drainage_time, MIN_DRAINAGE_TIME)
!
!    scav = 1.0 / drainage_time
!  end subroutine

  subroutine prepare_wetdep(wscav, radius, scheme, precip, cw, ccf, use_ccf)
    type(wetdep_scheme_t), intent(in) :: scheme
    real, intent(out) :: wscav(:,:,:)
    real, intent(in) :: precip(:,:,:)
    real, intent(in) :: radius
    real, intent(in) :: cw(:,:,:)
    !> Cloud cover fraction
    real, intent(in) :: ccf(:,:,:)
    logical, intent(in) :: use_ccf

    if (scheme == WETDEP_SCHEME_BARTNICKI) then
      call wet_deposition_rate_bartnicki(wscav, radius, precip, cw, ccf, &
                use_ccf=use_ccf)
    else
      error stop "Not implemented"
    endif
  end subroutine

  subroutine wetdep_using_precomputed_wscav(part, wscav, dep, tstep)
    use iso_fortran_env, only: real64
    use particleml, only: particle
    use snapparML, only: def_comp
    use snapgrdML, only: ivlevel
    type(particle), intent(inout) :: part
    real, intent(in) :: wscav(:,:,:,:)
    real(real64), intent(inout) :: dep(:,:,:)
    real, intent(in) :: tstep

    real :: radlost, rkw
    integer :: ivlvl, i, j, k, mm

    ivlvl = part%z * 10000.0
    k = ivlevel(ivlvl)
    i = int(part%x)
    j = int(part%y)

    mm = def_comp(part%icomp)%to_running

    rkw = wscav(i,j,k,mm)

    radlost = part%scale_rad(exp(-rkw*tstep))

    !$OMP atomic
    dep(i, j, mm) = dep(i, j, mm) + real(radlost, kind=real64)
  end subroutine

end module wetdep
