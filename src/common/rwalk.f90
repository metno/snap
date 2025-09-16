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
  real(real64), save :: tfactor_v ! tfactor=tstep/tmix
  real(real64), save :: tsqrtfactor_v ! tsqrtfactor_v=1/sqrt(tmix/tstep)
  real(real64), save :: tfactor_h ! tfactor=tstep/thour
  real(real64), save :: tsqrtfactor_h ! tsqrtfactor_h=1/sqrt(thour/tstep)
  real(real64), save :: tstep

  real(real64), parameter :: hmax = 2500.0 ! maximum mixing height
  real(real64), parameter :: tmix_v = 15.0*60.0 ! Characteristic mixing time = 15 min (to reach full bl-height)
  real(real64), parameter :: tmix_h = 15.0*60.0 ! Horizontal base-time time = 15 min (to reach ax^b width)
  real(real64), parameter :: lmax = 0.28 ! Maximum l-eta in the mixing layer
  real(real64), parameter :: labove = 0.03 ! Standard l-eta above the mixing layer
  real(real64), parameter :: entrainment = 0.1 ! Entrainment zone = 10%*h

  real(real64), save, public :: a_in_bl = 0.5
  real(real64), save, public :: a_above_bl = 0.25
  real(real64), save, public :: b = 0.875
  character(len=64), save, public :: diffusion_method = ''

  public rwalk, rwalk_init, flexpart_diffusion, diffusion_fields, air_density

  contains

!> Initialise constants needed for rwalk
subroutine rwalk_init(timestep)
!> time step in seconds (trajectory calculations)
  real, intent(in) :: timestep

  tfactor_v = timestep/tmix_v
  tsqrtfactor_v=sqrt(tfactor_v)
  tfactor_h = timestep/tmix_h
  tsqrtfactor_h=sqrt(tfactor_h)
  tstep = timestep

  ! l-eta above mixing height
  vrdbla = labove*tsqrtfactor_v
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
  real(real64) :: rv, top_entrainment, bl_entrainment_thickness

  real(real64) :: a

! the random_number function returns 3 (x,y,z) random real numbers between 0.0 and 1.0
  call random_number(rnd)
  rnd = rnd - 0.5

! horizontal diffusion
  if (part%z > part%tbl) then ! in boundary layer
    a = a_in_bl
  else ! above boundary layer
    a = a_above_bl
  endif

  vabs = hypot(pextra%u, pextra%v)
  rl = 2*a*((vabs*tmix_h)**b) * tsqrtfactor_h ! sqrt error/sigma propagation
  ! part%x = part%x + rl*rnd(1)*pextra%rmx
  ! part%y = part%y + rl*rnd(2)*pextra%rmy


! vertical diffusion
  if (part%z <= part%tbl) then ! Above boundary layer
      part%z = part%z + vrdbla*rnd(3)
  else ! In boundary layer
    bl_entrainment_thickness = (1.0 - part%tbl)*(1.+entrainment)
    if (blfullmix .or. (tsqrtfactor_v .gt. 1.0)) then ! full mixing
      part%z = 1.0 - bl_entrainment_thickness*(rnd(3)+0.5)
    else ! vertical mixing splittet in smaller time-steps      
      rv  = (1-part%tbl)*tsqrtfactor_v

      part%z = part%z + rv*rnd(3)

    !... reflection from the ABL top
    !... but allow for entrainment
      ! top_entrainment 10% higher than tbl
      top_entrainment = max(0., 1.0 - bl_entrainment_thickness)
      if(part%z < top_entrainment) then
        part%z = 2.0*part%tbl - part%z
      endif

    !... reflection from the bottom
      if(part%z > 1.0) then
        part%z = 2.0 - part%z
      endif

    !..vertical limits
      part%z = min(part%z, 1.0d0)
      part%z = max(part%z, real(top_entrainment, kind=kind(part%z)))
    end if
  end if
end subroutine rwalk

subroutine flexpart_diffusion(part,pextra)
  USE particleML, only: extraParticle, Particle
  use init_random_seedML, only: generate_normal_randoms
  USE snapfldML, only: hlevel2, hbl2, ps2, hlayer2
  USE snapgrdML, only: ivlayer, vlevel, alevel, blevel
  use snapdimML, only: nx, ny, nk

  !> particle with information
  type(Particle), intent(inout)  :: part
  !> extra information regarding the particle (u, v, rmx, rmy)
  type(extraParticle), intent(inout) :: pextra
  
  integer :: ivlvl
  integer, parameter :: max_rands=4
  integer :: nrand
  integer :: i, j, k
  integer :: above_index, below_index
  real :: rands(max_rands)
  real :: above_layer, below_layer
  real :: pressure_above, pressure_below
  real :: weight
  real :: particle_pressure
  real :: height_k
  real :: height_init

  call generate_normal_randoms(rands, max_rands)

  nrand=1

  ! First time step, set to zero
  ! convert particle height to metres
  if (part%z.lt.1e-10) then 
    ivlvl = 10000 ! Particle isp at the surface
    k = ivlayer(ivlvl) 
  else
    ivlvl = part%z*10000.
    k = ivlayer(ivlvl) ! Layer below particle
  endif

  ! Find interpolation weight
  weight = (part%z - vlevel(k)) / (vlevel(k+1) - vlevel(k))
  
  ! Get particle position
  i = part%x
  j = part%y

  below_layer = hlevel2(i, j, k) ! Height level below particle
  above_layer = hlevel2(i, j, k+1) ! Height level above particle

  pextra%zmetres = below_layer + weight * (above_layer - below_layer)

  ! convert tbl height to metres
  ivlvl = part%tbl*10000.
  k = ivlayer(ivlvl) ! Layer below blh

  ! Find interpolation weight
  weight = (part%tbl - vlevel(k)) / (vlevel(k+1) - vlevel(k))

  below_layer = hlevel2(i, j, k) ! Height level below particle
  above_layer = hlevel2(i, j, k+1) ! Height level above particle

  pextra%tblmetres = below_layer + weight * (above_layer - below_layer)

  !write(*,*) 'start diffusion call, position: ', part%x, part%y, part%z, pextra%zmetres
  height_init = part%z

  ! Check if particle within abl
  if (part%z.ge.part%tbl) then
    !write(*,*) 'In BL', part%tbl, pextra%tblmetres
    call diffusion_within_abl(part,pextra,nrand,max_rands,rands)

    ! Get new particle position 
    i = part%x
    j = part%y

    above_index = nk
    do k = 2, nk
      height_k = hlevel2(i, j, k)
      if (pextra%zmetres < height_k) then
          above_index = k
          exit  
      end if
    end do

    below_index = above_index - 1

    pressure_below = alevel(below_index) + blevel(below_index) * ps2(i,j)
    pressure_above = alevel(above_index) + blevel(above_index) * ps2(i,j)

    weight = (pextra%zmetres - hlevel2(i, j, below_index)) /  &
    (hlevel2(i, j, above_index) - hlevel2(i, j, below_index))

    particle_pressure = pressure_below + weight * (pressure_above - pressure_below)

    part%z = particle_pressure / ps2(i, j)

    part%z = min(part%z, 1.0d0) ! set minimum height

  else
    !write(*,*) 'Above BL', part%tbl, pextra%tblmetres
    call diffusion_above_abl(part, pextra, nrand, max_rands, rands)
  endif

  !write(*,*) 'end diffusion call, position: ', part%x, part%y, part%z, pextra%zmetres

  ! open(unit=11, file="/lustre/storeB/users/chbou7748/ETEX_diffusion/model_runs/diffusion_schemes/output_files/ & 
  ! turb_time_series_flex.dat", status="unknown", position="append")

  ! write(11,'(3F12.6)') pextra%turbvelu* tstep*pextra%rmx, pextra%turbvelv* tstep*pextra%rmy, part%z - height_init

  ! close(11)

end subroutine flexpart_diffusion

subroutine diffusion_within_abl(part, pextra, nrand, max_rands, rands)
  USE particleML, only: extraParticle, Particle
  use snapfldML, only: u_star, w_star, obukhov_l, rho, rhograd
  USE snapgrdML, only: ivlayer
  
  !> particle with information
  type(Particle), intent(inout)  :: part
  !> extra information regarding the particle
  type(extraParticle), intent(inout) :: pextra
  ! Random numbers
  integer, intent(inout) :: nrand
  integer, intent(in) :: max_rands
  real, intent(in) :: rands(:)


  integer :: i, j, k
  real :: sigu, sigv, sigw ! Turbulent velocity standard deviations
  real :: tlu, tlv, tlw ! Lagrangian timescales
  real :: ru, rv, rw ! Lagrangian timescales
  real :: delz ! Turbulent vertical displacement (m)
  real :: dsigw2dz 
  real :: dttlw
  real :: rhoaux ! Density correction factor
  real :: s1, s2
  integer :: part_vert_index
  real :: scaled_height

  ! Dimensionless height 
  scaled_height = pextra%zmetres/pextra%tblmetres

  ! Get particle position
  i = part%x
  j = part%y
  part_vert_index = part%z*10000
  k = ivlayer(part_vert_index) ! Vertical layer of particle
  k = max(k, 2) ! temp fix to avoid infinity in rhograd
  
  ! Case 1, Neutral Conditions
  if (pextra%tblmetres/ABS(obukhov_l(i,j)).lt.1.) then
    u_star = max(1.e-4, u_star)

    ! Eq. 7.25 Hanna 1982: sigu/ust=2.0*exp(-3*f*z/ust),
    ! where f, the Coriolis parameter, is set to 1e-4
    ! Standard deviations of turbulent velocity fluctuations
    sigu = 2.0 * u_star(i,j) * EXP(-3.e-4*pextra%zmetres/u_star(i,j))
    sigu = MAX(sigu, 1.e-5)


    ! Eq. 7.26 Hanna 1982: sigv/ust=sigw/ust=1.3*exp(-2*f*z/ust),
    ! where f, the Coriolis parameter, is set to 1e-4
    sigv = 1.3 * u_star(i,j) * EXP(-2.e-4*pextra%zmetres/u_star(i,j))
    sigv=max(sigv,1.e-5)
    sigw=sigv

    ! Vertical gradient of sigw
    dsigw2dz=-6.76e-4*u_star(i,j)*exp(-4.e-4*pextra%zmetres/u_star(i,j))

    ! Lagrangian timescales
    tlu=0.5*pextra%zmetres/sigw/(1.+1.5e-3*pextra%zmetres/u_star(i,j))
    tlv=tlu
    tlw=tlu
  
  ! Case 2 , Unstable Conditions
  elseif (obukhov_l(i,j).lt.0.) then
    ! Eq. 4.15 Caughey 1982
    sigu=u_star(i,j)*(12.-0.5*pextra%tblmetres/obukhov_l(i,j))**0.33333
    sigu=MAX(sigu,1.e-6)
    sigv=sigu

    ! Eq. 7.15 Hanna 1982
    if (scaled_height.lt.0.03) then
      sigw=0.96*w_star(i,j)*(3*scaled_height-obukhov_l(i,j)/pextra%tblmetres)**0.33333
      dsigw2dz=1.8432*w_star(i,j)*w_star(i,j)/pextra%tblmetres*(3*scaled_height-obukhov_l(i,j)/pextra%tblmetres)**(-0.33333)
    else if (scaled_height.lt.0.4) then
      s1=0.96*(3*scaled_height-obukhov_l(i,j)/pextra%tblmetres)**0.33333
      s2=0.763*scaled_height**0.175
      if (s1.lt.s2) then
        sigw=w_star(i,j)*s1
        dsigw2dz=1.8432*w_star(i,j)*w_star(i,j)/pextra%tblmetres*(3*scaled_height-obukhov_l(i,j)/pextra%tblmetres)**(-0.33333)
      else
        sigw=w_star(i,j)*s2
        dsigw2dz=0.203759*w_star(i,j)*w_star(i,j)/pextra%tblmetres*scaled_height**(-0.65)
      endif
    else if (scaled_height.lt.0.96) then
      sigw=0.722*w_star(i,j)*(1-scaled_height)**0.207
      dsigw2dz=-.215812*w_star(i,j)*w_star(i,j)/pextra%tblmetres*(1-scaled_height)**(-0.586)
    else if (scaled_height.lt.1.00) then
      sigw=0.37*w_star(i,j)
      dsigw2dz=0.
    endif
    sigw=max(sigw,1.e-6) 


    ! Determine average Lagrangian time scale
    ! Eq. 7.17 Hanna  1982
    tlu=0.15*pextra%tblmetres/sigu
    tlv=tlu
    if (pextra%zmetres.lt.ABS(obukhov_l(i,j))) then
      tlw=0.1*pextra%zmetres/(sigw*(0.55-0.38*ABS(pextra%zmetres/obukhov_l(i,j))))
    else if (scaled_height.lt.0.1) then
      tlw=0.59*pextra%zmetres/sigw
    else
      tlw=0.15*pextra%tblmetres/sigw*(1.-EXP(-5*scaled_height))
    endif


  ! Case 3, Stable Conditions 
  
  else
    ! Standard deviations of turbulent velocity fluctuations
    sigu=2.*u_star(i,j)*(1.-scaled_height) !. 7.20, Hanna
    sigv=1.3*u_star(i,j)*(1.-scaled_height) !. 7.19, Hanna
    sigu=max(sigu,1.e-6)
    sigv=max(sigv,1.e-6)
    sigw=sigv !. 7.19, Hanna
    dsigw2dz=3.38*u_star(i,j)*u_star(i,j)*(scaled_height-1.)/pextra%tblmetres

    ! Lagrangian timescales
    tlu=0.15*pextra%tblmetres/sigu*(sqrt(scaled_height))
    tlv=0.467*tlu
    tlw=0.1*pextra%tblmetres/sigw*scaled_height**0.8
  
  endif

  ! Clamp lagrangian timescales
  tlu=max(10.,tlu)
  tlv=max(10.,tlv)
  tlw=max(30.,tlw)

  !write(*,*) 'Atmospheric Params: ', pextra%zmetres, pextra%tblmetres, w_star(i,j), obukhov_l(i, j)

  ! Calculate turbulent horizontal velocities
  if (nrand+1.gt.max_rands) nrand=1
  if (tstep/tlu.lt..5) then
    pextra%turbvelu=(1.-tstep/tlu)*pextra%turbvelu+rands(nrand)*sigu*sqrt(2.*tstep/tlu)
  else
    ru=exp(-tstep/tlu)
    pextra%turbvelu=ru*pextra%turbvelu+rands(nrand)*sigu*sqrt(1.-ru**2)
  endif
  if (tstep/tlv.lt..5) then
    pextra%turbvelv=(1.-tstep/tlv)*pextra%turbvelv+rands(nrand+1)*sigv*sqrt(2.*tstep/tlv)
  else
    rv=exp(-tstep/tlv)
    pextra%turbvelv=rv*pextra%turbvelv+rands(nrand+1)*sigv*sqrt(1.-rv**2)
  endif
  nrand=nrand+2

  ! Calculate new horizontal positions. Maybe should only update at the end?
  part%x = part%x + pextra%turbvelu * tstep*pextra%rmx
  part%y = part%y + pextra%turbvelv * tstep*pextra%rmy

  ! Factor for density correction
  rhoaux=rhograd(i,j,k)/rho(i,j,k)

  ! ratio of time step to lagrangian timescale for autocorrelation
  dttlw = tstep/tlw

  ! Calculate turbulent vertical velocity
  rw=exp(-dttlw)
  pextra%turbvelw=(rw*pextra%turbvelw+rands(nrand)*sqrt(1.-rw**2)*sigw &
        +tlw*(1.-rw)*(dsigw2dz+rhoaux*sigw**2)) * pextra%icbt
  delz=pextra%turbvelw*tstep 

  !write(*,*) 'Vertical Turbulence: ', rw, tlw, sigw, dsigw2dz, rhoaux, rands(nrand), pextra%turbvelw, delz

  ! Calculate new vertical position
  if (abs(delz).gt.pextra%tblmetres) delz=mod(delz,pextra%tblmetres)

  if (delz.lt.-pextra%zmetres) then         ! reflection at ground
    pextra%zmetres = -pextra%zmetres - delz
    pextra%icbt = -1
  else if (delz.gt.(pextra%tblmetres-pextra%zmetres)) then ! reflection at top
    pextra%zmetres = -pextra%zmetres-delz+2.*pextra%tblmetres
    pextra%icbt = -1
  else                         ! no reflection
    pextra%zmetres = pextra%zmetres+delz
    pextra%icbt = 1
  endif

  !write(*,*) 'Turbulent Displacements: ', pextra%turbvelu * tstep*pextra%rmx, pextra%turbvelv * tstep*pextra%rmy, delz

end subroutine diffusion_within_abl

subroutine diffusion_above_abl(part, pextra, nrand, max_rands, rands)

  USE particleML, only: extraParticle, Particle

  !> particle with information
  type(Particle), intent(inout)  :: part
  !> extra information regarding the particle 
  type(extraParticle), intent(inout) :: pextra

  ! Random numbers
  integer, intent(inout) :: nrand
  integer, intent(in) :: max_rands
  real, intent(in) :: rands(:)

  ! turbulence factors for the troposphere
  real :: d_trop=50.
  
  real :: uxscale

  ! assume within troposphere
  uxscale=sqrt(2.*d_trop/tstep)
  if (nrand+1.gt.max_rands) nrand=1
  pextra%turbvelu=rands(nrand)*uxscale
  pextra%turbvelv=rands(nrand+1)*uxscale
  nrand=nrand+2
  pextra%turbvelw=0

  part%x = part%x + pextra%turbvelu * tstep*pextra%rmx
  part%y = part%y + pextra%turbvelv * tstep*pextra%rmy

end subroutine diffusion_above_abl

subroutine diffusion_fields(u_star, w_star, obukhov_l)
  use snapfldML, only: ps2, t2m, t2_dew, ishf, xsurfstress, ysurfstress, hbl2
  use snapdimML, only: nx, ny

  real, intent(out) :: u_star(:, :)
  real, intent(out) :: w_star(:, :)
  real, intent(out) :: obukhov_l(:, :)

  real, parameter :: r=287, g=9.81, k=0.4, cpa=1004.6

  real, allocatable :: stress(:, :)
  real, allocatable :: rho_a(:, :)
  real, allocatable :: tv(:, :)
  real, allocatable :: vp(:, :)
  real, allocatable :: w(:, :)

  allocate(stress(nx, ny))
  allocate(rho_a(nx, ny))
  allocate(tv(nx, ny))
  allocate(vp(nx, ny))
  allocate(w(nx, ny))

  ! Tetens Equation, vp is the vapour pressure in Pa
  vp = 0.61078 * EXP(17.27 * (t2_dew - 273.15) / (t2_dew - 35.85)) * 1000

  ! Mixing ratio, convert ps2 to Pa from hPa
  w = (0.622 * vp) / ((ps2*100) - vp)

  ! Calculate virtual potential temeperature
  tv = (t2m * (100000/(ps2*100))**(r/cpa))  * (1 + 0.608 * w)

  ! Calculate air density
  rho_a = (ps2*100)/(r*tv)

  stress = HYPOT(xsurfstress, ysurfstress)

  ! Calculate friction velocity
  u_star = sqrt(stress/rho_a)
  
  ! Calculate the obukhov length
  obukhov_l = - rho_a * cpa * t2m * (u_star**3)/(k*g*ishf)

  ! Calculate the convective velocity scale, p.622/118 stull
  w_star = ((g*hbl2*ishf)/(tv*cpa*rho_a))**0.333
  
end subroutine

subroutine air_density(rho, rhograd)
  use snapfldML, only: spec_humid, t2_abs, ps2, hlevel2
  use snapdimML, only: nx, ny, nk
  use snapgrdML, only: alevel, blevel, ahalf, bhalf

  real, parameter :: r=287

  real, intent(out) :: rho(:, :, :)
  real, intent(out) :: rhograd(:, :, :)

  real, allocatable :: tv(:, :, :)
  real, allocatable :: pressures(:, :, :)

  integer :: i, j, k

  allocate(tv(nx, ny, nk))
  allocate(pressures(nx, ny, nk))

  tv = t2_abs * (0.608 * spec_humid + 1)

  do i=1, nx
    do j=1, ny
      do k=1, nk
        pressures(i, j, k) = alevel(k) + blevel(k) * ps2(i, j)*100 ! convert from hPa to Pa
      end do
    end do
  end do

  rho = pressures / (r * tv)
  ! Set first layer to zero
  rho(:, :, 1) = 0.0

  ! interior points
  do k = 3, nk-1
    do j = 1, ny
      do i = 1, nx
        rhograd(i,j,k) = (rho(i,j,k+1) - rho(i,j,k-1)) / (hlevel2(i,j,k+1) - hlevel2(i,j,k-1))
      end do
    end do
  end do

  
  do j = 1, ny
    do i = 1, nx
      ! bottom boundary (forward difference)
      rhograd(i,j,2) = (rho(i,j,3) - rho(i,j,2)) / (hlevel2(i,j,3) - hlevel2(i,j,2))

      ! top boundary (backward difference)
      rhograd(i,j,nk) = (rho(i,j,nk) - rho(i,j,nk-1)) / (hlevel2(i,j,nk) - hlevel2(i,j,nk-1))
    end do
  end do

end subroutine


subroutine diffusion_within_abl_horizontal(part, pextra, nrand, max_rands, rands)

  USE particleML, only: extraParticle, Particle
  use snapfldML, only: u_star, w_star, obukhov_l, rho, rhograd
  USE snapgrdML, only: ivlayer
  
  !> particle with information
  type(Particle), intent(inout)  :: part
  !> extra information regarding the particle
  type(extraParticle), intent(inout) :: pextra
  ! Random numbers
  integer, intent(inout) :: nrand
  integer, intent(in) :: max_rands
  real, intent(in) :: rands(:)


  integer :: i, j, k
  real :: sigu, sigv, sigw ! Turbulent velocity standard deviations
  real :: tlu, tlv ! Lagrangian timescales
  real :: ru, rv ! Lagrangian timescales
  integer :: part_vert_index
  real :: scaled_height

  ! Dimensionless height 
  scaled_height = pextra%zmetres/pextra%tblmetres

  ! Get particle position
  i = part%x
  j = part%y
  part_vert_index = part%z*10000
  k = ivlayer(part_vert_index) ! Vertical layer of particle
  k = max(k, 2) ! temp fix to avoid infinity in rhograd
  
  ! Case 1, Neutral Conditions
  if (pextra%tblmetres/ABS(obukhov_l(i,j)).lt.1.) then
    u_star = max(1.e-4, u_star)

    ! Eq. 7.25 Hanna 1982: sigu/ust=2.0*exp(-3*f*z/ust),
    ! where f, the Coriolis parameter, is set to 1e-4
    ! Standard deviations of turbulent velocity fluctuations
    sigu = 2.0 * u_star(i,j) * EXP(-3.e-4*pextra%zmetres/u_star(i,j))
    sigu = MAX(sigu, 1.e-5)


    ! Eq. 7.26 Hanna 1982: sigv/ust=sigw/ust=1.3*exp(-2*f*z/ust),
    ! where f, the Coriolis parameter, is set to 1e-4
    sigv = 1.3 * u_star(i,j) * EXP(-2.e-4*pextra%zmetres/u_star(i,j))
    sigv=max(sigv,1.e-5)
    sigw=sigv

    ! Lagrangian timescales
    tlu=0.5*pextra%zmetres/sigw/(1.+1.5e-3*pextra%zmetres/u_star(i,j))
    tlv=tlu
  
  ! Case 2 , Unstable Conditions
  elseif (obukhov_l(i,j).lt.0.) then
    ! Eq. 4.15 Caughey 1982
    sigu=u_star(i,j)*(12.-0.5*pextra%tblmetres/obukhov_l(i,j))**0.33333
    sigu=MAX(sigu,1.e-6)
    sigv=sigu


    ! Determine average Lagrangian time scale
    ! Eq. 7.17 Hanna  1982
    tlu=0.15*pextra%tblmetres/sigu
    tlv=tlu

  ! Case 3, Stable Conditions 
  
  else
    ! Standard deviations of turbulent velocity fluctuations
    sigu=2.*u_star(i,j)*(1.-scaled_height) !. 7.20, Hanna
    sigv=1.3*u_star(i,j)*(1.-scaled_height) !. 7.19, Hanna
    sigu=max(sigu,1.e-6)
    sigv=max(sigv,1.e-6)

    ! Lagrangian timescales
    tlu=0.15*pextra%tblmetres/sigu*(sqrt(scaled_height))
    tlv=0.467*tlu

  endif

  ! Clamp lagrangian timescales
  tlu=max(10.,tlu)
  tlv=max(10.,tlv)

  !write(*,*) 'Atmospheric Params: ', pextra%zmetres, pextra%tblmetres, w_star(i,j), obukhov_l(i, j)

  ! Calculate turbulent horizontal velocities
  if (nrand+1.gt.max_rands) nrand=1
  if (tstep/tlu.lt..5) then
    pextra%turbvelu=(1.-tstep/tlu)*pextra%turbvelu+rands(nrand)*sigu*sqrt(2.*tstep/tlu)
  else
    ru=exp(-tstep/tlu)
    pextra%turbvelu=ru*pextra%turbvelu+rands(nrand)*sigu*sqrt(1.-ru**2)
  endif
  if (tstep/tlv.lt..5) then
    pextra%turbvelv=(1.-tstep/tlv)*pextra%turbvelv+rands(nrand+1)*sigv*sqrt(2.*tstep/tlv)
  else
    rv=exp(-tstep/tlv)
    pextra%turbvelv=rv*pextra%turbvelv+rands(nrand+1)*sigv*sqrt(1.-rv**2)
  endif
  nrand=nrand+2

  ! open(unit=11, file="/lustre/storeB/users/chbou7748/ETEX_diffusion/model_runs/diffusion_schemes/output_files/ & 
  ! turb_time_series_flex.dat", status="unknown", position="append")

  ! write(11,'(2F12.6)') pextra%turbvelu* tstep*pextra%rmx, pextra%turbvelv* tstep*pextra%rmy

  ! close(11)

  ! Calculate new horizontal positions. Maybe should only update at the end?
  part%x = part%x + pextra%turbvelu * tstep*pextra%rmx
  part%y = part%y + pextra%turbvelv * tstep*pextra%rmy

end subroutine diffusion_within_abl_horizontal


end module rwalkML
