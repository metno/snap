!> Wet deposition of radionuclides. Method: Combination of J.Bartnicki 2003 and Takemura et al. 2000.
!> This module considers two different regimes where wet deposition can be calculated: incloud (in model levels) and subcloud (at surface level).
! [TODO: Discuss more]

module wetdepmlclean
  use iso_fortran_env, only: real64
  implicit none
  private
  
  !> Minimum sigma value for [Bartnicki] scavenging (i.e., approxmiately maximum pressure altitude)
  real, save :: vminprec = -1.      
  !> minimum precipitation intensity (mm/hour)
  real, parameter :: precmin = 0.01

  public :: wetdep, init, requires_extra_precip_fields, &
      wetdep_precompute


  ! public :: operator(==), operator(/=)  ! [TODO]: Try different way of writing

  ! !> Define subcloud scheme types (at surface level)
  ! type, public :: wetdep_subcloud_scheme_t
  !   integer, private :: scheme
  !   character(len=32), public :: description
  ! end type

  ! type(wetdep_subcloud_scheme_t), parameter, public :: WETDEP_SUBCLOUD_SCHEME_UNDEFINED = &
  !     wetdep_subcloud_scheme_t(0, "Not defined")
  ! type(wetdep_subcloud_scheme_t), parameter, public :: WETDEP_SUBCLOUD_SCHEME_NONE = & 
  !     wetdep_subcloud_scheme_t(1, "No scheme (skip)")
  ! type(wetdep_subcloud_scheme_t), parameter, public :: WETDEP_SUBCLOUD_SCHEME_BARTNICKI = & 
  !     wetdep_subcloud_scheme_t(2, "Bartnicki")

  integer, parameter, public :: WETDEP_SUBCLOUD_SCHEME_UNDEFINED = 0 
  integer, parameter, public :: WETDEP_SUBCLOUD_SCHEME_NONE = 1 
  integer, parameter, public :: WETDEP_SUBCLOUD_SCHEME_BARTNICKI = 2
  
  

  ! !> Define incloud scheme types (in model levels)
  ! type, public :: wetdep_incloud_scheme_t
  !   integer, private :: scheme
  !   character(len=32), public :: description
  ! end type

  ! type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_UNDEFINED = &
  !     wetdep_incloud_scheme_t(0, "Not defined")
  ! type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_NONE = &
  !     wetdep_incloud_scheme_t(1, "No scheme (skip)")
  ! type(wetdep_incloud_scheme_t), parameter, public :: WETDEP_INCLOUD_SCHEME_TAKEMURA = &
  !     wetdep_incloud_scheme_t(2, "Takemura") 
      
  integer, parameter, public :: WETDEP_INCLOUD_SCHEME_UNDEFINED = 0 
  integer, parameter, public :: WETDEP_INCLOUD_SCHEME_NONE = 1 
  integer, parameter, public :: WETDEP_INCLOUD_SCHEME_TAKEMURA = 3

  !> Combine the two regimes into one scheme
  type, public :: wetdep_scheme_t
    integer :: subcloud
    integer :: incloud
    ! type(wetdep_subcloud_scheme_t) :: subcloud
    ! type(wetdep_incloud_scheme_t) :: incloud
    !> Use 3D precip and precomputed parameters
    logical :: use_vertical
    !> Whether to use cloud fraction correction for subcloud schemes
    logical :: use_cloudfraction
  end type

  !> As default, the scheme is undefined
  type(wetdep_scheme_t), save, public :: wetdep_scheme = &
      wetdep_scheme_t(WETDEP_SUBCLOUD_SCHEME_UNDEFINED, WETDEP_INCLOUD_SCHEME_UNDEFINED, .false., .false.)


  ! interface operator (==)                                    ![TODO]: Write differently
  !   module procedure :: equal_subcloud_scheme, equal_incloud_scheme
  ! end interface

  ! interface operator (/=)
  !   module procedure :: not_equal_subcloud_scheme, not_equal_incloud_scheme
  ! end interface


contains

  ! !> Functions for checking input scheme. Used to define operator for scheme type.
  ! logical pure function equal_subcloud_scheme(this, other) result(eq)  ![TODO] Change
  !   type(wetdep_subcloud_scheme_t), intent(in) :: this, other
  !   eq = this%scheme == other%scheme
  ! end function
  ! !> Functions for checking input scheme. Used to define operator for scheme type.
  ! logical pure function not_equal_subcloud_scheme(this, other) result(eq)
  !   type(wetdep_subcloud_scheme_t), intent(in) :: this, other
  !   eq = .not. (this == other)
  ! end function
  ! !> Functions for checking input scheme. Used to define operator for scheme type.
  ! logical pure function equal_incloud_scheme(this, other) result(eq)
  !   type(wetdep_incloud_scheme_t), intent(in) :: this, other
  !   eq = this%scheme == other%scheme
  ! end function
  ! !> Functions for checking input scheme. Used to define operator for scheme type.
  ! logical pure function not_equal_incloud_scheme(this, other) result(eq)
  !   type(wetdep_incloud_scheme_t), intent(in) :: this, other
  !   eq = .not. (this == other)
  ! end function

  !> Initialise for surface bartnicki scheme
  subroutine init(tstep) ![TODO]: combine with wetdep2_init  only thing happening
    real, intent(in) :: tstep

    if (wetdep_scheme%subcloud == WETDEP_SUBCLOUD_SCHEME_BARTNICKI.and. &
      .not.wetdep_scheme%use_vertical) then

      call wetdep2_init(tstep)

    endif
  end subroutine

!> Initialisation routine for :: Bartnicki.takemura
  !> Output diagnostics
  subroutine wetdep2_init(tstep)

    USE snapdebug, only: iulog
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
      write (iulog, *) 'WETDEP2 m,r,depconst(m): ', m, rm, run_comp(m)%depconst  ![TODO]: get rid of if not important... or implement everywhere else too.
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
    
    if(vminprec < 0.) then                        !!Setting minimum sigma level (maximum pressure level)
      block
        USE snapgrdML, only: alevel, blevel, vlevel
        USE snapdimML, only: nk

        integer :: k
        real :: p1,p2
        real, parameter :: plim = 550.0

        p2 = 1000.
        p1 = p2
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
        vminprec = max(vminprec, 0.67)
      end block
    end if
    
  end subroutine

  !> Flag for data from model layers
  pure logical function requires_extra_precip_fields()
    requires_extra_precip_fields = wetdep_scheme%use_vertical
  end function

  !> Move activity from particle to wet deposition field depending on the precipitation at the place of the particle.
  subroutine wetdep(tstep, part, pextra)
    use iso_fortran_env, only: real64
    USE particleML, only: particle, extraParticle
    use snapparML, only: def_comp
    use snapfldml, only: depwet

    real, intent(in) :: tstep
    type(particle), intent(inout) :: part
    type(extraParticle), intent(in) :: pextra

    if (def_comp(part%icomp)%kwetdep == 1) then       ! If wet deposition on (for each component)
      if (wetdep_scheme%use_vertical) then            ! If using bartnicki-takemura scheme
        block
          use snapfldML, only: wscav
          call wetdep_using_precomputed_wscav(part, wscav, depwet, tstep)
        end block
      else if (wetdep_scheme%subcloud == WETDEP_SUBCLOUD_SCHEME_BARTNICKI) then  ! If using the bartnicki surface scheme
          call wetdep2(depwet, tstep, part, pextra)
      endif
    endif
  end subroutine

  
  !> Part of Bartnicki scheme for radii between 1.4 and 10 micrometres
  pure real function wet_deposition_constant(rm) result(depconst)
!> Radius in micrometer
    real, intent(in) :: rm

    real, parameter :: b0 = -0.1483
    real, parameter :: b1 = 0.3220133
    real, parameter :: b2 = -3.0062e-2
    real, parameter :: b3 = 9.34458e-4

    depconst = b0 + b1*rm + b2*rm*rm + b3*rm*rm*rm
  end function

!> Method:   J.Bartnicki 2003 for surface wet deposition
  subroutine wetdep2(depwet, tstep, part, pextra)
    USE iso_fortran_env, only: real64
    USE particleML, only: Particle, extraParticle
    USE snapparML, only: def_comp, run_comp
    USE snapdimML, only: hres_pos

!> Field which wet deposition gets added to
    real(real64), intent(inout) :: depwet(:, :, :)
!> Timestep of the simulation, affects the deposition rate
    real, intent(in) :: tstep
!> particle
    type(Particle), intent(inout) :: part
!> uses the precipitation at the particle position
    type(extraParticle), intent(in) :: pextra

    integer :: m, i, j, mm, mo
    real :: deprate, dep, q

    m = part%icomp

    !..assumens no precipitation if pressure less than approx. 650 hPa.
    !..and if less than a minimum precipitation intensity (mm/hour)
    if (pextra%prc > precmin &
        .AND. part%z > vminprec) then
      !..find particles with wet deposition and
      !..reset precipitation to zero if not wet deposition
      q = pextra%prc

      mm = def_comp(m)%to_running

      deprate = wet_deposition_rate(def_comp(m)%radiusmym, q, run_comp(mm)%depconst, tstep)

      dep = part%scale_rad(1.0 - deprate)

      i = hres_pos(part%x)
      j = hres_pos(part%y)
      mo = def_comp(m)%to_output
       ![TODO]: check standards on parallelisations
      !$OMP atomic                          
      depwet(i, j, mo) = depwet(i, j, mo) + dble(dep)
    end if
  end subroutine wetdep2


  pure elemental real function wet_deposition_rate(radius, q, depconst, tstep) result(deprate)
    !> radius in micrometer
    real, intent(in) :: radius
    !> precipitation intensity in mm/h
    real, intent(in) :: q
    !> deposition constant (precalculated)
    real, intent(in) :: depconst
    !> length of a timestep in seconds
    real, intent(in) :: tstep

    real :: rkw

    rkw = wet_subcloud_bartnicki(radius, q, depconst)

    deprate = 1.0 - exp(-tstep*rkw)
  end function



!> Bartnicki scheme without cloud fraction adjustments [1/s]
  pure elemental real function wet_subcloud_bartnicki(radius, q, depconst, use_convective) result(rkw)  ![TODO]: inputs of all uses should use opposite use_convective now
    !> radius in micrometer
    real, intent(in) :: radius
    !> precipitation intensity in mm/h
    real, intent(in) :: q
    !> deposition constant (precalculated)
    real, intent(in) :: depconst
    logical, optional, intent(in) :: use_convective

    real, parameter :: a0 = 8.4e-5
    real, parameter :: a1 = 2.7e-4
    real, parameter :: a2 = -3.618e-6
    logical :: convect

    if (present(use_convective)) then
      convect = use_convective
    else
      convect = .true.
    endif

    rkw = 0
    if (radius > 0.05 .AND. radius <= 1.4) then   ![TODO]: All these if statements do not need to be separate. Can use elif.
      rkw = a0*q**0.79
    else if (radius > 1.4 .AND. radius <= 10.0) then
      rkw = depconst*(a1*q + a2*q*q)
    else if (radius > 10.0) then
      rkw = a1*q + a2*q*q
    end if
    if (convect .and. q > 7.0) then ! convective   ![TODO]: Is this just for a specific radius? Or not? Should be first in if statement otherwise
      rkw = 3.36e-4*q**0.79
    endif
    if (radius <= 0.1) then ! gas               ![TODO]: Figure out how to fix overlap here  !!!Where is this from? Not in baklanov paper...
      rkw = 1.12e-4*q**0.79
    endif
  end function

  !> Scales the precip intensity according to cloud fraction for bartnicki scheme as wished
  subroutine wet_subcloud_bartnicki_ccf(wscav, radius, precip, ccf, use_ccf)
    !> Scavenging rate [1/s]
    real, intent(out) :: wscav(:,:)
    !> Precipitation intensity [mm/h]    !!![GEORGE]: Why is this accumulated eventually?
    real, intent(in) :: precip(:,:)
    real, intent(in) :: radius
    real, intent(in) :: ccf(:,:)
    logical, intent(in) :: use_ccf

    real :: depconst
    integer :: nx, ny

    nx = size(precip,1)
    ny = size(precip,2)

    depconst = wet_deposition_constant(radius)
    if (.not.use_ccf) then
      wscav(:,:) = wet_subcloud_bartnicki(radius, precip, depconst, use_convective=.False.) 
    else
      block
        integer :: i, j   !> GEORGE: is block neccessary? integers don't take much memory...
        real :: precip_scaled

        do j=1,ny
          do i=1,nx
            if (precip(i,j) <= 0.0) then
              wscav(i,j) = 0.0
              cycle
            endif

            if (ccf(i,j) > 0.0) then
              ! Scale up precip intensity
              precip_scaled = precip(i,j) / ccf(i,j)
            else
              precip_scaled = precip(i,j)               !> GEORGE: if ccf = 0 the surely no precipitation? Otherwise, could use mask to make ccf_{=0} = 1 for no for loops?
            endif

            wscav(i,j) = wet_subcloud_bartnicki(radius, precip_scaled, depconst, use_convective=.False.) !> Convective rain cannot be used here due to the precip scaling

            if (ccf(i,j) > 0.0) then
              ! Scale down efficiency
              wscav(i,j) = wscav(i,j) * ccf(i,j)
            endif
          enddo
        enddo
      end block
    endif
  end subroutine

      !> Aerosol rainout process also known as GCM-type wet deposition process
  subroutine wetdep_incloud_takemura(lambda, q, cloud_water, cloud_fraction)
    real, intent(out) :: lambda(:,:)
    !> vertical flux of hydrometeors [GEORGE: units?/translate??]
    real, intent(in) :: q(:,:)
    !> Fraction of aerosol mass in cloud water to total aerosol mass in the grid
    !> or the absorbtion coefficient
    !> Usually very high     !! [GEORGE]: Does this mean 1.0 is not physical?? - book suggests this should be lower
    real, parameter :: f_inc = 1.0
    !> cloud water
    real, intent(in) :: cloud_water(:,:)
    !> Cloud fraction
    real, intent(in) :: cloud_fraction(:,:)


    where (q > 0.0)
      lambda = q/(q + cloud_water) * f_inc * cloud_fraction / 3600.0  ! Converting to [1/s]
    elsewhere
      lambda = 0.0
    end where
  end subroutine

  !> Should be called every input timestep to prepare the scavenging rates
  subroutine wetdep_precompute()
    use snapparML, only: ncomp, run_comp
    use snapfldML, only: wscav, cw3d, precip3d, cloud_cover

    integer :: i
    if (wetdep_scheme%use_vertical) then   !skip precomputation if no vertical scheme
      do i=1,ncomp
        if (.not.run_comp(i)%defined%kwetdep == 1) cycle  ! skip precomputation if WET.DEP = off for specific component
      
        if (.not.(allocated(precip3d).and.allocated(cw3d).and.allocated(wscav))) then
          error stop "Some wetdep/precip fields not allocated"
        endif
        call prepare_wetdep_3d(wscav(:,:,:,i), run_comp(i)%defined%radiusmym, precip3d, cw3d, cloud_cover)
      enddo
    endif 
  end subroutine

    !> Precompute wet scavenging coefficients
  subroutine prepare_wetdep_3d(wscav, radius, precip, cw, ccf)
    !> Wet scavenging coefficient [1/s]
    real, intent(out) :: wscav(:,:,:)
    !> Precipitation intensity [mm/h]
    real, intent(in) :: precip(:,:,:)
    !> Radius of particle
    real, intent(in) :: radius
    !> Cloud water
    real, intent(in) :: cw(:,:,:)
    !> Cloud cover fraction
    real, intent(in) :: ccf(:,:,:)

    real, allocatable :: wscav_tmp(:,:,:)
    real, allocatable :: accum_precip(:,:), accum_ccf(:,:)

    integer :: nk, k, nx, ny

    nx = size(wscav,1)
    ny = size(wscav,2)
    nk = size(wscav,3)

    allocate(wscav_tmp, mold=wscav)
    allocate(accum_precip(nx,ny), accum_ccf(nx,ny))

    accum_precip(:,:) = 0.0
    accum_ccf(:,:) = 0.0
    do k=nk,1,-1
      ! Accumulated precipitation in the column
      accum_precip(:,:) = accum_precip(:,:) + precip(:,:,k)
      accum_ccf(:,:) = accum_ccf(:,:) + ccf(:,:,k)
      where (accum_ccf >= 1.0)
        accum_ccf = 1.0
      endwhere

      ! Subcloud
      ! select case (wetdep_scheme%subcloud%scheme)
      !   case (WETDEP_SUBCLOUD_SCHEME_BARTNICKI%scheme)
      !     call wet_subcloud_bartnicki_ccf(wscav(:,:,k), radius, accum_precip(:,:), &
      !      accum_ccf(:,:), use_ccf=wetdep_scheme%use_cloudfraction)
      !   case (WETDEP_SUBCLOUD_SCHEME_NONE%scheme)
      !     wscav(:,:,k) = 0.0
      !   case default
      !     error stop wetdep_scheme%subcloud%description
      ! end select

      ! ! Incloud
      ! select case (wetdep_scheme%incloud%scheme)
      !   case (WETDEP_INCLOUD_SCHEME_NONE%scheme)
      !     wscav_tmp(:,:,k) = 0.0
      !   case (WETDEP_INCLOUD_SCHEME_TAKEMURA%scheme)
      !     call wetdep_incloud_takemura(wscav_tmp(:,:,k), precip(:,:,k), cw(:,:,k), ccf(:,:,k))
      !   case default
      !     error stop wetdep_scheme%incloud%description
      ! end select

      select case (wetdep_scheme%subcloud)
        case (WETDEP_SUBCLOUD_SCHEME_BARTNICKI)
          call wet_subcloud_bartnicki_ccf(wscav(:,:,k), radius, accum_precip(:,:), &
           accum_ccf(:,:), use_ccf=wetdep_scheme%use_cloudfraction)
        case (WETDEP_SUBCLOUD_SCHEME_NONE)
          wscav(:,:,k) = 0.0
        case default
          error stop "Subcloud scheme undefined"
      end select

      ! Incloud
      select case (wetdep_scheme%incloud)
        case (WETDEP_INCLOUD_SCHEME_NONE)
          wscav_tmp(:,:,k) = 0.0
        case (WETDEP_INCLOUD_SCHEME_TAKEMURA)
          call wetdep_incloud_takemura(wscav_tmp(:,:,k), precip(:,:,k), cw(:,:,k), ccf(:,:,k))
        case default
          error stop "Incloud scheme undefined"
      end select
      wscav(:,:,k) = max(wscav(:,:,k), wscav_tmp(:,:,k))
    end do
  end subroutine

  subroutine wetdep_using_precomputed_wscav(part, wscav, dep, tstep)
    use iso_fortran_env, only: real64
    use particleml, only: particle
    use snapparML, only: def_comp
    use snapgrdML, only: ivlevel
    use snapdimML, only: hres_pos
    type(particle), intent(inout) :: part
    real, intent(in) :: wscav(:,:,:,:)
    real(real64), intent(inout) :: dep(:,:,:)
    real, intent(in) :: tstep

    real :: radlost, rkw
    integer :: ivlvl, i, j, k, mm, mo

    ivlvl = nint(part%z * 10000.0)
    k = ivlevel(ivlvl)
    i = nint(part%x)
    j = nint(part%y)

    mm = def_comp(part%icomp)%to_running

    rkw = wscav(i,j,k,mm)

    radlost = part%scale_rad(exp(-rkw*tstep))

    i = hres_pos(part%x)
    j = hres_pos(part%y)
    mo = def_comp(part%icomp)%to_output
    !$OMP atomic
    dep(i, j, mo) = dep(i, j, mo) + real(radlost, kind=real64)
  end subroutine

end module wetdepmlclean