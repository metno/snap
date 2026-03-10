!> Wet deposition of radionuclides.
!> Method: Combination of J.Bartnicki 2003 and Takemura et al. 2000.
!> This module considers two different regimes where wet deposition 
!>can be calculated: incloud (in model levels) and subcloud (at surface level).
!! [GEORGE]: Describe more

module wetdepmlclean2
  use iso_fortran_env, only: real64
  implicit none
  private
  
  !> Minimum sigma value for [Bartnicki] scavenging 
  !>(i.e., approxmiately maximum pressure altitude)
  real, save :: vminprec = -1.      
  !> minimum precipitation intensity (mm/hour)
  real, parameter :: precmin = 0.01

  public :: wetdep, init, requires_extra_precip_fields, &
      wetdep_precompute, wet_deposition_constant, wetdep_incloud_takemura

  integer, parameter, public :: WETDEP_SUBCLOUD_SCHEME_UNDEFINED = 0 
  integer, parameter, public :: WETDEP_SUBCLOUD_SCHEME_NONE = 1 
  integer, parameter, public :: WETDEP_SUBCLOUD_SCHEME_BARTNICKI = 2
      
  integer, parameter, public :: WETDEP_INCLOUD_SCHEME_UNDEFINED = 0 
  integer, parameter, public :: WETDEP_INCLOUD_SCHEME_NONE = 1 
  integer, parameter, public :: WETDEP_INCLOUD_SCHEME_TAKEMURA = 3

  !> Combine the two regimes into one scheme
  type, public :: wetdep_scheme_t
    integer :: subcloud
    integer :: incloud
    !> Use 3D precip and precomputed parameters
    logical :: use_vertical
    !> Whether to use cloud fraction correction for subcloud schemes
    logical :: use_cloudfraction
  end type

  !> As default, the scheme is undefined
  type(wetdep_scheme_t), save, public :: wetdep_scheme = &
      wetdep_scheme_t(WETDEP_SUBCLOUD_SCHEME_UNDEFINED, WETDEP_INCLOUD_SCHEME_UNDEFINED, .false., .false.)

contains

  !> Initialise wet deposition
  subroutine init()

    if (wetdep_scheme%subcloud == WETDEP_SUBCLOUD_SCHEME_BARTNICKI) then

      call wetdep2_init()

    endif
  end subroutine

!> Initialisation routine for the subcloud bartnicki scheme
  subroutine wetdep2_init()
    USE snapparML, only: ncomp
    USE snapparML, only: run_comp, def_comp

    integer :: m, mm
    real :: rm

    !> Define
    do m = 1, ncomp
      mm = run_comp(m)%to_defined
      rm = def_comp(mm)%radiusmym
      run_comp(m)%depconst = wet_deposition_constant(rm)
    end do

    if(.not.wetdep_scheme%use_vertical .and. vminprec < 0.) then                        
    ! Set minimum sigma level (maximum pressure level)
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
        vminprec = max(vminprec, 0.67)
      end block
    end if
    
  end subroutine

  !> Flag for data from model layers
  pure logical function requires_extra_precip_fields()
    requires_extra_precip_fields = wetdep_scheme%use_vertical
  end function

  !> Move activity from particle to wet deposition field 
  !>if wet deposition applicable
  subroutine wetdep(tstep, part, pextra)
    use iso_fortran_env, only: real64
    USE particleML, only: particle, extraParticle
    use snapparML, only: def_comp
    use snapfldml, only: depwet
    USE snapdimML, only: hres_pos

    real, intent(in) :: tstep
    type(particle), intent(inout) :: part
    type(extraParticle), intent(in) :: pextra
    integer :: i, j, mo, m
    m = part%icomp
    if (def_comp(m)%kwetdep == 1) then 
      
      ! If wet deposition applied to the component
      call calc_dep(depwet,tstep, part, pextra)
    endif
  end subroutine
  
  !> Calculates the wet deposition of a single component using 
  !> either the 2D or 3D scheme, depending on flags
  subroutine calc_dep(dep, tstep, part, pextra)
    USE iso_fortran_env, only: real64
    USE particleML, only: Particle, extraParticle
    USE snapparML, only: def_comp, run_comp

    !> Field which wet deposition gets added to
    real(real64), intent(inout) :: dep
    !> Timestep of the simulation, affects the deposition rate
    real, intent(in) :: tstep
    !> Particle description
    type(Particle), intent(inout) :: part
    !> uses the precipitation at the particle position
    type(extraParticle), intent(in) :: pextra
    integer :: m,mm
    real :: radlost, rkw
    
    m = part%icomp
    mm = def_comp(m)%to_running
    radlost = 0.0
    !! Figure out this bit
    if (wetdep_scheme%use_vertical) then
      !! in 3D case for bartnicki-takemura.
      call wetdep_3D(rkw, part,def_comp(m)%radiusmym)
      radlost = part%scale_rad(exp(-tstep*rkw))
    else if  (wetdep_scheme%subcloud == WETDEP_SUBCLOUD_SCHEME_BARTNICKI) then
      !! in 2D case just bartnicki
      if (pextra%prc > precmin &
        .AND. part%z > vminprec) then  ! [GEORGE]: I would argue precmin should apply for all schemes? Or is this not true for in cloud, as you can have absorption before precipitation?
        !depends on the precipitation and altitude at the place of the particle.
        rkw = wet_subcloud_bartnicki(def_comp(m)%radiusmym, pextra%prc, run_comp(mm)%depconst)
        radlost = part%scale_rad(exp(-tstep*rkw))
      end if
    end if
  
    dep = dep + dble(radlost)

  end subroutine


  subroutine wetdep_3D(rkw, part, radius)
    
    use iso_fortran_env, only: real64
    use particleml, only: particle 
    use snapgrdML, only: ivlevel 
    use snapfldML, only: precip3d, cw3d, cloud_cover !accum_precip,accum_ccf,
    
    !> Wet scavenging coefficient [1/s] of specific component
    real, intent(out) :: rkw
    !> Particle
    type(particle), intent(in) :: part
    !> Radius of particle
    real, intent(in) :: radius

    real :: rkw_tmp, mlprecip, mlccf
    integer :: ivlvl, i, j, k, nx, ny
    
    nx = size(precip3d,1)
    ny = size(precip3d,2)


    ivlvl = nint(part%z * 10000.0)
    k = ivlevel(ivlvl)
    i = nint(part%x)
    j = nint(part%y)  

    select case (wetdep_scheme%subcloud)
      case (WETDEP_SUBCLOUD_SCHEME_BARTNICKI)
        call calc_ml_var(k,precip3d(i,j,:),mlprecip)
        call calc_ml_var(k,cloud_cover(i,j,:),mlccf)
        call wet_subcloud_bartnicki_ccf(rkw, radius, mlprecip, &
          mlccf, use_ccf=wetdep_scheme%use_cloudfraction)
        ! call wet_subcloud_bartnicki_ccf(rkw, radius, accum_precip(i,j,k), &
        !   accum_ccf(i,j,k), use_ccf=wetdep_scheme%use_cloudfraction)
    case (WETDEP_SUBCLOUD_SCHEME_NONE)
        rkw = 0.0
      case default
        error stop "Subcloud scheme undefined"    
    end select


    ! Incloud
    select case (wetdep_scheme%incloud)
      case (WETDEP_INCLOUD_SCHEME_NONE)
        rkw_tmp = 0.0
      case (WETDEP_INCLOUD_SCHEME_TAKEMURA)
        call wetdep_incloud_takemura(rkw_tmp, precip3d(i,j,k), cw3d(i,j,k), cloud_cover(i,j,k))
      case default
        error stop "Incloud scheme undefined"
    end select
    rkw = max(rkw, rkw_tmp)
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
!> without cloud fraction adjustments [1/s]
  pure elemental real function wet_subcloud_bartnicki(radius, q, depconst, &
   use_convective) result(rkw)
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
    if (radius <= 0.1) then
    ! Gas scavenging [Aligns with NAME parameters for SO2 and ammonia]
      rkw = 1.12e-4*q**0.79 
    else
    ! Particle scavenging [Baklanov 2000]
      if (convect .and. q > 7.0) then               
      ! Convective scavenging [eq. 21]
        rkw = 3.36e-4*q**0.79
      else 
      ! Stratiform scavenging [eq. 16]
        if (radius > 0.1 .AND. radius <= 1.4) then    
          rkw = a0*q**0.79
        else if (radius > 1.4 .AND. radius <= 10.0) then   
          rkw = depconst*(a1*q + a2*q*q)
        else if (radius > 10.0) then          
          rkw = a1*q + a2*q*q
        end if
      end if
    end if

  end function

  !> Scales the precip intensity according to cloud fraction for bartnicki scheme as wished
  elemental subroutine wet_subcloud_bartnicki_ccf(wscav, radius, precip, ccf, use_ccf)
    !> Scavenging rate [1/s]
    real, intent(out) :: wscav
    !> Precipitation intensity [mm/h]
    real, intent(in) :: precip
    real, intent(in) :: radius
    real, intent(in) :: ccf
    logical, intent(in) :: use_ccf

    real :: depconst

    depconst = wet_deposition_constant(radius)

    if (.not.use_ccf) then
      wscav = wet_subcloud_bartnicki(radius, precip, depconst, use_convective=.False.)    !!! [GEORGE:] Does not use this currently
    else
      block
          !! [GEORGE]: is block neccessary? integers don't take much memory...
        real :: precip_scaled

        if (precip <= 0.0) then
          wscav = 0.0
        else

          if (ccf > 0.0) then
            ! Scale up precip intensity
            precip_scaled = precip/ ccf
          else
            !! Accounts for no instantaneous cloud fraction
            precip_scaled = precip           
          endif

          wscav = wet_subcloud_bartnicki(radius, precip_scaled, depconst, use_convective=.False.) !> Convective rain cannot be used here due to the precip scaling

          if (ccf > 0.0) then
            ! Scale down efficiency
            wscav = wscav * ccf
          endif
        end if
      end block
    endif
  end subroutine

  !> Aerosol rainout process also known as GCM-type wet deposition process
  elemental subroutine wetdep_incloud_takemura(wscav, q, cloud_water, cloud_fraction)
    !> Scavenging coefficient [1/s]
    real, intent(out) :: wscav
    !> Mass fraction of precipitation in model layer
    real, intent(in) :: q
    !> Fraction of aerosol mass in cloud water to total aerosol mass in the grid
    !> or the absorbtion coefficient
    !> Usually very high                            !! [GEORGE]: Does this mean 1.0 is not physical?? - book suggests this should be lower
    real, parameter :: f_inc = 1.0
    !> Mass fraction of cloud water
    real, intent(in) :: cloud_water
    !> Cloud fraction
    real, intent(in) :: cloud_fraction


    if (q > 0.0) then
      wscav = q/(q + cloud_water) * f_inc * cloud_fraction / 3600.0  ! Converting to [1/s]
    else
      wscav = 0.0
    end if
  end subroutine

  !> Should be called every input timestep to prepare the scavenging rates
  subroutine wetdep_precompute()
    use snapfldML, only: cw3d, precip3d, cloud_cover !, accum_precip, accum_ccf

    if (wetdep_scheme%use_vertical) then   
      !skip precomputation if no vertical scheme

      if (.not.(allocated(precip3d).and.allocated(cw3d).and.allocated(cloud_cover) &
    )) then  !.and.allocated(accum_precip).and.allocated(accum_ccf)
        error stop "Some wetdep/precip fields not allocated"
      endif

      ! call calc_accum_precip(accum_precip,accum_ccf,precip3d,cloud_cover)
    endif 
  end subroutine
  
  !> Calculates the accumulated precipitation and cloud cover downwards
  !> For the subcloud bartnicki scheme
  !> Precalculated because otherwise the calculation is repeated ncomp times unnecessarily.
  ![GEORGE]: why accumulated??
  ! subroutine calc_accum_precip(accum_precip,accum_ccf, precip, ccf) 

  !   real, intent(out) :: accum_precip(:,:,:), accum_ccf(:,:,:)
  !   !> Precipitation intensity [mm/h], 3D
  !   real, intent(in) :: precip(:,:,:)
  !   !> Cloud cover fraction
  !   real, intent(in) :: ccf(:,:,:)

  !   integer :: nk, k

  !   nk = size(precip,3)

  !   accum_precip(:,:,nk) = precip(:,:,nk)
  !   accum_ccf(:,:,nk) = ccf(:,:,nk)

  !   do k=nk-1,1,-1
  !     ! Accumulated precipitation in the column
  !     accum_precip(:,:,k) = accum_precip(:,:,k+1) + precip(:,:,k)
  !     accum_ccf(:,:,k) = accum_ccf(:,:,k+1) + ccf(:,:,k)
  !   end do

  !   where (accum_ccf >= 1.0)
  !     accum_ccf = 1.0
  !   endwhere
    
  ! end subroutine

  !> Integrates instantaneous measurements of precipitation 
  !> or cloud fraction at model levels upwards
  !> output: intensities for each level
  recursive subroutine calc_ml_var(k,var, accum_var)
    !> model level
    integer, intent(in) :: k
    !>Instantaneous variable at model level
    real, intent(in) :: var(:)
    !> Output of accumulation -> rate or intenity
    real, intent(out) :: accum_var
    integer :: nk

    nk = size(var)
    if (k .eq. nk) then
      accum_var = var(k)
    else
      call calc_ml_var(k+1,var,accum_var)
      accum_var = accum_var + var(k+1)
    end if

  end subroutine

end module wetdepmlclean2