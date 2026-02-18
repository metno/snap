program testDryDep
  use snapparML, only: defined_component, push_down_dcomp, def_comp, run_comp, ncomp,&
       GRAV_TYPE_COMPUTED, GRAV_TYPE_FIXED
  use particleML, only: Particle
  use vgravtablesML, only : vgravtables_init
  use drydepML, only: drydep_precompute_meteo, drydep_precompute_raero, drydep_precompute_particle, drydep_scheme, &
       DRYDEP_SCHEME_EMERSON
  use datetime, only: datetime_t
  use iso_fortran_env, only: real64
  implicit none

  type(defined_component), pointer :: d_comp
  type(datetime_t) :: itimefi
  integer :: i
  integer(kind=1) :: classnr
  real(4) :: ps2, t2m, yflux, xflux, surface_stress, z0, hflux, &
        vd_dep
  real(kind=real64) :: ustar, raero, my, nu

  ncomp = 2
  allocate(run_comp(ncomp))

  ! Define two components with dry deposition rates
  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "Cs137"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 1.0
  d_comp%densitygcm3 = 2.3
  d_comp%grav_type = GRAV_TYPE_COMPUTED


  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "I131"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 0.05 ! gas <= 0.05
  d_comp%densitygcm3 = 0.001
  d_comp%gravityms = 0.00001
  d_comp%grav_type = GRAV_TYPE_FIXED

  do i = 1, ncomp
    run_comp(i)%to_defined = i
    run_comp(i)%defined => def_comp(i)
    def_comp(i)%to_running = i
  end do
  call vgravtables_init()


  drydep_scheme = DRYDEP_SCHEME_EMERSON
  itimefi%year = 2020
  itimefi%month = 4
  itimefi%day = 15
  itimefi%hour = 12

  ps2 = 1013.25 ! hPa
  t2m = 280.0 ! K
  yflux = 1.0 ! N /m2
  xflux = 1.0 ! N /m2
  surface_stress = HYPOT(yflux, xflux) ! N /m2
  z0 = 0.1 ! m
  hflux = 100.0 ! W hr/m2
  classnr = 21 ! class number 11=sea, 21=mixed forest
  call drydep_precompute_meteo(ps2*100., t2m, surface_stress, ustar, my, nu)
  call drydep_precompute_raero(ps2*100., t2m, z0, hflux, ustar, raero)


  ! Test dry-dep velocity for Cs137
  call drydep_precompute_particle(ps2*100., t2m, &
    ustar, raero, my, nu, itimefi, &
    def_comp(1), classnr, vd_dep)
  if (abs(vd_dep - 1.5790e-2) .gt. 1.0e-5) then
    print *, "Error in dry-dep velocity for gas phase Cs137: ", vd_dep, abs(vd_dep - 1.5790e-2)
    stop 2
  end if
  ! iodine
  call drydep_precompute_particle(ps2*100., t2m, &
    ustar, raero, my, nu, itimefi, &
    def_comp(2), classnr, vd_dep)
  if (abs(vd_dep - 0.008) .gt. 1.0e-4) then
    print *, "Error in dry-dep velocity for gas phase I131: ", vd_dep
    stop 2
  end if


  ! test numerical limits of dry deposition
  block
    integer, parameter :: tstep = 90, h = 30
    type(Particle) :: part
    real(4) :: deprate_m1, rad
    real(4) ::dep
    real(4) :: vd

    ! test various deposition velocities
    do i = 1, 8
      rad = part%set_rad(1.) ! initial activity 1 Bq
      vd = 0.1**i
      deprate_m1 = 1-exp(-tstep*vd/h)
      print *, "vd=", vd, " deprate_m1=", deprate_m1
      dep = part%scale_rad(1-deprate_m1)
      if (dep == 0.0 .or. part%rad() == 1.0) then
        print *, "Dry deposition resulted in zero removal at vd=", vd, &
          "giving deprate_m1=", deprate_m1, &
          " for tstep=", tstep, " and h=", h, &
          "dep=", dep, " remaining rad=", part%rad()
        stop 3
      end if
    end do
  end block

  print *, "Dry deposition test passed."

end program testDryDep
