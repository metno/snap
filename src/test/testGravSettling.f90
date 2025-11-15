program testGravSettling
  use vgravtablesML, only : vgravtables_init, vgrav, vgrav_iter
  use snapparML, only: def_comp, push_down_dcomp, defined_component, GRAV_TYPE_COMPUTED, &
    ncomp, run_comp
  implicit none

  integer :: i, t , p
  real(8) :: vg1, vg2
  type(defined_component), pointer :: d_comp

  ! define 4 components 1,10,50,100 µm radius
  ncomp = 4
  allocate (run_comp(ncomp))
  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "Ra100"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 100.0
  d_comp%densitygcm3 = 2.3
  d_comp%grav_type = GRAV_TYPE_COMPUTED

  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "Ra50"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 50.0
  d_comp%densitygcm3 = 2.3
  d_comp%grav_type = GRAV_TYPE_COMPUTED

  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "Ra10"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 10.0
  d_comp%densitygcm3 = 2.3
  d_comp%grav_type = GRAV_TYPE_COMPUTED

  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "Ra1"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 1.0
  d_comp%densitygcm3 = 2.3
  d_comp%grav_type = GRAV_TYPE_COMPUTED


  do i = 1, ncomp
    run_comp(i)%to_defined = i
    run_comp(i)%defined => def_comp(i)
    def_comp(i)%to_running = i
  end do

  call vgravtables_init()
  do i = 1, ncomp
    print *, "Component ", i, ": ", def_comp(i)%compname, ", radius ", &
      def_comp(i)%radiusmym, " µm, density ", def_comp(i)%densitygcm3, " g/cm3"
    print *, 2*def_comp(i)%radiusmym, "µm diam, P=1013.25hPa, T=300K:"
    print *, 'Tabulated Gravitational settling,', vgrav(i, 1013.25, 300.0), "m/s"
    print *, 'Iterated gravitational settling:', &
      vgrav_iter(2*def_comp(i)%radiusmym, def_comp(i)%densitygcm3, 1013.25, 300.0), "m/s"
  end do

  ! ensure that tabulated values are within 1% of iterated values
  do i = 1, ncomp
    do p = 100, 1100, 1 ! hPa
      do t = 200, 350, 1 ! K
        vg1 = vgrav(i, real(p), real(t))
        vg2 = vgrav_iter(2*def_comp(i)%radiusmym, def_comp(i)%densitygcm3, real(p), real(t))
        if ( abs( vg1 - vg2 ) / vg2 > 0.01 ) then
          print *, "Error: Tabulated and iterated gravitational settling differ by more than 1% for component ", i, &
            " at p=", p, " hPa, t=", t, " K: ", vg1, " vs ", vg2
          stop 1
        end if
      end do
    end do
  end do
end program testGravSettling
