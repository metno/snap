program testWetDep_new
  use snapparML, only: defined_component, push_down_dcomp, def_comp, run_comp, ncomp,&
       GRAV_TYPE_COMPUTED, GRAV_TYPE_FIXED
  use wetdepML, only: wetdep, init, requires_extra_precip_fields, &
      wet_deposition_constant, wetdep_scheme_t, wetdep_scheme, &
      WETDEP_SUBCLOUD_SCHEME_BARTNICKI, WETDEP_INCLOUD_SCHEME_NONE, &
      WETDEP_INCLOUD_SCHEME_TAKEMURA, &
      wet_subcloud_bartnicki_ccf, wetdep_precompute, vminprec, &
      wet_subcloud_bartnicki, wetdep_3D, wetdep_incloud_takemura
  use datetime, only: datetime_t
  use iso_fortran_env, only: real64
  use snapfldML, only: cw3d, precip3d, cloud_cover, depwet, precip
  use particleml, only: particle, extraParticle
  USE iso_fortran_env, only: real64
  USE snapdimML, only: hres_pos
  use snapgrdML, only: ivlevel 

  implicit none 
  

  type(defined_component), pointer :: d_comp
  type(datetime_t) :: itimefi
  type(wetdep_scheme_t) :: wetdep_scheme_1, wetdep_scheme_2
  integer :: i, mo, j, k, ivlvl, c, scheme,nk, mm
  real :: rad,  t1,t2, mlprecip, mlccf, tstep, radlost_b, &
          radlost_t, radlost_3d, radlost_b3d, rkw_b, rkw_3d, rkw_b3d, rkw_t
  type(Particle) :: part
  type(extraParticle) :: pextra
  real(real64) :: dep
  character(:), allocatable :: name


  ncomp = 2
  allocate(run_comp(ncomp))

  ! Define two components with dry deposition rates
  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "Cs137"
  d_comp%output_name = d_comp%compname ! defaults
  d_comp%radiusmym = 1.0
  d_comp%densitygcm3 = 2.3
  d_comp%grav_type = GRAV_TYPE_COMPUTED
  d_comp%kwetdep = 1


  call push_down_dcomp(def_comp, top=d_comp)
  d_comp%compname = "I131"
  d_comp%output_name = d_comp%compname ! default
  d_comp%radiusmym = 0.05 ! gas <= 0.05
  d_comp%densitygcm3 = 0.001
  d_comp%gravityms = 0.00001
  d_comp%grav_type = GRAV_TYPE_FIXED
  d_comp%kwetdep = 1

  print *, "1. Testing Bartnicki depconst"
  do i = 1, ncomp
    run_comp(i)%to_defined = i
    run_comp(i)%defined => def_comp(i)
    def_comp(i)%to_running = i
    def_comp(i)%to_output = i
    rad = def_comp(i)%radiusmym
    run_comp(i)%depconst = wet_deposition_constant(rad)
    print *, 'WETDEP2 m,r,depconst(m): ', i, rad, run_comp(i)%depconst
  end do
  print *, '------------------------------------------ ' 
  
  wetdep_scheme_1 = wetdep_scheme_t( &
              WETDEP_SUBCLOUD_SCHEME_BARTNICKI, &
              WETDEP_INCLOUD_SCHEME_NONE, &
              .false., .false. &
            )
  wetdep_scheme_2 = wetdep_scheme_t( &
              WETDEP_SUBCLOUD_SCHEME_BARTNICKI, &
              WETDEP_INCLOUD_SCHEME_TAKEMURA, &
              .true., .true. &
            )  
  itimefi%year = 2020
  itimefi%month = 4
  itimefi%day = 15
  itimefi%hour = 12
  part%x = 2 !relates to 201.0 in full domain
  part%y = 2 !relates to 589.0 in full domain
  part%z = 0.7
  tstep=90
  
  print *, "2. Testing wetdep subroutine"
  allocate(depwet(nint(part%x),nint(part%y),ncomp))

  call fetchTestData(precip3d,cw3d,cloud_cover,precip)
  do scheme = 1,2

    if (scheme == 2) then
      wetdep_scheme = wetdep_scheme_2
      name = "Bartnicki-Takemura"
    else if (scheme == 1) then
      wetdep_scheme = wetdep_scheme_1
      name = "Bartnicki"
    else 
      stop "Scheme undefined"
    end if
  
    print *, "                  _____________________________"
    print *, "                  WETDEP SCHEME: ", name
    print *, "                  _____________________________"
    call CPU_TIME(t1)
    call wetdep_precompute()  
    call CPU_TIME(t2)
    print *, "Precompute passed"
    print *, 'Time for computation', t2-t1
    vminprec = 0.1
    call init()
    print *, "Initialisation passed"


    do c =1,ncomp
      print *, "----------------------------------------------------"
      print *, "COMPONENT: ", def_comp(c)%compname
      part%icomp = c
      mo = def_comp(part%icomp)%to_output
      mm = def_comp(part%icomp)%to_running
      i = nint(part%x)
      j = nint(part%y) 
      ivlvl = nint(part%z * 10000.0)
      ivlevel(ivlvl)= 33
      k = ivlevel(ivlvl)
      print *, "Grid point i, j, k:", i, j, k
       
      pextra%prc = precip(i,j)

      print *, "Testing schemes..."
      rkw_b = wet_subcloud_bartnicki(def_comp(part%icomp)%radiusmym,& 
                            pextra%prc, run_comp(part%icomp)%depconst)
      rad = part%set_rad(1.) ! initial activity 1 Bq
      radlost_b= part%scale_rad(exp(-tstep*rkw_b))
   
      nk = size(precip3d,3)
      mlprecip =  sum(precip3d(i,j,k:nk))
      mlccf=sum(cloud_cover(i,j,k:nk))
      if (mlccf >= 1.0) mlccf = 1.0

      print *, "mlprecip, mlccf", mlprecip, mlccf
      call wet_subcloud_bartnicki_ccf(rkw_b3d,def_comp(part%icomp)%radiusmym,&
                                mlprecip,mlccf,wetdep_scheme%use_cloudfraction)
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      radlost_b3d= part%scale_rad(exp(-tstep*rkw_b3d))
  
      call wetdep_incloud_takemura(rkw_t,precip3d(i,j,k),cw3d(i,j,k),cloud_cover(i,j,k))
      
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      radlost_t= part%scale_rad(exp(-tstep*rkw_t))
  
  
      call wetdep_3D(rkw_3d,part,def_comp(part%icomp)%radiusmym)
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      radlost_3d= part%scale_rad(exp(-tstep*rkw_3d))      
          
      ! print *, "----------------------------------------------------"
      write (*,10) "Scheme", "Scavenging rate [1/s]", "Deposited radiation"
      write (*,11) "Bartnicki", rkw_b, radlost_b
      write (*,11) "Bart. ccf", rkw_b3d, radlost_b3d
      write (*,11) "Takemura", rkw_t, radlost_t
      write (*,11) "Wetdep 3D", rkw_3d, radlost_3d
      ! print *, "----------------------------------------------------"    
      
      10 FORMAT(5x,A9, 3x, A21, A21)
      11 FORMAT(5x,A9, 3x, ES21.8, ES21.8)
      
      i = hres_pos(part%x)
      j = hres_pos(part%y) 
      print *, "hres i,j", i, j  
      print *, "mo", mo  
      depwet(i,j,mo) = 0.0
      print *, "Computing from wetdep subroutine..."
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      call CPU_TIME(t1)
      call wetdep(tstep,part,pextra)
      call CPU_TIME(t2)
      print *, "Completed calculations!"
      print *, 'Time for computation', t2-t1

      print *, shape(depwet)
      print *, maxval(depwet(:,:,:))
      dep = depwet(i,j,mo)
      print*, "Deposition: ", dep

      select case (name)
      case ("Bartnicki-Takemura")
        if (abs(dep - radlost_b3d) > 1.0e-5 .and. abs(dep - radlost_t) > 1.0e-5) &
          stop "Test failed: calculated deposition does not match expected &
                                    &bartnicki ccf or takemura deposition "

        select case (def_comp(c)%compname)
        case ("Cs137")
          if (abs(dep - 9.81414318e-03) > 1.0e-5) &
            stop "Test failed: calculated deposition for bartnicki-takemura &
                                        &scheme does not match expected value"
          
        case ("I131")
          if (abs(dep - 1.30640268E-02) > 1.0e-5) &
            stop "Test failed: calculated deposition for bartnicki-takemura &
                                      &scheme does not match expected value"
        
        end select

      case ("Bartnicki")
        if (abs(dep - radlost_b) > 1.0e-5) &
          stop "Test failed: calculated deposition does not match expected &
                                                      &bartnicki deposition"
        
        select case (def_comp(c)%compname)
        case ("Cs137")
          if (abs(dep - 1.28421783e-2) > 1.0e-5) &
            stop "Test failed: calculated deposition for Bartnicki scheme &
                                              &does not match expected value"
        case ("I131")
          if (abs(dep - 1.70861483e-02) > 1.0e-5) &
            stop "Test failed: calculated deposition for Bartnicki scheme &
                                            &does not match expected value"
        end select
      end select
    end do
  end do

  
  print *, "                  _____________________________"
  print *, "               HIGHER ALTITUDE (in-cloud dominance):"
  print *, "                  _____________________________"


  part%z = 0.5

  scheme = 2

  wetdep_scheme = wetdep_scheme_2
  name = "Bartnicki-Takemura"
  
  call CPU_TIME(t1)
  call wetdep_precompute()  
  call CPU_TIME(t2)
  print *, "Precompute passed"
  print *, 'Time for computation', t2-t1
  vminprec = 0.1
  call init()
  print *, "Initialisation passed" 

  do c =1,ncomp
      print *, "----------------------------------------------------"
      print *, "COMPONENT: ", def_comp(c)%compname
      part%icomp = c
      mo = def_comp(part%icomp)%to_output

      i = nint(part%x)
      j = nint(part%y) 
      ivlvl = nint(part%z * 10000.0)
      ivlevel(ivlvl)= 42
      k = ivlevel(ivlvl)
      print *, "Grid point i, j, k:", i, j, k
    
    
      pextra%prc = precip(i,j)

      print *, "Testing schemes..."
      rkw_b = wet_subcloud_bartnicki(def_comp(part%icomp)%radiusmym,& 
                            pextra%prc, run_comp(part%icomp)%depconst)
      rad = part%set_rad(1.) ! initial activity 1 Bq
      radlost_b= part%scale_rad(exp(-tstep*rkw_b))

    
      nk = size(precip3d,3)
      mlprecip =  sum(precip3d(i,j,k:nk))
      mlccf=sum(cloud_cover(i,j,k:nk))
      if (mlccf >= 1.0) mlccf = 1.0
      print *, "mlprecip, mlccf", mlprecip, mlccf
      call wet_subcloud_bartnicki_ccf(rkw_b3d,def_comp(part%icomp)%radiusmym,&
                                mlprecip,mlccf,wetdep_scheme%use_cloudfraction)
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      radlost_b3d= part%scale_rad(exp(-tstep*rkw_b3d))
  
      call wetdep_incloud_takemura(rkw_t,precip3d(i,j,k),cw3d(i,j,k),cloud_cover(i,j,k))
      
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      radlost_t= part%scale_rad(exp(-tstep*rkw_t))
  
  
      call wetdep_3D(rkw_3d,part,def_comp(part%icomp)%radiusmym)
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      radlost_3d= part%scale_rad(exp(-tstep*rkw_3d))      
    
      write (*,10) "Scheme", "Scavenging rate [1/s]", "Deposited radiation"
      write (*,11) "Bartnicki", rkw_b, radlost_b
      write (*,11) "Bart. ccf", rkw_b3d, radlost_b3d
      write (*,11) "Takemura", rkw_t, radlost_t
      write (*,11) "Wetdep 3D", rkw_3d, radlost_3d    
      
      i = hres_pos(part%x)
      j = hres_pos(part%y) 

      depwet(i,j,mo) = 0.0
      print *, "Computing from wetdep subroutine..."
      rad = part%set_rad(1.) ! reset initial activity 1 Bq
      call CPU_TIME(t1)
      call wetdep(tstep,part,pextra)
      call CPU_TIME(t2)
      print *, "Completed calculations!"
      print *, 'Time for computation', t2-t1

  
      dep = depwet(i,j,mo)
      print*, "Deposition: ", dep

      if (abs(dep - radlost_b3d) > 1.0e-5 .and. abs(dep - radlost_t) > 1.0e-5) &
        stop "Test failed: calculated deposition does not match expected &
                                    &bartnicki ccf or takemura deposition "

      select case (def_comp(c)%compname)
      case ("Cs137")
        if (abs(dep - 2.27106214e-02) > 1.0e-5) &
          stop "Test failed: calculated deposition for bartnicki-takemura &
                                        &scheme does not match expected value"
          
      case ("I131")
        if (abs(dep - 1.13438964e-02) > 1.0e-5) &
          stop "Test failed: calculated deposition for bartnicki-takemura &
                                      &scheme does not match expected value"
        
      end select
  end do
  print *, "All tests passed"
   
  contains

  subroutine fetchTestData(prec,cw,cf,pr)
    use netcdf
    real, allocatable, intent(inout) :: prec(:,:,:), cw(:,:,:), cf(:,:,:), pr(:,:)
    integer :: varid, id, status, nx, ny, nk
    integer, dimension(nf90_max_var_dims) :: dimIDs 
    status = nf90_open(path = "/lustre/storeB/users/geche8548/wet-deposition/wetdeptest-small.nc", mode = nf90_nowrite, ncid= id)
    if (status /= nf90_noerr) then 
      print *, "Error in opening file "
      stop 2
    end if  

    status = nf90_inq_varid(id, "precip3d", varid)
    if (status /= nf90_noerr) then 
      print *, "Error: no precip3d variable"
      stop 2
    end if 

    status = nf90_inquire_variable(id, VarId, dimids = dimIDs)
    if (status /= nf90_noerr) then 
      print *, "Error finding dimension ids"
      stop 2
    end if 
    status = nf90_inquire_dimension(id, dimIDs(3), len = nk)
    status = nf90_inquire_dimension(id, dimIDs(1), len = nx)
    status = nf90_inquire_dimension(id, dimIDs(2), len = ny)
    
    allocate(prec(nx,ny,nk))
    status = nf90_get_var(id, varid, prec)
    if (status /= nf90_noerr) then 
      print *, "Error reading precip3d"
      stop 2
    end if 

    status = nf90_inq_varid(id, "cw3d", varid)
    if (status /= nf90_noerr) then 
      print *, "Error: no cw3d variable"
      stop 2
    end if 

    allocate(cw(nx,ny,nk))
    status = nf90_get_var(id, varid, cw)
    if (status /= nf90_noerr) then 
      print *, "Error reading cw3d"
      stop 2
    end if 

    status = nf90_inq_varid(id, "cloud_cover", varid)
    if (status /= nf90_noerr) then 
      print *, "Error: no cloud_cover variable"
      stop 2
    end if 

    allocate(cf(nx,ny,nk))
    status = nf90_get_var(id, varid, cf)
    if (status /= nf90_noerr) then 
      print *, "Error reading cloud_cover"
      stop 2
    end if  
    
    prec = prec * 100
    cw = cw * 100

    status = nf90_inq_varid(id, "surface_precip", varid)
    if (status /= nf90_noerr) then 
      print *, "Error: no surface precip variable"
      stop 2
    end if 

    allocate(pr(nx,ny))
    status = nf90_get_var(id, varid, pr)
    if (status /= nf90_noerr) then 
      print *, "Error reading surface precip"
      stop 2
    end if     

    status = nf90_close(id)
    if (status /= nf90_noerr) then 
      print *, "Error closing file"
      stop 2
    end if 

  end subroutine

end program