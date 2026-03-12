program testWetDep
  use snapparML, only: defined_component, push_down_dcomp, def_comp, run_comp, ncomp,&
       GRAV_TYPE_COMPUTED, GRAV_TYPE_FIXED
  use wetdepML, only: wetdep, wetdep2, init, requires_extra_precip_fields, &
      wet_deposition_constant, wetdep_scheme_t, wetdep_scheme, &
      WETDEP_SUBCLOUD_SCHEME_BARTNICKI, WETDEP_INCLOUD_SCHEME_NONE, &
      WETDEP_INCLOUD_SCHEME_TAKEMURA, wet_deposition_constant,&
      wet_subcloud_bartnicki_ccf, wetdep_precompute, vminprec, &
      wet_subcloud_bartnicki, wetdep_incloud_takemura, wetdep_using_precomputed_wscav
  use datetime, only: datetime_t
  use iso_fortran_env, only: real64
  use snapfldML, only: cw3d, precip3d, cloud_cover, depwet, precip, wscav
  use particleml, only: particle, extraParticle
  USE iso_fortran_env, only: real64
  USE snapdimML, only: hres_pos
  use snapgrdML, only: ivlevel 

  implicit none 
  

  type(defined_component), pointer :: d_comp
  type(datetime_t) :: itimefi
  type(wetdep_scheme_t) :: wetdep_scheme_1, wetdep_scheme_2
  integer :: i, mo, j, k, ivlvl, c, scheme
  real :: rad,  t1,t2, mlprecip, mlccf, tstep, radlost_b, &
          radlost_t, radlost_3d, radlost_b3d, rkw_b, rkw_3d, rkw_b3d, rkw_t
  type(Particle) :: part
  type(extraParticle) :: pextra
  real(real64) :: dep
  logical :: CREATE_MOD = .false.
  character(:), allocatable :: name
  real, allocatable:: wscav_tmp (:,:)
  if (CREATE_MOD) then
    call createTestFile
  
  else


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
      rad = def_comp(i)%radiusmym
      run_comp(i)%depconst = wet_deposition_constant(rad)
      print *, 'WETDEP2 m,r,depconst(m): ', i, rad, run_comp(i)%depconst
    end do
    print *, '------------------------------------------ ' 
  
    !     write (iulog, *) 'WETDEP2 PREPARE .... q,deprate(1:ncomp):'
  
  !     do n = 1, 200
  !       q = float(n)*0.1
  !       do m = 1, ncomp
  !         mm = run_comp(m)%to_defined
  !         ratdep(m) = wet_deposition_rate(def_comp(mm)%radiusmym, q, run_comp(m)%depconst, tstep)
  !       end do
  !       write (iulog, 1010) q, (ratdep(m), m=1, ncomp)
  ! 1010  format(1x, f5.1, ':', 12f7.4)
  !     end do
  !     write (iulog, *) '-------------------------------------------------'
  
  
  
  !             write(iulog,*) 'POSINT. precmin,vminprec: ',precmin,vminprec
    
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
    part%x = 201.0
    part%y = 589.0
    part%z = 0.7
    tstep=90
    
    print *, "2. Testing wetdep subroutine"
    allocate(depwet(nint(part%x),nint(part%y),ncomp))
    allocate(wscav_tmp(nint(part%x),nint(part%y)))
    call fetchTestData(precip3d,cw3d,cloud_cover,precip,wscav)
    
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
  
  
  
      do c =1,ncomp
        print *, "----------------------------------------------------"
        print *, "COMPONENT: ", def_comp(c)%compname
        part%icomp = c
        mo = def_comp(part%icomp)%to_output

        call CPU_TIME(t1)
        call wetdep_precompute()  
        call CPU_TIME(t2)
        print *, "Precompute passed"
        print *, 'Time for computation', t2-t1
        vminprec = 0.1
        call init()
        print *, "Initialisation passed"
  
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
  
      
        ! call calc_ml_var(k,cloud_cover(i,j,:),mlccf)  

        ! print *, "mlprecip, mlccf", mlprecip, mlccf
        ! call wet_subcloud_bartnicki_ccf(,def_comp(part%icomp)%radiusmym,&
        !                            mlprecip,mlccf,wetdep_scheme%use_cloudfraction)
        ! rkw_b3d = wscav_tmp(i,j)
                                   ! rad = part%set_rad(1.) ! reset initial activity 1 Bq
        ! radlost_b3d= part%scale_rad(exp(-tstep*rkw_b3d))
    
        call wetdep_incloud_takemura(wscav_tmp(:,:),precip3d(:,:,k),cw3d(:,:,k),cloud_cover(:,:,k))
        rkw_t = wscav_tmp(i,j)
        rad = part%set_rad(1.) ! reset initial activity 1 Bq
        radlost_t= part%scale_rad(exp(-tstep*rkw_t))
    
        rkw_3d = wscav(i,j,k,part%icomp)
        i = hres_pos(part%x)
        j = hres_pos(part%y)
        mo = def_comp(part%icomp)%to_output
        rad = part%set_rad(1.) ! reset initial activity 1 Bq
        depwet(i,j,mo) = 0.0
        call wetdep_using_precomputed_wscav(part,wscav,depwet,tstep)

        radlost_3d= depwet(i,j,mo)     
      
        ! print *, "----------------------------------------------------"
        write (*,10) "Scheme", "Scavenging rate [1/s]", "Deposited radiation"
        write (*,11) "Bartnicki", rkw_b, radlost_b
        write (*,11) "Bart. ccf", rkw_b3d, radlost_b3d
        write (*,11) "Takemura", rkw_t, radlost_t
        write (*,11) "Wetdep 3D", rkw_3d, radlost_3d
        ! print *, "----------------------------------------------------"    
        
        10 FORMAT(5x,A9, 3x, A21, A21)
        11 FORMAT(5x,A9, 3x, ES21.8, ES21.8)

    
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

        select case (name)
        case ("Bartnicki-Takemura")
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
  
      
        ! call calc_ml_var(k,cloud_cover(i,j,:),mlccf)  

        ! print *, "mlprecip, mlccf", mlprecip, mlccf
        ! call wet_subcloud_bartnicki_ccf(,def_comp(part%icomp)%radiusmym,&
        !                            mlprecip,mlccf,wetdep_scheme%use_cloudfraction)
        ! rkw_b3d = wscav_tmp(i,j)
                                   ! rad = part%set_rad(1.) ! reset initial activity 1 Bq
        ! radlost_b3d= part%scale_rad(exp(-tstep*rkw_b3d))
    
        call wetdep_incloud_takemura(wscav_tmp(:,:),precip3d(:,:,k),cw3d(:,:,k),cloud_cover(:,:,k))
        rkw_t = wscav_tmp(i,j)
        rad = part%set_rad(1.) ! reset initial activity 1 Bq
        radlost_t= part%scale_rad(exp(-tstep*rkw_t))
    
        rkw_3d = wscav(i,j,k,part%icomp)
        i = hres_pos(part%x)
        j = hres_pos(part%y)
        mo = def_comp(part%icomp)%to_output
        rad = part%set_rad(1.) ! reset initial activity 1 Bq
        depwet(i,j,mo) = 0.0
        call wetdep_using_precomputed_wscav(part,wscav,depwet,tstep)

        radlost_3d= depwet(i,j,mo)     
      
        ! print *, "----------------------------------------------------"
        write (*,10) "Scheme", "Scavenging rate [1/s]", "Deposited radiation"
        write (*,11) "Bartnicki", rkw_b, radlost_b
        write (*,11) "Bart. ccf", rkw_b3d, radlost_b3d
        write (*,11) "Takemura", rkw_t, radlost_t
        write (*,11) "Wetdep 3D", rkw_3d, radlost_3d
        ! print *, "----------------------------------------------------"    

    
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

        if (abs(dep - radlost_t) > 1.0e-5) &
          stop "Test failed: calculated deposition does not match expected &
                                      &bartnicki ccf or takemura deposition "

        select case (def_comp(c)%compname)
        case ("Cs137")
          if (abs(dep - 2.27106214E-02) > 1.0e-5) &
            stop "Test failed: calculated deposition for bartnicki-takemura &
                                         &scheme does not match expected value"
            
        case ("I131")
          if (abs(dep - 2.27106214E-02) > 1.0e-5) &
            stop "Test failed: calculated deposition for bartnicki-takemura &
                                        &scheme does not match expected value"
          
        end select
    end do
    print *, "All tests passed"
  end if
   
  contains

  recursive subroutine calc_ml_var(k,var, accum_var, cloud)
    !> model level
    integer, intent(in) :: k
    !>Instantaneous variable at model level
    real, intent(in) :: var(:)
    logical, intent(in) :: cloud
    !> Output of accumulation -> rate or intenity
    real, intent(out) :: accum_var
    integer :: nk

    nk = size(var)
    if (cloud .and. accum_var >= 1.0) then
      accum_var = 1.0
    else if (k .eq. nk) then
      accum_var = var(k)
    else
      call calc_ml_var(k+1,var,accum_var,cloud)
      accum_var = accum_var + var(k)
    end if
    if (cloud .and. accum_var >= 1.0) accum_var = 1.0
  end subroutine

  subroutine fetchTestData(prec,cw,cf,pr, wscav)
    use netcdf
    real, allocatable, intent(inout) :: prec(:,:,:), cw(:,:,:), cf(:,:,:), pr(:,:), wscav(:,:,:,:)
    integer :: varid, id, status, nx, ny, nk
    integer, dimension(nf90_max_var_dims) :: dimIDs 
    status = nf90_open(path = "./data/wetdeptest.nc", mode = nf90_nowrite, ncid= id)
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
    allocate(wscav(nx,ny,nk,ncomp))
    status = nf90_close(id)
    if (status /= nf90_noerr) then 
      print *, "Error closing file"
      stop 2
    end if  

  end subroutine




  subroutine createTestFile
    use netcdf
    integer :: id, status, varid, id_new,nk, nx, ny, nt, xdimid,ydimid,kdimid, &
               precipvarid,cw3dvarid, ccvarid, prvarid
    real, allocatable ::  tmpvar(:,:,:), tmpvar2d(:,:)
    real, allocatable :: precip3d(:,:,:), cw3d(:,:,:), cloud_cover(:,:,:), pr(:,:)
    integer, dimension(nf90_max_var_dims) :: dimIDs 
    
    status = nf90_open(path = &
    "/lustre/storeB/immutable/archive/projects/metproduction/MEPS/2026/02/08/meps_det_2_5km_20260208T00Z.nc",&
                      mode = nf90_nowrite, ncid= id)
    if (status /= nf90_noerr) then 
      print *, "Error in opening file "
      stop 2
    end if
    print *, "succesfully opened!"
  
    
    status = nf90_inq_varid(id, "mass_fraction_of_rain_in_air_ml", varid)
    if (status /= nf90_noerr) then 
      print *, "Error: no 3D rain variable"
      stop 2
    end if 

    status = nf90_inquire_variable(id, VarId, dimids = dimIDs)
    if (status /= nf90_noerr) then 
      print *, "Error finding dimension ids"
      stop 2
    end if 
    status = nf90_inquire_dimension(id, dimIDs(3), len = nk)
    status = nf90_inquire_dimension(id, dimIDs(4), len = nt)
    status = nf90_inquire_dimension(id, dimIDs(1), len = nx)
    status = nf90_inquire_dimension(id, dimIDs(2), len = ny)
    print *, "dimension sizes", nx,ny,nk

    
    allocate(tmpvar(nx,ny,nk))
    status = nf90_get_var(id, varid, tmpvar, start=(/1,1,1,nt/), count=(/nx,ny,nk,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading rain"
      stop 2
    end if 

    allocate(precip3d(nx,ny,nk))
    precip3d(:,:,:) = tmpvar(:,:,:) 
    deallocate(tmpvar)

    status = nf90_inq_varid(id, "mass_fraction_of_snow_in_air_ml", varid)
    if (status /= nf90_noerr) then 
      print *, "Warning: no 3D snow variable"
      stop 2
    end if 
    
    allocate(tmpvar(nx,ny,nk))
    status = nf90_get_var(id, varid, tmpvar, start=(/1,1,1,nt/), count=(/nx,ny,nk,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading snow"
      stop 2
    end if 

    precip3d(:,:,:) = precip3d(:,:,:) + tmpvar(:,:,:)
    deallocate(tmpvar)

    status = nf90_inq_varid(id, "mass_fraction_of_graupel_in_air_ml", varid)
    if (status /= nf90_noerr) then 
      print *, "Warning: no 3D graupel variable"
      stop 2
    end if 

    allocate(tmpvar(nx,ny,nk))

    status = nf90_get_var(id, varid, tmpvar, start=(/1,1,1,nt/), count=(/nx,ny,nk,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading graupel"
      stop 2
    end if 

    precip3d(:,:,:) = precip3d(:,:,:) + tmpvar(:,:,:)
    deallocate(tmpvar)
    print *, "successfully read data and created precip3d!"
    
    status = nf90_inq_varid(id, "mass_fraction_of_cloud_condensed_water_in_air_ml", varid)
    if (status /= nf90_noerr) then 
      print *, "Warning: no 3D cloud water variable"
      stop 2
    end if 

    allocate(tmpvar(nx,ny,nk))
    status = nf90_get_var(id, varid, tmpvar, start=(/1,1,1,nt/), &
                                              count=(/nx,ny,nk,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading cloud water"
      stop 2
    end if 

    allocate(cw3d(nx,ny,nk))
    cw3d(:,:,:) = tmpvar(:,:,:)
    deallocate(tmpvar)


    status = nf90_inq_varid(id, "mass_fraction_of_cloud_ice_in_air_ml", varid)
    if (status /= nf90_noerr) then 
      print *, "Warning: no 3D cloud ice variable"
      stop 2
    end if
    
    allocate(tmpvar(nx,ny,nk))
    status = nf90_get_var(id, varid, tmpvar, start=(/1,1,1,nt/),&
                                             count=(/nx,ny,nk,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading cloud ice"
      stop 2
    end if 

    cw3d(:,:,:) = cw3d(:,:,:) + tmpvar(:,:,:)
    deallocate(tmpvar)
    print *, "successfully read data and created cw3d!"


    status = nf90_inq_varid(id, "cloud_area_fraction_ml", varid)
    if (status /= nf90_noerr) then 
      print *, "Warning: no cloud fraction variable"
      stop 2
    end if
    
    allocate(tmpvar(nx,ny,nk))
    status = nf90_get_var(id, varid, tmpvar, start=(/1,1,1,nt/), &
                                             count=(/nx,ny,nk,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading cloud cover"
      stop 2
    end if 
     
    
    allocate(cloud_cover(nx,ny,nk))
    cloud_cover(:,:,:) = tmpvar(:,:,:)

    deallocate(tmpvar)

    status = nf90_inq_varid(id, "precipitation_amount_acc", varid)
    if (status /= nf90_noerr) then 
      print *, "Error: no acc precip variable"
      stop 2
    end if

    allocate(tmpvar2d(nx,ny))
    status = nf90_get_var(id, varid, tmpvar2d, start=(/1,1,1,nt/),&
                                                count=(/nx,ny,1,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading acc precip"
      stop 2
    end if    

    allocate(pr(nx,ny))
    
    pr(:,:) = tmpvar2d(:,:)

    status = nf90_get_var(id, varid, tmpvar2d, start=(/1,1,1,nt-1/),&
                                                  count=(/nx,ny,1,1/))
    if (status /= nf90_noerr) then 
      print *, "Error on reading acc precip"
      stop 2
    end if 

    pr(:,:) = pr(:,:) - tmpvar2d(:,:)

    where (pr < 0.0)
      pr = 0.0
    end where

    deallocate(tmpvar2d)
    
    
    status = nf90_close(id)
    if (status /= nf90_noerr) then 
      print *, "Error closing file"
      stop 2
    end if

    print *, "successfully closed!"

    status = nf90_create(path = "./snap_testdata/wetdeptest.nc", &
                            cmode = nf90_clobber, ncid= id_new)
    if (status /= nf90_noerr) then 
      print *, "Error on creating new file"
      stop 2
    end if 
    status = nf90_def_dim(id_new, "x", size(precip3d,1), xdimid)
    status = nf90_def_dim(id_new, "y", size(precip3d,2), ydimid)
    status = nf90_def_dim(id_new, "k", size(precip3d,3), kdimid)
    if (status /= nf90_noerr) then 
      print *, "Error on defining dimensions"
      stop 2
    end if  
    
    status = nf90_def_var(id_new, "precip3d", nf90_float, &
                      (/ xdimid, ydimid, kdimid /), precipvarid)
    if (status /= nf90_noerr) then 
      print *, "Error on defining precip3d"
      stop 2
    end if  

    status = nf90_def_var(id_new, "cw3d", nf90_float, &
                      (/ xdimid, ydimid, kdimid /), cw3dvarid)
    if (status /= nf90_noerr) then 
      print *, "Error on defining cw3d"
      stop 2
    end if  

    status = nf90_def_var(id_new, "cloud_cover", nf90_float, &
                      (/ xdimid, ydimid, kdimid /), ccvarid)
    if (status /= nf90_noerr) then 
      print *, "Error on defining cloud_cover"
      stop 2
    end if 
    
    status = nf90_def_var(id_new, "surface_precip", nf90_float, &
                      (/ xdimid, ydimid/), prvarid)
    if (status /= nf90_noerr) then 
      print *, "Error on defining surface precip"
      stop 2
    end if 

    status = nf90_enddef(id_new)
    if (status /= nf90_noerr) then 
      print *, "Error on ending definition status"
      stop 2
    end if  
    status = nf90_put_var(id_new, precipvarid, precip3d )
    if (status /= nf90_noerr) then 
      print *, "Error writing precipitation to file"
      stop 2
    end if  
    print *, "Written precip to file!"
    deallocate(precip3d)

    status = nf90_put_var(id_new, cw3dvarid, cw3d )
    if (status /= nf90_noerr) then 
      print *, "Error writing cloud water to file"
      stop 2
    end if  
    print *, "Written cloud water to file!"
    deallocate(cw3d)

    status = nf90_put_var(id_new, ccvarid, cloud_cover )
    if (status /= nf90_noerr) then 
      print *, "Error writing cloud_cover to file"
      stop 2
    end if  
    print *, "Written cloud cover to file!"
    deallocate(cloud_cover)

    status = nf90_put_var(id_new, prvarid, pr )
    if (status /= nf90_noerr) then 
      print *, "Error writing surface precip to file"
      stop 2
    end if  
    print *, "Written surface precip to file!"
    deallocate(pr)

    status = nf90_close(id_new)
    if (status /= nf90_noerr) then 
      print *, "Error closing new file"
      stop 2
    end if  
    print *, "finished!"

  end subroutine

end program