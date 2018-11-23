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

subroutine readfield_nc(iunit,istep,nhleft,itimei,ihr1,ihr2, &
  itimefi,ierror)

!  Purpose:  Read fields from NetCDF files. (see readfile.f for felt-files)

!  Parameters:
!             iunit      filehandle-unit (dummy)
!             istep      current timestep (always positive), negative istep means reset
!             nhleft     remaining run-hours (negative for backward-calculations)
!             itimei(5)  initial time
!             ihr1       minimal time-offset?
!             ihr2       maximal time-offset?
!             itimefi(5) final time (output)
!             ierror     error (output)

  USE particleML
  USE fileInfoML
  USE snapfilML
  USE snapfldML
  USE snapgrdML
  USE snapmetML
  USE snaptabML
  USE snapdebugML

#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

!..input/output
  integer, INTENT(INOUT) ::  iunit,istep,nhleft,ihr1,ihr2, &
  itimei(5)
  integer, INTENT(OUT) :: itimefi(5),ierror

! netcdf
  include 'netcdf.inc'

! local variables
  integer :: i, j, k, n, ilevel, ierr1, ierr2, i1, i2
  integer :: itime(5,4),ihours(4)
  integer :: ihdiff, ihdif1, ihdif2, nhdiff
  integer, save :: ncid = 0
  integer, save :: ntav1, ntav2 = 0
  character*(1024), save :: file_name = ""
  real ::     alev(nk),blev(nk),db, precip1,dxgrid,dygrid
  integer :: kk, ifb, kfb
  real ::    rcp, dred, red, p, px, dp, p1, p2,ptop
  real ::    totalprec
  real,save     ::    t2thetafac(15000)
  real ::    unitScale

  integer :: varid, retval, timepos

  if (istep < 0) then
  ! set all 'save' variables to default values,
  ! ncid not needed, will close automaticall
    ntav1 = 0
    ntav2 = 0
    return
  end if

#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('READFIELD_NC',0,ZHOOK_HANDLE)
#endif



!..get time offset in hours (as iavail(n)%oHour)
  if (nhleft < 0) then
    ihr1 = -ihr1
    ihr2 = -ihr2
  end if
  ihours(1)=ihr1
  ihours(2)=ihr2
  ihours(3)=0
  ihours(4)=nhleft
  do n=1,4
    do i=1,5
      itime(i,n)=itimei(i)
    end do
    itime(5,n)=itime(5,n)+ihours(n)
    call hrdiff(0,1,itimer(1,1),itime(1,n),ihours(n),ierr1,ierr2)
  end do
  ihdif1=ihours(1)
  ihdif2=ihours(2)

  write(9,*) '*READFIELD* Requested time: ',(itime(i,1),i=1,4)
  write(9,*) '                Time limit: ',(itime(i,2),i=1,4)
  write(9,*) '                 ihr1,ihr2: ', ihr1, ihr2


!..search in list of available timesteps with model level data
  if(ihdif1 > ihdif2) then
  !..using the backward list
    i=ihdif1
    ihdif1=ihdif2
    ihdif2=i
    kfb=2
    ifb=10
  else
    kfb=1
    ifb=9
  end if

  ntav1 = ntav2
  ntav2 = 0
  n=kavail(kfb)
  do while ((ntav2 == 0) .AND. (n > 0))
    if(iavail(n)%oHour >= ihdif1 .AND. &
    iavail(n)%oHour <= ihdif2) then
      ntav2 = n
    end if
  !..pointer to next timestep (possibly same time)
    if (ifb == 9) then
      n=iavail(n)%nAvail
    else
      n=iavail(n)%pAvail
    end if
  end do

  if(idebug == 1) then
    write(9,*) 'MODEL LEVEL SEARCH LIST.   ntav2=',ntav2
    write(9,*) 'nx,ny,nk: ',nx,ny,nk
    write(9,*) 'istep,nhleft: ',istep,nhleft
    write(9,*) 'itimei(5), ihr1, ihr2:',(itimei(i),i=1,5),ihr1,ihr2
    write(9,*) 'kfb,ifb,ihdif1,ihdif2:',kfb,ifb,ihdif1,ihdif2
    write(9,fmt='(7(1x,i4),1x,i6,2i5)') (iavail(ntav2))
    flush(9)
  end if

  if(ntav2 < 1) then
    write(9,*) '*READFIELD* No model level data available'
    write(6,*) '*READFIELD* No model level data available'
    ierror=1
#if defined(DRHOOK)
  !     before the return statement
    IF (LHOOK) CALL DR_HOOK('READFIELD_NC',1,ZHOOK_HANDLE)
#endif
    return
  end if

! time between two inputs
! open the correct file, if required
  if (file_name /= filef(iavail(ntav2)%fileNo)) then
    if (ncid /= 0) then
      call check(nf_close(ncid), "close ncid")
    end if
    file_name = filef(iavail(ntav2)%fileNo)
    call check(nf_open(file_name, NF_NOWRITE, ncid), file_name)
  end if

!     set timepos and nhdiff
  nhdiff = 3
  if (ntav1 /= 0) &
  nhdiff = abs(iavail(ntav2)%oHour - iavail(ntav1)%oHour)
  nprecip = nhdiff

  timepos = iavail(ntav2)%timePos
  itimefi(1) = iavail(ntav2)%aYear
  itimefi(2) = iavail(ntav2)%aMonth
  itimefi(3) = iavail(ntav2)%aDay
  itimefi(4) = iavail(ntav2)%aHour
  itimefi(5) = iavail(ntav2)%fcHour

  if(idebug == 1) then
    write(9,*) 'READING DATA FROM file=',trim(file_name)
    write(9,*) 'READING DATA FROM position=',timepos, ' for ', &
    itimefi
  end if




  if( .TRUE. ) then
  !..move data from input time step 2 to 1
  
    call copyfield(u2,u1,nx,ny,nk)
    call copyfield(v2,v1,nx,ny,nk)
    call copyfield(w2,w1,nx,ny,nk)
    call copyfield(t2,t1,nx,ny,nk)
    call copyfield(hlevel2,hlevel1,nx,ny,nk)
    call copyfield(hlayer2,hlayer1,nx,ny,nk)
  
    call copyfield(ps2,ps1,nx,ny,1)
    call copyfield(bl2,bl1,nx,ny,1)
    call copyfield(hbl2,hbl1,nx,ny,1)
  
    if(imslp /= 0) then
      call copyfield(pmsl2,pmsl1,nx,ny,1)
    end if
  
  end if

  do k=nk-kadd,2,-1
  
  !..input model level no.
    ilevel=klevel(k)

    call calc_2d_start_length(start4d, count4d, nx, ny, ilevel, &
    enspos, timepos, has_dummy_dim)
  
  !..u
  !     Get the varid of the data variable, based on its name.
    call nfcheckload(ncid, xwindv, start4d, count4d, u2(1,1,k))
  !        call readfd(iunit,nav,ivc,iu,ilevel,0,u2(1,1,k),ierror)
  !        if(ierror.ne.0) goto 100
  
  !..v
    call nfcheckload(ncid, ywindv, start4d, count4d, v2(1,1,k))
  !        call readfd(iunit,nav,ivc,iv,ilevel,0,v2(1,1,k),ierror)
  !        if(ierror.ne.0) goto 100
  ! bug in chernobyl borders from destaggering
    do j=1,ny
      do i=1,nx
        if (v2(i,j,k) >= 1e+30) v2(i,j,k) = 0.
      end do
    end do

  
  !..pot.temp. or abs.temp.
    call nfcheckload(ncid, pottempv, start4d, count4d, t2(1,1,k))
  !        call readfd(iunit,nav,ivc,it,ilevel,0,t2(1,1,k),ierror)
  !        if(ierror.ne.0) goto 100
  

  
  !   TOOD read ptop from file (only needed for sigma), but not in emep data
    ptop=100.
  !       if(ivcoor.eq.2) ptop=idata(19)
  !..p0 for hybrid loaded to ptop, ap is a * p0
    if (ivcoor /= 2 .AND. .NOT. ptopv == '') then
      call nfcheckload(ncid, ptopv, (/0/), (/1/), ptop)
    end if
  !..alevel (here) only for eta levels
    if ( .NOT. apv == '') then
      call nfcheckload(ncid, apv, (/ilevel/), (/1/), alev(k))
      call nfcheckload(ncid, bv, (/ilevel/), (/1/), blev(k))
      if (ivcoor /= 2 .AND. .NOT. ptopv == '') then
      !..p0 for hybrid loaded to ptop, ap is a * p0
        alev(k) = alev(k) * ptop
      end if
    ! TODO: check unit (here Pa -> hPa
      alev(k) = alev(k) / 100
    end if
    if ( .NOT. sigmav == '') then
    ! reusing blev(k) for sigma(k) later
      call nfcheckload(ncid, sigmav, (/ilevel/), (/1/), blev(k))
    end if
  
  !..sigma_dot/eta_dot (0 at surface)
  !..eta: eta_dot (or omega) stored in the same levels as u,v,th.
    if (sigmadotv == '') then
      do j=1,ny
        do i=1,nx
        ! no vertical velocity
          w2(i,j,k) = 0
        end do
      end do
    else
      call nfcheckload(ncid, sigmadotv, &
      start4d, count4d, w2(1,1,k))
    end if
  !        call readfd(iunit,nav,ivc,iw,ilevel,0,w2(1,1,k),ierror)
  !        if(ierror.ne.0) goto 100
  
  !.....end do k=nk-kadd,2,-1
  end do


!..surface pressure, 10m wind and possibly mean sea level pressure,
!..precipitation

  call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
  enspos, timepos, has_dummy_dim)

! ps
  call nfcheckload(ncid, psv, start3d, count3d, ps2(1,1))
!  input ps, must be hPa, otherwise:
  if (nctype == 'arome' .OR. nctype == 'dmi_eps' .OR. &
  nctype == 'ec_det' .OR. nctype == 'h12_grib') then
    do j=1,ny
      do i=1,nx
        ps2(i,j) = ps2(i,j) *0.01
      end do
    end do
  endif
!      call readfd(iunit,navps,ivc,8,ilevel,0,ps2(1,1),ierror)


! u10m
! v10m
  call nfcheckload(ncid, xwind10mv, start3d, count3d, u2(1,1,1))
  call nfcheckload(ncid, ywind10mv, start3d, count3d, v2(1,1,1))

!..mean sea level pressure, not used in computations,
!..(only for graphics and/or output to results file)
  if(imslp /= 0) then
    if ( .NOT. mslpv == '') then
      write(9,*) 'Mslp not found. Not important.'
      imslp=0
    else
      call nfcheckload(ncid, mslpv, start3d, count3d, pmsl2(1,1))
    !        call readfd(iunit,nav,ivc,58,ilevel,0,pmsl2(1,1),ierror)
    end if
  end if

!..precipitation......................................................

  if (precaccumv /= '') then
  !..precipitation between input time 't1' and 't2'
    if (timepos /= 1) then
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
      enspos, timepos - 1, has_dummy_dim)
      call nfcheckload(ncid, precaccumv, &
      start3d, count3d, field1(1,1))
    !         call readfd(iunit,nav,ivc,17,ilevel,ihrpr1,field1,ierror)
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
      enspos, timepos, has_dummy_dim)
      call nfcheckload(ncid, precaccumv, &
      start3d, count3d, field2(1,1))
    !         call readfd(iunit,nav,ivc,17,ilevel,ihrpr2,field2,ierror)
    !..the difference below may get negative due to different scaling
      do j=1,ny
        do i=1,nx
          precip1=max(field2(i,j)-field1(i,j),0.)/nhdiff
          do k=1,nprecip
            precip(i,j,k)=precip1
          end do
        end do
      end do
    end if
  else if (precstratiaccumv /= '') then
  ! accumulated stratiform and convective precipitation
  !..precipitation between input time 't1' and 't2'
    if (timepos /= 1) then
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
      enspos, timepos - 1, has_dummy_dim)
      call nfcheckload(ncid, precstratiaccumv, &
      start3d, count3d, field1(1,1))
      call nfcheckload(ncid, precconaccumv, &
      start3d, count3d, field2(1,1))
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
      enspos, timepos, has_dummy_dim)
      call nfcheckload(ncid, precstratiaccumv, &
      start3d, count3d, field3(1,1))
      call nfcheckload(ncid, precconaccumv, &
      start3d, count3d, field4(1,1))
    !..the difference below may get negative due to different scaling
      unitScale = 1.
      if (nctype == 'ec_det') unitScale = 1000.
      do j=1,ny
        do i=1,nx
          precip1=max(field3(i,j)+field4(i,j)- &
          (field1(i,j)+field2(i,j)),0.)/nhdiff
          do k=1,nprecip
            precip(i,j,k)=precip1 * unitScale
          end do
        end do
      end do
    else
    ! timepos eq 1, check if precipitation already present / assume dummy step 0
      call calc_2d_start_length(start3d, count3d, nx, ny, -1, &
      enspos, timepos, has_dummy_dim)
      call nfcheckload(ncid, precstratiaccumv, &
      start3d, count3d, field3(1,1))
      call nfcheckload(ncid, precconaccumv, &
      start3d, count3d, field4(1,1))
      totalprec = 0
      do j=1,ny
        do i=1,nx
          field1(i,j) = 0.
          field2(i,j) = 0.
          totalprec = totalprec + field3(i,j) + field4(i,j)
        end do
      end do
      if (totalprec > 1e-5) then
      !..the difference below may get negative due to different scaling
        write(9,*) "found precip in first timestep, assuming ", &
        "empty 0 timestep to deaccumulate precip"
        unitScale = 1.
        if (nctype == 'ec_det') unitScale = 1000.
        do j=1,ny
          do i=1,nx
            precip1=max(field3(i,j)+field4(i,j)- &
            (field1(i,j)+field2(i,j)),0.)/nhdiff
            do k=1,nprecip
              precip(i,j,k)=precip1 * unitScale
            end do
          end do
        end do
      endif
    end if
  else
  !..non-accumulated emissions in stratiform an convective
    call nfcheckload(ncid, precstrativrt, &
    start3d, count3d, field1(1,1))
    call nfcheckload(ncid, precconvrt, &
    start3d, count3d, field2(1,1))

    do j=1,ny
      do i=1,nx
      !..precipitation must be larger 0, m/s -> mm/h
        precip1=max(field1(i,j)+field2(i,j),0.)*3600*1000
        do k=1,nprecip
          precip(i,j,k)=precip1
        end do
      end do
    end do

  end if

! first time initialized data
  if(istep == 0) then
  
    do k=2,nk-kadd
      alevel(k)=alev(k)
      blevel(k)=blev(k)
    end do
  
    if(kadd > 0) then
      if(ivcoor == 2) then
      !..sigma levels ... blevel=sigma
        db=blevel(nk-kadd-1)-blevel(nk-kadd)
        db=max(db,blevel(nk-kadd)/float(kadd))
        do k=nk-kadd+1,nk
          blevel(k)=max(blevel(k-1)-db,0.)
        end do
      elseif(ivcoor == 10) then
      !..eta (hybrid) levels
        p1=alevel(nk-kadd)+blevel(nk-kadd)*1000.
        p2=alevel(nk-kadd-1)+blevel(nk-kadd-1)*1000.
        dp=p2-p1
        if(p1-dp*kadd < 10.) dp=(p1-10.)/kadd
        db=blevel(nk-kadd-1)-blevel(nk-kadd)
        db=max(db,blevel(nk-kadd)/float(kadd))
        do k=nk-kadd+1,nk
          p1=p1-dp
          blevel(k)=max(blevel(k-1)-db,0.)
          alevel(k)=p1-blevel(k)*1000.
        end do
      else
        write(6,*) 'PROGRAM ERROR.  ivcoor= ',ivcoor
        stop 255
      end if
    end if
  
    if(ivcoor == 2) then
    !..sigma levels (norlam)
      do k=2,nk
        alevel(k)=ptop*(1.-blevel(k))
      end do
    end if
  
  !..surface
    alevel(1)=0.
    blevel(1)=1.
  
    if(ivcoor == 2) then
    !..sigma levels ... vlevel=sigma
      do k=1,nk
        vlevel(k)=blevel(k)
      end do
    elseif(ivcoor == 10) then
    !..eta (hybrid) levels ... vlevel=eta (eta as defined in Hirlam)
      do k=1,nk
        vlevel(k)=alevel(k)/1013.26 + blevel(k)
      end do
    else
      write(6,*) 'PROGRAM ERROR.  ivcoor= ',ivcoor
      stop 255
    end if
  
  !..half levels where height is found,
  !..alevel and blevel are in the middle of each layer
    ahalf(1)=alevel(1)
    bhalf(1)=blevel(1)
    vhalf(1)=vlevel(1)
  !..check if subselection of levels
    do k=2,nk-1
      if (klevel(k+1) /= klevel(k)-1) then
        manual_level_selection = .TRUE. 
      endif
    end do
    do k=2,nk-1
      if ( .NOT. manual_level_selection) then
        ahalf(k)=alevel(k)+(alevel(k)-ahalf(k-1))
        bhalf(k)=blevel(k)+(blevel(k)-bhalf(k-1))
        vhalf(k)=ahalf(k)/1013.26+bhalf(k)
      else
        ahalf(k)=(alevel(k)+alevel(k+1))*0.5
        bhalf(k)=(blevel(k)+blevel(k+1))*0.5
        vhalf(k)=ahalf(k)/1013.26+bhalf(k)
      end if
    end do
    ahalf(nk)=alevel(nk)
    bhalf(nk)=blevel(nk)
    vhalf(nk)=vlevel(nk)

  !..compute map ratio
    call mapfield(1,0,igtype,gparam,nx,ny,xm,ym,0., &
    dxgrid,dygrid,ierror)
    if(ierror /= 0) then
      write(9,*) 'MAPFIELD ERROR. ierror= ',ierror
      write(6,*) 'MAPFIELD ERROR. ierror= ',ierror
      stop 255
    end if
    gparam(7)=dxgrid
    gparam(8)=dygrid
  !..size of each grid square (m**2)
    do j=1,ny
      do i=1,nx
        garea(i,j)=(dxgrid/xm(i,j))*(dygrid/ym(i,j))
        dgarea(i,j)=dble(garea(i,j))
      end do
    end do

    if (temp_is_abs) then
      rcp=r/cp
    ! create precomputed table for pressures between 0.1 and 1500hPa
      do i=1,15000
        t2thetafac(i) = 1./((real(i)/10.*0.001)**rcp)
      end do
    end if

  ! end initialization
  end if

  if (temp_is_abs) then
  !..abs.temp. -> pot.temp.
    do k=2,nk-kadd
      do j=1,ny
        do i=1,nx
          p=alevel(k)+blevel(k)*ps2(i,j)
        ! t2thetafac is 50% faster, and less then 0.5% difference in theta
          t2(i,j,k)=t2(i,j,k) * t2thetafac(nint(p*10.+.5))
        ! 2(i,j,k)=t2(i,j,k)/((p*0.001)**rcp)
        end do
      end do
    end do
  end if

  if (sigmadot_is_omega) then
  !..omega -> etadot, or rather etadot derived from continuity-equation (mean of both)
    call om2edot
  else if (sigmadotv == '') then
  !..omega -> etadot, or rather etadot derived from continuity-equation
    call om2edot
  ! om2edot take means of omega (=0) and continuity-equation, -> use only continuity equation
    do k=1,nk
      do j=1,ny
        do i=1,nx
          w2(i,j,k)=2.*w2(i,j,k)
        end do
      end do
    end do
  end if

!..sigma_dot/eta_dot 0 at surface
  do j=1,ny
    do i=1,nx
      w2(i,j,1)=0.
    end do
  end do

!..no temperature at or near surface (not used, yet)
  do j=1,ny
    do i=1,nx
      t2(i,j,1)=-999.
    end do
  end do
  if(kadd > 0) then
  !..levels added at the top
    dred=0.5/float(kadd)
    red=1.
    kk=nk-kadd
    do k=nk-kadd+1,nk
      red=red-dred
      do j=1,ny
        do i=1,nx
          u2(i,j,k)=u2(i,j,kk)
          v2(i,j,k)=v2(i,j,kk)
          w2(i,j,k)=w2(i,j,kk)*red
          t2(i,j,k)=t2(i,j,kk)
        end do
      end do
    end do
  end if

  if(nhleft < 0) then
  ! backward-calculation, switch sign of winds
    do k=1,nk
      do j=1,ny
        do i=1,nx
          u2(i,j,k)=-u2(i,j,k)
          v2(i,j,k)=-v2(i,j,k)
          w2(i,j,k)=-w2(i,j,k)
        end do
      end do
    end do
  end if


! test---------------------------------------------------------------
  write(9,*) 'k,k_model,alevel,blevel,vlevel,p,dp:'
  px=alevel(nk)+blevel(nk)*1000.
  do k=nk,1,-1
    p=alevel(k)+blevel(k)*1000.
    write(9,fmt='(1x,2i5,f9.2,2f9.5,f8.0,f6.0)') &
    k,klevel(k),alevel(k),blevel(k),vlevel(k),p,p-px
    px=p
  end do

! test---------------------------------------------------------------

  if(idebug == 1) then
    call ftest('u  ',nk,1,nx,ny,nk,   u2,0)
    call ftest('v  ',nk,1,nx,ny,nk,   v2,0)
    call ftest('w  ',nk,1,nx,ny,nk,   w2,0)
    call ftest('t  ',nk,1,nx,ny,nk,   t2,0)
    call ftest('ps ',1, 1,nx,ny, 1,  ps2,0)
    if (istep > 0) &
    call ftest('pre',1,nprecip,nx,ny,nprecip,precip,0)
  end if

! close file
!      call check(nf_close(ncid), "")

  if (istep == 0) then
  ! test---------------------------------------------------------------
    write(9,*) 'k,ahalf,bhalf,vhalf,p,dp:'
    px=ahalf(nk)+bhalf(nk)*1000.
    do k=nk,1,-1
      p=ahalf(k)+bhalf(k)*1000.
      write(9,fmt='(1x,i5,f9.2,2f9.5,f8.0,f6.0)') &
      k,ahalf(k),bhalf(k),vhalf(k),p,p-px
      px=p
    end do
  ! test---------------------------------------------------------------
  
  !..level table for (vertical) interpolation
  !..(remember that fields are stored bottom to top
  !.. and that all parameters now are in the same levels)
    write(9,*) 'ivlevel:'
    write(9,*) 'k,i1,i2,vlevel(k+1),vlevel(k)'
    i2=-1
    do k=nk-1,1,-1
      i1=i2+1
      i2=vlevel(k)*10000.
      if(k == 1) i2=10000
      do i=i1,i2
        ivlevel(i)=k
      end do
      write(9,*) k,i1,i2,vlevel(k+1),vlevel(k)
    end do
  
  !..level table for concentration in each sigma/eta layer
  !..(layers here as in the input model, no '10m' layer,
  !.. but ordering bottom to top, reorder at time of output)
    write(9,*) 'ivlayer:'
    write(9,*) 'k,i1,i2,vhalf(k+1),vhalf(k)'
    i2=-1
    do k=nk-1,1,-1
      i1=i2+1
      i2=nint(vhalf(k)*10000.)
      if(k == 1) i2=10000
      do i=i1,i2
        ivlayer(i)=k
      end do
      write(9,*) k,i1,i2,vhalf(k+1),vhalf(k)
    end do
  end if

#if defined(DRHOOK)
! before the return statement
  IF (LHOOK) CALL DR_HOOK('READFIELD_NC',1,ZHOOK_HANDLE)
#endif
  return



end subroutine readfield_nc


subroutine calc_2d_start_length(start, length, nx, ny, zpos, &
  enspos, tpos, has_2d_dummy_height)
! calculate the start and length paramters for slicing
! a 2d field from a 3-5d dataset
  integer, intent (inout) :: start(7), length(7)
  integer, intent (in) :: nx, ny, zpos, enspos, tpos
  logical, intent (in) :: has_2d_dummy_height
  integer :: pos

  do pos = 1, 7
    start(pos) = 1
    length(pos) = 1
  end do

  length(1) = nx
  length(2) = ny

  pos = 2
! ensemble enspos given in C notation, e.g. first member = 0
  if (enspos >= 0) then
    pos = pos + 1
    start(pos) = enspos+1
  end if
! z
  if (zpos >= 1) then
    pos = pos + 1
    start(pos) = zpos
  elseif (has_2d_dummy_height) then
    pos = pos + 1
  end if
! time
  pos = pos + 1
  start(pos) = tpos

end subroutine calc_2d_start_length


subroutine check(status, errmsg)
  implicit none
! netcdf
  include 'netcdf.inc'
  integer, intent ( in) :: status
  character(len=*), intent(in) :: errmsg

  if(status /= nf_noerr) then
    print *, trim(nf_strerror(status)), ": ", trim(errmsg)
    call exit(1)
  endif
end subroutine check

subroutine nfcheckload(ncid, varname, start, length, field)
  use, intrinsic :: IEEE_ARITHMETIC
  implicit none
  include 'netcdf.inc'
  integer, intent (in) :: ncid, start(*), length(*)
  character(len=*), intent(in) :: varname
  real(kind=4), intent (inout) :: field(*)

  real(kind=4) factor, offset, fillvalue, nan
  integer :: varid, attlen, atttype, status, ndims, siz, i

!      write(*,*) "inq ", varname
  call check(nf_inq_varid(ncid, varname, varid), varname)
!      write(*,*) "get ", varname
  call check(nf_get_vara_real(ncid, varid, start, length, &
  field), varname)

! calculate the field size
  call check(nf_inq_varndims(ncid, varid,ndims), "dims"//varname)
  siz=1
  do i =1,ndims
    siz = siz * length(i)
  end do

! translate fill-value to IEEE undefined
  status = NF_GET_ATT_REAL(ncid, varid, "_FillValue", fillvalue)

  if (status == NF_NOERR) then
    nan = IEEE_VALUE(nan, IEEE_QUIET_NAN)
    call check(nf_inq_varndims(ncid, varid,ndims), "dims"//varname)
    do i = 1, siz
      if (field(i) == fillvalue) then
        field(i) = nan
      end if
    end do
  end if
!  check add_offset and scale_factor
  factor = 1.
  offset = 0.
  status = nf_inq_att(ncid, varid, "scale_factor", attlen, atttype)
  if (status == nf_noerr) then
    call check(nf_get_att_real(ncid, varid, "scale_factor",factor), &
    "scale_factor "//varname)
  end if
  status = nf_inq_att(ncid, varid, "add_offset", attlen, atttype)
  if (status == nf_noerr) then
    call check(nf_get_att_real(ncid, varid, "add_offset", offset), &
    "add_offset "//varname)
  end if
  if (factor /= 1. .OR. offset /= 0.) then
  !        write(*,*) "scaling:", varname, factor, offset, siz
    do i = 1, siz
      field(i) = field(i)*factor + offset
    end do
  end if


end subroutine nfcheckload
