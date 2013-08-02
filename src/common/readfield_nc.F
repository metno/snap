      subroutine readfield_nc(iunit,istep,nhleft,itimei,ihr1,ihr2,
     +                  itimefi,ierror)
!
!  Purpose:  Read fields from NetCDF files. (see readfile.f for felt-files)
!
!  Parameters:
!             iunit      filehandle-unit (dummy)
!             istep      current timestep
!             nhleft     remaining run-hours
!             itimei(5)  initial time
!             ihr1       minimal time-offset?
!             ihr2       maximal time-offset?
!             itimefi(5) final time (output)
!             ierror     error (output)
!

#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
!
!..input/output
      integer, INTENT(IN) ::  iunit,istep,nhleft,ihr1,ihr2,
     +                           itimei(5)
      integer, INTENT(OUT) :: itimefi(5),ierror

      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snaptab.inc'
      include 'snapdebug.inc'
! netcdf
      include 'netcdf.inc'
!
! local variables
      integer i, j, k, n, ilevel, ierr1, ierr2, i1, i2
      integer itime(5,4),ihours(4)
      integer ihdiff, ihdif1, ihdif2
      integer, save :: timepos
      real     alev(nk),blev(nk),db, precip1,dxgrid,dygrid
      integer kk
      real    dred, red, p, px, dp, p1, p2,ptop

      integer ncid, varid, retval
      integer start3d(3), start4d(4), count3d(3), count4d(4)
      character*(*) file_name
      parameter (file_name='/disk1/myopdata/hirlam12/h12all00.nc')

#if defined(DRHOOK)
! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('READFIELD_NC',0,ZHOOK_HANDLE)
#endif

      call check(nf_open(file_name, NF_NOWRITE, ncid), file_name)


!..get time offset in hours (as iavail(8,n))
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
!

!     TODO: set timepos to something useful
      ihdif1 = 6
      nprecip = 6
      if (istep .eq. 0) then
        timepos = 2
      else
        timepos = timepos + 1
      end if
      do n=1,4
        itimefi(n) = itimei(n)
      end do
      itimefi(5) = -12 + timepos*6
      call vtime(itimefi, ierror)

!

      write(9,*) '*READFIELD_NC* Requested time: ',(itime(i,1),i=1,4)
      write(9,*) '                Time limit: ',(itime(i,2),i=1,4)
!
      if(.true.) then
!..move data from input time step 2 to 1
!
       call copyfield(u2,u1,nx,ny,nk)
       call copyfield(v2,v1,nx,ny,nk)
       call copyfield(w2,w1,nx,ny,nk)
       call copyfield(t2,t1,nx,ny,nk)
       call copyfield(hlevel2,hlevel1,nx,ny,nk)
       call copyfield(hlayer2,hlayer1,nx,ny,nk)
!
       call copyfield(ps2,ps1,nx,ny,1)
       call copyfield(bl2,bl1,nx,ny,1)
       call copyfield(hbl2,hbl1,nx,ny,1)
!
        if(nxad.eq.nx .and. nyad.eq.ny .and. imslp.ne.0) then
         call copyfield(pmsl2,pmsl1,nx,ny,1)
        end if
!
      end if

      do k=nk-kadd,2,-1
c
c..input model level no.
        ilevel=klevel(k)

        start4d(1) = 1
        start4d(2) = 1
        start4d(3) = ilevel
        start4d(4) = timepos
        count4d(1) = nx
        count4d(2) = ny
        count4d(3) = 1
        count4d(4) = 1
c
c..u
!     Get the varid of the data variable, based on its name.
        call check(nf_inq_varid(ncid, 'x_wind_ml', varid), 'x_wind_ml')
        call check(nf_get_vara_real(ncid, varid, start4d, count4d,
     +             u2(1,1,k)), 'x_wind_ml')
!        call readfd(iunit,nav,ivc,iu,ilevel,0,u2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
!
!..v
        call check(nf_inq_varid(ncid, 'y_wind_ml', varid), 'y_wind_ml')
        call check(nf_get_vara_real(ncid, varid, start4d, count4d,
     +             v2(1,1,k)), 'y_wind_ml')
!        call readfd(iunit,nav,ivc,iv,ilevel,0,v2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
c
c..pot.temp. or abs.temp.
        call check(nf_inq_varid(ncid, 'air_potential_temperature_ml',
     +                           varid), 'air_potential_temperature_ml')
        call check(nf_get_vara_real(ncid, varid, start4d, count4d,
     +             t2(1,1,k)), 'air_potential_temperature_ml')
!        call readfd(iunit,nav,ivc,it,ilevel,0,t2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
c
c..alevel (here) only for eta levels
        call check(nf_inq_varid(ncid, 'ap', varid), 'ap')
        call check(nf_get_vara_real(ncid, varid, ilevel, 1,
     +             alev(k)), 'ap')
c TODO: check unit (here Pa -> hPa
        alev(k) = alev(k) / 100
        call check(nf_inq_varid(ncid, 'b', varid), 'b')
        call check(nf_get_vara_real(ncid, varid, ilevel, 1,
     +             blev(k)), 'b')
c
c   TOOD read ptop from file (only needed for sigma)
c..ptop for sigma (Norlam)
      ptop=0.
      if(ivcoor.eq.2) ptop=idata(19)

c
c..sigma_dot/eta_dot (0 at surface)
c..eta: eta_dot (or omega) stored in the same levels as u,v,th.
        call check(nf_inq_varid(ncid, 'sigma_dot_ml', varid),
     +              'sigma_dot_ml')
        call check(nf_get_vara_real(ncid, varid, start4d, count4d,
     +             w2(1,1,k)), 'sigma_dot_ml')
!        call readfd(iunit,nav,ivc,iw,ilevel,0,w2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
c
c.....end do k=nk-kadd,2,-1
      end do
c
c
c..surface pressure, 10m wind and possibly mean sea level pressure,
c..precipitation
c
      start3d(1) = 1
      start3d(2) = 1
      start3d(3) = timepos
      count3d(1) = nx
      count3d(2) = ny
      count3d(3) = 1

c ps
      call check(nf_inq_varid(ncid, 'surface_air_pressure', varid),
     +            'surface_air_pressure')
      call check(nf_get_vara_real(ncid, varid, start3d, count3d,
     +             ps2(1,1)), 'surface_air_pressure')
c TODO: something wrong with input ps, should be hPa, but is 10*hPa
      do j=1,ny
        do i=1,nx
          ps2(i,j) = ps2(i,j)/10
        end do
      end do
c      call readfd(iunit,navps,ivc,8,ilevel,0,ps2(1,1),ierror)

c u10m
      call check(nf_inq_varid(ncid, 'x_wind_10m', varid), 'x_wind_10m')
      call check(nf_get_vara_real(ncid, varid, start3d, count3d,
     +             u2(1,1,1)), 'x_wind_10m')
c      call readfd(iunit,nav,ivc,33,ilevel,0,u2(1,1,1),ierror)
c v10m
      call check(nf_inq_varid(ncid, 'y_wind_10m', varid),'y_wind_10m')
      call check(nf_get_vara_real(ncid, varid, start3d, count3d,
     +             v2(1,1,1)), 'y_wind_10m')
c      call readfd(iunit,nav,ivc,34,ilevel,0,v2(1,1,1),ierror)

c..mean sea level pressure, not used in computations,
c..(only for graphics and/or output to results file)
      if(imslp.ne.0) then
        retval = nf_inq_varid(ncid, 'air_pressure_at_sea_level', varid)
        if (retval .eq. nf_noerr) then
          call check(nf_get_vara_real(ncid, varid, start3d, count3d,
     +                 pmsl2(1,1)), 'air_pressure_at_sea_level')
c        call readfd(iunit,nav,ivc,58,ilevel,0,pmsl2(1,1),ierror)
        else
          write(9,*) 'Mslp not found. Not important.'
c..stop input of mslp at later timesteps
          imslp=0
        end if
      end if

c..precipitation......................................................
c
c..precipitation between input time 't1' and 't2'
      if (timepos .ne. 1) then
        call check(nf_inq_varid(ncid, 'precipitation_amount_acc',
     +               varid), 'precipitation_amount_acc')
        start3d(3) = timepos - 1
        call check(nf_get_vara_real(ncid, varid, start3d, count3d,
     +               field1(1,1)), 'precipitation_amount_acc')
c       call readfd(iunit,nav,ivc,17,ilevel,ihrpr1,field1,ierror)
        start3d(3) = timepos
        call check(nf_get_vara_real(ncid, varid, start3d, count3d,
     +               field2(1,1)), 'precipitation_amount_acc')
c       call readfd(iunit,nav,ivc,17,ilevel,ihrpr2,field2,ierror)
c..the difference below may get negative due to different scaling
        do j=1,ny
          do i=1,nx
            precip1=max(field2(i,j)-field1(i,j),0.)/ihdif1
            do k=1,nprecip
              precip(i,j,k)=precip1
            end do
          end do
        end do
      end if

c first time initialized data
      if(istep.eq.0) then
c
        do k=2,nk-kadd
          alevel(k)=alev(k)
          blevel(k)=blev(k)
        end do
c
        if(kadd.gt.0) then
          if(ivcoor.eq.2) then
c..sigma levels ... blevel=sigma
            db=blevel(nk-kadd-1)-blevel(nk-kadd)
            db=max(db,blevel(nk-kadd)/float(kadd))
            do k=nk-kadd+1,nk
              blevel(k)=max(blevel(k-1)-db,0.)
            end do
          elseif(ivcoor.eq.10) then
c..eta (hybrid) levels
            p1=alevel(nk-kadd)+blevel(nk-kadd)*1000.
            p2=alevel(nk-kadd-1)+blevel(nk-kadd-1)*1000.
            dp=p2-p1
            if(p1-dp*kadd.lt.10.) dp=(p1-10.)/kadd
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
c
        if(ivcoor.eq.2) then
c..sigma levels (norlam)
          do k=2,nk
            alevel(k)=ptop*(1.-blevel(k))
          end do
        end if
c
c..surface
        alevel(1)=0.
        blevel(1)=1.
c
        if(ivcoor.eq.2) then
c..sigma levels ... vlevel=sigma
          do k=1,nk
            vlevel(k)=blevel(k)
          end do
        elseif(ivcoor.eq.10) then
c..eta (hybrid) levels ... vlevel=eta (eta as defined in Hirlam)
          do k=1,nk
            vlevel(k)=alevel(k)/1013.26 + blevel(k)
          end do
        else
          write(6,*) 'PROGRAM ERROR.  ivcoor= ',ivcoor
          stop 255
        end if
c
c..half levels where height is found,
c..alevel and blevel are in the middle of each layer
        ahalf(1)=alevel(1)
        bhalf(1)=blevel(1)
        vhalf(1)=vlevel(1)
c..is the following the best we can do ???
        do k=2,nk-1
          if(klevel(k+1).eq.klevel(k)-1) then
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
c
c..get grid parameters from field identification
!     TODO: get grid parameters from file
c..get grid parameters from field identification
!      call gridpar(+1,ldata,idata,igtype,ix,iy,gparam,ierror)
        igtype = 3
        gparam(1) = -46.400002
        gparam(2) = -36.400002
        gparam(3) = 0.10800000
        gparam(4) = 0.10800000
        gparam(5) = 0.0000000
        gparam(6) = 65.000000
c..compute map ratio
        call mapfield(1,0,igtype,gparam,nx,ny,xm,ym,0.,
     +                dxgrid,dygrid,ierror)
        if(ierror.ne.0) then
          write(9,*) 'MAPFIELD ERROR. ierror= ',ierror
          write(6,*) 'MAPFIELD ERROR. ierror= ',ierror
          stop 255
        end if
        gparam(7)=dxgrid
        gparam(8)=dygrid
c..size of each grid square (m**2)
        do j=1,ny
          do i=1,nx
            garea(i,j)=(dxgrid/xm(i,j))*(dygrid/ym(i,j))
            dgarea(i,j)=dble(garea(i,j))
          end do
        end do

c
      end if
c


c..sigma_dot/eta_dot 0 at surface
      do j=1,ny
        do i=1,nx
          w2(i,j,1)=0.
        end do
      end do

c..no temperature at or near surface (not used, yet)
      do j=1,ny
        do i=1,nx
          t2(i,j,1)=-999.
        end do
      end do
      if(kadd.gt.0) then
c..levels added at the top
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
c
c-test---------------------------------------------------------------
      write(9,*) 'k,k_model,alevel,blevel,vlevel,p,dp:'
      px=alevel(nk)+blevel(nk)*1000.
      do k=nk,1,-1
        p=alevel(k)+blevel(k)*1000.
        write(9,fmt='(1x,2i5,f9.2,2f9.5,f8.0,f6.0)')
     +          k,klevel(k),alevel(k),blevel(k),vlevel(k),p,p-px
        px=p
      end do
c-test---------------------------------------------------------------
c
      if(idebug.eq.1) then
        call ftest('u  ',nk,1,nx,ny,nk,   u2,0)
        call ftest('v  ',nk,1,nx,ny,nk,   v2,0)
        call ftest('w  ',nk,1,nx,ny,nk,   w2,0)
        call ftest('t  ',nk,1,nx,ny,nk,   t2,0)
        call ftest('ps ',1, 1,nx,ny, 1,  ps2,0)
        if (istep.gt.0)
     +    call ftest('pre',1,nprecip,nx,ny,nprecip,precip,0)
      end if
c
c close file
      call check(nf_close(ncid), "")

      if (istep .eq. 0) then
c-test---------------------------------------------------------------
        write(9,*) 'k,ahalf,bhalf,vhalf,p,dp:'
        px=ahalf(nk)+bhalf(nk)*1000.
        do k=nk,1,-1
          p=ahalf(k)+bhalf(k)*1000.
          write(9,fmt='(1x,i5,f9.2,2f9.5,f8.0,f6.0)')
     +            k,ahalf(k),bhalf(k),vhalf(k),p,p-px
          px=p
        end do
c-test---------------------------------------------------------------
c
c..level table for (vertical) interpolation
c..(remember that fields are stored bottom to top
c.. and that all parameters now are in the same levels)
        write(9,*) 'ivlevel:'
        write(9,*) 'k,i1,i2,vlevel(k+1),vlevel(k)'
        i2=-1
        do k=nk-1,1,-1
          i1=i2+1
          i2=vlevel(k)*10000.
          if(k.eq.1) i2=10000
          do i=i1,i2
            ivlevel(i)=k
          end do
          write(9,*) k,i1,i2,vlevel(k+1),vlevel(k)
        end do
c
c..level table for concentration in each sigma/eta layer
c..(layers here as in the input model, no '10m' layer,
c.. but ordering bottom to top, reorder at time of output)
        write(9,*) 'ivlayer:'
        write(9,*) 'k,i1,i2,vhalf(k+1),vhalf(k)'
        i2=-1
        do k=nk-1,1,-1
          i1=i2+1
          i2=nint(vhalf(k)*10000.)
          if(k.eq.1) i2=10000
          do i=i1,i2
            ivlayer(i)=k
          end do
          write(9,*) k,i1,i2,vhalf(k+1),vhalf(k)
        end do
      end if
c
#if defined(DRHOOK)
! before the return statement
      IF (LHOOK) CALL DR_HOOK('READFIELD_NC',1,ZHOOK_HANDLE)
#endif
      return



      end subroutine readfield_nc


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
