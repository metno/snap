      subroutine readfield_nc(iunit,istep,nhleft,itimei,ihr1,ihr2,
     +                  itimefi,ierror)
!
!  Purpose:  Read fields from NetCDF files. (see readfile.f for felt-files)
!
!  Parameters:
!             iunit      filehandle-unit (dummy)
!             istep      current timestep (always positive)
!             nhleft     remaining run-hours (negative for backward-calculations)
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
      integer, INTENT(INOUT) ::  iunit,istep,nhleft,ihr1,ihr2,
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
      integer ihdiff, ihdif1, ihdif2, nhdiff
      integer, save :: ncid = 0
      integer, save :: ntav1, ntav2 = 0
      character*(1024), save :: file_name = ""
      real     alev(nk),blev(nk),db, precip1,dxgrid,dygrid
      real     astatic(65), bstatic(65)
      integer kk, ifb, kfb
      real    rcp, dred, red, p, px, dp, p1, p2,ptop

      integer varid, retval, timepos
      integer start3d(3), start4d(4), count3d(3), count4d(4)
      character*(80) xwindv,ywindv,xwind10mv,ywind10mv,pottempv,ptopv
      character*(80) sigmadotv,apv,bv,sigmav,psv,mslpv,precaccumv
      character*(80) precstrativrt,precconvrt
      logical temp_is_abs, has_dummy_dim


#if defined(DRHOOK)
! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('READFIELD_NC',0,ZHOOK_HANDLE)
#endif

      astatic = (/1000., 3000.1064453125, 5001.1552734375,
     & 6956.677734375, 8772.134765625,
     & 10401.19140625, 11845.974609375, 13108.7890625, 14192.05859375,
     & 15098.3046875, 15831.09375, 16393.41015625, 16786.13671875,
     & 17011.248046875, 17073.814453125, 16981.732421875, 16745.3867187,
     & 16377.24609375, 15891.423828125, 15303.205078125, 14628.5859375,
     & 13883.833984375, 13085.064453125, 12247.884765625, 11387.0859375,
     & 10516.384765625, 9648.2421875, 8793.732421875, 7962.462890625,
     & 7162.55078125, 6400.638671875, 5681.939453125, 5010.318359375,
     & 4388.38745117188, 3817.61083984375, 3298.43225097656, 2830.3980,
     & 2412.28137207031, 2042.20617675781, 1717.76794433594, 1436.1484,
     & 1193.76184082031, 986.16552734375, 809.11083984375, 658.9615478,
     & 532.382568359375, 426.338134765625, 338.084106445312, 265.156356,
     & 205.356636047363, 156.736114501953, 117.57788848877, 86.37949371,
     & 61.8351898193359, 42.8186264038086, 28.3661956787109, 17.6612434,
     & 10.0193367004395, 4.87473726272583, 1.76825225353241, 0.33672177,
     & 0., 0., 0., 0. /)
      bstatic =  (/ 0., 0., 0., 0.000477339955978096, 0.00239018991123,
     & 0.00622448418289423,
     & 0.0119905434548855, 0.0196991395205259, 0.0293618012219667,
     & 0.0409907661378384, 0.054624704644084, 0.0703164525330067,
     & 0.0880625545978546, 0.107799530029297, 0.129433423280716,
     & 0.152842253446579, 0.177879512310028, 0.204378187656403,
     & 0.232155054807663, 0.261015236377716, 0.290756821632385,
     & 0.321175336837769, 0.352067738771439, 0.383236020803452,
     & 0.414490222930908, 0.445650935173035, 0.476551234722137,
     & 0.507037848234177, 0.53697195649147, 0.566229432821274,
     & 0.594700753688812, 0.6222904920578, 0.648916512727737, 0.6745090,
     & 0.699009656906128, 0.722369998693466, 0.74455052614212,
     & 0.765519380569458, 0.78525111079216, 0.80372542142868, 0.8209261,
     & 0.836873173713684, 0.851647526025772, 0.865336537361145,
     & 0.878005385398865, 0.889716595411301, 0.90053004026413,
     & 0.910503029823303, 0.91969045996666, 0.928144991397858,
     & 0.935917228460312, 0.943055868148804, 0.949608027935028,
     & 0.955619394779205, 0.96113446354866, 0.966196745634079, 0.970849,
     & 0.975133717060089, 0.97909289598465, 0.982768833637238,
     & 0.986204296350479, 0.989445954561234, 0.992544859647751,
     & 0.995552182197571, 0.998519629240036 /)

      temp_is_abs = .false.
      has_dummy_dim = .false.
      if (nctype.eq.'h12') then
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
        pottempv = 'air_potential_temperature_ml'
        sigmav = ''
        ptopv = ''
        apv = 'ap'
        bv = 'b'
        sigmadotv = 'sigma_dot_ml'
        psv = 'surface_air_pressure'
        mslpv = 'air_pressure_at_sea_level'
        precaccumv = 'precipitation_amount_acc'
        precstrativrt = ''
        precconvrt = ''
c..get grid parameters from field identification

      else if (nctype.eq.'arome') then
        has_dummy_dim = .true.
        xwindv = 'x_wind_ml'
        ywindv = 'y_wind_ml'
        xwind10mv = 'x_wind_10m'
        ywind10mv = 'y_wind_10m'
c       !! real temperature, convert to pot-temp later
        pottempv = 'air_temperature_ml'
        temp_is_abs = .true.
        sigmav = ''
        ptopv = ''
c this is a bug in the arome test-file, apv and bv are therefore
c hardcoded, see below
        apv = 'arome'
        bv = 'arome'
c upward_air_velocity_ml, not used yet?
        sigmadotv = ''
        psv = 'surface_air_pressure'
        mslpv = 'air_pressure_at_sea_level'
        precaccumv = 'precipitation_amount_acc'
        precstrativrt = ''
        precconvrt = ''
c..get grid parameters from field identification

      else if (nctype.eq.'emep') then
        xwindv = 'u_wind'
        ywindv = 'v_wind'
        xwind10mv = 'u10'
        ywind10mv = 'v10'
        pottempv = 'potential_temperature'
        sigmav = 'k'
        ptopv = ''
        apv = ''
        bv = ''
        sigmadotv = ''
        psv = 'surface_pressure'
        mslpv = 'air_pressure_at_sea_level'
        precaccumv = 'precipitation_amount_acc'
!.. non accumulated precipitation rates in m/s
        precstrativrt = 'large_scale_precipitations'
        precconvrt = 'convective_precipitations'
c..get grid parameters from field identification
c set as long as sortfield still is called
        ivcoor = 2
      else
        write(*,*) "undefined grid.nctype: ", nctype
        call exit(1)
      end if


!..get time offset in hours (as iavail(8,n))
      if (nhleft .lt. 0) then
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

!
c..search in list of available timesteps with model level data
      if(ihdif1.gt.ihdif2) then
c..using the backward list
        i=ihdif1
        ihdif1=ihdif2
        ihdif2=i
        kfb=2
        ifb=10
      else
        kfb=1
        ifb=9
      end if
c
      ntav1 = ntav2
      ntav2 = 0
      n=kavail(kfb)
      do while ((ntav2.eq.0) .and. (n.gt.0))
        if(iavail(8,n).ge.ihdif1 .and. iavail(8,n).le.ihdif2) then
          ntav2 = n
        end if
c..pointer to next timestep (possibly same time)
        n=iavail(ifb,n)
      end do
c
      if(idebug.eq.1) then
        write(9,*) 'MODEL LEVEL SEARCH LIST.   ntav2=',ntav2
        write(9,*) 'nx,ny,nk: ',nx,ny,nk
        write(9,*) 'istep,nhleft: ',istep,nhleft
        write(9,*) 'itimei(5), ihr1, ihr2:',(itimei(i),i=1,5),ihr1,ihr2
        write(9,*) 'kfb,ifb,ihdif1,ihdif2:',kfb,ifb,ihdif1,ihdif2
        write(9,fmt='(7(1x,i4),1x,i6,2i5)') (iavail(i,ntav2),i=1,10)
        flush(9)
      end if
c
      if(ntav2.lt.1) then
        write(9,*) '*READFIELD* No model level data available'
        write(6,*) '*READFIELD* No model level data available'
        ierror=1
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('READFIELD_NC',1,ZHOOK_HANDLE)
#endif
        return
      end if

! time between two inputs
! open the correct file, if required
      if (file_name .ne. filef(iavail(6, ntav2))) then
        if (ncid.ne.0) then
          call check(nf_close(ncid), "close ncid")
        end if
        file_name = filef(iavail(6, ntav2))
        call check(nf_open(file_name, NF_NOWRITE, ncid), file_name)
      end if

!     set timepos and nhdiff
      nhdiff = 3
      if (ntav1.ne.0) nhdiff = abs(iavail(8, ntav2) - iavail(8, ntav1))
      nprecip = nhdiff
      timepos = iavail(7, ntav2)
      do n=1,5
        itimefi(n) = iavail(n, ntav2)
      end do


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
        if(imslp.ne.0) then
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
        call nfcheckload(ncid, xwindv, start4d, count4d, u2(1,1,k))
!        call readfd(iunit,nav,ivc,iu,ilevel,0,u2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
!
!..v
        call nfcheckload(ncid, ywindv, start4d, count4d, v2(1,1,k))
!        call readfd(iunit,nav,ivc,iv,ilevel,0,v2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
c
c..pot.temp. or abs.temp.
        call nfcheckload(ncid, pottempv, start4d, count4d, t2(1,1,k))
!        call readfd(iunit,nav,ivc,it,ilevel,0,t2(1,1,k),ierror)
!        if(ierror.ne.0) goto 100
c

c..alevel (here) only for eta levels
        if (.not.apv.eq.'') then
          if (apv.eq.'arome') then
            alev(k) = astatic(ilevel)
            blev(k) = bstatic(ilevel)
          else
            call nfcheckload(ncid, apv, (/ilevel/), (/1/), alev(k))
            call nfcheckload(ncid, bv, (/ilevel/), (/1/), blev(k))
          end if
c TODO: check unit (here Pa -> hPa
          alev(k) = alev(k) / 100
        end if
        if (.not.sigmav.eq.'') then
c reusing blev(k) for sigma(k) later
          call nfcheckload(ncid, sigmav, (/ilevel/), (/1/), blev(k))
        end if
c
c   TOOD read ptop from file (only needed for sigma), but not in emep data
       ptop=100.
c      if(ivcoor.eq.2) ptop=idata(19)
c
c..sigma_dot/eta_dot (0 at surface)
c..eta: eta_dot (or omega) stored in the same levels as u,v,th.
        if (sigmadotv.eq.'') then
          do j=1,ny
            do i=1,nx
c no vertical velocity
              w2(i,j,k) = 0
            end do
          end do
        else
          call nfcheckload(ncid, sigmadotv,
     +                     start4d, count4d, w2(1,1,k))
        end if
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

      if (has_dummy_dim) then
        start4d(1) = 1
        start4d(2) = 1
        start4d(3) = 1
        start4d(4) = timepos
        count4d(1) = nx
        count4d(2) = ny
        count4d(3) = 1
        count4d(4) = 1
      end if

c ps
      if (has_dummy_dim) then
       call nfcheckload(ncid, psv, start4d, count4d, ps2(1,1))
      else
       call nfcheckload(ncid, psv, start3d, count3d, ps2(1,1))
      endif
c  input ps, must be hPa, otherwise:
      if (nctype.eq.'arome') then
        do j=1,ny
          do i=1,nx
            ps2(i,j) = ps2(i,j) *0.01
          end do
        end do
      endif
c      call readfd(iunit,navps,ivc,8,ilevel,0,ps2(1,1),ierror)


c u10m
c v10m
      if (has_dummy_dim) then
        call nfcheckload(ncid, xwind10mv, start4d, count4d, u2(1,1,1))
        call nfcheckload(ncid, ywind10mv, start4d, count4d, v2(1,1,1))
      else
        call nfcheckload(ncid, xwind10mv, start3d, count3d, u2(1,1,1))
        call nfcheckload(ncid, ywind10mv, start3d, count3d, v2(1,1,1))
      endif

c..mean sea level pressure, not used in computations,
c..(only for graphics and/or output to results file)
      if(imslp.ne.0) then
        if (.not.mslpv.eq.'') then
          write(9,*) 'Mslp not found. Not important.'
          imslp=0
        else
          if (has_dummy_dim) then
            call nfcheckload(ncid, mslpv, start4d, count4d, pmsl2(1,1))
          else
            call nfcheckload(ncid, mslpv, start3d, count3d, pmsl2(1,1))
          endif
c        call readfd(iunit,nav,ivc,58,ilevel,0,pmsl2(1,1),ierror)
        end if
      end if

c..precipitation......................................................
c
      if (nctype.eq.'h12'.or.nctype.eq.'arome') then
c..precipitation between input time 't1' and 't2'
        if (timepos .ne. 1) then
          if (has_dummy_dim) then
            start4d(4) = timepos - 1
            call nfcheckload(ncid, precaccumv,
     +                        start4d, count4d, field1(1,1))
c         call readfd(iunit,nav,ivc,17,ilevel,ihrpr1,field1,ierror)
            start4d(4) = timepos
            call nfcheckload(ncid, precaccumv,
     +                      start4d, count4d, field2(1,1))
          else
            start3d(3) = timepos - 1
            call nfcheckload(ncid, precaccumv,
     +                        start3d, count3d, field1(1,1))
c         call readfd(iunit,nav,ivc,17,ilevel,ihrpr1,field1,ierror)
            start3d(3) = timepos
            call nfcheckload(ncid, precaccumv,
     +                      start3d, count3d, field2(1,1))
         endif
c         call readfd(iunit,nav,ivc,17,ilevel,ihrpr2,field2,ierror)
c..the difference below may get negative due to different scaling
          do j=1,ny
            do i=1,nx
              precip1=max(field2(i,j)-field1(i,j),0.)/nhdiff
              do k=1,nprecip
                precip(i,j,k)=precip1
              end do
            end do
          end do
        end if
      else if (nctype.eq."emep") then
c..non-accumulated emissions in stratiform an convective
        if (has_dummy_dim) then
          call nfcheckload(ncid, precstrativrt,
     +                    start4d, count4d, field1(1,1))
          call nfcheckload(ncid, precconvrt,
     +                    start4d, count4d, field2(1,1))
        else
          call nfcheckload(ncid, precstrativrt,
     +                    start3d, count3d, field1(1,1))
          call nfcheckload(ncid, precconvrt,
     +                    start3d, count3d, field2(1,1))
        endif

        do j=1,ny
          do i=1,nx
c..precipitation must be larger 0, m/s -> mm/h
            precip1=max(field1(i,j)+field2(i,j),0.)*3600*1000
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
      if (temp_is_abs) then
c..abs.temp. -> pot.temp.
        rcp=r/cp
        do k=2,nk-kadd
          do j=1,ny
            do i=1,nx
              p=alevel(k)+blevel(k)*ps2(i,j)
              t2(i,j,k)=t2(i,j,k)/((p*0.001)**rcp)
            end do
          end do
        end do
      end if

      if (sigmadotv.eq.'') then
c..omega -> etadot, or rather etadot derived from continuity-equation
        call om2edot
      end if

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

      if(nhleft.lt.0) then
c backward-calculation, switch sign of winds
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
c      call check(nf_close(ncid), "")

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

      subroutine nfcheckload(ncid, varname, start, length, field)
      implicit none
      include 'netcdf.inc'
      integer, intent ( in) :: ncid, start(1), length(1)
      character(len=*), intent(in) :: varname
      real(kind=4), intent (inout) :: field(*)

      real(kind=4) factor, offset
      integer :: varid, attlen, atttype, status, ndims, siz, i

      call check(nf_inq_varid(ncid, varname, varid), varname)
      call check(nf_get_vara_real(ncid, varid, start, length,
     +             field), varname)
!  check add_offset and scale_factor
      factor = 1.
      offset = 0.
      status = nf_inq_att(ncid, varid, "scale_factor", attlen, atttype)
      if (status .eq. nf_noerr) then
        call check(nf_get_att_real(ncid, varid, "scale_factor",factor),
     +       "scale_factor "//varname)
      end if
      status = nf_inq_att(ncid, varid, "add_offset", attlen, atttype)
      if (status .eq. nf_noerr) then
        call check(nf_get_att_real(ncid, varid, "add_offset", offset),
     +      "add_offset "//varname)
      end if
      if (factor /= 1. .or. offset /= 0.) then
        call check(nf_inq_varndims(ncid, varid,ndims), "dims"//varname)
        siz=1
        do i =1,ndims
          siz = siz * length(i)
        end do
c        write(*,*) "scaling:", varname, factor, offset, siz
        do i = 1, siz
          field(i) = field(i)*factor + offset
        end do
      end if


      end subroutine nfcheckload
