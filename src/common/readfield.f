      subroutine readfield(iunit,istep,nhleft,itimei,ihr1,ihr2,
     +                     itimefi,ierror)
c
c  Purpose:  Read fields from FELT files.
c            (FELT files: forecasts and archives)
c
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snaptab.inc'
      include 'snapdebug.inc'
c
c..input/output
      integer   iunit,istep,nhleft,ihr1,ihr2,ierror
      integer   itimei(5),itimefi(5)
c
c..local
      integer   itime(5,4),ihours(4)
      integer   itav(mavail)
      real      alev(nk),blev(nk)
c
      integer n,i,j,k,ierr1,ierr2,ihdif1,ihdif2,kfb,ifb,kk
      integer mtav,ntav,nav,levelw,ivc,iu,iv,iw,it,ilevel,nhdiff,navps
      integer mhprecip,lprog2,ihdiff,ihrpr1,ihrpr2,iprog1,ix,iy
      integer ierr,i1,i2
      real    whelp,ptop,prec1,prec2,db,p1,p2,dp,dxgrid,dygrid
      real    rcp,p,dred,red,px
c
      integer itryprecip
c
      data itryprecip/1/
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('READFIELD',0,ZHOOK_HANDLE)
#endif
c..get time offset in hours (as iavail(8,n))
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
c
      write(9,*) '*READFIELD* Requested time: ',(itime(i,1),i=1,4)
      write(9,*) '                Time limit: ',(itime(i,2),i=1,4)
c
      if(navailt2.gt.0) then
c
c..move data from input time step 2 to 1
c
       navailt1=navailt2
       navailt2=0
c
       call copyfield(u2,u1,nx,ny,nk)
       call copyfield(v2,v1,nx,ny,nk)
       call copyfield(w2,w1,nx,ny,nk)
       call copyfield(t2,t1,nx,ny,nk)
       call copyfield(hlevel2,hlevel1,nx,ny,nk)
       call copyfield(hlayer2,hlayer1,nx,ny,nk)
c
       call copyfield(ps2,ps1,nx,ny,1)
       call copyfield(bl2,bl1,nx,ny,1)
       call copyfield(hbl2,hbl1,nx,ny,1)
c
        if(nxad.eq.nx .and. nyad.eq.ny .and. imslp.ne.0) then
         call copyfield(pmsl2,pmsl1,nx,ny,1)
        end if
c
      end if
c
c
c..search in list of available timesteps with model level data
      if(ihdif1.gt.ihdif2) then
c..first field input, using the backward list
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
      mtav=0
      n=kavail(kfb)
      do while (n.gt.0)
        if((iavail(7,n).eq.1 .or. iavail(7,n).eq.3) .and.
     +	   iavail(8,n).ge.ihdif1 .and. iavail(8,n).le.ihdif2) then
          mtav=mtav+1
          itav(mtav)=n
        end if
c..pointer to next timestep (possibly same time)
        n=iavail(ifb,n)
      end do
c
      if(idebug.eq.1) then
        write(9,*) 'MODEL LEVEL SEARCH LIST.   mtav=',mtav
        write(9,*) 'istep,nhleft: ',istep,nhleft
        write(9,*) 'kfb,ifb,ihdif1,ihdif2:',kfb,ifb,ihdif1,ihdif2
        do j=1,mtav
          n=itav(j)
          write(9,fmt='(7(1x,i4),1x,i6,2i5)') (iavail(i,n),i=1,10)
        end do
      end if
c
      if(mtav.lt.1) then
        write(9,*) '*READFIELD* No model level data available'
        write(6,*) '*READFIELD* No model level data available'
        ierror=1
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('READFIELD',1,ZHOOK_HANDLE)
#endif
        return
      end if
c
c..loop on avaiable model data sets...................................
c
      ntav=0
      ierror=-1
c
      do while (ierror.ne.0 .and. ntav.lt.mtav)
c
      ntav=ntav+1
      nav=itav(ntav)
c
c..kadd = levels added at the top (when upper model levels are missing)
c
      levelw=0
c
c..read fields......................................................
c
      ierror=0
c
c..model level fields,
c..read sequence as fields usually are stored on the felt files
c
c..vertical coordinate (2=sigma 10=eta)
      ivc=ivcoor
c
c..parameter no. for u,v,sigma_dot/eta_dot and pot.temp.
      iu=2
      iv=3
      iw=11
      it=18
c
      if(iprod.eq.98) then
c..ECMWF data : omega and abs.temp. input (converted later)
        iw=13
        it=4
      end if
c
      do k=nk-kadd,2,-1
c
c..input model level no.
        ilevel=klevel(k)
c
c..u
        call readfd(iunit,nav,ivc,iu,ilevel,0,u2(1,1,k),ierror)
        if(ierror.ne.0) goto 100
c
c..v
        call readfd(iunit,nav,ivc,iv,ilevel,0,v2(1,1,k),ierror)
        if(ierror.ne.0) goto 100
c
c..pot.temp. or abs.temp.
        call readfd(iunit,nav,ivc,it,ilevel,0,t2(1,1,k),ierror)
        if(ierror.ne.0) goto 100
c
c..alevel (here) only for eta levels
        alev(k)=idata( 8)*0.1
        blev(k)=idata(19)*0.0001
c
c..sigma_dot/eta_dot (0 at surface)
        if(ivc.eq.2) then
c..sigma: sigma_dot stored in 'sigma1' levels (between u,v,th levels),
c..       compute and use mean value in the same levels as u,v,th.
          if(ilevel.eq.1) then
            do j=1,ny
              do i=1,nx
                field1(i,j)=0.
              end do
            end do
          elseif(ilevel.ne.levelw) then
            call readfd(iunit,nav,ivc,iw,ilevel,0,field1(1,1),ierror)
            if(ierror.ne.0) goto 100
          end if
          if(ilevel+1.lt.klevel(2)) then
            call readfd(iunit,nav,ivc,iw,ilevel+1,0,w2(1,1,k),ierror)
            if(ierror.ne.0) goto 100
          else
            do j=1,ny
              do i=1,nx
                w2(i,j,k)=0.
              end do
            end do
          end if
          do j=1,ny
            do i=1,nx
              whelp=w2(i,j,k)
              w2(i,j,k)=(field1(i,j)+w2(i,j,k))*0.5
              field1(i,j)=whelp
            end do
          end do
          levelw=ilevel+1
        else
c..eta: eta_dot (or omega) stored in the same levels as u,v,th.
          call readfd(iunit,nav,ivc,iw,ilevel,0,w2(1,1,k),ierror)
          if(ierror.ne.0) goto 100
        end if
c
c.....end do k=nk-kadd,2,-1
      end do
c
  100 continue
c
c.....end do while (ierror.ne.0 .and. ntav.lt.mtav)
      end do
c
      if(ierror.ne.0) then
        write(9,*) 'Model level data not found'
        write(6,*) 'Model level data not found'
        goto 200
      end if
c
      navailt2=nav
c
      itimefi(1)=idata(12)
      itimefi(2)=idata(13)/100
      itimefi(3)=idata(13)-itimefi(2)*100
      itimefi(4)=idata(14)/100
      itimefi(5)=idata( 4)
c
c..surface pressure, 10m wind and possibly mean sea level pressure
c
      navps=nav
c
      if(iavail(7,nav).eq.1) then
        k=0
       j=0
       do while (k.eq.0 .and. j.lt.navail)
         j=j+1
         if(iavail(1,j).eq.iavail(1,nav) .and.
     +	     iavail(2,j).eq.iavail(2,nav) .and.
     +	     iavail(3,j).eq.iavail(3,nav) .and.
     +	     iavail(4,j).eq.iavail(4,nav) .and.
     +	     iavail(5,j).eq.iavail(5,nav) .and.
     +	     iavail(7,j).ne.1) k=j
       end do
       if(k.gt.0) nav=k
      end if
c
      ivc=2
      ilevel=1000
c
      if(imslp.eq.0 .or. nxad.ne.nx .or. nyad.ne.ny) imslp=0
c
c..surface pressure
      call readfd(iunit,navps,ivc,8,ilevel,0,ps2(1,1),ierror)
      if(ierror.ne.0 .and. navps.ne.nav) then
        call readfd(iunit,nav,ivc,8,ilevel,0,ps2(1,1),ierror)
      end if
      if(ierror.ne.0 .and. iprod.eq.98) then
c..trying ln(surface.pressure), ln(Pa) ... Ecmwf MARS data
        call readfd(iunit,nav,ivc,152,ilevel,0,ps2(1,1),ierror)
        if(ierror.eq.0) then
          do j=1,ny
            do i=1,nx
              ps2(i,j)=exp(ps2(i,j))*0.01
            end do
          end do
        end if
      end if
      if(ierror.ne.0) goto 200
c..ptop for sigma (Norlam)
      ptop=0.
      if(ivcoor.eq.2) ptop=idata(19)
c
c..u10m
      call readfd(iunit,nav,ivc,33,ilevel,0,u2(1,1,1),ierror)
      if(ierror.ne.0) goto 200
c..v10m
      call readfd(iunit,nav,ivc,34,ilevel,0,v2(1,1,1),ierror)
      if(ierror.ne.0) goto 200
c
c..mean sea level pressure, not used in computations,
c..(only for graphics and/or output to results file)
      if(imslp.ne.0) then
        call readfd(iunit,nav,ivc,58,ilevel,0,pmsl2(1,1),ierror)
        if(ierror.ne.0) then
          write(9,*) 'Mslp not found. Not important.'
c..stop input of mslp at later timesteps
          imslp=0
        end if
      end if
c
      navailt2=nav
c
c..precipitation......................................................
c
c..precipitation between input time 't1' and 't2'
c
      if(navailt1.le.0) goto 190
c
      nhdiff=iavail(8,navailt2)-iavail(8,navailt1)
c
      if(nhdiff.gt.mprecip) then
        write(6,*) '*READFIELD* PRECIPITATION PROBLEM'
        write(6,*) '     nhdiff,mprecip: ',nhdiff,mhprecip
        write(6,*) '   Recompile with mprecip=',nhdiff
        write(9,*) '*READFIELD* PRECIPITATION PROBLEM'
        write(9,*) '     nhdiff,mprecip: ',nhdiff,mhprecip
        write(9,*) '   Recompile with mprecip=',nhdiff
        ierror=1
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif
        return
      end if
c
      nprecip=nhdiff
      iprecip=1
c
      if(inprecip.eq.0) then
        do n=1,nhdiff
          do j=1,ny
            do i=1,nx
              precip(i,j,n)=0.
            end do
          end do
        end do
        goto 190
      end if
c
c..starting from previous precipitation option used
c
      ivc=2
      ilevel=1000
c
      goto (110,120,130,140,150,160,170) itryprecip
c
  110 itryprecip=1
c
c..first try to read hourly precipitation
      ierror=0
      lprog2=-999
      ihdiff=0
      do while (ierror.eq.0 .and. ihdiff.lt.nhdiff)
       ihdiff=ihdiff+1
       if(ihdiff.le.(nhdiff+1)/2) then
         nav=navailt1
         ihrpr1=ihdiff-1
         ihrpr2=ihdiff
       else
         nav=navailt2
         ihrpr1=-nhdiff+ihdiff-1
         ihrpr2=-nhdiff+ihdiff
       end if
       iprog1=iavail(5,nav)+ihrpr1
       if(iprog1.eq.0) then
         do j=1,ny
           do i=1,nx
             field1(i,j)=0.
           end do
         end do
       elseif(iprog1.eq.lprog2) then
         do j=1,ny
           do i=1,nx
             field1(i,j)=field2(i,j)
           end do
         end do
       else
          call readfd(iunit,nav,ivc,17,ilevel,ihrpr1,field1,ierror)
       end if
       if(ierror.eq.0) then
          call readfd(iunit,nav,ivc,17,ilevel,ihrpr2,field2,ierror)
          if(ierror.eq.0) then
c..the difference below may get negative due to different scaling
           do j=1,ny
             do i=1,nx
               precip(i,j,ihdiff)=max(field2(i,j)-field1(i,j),0.)
             end do
           end do
           lprog2=iavail(5,nav)+ihrpr2
         end if
       end if
      end do
c
      if(ierror.eq.0) goto 190
c
      if(nhdiff.ne.6) goto 200
c
c..try some other precipitation options (only for 6hr interval)
c
  120 itryprecip=2
c
c..3 hours total precipitation
      nav=navailt1
      call readfd(iunit,nav,ivc,17,ilevel,+0,field1,ierror)
      if(ierror.eq.0) then
        call readfd(iunit,nav,ivc,17,ilevel,+3,field2,ierror)
        if(ierror.eq.0) then
         nav=navailt2
          call readfd(iunit,nav,ivc,17,ilevel,-3,field3,ierror)
          if(ierror.eq.0) then
            call readfd(iunit,nav,ivc,17,ilevel,-0,field4,ierror)
           if(ierror.eq.0) then
             do j=1,ny
       	do i=1,nx
       	  prec1=max(field2(i,j)-field1(i,j),0.)/3.
       	  prec2=max(field4(i,j)-field3(i,j),0.)/3.
       	  precip(i,j,1)=prec1
       	  precip(i,j,2)=prec1
       	  precip(i,j,3)=prec1
       	  precip(i,j,4)=prec2
       	  precip(i,j,5)=prec2
       	  precip(i,j,6)=prec2
       	end do
             end do
             goto 190
           end if
         end if
       end if
      end if
c
  130 itryprecip=3
c
c..3 hours frontal and convective precipitation
      nav=navailt1
      call readfd(iunit,nav,ivc,19,ilevel,+0,precip(1,1,1),ierr1)
      call readfd(iunit,nav,ivc,20,ilevel,+0,precip(1,1,2),ierr2)
      if(ierr1.eq.0 .and. ierr2.eq.0) then
        call readfd(iunit,nav,ivc,19,ilevel,+3,precip(1,1,3),ierr1)
        call readfd(iunit,nav,ivc,20,ilevel,+3,precip(1,1,4),ierr2)
        if(ierr1.eq.0 .and. ierr2.eq.0) then
         nav=navailt2
          call readfd(iunit,nav,ivc,19,ilevel,-3,field1,ierr1)
          call readfd(iunit,nav,ivc,20,ilevel,-3,field2,ierr2)
          if(ierr1.eq.0 .and. ierr2.eq.0) then
            call readfd(iunit,nav,ivc,19,ilevel,-0,field3,ierr1)
            call readfd(iunit,nav,ivc,20,ilevel,-0,field4,ierr2)
           if(ierr1.eq.0 .and. ierr2.eq.0) then
             do j=1,ny
       	do i=1,nx
       	  prec1=max( precip(i,j,3)+precip(i,j,4)
     +			    -precip(i,j,1)-precip(i,j,2),0.)/3.
       	  prec2=max( field3(i,j)+field4(i,j)
     +			    -field1(i,j)-field2(i,j),0.)/3.
       	  precip(i,j,1)=prec1
       	  precip(i,j,2)=prec1
       	  precip(i,j,3)=prec1
       	  precip(i,j,4)=prec2
       	  precip(i,j,5)=prec2
       	  precip(i,j,6)=prec2
       	end do
             end do
             goto 190
           end if
         end if
       end if
      end if
c
  140 itryprecip=4
c
      if(iprod.ne.88) goto 150
c
c..3 hours frontal and convective precipitation (Norlam, not accumulated)
      nav=navailt1
      call readfd(iunit,nav,ivc,15,ilevel,+3,field1,ierr1)
      call readfd(iunit,nav,ivc,16,ilevel,+3,field2,ierr2)
      if(ierr1.eq.0 .and. ierr2.eq.0) then
       nav=navailt2
        call readfd(iunit,nav,ivc,15,ilevel,+0,field3,ierr1)
        call readfd(iunit,nav,ivc,16,ilevel,+0,field4,ierr2)
        if(ierr1.eq.0 .and. ierr2.eq.0) then
         do j=1,ny
           do i=1,nx
             prec1=field1(i,j)+field2(i,j)
             prec2=field3(i,j)+field4(i,j)
             precip(i,j,1)=prec1
             precip(i,j,2)=prec1
             precip(i,j,3)=prec1
             precip(i,j,4)=prec2
             precip(i,j,5)=prec2
             precip(i,j,6)=prec2
           end do
         end do
         goto 190
       end if
      end if
c
  150 itryprecip=5
c
      if(iprod.ne.88) goto 160
c
c..Lam50e frontal and convective precipitation, 2hr*1.5 and 3.hr !!!
      nav=navailt1
      call readfd(iunit,nav,ivc,40,ilevel,+2,field1,ierr1)
      call readfd(iunit,nav,ivc,41,ilevel,+2,field2,ierr2)
      if(ierr1.eq.0 .and. ierr2.eq.0) then
       nav=navailt2
        call readfd(iunit,nav,ivc,15,ilevel,+0,field3,ierr1)
        call readfd(iunit,nav,ivc,16,ilevel,+0,field4,ierr2)
        if(ierr1.eq.0 .and. ierr2.eq.0) then
         do j=1,ny
           do i=1,nx
             prec1=(field1(i,j)+field2(i,j))*1.5
             prec2= field3(i,j)+field4(i,j)
             precip(i,j,1)=prec1
             precip(i,j,2)=prec1
             precip(i,j,3)=prec1
             precip(i,j,4)=prec2
             precip(i,j,5)=prec2
             precip(i,j,6)=prec2
           end do
         end do
         goto 190
       end if
      end if
c
  160 itryprecip=6
c
      if(iprod.ne.98) goto 170
c
c..total precipitation (EC mars ERA, 6hr. only)
c
c????????????? analysis or ????????????????????????????????????
c
      nav=navailt1
      call readfd(iunit,nav,ivc,17,ilevel,+0,field1,ierr1)
      nav=navailt2
      call readfd(iunit,nav,ivc,17,ilevel,+0,field2,ierr2)
      if(ierr1.eq.0 .and. ierr2.eq.0) then
       write(6,*) '================================================='
       write(6,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
       write(6,*) '================================================='
       write(9,*) '================================================='
       write(9,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
       write(9,*) '================================================='
       do j=1,ny
         do i=1,nx
           prec1=field1(i,j)/6.0
           prec2=field2(i,j)/6.0
           precip(i,j,1)=prec1
           precip(i,j,2)=prec1
           precip(i,j,3)=prec1
           precip(i,j,4)=prec2
           precip(i,j,5)=prec2
           precip(i,j,6)=prec2
         end do
       end do
       goto 190
      end if
c
  170 itryprecip=7
c
      if(iprod.ne.98) goto 180
c
c..frontal and convective precipitation (EC mars ERA, 6hr. only)
c
c????????????? analysis or ????????????????????????????????????
c
      nav=navailt1
      call readfd(iunit,nav,ivc,19,ilevel,+0,field1,ierr1)
      call readfd(iunit,nav,ivc,20,ilevel,+0,field2,ierr2)
      if(ierr1.eq.0 .and. ierr2.eq.0) then
        nav=navailt2
        call readfd(iunit,nav,ivc,19,ilevel,+0,field3,ierr2)
        call readfd(iunit,nav,ivc,20,ilevel,+0,field4,ierr2)
        if(ierr1.eq.0 .and. ierr2.eq.0) then
       write(6,*) '================================================='
       write(6,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
       write(6,*) '================================================='
       write(9,*) '================================================='
       write(9,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
       write(9,*) '================================================='
         do j=1,ny
           do i=1,nx
             prec1=(field1(i,j)+field2(i,j))/6.0
             prec2=(field3(i,j)+field4(i,j))/6.0
             precip(i,j,1)=prec1
             precip(i,j,2)=prec1
             precip(i,j,3)=prec1
             precip(i,j,4)=prec2
             precip(i,j,5)=prec2
             precip(i,j,6)=prec2
           end do
         end do
         goto 190
       end if
      end if
c
  180 continue
c
      write(6,*) 'NO PRECIPITATION FOUND !!!!!!!!!!!!!!!!!!!'
      write(9,*) 'NO PRECIPITATION FOUND !!!!!!!!!!!!!!!!!!!'
      if(inprecip.eq.-1) then
        write(6,*) 'Not important, not wet depositions.'
        write(9,*) 'Not important, not wet depositions.'
        do n=1,nhdiff
          do j=1,ny
            do i=1,nx
              precip(i,j,n)=0.
            end do
          end do
        end do
c..not reading precipitation at later timesteps
        inprecip=0
        ierror=0
      else
        ierror=999
        goto 200
      end if
c
c..precipitation end..................................................
c
  190 continue
c
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
        call gridpar(+1,ldata,idata,igtype,ix,iy,gparam,ierror)
        if(ierror.ne.0) then
          write(9,*) 'GRIDPAR ERROR. ierror= ',ierror
          write(6,*) 'GRIDPAR ERROR. ierror= ',ierror
          stop 255
        end if
        if(ixbase.ne.1 .or. iybase.ne.1 .or. ixystp.ne.1) then
c..adjustment when computation field area is not the same as input
          if(igtype.eq.1 .or. igtype.eq.4) then
            gparam(1)=1.+(gparam(1)-float(ixbase))/float(ixystp)
            gparam(2)=1.+(gparam(2)-float(iybase))/float(ixystp)
            gparam(3)=gparam(3)/float(ixystp)
          elseif(igtype.eq.2 .or. igtype.eq.3) then
            gparam(1)=gparam(1)+gparam(3)*float(ixbase-1)
            gparam(2)=gparam(2)+gparam(4)*float(iybase-1)
            gparam(3)=gparam(3)*float(ixystp)
            gparam(4)=gparam(4)*float(ixystp)
          else
            write(9,*) 'UNKNOWN gridtype: ',igtype
            write(6,*) 'UNKNOWN gridtype: ',igtype
            stop 255
          end if
        end if
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
      if(iprod.eq.98) then
c..conversions of ECMWF input data
c
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
c
c..omega -> etadot
        call om2edot
c
      end if
c
c..sigma_dot/eta_dot 0 at surface
      do j=1,ny
        do i=1,nx
          w2(i,j,1)=0.
        end do
      end do
c
c..no temperature at or near surface (not used, yet)
      do j=1,ny
        do i=1,nx
          t2(i,j,1)=-999.
        end do
      end do
c
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
  200 continue
c
c..close last used FELT file
      call readfd(iunit,0,0,0,0,0,0.,ierr)
c
      if(ierror.ne.0) then
       navailt2=0
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif
       return
      end if
c
      if(istep.eq.0) then
c
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
c
      end if
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('FORWRD',1,ZHOOK_HANDLE)
#endif
      return
      end
