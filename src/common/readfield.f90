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

module readfieldML
  implicit none
  private

  public readfield

  contains

subroutine readfield(iunit,istep,nhleft,itimei,ihr1,ihr2, &
  itimefi,ierror)

!  Purpose:  Read fields from FELT files.
!            (FELT files: forecasts and archives)
!  Parameters:
!             iunit      filehandle-unit (dummy)
!             istep      current timestep (always positive)
!             nhleft     remaining run-hours (negative for backward)
!             itimei(5)  initial time
!             ihr1       minimal time-offset?
!             ihr2       maximal time-offset?
!             itimefi(5) final time (output)
!             ierror     error (output)
  USE iso_fortran_env, only: error_unit
  USE particleML
  USE snapfilML
  USE snapgrdML
  USE snapfldML
  USE snaptabML
  USE snapdebug, only: idebug, iulog
  USE readfdML, only: readfd
  USE om2edotML, only: om2edot
  USE ftestML, only: ftest
  USE snapdimML, only: nx, ny, nk, ldata, mavail, mprecip
  USE milibML, only: gridpar, mapfield, hrdiff
  implicit none

!..input/output
  integer ::   iunit,istep,nhleft,ihr1,ihr2,ierror
  integer ::   itimei(5),itimefi(5)

!..local
  integer ::   itime(5,4),ihours(4)
  integer ::   itav(mavail)
  real ::      alev(nk),blev(nk)

  integer :: n,i,j,k,ierr1,ierr2,ihdif1,ihdif2,kfb,ifb,kk
  integer :: mtav,ntav,nav,levelw,ivc,iu,iv,iw,it,ilevel,nhdiff,navps
  integer :: lprog2,ihdiff,ihrpr1,ihrpr2,iprog1,ix,iy
  integer :: ierr,i1,i2
  real ::    whelp,ptop,prec1,prec2,db,p1,p2,dp,dxgrid,dygrid
  real ::    p,dred,red,px

  integer, save :: itryprecip=1

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

  write(iulog,*) '*READFIELD* Requested time: ',(itime(i,1),i=1,4)
  write(iulog,*) '                Time limit: ',(itime(i,2),i=1,4)
  write(iulog,*) '                 ihr1,ihr2: ', ihr1, ihr2

  if(navailt2 > 0) then
  
  !..move data from input time step 2 to 1
  
    navailt1=navailt2
    navailt2=0
 
    u1 = u2
    v1 = v2
    w1 = w2
    t1 = t2
    hlevel1 = hlevel2
    hlayer1 = hlayer2

    ps1 = ps2
    bl1 = bl2
    hbl1 = hbl2
  
    if(imslp /= 0) then
      pmsl1 = pmsl2
    end if
  
  end if


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

  if(idebug == 1) then
    write(iulog,*) 'istep,nhleft: ',istep,nhleft
    write(iulog,*) 'kfb,ifb,ihdif1,ihdif2:',kfb,ifb,ihdif1,ihdif2
  end if

  mtav=0
  n=kavail(kfb)
  do while (n > 0)
    if((iavail(n)%fileType == 1 .OR. iavail(n)%fileType == 3) &
     .AND. iavail(n)%oHour >= ihdif1 &
     .AND. iavail(n)%oHour <= ihdif2) then
      mtav=mtav+1
      itav(mtav)=n
    end if
  !..pointer to next timestep (possibly same time)
    if (ifb == 9) then
      n=iavail(n)%nAvail
    else
      n=iavail(n)%pAvail
    end if
  end do

  if(idebug == 1) then
    write(iulog,*) 'MODEL LEVEL SEARCH LIST.   mtav=',mtav
    do j=1,mtav
      n=itav(j)
      write(iulog,fmt='(7(1x,i4),1x,i6,2i5)') (iavail(n))
    end do
  end if

  if(mtav < 1) then
    write(iulog,*) '*READFIELD* No model level data available'
    write(error_unit,*) '*READFIELD* No model level data available'
    ierror=1
    return
  end if

!..loop on avaiable model data sets...................................

  ntav=0
  ierror=-1

  do while (ierror /= 0 .AND. ntav < mtav)
  
    ntav=ntav+1
    nav=itav(ntav)
  
  !..kadd = levels added at the top (when upper model levels are missing)
  
    levelw=0
  
  !..read fields......................................................
  
    ierror=0
  
  !..model level fields,
  !..read sequence as fields usually are stored on the felt files
  
  !..vertical coordinate (2=sigma 10=eta)
    ivc=ivcoor
  
  !..parameter no. for u,v,sigma_dot/eta_dot and pot.temp.
    iu=2
    iv=3
    iw=11
    it=18
  
    if(iprod == 98) then
    !..ECMWF data : omega and abs.temp. input (converted later)
      iw=13
      it=4
    end if
    if(iprod == 88 .AND. igridr == 1999) then
    !..Nora10 (era interim downscaled to hirlam10)
      iw=13
      itryprecip=8
    end if
  
    do k=nk-kadd,2,-1
    
    !..input model level no.
      ilevel=klevel(k)
    
    !..u
      call readfd(iunit,nav,ivc,iu,ilevel,0,u2(:,:,k),ierror)
      if(ierror /= 0) goto 100
    
    !..v
      call readfd(iunit,nav,ivc,iv,ilevel,0,v2(:,:,k),ierror)
      if(ierror /= 0) goto 100
    
    !..pot.temp. or abs.temp.
      call readfd(iunit,nav,ivc,it,ilevel,0,t2(:,:,k),ierror)
      if(ierror /= 0) goto 100
    
    !..alevel (here) only for eta levels
      alev(k)=idata( 8)*0.1
      blev(k)=idata(19)*0.0001
    
    !..sigma_dot/eta_dot (0 at surface)
      if(ivc == 2) then
      !..sigma: sigma_dot stored in 'sigma1' levels (between u,v,th levels),
      !..       compute and use mean value in the same levels as u,v,th.
        if(ilevel == 1) then
          do j=1,ny
            do i=1,nx
              field1(i,j)=0.
            end do
          end do
        elseif(ilevel /= levelw) then
          call readfd(iunit,nav,ivc,iw,ilevel,0,field1(:,:),ierror)
          if(ierror /= 0) goto 100
        end if
        if(ilevel+1 < klevel(2)) then
          call readfd(iunit,nav,ivc,iw,ilevel+1,0,w2(:,:,k),ierror)
          if(ierror /= 0) goto 100
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
      !..eta: eta_dot (or omega) stored in the same levels as u,v,th.
        call readfd(iunit,nav,ivc,iw,ilevel,0,w2(:,:,k),ierror)
        if(ierror /= 0) goto 100
      end if
    
    !.....end do k=nk-kadd,2,-1
    end do
  
    100 continue
  
  !.....end do while (ierror.ne.0 .and. ntav.lt.mtav)
  end do

  if(ierror /= 0) then
    write(iulog,*) 'Model level data not found'
    write(error_unit,*) 'Model level data not found'
    goto 200
  end if

  navailt2=nav

  itimefi(1)=idata(12)
  itimefi(2)=idata(13)/100
  itimefi(3)=idata(13)-itimefi(2)*100
  itimefi(4)=idata(14)/100
  itimefi(5)=idata( 4)
  write(iulog,*) '*READFIELD* Used time: ',(itimefi(i),i=1,5)


!..surface pressure, 10m wind and possibly mean sea level pressure

  navps=nav

  if(iavail(nav)%fileType == 1) then
    k=0
    j=0
    do while (k == 0 .AND. j < navail)
      j=j+1
      if(iavail(j)%aYear == iavail(nav)%aYear .AND. &
      iavail(j)%aMonth == iavail(nav)%aMonth .AND. &
      iavail(j)%aDay == iavail(nav)%aDay .AND. &
      iavail(j)%aHour == iavail(nav)%aHour .AND. &
      iavail(j)%fcHour == iavail(nav)%fcHour .AND. &
      iavail(j)%fileType /= 1) k=j
    end do
    if(k > 0) nav=k
  end if

  ivc=2
  ilevel=1000

  if(imslp == 0) imslp=0

!..surface pressure
  call readfd(iunit,navps,ivc,8,ilevel,0,ps2(:,:),ierror)
  if(ierror /= 0 .AND. navps /= nav) then
    call readfd(iunit,nav,ivc,8,ilevel,0,ps2(:,:),ierror)
  end if
  if(ierror /= 0 .AND. iprod == 98) then
  !..trying ln(surface.pressure), ln(Pa) ... Ecmwf MARS data
    call readfd(iunit,nav,ivc,152,ilevel,0,ps2(:,:),ierror)
    if(ierror == 0) then
      do j=1,ny
        do i=1,nx
          ps2(i,j)=exp(ps2(i,j))*0.01
        end do
      end do
    end if
  end if
  if(ierror /= 0) goto 200
!..ptop for sigma (Norlam)
  ptop=0.
  if(ivcoor == 2) ptop=idata(19)

!..u10m
  call readfd(iunit,nav,ivc,33,ilevel,0,u2(:,:,:),ierror)
  if(ierror /= 0) goto 200
!..v10m
  call readfd(iunit,nav,ivc,34,ilevel,0,v2(:,:,:),ierror)
  if(ierror /= 0) goto 200

!..mean sea level pressure, not used in computations,
!..(only for output to results file)
  if(imslp /= 0) then
    call readfd(iunit,nav,ivc,58,ilevel,0,pmsl2(:,:),ierror)
    if(ierror /= 0) then
      write(iulog,*) 'Mslp not found. Not important.'
    !..stop input of mslp at later timesteps
      imslp=0
    end if
  end if

  navailt2=nav

!..precipitation......................................................

!..precipitation between input time 't1' and 't2'

  if(navailt1 <= 0) goto 190

  if(nhleft > 0) then
    nhdiff=iavail(navailt2)%oHour-iavail(navailt1)%oHour
  else
  ! backward calculation
    nhdiff=iavail(navailt1)%oHour-iavail(navailt2)%oHour
  end if

  if(nhdiff > mprecip) then
    write(error_unit,*) '*READFIELD* PRECIPITATION PROBLEM'
    write(error_unit,*) '     nhdiff,mprecip: ',nhdiff,mprecip
    write(error_unit,*) '   Recompile with mprecip=',nhdiff
    write(iulog,*) '*READFIELD* PRECIPITATION PROBLEM'
    write(iulog,*) '     nhdiff,mprecip: ',nhdiff,mprecip
    write(iulog,*) '   Recompile with mprecip=',nhdiff
    ierror=1
    return
  end if

  nprecip=nhdiff
  iprecip=1

  if(inprecip == 0) then
    do n=1,nhdiff
      do j=1,ny
        do i=1,nx
          precip(i,j,n)=0.
        end do
      end do
    end do
    goto 190
  end if

!..starting from previous precipitation option used

  ivc=2
  ilevel=1000

  goto (110,120,130,140,150,160,170,175) itryprecip

  110 itryprecip=1

!..first try to read hourly precipitation
  ierror=0
  lprog2=-999
  ihdiff=0
  do while (ierror == 0 .AND. ihdiff < nhdiff)
    ihdiff=ihdiff+1
    if(ihdiff <= (nhdiff+1)/2) then
      nav=navailt1
      ihrpr1=ihdiff-1
      ihrpr2=ihdiff
    else
      nav=navailt2
      ihrpr1=-nhdiff+ihdiff-1
      ihrpr2=-nhdiff+ihdiff
    end if
    iprog1=iavail(nav)%fcHour+ihrpr1
    if(iprog1 == 0) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=0.
        end do
      end do
    elseif(iprog1 == lprog2) then
      do j=1,ny
        do i=1,nx
          field1(i,j)=field2(i,j)
        end do
      end do
    else
      call readfd(iunit,nav,ivc,17,ilevel,ihrpr1,field1,ierror)
    end if
    if(ierror == 0) then
      call readfd(iunit,nav,ivc,17,ilevel,ihrpr2,field2,ierror)
      if(ierror == 0) then
      !..the difference below may get negative due to different scaling
        do j=1,ny
          do i=1,nx
            precip(i,j,ihdiff)=max(field2(i,j)-field1(i,j),0.)
          end do
        end do
        lprog2=iavail(nav)%fcHour+ihrpr2
      end if
    end if
  end do

  if(ierror == 0) goto 190

  if(nhdiff /= 6) goto 200

!..try some other precipitation options (only for 6hr interval)

  120 itryprecip=2

!..3 hours total precipitation
  nav=navailt1
  call readfd(iunit,nav,ivc,17,ilevel,+0,field1,ierror)
  if(ierror == 0) then
    call readfd(iunit,nav,ivc,17,ilevel,+3,field2,ierror)
    if(ierror == 0) then
      nav=navailt2
      call readfd(iunit,nav,ivc,17,ilevel,-3,field3,ierror)
      if(ierror == 0) then
        call readfd(iunit,nav,ivc,17,ilevel,-0,field4,ierror)
        if(ierror == 0) then
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

  130 itryprecip=3

!..3 hours frontal and convective precipitation
  nav=navailt1
  call readfd(iunit,nav,ivc,19,ilevel,+0,precip(:,:,1),ierr1)
  call readfd(iunit,nav,ivc,20,ilevel,+0,precip(:,:,2),ierr2)
  if(ierr1 == 0 .AND. ierr2 == 0) then
    call readfd(iunit,nav,ivc,19,ilevel,+3,precip(:,:,3),ierr1)
    call readfd(iunit,nav,ivc,20,ilevel,+3,precip(:,:,4),ierr2)
    if(ierr1 == 0 .AND. ierr2 == 0) then
      nav=navailt2
      call readfd(iunit,nav,ivc,19,ilevel,-3,field1,ierr1)
      call readfd(iunit,nav,ivc,20,ilevel,-3,field2,ierr2)
      if(ierr1 == 0 .AND. ierr2 == 0) then
        call readfd(iunit,nav,ivc,19,ilevel,-0,field3,ierr1)
        call readfd(iunit,nav,ivc,20,ilevel,-0,field4,ierr2)
        if(ierr1 == 0 .AND. ierr2 == 0) then
          do j=1,ny
            do i=1,nx
              prec1=max( precip(i,j,3)+precip(i,j,4) &
              -precip(i,j,1)-precip(i,j,2),0.)/3.
              prec2=max( field3(i,j)+field4(i,j) &
              -field1(i,j)-field2(i,j),0.)/3.
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

  140 itryprecip=4

  if(iprod /= 88) goto 150

!..3 hours frontal and convective precipitation (Norlam, not accumulated)
  nav=navailt1
  call readfd(iunit,nav,ivc,15,ilevel,+3,field1,ierr1)
  call readfd(iunit,nav,ivc,16,ilevel,+3,field2,ierr2)
  if(ierr1 == 0 .AND. ierr2 == 0) then
    nav=navailt2
    call readfd(iunit,nav,ivc,15,ilevel,+0,field3,ierr1)
    call readfd(iunit,nav,ivc,16,ilevel,+0,field4,ierr2)
    if(ierr1 == 0 .AND. ierr2 == 0) then
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

  150 itryprecip=5

  if(iprod /= 88) goto 160

!..Lam50e frontal and convective precipitation, 2hr*1.5 and 3.hr !!!
  nav=navailt1
  call readfd(iunit,nav,ivc,40,ilevel,+2,field1,ierr1)
  call readfd(iunit,nav,ivc,41,ilevel,+2,field2,ierr2)
  if(ierr1 == 0 .AND. ierr2 == 0) then
    nav=navailt2
    call readfd(iunit,nav,ivc,15,ilevel,+0,field3,ierr1)
    call readfd(iunit,nav,ivc,16,ilevel,+0,field4,ierr2)
    if(ierr1 == 0 .AND. ierr2 == 0) then
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

  160 itryprecip=6

  if(iprod /= 98) goto 170

!..total precipitation (EC mars ERA, 6hr. only)

!????????????? analysis or ????????????????????????????????????

  nav=navailt1
  call readfd(iunit,nav,ivc,17,ilevel,+0,field1,ierr1)
  nav=navailt2
  call readfd(iunit,nav,ivc,17,ilevel,+0,field2,ierr2)
  if(ierr1 == 0 .AND. ierr2 == 0) then
    write(error_unit,*) '================================================='
    write(error_unit,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
    write(error_unit,*) '================================================='
    write(iulog,*) '================================================='
    write(iulog,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
    write(iulog,*) '================================================='
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

  170 itryprecip=7

  if(iprod /= 98) goto 180

!..frontal and convective precipitation (EC mars ERA, 6hr. only)

!????????????? analysis or ????????????????????????????????????

  nav=navailt1
  call readfd(iunit,nav,ivc,19,ilevel,+0,field1,ierr1)
  call readfd(iunit,nav,ivc,20,ilevel,+0,field2,ierr2)
  if(ierr1 == 0 .AND. ierr2 == 0) then
    nav=navailt2
    call readfd(iunit,nav,ivc,19,ilevel,+0,field3,ierr2)
    call readfd(iunit,nav,ivc,20,ilevel,+0,field4,ierr2)
    if(ierr1 == 0 .AND. ierr2 == 0) then
      write(error_unit,*) '================================================='
      write(error_unit,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
      write(error_unit,*) '================================================='
      write(iulog,*) '================================================='
      write(iulog,*) 'WARNING: CHECK USE OF EC ERA PRECIPITATION !!!!!!'
      write(iulog,*) '================================================='
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


  175 itryprecip=8

! nora-era felt-files, with hourly non-accumulated precipitation in param 17
! with forecast hour 3 meaning precip between 2 and 3

  ierror=0
  lprog2=-999
  ihdiff=0
  do while (ierror == 0 .AND. ihdiff < nhdiff)
    ihdiff=ihdiff+1
    if ((iavail(navailt1)%fcHour+ihdiff) < 9) then
    ! hours 4 to 8
      nav=navailt1
      ihrpr2=ihdiff
    else
    ! hours 9 = hours 3 on next file
      nav=navailt2
      ihrpr2=ihdiff - 3
    end if
    call readfd(iunit,nav,ivc,17,ilevel,ihrpr2,field2,ierror)
    if(ierror == 0) then
    !..the difference below may get negative due to different scaling
      do j=1,ny
        do i=1,nx
          precip(i,j,ihdiff)=field2(i,j)
        end do
      end do
    end if
  end do

  if(ierror == 0) goto 190


  180 continue

  write(error_unit,*) 'NO PRECIPITATION FOUND !!!!!!!!!!!!!!!!!!!'
  write(iulog,*) 'NO PRECIPITATION FOUND !!!!!!!!!!!!!!!!!!!'
  if(inprecip == -1) then
    write(error_unit,*) 'Not important, not wet depositions.'
    write(iulog,*) 'Not important, not wet depositions.'
    do n=1,nhdiff
      do j=1,ny
        do i=1,nx
          precip(i,j,n)=0.
        end do
      end do
    end do
  !..not reading precipitation at later timesteps
    inprecip=0
    ierror=0
  else
    ierror=999
    goto 200
  end if

!..precipitation end..................................................

  190 continue

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
        write(error_unit,*) 'PROGRAM ERROR.  ivcoor= ',ivcoor
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
      write(error_unit,*) 'PROGRAM ERROR.  ivcoor= ',ivcoor
      stop 255
    end if
  
  !..half levels where height is found,
  !..alevel and blevel are in the middle of each layer
    ahalf(1)=alevel(1)
    bhalf(1)=blevel(1)
    vhalf(1)=vlevel(1)
  !..is the following the best we can do ???
    do k=2,nk-1
      if(klevel(k+1) == klevel(k)-1) then
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
  
  !..get grid parameters from field identification
    call gridpar(+1,ldata,idata,igtype,ix,iy,gparam,ierror)
    if(ierror /= 0) then
      write(iulog,*) 'GRIDPAR ERROR. ierror= ',ierror
      write(error_unit,*) 'GRIDPAR ERROR. ierror= ',ierror
      stop 255
    end if
    if(ixbase /= 1 .OR. iybase /= 1 .OR. ixystp /= 1) then
    !..adjustment when computation field area is not the same as input
      if(igtype == 1 .OR. igtype == 4) then
        gparam(1)=1.+(gparam(1)-float(ixbase))/float(ixystp)
        gparam(2)=1.+(gparam(2)-float(iybase))/float(ixystp)
        gparam(3)=gparam(3)/float(ixystp)
      elseif(igtype == 2 .OR. igtype == 3) then
        gparam(1)=gparam(1)+gparam(3)*float(ixbase-1)
        gparam(2)=gparam(2)+gparam(4)*float(iybase-1)
        gparam(3)=gparam(3)*float(ixystp)
        gparam(4)=gparam(4)*float(ixystp)
      else
        write(iulog,*) 'UNKNOWN gridtype: ',igtype
        write(error_unit,*) 'UNKNOWN gridtype: ',igtype
        stop 255
      end if
    end if
  !..compute map ratio
    call mapfield(1,0,igtype,gparam,nx,ny,xm,ym, &
    xm, & ! Ignored when icori = 0
    dxgrid,dygrid,ierror)
    if(ierror /= 0) then
      write(iulog,*) 'MAPFIELD ERROR. ierror= ',ierror
      write(error_unit,*) 'MAPFIELD ERROR. ierror= ',ierror
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
  
  end if

  if(it == 4) then
  
  !..abs.temp. -> pot.temp.
    !rcp=r/cp
    do k=2,nk-kadd
      do j=1,ny
        do i=1,nx
          p=alevel(k)+blevel(k)*ps2(i,j)
          t2(i,j,k)=t2(i,j,k)/((p*0.001)**rcp)
        end do
      end do
    end do
  end if

  if (iw == 13) then
  
  !..omega -> etadot
    call om2edot
  
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

! test---------------------------------------------------------------
  write(iulog,*) 'k,k_model,alevel,blevel,vlevel,p,dp:'
  px=alevel(nk)+blevel(nk)*1000.
  do k=nk,1,-1
    p=alevel(k)+blevel(k)*1000.
    write(iulog,fmt='(1x,2i5,f9.2,2f9.5,f8.0,f6.0)') &
    k,klevel(k),alevel(k),blevel(k),vlevel(k),p,p-px
    px=p
  end do
! test---------------------------------------------------------------

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

  if(idebug == 1) then
    call ftest('u  ',nk,1,nx,ny,nk,   u2,0)
    call ftest('v  ',nk,1,nx,ny,nk,   v2,0)
    call ftest('w  ',nk,1,nx,ny,nk,   w2,0)
    call ftest('t  ',nk,1,nx,ny,nk,   t2,0)
    call ftest('ps ',1, 1,nx,ny, 1,  ps2,0)
    if (istep > 0) &
    call ftest('pre',1,nprecip,nx,ny,nprecip,precip,0)
  end if

  200 continue

!..close last used FELT file
  call readfd(iunit,0,0,0,0,0,u2(:,:,k),ierr)

  if(ierror /= 0) then
    navailt2=0
    return
  end if

  if(istep == 0) then
  
  ! test---------------------------------------------------------------
    write(iulog,*) 'k,ahalf,bhalf,vhalf,p,dp:'
    px=ahalf(nk)+bhalf(nk)*1000.
    do k=nk,1,-1
      p=ahalf(k)+bhalf(k)*1000.
      write(iulog,fmt='(1x,i5,f9.2,2f9.5,f8.0,f6.0)') &
      k,ahalf(k),bhalf(k),vhalf(k),p,p-px
      px=p
    end do
  ! test---------------------------------------------------------------
  
  !..level table for (vertical) interpolation
  !..(remember that fields are stored bottom to top
  !.. and that all parameters now are in the same levels)
    write(iulog,*) 'ivlevel:'
    write(iulog,*) 'k,i1,i2,vlevel(k+1),vlevel(k)'
    i2=-1
    do k=nk-1,1,-1
      i1=i2+1
      i2=vlevel(k)*10000.
      if(k == 1) i2=10000
      do i=i1,i2
        ivlevel(i)=k
      end do
      write(iulog,*) k,i1,i2,vlevel(k+1),vlevel(k)
    end do
  
  !..level table for concentration in each sigma/eta layer
  !..(layers here as in the input model, no '10m' layer,
  !.. but ordering bottom to top, reorder at time of output)
    write(iulog,*) 'ivlayer:'
    write(iulog,*) 'k,i1,i2,vhalf(k+1),vhalf(k)'
    i2=-1
    do k=nk-1,1,-1
      i1=i2+1
      i2=nint(vhalf(k)*10000.)
      if(k == 1) i2=10000
      do i=i1,i2
        ivlayer(i)=k
      end do
      write(iulog,*) k,i1,i2,vhalf(k+1),vhalf(k)
    end do
  
  end if

  return
end subroutine readfield
end module readfieldML
