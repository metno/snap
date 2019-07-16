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

module ensembleML
  implicit none
  private

  integer, parameter, public :: nxep=151, nyep=91

  public ensemble

  contains

subroutine ensemble(icall,itime,tf1,tf2,tnow,istep,nstep,nsteph, &
  np)
  USE iso_fortran_env, only: error_unit, real32, real64
  USE particleML
  USE snapepsML
  USE snapfldML
  USE snapparML
  USE snapgrdML
  USE snapdimML, only: nx,ny,nk,mcomp
  USE epinterpML, only: epinterp
  USE ftestML, only: ftest
  USE snapdebug, only: iulog
  USE milibML, only: xyconvert, mapfield, rlunit, vtime
  USE releaseML, only: npart, mpart

!  Purpose: Interpolate particle positions to ENSEMBLE grid,
!           and store data in this grid (and model levels),
!        interpolation of concentrations to fixed heights.

!    icall=0 : initialize (after first fields read),
!            =1 : new fields read
! 2,3,4 need to be called within a particle loop (np)
!            =2 : before drydep
!            =3 : after drydep, before wetdep
!            =4 : after wetdep
!            =5 : after forwrd,rwalk
!            =6 : output etc...
!            =7 : final output of timeseries for each gridpoint

!  np: particle-id from particle loop for icall=2,3,4

  implicit none

!..input
  integer, INTENT(IN) :: icall,istep,nstep,nsteph, np
  integer, INTENT(IN) :: itime(5)
  real, INTENT(IN)    ::    tf1,tf2,tnow

!---------------------------------------------------------
! ccc parameter (nxep=151,nyep=91)

  integer, parameter :: nheights=5

  integer, parameter :: nepout=nheights+4

  real, save :: heights(nheights) = [0.0, 200.0, 500.0, 1300.0, 3000.0]

  integer, save :: igridep = 2
  integer :: AllocStat
  real, save :: gparep(6) = [-15.,30.,0.5,0.5,0.,0.]

! nxep,nyep,mcomp arrays, (nxep,nyep,nk,mcomp in case of concep)
  real(real64), allocatable, save :: drydepep(:,:,:), &
  wetdepep(:,:,:), concsurfep(:,:,:), conc(:,:,:), concep(:,:,:,:)

! nxep,nyep,mk arrays
  real(real32), allocatable, save :: hlayer1ep(:,:,:), &
  hlayer2ep(:,:,:), hlevel1ep(:,:,:),hlevel2ep(:,:,:)
! nxep, nyep arrays
  real(real32), allocatable, save :: precipep(:,:), gareaep(:,:), &
  xmodel(:,:), ymodel(:,:), prectmp(:,:)

! mpart arrays
  real(real32), allocatable, save :: pbq1(:),pbq2(:), &
  xep(:),yep(:)

! nxep, nyep, nepout
  real(real32), allocatable, save :: epfield(:,:,:)

! nxep, nyep
  integer, allocatable, save :: inside(:,:)
! 2, nxep*nyep
  integer, allocatable, save :: ijlist(:,:)


!..using allocated memory for the final output (timeseries at each pos.)

  integer :: lepdata
  integer :: mbuffer
! buffer nepout, mbuffer, should be used after dallocation of other arrays
  real, allocatable :: buffer(:,:)
!      equivalence (drydepep(1,1,1),buffer(1,1))


  integer :: matimev
! 5,matimev
  integer, allocatable :: iatimev(:,:)

  real, save :: dxgr(5) = [-0.5,-0.5,+0.5,+0.5,0.0]
  real, save :: dygr(5) = [-0.5,+0.5,-0.5,+0.5,0.0]

  integer, save :: itimerel(5) = [0,0,0,0,0]
  integer :: itimev(5)

  integer, save :: nsaveconc=0,ntoutput=0

  real ::    hh,dh,rt1,rt2,rk1,rk2
  real ::    hl(nk),cl(nk)

  integer :: i,j,k,l,m,mm,n,ierror,ninside,ivlvl,ihour,kk,npos
  integer :: n1,n2,it,lrunit,no,mtoutput

  real ::    undef,dxgridep,dygridep,x1,y1,x2,y2,hrstep,scale
  real ::    cscale,dscale,cavg,hmax,glon,glat,ttmax

  character(128) :: filename


!---------------------------------------------------------

  if (nxep < 2 .OR. nyep < 2) return

!##################################################################
  write(error_unit,*) '========== ENSEMBLE  icall= ',icall
  write(iulog,*) '========== ENSEMBLE  icall= ',icall
!##################################################################

  undef=+1.e+35

  if(icall == 0) then
  ! initialize
    ALLOCATE ( drydepep(nxep,nyep,mcomp), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( wetdepep(nxep,nyep,mcomp), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( concsurfep(nxep,nyep,mcomp), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( conc(nxep,nyep,mcomp), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( concep(nxep,nyep,nk,mcomp), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( hlayer1ep(nxep,nyep,nk), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( hlayer2ep(nxep,nyep,nk), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( hlevel1ep(nxep,nyep,nk), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( hlevel2ep(nxep,nyep,nk), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( precipep(nxep,nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( gareaep(nxep,nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( xmodel(nxep,nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( ymodel(nxep,nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( prectmp(nxep,nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( pbq1(mpart), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( pbq2(mpart), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( xep(mpart), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    ALLOCATE ( yep(mpart), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( epfield(nxep,nyep,nepout), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( inside(nxep,nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    ALLOCATE ( ijlist(2, nxep*nyep), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
  
  


  
  !..time of release
    do i=1,5
      itimerel(i)=itime(i)
    end do
  
  !..compute map ratio
    call mapfield(1,0,igridep,gparep, &
    nxep,nyep,xmodel,ymodel, &
    xmodel, & ! Ignored when icori = 0
    dxgridep,dygridep,ierror)
    if(ierror /= 0) then
      write(iulog,*) 'ensemble MAPFIELD ERROR. ierror= ',ierror
      write(error_unit,*) 'ensemble MAPFIELD ERROR. ierror= ',ierror
      stop 255
    end if
  !..size of each grid square (m**2)
    dxgridep=abs(dxgridep)
    dygridep=abs(dygridep)
    do j=1,nyep
      do i=1,nxep
        gareaep(i,j)= (dxgridep/xmodel(i,j)) &
        *(dygridep/ymodel(i,j))
      end do
    end do
  
  !..no output unless all corners of gridsquare are inside model grid
    do j=1,nyep
      do i=1,nxep
        inside(i,j)=1
      end do
    end do
    do n=1,5
      do j=1,nyep
        do i=1,nxep
          xmodel(i,j)=float(i)+dxgr(n)
          ymodel(i,j)=float(j)+dygr(n)
        end do
      end do
      call xyconvert(nxep*nyep,xmodel,ymodel, &
      igridep,gparep,igtype,gparam,ierror)
      if(ierror /= 0) then
        write(error_unit,*) 'ensemble XYCONVERT ERROR'
        write(iulog,*) 'ensemble XYCONVERT ERROR'
        stop 17
      end if
      x1=1.
      x2=nx
      y1=1.
      y2=ny
    !##################################################################
      ninside=0
    !##################################################################
      do j=1,nyep
        do i=1,nxep
          if(xmodel(i,j) < x1 .OR. xmodel(i,j) > x2 .OR. &
          ymodel(i,j) < y1 .OR. ymodel(i,j) > y2) &
          inside(i,j)=0
        !##################################################################
          ninside=ninside+inside(i,j)
        !##################################################################
        end do
      end do
    end do
  !##################################################################
    write(error_unit,*) 'nxep*nyep,ninside,noutside: ', &
    nxep*nyep,ninside,nxep*nyep-ninside
    write(iulog,*) 'nxep*nyep,ninside,noutside: ', &
    nxep*nyep,ninside,nxep*nyep-ninside
    call ftest('xmodel', xmodel)
    call ftest('ymodel', ymodel)
    call ftest('gareap', gareaep)
  !##################################################################
  
  !..output position sequence
    no=0
    do j=nyep,1,-1
      do i=1,nxep
        no=no+1
        ijlist(1,no)=i
        ijlist(2,no)=j
      end do
    end do
  
  !..initialize
    do m=1,ncomp
      do j=1,nyep
        do i=1,nxep
          drydepep(i,j,m)=0.0d0
          wetdepep(i,j,m)=0.0d0
          concsurfep(i,j,m)=0.0d0
        end do
      end do
    end do
    do j=1,nyep
      do i=1,nxep
        precipep(i,j)=0.0
      end do
    end do
  
  !..get machine dependant unit of recordlength in bytes
    call rlunit(lrunit)
  
    do m=1,ncomp
    
      mm= run_comp(m)%to_defined
    
      filename= 'ensemble.tmp.'//def_comp(mm)%compnamemc
      open(60+m,file=filename, &
      access='direct',form='unformatted', &
      recl=(5+nxep*nyep*nepout)*4/lrunit, &
      status='unknown')
    
      filename= 'ensemble.test.'//def_comp(mm)%compnamemc
      open(70+m,file=filename, &
      access='sequential',form='formatted', &
      status='unknown')
    
    end do
  
  end if

  if (icall == 1) then
    hlayer1ep = hlayer2ep
    hlevel1ep = hlevel1ep
  end if

  if (icall == 0 .OR. icall == 1) then
  
  !..height of model levels and thickness of model layers
  
    do k=1,nk
      call epinterp(nx,ny,hlevel2(:,:,k), &
      nxep*nyep,xmodel,ymodel, &
      hlevel2ep(1,1,k),inside)
    end do
  
    do k=1,nk
      call epinterp(nx,ny,hlayer2(:,:,k), &
      nxep*nyep,xmodel,ymodel, &
      hlayer2ep(1,1,k),inside)
    end do
  
  !##################################################################
    call ftest('hlayer',nk,1,nxep,nyep,nk,hlayer2ep,1)
    call ftest('hlevel',nk,1,nxep,nyep,nk,hlevel2ep,1)
  !##################################################################
  
  !##################################################################
    i=nxep/2
    j=nyep/2
    do k=nk,1,-1
      write(iulog,fmt='(''    k,hlayer,hlevel:'',i3,2f8.0)') &
      k,hlayer2ep(i,j,k),hlevel2ep(i,j,k)
    end do
  !##################################################################
  end if

  if(icall == 2) then
  
  !..store Bq before drydep
  !       do n=1,npart // particle loop now in snap.f
    pbq1(np)=pdata(np)%rad
  !       end do
  
  end if

  if(icall == 3) then
  
  !..store Bq after drydep, before wetdep
  !       do n=1,npart // particle loop now in snap.f
    pbq2(np)=pdata(np)%rad
  !       end do
  
  end if

  if(icall == 4) then
  
  !       do n=1,npart // np loop now in snap.F
    xep(1)= pdata(np)%x
    yep(1)= pdata(np)%y
    call xyconvert(1,xep,yep,igtype,gparam, &
    igridep,gparep,ierror)
    if(ierror /= 0) then
      write(error_unit,*) 'ensemble XYCONVERT ERROR'
      write(iulog,*) 'ensemble XYCONVERT ERROR'
      stop 17
    end if

    i=nint(xep(1))
    j=nint(yep(1))
    m=def_comp(pdata(n)%icomp)%to_running
    if(i > 0 .AND. i <= nxep .AND. &
    j > 0 .AND. j <= nyep) then
      drydepep(i,j,m)= drydepep(i,j,m) + dble(pbq1(np)-pbq2(np))
      wetdepep(i,j,m)= wetdepep(i,j,m) + &
      dble(pbq2(np)-pdata(np)%rad)
    end if
  !       end do
  
  end if

  if(icall == 5) then
  !..convert positions to ensemble grid
    do n=1,npart
      xep(n)= pdata(n)%x
      yep(n)= pdata(n)%y
    end do
    call xyconvert(npart,xep,yep,igtype,gparam, &
    igridep,gparep,ierror)
    if(ierror /= 0) then
      write(error_unit,*) 'ensemble XYCONVERT ERROR'
      write(iulog,*) 'ensemble XYCONVERT ERROR'
      stop 17
    end if
  
  !..for linear interpolation in time
    rt1=(tf2-tnow)/(tf2-tf1)
    rt2=(tnow-tf1)/(tf2-tf1)
    hrstep=1./float(nsteph)
  
    do m=1,ncomp
      do j=1,nyep
        do i=1,nxep
          conc(i,j,m)=0.0d0
        end do
      end do
    end do
  
    do n=1,npart
      ivlvl=pdata(n)%z*10000.
      k=ivlayer(ivlvl)
      if(k == 1) then
        i=nint(xep(n))
        j=nint(yep(n))
        if(i > 0 .AND. i <= nxep .AND. &
        j > 0 .AND. j <= nyep) then
          m=def_comp(pdata(n)%icomp)%to_running
          conc(i,j,m)= conc(i,j,m)+dble(pdata(n)%rad)
        end if
      end if
    end do
  
    do m=1,ncomp
      do j=1,nyep
        do i=1,nxep
          if(conc(i,j,m) > 0.0d0) then
            dh= rt1*hlayer1ep(i,j,1)+rt2*hlayer2ep(i,j,1)
            concsurfep(i,j,m)= concsurfep(i,j,m) &
            +conc(i,j,m)*hrstep/(dh*gareaep(i,j))
          end if
        end do
      end do
    end do
  
    if(nsaveconc >= 0) then
      if(nsaveconc == 0) then
        do m=1,ncomp
          do k=1,nk
            do j=1,nyep
              do i=1,nxep
                concep(i,j,k,m)=0.0d0
              end do
            end do
          end do
        end do
      end if
      nsaveconc=nsaveconc+1
    !##################################################################
    !      minv=+999999
    !      maxv=-999999
    !      mink=+999999
    !      maxk=-999999
    !##################################################################
      do n=1,npart
        i=nint(xep(n))
        j=nint(yep(n))
        if(i > 0 .AND. i <= nxep .AND. &
        j > 0 .AND. j <= nyep) then
          ivlvl=pdata(n)%z*10000.
          k=ivlayer(ivlvl)
          m=def_comp(pdata(n)%icomp)%to_running
        !..in each sigma/eta (input model) layer
          concep(i,j,k,m)=concep(i,j,k,m)+dble(pdata(n)%rad)
        !##################################################################
        !          minv=min(minv,ivlvl)
        !          maxv=max(maxv,ivlvl)
        !          mink=min(mink,k)
        !          maxk=max(maxk,k)
        !##################################################################
        end if
      end do
    !##################################################################
    !      write(iulog,*) 'minv,maxv,mink,maxk: ',minv,maxv,mink,maxk
    !##################################################################
    end if
  
  !..precipitation (from intensity per hour)
    scale= 1./float(nsteph)
    call epinterp(nx,ny,precip(:,:,iprecip), &
    nxep*nyep,xmodel,ymodel,prectmp,inside)
    do j=1,nyep
      do i=1,nxep
        if(inside(i,j) == 1) &
        precipep(i,j)= precipep(i,j)+scale*prectmp(i,j)
      end do
    end do
  
  end if

  if(icall == 6) then
  
    write(error_unit,*) 'istep,nstep,nsteph,itime,tf1,tf2,tnow:'
    write(iulog,*) 'istep,nstep,nsteph,itime,tf1,tf2,tnow:'
    write(error_unit,900) istep,nstep,nsteph,itime,tf1,tf2,tnow
    write(iulog,900) istep,nstep,nsteph,itime,tf1,tf2,tnow
    900 format(1x,3i4,4x,i4,4i3,4x,3f9.1)
  
    do i=1,5
      itimev(i)=itime(i)
    end do
    call vtime(itimev,ierror)
  !..minute
    itimev(5)=0
  
    ihour=itimev(4)
  
    if(mod(ihour,ensembleStepHours) == 0) then
    
    !..output
    !################################################################
      write(error_unit,*) 'out nsaveconc= ',nsaveconc
    !################################################################
    
    !..fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
    ! c       cscale=10.**12
    
    !..fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m2)
    ! c       dscale=10.**9
    
      cscale= 1.0
      dscale= 1.0
    
    !..for linear interpolation in time
      rt1=(tf2-tnow)/(tf2-tf1)
      rt2=(tnow-tf1)/(tf2-tf1)
    
      cavg=float(nsaveconc)
    
      hmax=heights(nheights)+1.
    
      mtoutput= ntoutput
    
      do m=1,ncomp
      
        ntoutput= mtoutput
      
        do j=1,nyep
          do i=1,nxep
            glon= gparep(1)+gparep(3)*(i-1)
            glat= gparep(2)+gparep(4)*(j-1)
            if(inside(i,j) == 1) then
              hh=-1.
              kk=0
              do while (hh < hmax .AND. kk < nk)
                kk=kk+1
                dh=rt1*hlayer1ep(i,j,kk)+rt2*hlayer2ep(i,j,kk)
                hh=rt1*hlevel1ep(i,j,kk)+rt2*hlevel2ep(i,j,kk)
                hl(kk)=hh
                cl(kk)=concep(i,j,kk,m)/(dh*gareaep(i,j)*cavg)
              end do
              k=2
              do l=1,nheights
                do while (heights(l) > hl(k) .AND. k < kk)
                  k=k+1
                end do
                rk1=(hl(k)-heights(l))  /(hl(k)-hl(k-1))
                rk2=(heights(l)-hl(k-1))/(hl(k)-hl(k-1))
                epfield(i,j,l)= (rk1*cl(k-1)+rk2*cl(k))*cscale
              end do
              epfield(i,j,nheights+1)=concsurfep(i,j,m)*cscale
              epfield(i,j,nheights+2)=drydepep(i,j,1)*dscale &
              /gareaep(i,j)
              epfield(i,j,nheights+3)=wetdepep(i,j,1)*dscale &
              /gareaep(i,j)
            !..precipitation unit 0.1 mm !!!!!!!!!!!!!!!!!!!!
              epfield(i,j,nheights+4)=precipep(i,j) * 10.
            else
              do l=1,nepout
                epfield(i,j,l)=-9.
              end do
            end if
          
          ! c          write(97,1001) (itimev(l),l=1,5),glon,glat,
          ! c  +                 (epfield(i,j,l),l=1,nepout)
            1001 format(i4.4,4i2.2,f10.5,1x,f10.5,9(1x,1pe11.4))
          !################################################################
            ttmax=0.
            do l=1,nepout-1
              ttmax=max(ttmax,epfield(i,j,l))
            end do
            if(ttmax > 0.) &
            write(70+m,1001) (itimev(l),l=1,5),glon,glat, &
            (epfield(i,j,l),l=1,nepout)
          !################################################################
          end do
        end do
      
        ntoutput=ntoutput+1
      
      !..write temporary binary file
      !################################################################
      !      write(error_unit,*) 'WRITE unit,rec: ',60+m,ntoutput
      !################################################################
        write(60+m,rec=ntoutput) itimev,epfield
      
      end do
    
    end if
  
  !..check if one hour before next output
    nsaveconc=-1
    if(mod(ihour+1,ensembleStepHours) == 0) nsaveconc=0
  !################################################################
    write(error_unit,*) 'set nsaveconc= ',nsaveconc
  !################################################################
  
  end if

  if(icall == 7) then
  
    DEALLOCATE ( drydepep )
    DEALLOCATE ( wetdepep )
    DEALLOCATE ( concsurfep )
    DEALLOCATE ( conc )
    DEALLOCATE ( concep )
    DEALLOCATE ( hlayer1ep )
    DEALLOCATE ( hlayer2ep )
    DEALLOCATE ( hlevel1ep )
    DEALLOCATE ( hlevel2ep )
    DEALLOCATE ( precipep )
    DEALLOCATE ( gareaep )
    DEALLOCATE ( xmodel )
    DEALLOCATE ( ymodel )
    DEALLOCATE ( prectmp )
    DEALLOCATE ( pbq1 )
    DEALLOCATE ( pbq2 )
    DEALLOCATE ( xep )
    DEALLOCATE ( yep )

    DEALLOCATE ( inside )

    DEALLOCATE ( ijlist )

  ! reuse just deallocated memory
    lepdata=  nxep*nyep*(3+nk+1)*mcomp*2 &
    + nxep*nyep*(1+nk*4+3+1) &
    + mpart*4
    mbuffer= lepdata/nepout
    ALLOCATE ( buffer(nepout,mbuffer), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"
    matimev= 100 + nxep*nyep/5
    ALLOCATE ( iatimev(5,matimev), STAT = AllocStat)
    IF (AllocStat /= 0) STOP "*** Not enough memory ***"

    if (ntoutput < 1) return
  
    npos= mbuffer/ntoutput
  
    write(error_unit,*) 'Final ENSEMBLE PROJECT output'
    write(error_unit,*) 'lepdata,nepout:   ',lepdata,nepout
    write(error_unit,*) 'mbuffer,ntoutput: ',mbuffer,ntoutput
    write(error_unit,*) 'matimev:          ',matimev
    write(error_unit,*) 'npos:             ',npos
    write(iulog,*) 'Final ENSEMBLE PROJECT output'
    write(iulog,*) 'lepdata,nepout:   ',lepdata,nepout
    write(iulog,*) 'mbuffer,ntoutput: ',mbuffer,ntoutput
    write(iulog,*) 'matimev:          ',matimev
    write(iulog,*) 'npos:             ',npos
    write(iulog,*) 'file: ', trim(ensemblefile)
  
    if (npos < 1) stop 17
    if (ntoutput > matimev) stop 17
  
    do m=1,ncomp
    
      mm= run_comp(m)%to_defined
    
      k=index(ensemblefile,' ')
      if (k < 0) k=len(ensemblefile)+1
      k=k-1
      filename=ensemblefile(1:k)//'_'//def_comp(mm)%compnamemc

      open(98,file=filename, &
      access='sequential',form='formatted', &
      status='unknown')
    
      write(98,fmt='(i2.2)') ensembleparticipant
      write(98,fmt='(a7)')   ensembleRandomKey
      write(98,fmt='(a5)')   def_comp(mm)%compnamemc
      write(98,fmt='(i2.2)') ntoutput
    !..release start
      do i=1,5
        itimev(i)=itimerel(i)
      end do
      itimev(5)=00
      call vtime(itimev,ierror)
      write(98,fmt='(i4.4,4i2.2)') itimev
    !..last analysis
      do i=1,5
        itimev(i)=itime(i)
      end do
      itimev(5)=00
      call vtime(itimev,ierror)
      write(98,fmt='(i4.4,4i2.2)') itimev
    
      do n1=1,nxep*nyep,npos
      
        n2=min(n1+npos-1,nxep*nyep)
      
        write(error_unit,*) 'read/write  n1,n2,nxep*nyep: ',n1,n2,nxep*nyep
      
        do it=1,ntoutput
        
        !################################################################
        !        write(error_unit,*) 'READ unit,rec: ',60+m,it
        !################################################################
          read(60+m,rec=it) itimev,epfield
        
          do i=1,5
            iatimev(i,it)=itimev(i)
          end do
        
          do n=n1,n2
            i=ijlist(1,n)
            j=ijlist(2,n)
            no=(n-n1)*ntoutput+it
            do k=1,nepout
              buffer(k,no)= epfield(i,j,k)
            end do
          end do
        
        end do
      
        do n=n1,n2
          i=ijlist(1,n)
          j=ijlist(2,n)
          glon= gparep(1)+gparep(3)*(i-1)
          glat= gparep(2)+gparep(4)*(j-1)
          write(98,fmt='(f10.5,1x,f10.5)') glon,glat
          no=(n-n1)*ntoutput
          do it=1,ntoutput
            no=no+1
            write(98,fmt='(i4.4,4i2.2,9(1x,1pe11.4))') &
            (iatimev(i,it),i=1,5), &
            (buffer(k,no),k=1,nepout)
          end do
        end do
      
      end do
    
      close(98)
      close(60+m)
      close(70+m)
    
    end do
  
    DEALLOCATE ( epfield )

  end if

  return
end subroutine ensemble
end module ensembleML
