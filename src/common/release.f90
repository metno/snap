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

module releaseML
  implicit none
  private

  public release

  contains

subroutine release(istep,nsteph,tf1,tf2,tnow,ierror)

!  Purpose:  Release one plume of particles
!            The particles are spread in a cylinder volume if radius>0,
!	     otherwise in a column
  USE iso_fortran_env, only: error_unit
  USE particleML
  USE snapgrdML
  USE snapfldML
  USE snapparML
  USE snapposML
  USE snaptabML
  USE snapdimML, only: nx, ny, nk, mcomp
  USE snapdebug, only: iulog

  implicit none

!..input/output
  integer :: istep,nsteph
  real ::    tf1,tf2,tnow
  integer :: ierror

!..local
  integer :: ih,i,j,n,k,m,itab,nprel,nt,npar1,numradius,nrad
  integer :: nrel(mcomp),nrel2(mcomp,2)
  real ::    x,y,dxgrid,dygrid,dx,dy,xrand,yrand,zrand,twodx,twody
  real ::    rt1,rt2,dxx,dyy,c1,c2,c3,c4,tstep
  real ::    ginv,ps,rtab,th,p,pihu,pih,pif,hhu,h1,h2,h,vlev
  real ::    e,a,b,c,ecos,esin,s,gcos,gsin,rcos,smax,b2,en,en2
  real ::    rbqmax,rbq,pscale,radius,hlower,hupper,stemradius
  real ::    pi,volume1,volume2
  real ::    relbq(mcomp),pbq(mcomp)
  real ::    hlevel(nk)
  real ::    hlower2(2),hupper2(2),radius2(2)



! tood for reading multi-layer, many-timesteps emissions from a file
! format: starttime, stoptime, hlower, hupper, radius, componentId, Emissions (kg/s)

! a) know total emissions per component (same total # of model-particles / component emitted, but with a scaling factor of total amount)
!   -> reloop once over complete file
! b) find correct timestep in file (linear)
! c) loop over all heights, set hlower, hupper, radius, nrel(m), pbq(m) (m=components)
! d) reuse existing code





!..for random number functions
  real :: rnd(3)


  if(itprof == 2) then
  !..single bomb release
    tstep=1.
  else
    tstep=3600./float(nsteph)
  end if

!..particle number scaled according to max Bq released
  rbqmax=0.
  do n=1,ntprof
    rbq=0.
    do m=1,ncomp
      do ih=1,nrelheight
        rbq=rbq+relbqsec(n,m,ih)
      end do
    end do
    if(rbqmax < rbq) rbqmax=rbq
  end do
  pscale= float(mprel)/(rbqmax*tstep)

  nt=1
  do n=2,ntprof
    if(frelhour(n)*nsteph <= istep) nt=n
  end do

! loop over all heights
  do ih=1,nrelheight
  
    if(itprof /= 4 .AND. nt < ntprof) then
      c1=frelhour(nt)  *nsteph
      c2=frelhour(nt+1)*nsteph
      c3=istep
      rt1=(c2-c3)/(c2-c1)
      rt2=(c3-c2)/(c2-c1)
      do n=1,ncomp
        relbq(n)= relbqsec(nt,n,ih)*rt1 + relbqsec(nt+1,n,ih)*rt2
      end do
      radius=         relradius(nt,ih)*rt1 +  relradius(nt+1,ih)*rt2
      hupper=          relupper(nt,ih)*rt1 +   relupper(nt+1,ih)*rt2
      hlower=          rellower(nt,ih)*rt1 +   rellower(nt+1,ih)*rt2
    ! stemradius not with height profiles, nrelheight must be 1
      stemradius= relstemradius(nt)*rt1 + relstemradius(nt+1)*rt2
    else
      do n=1,ncomp
        relbq(n)= relbqsec(nt,n,ih)
      end do
      radius=         relradius(nt,ih)
      hupper=          relupper(nt,ih)
      hlower=          rellower(nt,ih)
    ! stemradius not with height profiles, nrelheight must be 1
      stemradius= relstemradius(nt)
    end if
  
    nprel=0
    do m=1,ncomp
    ! Number of particles equal for each component, but not Bq
      nrel(m)=nint(float(mprel)/float(ncomp))
      if(nrel(m) > 0) then
        pbq(m)= relbq(m)*tstep/float(nrel(m))
        nprel= nprel + nrel(m)
      else
        nrel(m)= 0
        pbq(m)=0.
      end if
    end do
  
  !################################################################
  !      if(mod(istep,nsteph).eq.0) then
    do m=1,ncomp
    ! c	  write(error_unit,*) 'release comp,num,bq:',m,nrel(m),pbq(m)
      write(iulog,*) 'release comp,num,bq:',m,nrel(m),pbq(m)
    end do
  ! c	write(error_unit,*) 'nprel: ',nprel
    write(iulog,*) 'nprel: ',nprel
  !      end if
  !################################################################
  
    if(nplume+1 > mplume .OR. npart+nprel > mpart) then
      ierror=1
      return
    end if
  
    npar1= npart+1
  
  !---------------------------------------------------
  
    x= relpos(3,irelpos)
    y= relpos(4,irelpos)
    i= nint(x)
    j= nint(y)
  
  !..compute height in model (eta/sigma) levels
  
    rt1=(tf2-tnow)/(tf2-tf1)
    rt2=(tnow-tf1)/(tf2-tf1)
    i= int(x)
    j= int(y)
    dxx=x-i
    dyy=y-j
    c1=(1.-dyy)*(1.-dxx)
    c2=(1.-dyy)*dxx
    c3=dyy*(1.-dxx)
    c4=dyy*dxx

    ps= rt1*(c1*ps1(i,j)  +c2*ps1(i+1,j) &
    +c3*ps1(i,j+1)+c4*ps1(i+1,j+1)) &
    +rt2*(c1*ps2(i,j)  +c2*ps2(i+1,j) &
    +c3*ps2(i,j+1)+c4*ps2(i+1,j+1))

    rtab=ps*pmult
    itab=rtab
    pihu=  pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)

    hlevel(1)= 0.
    hhu= 0.
  
    ginv=1./g
  
    do k=2,nk
    
      th= rt1*(c1*t1(i,j,  k)+c2*t1(i+1,j,  k) &
      +c3*t1(i,j+1,k)+c4*t1(i+1,j+1,k)) &
      +rt2*(c1*t2(i,j,  k)+c2*t2(i+1,j,  k) &
      +c3*t2(i,j+1,k)+c4*t2(i+1,j+1,k))
    
      p=ahalf(k)+bhalf(k)*ps
      rtab=p*pmult
      itab=rtab
      pih= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
    
      p=alevel(k)+blevel(k)*ps
      rtab=p*pmult
      itab=rtab
      pif= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
    
      h1=hhu
      h2=h1 + th*(pihu-pih)*ginv
      hlevel(k)= h1 + (h2-h1)*(pihu-pif)/(pihu-pih)
    
      hhu= h2
      pihu=pih
    
    end do
  
    if (hupper > hlevel(nk)) hupper= hlevel(nk)
  
  !---------------------------------------------------
  
  !..release types:
  !..1) one coloumn
  !..2) one cylinder
  !..3) two cylinders, mushroom (upside/down mushroom is possible!)
  
    if(stemradius <= 0. .OR. hlower < 1. .OR. radius <= 0.)then
    
      numradius=1
      hlower2(1)=hlower
      hupper2(1)=hupper
      radius2(1)=radius
    
      do m=1,ncomp
        nrel2(m,1)=nrel(m)
      end do
    
    else
    
      numradius=2
      hlower2(1)=0.
      hupper2(1)=hlower
      radius2(1)=stemradius
      hlower2(2)=hlower
      hupper2(2)=hupper
      radius2(2)=radius
    
      pi=3.141592654
      volume1=pi * stemradius**2 * hlower
      volume2=pi * radius**2 * (hupper - hlower)
    
      do m=1,ncomp
        nrel2(m,1)= nint(nrel(m)*volume1/(volume1+volume2))
        nrel2(m,2)= nrel(m)-nrel2(m,1)
      end do
    
    end  if
  
  !---------------------------------------------------
  
  !++++++++++++++++++++++++++++++++++++++++++
    do nrad=1,numradius
    !++++++++++++++++++++++++++++++++++++++++++
    
      radius=radius2(nrad)
      hlower=hlower2(nrad)
      hupper=hupper2(nrad)
    
      dxgrid= gparam(7)
      dygrid= gparam(8)
      dx= radius/(dxgrid/xm(i,j))
      dy= radius/(dygrid/ym(i,j))
    
      if (x-dx < 1.01 .OR. x+dx > nx-0.01 .OR. &
      y-dy < 1.01 .OR. y+dy > ny-0.01) then
        write(error_unit,*) 'RELEASE ERROR: Bad position'
        ierror=1
        return
      end if
    
      twodx= 2.*dx
      twody= 2.*dy
    
    
    !----------------------------------------------
      if (dx > 0.01 .AND. dy > 0.01) then
      !----------------------------------------------
      
      !..want an uniform spread in x,y,z within a real cylinder,
      !..i.e. usually an ellipse in the model grid
      !..(ellipse code from DIANA diFieldEdit.cc FieldEdit::editWeight,
      !.. and is probably more general than needed here (axis in x/y direction))
      
        if (dx >= dy) then
          e= sqrt(1.-(dy*dy)/(dx*dx))
        else
          e= sqrt(1.-(dx*dx)/(dy*dy))
        end if
        a= sqrt(dx*dx+dy*dy)
        b= a*sqrt(1.-e*e)
        c= 2.*sqrt(a*a-b*b)
        ecos= dx/a
        esin= dy/a
        en=   0.5*c/a
        en2=  en*en
        b2=   b*b
      
        do m=1,ncomp
        
          nprel= npart+nrel2(m,nrad)
        
          do while (npart < nprel)
          
          !..the rand function returns random real numbers between 0.0 and 1.0
            call random_number(rnd)
            xrand=rnd(1)
            yrand=rnd(2)
            zrand=rnd(3)
          
            dx= twodx*(xrand-0.5)
            dy= twody*(yrand-0.5)
          
            s= sqrt(dx*dx + dy*dy)
            gcos= dx/s
            gsin= dy/s
            rcos= gcos*ecos + gsin*esin
            smax= sqrt(b2/(1.-en2*rcos*rcos))
          
            if(s <= smax) then
            
              h= hlower + (hupper-hlower)*zrand
              k= 2
              do while (k < nk .AND. h > hlevel(k))
                k=k+1
              end do
              vlev= vlevel(k-1) + (vlevel(k)-vlevel(k-1)) &
              *(h-hlevel(k-1))/(hlevel(k)-hlevel(k-1))
            
              npart=npart+1
              pdata(npart)%x= x+dx
              pdata(npart)%y= y+dy
              pdata(npart)%z= vlev
              pdata(npart)%rad= pbq(m)
              pdata(npart)%grv= 0
              pdata(npart)%active = .TRUE. 
              icomp(npart)=   idefcomp(m)
            !..an unique particle identifier (for testing...)
              nparnum=nparnum+1
              iparnum(npart)=nparnum
            
            end if
          
          end do
        
        end do
      
      !----------------------------------------------
      else
      !----------------------------------------------
      
      !..distribution in a column (radius=0)
      
        do m=1,ncomp
        
          nprel= nrel2(m,nrad)
        
          do n=1,nprel
          
          !..the rand function returns random real numbers between 0.0 and 1.0
          !                zrand=rand()
            call random_number(zrand)
          
            h= hlower + (hupper-hlower)*zrand
            k= 2
            do while (k < nk .AND. h > hlevel(k))
              k=k+1
            end do
            vlev= vlevel(k-1) + (vlevel(k)-vlevel(k-1)) &
            *(h-hlevel(k-1))/(hlevel(k)-hlevel(k-1))
          
            npart=npart+1
            pdata(npart)%x= x
            pdata(npart)%y= y
            pdata(npart)%z= vlev
            pdata(npart)%rad= pbq(m)
            pdata(npart)%active = .TRUE. 
            icomp(npart)=   idefcomp(m)
          !..an unique particle identifier (for testing...)
            nparnum=nparnum+1
            iparnum(npart)=nparnum
          
          end do
        
        end do
      
      !----------------------------------------------
      end if
    !----------------------------------------------
    
    !++++++++++++++++++++++++++++++++++++++++++
    !.....end do nrad=1,numradius
    end do
  !++++++++++++++++++++++++++++++++++++++++++
  
    do n=1,ncomp
      m=idefcomp(n)
      totalbq(m)= totalbq(m) + pbq(n)*nrel(n)
      numtotal(m)=  numtotal(m) + nrel(n)
    end do
  
  !################################################################
  ! c   if(mod(istep,nsteph*3).eq.0) then
  !      if(mod(istep,nsteph).eq.0) then
    do n=1,ncomp
      m=idefcomp(n)
    ! c	  write(error_unit,*) 'comp,m,totalbq,numtotal: ',
    ! c  +			n,m,totalbq(m),numtotal(m)
      write(iulog,*) 'comp,m,totalbq,numtotal: ', &
      n,m,totalbq(m),numtotal(m)
    end do
  ! c	write(error_unit,*) 'nparnum: ',nparnum
    write(iulog,*) 'nparnum: ',nparnum
  !      end if
  !################################################################
  
    nplume=nplume+1
    iplume(1,nplume)=npar1
    iplume(2,nplume)=npart
  !.....end do ih=1,nrelheight
  end do

  write(iulog,*) '*RELEASE*  plumes,particles: ',nplume,npart
!#######################################################################
!     write(error_unit,*) '*RELEASE*  plumes,particles: ',nplume,npart
!#######################################################################

  ierror= 0

  return
end subroutine release
end module releaseML
