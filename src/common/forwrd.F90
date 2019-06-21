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

module forwrdML
  implicit none
  private

#if defined(TRAJ)
!> calculation of distance and speed
  real, save, public :: speed
#endif

  public forwrd

  contains

!> Purpose:  Move all particles one timestep forward
!>
!> Notes:
!>   - sigma levels (norlam) or eta levels (hirlam,...)
!>     defined by alevel and blevel
!>   - horizontal wind components in unit m/s
!>   - vertical   wind component  in unit vlevel/second
!>     (vlevel = sigma or eta(alevel,blevel))
!>   - all wind components in non-staggered horizontal grid
!>     and in the same levels
!>   - lower model level is level 2
subroutine forwrd(tf1, tf2, tnow, tstep, np, pextra)
  USE iso_fortran_env, only: real64
  USE particleML, only: pdata, particle, extraParticle
  USE snapgrdML, only: vlevel
!> time in seconds for field set 1 (e.g. 0.)
  real, intent(in) :: tf1
!> time in seconds for field set 2 (e.g. 21600, if 6 hours)
  real, intent(in) :: tf2
!> time in seconds for current paricle positions
  real, intent(in) :: tnow
!> timestep in seconds
  real, intent(in) :: tstep
!> particle loop index, np = 0 is initalization
  integer, intent(in) :: np

  type(extraParticle), intent(inout) :: pextra

  real(real64) :: dx1, dy1, dz1, u, v
#if defined(PETTERSEN)
  type(particle) :: nparticle
  real(real64) :: dx2, dy2, dz2
  real :: vmin, vmax
#endif


  if (np == 0) then
  ! init
    call forwrd_dx(tf1,tf2,tnow,tstep,np,dx1,dy1,dz1,u,v)
    return
  endif

  call forwrd_dx(tf1,tf2,tnow,tstep,np, dx1, dy1, dz1, &
      u, v)
!..store u,v for rwalk
  pextra%u=u
  pextra%v=v
#if defined(TRAJ)
  speed=hypot(u, v)
#endif
#if defined(PETTERSEN)
  nparticle = pdata(np)
  nparticle%x=nparticle%x+dx1*pextra%rmx
  nparticle%y=nparticle%y+dy1*pextra%rmy
  nparticle%z=nparticle%z+dz1
  if (nparticle%x < 1. .OR. nparticle%y < 1. .OR. &
      nparticle%x > nx .OR. nparticle%y > ny) then
    nparticle%active = .FALSE.
    pdata(np) = nparticle
  else
    vmin=vlevel(nk)
    vmax=vlevel( 1)
    if (nparticle%z > vmax) then
      nparticle%z = vmax
    else if (nparticle%z < vmin) then
      nparticle%z = vmin
    end if

    call forwrd_dx(tf1,tf2,tnow+tstep,tstep, &
        np, dx2, dy2, dz2, u, v)
    pdata(np)%x=pdata(np)%x+.5*(dx1 + dx2)*pextra%rmx
    pdata(np)%y=pdata(np)%y+.5*(dy1 + dy2)*pextra%rmy
    pdata(np)%z=pdata(np)%z+.5*(dz1 + dz2)
  endif
#else
  pdata(np)%x=pdata(np)%x+dx1*pextra%rmx
  pdata(np)%y=pdata(np)%y+dy1*pextra%rmy
  pdata(np)%z=pdata(np)%z+dz1
#endif
  if(pdata(np)%z > vlevel(1)) pdata(np)%z=vlevel( 1)

  return
end subroutine forwrd


!> Purpose:  calculate dx,dy,dz forward movement of particle at pos np
!>
!> Notes:
!>   - sigma levels (norlam) or eta levels (hirlam,...)
!>     defined by alevel and blevel
!>   - horizontal wind components in unit m/s
!>   - vertical   wind component  in unit vlevel/second
!>     (vlevel = sigma or eta(alevel,blevel))
!>   - all wind components in non-staggered horizontal grid
!>     and in the same levels
!>   - lower model level is level 2
subroutine forwrd_dx(tf1, tf2, tnow, tstep, np, &
    delx, dely, delz, u, v)
  USE iso_fortran_env, only: real64
  USE particleML, only: pdata, particle
  USE snapgrdML, only: gparam, vlevel, vhalf, alevel, ahalf, blevel, bhalf, &
      ivlayer, ivlevel
  USE snapfldML, only: u1, u2, v1, v2, w1, w2, t1, t2, ps1, ps2
  USE snapparML, only: ncomp, idefcomp, kgravity, compname, icomp, gravityms
  USE snaptabML, only: cp, g, pmult, r, pitab
  USE vgravtablesML, only: vgravtables, vgtable, pbasevg, tbasevg, pincrvg, tincrvg
  USE snapdimML, only: nk
  USE snapdebug, only: iulog

!> time in seconds for field set 1 (e.g. 0.)
  real, intent(in) :: tf1
!> time in seconds for field set 2 (e.g. 21600, if 6 hours)
  real, intent(in) :: tf2
!> time in seconds for current paricle positions
  real, intent(in) :: tnow
!> timestep in seconds
  real, intent(in) :: tstep
!> particle loop index, np = 0 is initalisation
  integer, intent(in) :: np

!> delx: offset in x; without mapfactor
  real(real64), intent(out) :: delx
!> dely: offset in y; without mapfactor
  real(real64), intent(out) :: dely
!> delz: offset in z
  real(real64), intent(out) :: delz
!> wind-speed in x
  real(real64), intent(out) :: u
!> wind-speed in y
  real(real64), intent(out) :: v

  integer :: i,j,m,ilvl,k1,k2,itab,kt1,kt2,ip,it
  real(real64) ::    dt,rt1,rt2,dx,dy,c1,c2,c3,c4,vlvl
  real(real64) ::    dz1,dz2,uk1,uk2,vk1,vk2,wk1,wk2,w
  real(real64) ::    th,tk1,tk2,ps,p,pi,t,gravity,grav1,grav2,pvg,tvg
  real(real64) ::    pi1,pi2,dz,deta,wg,rtab
  logical :: compute_grav_table
! c

  real(real64), parameter :: ginv = 1.0/g
  real(real64), parameter :: cpinv = 1.0/cp
  real(real64), parameter :: rcpinv = cp/r
  real(real64), save :: vmax, dxgrid,dygrid



! initialization
  if (np == 0) then
    compute_grav_table = .false.
    grav_do: do i=1,ncomp
      m= idefcomp(i)
      if(kgravity(m) > 1) then
        compute_grav_table = .true.
        exit grav_do
      endif
    end do grav_do

    if (compute_grav_table) then
      write(iulog,*) 'Computing gravity tables...'
      call vgravtables()
      write(iulog,*) 'Surface gravity (1000hPa, 300K):'
      it = (300-tbasevg)/tincrvg
      ip = (1000-pbasevg)/pincrvg
      do i=1,ncomp
        m= idefcomp(i)
        write(iulog,*) ' particle ', compname(m), ": ", vgtable(it,ip,m)
      end do
    else
      write(iulog,*) 'Computation of gravity tables not needed'
    end if
    dxgrid=gparam(7)
    dygrid=gparam(8)
    vmax=vlevel( 1)
    return

  end if

  dt=tstep


!..for linear interpolation in time
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)


!..for horizontal interpolations
  i=pdata(np)%x
  j=pdata(np)%y
  dx=pdata(np)%x-i
  dy=pdata(np)%y-j
  c1=(1.-dy)*(1.-dx)
  c2=(1.-dy)*dx
  c3=dy*(1.-dx)
  c4=dy*dx

!..for vertical interpolation (sigma/eta levels)
  vlvl=pdata(np)%z
  ilvl=vlvl*10000.
  k1=ivlevel(ilvl)
  k2=k1+1
  dz1=(vlvl-vlevel(k2))/(vlevel(k1)-vlevel(k2))
  dz2=1.-dz1

!..interpolation

!..u
  uk1= rt1*(c1*u1(i,j,k1)  +c2*u1(i+1,j,k1) &
      +c3*u1(i,j+1,k1)+c4*u1(i+1,j+1,k1)) &
      +rt2*(c1*u2(i,j,k1)  +c2*u2(i+1,j,k1) &
      +c3*u2(i,j+1,k1)+c4*u2(i+1,j+1,k1))
  uk2= rt1*(c1*u1(i,j,k2)  +c2*u1(i+1,j,k2) &
      +c3*u1(i,j+1,k2)+c4*u1(i+1,j+1,k2)) &
      +rt2*(c1*u2(i,j,k2)  +c2*u2(i+1,j,k2) &
      +c3*u2(i,j+1,k2)+c4*u2(i+1,j+1,k2))
  u=uk1*dz1+uk2*dz2
!..v
  vk1= rt1*(c1*v1(i,j,k1)  +c2*v1(i+1,j,k1) &
      +c3*v1(i,j+1,k1)+c4*v1(i+1,j+1,k1)) &
      +rt2*(c1*v2(i,j,k1)  +c2*v2(i+1,j,k1) &
      +c3*v2(i,j+1,k1)+c4*v2(i+1,j+1,k1))
  vk2= rt1*(c1*v1(i,j,k2)  +c2*v1(i+1,j,k2) &
      +c3*v1(i,j+1,k2)+c4*v1(i+1,j+1,k2)) &
      +rt2*(c1*v2(i,j,k2)  +c2*v2(i+1,j,k2) &
      +c3*v2(i,j+1,k2)+c4*v2(i+1,j+1,k2))
  v=vk1*dz1+vk2*dz2
!..w
  wk1= rt1*(c1*w1(i,j,k1)  +c2*w1(i+1,j,k1) &
      +c3*w1(i,j+1,k1)+c4*w1(i+1,j+1,k1)) &
      +rt2*(c1*w2(i,j,k1)  +c2*w2(i+1,j,k1) &
      +c3*w2(i,j+1,k1)+c4*w2(i+1,j+1,k1))
  wk2= rt1*(c1*w1(i,j,k2)  +c2*w1(i+1,j,k2) &
      +c3*w1(i,j+1,k2)+c4*w1(i+1,j+1,k2)) &
      +rt2*(c1*w2(i,j,k2)  +c2*w2(i+1,j,k2) &
      +c3*w2(i,j+1,k2)+c4*w2(i+1,j+1,k2))
  w=wk1*dz1+wk2*dz2

  m=icomp(np)

  if(kgravity(m) > 0) then

  !..potential temperature (no pot.temp. at surface...)
    kt1= max(k1,2)
    kt2= kt1+1
    tk1= rt1*(c1*t1(i,j,  kt1)+c2*t1(i+1,j,  kt1) &
        +c3*t1(i,j+1,kt1)+c4*t1(i+1,j+1,kt1)) &
        +rt2*(c1*t2(i,j,  kt1)+c2*t2(i+1,j,  kt1) &
        +c3*t2(i,j+1,kt1)+c4*t2(i+1,j+1,kt1))
    tk2= rt1*(c1*t1(i,j,  kt2)+c2*t1(i+1,j,  kt2) &
        +c3*t1(i,j+1,kt2)+c4*t1(i+1,j+1,kt2)) &
        +rt2*(c1*t2(i,j,  kt2)+c2*t2(i+1,j,  kt2) &
        +c3*t2(i,j+1,kt2)+c4*t2(i+1,j+1,kt2))
    th=tk1*dz1+tk2*dz2

  !..pressure
    ps= rt1*(c1*ps1(i,j)  +c2*ps1(i+1,j) &
        +c3*ps1(i,j+1)+c4*ps1(i+1,j+1)) &
        +rt2*(c1*ps2(i,j)  +c2*ps2(i+1,j) &
        +c3*ps2(i,j+1)+c4*ps2(i+1,j+1))

    if(kgravity(m) == 2) then
      p= alevel(k1) + blevel(k1)*ps
      rtab=p*pmult
      itab=int(rtab)
      pi1= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
      p= alevel(k2) + blevel(k2)*ps
      rtab=p*pmult
      itab=rtab
      pi2= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
      pi= pi1*dz1+pi2*dz2
      t=th*pi*cpinv
      p= 1000.*(pi*cpinv)**rcpinv
    ! old       gravity= vgrav(radiusmym(m),densitygcm3(m),p,t)
      ip= (p-pbasevg)/pincrvg
      if(ip < 1) ip=1
      if(ip >= size(vgtable,2)) ip=size(vgtable,2)-1
      pvg= pbasevg + ip*pincrvg
      it= (t-tbasevg)/tincrvg
      if(it < 1) it=1
      if(it >= size(vgtable,1)) it=size(vgtable,1)-1
      tvg= tbasevg + it*tincrvg
      grav1= vgtable(it,ip,m) &
          + (vgtable(it+1,ip,m)-vgtable(it,ip,m)) &
          *(t-tvg)/tincrvg
      ip= ip+1
      grav2= vgtable(it,ip,m) &
          + (vgtable(it+1,ip,m)-vgtable(it,ip,m)) &
          *(t-tvg)/tincrvg
      gravity= grav1 + (grav2-grav1) * (p-pvg)/pincrvg
    !######################################################################
    !     if(np.lt.21) write(error_unit,*) '  p,t,gravity: ',p,t,gravity
    !######################################################################
    else
      gravity= gravityms(m)
    end if

    pdata(np)%grv= gravity


  !..gravity ... a very simple, probably too simple (!!!) conversion
  !............. from m/s to model etadot/sigmadot "vertical velocity"
    k1=ivlayer(ilvl)
  !.......................................???????????????????????????????
    if (k1 == nk) k1=k1-1
  !.......................................???????????????????????????????
    k2=k1+1
    p=ahalf(k1)+bhalf(k1)*ps
    rtab=p*pmult
    itab=rtab
    pi1= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
    p=ahalf(k2)+bhalf(k2)*ps
    rtab=p*pmult
    itab=rtab
    pi2= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
    dz= th*(pi1-pi2)*ginv
    deta= vhalf(k1)-vhalf(k2)
    wg= gravity*deta/dz
  !######################################################################
  !   if(np.lt.21) write(error_unit,*) 'np,k2,w,wg: ',np,k2,w,wg
  !######################################################################
#if !defined(TRAJ)
    w= w + wg
#endif
  end if


!..update position

  delx=u*dt
  dely=v*dt
  delz=w*dt

  return
end subroutine forwrd_dx
end module forwrdML
