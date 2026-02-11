! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2026   Norwegian Meteorological Institute
! License: GNU GPL v3 or later


module forwrdML
  implicit none
  private

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
subroutine forwrd(tf1, tf2, tnow, tstep, part, pextra)
  USE iso_fortran_env, only: real64
  USE particleML, only: particle, extraParticle
  USE snapgrdML, only: vlevel
!> time in seconds for field set 1 (e.g. 0.)
  real, intent(in) :: tf1
!> time in seconds for field set 2 (e.g. 21600, if 6 hours)
  real, intent(in) :: tf2
!> time in seconds for current paricle positions
  real, intent(in) :: tnow
!> timestep in seconds
  real, intent(in) :: tstep
!> particle
  type(Particle), intent(inout) :: part
!> extra information for the particle (u, v, rm{x,y})
  type(extraParticle), intent(inout) :: pextra

  real(real64) :: dx1, dy1, dz1
  real u, v
#if defined(PETTERSEN)
  type(particle) :: nparticle
  real(real64) :: dx2, dy2, dz2
  real :: vmin, vmax
#endif

  call forwrd_dx(tf1,tf2,tnow,tstep,part, dx1, dy1, dz1, &
      u, v)
!..store u,v for rwalk
  pextra%u = u
  pextra%v = v
#if defined(PETTERSEN)
  nparticle = part
  nparticle%x = nparticle%x + dx1*pextra%rmx
  nparticle%y = nparticle%y + dy1*pextra%rmy
  nparticle%z = nparticle%z + dz1
  if (nparticle%x < 1. .OR. nparticle%y < 1. .OR. &
      nparticle%x > nx .OR. nparticle%y > ny) then
    nparticle%active = .FALSE.
    part = nparticle
  else
    vmin = vlevel(nk)
    vmax = vlevel( 1)
    if (nparticle%z > vmax) then
      nparticle%z = vmax
    else if (nparticle%z < vmin) then
      nparticle%z = vmin
    end if

    call forwrd_dx(tf1,tf2,tnow+tstep,tstep, &
        np, dx2, dy2, dz2, u, v)
    part%x = part%x + .5*(dx1 + dx2)*pextra%rmx
    part%y = part%y + .5*(dy1 + dy2)*pextra%rmy
    part%z = part%z + .5*(dz1 + dz2)
  endif
#else
  part%x = part%x + dx1*pextra%rmx
  part%y = part%y + dy1*pextra%rmy
  part%z = part%z + dz1
#endif
  part%z = min(part%z, dble(vlevel(1)))

end subroutine forwrd


!> Interpolation in 2D
!> private, inlined only for better readability
  pure real function interp(a00, a10, a01, a11, c1, c2, c3, c4)
    real, intent(in) :: a00, a10, a01, a11, c1, c2, c3, c4
    interp = c1*a00 + c2*a10 + c3*a01 + c4*a11
  end function interp

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
subroutine forwrd_dx(tf1, tf2, tnow, tstep, part, &
    delx, dely, delz, u, v)
  USE iso_fortran_env, only: real64
  USE particleML, only: particle
  USE snapgrdML, only: vlevel, vhalf, alevel, ahalf, blevel, bhalf, &
      ivlayer, ivlevel
  USE snapfldML, only: u1, u2, v1, v2, w1, w2, t1, t2, ps1, ps2
  USE snaptabML, only: cp, g, r, surface_height_sigma, exner
  USE vgravtablesML, only: vgrav
  USE snapdimML, only: nk
  USE snapparML, only: def_comp

!> time in seconds for field set 1 (e.g. 0.)
  real, intent(in) :: tf1
!> time in seconds for field set 2 (e.g. 21600, if 6 hours)
  real, intent(in) :: tf2
!> time in seconds for current paricle positions
  real, intent(in) :: tnow
!> timestep in seconds
  real, intent(in) :: tstep
!> particle
  type(Particle), intent(inout) :: part

!> delx: offset in x; without mapfactor
  real(real64), intent(out) :: delx
!> dely: offset in y; without mapfactor
  real(real64), intent(out) :: dely
!> delz: offset in z
  real(real64), intent(out) :: delz
!> wind-speed in x
  real, intent(out) :: u
!> wind-speed in y
  real, intent(out) :: v

  integer :: i,j,m,ilvl,k1,k2,kt1,kt2
  real :: dt,rt1,rt2,dx,dy,c1,c2,c3,c4,vlvl
  real :: dz1,dz2,ut1,ut2,vt1,vt2,wt1,wt2,w
  real :: th,tt1,tt2,ps,p,pi,t,gravity
  real :: pi1,pi2,dz,deta,wg

  real, parameter :: ginv = 1.0/g
  real, parameter :: cpinv = 1.0/cp
  real, parameter :: rcpinv = cp/r


  dt = tstep


!..for linear interpolation in time
  rt1 = (tf2-tnow)/(tf2-tf1)
  rt2 = (tnow-tf1)/(tf2-tf1)


!..for horizontal interpolations
  i = part%x
  j = part%y
  dx = part%x-i
  dy = part%y-j
  c1 = (1.-dy)*(1.-dx)
  c2 = (1.-dy)*dx
  c3 = dy*(1.-dx)
  c4 = dy*dx

!..for vertical interpolation (sigma/eta levels)
  vlvl = part%z
  ilvl = vlvl*10000.
  k1 = ivlevel(ilvl)
  k2 = k1 + 1
  dz1 = (vlvl-vlevel(k2))/(vlevel(k1)-vlevel(k2))
  dz2 = 1.0 - dz1

!..interpolation

!..u
  ut1 = dz1*(interp(u1(i,j,k1), u1(i+1,j,k1), u1(i,j+1,k1), u1(i+1,j+1,k1), c1, c2, c3, c4)) &
      +dz2*(interp(u1(i,j,k2), u1(i+1,j,k2), u1(i,j+1,k2), u1(i+1,j+1,k2), c1, c2, c3, c4))
  ut2 = dz1*(interp(u2(i,j,k1), u2(i+1,j,k1), u2(i,j+1,k1), u2(i+1,j+1,k1), c1, c2, c3, c4))&
      +dz2*(interp(u2(i,j,k2), u2(i+1,j,k2), u2(i,j+1,k2), u2(i+1,j+1,k2), c1, c2, c3, c4))
  u = ut1*rt1 + ut2*rt2

  !..v
  vt1 = dz1*(interp(v1(i,j,k1), v1(i+1,j,k1), v1(i,j+1,k1), v1(i+1,j+1,k1), c1, c2, c3, c4)) &
      +dz2*(interp(v1(i,j,k2), v1(i+1,j,k2), v1(i,j+1,k2), v1(i+1,j+1,k2), c1, c2, c3, c4))
  vt2 = dz1*(interp(v2(i,j,k1), v2(i+1,j,k1), v2(i,j+1,k1), v2(i+1,j+1,k1), c1, c2, c3, c4)) &
      +dz2*(interp(v2(i,j,k2), v2(i+1,j,k2), v2(i,j+1,k2), v2(i+1,j+1,k2), c1, c2, c3, c4))
  v = vt1*rt1 + vt2*rt2
!..w
  wt1 = dz1*(interp(w1(i,j,k1), w1(i+1,j,k1), w1(i,j+1,k1), w1(i+1,j+1,k1), c1, c2, c3, c4)) &
      +dz2*(interp(w1(i,j,k2), w1(i+1,j,k2), w1(i,j+1,k2), w1(i+1,j+1,k2), c1, c2, c3, c4))
  wt2 = dz1*(interp(w2(i,j,k1), w2(i+1,j,k1), w2(i,j+1,k1), w2(i+1,j+1,k1), c1, c2, c3, c4)) &
      +dz2*(interp(w2(i,j,k2), w2(i+1,j,k2), w2(i,j+1,k2), w2(i+1,j+1,k2), c1, c2, c3, c4))
  w = wt1*rt1 + wt2*rt2

  m = part%icomp

  if(def_comp(m)%grav_type > 0) then

  !..potential temperature (no pot.temp. at surface...)
    kt1 = max(k1, 2)
    kt2 = kt1 + 1
    tt1 = dz1*(interp(t1(i,j,  kt1), t1(i+1,j,  kt1), t1(i,j+1,kt1), t1(i+1,j+1,kt1), c1, c2, c3, c4)) &
        +dz2*(interp(t2(i,j,  kt1), t2(i+1,j,  kt1), t2(i,j+1,kt1), t2(i+1,j+1,kt1), c1, c2, c3, c4))
    tt2 = dz1*(interp(t1(i,j,  kt2), t1(i+1,j,  kt2), t1(i,j+1,kt2), t1(i+1,j+1,kt2), c1, c2, c3, c4)) &
        +dz2*(interp(t2(i,j,  kt2), t2(i+1,j,  kt2), t2(i,j+1,kt2), t2(i+1,j+1,kt2), c1, c2, c3, c4))
    th = tt1*rt1 + tt2*rt2

  !..pressure
    ps = rt1*(interp(ps1(i,j), ps1(i+1,j), ps1(i,j+1), ps1(i+1,j+1), c1, c2, c3, c4)) &
        +rt2*(interp(ps2(i,j), ps2(i+1,j), ps2(i,j+1), ps2(i+1,j+1), c1, c2, c3, c4))

    if(def_comp(m)%grav_type == 2) then
      p = alevel(k1) + blevel(k1)*ps
      pi1 = exner(p)
      p = alevel(k2) + blevel(k2)*ps
      pi2 = exner(p)
      pi = pi1*dz1+pi2*dz2
      t = th*pi*cpinv
      p = 1000.*(pi*cpinv)**rcpinv

      gravity = vgrav(def_comp(m)%to_running, real(p), real(t))
    !######################################################################
    !     if(np.lt.21) write(error_unit,*) '  p,t,gravity: ',p,t,gravity
    !######################################################################
    else
      gravity = def_comp(m)%gravityms
    end if

    part%grv = gravity
    if (def_comp(part%icomp)%kdrydep == 1 .and. part%z > surface_height_sigma) then
      ! Gravitional settling should only be handled in the dry deposition module
      ! for particles in the surface layer
      wg = 0.0
    else
    !..gravity ... a very simple, probably too simple (!!!) conversion
    !............. from m/s to model etadot/sigmadot "vertical velocity"
      k1 = ivlayer(ilvl)
    !.......................................???????????????????????????????
      if (k1 == nk) k1 = k1-1
    !.......................................???????????????????????????????
      k2 = k1 + 1
      p = ahalf(k1) + bhalf(k1)*ps
      pi1 = exner(p)
      p = ahalf(k2) + bhalf(k2)*ps
      pi2 = exner(p)
      dz = th*(pi1-pi2)*ginv
      deta = vhalf(k1)-vhalf(k2)
      wg = gravity*deta/dz
    endif
  !######################################################################
  !   if(np.lt.21) write(error_unit,*) 'np,k2,w,wg: ',np,k2,w,wg
  !######################################################################
    w = w + wg
  end if


!..update position

  delx=u*dt
  dely=v*dt
  delz=w*dt

  return
end subroutine forwrd_dx

end module forwrdML
