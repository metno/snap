c
c  milib
c  
c  $Id$
c
c  Copyright (C) 2006 met.no
c
c  Contact information:
c  Norwegian Meteorological Institute
c  Box 43 Blindern
c  0313 OSLO
c  NORWAY
c  email: diana@met.no
c  
c  This library is free software; you can redistribute it and/or
c  modify it under the terms of the GNU Lesser General Public
c  License as published by the Free Software Foundation; either
c  version 2.1 of the License, or (at your option) any later version.
c
c  This library is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
c  Lesser General Public License for more details.
c  
c  You should have received a copy of the GNU Lesser General Public
c  License along with this library; if not, write to the Free Software
c  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c
      subroutine mapfield(imapr,icori,igtype,grid,nx,ny,xm,ym,fc,
     +                    hx,hy,ierror)
c
c  NAME:
c     mapfield
c
c  PURPOSE:
c     Compute parameters (fields) dependant of the map projection,
c     map ratio (in x and y direction) and coriolis parameter.
c
c  SYNOPSIS:
c     subroutine mapfield(imapr,icori,igtype,grid,nx,ny,xm,ym,fc,
c    +                          hx,hy,ierror)
c     integer imapr,icori,igtype,nx,ny,ierror
c     real    grid(6),xm(nx,ny),ym(nx,ny),fc(nx,ny),hx,hy
c
c  INPUT:
c     imapr  - 0 : not compute map ratio
c              1 : compute map ratio
c              2,... : compute map ratio divided by grid resolution
c                       in meter times imapr-1
c     icori  - 0 : not compute coriolis parameter
c              1 : compute coriolis parameter
c     igtype - grid type, 1 = polarstereographic grid (true at 60 deg. N)
c                          2 = geographic
c                          3 = spherical rotated grid
c                          4 = polarstereographic grid
c                          5 = mercator grid (unrotated)
c                         6 = lambert (tangent,non-oblique) grid
c     grid   - grid parameters
c                igtype=1,4, polarstereographic grid:
c                           grid(1) = x position of north pole (xp)
c                           grid(2) = y position of north pole (yp)
c                           grid(3) = no. of grid units between
c                                     North Pole and Equator
c                           grid(4) = grid rotation angle (degrees),
c                                     positive east, negative west
c                           grid(5) = projection latitude
c                                     (degrees) standard is 60 (60 deg. N)
c                           grid(6) = 0.  (not used)
c                igtype=2,3, geographic or spherical rotated grid:
c                           grid(1) = western boundary (degrees)
c                                     (longitude for x=1)
c                           grid(2) = southern boundary (degrees)
c                                     (latitude for y=1)
c                           grid(3) = longitude increment (degrees)
c                           grid(4) = latitude  increment (degrees)
c                           grid(5) = longitude position of rotated equator
c                                     (degrees)  (0 if geographic grid)
c                           grid(6) = latitude  position of rotated equator
c                                     (degrees)  (0 if geographic grid)
c                igtype=5, mercator (unrotated) grid:
c                           grid(1) = western boundary (degrees)
c                                     (longitude for x=1)
c                           grid(2) = southern boundary (degrees)
c                                     (latitude for y=1)
c                           grid(3) = x (longitude) increment (km)
c                           grid(4) = y (latitude)  increment (km)
c                           grid(5) = reference (construction) latitude
c                                    (degrees)
c                           grid(6) = 0.  (not used)
c                igtype=6, lambert (tangent,non-oblique) grid:
c                           grid(1) = west (degrees)
c                                     (longitude for x=1,y=1)
c                           grid(2) = south (degrees)
c                                     (latitude for x=1,y=1)
c                           grid(3) = x (easting) increment (km)
c                           grid(4) = y (northing) increment (km)
c                           grid(5) = reference longitude (degrees)
c                           grid(6) = reference (tangent) latitude (degrees)
c     nx     - no. of gridpoints in x direction
c     ny     - no. of gridpoints in y direction
c
c  OUTPUT:
c     xm     - map ratio in x direction (if imapr>0)
c     ym     - map ratio in x direction (if imapr>0)
c     fc     - coriolis parameter (if icori>0)
c     hx     - grid resolution in meter in x direction (map ratio = 1)
c     hy     - grid resolution in meter in y direction (map ratio = 1)
c     ierror - error status: 0 = no error
c                             1 = bad value in input grid array
c                             2 = unknown grid type (igtype)
c                            3 = internal conversion error
c
c  NOTES:
c     To avoid division by zero in calling routines, some uncorrect
c     values may be returned:
c     1) Coriolis parameter (all grid types):
c         The minimum value is computed 1/100 grid unit from equator.
c         Correct value is 0 at equator.
c     2) Map ratio (x or longitude direction), geographic and spherical
c         rotated grids:
c         The minimum value is computed 1/100 grid unit from the pole.
c         Correct value is infinite at the pole.
c     Example of map ratio usage, xm,ym computed with imapr=1:
c        x_gradient = xm(i,j)*(field(i+1,j)-field(i-1,j))/(hx*2.) 
c        y_gradient = ym(i,j)*(field(i,j+1)-field(i,j-1))/(hy*2.)
c     Example of map ratio usage, xm,ym computed with imapr=3:
c        x_gradient = xm(i,j)*(field(i+1,j)-field(i-1,j))
c        y_gradient = ym(i,j)*(field(i,j+1)-field(i,j-1))
c
c-----------------------------------------------------------------------
c  DNMI/FoU  09.06.1995  Anstein Foss
c  DNMI/FoU  31.10.1995  Anstein Foss
c  DNMI/FoU  04.06.1996  Anstein Foss ... mercator (unrotated)
c met.no/FoU 03.05.2005  Anstein Foss ... float() -> real()
c met.no/FoU 17.11.2008  Ole Vignes ..... lambert (tangent, non-oblique)
c-----------------------------------------------------------------------
c
      integer imapr,icori,igtype,nx,ny,ierror
      real    grid(6),xm(nx,ny),ym(nx,ny),fc(nx,ny),hx,hy
c
      parameter (nmax=1000)
      real      flon(nmax),flat(nmax)
c
      ierror=0
c
      if((igtype.eq.1 .or. igtype.eq.4) .and.
     +   (grid(3).eq.0. .or. grid(5).eq.0.)) ierror=1
      if((igtype.eq.2 .or. igtype.eq.3) .and. 
     +   (grid(3).eq.0. .or. grid(4).eq.0.)) ierror=1
      if(igtype.eq.5 .and. 
     +   (grid(3).eq.0. .or. grid(4).eq.0.)) ierror=1
      if(igtype.eq.5 .and. grid(6).ne.0.) ierror=1
c
      if(ierror.ne.0) return
c
c..earth radius (m)
ccc   rearth = 6.371e+6
      call earthr(rearth)
c
      zpi    = 2.0*asin(1.0)
      zpir18 = zpi/180.
      zfc    = 2.0*0.7292e-4
c
      if(igtype.eq.1 .or. igtype.eq.4) then
c
c..polarstereographic grid
c
        xp=grid(1)
        yp=grid(2)
        an=grid(3)
        fi=grid(4)
        fp=grid(5)
        fq=1.0+sin(fp*zpir18)
c..resolution
        dh=rearth*fq/an
        an2=an*an
        xm1=fq*0.5/an2
c..avoiding problems in calling routines (1/fc), coriolis at equator
        rpol2=(an-0.01)*(an-0.01)
        fc00=zfc*(an2-rpol2)/(an2+rpol2)
c
        if(imapr.gt.0) then
c..map_ratio (possibly divided by n*dh)
          s=xm1
          if(imapr.gt.1) s=s/(dh*real(imapr-1))
          do j=1,ny
            do i=1,nx
              rpol2=(i-xp)*(i-xp)+(j-yp)*(j-yp)
              xm(i,j)=s*(an2+rpol2)
                ym(i,j)=xm(i,j)
            end do
          end do
        end if
c
        if(icori.gt.0) then
c..coriolis parameter
          do j=1,ny
            do i=1,nx
              rpol2=(i-xp)*(i-xp)+(j-yp)*(j-yp)
              fc(i,j)=zfc*(an2-rpol2)/(an2+rpol2)
              fc(i,j)=max(fc(i,j),fc00)
            end do
          end do
        end if
c
        hx=dh
        hy=dh
c
      elseif(igtype.eq.2 .or. igtype.eq.3) then
c
c..geographic or spherical rotated grid
c
        west =grid(1)*zpir18
        south=grid(2)*zpir18
        dlon =grid(3)*zpir18
        dlat =grid(4)*zpir18
        xcen =grid(5)*zpir18
        ycen =grid(6)*zpir18
        hlon =rearth*dlon
        hlat =rearth*dlat
        sx=1.
        sy=1.
        if(imapr.gt.1) then
          sx=sx/(hlon*real(imapr-1))
          sy=sy/(hlat*real(imapr-1))
        end if
c..avoiding problems in calling routines (1/xm), map ratio at the poles
        clat90=cos((90.0-0.01*abs(grid(4)))*zpir18)
c..avoiding problems in calling routines (1/fc), coriolis at equator
        slat00=sin((00.0+0.01*abs(grid(4)))*zpir18)
c
        do j=1,ny
          do i1=1,nx,nmax
c
            i0=i1-1
            i2=min(i0+nmax,nx)
            do i=i1,i2
              flon(i-i0)=west +(i-1)*dlon
              flat(i-i0)=south+(j-1)*dlat
            end do
c
            if(imapr.gt.0) then
c..map_ratio (possibly divided by n*hlon and n*hlat)
              do i=i1,i2
                clat=cos(flat(i-i0))
                clat=max(clat,clat90)
                xm(i,j)=sx/clat
                ym(i,j)=sy
              end do
            end if
c
            if(icori.gt.0) then
c..coriolis parameter
              if(xcen.ne.0.0 .or. ycen.ne.0.0) then
                call sph2rot(-1,i2-i0,flon,flat,xcen,ycen,ierror)
                if(ierror.ne.0) then
                  ierror=3
                  return
                end if
              end if
              do i=i1,i2
                slat=sin(flat(i-i0))
                if(abs(slat).lt.slat00) then
                  if(slat.ge.0.0) then
                    slat=slat00
                  else
                    slat=-slat00
                  end if
                end if
                fc(i,j)=zfc*slat
              end do
            end if
c
          end do
        end do
c
        hx=hlon
        hy=hlat
c
      elseif(igtype.eq.5 .or. igtype.eq.6) then
c
c..mercator or lambert grid
c
        xw   =grid(1)*zpir18
        ys   =grid(2)*zpir18
        dx   =grid(3)*1000.
        dy   =grid(4)*1000.
        if (igtype.eq.5) then
           yc = grid(5)*zpir18
        else
           xc = grid(5)*zpir18
           yc = grid(6)*zpir18
        endif
        sx=cos(yc)
        sy=sx
        if(imapr.gt.1) then
          sx=sx/(dx*real(imapr-1))
          sy=sy/(dy*real(imapr-1))
        end if
c..avoiding problems in calling routines (1/xm), map ratio at the poles
        clat90=cos(89.999*zpir18)
c..avoiding problems in calling routines (1/fc), coriolis at equator
        slat00=sin(00.001*zpir18)
c
        do j=1,ny
          do i1=1,nx,nmax
c
            i0=i1-1
            i2=min(i0+nmax,nx)
            do i=i1,i2
              flon(i-i0)=i
              flat(i-i0)=j
            end do
c
            if (igtype.eq.5) then
               call mer2sph(+1,i2-i0,flon,flat,xw,ys,dx,dy,yc,ierror)
            else
               call lam2sph(+1,i2-i0,flon,flat,xw,ys,dx,dy,xc,yc,yc,
     +                      ierror)
            endif
            if(ierror.ne.0) then
              ierror=3
              return
            end if
c
            if(imapr.gt.0) then
c..map_ratio (possibly divided by n*dx and n*dy)
              do i=i1,i2
                clat=cos(flat(i-i0))
                clat=max(clat,clat90)
                xm(i,j)=sx/clat
                ym(i,j)=sy/clat
              end do
            end if
c
            if(icori.gt.0) then
c..coriolis parameter
              do i=i1,i2
                slat=sin(flat(i-i0))
                if(abs(slat).lt.slat00) then
                  if(slat.ge.0.0) then
                    slat=slat00
                  else
                    slat=-slat00
                  end if
                end if
                fc(i,j)=zfc*slat
              end do
            end if
c
          end do
        end do
c
        hx=dx
        hy=dy
c
      else
c
        ierror=2
c
      end if
c
      return
      end
