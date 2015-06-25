c
c  milib
c
c  $Id: lam2sph.f 2308 2009-01-08 10:19:22Z olev $
c
c  Copyright (C) 2008 met.no
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
      subroutine lam2sph(icall,n,x,y,xw,ys,dx,dy,x0,y1,y2,ierror)
c
c  conversion between Lambert (non-oblique) and spherical
c  coordinates. Lambert coordinates are given in real
c  numbers, where (xlam,ylam)=(1.0,1.0) in lower left corner of
c  the Lambert grid. spherical coordinates are given in radians.
c  xw,ys,x0,y1,y2 (x.west,y.south,x.ref,y.secant1,y.secant2) given in radians
c  and dx,dy (x and y resolution at y.secant1) in meters.
c
c---------------------------------------------------------------------
c met.no/FoU  13.11.2008  Ole Vignes ... double precision computations
c---------------------------------------------------------------------
c
      implicit none
c
      integer icall, n, ierror
      real    x(n), y(n), xw, ys, dx, dy, x0, y1, y2
c
      integer j
      real    rearth
      double precision zxw, zys, zdx, zdy, zx0, zy1, zy2, yy, ftan, zpi,
     +                 zpiq, zn, zninv, zF, zR0, zRe, zR, ztheta, ztn,
     +                 zxlam0, zylam0, xsph, ysph, xlam, ylam, ztwopi
c
c utility statement function
c
      ftan(yy) = tan(zpiq + 0.5d0*yy)
c pi
      zpiq = atan(1.0d0)
      zpi = 4.0d0*zpiq
      ztwopi = 2.0d0*zpi
c
      ierror = 0
c
      call earthr(rearth)
      zRe = rearth
      zxw = xw
      zys = ys
      zdx = dx
      zdy = dy
      zx0 = x0
      zy1 = y1
      zy2 = y2
c
c  test if tangent or secant projection
c
      if (abs(zy1-zy2) .lt. 1.0d-5) then
         ! tangent version
         zn = sin(zy1)
      else
         ! secant version
         zn = log(cos(zy1)/cos(zy2)) / log(ftan(zy2)/ftan(zy1))
      endif
c
c projection constants
c
      zninv = 1.0d0 / zn
      ztn   = ftan(zy1)**zn
      zF    = cos(zy1)*ztn*zninv
      zR0   = zRe*zF / ztn
c
c find Lambert coordinates of lower left corner (0,0) in grid
c
      zR = zRe*zF / (ftan(zys)**zn)
      ztheta = zn*(zxw - zx0)
      zxlam0 = zR*sin(ztheta) - zdx
      zylam0 = zR0 - zR*cos(ztheta) - zdy
c
      if (icall.eq.1) then
c
c  compute spherical coordinates as function of
c  lambert (non-oblique) coordinates
c
         do j = 1,n
	    xlam = zxlam0 + x(j) * zdx
	    ylam = zylam0 + y(j) * zdy
            zR = sign(1.0d0,zn)*sqrt(xlam*xlam + (zR0-ylam)*(zR0-ylam))
            ztheta = atan2(xlam,zR0-ylam)
	    xsph = zx0 + ztheta*zninv
	    ysph = 2.0d0*(atan((zRe*zF/zR)**zninv) - zpiq)
            if(xsph.lt.-zpi) xsph=xsph+ztwopi
            if(xsph.gt.+zpi) xsph=xsph-ztwopi
            x(j) = xsph
            y(j) = ysph
         enddo
c
      elseif (icall.eq.-1) then
c
c  compute lambert (non-oblique) coordinates as function of
c  spherical coordinates
c
         do j = 1,n
            xsph = x(j)
            ysph = y(j)
            zR = zRe*zF / (ftan(ysph)**zn)
            ztheta = zn*(xsph - zx0)
	    xlam = zR*sin(ztheta)
	    ylam = zR0 - zR*cos(ztheta)
	    x(j) = (xlam - zxlam0) / zdx
	    y(j) = (ylam - zylam0) / zdy
         enddo
c
      else
c
         ierror = 1
c
      endif
c
      return
      end
