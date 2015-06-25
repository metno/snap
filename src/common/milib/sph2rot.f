c
c  milib
c
c  $Id: sph2rot.f 523 2007-11-29 10:48:21Z martinr $
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
      subroutine sph2rot(icall,n,x,y,xcen,ycen,ierror)
c
c  conversion between spherical (x=xsph,y=ysph) and spherical rotated
c  (x=xrot,y=yrot) coordinates. (xcen,ycen) is the position of the
c  rotated equator/greenwich in terms of (longitude,latitude).
c  all values are given in radians.
c
c----------------------------------------------------------------------
c   DNMI/FoU  xx.xx.1995  Jan Erik Haugen ... Hirlam code (?)
c   DNMI/FoU  xx.05.1995  Anstein Foss ...... DNMI library version
c met.no/FoU  22.06.2006  Anstein Foss ...... double precision computations
c----------------------------------------------------------------------
c
      implicit none
c
      integer icall, n, ierror
      real    x(n), y(n), xcen, ycen
c
      integer j
      double precision zsycen,zcycen,xsph,ysph,zxmxc,zsxmxc,zcxmxc,
     +		       zsysph,zcysph,zsyrot,yrot,
     +		       zcyrot,zcxrot,zsxrot,xrot,zxcen,zycen
c
      ierror = 0
c
      zxcen= xcen
      zycen= ycen
      zsycen = sin(zycen)
      zcycen = cos(zycen)
c
      if (icall.eq.1) then
c
c  compute spherical rotated coordinates as function of
c  spherical coordinates
c
      do j = 1,n
         xsph = x(j)
         ysph = y(j)
         zxmxc  = xsph - zxcen
         zsxmxc = sin(zxmxc)
         zcxmxc = cos(zxmxc)
         zsysph = sin(ysph)
         zcysph = cos(ysph)
         zsyrot = zcycen*zsysph - zsycen*zcysph*zcxmxc
         zsyrot = max(zsyrot,-1.0d0)
         zsyrot = min(zsyrot,+1.0d0)
         yrot   = asin(zsyrot)
         zcyrot = cos(yrot)
         zcxrot = (zcycen*zcysph*zcxmxc +
     +             zsycen*zsysph)/zcyrot
         zcxrot = max(zcxrot,-1.0d0)
         zcxrot = min(zcxrot,+1.0d0)
         zsxrot = zcysph*zsxmxc/zcyrot
         xrot   = acos(zcxrot)
         if (zsxrot.lt.0.0d0) xrot = -xrot
         x(j) = xrot
         y(j) = yrot
      enddo
c
      elseif (icall.eq.-1) then
c
c  compute spherical coordinates as function of
c  spherical rotated coordinates
c
      do j = 1,n
         xrot = x(j)
         yrot = y(j)
         zsxrot = sin(xrot)
         zcxrot = cos(xrot)
         zsyrot = sin(yrot)
         zcyrot = cos(yrot)
         zsysph = zcycen*zsyrot + zsycen*zcyrot*zcxrot
         zsysph = max(zsysph,-1.0d0)
         zsysph = min(zsysph,+1.0d0)
         ysph   = asin(zsysph)
         zcysph = cos(ysph)
         zcxmxc = (zcycen*zcyrot*zcxrot -
     +             zsycen*zsyrot)/zcysph
         zcxmxc = max(zcxmxc,-1.0d0)
         zcxmxc = min(zcxmxc,+1.0d0)
         zsxmxc = zcyrot*zsxrot/zcysph
         zxmxc  = acos(zcxmxc)
         if (zsxmxc.lt.0.0d0) zxmxc = -zxmxc
         xsph = zxmxc + zxcen
         x(j) = xsph
         y(j) = ysph
      enddo
c
      else
c
      ierror=1
c
      endif
c
      return
      end
