c
c  milib
c
c  $Id: mer2sph.f 523 2007-11-29 10:48:21Z martinr $
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
      subroutine mer2sph(icall,n,x,y,xw,ys,dx,dy,yc,ierror)
c
c  conversion between mercator (unrotated) and spherical
c  coordinates. mercator coordinates are given in real
c  numbers, where (xmer,ymer)=(1.0,1.0) in lower left corner of
c  the mercator grid. spherical coordinates are given in radians.
c  xw,ys,yc (x.west,y.south,y.construction.latitude) given in radians
c  and dx,dy (x and y resolution at construction.latitude) in meter.
c
c---------------------------------------------------------------------
c   DNMI/FoU  15.05.1996  Anstein Foss
c met.no/FoU  22.06.2006  Anstein Foss ... double precision computations
c---------------------------------------------------------------------
c
      implicit none
c
      integer icall, n, ierror
      real    x(n), y(n), xw, ys, dx, dy, yc
c
      integer ieq, j
      real    rearth, ypos
      double precision zxw, zys, zdx, zdy, zyc, zpih, zrcos,
     +                 zxmerc, zymerc, xsph, ysph, xmer, ymer
c
      ierror = 0
c
      zxw= xw
      zys= ys
      zdx= dx
      zdy= dy
      zyc= yc
c
      call earthr(rearth)
c
      zpih   = asin(1.0d0)
      zrcos  = rearth*cos(zyc)
      zxmerc = zrcos*zxw - zdx
      zymerc = zrcos*log((1.+sin(zys))/cos(zys)) - zdy
c
c  test if possible to avoid heavy computations
c  dependant on y or latitude
      if(n.lt.4) then
	ieq=0
      elseif(y(1).eq.y(2) .or. y(2).eq.y(3) .or. y(3).eq.y(4)) then
	ieq=1
      else
	ieq=0
      end if
c
      if (icall.eq.1) then
c
c  compute spherical coordinates as function of
c  mercator (unrotated) coordinates
c
      if (ieq.eq.0) then
         do j = 1,n
	    xmer = zxmerc + x(j) * zdx
	    ymer = zymerc + y(j) * zdy
	    xsph = xmer/zrcos
	    ysph = 2. * atan(exp(ymer/zrcos))-zpih
            x(j) = xsph
            y(j) = ysph
         enddo
      else
	 ypos=-1.e+35
	 if(ypos.eq.y(1)) ypos=0.
         do j = 1,n
	    xmer = zxmerc + x(j) * zdx
	    xsph = xmer/zrcos
            x(j) = xsph
	    if (y(j).ne.ypos) then
	       ypos = y(j)
	       ymer = zymerc + y(j) * zdy
	       ysph = 2.0d0 * atan(exp(ymer/zrcos))-zpih
	    endif
            y(j) = ysph
         enddo
      endif
c
      elseif (icall.eq.-1) then
c
c  compute mercator (unrotated) coordinates as function of
c  spherical coordinates
c
      if (ieq.eq.0) then
         do j = 1,n
            xsph = x(j)
            ysph = y(j)
	    xmer = zrcos * xsph
	    ymer = zrcos * log((1.+sin(ysph))/cos(ysph))
	    x(j) = (xmer - zxmerc) / zdx
	    y(j) = (ymer - zymerc) / zdy
         enddo
      else
	 ysph=-1.e+35
	 if(ysph.eq.y(1)) ysph=0.
         do j = 1,n
            xsph = x(j)
	    xmer = zrcos * xsph
	    x(j) = (xmer - zxmerc) / zdx
	    if (y(j).ne.ysph) then
               ysph = y(j)
	       ymer = zrcos * log((1.+sin(ysph))/cos(ysph))
	       ypos = (ymer - zymerc) / zdy
	    endif
	    y(j) = ypos
         enddo
      endif
c
      else
c
      ierror = 1
c
      endif
c
      return
      end
