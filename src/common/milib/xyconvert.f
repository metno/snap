c
c  milib
c  
c  $Id: xyconvert.f 2308 2009-01-08 10:19:22Z olev $
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
      subroutine xyconvert(npos,x,y,igtypa,ga,igtypr,gr,ierror)
c
c****************************************************************
c
c     xyconvert - convert coordinates from one grid to another
c
c  purpose:
c
c     coordinate conversion between different grids; spherical,
c     spherical rotated, polar stereographic or mercator.
c     (you may use uvconvert to turn vector (velocity) components.)
c
c  input/output parameters:
c
c     npos   - no. of positions (in x,y)
c     x      - input/output x position
c     y      - input/output y position
c     igtypa - input  grid type
c     ga     - input  grid description
c     igtypr - output grid type
c     gr     - output grid description
c     ierror - output error status, 0=no error
c
c  description of g = ga and gr (for igtype = igtypa and igtypr):
c
c  for spherical (rotated) grid, igtype=2,3:
c
c     g(1) - western boundary (degrees)
c     g(2) - southern boundary (degrees)
c     g(3) - longitude increment (degrees)
c     g(4) - latitude increment (degrees)
c     g(5) - xcen: longitude position of rotated equator (degrees)
c     g(6) - ycen: latitude  position of rotated equator (degrees)
c            (lamda,theta)=(xcen,ycen) at (lamda',theta')=(0,0),
c            where (lamda,theta) are usual spherical coord. and
c            (lamda',theta') are rotated spherical coord.
c            xcen = ycen = 0 for usual spherical coord.
c
c     (note: if the coordinates are geographic longitude(x),latitude(y)
c            set igtype=2 and g(1:6) = 1.,1.,1.,1.,0.,0. )
c
c  for polar stereographic grid, igtype=1,4:
c
c     g(1) - x-position of north pole
c     g(2) - y-position of north pole
c     g(3) - number of grid distances between pole and equator
c     g(4) - rotation angle of the grid (degrees)
c     g(5) - projection latitude (degrees)
c            (60 degrees north for igtype=1)
c     g(6) - 0. (not used)
c
c  for mercator (unrotated) grid, igtype=5:
c
c     g(1) - western boundary (longitude for x=1) (degrees)
c     g(2) - southern boundary (latitude for y=1) (degrees)
c     g(3) - x (longitude) increment (km)
c     g(4) - y (latitude)  increment (km)
c     g(5) - reference (construction) latitude (degrees)
c     g(6) - 0.  (not used)
c
c  for lambert (tangent, non-oblique) grid, igtype=6:
c
c     g(1) - west (longitude for x=1,y=1) (degrees)
c     g(2) - south (latitude for x=1,y=1) (degrees)
c     g(3) - x (easting) increment (km)
c     g(4) - y (northing) increment (km)
c     g(5) - reference longitude (degrees)
c     g(6) - reference latitude (cone tangent) (degrees)
c
c  externals:
c
c     pol2sph - polar sterographic <-> spherical coordinates
c     sph2rot - spherical <-> spherical rotated coordinates
c     mer2sph - mercator (unrotated) <-> spherical coordinates
c     lam2sph - lambert (non-oblique) <-> spherical coordinates
c
c  history:
c
c     j.e. haugen/dnmi      nov -94 ... grd2grd
c     a.   foss   dnmi   02.02.1995 ... no size limits
c     a.   foss   dnmi   25.08.1995 ... xyconvert
c     a.   foss   dnmi   15.05.1996 ... mercator (unrotated)
c     o. vignes met.no   17.11.2008 ... lambert (tangent,non-oblique)
c
c****************************************************************
c
      implicit none
c
      integer npos, igtypa, igtypr, ierror
      real    x(npos), y(npos), ga(6), gr(6)
c
      integer j, iconv2
      real    zpir18, rearth,
     +        xwa, ysa, dxa, dya, xca, yca,
     +        xwr, ysr, dxr, dyr, xcr, ycr,
     +        xpa, ypa, ana, fia, fpa,
     +        xpr, ypr, anr, fir, fpr,
     +        xa, ya, zcrot, zsrot, zx1, zx2, zx3, zy1, zy2, zy3
c
      zpir18 = 2.0*asin(1.0)/180.
      call earthr(rearth)
c
      ierror = 0
      iconv2 = 1
c
c..phase 1 : convert from input to geographic coordinates
c
      if (igtypa.eq.2 .or. igtypa.eq.3) then
c
c..sph->geographic
c
      xwa = ga(1)*zpir18
      ysa = ga(2)*zpir18
      dxa = ga(3)*zpir18
      dya = ga(4)*zpir18
      xca = ga(5)*zpir18
      yca = ga(6)*zpir18
c
      do j = 1,npos
         x(j) = xwa + (x(j)-1.)*dxa
         y(j) = ysa + (y(j)-1.)*dya
      enddo
c
      if(xca.ne.0. .or. yca.ne.0.) then
         call sph2rot(-1,npos,x(1),y(1),xca,yca,ierror)
      endif
c
      elseif (igtypa.eq.1 .or. igtypa.eq.4) then
c
c..pol->geographic (unless pol->pol with ga(5)=gr(5) is requested)
c
        xpa = ga(1)
        ypa = ga(2)
        ana = ga(3)
        fia = ga(4)*zpir18
        fpa = ga(5)*zpir18
c
	if((igtypr.eq.1 .or. igtypr.eq.4) .and. ga(5).eq.gr(5)) then
c
         xpr = gr(1)
         ypr = gr(2)
         anr = gr(3)
         fir = gr(4)*zpir18
         fpr = gr(5)*zpir18
c
         zcrot=cos(fia-fir)
         zsrot=sin(fia-fir)
         zx2=+zcrot*(anr/ana)
         zx3=-zsrot*(anr/ana)
         zy2=+zsrot*(anr/ana)
         zy3=+zcrot*(anr/ana)
         zx1=xpr-zx2*xpa-zx3*ypa
         zy1=ypr-zy2*xpa-zy3*ypa
         do j = 1,npos
            xa = x(j)
            ya = y(j)
            x(j) = zx1 + zx2*xa + zx3*ya
            y(j) = zy1 + zy2*xa + zy3*ya
         enddo
c
	 iconv2 = 0
c
	else
c
         call pol2sph(+1,npos,x(1),y(1),fpa,xpa,ypa,ana,fia,ierror)
c
	end if
c
      elseif (igtypa.eq.5) then
c
c..mercator->geographic
c
         xwa = ga(1)*zpir18
         ysa = ga(2)*zpir18
         dxa = ga(3)*1000.
         dya = ga(4)*1000.
         yca = ga(5)*zpir18
c
	 call mer2sph(+1,npos,x(1),y(1),xwa,ysa,dxa,dya,yca,ierror)
c
      elseif (igtypa.eq.6) then
c
c..lambert->geographic
c
         xwa = ga(1)*zpir18
         ysa = ga(2)*zpir18
         dxa = ga(3)*1000.
         dya = ga(4)*1000.
         xca = ga(5)*zpir18
         yca = ga(6)*zpir18
c
	 call lam2sph(+1,npos,x(1),y(1),xwa,ysa,dxa,dya,xca,yca,yca,
     +                ierror)
c
      else
c
	ierror = 1
c
      end if
c
      if (iconv2.eq.0 .or. ierror.ne.0) return
c
c..phase 2 : convert from geographic to output coordinates
c
      if (igtypr.eq.2 .or. igtypr.eq.3) then
c
c..geographic->sph
c
      xwr = gr(1)*zpir18
      ysr = gr(2)*zpir18
      dxr = gr(3)*zpir18
      dyr = gr(4)*zpir18
      xcr = gr(5)*zpir18
      ycr = gr(6)*zpir18
c
      if(xcr.ne.0. .or. ycr.ne.0.) then
         call sph2rot(+1,npos,x(1),y(1),xcr,ycr,ierror)
         if(ierror.ne.0) return
      endif
c
      do j = 1,npos
         x(j) = (x(j) - xwr)/dxr + 1.
         y(j) = (y(j) - ysr)/dyr + 1.
      enddo
c
      elseif (igtypr.eq.1 .or. igtypr.eq.4) then
c
c..geographic->pol (unless pol->pol with ga(5)=gr(5) is requested)
c
         xpr = gr(1)
         ypr = gr(2)
         anr = gr(3)
         fir = gr(4)*zpir18
         fpr = gr(5)*zpir18
c
         call pol2sph(-1,npos,x(1),y(1),fpr,xpr,ypr,anr,fir,ierror)
c
      elseif (igtypr.eq.5) then
c
c..geographic->mercator
c
         xwr = gr(1)*zpir18
         ysr = gr(2)*zpir18
         dxr = gr(3)*1000.
         dyr = gr(4)*1000.
         ycr = gr(5)*zpir18
c
	 call mer2sph(-1,npos,x(1),y(1),xwr,ysr,dxr,dyr,ycr,ierror)
c
      elseif (igtypr.eq.6) then
c
c..geographic->lambert
c
         xwr = gr(1)*zpir18
         ysr = gr(2)*zpir18
         dxr = gr(3)*1000.
         dyr = gr(4)*1000.
         xcr = gr(5)*zpir18
         ycr = gr(6)*zpir18
c
	 call lam2sph(-1,npos,x(1),y(1),xwr,ysr,dxr,dyr,xcr,ycr,ycr,
     +                ierror)
c
      else
c
         ierror = 1
c
      end if
c
      return
      end
