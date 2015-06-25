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
      subroutine gridpar(icall,ldata,idata,igtype,nx,ny,grid,ierror)
c
c  NAME:
c     gridpar
c
c  PURPOSE:
c     Conversion between integer*2 field identification and
c     variables used in programs. Note that some gridtypes also
c     has extended (geometry) identification behind the field data.
c
c  SYNOPSIS:
c     subroutine gridpar(icall,ldata,idata,igtype,nx,ny,grid,ierror)
c     integer   icall,ldata,igtype,nx,ny,ierror
c     integer*2 idata(ldata)
c     real      grid(6)
c
c  INPUT:
c     icall  - +1 : from idata to igtype,nx,ny,grid
c              -1 : from igtype,nx,ny,grid to idata
c     ldata  - length of the idata array
c
c  INPUT/OUTPUT:
c     idata  - field identification, field data (not touched)
c              and possibly extra geometry specifications
c              (max 20 words in this version)
c     igtype - grid type, 1 = polarstereographic grid (true at 60N)
c                         2 = geographic
c                         3 = spherical rotated grid
c                         4 = polarstereographic grid
c                         5 = mercator grid (unrotated)
c                         6 = lambert (tangent,non-oblique) grid
c                         * = unknown grid type (-32767 to +32 accepted)
c              (note that 'codes' are used in the field identification
c               when extra geometry identification is stored after
c               the field data, the reason for the +32 limit)
c     nx     - no. of gridpoints in x direction (1 - 32767)
c     ny     - no. of gridpoints in y direction (1 - 32767)
c     grid   - grid parameters
c                igtype=1,4, polarstereographic grid:
c                          grid(1) = x position of north pole (xp)
c                          grid(2) = y position of north pole (yp)
c                          grid(3) = no. of grid units between
c                                    North Pole and Equator
c                          grid(4) = grid rotation angle (degrees),
c                                    positive east, negative west
c                          grid(5) = projection latitude
c                                    (degrees) standard is 60 (60 deg. N)
c                          grid(6) = 0.  (not used)
c                igtype=2,3, geographic or spherical rotated grid:
c                          grid(1) = western boundary (degrees)
c                                    (longitude for x=1)
c                          grid(2) = southern boundary (degrees)
c                                    (latitude for y=1)
c                          grid(3) = longitude increment (degrees)
c                          grid(4) = latitude  increment (degrees)
c                          grid(5) = longitude position of rotated equator
c                                    (degrees)  (0 if geographic grid)
c                          grid(6) = latitude  position of rotated equator
c                                    (degrees)  (0 if geographic grid)
c                igtype=5, mercator (unrotated) grid:
c                          grid(1) = western boundary (degrees)
c                                    (longitude for x=1)
c                          grid(2) = southern boundary (degrees)
c                                    (latitude for y=1)
c                          grid(3) = x (longitude) increment (km)
c                          grid(4) = y (latitude)  increment (km)
c                          grid(5) = reference (construction) latitude
c                                    (degrees)
c                          grid(6) = 0.  (not used)
c                igtype=6, lambert (tangent,non-oblique) grid:
c			   grid(1) = west (degrees)
c				     (longitude for x=1,y=1)
c			   grid(2) = south (degrees)
c				     (latitude for x=1,y=1)
c			   grid(3) = x (easting) increment (km)
c			   grid(4) = y (northing) increment (km)
c			   grid(5) = reference longitude (degrees)
c			   grid(6) = reference (tangent) latitude (degrees)
c                igtype=*, unknown grid type,
c                          only use grid type less than 1 if the
c                          grid parameters have no meaning:
c                          grid(1:6) : unknown grid parameters
c
c  OUTPUT:
c     ierror - error status: 0 = no error
c                            1 = bad value in input identification
c                                or in input grid parameters etc.
c                            2 = ldata too small
c                            3 = unknown icall
c                            4 = ldata too small for needed extra
c                                geometry identification (icall=-1),
c                                but the best possible identification
c                                is done (see NOTES below)
c
c  DOCUMENTATION:
c     Basic document on felt files:
c          FILE STRUKTUR FOR "SANNTIDS" LAGRING AV GRID-DATA
c               Forskningsavdeling DNMI, oktober 1982
c     See also /usr/local/doc/felt.doc (at DNMI)
c
c  NOTES:
c   - This routine maintain compability with old formats
c     (see comments in the source code),
c     new formats added when required for some reason.
c   - Specyfing ldata too short to hold extra geometry identification
c     (for icall=-1) will restrict this routine from making this even
c     when it seems appropriate. In order to be compatible with
c     old unextended (or less extended) formats you then may
c     disregard the returned ierror=4 (in special cases).
c   - Avoid calling this routine more often than necessary,
c     i.e. only after reading the first field or only before output
c     of the first field.
c     In models with several calling routines you may do a 'setup' call
c     and keep (part of) the first 20 words of identification and
c     possibly extra geometry specification locally (see note below).
c   - The value of nx and ny (in idata if icall=+1) does not influence
c     conversion of grid parameters.
c     You may then use this routine to convert between field
c     identification and grid parameters with nx=ny=1 and possibly
c     find extra identification in word 22,23,... in idata.
c     (see DOCUMENTATION to find format description).
c
c-----------------------------------------------------------------------
c  DNMI/FoU  05.05.1995  Anstein Foss
c  DNMI/FoU  09.06.1995  Anstein Foss
c  DNMI/FoU  14.05.1996  Anstein Foss ... mercator (unrotated)
c  DNMI/FoU  02.09.1996  Anstein Foss ... gridtype 2012 -> 2 (bad test)
c  DNMI/FoU  15.10.1996  Anstein Foss ... even better scaling when needed
c  DNMI/FoU  17.02.1997  Anstein Foss ... and again (for 'image' fields)
c  DNMI/FoU  11.04.2003  Anstein Foss ... slightly less precision
c met.no/FoU 03.05.2005  Anstein Foss ... float() -> real()
c met.no/FoU 20.11.2008  Ole Vignes ..... lambert (tangent, non-oblique)
c-----------------------------------------------------------------------
c
      implicit none
c
c..input/output
      integer   icall,ldata,igtype,nx,ny,ierror
      integer*2 idata(ldata)
      real      grid(6)
c
c..local
      integer   ngw,lgeom,i,igr,ig1,ig2,ld,igscale,kgeom,lgeom1,lgeom2
      integer*2 igeom1(12),igeom2(20)
      real      gscale,glog,gws,dglim,dgmax,dgprv,grx
      real      gw(6),gr(6)
c
      ierror=0
c
      if(icall.eq.+1) then
c
c..idata(ldata) -> igtype,nx,ny,grid(6)
c
        igtype=0
        nx=0
        ny=0
        do i=1,6
          grid(i)=0.
        end do
        if(ldata.lt.20) then
          ierror=2
          return
        end if
        igtype=idata(9)
        lgeom =0
        if(igtype.gt.999) then
          i=igtype
          igtype=igtype/1000
          lgeom =i-igtype*1000
        end if
        nx=idata(10)
        ny=idata(11)
        if(nx.lt.1 .or. ny.lt.1) then
          ierror=1
          return
        end if
c
        if(igtype.eq.1 .or. igtype.eq.4) then
c
c..polarstereographic (type 1 always true at 60 degrees North)
c
          if(idata(17).gt.0) then
c..the standard
            grid(1)=idata(15)*0.01
            grid(2)=idata(16)*0.01
            grid(3)=idata(17)*0.1
            grid(4)=idata(18)
          else
c..an old extension
            grid(1)=idata(15)
            grid(2)=idata(16)
            grid(3)=-idata(17)*0.1
            grid(4)=idata(18)
          end if
          grid(5)=60.
c
        elseif(igtype.eq.2 .or. igtype.eq.3) then
c
c..geographic (2) or spherical rotated grid (3)
c
          grid(1)=idata(16)*0.01
          grid(2)=idata(15)*0.01
          grid(3)=idata(18)*0.01
          grid(4)=idata(17)*0.01
c
        elseif(igtype.eq.5) then
c
c..mercator grid (5)
c
          grid(1)=idata(15)*0.01
          grid(2)=idata(16)*0.01
          grid(3)=idata(17)*0.1
          grid(4)=idata(18)*0.1
c
        elseif(igtype.eq.6) then
c
c..lambert grid (6)
c
          grid(1)=idata(15)*0.01
          grid(2)=idata(16)*0.01
          grid(3)=idata(17)*0.1
          grid(4)=idata(18)*0.1
c
        else
c
c..unknown/undefined grid type
c
          grid(1)=idata(15)
          grid(2)=idata(16)
          grid(3)=idata(17)
          grid(4)=idata(18)
c
        end if
c
        if(lgeom.gt.0) then
c
          if(igtype.eq.1 .or. igtype.eq.4) then
            gscale=100.
            ngw=5
          elseif(igtype.eq.2 .or. igtype.eq.3) then
            gscale=10000.
            ngw=6
          elseif(igtype.eq.5) then
            gscale=10000.
            ngw=5
          elseif(igtype.eq.6) then
            gscale=10000.
            ngw=6
          else
            gscale=100.
            ngw=6
          end if
c
          ld=20+nx*ny
c
          if(lgeom.eq.ngw*2 .and. ld+lgeom.le.ldata) then
c
c..first extended method
            do i=1,ngw
              ig1=idata(ld+1)
              ig2=idata(ld+2)
              ld=ld+2
              grid(i)=real(ig1*10000+ig2)/gscale
            end do
c
          elseif(lgeom.eq.2+ngw*3 .and. ld+lgeom.le.ldata) then
c
c..second extended method
            if(idata(ld+1).eq.ngw .and. idata(ld+2).eq.3) then
              ld=ld+2
              do i=1,ngw
                igscale=idata(ld+1)
                ig1    =idata(ld+2)
                ig2    =idata(ld+3)
                ld=ld+3
                gscale=10.**igscale
                grid(i)=real(ig1*10000+ig2)*gscale
              end do
            else
              ierror=2
            end if
c
          else
c
            ierror=2
c
          end if
c
        end if
c
        if(ierror.eq.0) then
c
          if(igtype.eq.1 .or. igtype.eq.4) then
c..the DNMI "standard" (150km grid => an=grid(3)=79.)
            if(grid(3).ne.0.) grid(3)=79.*150./grid(3)
            if(grid(3).eq.0.   .or. grid(5).eq.0. .or.
     +         grid(5).lt.-90. .or. grid(5).gt.+90.) ierror=1
          elseif(igtype.eq.2 .or. igtype.eq.3) then
            if(grid(3).eq.0. .or. grid(4).eq.0.) ierror=1
          elseif(igtype.eq.5 .or. igtype.eq.6) then
            if(grid(3).eq.0. .or. grid(4).eq.0.) ierror=1
          end if
c
        end if
c
      elseif(icall.eq.-1) then
c
c..igtype,nx,ny,grid(6) -> idata(ldata)
c
        if(ldata.lt.20) then
          ierror=2
          return
        end if
c
        if(igtype.gt.32 .or.
     +     nx.lt.1 .or. nx.gt.32767 .or.
     +     ny.lt.1 .or. ny.gt.32767) then
          ierror=1
          return
        end if
c
        idata( 9)=igtype
        idata(10)=nx
        idata(11)=ny
        do i=1,6
          gw(i)=grid(i)
        end do
c
        if(igtype.eq.1 .or. igtype.eq.4) then
c
c..polarstereographic (type 1 always true at 60 degrees North)
c
          if(gw(3).eq.0.) then
            ierror=1
            return
          end if
c
c..the DNMI "standard" (150km grid => an=grid(3)=79.)
          gw(3)=150.*79./gw(3)
c
          if(abs(gw(1)).lt.327.66 .and.
     +       abs(gw(2)).lt.327.66) then
c..the standard
            idata(15)=nint(gw(1)*100.)
            idata(16)=nint(gw(2)*100.)
            idata(17)=nint(gw(3)*10.)
            idata(18)=nint(gw(4))
            gr(1)=real(idata(15))*0.01
            gr(2)=real(idata(16))*0.01
            gr(3)=real(idata(17))*0.1
            gr(4)=real(idata(18))
            gr(5)=60.
          elseif(abs(gw(1)).lt.32766. .and.
     +           abs(gw(2)).lt.32766.) then
c..an old extension
            idata(15)=nint(gw(1))
            idata(16)=nint(gw(2))
            idata(17)=-nint(gw(3)*10.)
            idata(18)=nint(gw(4))
            gr(1)=real(idata(15))
            gr(2)=real(idata(16))
            gr(3)=-real(idata(17))*0.1
            gr(4)=real(idata(18))
            gr(5)=60.
          else
c..old impossible case
            idata(15)=32767
            idata(16)=32767
            idata(17)=32767
            idata(18)=32767
            gr(1)=327.67
            gr(2)=327.67
            gr(3)=3276.7
            gr(4)=32767.
            gr(5)=60.
          end if
c
          gscale=100.
          ngw=5
c
        elseif(igtype.eq.2 .or. igtype.eq.3) then
c
c..geographic (2) or spherical rotated grid (3)
c
          if(gw(3).eq.0. .or. gw(4).eq.0.) then
            ierror=1
            return
          end if
c
          if(gw(1).gt.+180.) gw(1)=gw(1)-360.
          if(gw(1).lt.-180.) gw(1)=gw(1)+360.
c
          idata(15)=nint(gw(2)*100.)
          idata(16)=nint(gw(1)*100.)
          idata(17)=nint(gw(4)*100.)
          idata(18)=nint(gw(3)*100.)
          gr(1)=real(idata(16))*0.01
          gr(2)=real(idata(15))*0.01
          gr(3)=real(idata(18))*0.01
          gr(4)=real(idata(17))*0.01
          gr(5)=0.
          gr(6)=0.
c
          gscale=10000.
          ngw=6
c
        elseif(igtype.eq.5) then
c
c..mercator grid (5)
c
          if(gw(3).eq.0. .or. gw(4).eq.0.) then
            ierror=1
            return
          end if
c
          idata(15)=nint(gw(1)*100.)
          idata(16)=nint(gw(2)*100.)
          idata(17)=nint(gw(3)*10.)
          idata(18)=nint(gw(4)*10.)
          gr(1)=real(idata(15))*0.01
          gr(2)=real(idata(16))*0.01
          gr(3)=real(idata(17))*0.1
          gr(4)=real(idata(18))*0.1
          gr(5)=0.
c
          gscale=10000.
          ngw=5
c
        elseif(igtype.eq.6) then
c
c..lambert grid (6)
c
          if(gw(3).eq.0. .or. gw(4).eq.0.) then
            ierror=1
            return
          end if
c
          idata(15)=nint(gw(1)*100.)
          idata(16)=nint(gw(2)*100.)
          idata(17)=nint(gw(3)*10.)
          idata(18)=nint(gw(4)*10.)
          gr(1)=real(idata(15))*0.01
          gr(2)=real(idata(16))*0.01
          gr(3)=real(idata(17))*0.1
          gr(4)=real(idata(18))*0.1
          gr(5)=0.
          gr(6)=0.
c
          gscale=10000.
          ngw=6
c
        else
c
c..unknown/undefined grid type
c
          idata(15)=nint(gw(1))
          idata(16)=nint(gw(2))
          idata(17)=nint(gw(3))
          idata(18)=nint(gw(4))
          gr(1)=real(idata(15))
          gr(2)=real(idata(16))
          gr(3)=real(idata(17))
          gr(4)=real(idata(18))
          gr(5)=0.
          gr(6)=0.
c
          gscale=100.
          ngw=6
c
        end if
c
c..check if the standard packing above was good enough
c..or if the first or second extended method should be used,
c..for compability with old formats the least extended is preferred
c
c..a limit to avoid high precision complications
        dglim=1.e-8
c
        dgmax=0.
c
        if(igtype.gt.0) then
          do i=1,ngw
            if(gw(i).ne.0.) dgmax=max(dgmax,abs((gr(i)-gw(i))/gw(i)))
          end do
        end if
c
        if(dgmax.gt.dglim) then
c
          kgeom=0
          ld=20+nx*ny
c
c..first extended method, same scaling for all grid parameters
c..but don't use it unless it gives better results
c..(kept in the code for compability with old formats, possibly
c.. in models etc. not using this routine)
c
          dgprv=dgmax
          dgmax=0.
          lgeom1=0
c
          do i=1,ngw
            gws=gw(i)*gscale
c..check overflow
            if(gws.ne.0. .and. abs(gws).lt.3.e+8) then
              igr=nint(gws)
              ig1=igr/10000
              ig2=igr-ig1*10000
              igeom1(lgeom1+1)=ig1
              igeom1(lgeom1+2)=ig2
              grx=real(ig1*10000+ig2)/gscale
              dgmax=max(dgmax,abs((grx-gw(i))/gw(i)))
            else
c..zero or value not possible for this method
              igeom1(lgeom1+1)=0
              igeom1(lgeom1+2)=0
              if(gws.ne.0.) dgmax=1.
            end if
            lgeom1=lgeom1+2
          end do
c
          if(dgmax.lt.dgprv) then
            if(ldata.ge.ld+lgeom1) then
              kgeom=1
            else
              ierror=4
            end if
          else
            dgmax=dgprv
          end if
c
          if(dgmax.gt.dglim .and. ierror.eq.0) then
c
c..second extended method, good enough for any real*4 precision value
c..but don't use it unless it gives better results
c
            dgprv=dgmax
            dgmax=0.
c
            igeom2(1)=ngw
            igeom2(2)=3
            lgeom2=2
            do i=1,ngw
              if(gw(i).ne.0.) then
c..7 decimals precision (enough for real*4)
                glog=log10(abs(gw(i)))-7.
                igscale=int(glog)
                if(glog.gt.0.) igscale=igscale+1
c..keep scaling within real*4 range
                igscale=min(igscale,+25)
                igscale=max(igscale,-25)
                gscale=10.**(-igscale)
                igr=nint(gw(i)*gscale)
                ig1=igr/10000
                ig2=igr-ig1*10000
                igeom2(lgeom2+1)=igscale
                igeom2(lgeom2+2)=ig1
                igeom2(lgeom2+3)=ig2
                grx=real(ig1*10000+ig2)/gscale
                dgmax=max(dgmax,abs((grx-gw(i))/gw(i)))
              else
                igeom2(lgeom2+1)=0
                igeom2(lgeom2+2)=0
                igeom2(lgeom2+3)=0
              end if
              lgeom2=lgeom2+3
            end do
c
            if(dgmax.lt.dgprv) then
              if(ldata.ge.ld+lgeom2) then
                kgeom=2
              else
                ierror=4
              end if
ccc         else
ccc           dgmax=dgprv
            end if
c
          end if
c
          if(kgeom.eq.1) then
            idata(9)=igtype*1000+lgeom1
            do i=1,lgeom1
              idata(ld+i)=igeom1(i)
            end do
          elseif(kgeom.eq.2) then
            idata(9)=igtype*1000+lgeom2
            do i=1,lgeom2
              idata(ld+i)=igeom2(i)
            end do
          end if
c
        end if
c
c
      else
c
c..wrong icall
c
        ierror=3
c
      end if
c
      return
      end
