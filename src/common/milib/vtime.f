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
      subroutine vtime(itime,ierror)
c
c        'itime' is updated to give "verifying" date,time
c                                               (with prog.time = 0)
c        input:  itime(5) - itime(1): year
c                           itime(2): month (1-12)
c                           itime(3): day (1-28/29/30/31)
c                           itime(4): time in hours (00-23)
c                           itime(5): time in hours of prognosis
c                                     (negative, zero or positive)
c        output: itime(5) -  as above, itime(5)=0
c                ierror   -  0 = o.k. input date/time
c                            1 = not o.k. input date/time
c                                ('itime' not changed)
c
c-----------------------------------------------------------------------
c  DNMI/FoU  xx.xx.1992  Anstein Foss
c-----------------------------------------------------------------------
c
      integer itime(5)
c
      integer mdays(12)
      data mdays/31,28,31,30,31,30,31,31,30,31,30,31/
c
      iy=itime(1)
      im=itime(2)
      id=itime(3)
      ih=itime(4)
c
c..test input time
      ierror=0
      if(im.lt.1 .or. im.gt.12) then
        ierror=1
      else
        md=mdays(im)
        if(im.eq.2) then
          if(iy/4*4.eq.iy) md=29
          if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) md=28
        endif
        if(id.lt.1 .or. id.gt.md) ierror=1
      endif
      if(ih.lt.00 .or. ih.gt.23) ierror=1
c
      if(ierror.ne.0) return
c
      ih=ih+itime(5)
      if(ih.ge.0 .and. ih.le.23) goto 50
      if(ih.lt.0) goto 30
c
      nd=ih/24
      ih=ih-24*nd
      do 20 n=1,nd
        id=id+1
        if(id.gt.md) then
          im=im+1
          if(im.gt.12) then
            iy=iy+1
            im=1
            md=mdays(im)
          elseif(im.eq.2) then
            md=mdays(im)
            if(iy/4*4.eq.iy) md=29
            if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) md=28
          else
            md=mdays(im)
          endif
          id=1
        endif
   20 continue
      goto 50
c
   30 nd=(-ih+23)/24
      ih=ih+24*nd
      do 40 n=1,nd
        id=id-1
        if(id.lt.1) then
          im=im-1
          if(im.lt.1) then
            iy=iy-1
            im=12
            md=mdays(im)
          elseif(im.eq.2) then
            md=mdays(im)
            if(iy/4*4.eq.iy) md=29
            if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) md=28
          else
            md=mdays(im)
          endif
          id=md
        endif
   40 continue
c
   50 itime(1)=iy
      itime(2)=im
      itime(3)=id
      itime(4)=ih
      itime(5)=0
c
      return
      end
