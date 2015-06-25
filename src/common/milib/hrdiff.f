c
c  milib
c  
c  $Id: hrdiff.f 523 2007-11-29 10:48:21Z martinr $
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
      subroutine hrdiff(iup1,iup2,itime1,itime2,ihours,ierr1,ierr2)
c
c        calculate interval in hours between 'itime1' and 'itime2',
c        ihours = 'itime2' - 'itime1'  (positive,zero or negative)
c
c        itime.: year,month,day,time in hours, forecast length in hours
c                                            (positive,zero or negative)
c
c        if iup1=1: 'itime1' is updated to give valid date,time
c        if iup2=1: 'itime2' is updated to give valid date,time
c                                               (forecast length = 0)
c
c        ierr1,ierr2: 0 => o.k. itime1,itime2
c                     1 => not o.k. itime1,itime2
c
c-----------------------------------------------------------------------
c  DNMI/FoU  xx.xx.1990  Anstein Foss
c-----------------------------------------------------------------------
c
      integer itime1(5),itime2(5)
c
      integer mdays(12),it(5,2)
      data mdays/31,28,31,30,31,30,31,31,30,31,30,31/
c
      do 10 i=1,5
        it(i,1)=itime1(i)
        it(i,2)=itime2(i)
   10 continue
c
c        put  prog.time  into  year,month,day,time
c
      call vtime(it(1,1),ierr1)
      call vtime(it(1,2),ierr2)
c
c        nb| 'it' now gives "veryfing" time of 'itime1' and 'itime2'
      if(iup1.eq.1 .and. ierr1.eq.0) then
        do 40 i=1,5
   40   itime1(i)=it(i,1)
      endif
      if(iup2.eq.1 .and. ierr2.eq.0) then
        do 42 i=1,5
   42   itime2(i)=it(i,2)
      endif
c
      if(ierr1.ne.0 .or. ierr2.ne.0) then
        ihours=-32767
        return
      endif
c
      do 50 i=1,4
        if(it(i,1).ne.it(i,2)) goto 55
   50 continue
c        no time difference
      ihours=0
      return
c
   55 nt1=1
      nt2=2
      if(it(i,1).gt.it(i,2)) then
        nt1=2
        nt2=1
      endif
      nhh=0
c
      if(it(1,nt1).eq.it(1,nt2)) goto 70
      iy=it(1,nt1)
c        remaining hours first year:
      do 60 im=it(2,nt1),12
        md=mdays(im)
        if(im.eq.2) then
          if(iy/4*4.eq.iy) md=29
          if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) md=28
        endif
        nhh=nhh+md*24
   60 continue
      nhh=nhh-(it(3,nt1)-1)*24-it(4,nt1)
c        one year steps
      do 65 iy=it(1,nt1)+1,it(1,nt2)-1
        nd=365
        if(iy/4*4.eq.iy) nd=366
        if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) nd=365
        nhh=nhh+nd*24
   65 continue
      it(1,nt1)=it(1,nt2)
      it(2,nt1)=1
      it(3,nt1)=1
      it(4,nt1)=0
c
   70 if(it(2,nt1).eq.it(2,nt2)) goto 80
c        remaining hours first month
      iy=it(1,nt1)
      im=it(2,nt1)
      md=mdays(im)
      if(im.eq.2) then
        if(iy/4*4.eq.iy) md=29
        if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) md=28
      endif
      nhh=nhh+(md-it(3,nt1)+1)*24-it(4,nt1)
c        one month steps
      do 75 im=it(2,nt1)+1,it(2,nt2)-1
        md=mdays(im)
        if(im.eq.2) then
          if(iy/4*4.eq.iy) md=29
          if(iy/100*100.eq.iy .and. iy/400*400.ne.iy) md=28
        endif
        nhh=nhh+md*24
   75 continue
      it(2,nt1)=it(2,nt2)
      it(3,nt1)=1
      it(4,nt1)=0
c
   80 if(it(3,nt1).ne.it(3,nt2)) then
        nhh=nhh+(it(3,nt2)-it(3,nt1))*24-it(4,nt1)
        it(3,nt1)=it(3,nt2)
        it(4,nt1)=0
      endif
c        hours last day
      nhh=nhh+it(4,nt2)-it(4,nt1)
      it(4,nt1)=it(4,nt2)
c
      if(nt1.eq.1) then
        ihours=nhh
      else
        ihours=-nhh
      endif
c
      return
      end
