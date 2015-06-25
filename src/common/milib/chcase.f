c
c  milib
c  
c  $Id: chcase.f 523 2007-11-29 10:48:21Z martinr $
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
      subroutine chcase(mode,ntext,text)
c
c  NAME:
c     chcase
c
c  PURPOSE:
c     Change text string(s) to lowercase or uppercase letters.
c
c  SYNOPSIS:
c     subroutine chcase(mode,ntext,text)
c     integer       mode,ntext
c     character*(*) text(ntext)
c
c  INPUT:
c     mode            - output type:
c                         1 = lowercase
c                         2 = uppercase
c     ntext           - no. of text strings
c     text(ntext)     - the text strings
c
c  OUTPUT:
c     text(ntext)     - the text strings
c
c  NOTES:
c     - takes care of ascii a-z and A-Z (only)
c
c-----------------------------------------------------------------------
c  DNMI/FoU  30.09.1993  Anstein Foss
c-----------------------------------------------------------------------
c
      integer       mode,ntext
      character*(*) text(ntext)
c
      integer initi
      integer icase(0:255,1:2)
c
      data initi/0/
c
      save icase
c
      if(initi.eq.0) then
c..initialize conversion tables
	do i=0,255
	  icase(i,1)=i
	  icase(i,2)=i
	end do
c..from uppercase to lowercase
	do i=65,90
	  icase(i,1)=i+32
	end do
c..from lowercase to uppercase
	do i=97,122
	  icase(i,2)=i-32
	end do
	initi=1
      end if
c
      itab=0
      if(mode.eq.1) itab=1
      if(mode.eq.2) itab=2
      if(itab.eq.0) return
c
      ltext=len(text(1))
c
      do n=1,ntext
        do k=1,ltext
          ichr=ichar(text(n)(k:k))
          text(n)(k:k)=char(icase(ichr,itab))
        end do
      end do
c
      return
      end
