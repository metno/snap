c
c  milib
c  
c  $Id: lenstr.f 2352 2009-01-20 12:53:01Z martinls $
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
      integer function lenstr(text,minlen)
c
c  NAME:
c     lenstr
c
c  PURPOSE:
c     Return actual length of text string, excl. trailing blanks.
c
c  SYNOPSIS:
c     integer function lenstr(text,minlen)
c     character*(*) text
c     integer       minlen
c
c  INPUT:
c     text   - text string
c     minlen - the minimum length returned (see NOTES below)
c
c  OUTPUT:
c     lenstr - the string length
c
c  NOTES:
c     Use minlen=1 if you use lenstr to print a text string without
c     trailing blanks. E.g. filenames declared with a large maximum
c     length which hardly ever is longer than one line, but always
c     is printed with trailing blanks on two ore more lines.
c     Example:  write(6,*) filename(1:lenstr(filename,1))
c     (filename(1:0) may abort the program if filename=' ')
c
c-----------------------------------------------------------------------
c  DNMI/FoU  15.03.1995  Anstein Foss
c  DNMI/FoU  27.10.2008  Lisbeth Bergholt Terminate at '\0'
c-----------------------------------------------------------------------
c
      character*(*) text
      integer       minlen
c
      lt=len(text)
      l=0
      do k=1,lt
#ifdef G77
        if(text(k:k).eq.'\0') then 
          l=k-1
        elseif(text(k:k).ne.' ') then 
          l=k
        end if
#else
		if(text(k:k).ne.' ') l=k    
#endif
      end do
c
      if(l.lt.minlen) l=minlen
c
      lenstr=l
c
      return
      end
