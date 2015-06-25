c
c  milib
c  
c  $Id: rmfile.f 523 2007-11-29 10:48:21Z martinr $
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
      subroutine rmfile(filnam,iprint,ierror)
c
c  PURPOSE: Remove file.
c           Useful before 'creating' a direct access file in cases
c           where it already exists (and the existing file is larger
c           than the new file will be).
c           The file will not be removed if the input file name
c           is a link.
c
c  INPUT:   filnam    - character*(*)  - the file path and name
c           iprint    - integer        - 0 = do not print message
c                                        1 = print message
c
c  OUTPUT:  ierror    - integer        - 0 = no error (always)
c
c  METHOD:
c    Using fortran library functions (SGI): stat,lstat,system.
c    ( call system('rm -f filnam') ).
c
c  BUGS:
c    Max lenght of file path and name is 256 characters.
c    Always returning 'no error'.
c
c  NOTE:
c    If problems when porting this routine to another machine:
c    Just make it a dummy routine, and possibly remove (output) files
c    before running the programs.
c
c-----------------------------------------------------------------------
c  DNMI/FoU  04.01.1993  Anstein Foss
c  DNMI/FoU  03.05.2000  Anstein Foss ... very simple linux version
c  DNMI/FoU  03.03.2005  Anstein Foss ... minor change
c-----------------------------------------------------------------------
c
c..input/output:
      integer       iprint,ierror
      character*(*) filnam
c
c..local:
      character*262 cmd
c
      if(iprint.eq.1) then
        write(6,*) 'Remove file: ',filnam(1:lenstr(filnam,1))
      end if
c
      cmd = 'rm -f '//filnam(1:lenstr(filnam,1))
      call system(cmd)
c
      ierror=0
c
      return
      end
