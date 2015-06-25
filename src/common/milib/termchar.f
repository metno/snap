c
c  milib
c
c  $Id: termchar.f 2363 2009-01-20 15:01:38Z audunc $
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
      subroutine termchar(chr)
c
c  PURPOSE: Return a character used to terminate text strings
c           for internal free format read.
c
c-----------------------------------------------------------------------
c  DNMI/FoU  05.12.1995  Anstein Foss
c-----------------------------------------------------------------------
c
c..output:
      character*1 chr

#ifdef G77
c..SGI, DEC, SUN, Linux/g77
      chr=char(0)
#else
c..IBM RS/6000 xl fortran, Linux/gfortran
      chr=' '
#endif
c
      return
      end
