c
c  milib
c  
c  $Id: earthr.f 523 2007-11-29 10:48:21Z martinr $
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
      subroutine earthr(rearth)
c
c  NAME:
c     earthr
c
c  PURPOSE:
c     Return earth radius in unit meter,
c     i.e. the DNMI standard value.
c
c  SYNOPSIS:
c     subroutine earthr(rearth)
c     real rearth
c
c  OUTPUT:
c     rearth  - earth radius i unit meter
c
c-----------------------------------------------------------------------
c  DNMI/FoU  xx.xx.19xx  ............ ... rearth = 6368.00 km in models
c  DNMI/FoU  25.08.1995  Anstein Foss ... rearth = 6371.22 km
c  DNMI/FoU  21.03.1996  Anstein Foss ... rearth = 6371.00 km
c-----------------------------------------------------------------------
c
      real rearth
c
      rearth = 6371000.
c
      return
      end
