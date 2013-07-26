/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diProjection.h,v 2.0 2006/05/24 14:06:24 audunc Exp $

  Copyright (C) 2006 met.no

  Contact information:
  Norwegian Meteorological Institute
  Box 43 Blindern
  0313 OSLO
  NORWAY
  email: diana@met.no
  
  This file is part of Diana

  Diana is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Diana is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with Diana; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/
#ifndef diProjection_h
#define diProjection_h

//**********************************************************************
//
//   Spherical (rotated) grid, gridtype=2,3:
//
//      gs[0] - western boundary (degrees)
//      gs[1] - southern boundary (degrees)
//      gs[2] - longitude increment (degrees)
//      gs[3] - latitude increment (degrees)
//      gs[4] - xcen: longitude position of rotated equator (degrees)
//      gs[5] - ycen: latitude  position of rotated equator (degrees)
//              (lamda,theta)=(xcen,ycen) at (lamda',theta')=(0,0),
//              where (lamda,theta) are usual spherical coord. and
//              (lamda',theta') are rotated spherical coord.
//              xcen = ycen = 0 for usual spherical coord.
//
//      Note: if the coordinates are geographic longitude(x),latitude(y)
//             set gridtype=2 and gs[6]= { 1.,1.,1.,1.,0.,0. }
//
//   Polar stereographic grid, gridtype=1,4:
//
//      gs[0] - x-position of north pole
//      gs[1] - y-position of north pole
//      gs[2] - number of grid distances between pole and equator
//      gs[3] - rotation angle of the grid (degrees)
//      gs[4] - projection latitude (degrees)
//              (60 degrees north for gridtype=1)
//      gs[5] - 0. (not used)
//
//   Mercator (unrotated) grid, gridtype=5:
//
//      gs[0] - western boundary (longitude for x=1) (degrees)
//      gs[1] - southern boundary (latitude for y=1) (degrees)
//      gs[2] - x (longitude) increment (km)
//      gs[3] - y (latitude)  increment (km)
//      gs[4] - reference (construction) latitude (degrees)
//      gs[5] - 0.  (not used)
//
//**********************************************************************

using namespace std;

/**

  \brief Map Projection
  
  Specification of map projection
  - supported projection types in enum pType
  - grid specification in array gridspec

*/


class Projection {

public:
  enum pType {
    undefined_projection = 0,
    polarstereographic_60= 1,
    geographic=            2,
    spherical_rotated=     3,
    polarstereographic=    4,
    mercator=              5
  };
  enum { speclen= 6 };

private:
  int gridtype;
  float gridspec[speclen];    ///> grid specification
  float gridspecstd[speclen];

  // Copy members
  void memberCopy(const Projection& rhs);

public:
  // Constructors
  Projection(const int gt= 0);
  Projection(const int gt, const float gs[speclen]);
  Projection(const Projection &rhs);
  // Destructor
  ~Projection();

  // Assignment operator
  Projection& operator=(const Projection &rhs);
  // Equality operator
  bool operator==(const Projection &rhs) const;
  // Inequality operator
  bool operator!=(const Projection &rhs) const;

  int Gridtype() const {return gridtype; }
  void Gridspec(float gs[speclen]) const
       { for (int i=0; i<speclen; i++) gs[i]= gridspec[i]; }
  void Gridspecstd(float gs[speclen]) const
       { for (int i=0; i<speclen; i++) gs[i]= gridspecstd[i]; }
};

#endif
