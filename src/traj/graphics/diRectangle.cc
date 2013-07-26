/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diRectangle.cc,v 2.0 2006/05/24 14:06:28 audunc Exp $

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
#include <diRectangle.h>

// Default constructor
Rectangle::Rectangle()
  : x1(0),y1(0),x2(0),y2(0),
    ex1(0),ey1(0),ex2(0),ey2(0){
}

// Constructor
Rectangle::Rectangle(const float _x1, const float _y1,
		     const float _x2, const float _y2)
  : x1(_x1),y1(_y1),x2(_x2),y2(_y2),
    ex1(_x1),ey1(_y1),ex2(_x2),ey2(_y2){
}


// Equality operator
bool Rectangle::operator==(const Rectangle &rhs) const{
  return ((x1==rhs.x1 && y1==rhs.y1 &&
	   x2==rhs.x2 && y2==rhs.y2));
}

// Inequality operator
bool Rectangle::operator!=(const Rectangle &rhs) const{
  return ((x1!=rhs.x1 || y1!=rhs.y1 ||
	   x2!=rhs.x2 || y2!=rhs.y2));
}

ostream& operator<<(ostream& output, const Rectangle& r){
  return output << " x1:" << r.x1 << " y1:" << r.y1 <<
    " x2:" << r.x2 << " y2:" << r.y2;
}

void  Rectangle::setExtension(const float extend){
  // may use extend<0 ...
  ex1= x1 - extend;
  ey1= y1 - extend;
  ex2= x2 + extend;
  ey2= y2 + extend;
}

void Rectangle::putinside(const float& x, const float& y){
  
  float xdiff = x2 - x1;
  x1 += x - (x2+x1)/2.;
  x2 = x1+xdiff;

  float ydiff = y2 - y1;
  y1 += y - (y2+y1)/2.;
  y2 = y1+ydiff;

}
