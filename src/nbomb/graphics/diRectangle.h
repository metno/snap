/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diRectangle.h,v 2.0 2006/05/24 14:06:24 audunc Exp $

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

#ifndef diRectangle_h
#define diRectangle_h

#include <iostream>

using namespace std; 

/**
   \brief A rectangle

   Four corners define a rectangle
*/

class Rectangle {
public:
  float x1, y1, x2, y2;
private:
  // extended area for isnear
  float ex1, ey1, ex2, ey2;
public:
  // Constructors
  Rectangle();
  Rectangle(const float, const float, const float, const float);

  // Equality operator
  bool operator==(const Rectangle &rhs) const;
  // Inequality operator
  bool operator!=(const Rectangle &rhs) const;
  // ostream operator
  friend ostream& operator<<(ostream& output, const Rectangle& r);

  /// set tolerance for 'near' positions
  void setExtension(const float);

  /// return width of rectangle
  inline float width() const {return (x2-x1);}
  /// return height of rectangle
  inline float height()const {return (y2-y1);}

  /// return whether a point is inside rectangle
  inline bool isinside(const float& x, const float& y) const
  {return ((x>=x1)&&(x<=x2)&&(y>=y1)&&(y<=y2));}

  /// move rectangle so that x,y is inside
  void putinside(const float& x, const float& y);

  /// return whether a point is 'near' rectangle (see setExtension())
  inline bool isnear(const float& x, const float& y) const
  {return ((x>ex1)&&(x<ex2)&&(y>ey1)&&(y<ey2));}
};

#endif
