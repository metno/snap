/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diArea.h,v 2.0 2006/05/24 14:06:23 audunc Exp $

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
#ifndef diArea_h
#define diArea_h

#include <diRectangle.h>
#include <diProjection.h>
#include <miString.h>

using namespace std;


/**

  \brief Rectangular area with Projection

  Specification of rectangular area in a specific map projection.

*/

class Area {
private:
  Projection proj;  ///> The map projection
  Rectangle rect;   ///> The rectangular area
  miString name;    ///> name of this Area

  /// Copy members
  void memberCopy(const Area& rhs);
protected:
public:
  Area();
  Area(const Projection&, const Rectangle&);
  Area(const miString&, const Projection&, const Rectangle&);
  Area(const Area &rhs);
  ~Area();

  // Assignment operator
  Area& operator=(const Area &rhs);
  // Equality operator
  bool operator==(const Area &rhs) const;
  // Inequality operator
  bool operator!=(const Area &rhs) const;
  
  Projection P() const {return proj; }
  void setP(const Projection p) {proj= p; }
  Rectangle R() const {return rect; }
  void setR(const Rectangle r) {rect= r; }
  const miString& Name() const {return name; }
  void setName(const miString& n) {name= n; }
};

#endif
