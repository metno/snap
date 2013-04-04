/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diArea.cc,v 2.0 2006/05/24 14:06:26 audunc Exp $

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

#include <diArea.h>

// Default constructor
Area::Area(){
}

Area::Area(const Projection& p, const Rectangle& r)
: proj(p), rect(r){
}

Area::Area(const miString& n, const Projection& p,
	   const Rectangle& r)
: proj(p), rect(r), name(n){
}

// Copy constructor
Area::Area(const Area &rhs){
  // elementwise copy
  memberCopy(rhs);
}

// Destructor
Area::~Area(){
}

// Assignment operator
Area& Area::operator=(const Area &rhs){
  if (this == &rhs) return *this;

  // elementwise copy
  memberCopy(rhs);

  return *this;
}

// Equality operator
bool Area::operator==(const Area &rhs) const{
  return ((proj == rhs.proj) && (rect == rhs.rect));
}

// Inequality operator
bool Area::operator!=(const Area &rhs) const{
  return ((proj != rhs.proj) || (rect != rhs.rect));
}

void Area::memberCopy(const Area& rhs){
  // copy members
  proj= rhs.proj;
  rect= rhs.rect;
  name= rhs.name;
}
