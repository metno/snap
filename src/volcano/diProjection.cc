/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diProjection.cc,v 2.0 2006/05/24 14:06:28 audunc Exp $

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
#include <diProjection.h>
#include <milib.h>

#include <iostream>

using namespace std;

// Default constructor
Projection::Projection(const int gt)
  : gridtype(gt)
{
  for (int i=0; i<speclen; i++){
    gridspecstd[i]= 0.0;
    gridspec[i]= 0.0;
  }
}

Projection::Projection(const int gt, const float gs[speclen])
  : gridtype(gt)
{
  // Fix gridspec from fortran indexing (from 1) to C/C++ indexing (from 0).
  // To be used in fortran routines xyconvert and uvconvert,
  // gridspecstd must be used in fortran routine mapfield
  // and when writing a field to a field file.

  for (int i=0; i<speclen; i++) {
    gridspecstd[i]= gs[i];
    gridspec[i]=    gs[i];
  }

  // projection type 1 : polarstereographic at 60 degrees north !!!
  // (mapareas defined in setup without setting true latitude to 60.,
  //  no problem for fields when using gridpar)
  if (gt==1) {
    gridspecstd[4]= 60.;
    gridspec[4]=    60.;
  }

  if (gt>0) {
    int ierror= 1;
    movegrid(gridtype, gridspecstd, 1.0, 1.0, gridspec, &ierror);
    if (ierror){
      cerr << "movegrid error : " << ierror
           << " (in Projection::Projection)" << endl;
    }
  }
}

// Copy constructor
Projection::Projection(const Projection &rhs)
{
  // elementwise copy
  memberCopy(rhs);
}

// Destructor
Projection::~Projection()
{
}

// Assignment operator
Projection& Projection::operator=(const Projection &rhs)
{
  if (this == &rhs) return *this;

  // elementwise copy
  memberCopy(rhs);

  return *this;
}

// Equality operator
bool Projection::operator==(const Projection &rhs) const
{
  if (gridtype != rhs.gridtype) return false;
  for (int i=0; i<speclen; i++)
    if (gridspec[i] != rhs.gridspec[i]) return false;
  return true;
}

// Inequality operator
bool Projection::operator!=(const Projection &rhs) const
{
  if (gridtype != rhs.gridtype) return true;
  for (int i=0; i<speclen; i++)
    if (gridspec[i] != rhs.gridspec[i]) return true;
  return false;
}

void Projection::memberCopy(const Projection& rhs)
{
  // copy members
  gridtype= rhs.gridtype;
  for (int i=0; i<speclen; i++) gridspecstd[i]= rhs.gridspecstd[i];
  for (int i=0; i<speclen; i++) gridspec[i]=    rhs.gridspec[i];
}
