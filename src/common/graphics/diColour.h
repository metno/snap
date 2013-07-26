/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diColour.h,v 2.0 2006/05/24 14:06:23 audunc Exp $

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
#ifndef diColour_h
#define diColour_h

#include <map>
#include <miString.h>
#include <porttypes.h>

using namespace std;

/**

  \brief Colour type
  
  Colour definition, with R, G, B and alpha (translucency) component.
  - static list of defined colours, reachable by name

*/

class Colour {
public:
  enum cIndex{
    red   =0,
    green =1,
    blue  =2,
    alpha =3
  };
  enum {maxv= 255};
  /// 4 component colour data
  struct values{
    uchar_t rgba[4];
    inline values();
    inline values& operator=(const values &rhs);
    inline bool operator==(const values &rhs) const ;
  };
  /// 3 component colour data and name
  struct ColourInfo {
    int rgb[3];
    miString name;
  };

private:
  miString name;
  values v;
  uchar_t colourindex;
  static map<miString,Colour> cmap;
  static vector<ColourInfo> colours;

  // Copy members
  void memberCopy(const Colour& rhs);
public:
  // Constructors
  Colour(const miString);
  Colour(const values&);
  Colour(const uint32 =0);
  Colour(const uchar_t, const uchar_t,
	 const uchar_t, const uchar_t =maxv);
  Colour(const Colour &rhs);
  // Destructor
  ~Colour();

  // Assignment operator
  Colour& operator=(const Colour &rhs);
  // Equality operator
  bool operator==(const Colour &rhs) const;

  // static functions for static colour-map
  static void define(const miString, const uchar_t, const uchar_t,
		     const uchar_t, const uchar_t =maxv);
  static void define(const miString, const values&);
  static void setindex(const miString, const uchar_t);

  // static functions for static vector <ColourInfo> colours 
  static void addColourInfo(const ColourInfo& ci){colours.push_back(ci);}
  static vector<ColourInfo> getColourInfo(){return colours;}

  void set(const uchar_t r, const uchar_t g,
	   const uchar_t b, const uchar_t a =maxv){
    v.rgba[red]=r; v.rgba[green]=g;
    v.rgba[blue]=b; v.rgba[alpha]=a;}
  
  void set(const values& va)
  {v= va;}
  
  void set(const cIndex i,const uchar_t b){v.rgba[i]=b;}
  
  uchar_t R() const {return v.rgba[red];   }
  uchar_t G() const {return v.rgba[green]; }
  uchar_t B() const {return v.rgba[blue];  }
  uchar_t A() const {return v.rgba[alpha]; }
  
  float fR() const {return 1.0*v.rgba[red]/maxv;  }
  float fG() const {return 1.0*v.rgba[green]/maxv;}
  float fB() const {return 1.0*v.rgba[blue]/maxv; }
  float fA() const {return 1.0*v.rgba[alpha]/maxv;}
  
  const uchar_t* RGBA() const {return v.rgba; }
  const uchar_t* RGB()  const {return v.rgba; }
  uchar_t Index() const {return colourindex; }

  const miString& Name() const {return name;}

  void readColourMap(const miString fname);

  friend ostream& operator<<(ostream& out, const Colour& rhs);
  
};


// inline Colour::values member functions

inline Colour::values::values(){
  rgba[0]=0; rgba[1]=0;
  rgba[2]=0; rgba[3]=0; 
}


inline Colour::values& Colour::values::operator=(const Colour::values &rhs){
  if (this != &rhs){
    rgba[0]= rhs.rgba[0];
    rgba[1]= rhs.rgba[1];
    rgba[2]= rhs.rgba[2];
    rgba[3]= rhs.rgba[3];
  }
  return *this;
}

inline bool Colour::values::operator==(const Colour::values &rhs) const {
  return (rgba[0]==rhs.rgba[0] &&
	  rgba[1]==rhs.rgba[1] &&
	  rgba[2]==rhs.rgba[2] &&
	  rgba[3]==rhs.rgba[3]);
}

#endif
