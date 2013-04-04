/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diColour.cc,v 2.0 2006/05/24 14:06:26 audunc Exp $

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

#include <diColour.h>
#include <iomanip>
#include <iostream>
#include <stdio.h>

map<miString,Colour> Colour::cmap;
vector<Colour::ColourInfo> Colour::colours;


Colour::Colour(const values& va) : v(va){
}

Colour::Colour(const uint32 hexv){
  uint32 h= hexv;

  uchar_t a= 255;
  if (h>0xFFFFFF){
    a= (h & 0xFF); h = (h >> 8);
  }
  uchar_t b= (h & 0xFF); h = (h >> 8);
  uchar_t g= (h & 0xFF); h = (h >> 8);
  uchar_t r= (h & 0xFF);
  
  set(r,g,b,a);
}

Colour::Colour(const miString name_){
  //  cerr <<name_<<endl;
  int il;
  miString lname= name_.downcase();
  vector<miString> vstr = lname.split(":");
  int n = vstr.size();
  if (n<2){
    if(cmap.count(lname))
      memberCopy(cmap[lname]);
    else{
      cerr <<lname<<" ukjent"<<endl;
      set(0,0,0);
      name = "black";
    }
  } else if(n<3) {
    memberCopy(cmap[vstr[0]]);
    v.rgba[alpha]= atoi(vstr[1].c_str());
  } else if(n<4){
    set(atoi(vstr[0].cStr()),atoi(vstr[1].cStr()),atoi(vstr[2].cStr()));
    name = lname;
  } else {
    set(atoi(vstr[0].cStr()),atoi(vstr[1].cStr()),
	atoi(vstr[2].cStr()),atoi(vstr[3].cStr()));
    name = lname;
  }
}

Colour::Colour(const uchar_t r, const uchar_t g,
	       const uchar_t b, const uchar_t a){
  set(r,g,b,a);
  name =miString(int(r)) +":";
  name+=miString(int(g)) +":";
  name+=miString(int(b)) +":";
  name+=miString(int(a));
}

// Copy constructor
Colour::Colour(const Colour &rhs){
  // elementwise copy
  memberCopy(rhs);
}

// Destructor
Colour::~Colour(){
}

// Assignment operator
Colour& Colour::operator=(const Colour &rhs){
  if (this != &rhs) 
    // elementwise copy
    memberCopy(rhs);

  return *this;
}

// Equality operator
bool Colour::operator==(const Colour &rhs) const{
  return v==rhs.v;
}

void Colour::memberCopy(const Colour& rhs){
  // copy members
  v= rhs.v;
  name= rhs.name;
  colourindex= rhs.colourindex;
}

void Colour::define(const miString name_,
		    const uchar_t r, const uchar_t g,
		    const uchar_t b, const uchar_t a){
  Colour c(r,g,b,a);
  miString lname= name_.downcase();
  c.name= lname;
  cmap[lname]= c;
}

void Colour::define(const miString name_, const values& va){
  Colour c(va);
  miString lname= name_.downcase();
  c.name= lname;
  cmap[lname]= c;
}

// hack: colourindex is platform-dependent
void Colour::setindex(const miString name_, const uchar_t index){
  miString lname= name_.downcase();
  cmap[lname].colourindex= index;
}

void Colour::readColourMap(const miString fname){
  FILE *fp;
  char buffer[1024], name_[100];
  miString lname;
  int r,g,b;
  Colour c;
  if (fp=fopen(fname.c_str(),"r")){
    while (fgets(buffer,1023,fp)){
      if (buffer[0]=='#' || buffer[0]=='!') continue;
      sscanf(buffer,"%i %i %i %s\n",&r,&g,&b,name_);
      lname= miString(name_).downcase();
      define(lname,r,g,b,255);
    }
    cout << "Found " << cmap.size() << " colours in " << fname << endl;
    fclose(fp);
  }
}


ostream& operator<<(ostream& out, const Colour& rhs){ 
  return out << 
    " name: "  << rhs.name <<
    " red: "   << setw(3) << setfill('0') << int(rhs.v.rgba[0]) <<
    " green: " << setw(3) << setfill('0') << int(rhs.v.rgba[1]) <<
    " blue: "  << setw(3) << setfill('0') << int(rhs.v.rgba[2]) <<
    " alpha: " << setw(3) << setfill('0') << int(rhs.v.rgba[3]) <<
    " Index: " << int(rhs.colourindex) ;
}


