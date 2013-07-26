/*
  Diana - A Free Meteorological Visualisation Tool

  $Id: diPlotOptions.h,v 2.2 2006/05/31 10:26:02 lisbethb Exp $

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
#ifndef diPlotOptions_h
#define diPlotOptions_h

#include <diColour.h>
#include <diLinetype.h>
#include <miString.h>
#include <vector>

using namespace std;

const Colour WhiteC(255,255,255);
const Colour BlackC(0,0,0);

/**
   \brief ways of drawing polygons
*/
enum polyStyle {
  poly_fill,
  poly_border,
  poly_both,
  poly_none
};

/**
   \brief alignment types
*/
enum Alignment {
  align_left,
  align_right,
  align_top,
  align_bottom,
  align_center
};

/**
   \brief fill pattern
*/
struct Filltype {
  miString name;
  unsigned int bmap;
};

/**
   \brief string constants, field plot types
*/
const miString
  fpt_contour         = "contour",
  fpt_wind            = "wind",
  fpt_wind_colour     = "wind_colour",
  fpt_wind_temp_fl    = "wind_temp_fl",
  fpt_vector          = "vector",
  fpt_vector_colour   = "vector_colour",
  fpt_direction       = "direction",
  fpt_direction_colour= "direction_colour",
  fpt_box_pattern     = "box_pattern",
  fpt_box_alpha_shade = "box_alpha_shade",
  fpt_alpha_shade     = "alpha_shade",
  fpt_alarm_box       = "alarm_box";

/**
   \brief Options for one data plot
*/
struct PlotOptions {
  bool options_1;
  bool options_2;
  Colour textcolour,linecolour,linecolour_2,fillcolour,bordercolour;
  vector<Colour> colours;
  vector<Colour> palettecolours;
  vector<Colour> palettecolours_cold;
  miString palettename;
  int table;
  int alpha;
  int repeat;
  Linetype linetype;
  vector<Linetype> linetypes;
  int linewidth;
  vector<int> linewidths;
  Filltype filltype;
  vector<Filltype> filltypes;
  vector<miString> patterns;
  vector<float> limits;
  vector<float> values;
  vector<float> linevalues;
  vector<float> loglinevalues;
  vector<float> valuerange;
  vector<float> valuerange_2;
  vector<int>   forecastLength;
  vector<float> forecastValueMin;
  vector<float> forecastValueMax;
  float lineinterval;
  float lineinterval_2;
  float base;
  float base_2;
  int   density;
  float vectorunit;
  float relsize;
  miString extremeType;
  float    extremeSize;
  float    extremeRadius;
  int      lineSmooth;
  int      fieldSmooth;
  int      zeroLine;      // -1=no_option 0=off 1=on
  int      valueLabel;    //              0=off 1=on
  float    labelSize;
  int      gridLines;     // 0=no gridlines otherwise step
  int      gridLinesMax;  // 0=no limit otherwise skip if more than max
  int      undefMasking;
  Colour   undefColour;
  int      undefLinewidth;
  Linetype undefLinetype;
  miString fptype;
  int      discontinuous;
  int      contourShading;
  miString classSpecifications; // "value:name,value:name,....." split when used
  polyStyle polystyle;
  Alignment h_align;
  Alignment v_align;
  miString  fontname;
  miString  fontface;
  float     fontsize;
  bool      enabled;
  miString  fname;
  vector <miString> fdescr;

  // Constructor
  PlotOptions():
    options_1(false),options_2(false),
    textcolour(WhiteC), linecolour(WhiteC), linecolour_2(WhiteC),
    fillcolour(WhiteC), bordercolour(WhiteC), table(0),alpha(255), repeat(0),
    linewidth(1), lineinterval(10.0), lineinterval_2(10.0),
    base(0.0), base_2(0.0), density(0), vectorunit(1.0), relsize(1.0),
    extremeType("Ingen"), extremeSize(1.0), extremeRadius(1.0),
    lineSmooth(0), fieldSmooth(0), zeroLine(-1), valueLabel(1), labelSize(1.0),
    gridLines(0), gridLinesMax(0),
    undefMasking(0), undefColour(WhiteC), undefLinewidth(1),
    fptype(fpt_contour), discontinuous(0),contourShading(0),
    polystyle(poly_fill), h_align(align_left), v_align(align_bottom),
    fontname("Arial"), fontface("NORMAL"), fontsize(12.0), enabled(true)
  {
    // for Linux g++!
    limits.clear();
    values.clear();
    linevalues.clear();
    loglinevalues.clear();
    valuerange.clear();
    forecastLength.clear();
    forecastValueMin.clear();
    forecastValueMax.clear();
  }
};

#endif
