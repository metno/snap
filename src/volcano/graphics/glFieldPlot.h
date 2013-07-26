/**********************************************************************
 * SYSTEM:         
 * MODULE:         
 * FILE:           glFieldPlot.h
 * DESCRIPTION:    
 * CLASS(ES):
 * ROUTINES:       
 * AUTHOR:         Anstein Foss
 * UPDATES:        
 **********************************************************************/

#ifndef glFieldPlot_h
#define glFieldPlot_h

#include <miString.h>
#include <diFontManager.h>

const float fieldUndef=+1.e+35;

using namespace std;

bool plotContour(int nx, int ny, float *field,
		 const miString& specifications,
		 FontManager *fp, float *xymap);

void plotAlpha_shade(int nx, int ny, float *field, float *xymap);

#endif
