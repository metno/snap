/**********************************************************************
 * SYSTEM:         
 * MODULE:         
 * FILE:           diMapPlot.h
 * DESCRIPTION:    
 * CLASS(ES):      MapPlot
 * ROUTINES:       
 * AUTHOR:         Anstein Foss
 * UPDATES:        
 **********************************************************************/

#ifndef glMapPlot_h
#define glMapPlot_h

#include <vector>

#include <miString.h>
#include <diColour.h>

#include <GL/gl.h>

using namespace std;

class MapPlot {
private:
  GLuint drawlist;    // OpenGL drawlist
  int   mapgridtype;
  float mapgridspec[6];
  float mapgridpart[4];
  vector<miString> maplandfiles;
  vector<miString> mapcolours;
  vector<float>    maplinewidths;

  void drawMap();
  int pland4(const char* filename, int gridtype, float gridparam[],
	     float xylim[], float linewidth, Colour colour);
  void xyclip(int npos, float x[], float y[], float xylim[]);

  // Copy members
  void memberCopy(const MapPlot& rhs);
  // Equality operator
  bool operator==(const MapPlot &rhs) const;

public:
  // Constructors
  MapPlot();
  MapPlot(int gridtype, float *gridspec, float *gridpart,
	  const vector<miString>& landfiles,
	  const vector<miString>& colours,
	  const vector<float>& linewidths);
  // Destructor
  ~MapPlot();
  // Assignment operator
  MapPlot& operator=(const MapPlot &rhs);
  // resize map area
  void resize(float x1, float x2, float y1, float y2);
  // plot map
  bool plot();
};

#endif
