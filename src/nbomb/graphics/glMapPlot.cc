/**********************************************************************
 * SYSTEM:         
 * MODULE:         
 * FILE:           glMapPlot.cc
 * DESCRIPTION:    
 * CLASS(ES):      MapPlot
 * ROUTINES:       
 * AUTHOR:         
 * UPDATES:        
 **********************************************************************/

#include <glMapPlot.h>
#include <iostream>

#include <milib.h>

#include <math.h>
#include <stdio.h>

using namespace std;


// Default constructor
MapPlot::MapPlot()
  : drawlist(0) {
#ifdef DEBUGPRINT
  cerr << "++ MapPlot::Default Constructor" << endl;
#endif

  mapgridtype= 0;
  maplandfiles.clear();
  maplinewidths.clear();
  mapcolours.clear();
}


// Constructor
MapPlot::MapPlot(int gridtype, float *gridspec, float *gridpart,
		 const vector<miString>& landfiles,
		 const vector<miString>& colours,
		 const vector<float>& linewidths)
  : drawlist(0) {
#ifdef DEBUGPRINT
  cerr << "++ MapPlot::Constructor" << endl;
#endif

  mapgridtype= 0;
  maplandfiles.clear();
  maplinewidths.clear();
  mapcolours.clear();

  int i, n= landfiles.size();
  if (n<1) return;

  if (gridtype<1) return;

  if (colours.size()!=n || linewidths.size()!=n) {
    cerr << "ERROR. No. of landfiles:  " << n << endl;
    cerr << "       No. of colours:    " << colours.size() << endl;
    cerr << "       No. of linewidths: " << linewidths.size() << endl;
    return;
  }

  // fix indexing change from fortran to C/C++

  int ierror= 1;
  movegrid(gridtype, gridspec, 1.0, 1.0, mapgridspec, &ierror);
  if (ierror){
    cerr << "movegrid error : " << ierror
         << " (in MapPlot::MapPlot(...))" << endl;
    return;
  }

  for (i=0; i<4; i++) mapgridpart[i]= gridpart[i] - 1.;
  mapgridtype= gridtype;

  maplandfiles=  landfiles;
  maplinewidths= linewidths;
  mapcolours=    colours;

  drawMap();
}


// Destructor
MapPlot::~MapPlot(){
#ifdef DEBUGPRINT
  cerr << "++ MapPlot::Destructor" << endl;
#endif
  if (glIsList(drawlist)) glDeleteLists(drawlist,1);
}


void MapPlot::resize(float x1, float x2, float y1, float y2)
{
  mapgridpart[0]= x1;
  mapgridpart[1]= x2;
  mapgridpart[2]= y1;
  mapgridpart[3]= y2;

  drawMap();
}


void MapPlot::drawMap()
{
  if (glIsList(drawlist)) glDeleteLists(drawlist,1);

  if (mapgridtype==0) return;

  drawlist = glGenLists(1);
  glNewList(drawlist, GL_COMPILE);

  int i, n= maplandfiles.size();

  for (i=0; i<n; i++) {
    Colour col= Colour(mapcolours[i]);
    pland4(maplandfiles[i].c_str(),mapgridtype,mapgridspec,mapgridpart,
	   maplinewidths[i],col);
  }

//  // draw frame
//  // colour
//  Colour col= Colour(mapcolours[0]);
//  glColor4f(col.fR(), col.fG(), col.fB(), col.fA());
//  // linewidth
//  glLineWidth(maplinewidths[0]);
//
//  glBegin(GL_LINE_LOOP);
//  glVertex2f(mapgridpart[0],mapgridpart[2]);
//  glVertex2f(mapgridpart[1],mapgridpart[2]);
//  glVertex2f(mapgridpart[1],mapgridpart[3]);
//  glVertex2f(mapgridpart[0],mapgridpart[3]);
//  glEnd();

  glEndList();
}


bool MapPlot::plot(){
/***********************************************************************
 * ROUTINE:   MapPlot::plot
 * PURPOSE:   show the map
 * ALGORITHM: 
 * ARGUMENTS: 
 * 
 * RETURN VALUES: 
 * 
 * USES/DEPENDS ON: 
 * 
 * UPDATES: 
***********************************************************************/
#ifdef DEBUGPRINT
  cerr << "++ MapPlot::plot() ++" << endl;
#endif

  if (glIsList(drawlist)) glCallList(drawlist);

  return true;
}


int MapPlot::pland4(const char* filename, int gridtype, float gridparam[],
		    float xylim[], float linewidth, Colour colour) {
  //
  //       plot land.  data from 'standard' file, type 4.
  //
  //	 handling version 1 and 2 files.
  //	 (version 1 with unsorted lines, precision usually 1/100 degree,
  //	  version 2 with sorted lines (two level box system) with
  //	  best possible precision approx. 2m with 10x10 and 1x1 degree
  //	  boxes).
  //
  //       graphigs: OpenGL
  //
  //     input:
  //     ------
  //        filename:  file name
  //        gridtype:   grid type; 1=polarstereographic (true at 60N)
  //                               2=geographic
  //                               3=spherical (rotated)
  //                               4=polarstereographic
  //                               5=mercator (unrotated)
  //        grid[0-5]:  the grid description parameters
  //        xylim[0-3]: x1,x2,y1,y2
  //        linetype:   line type bitmask
  //        linewidth:  line width
  //        colour:     colour-components rgba
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //  standard file structure:
  //  ------------------------
  //  - record format: access='direct',form='unformatted'
  //  - record length: 2048 bytes
  //  - file header (first part of record 1), integer*2 words:
  //      1 - identifier = 104
  //      2 - version    = 1
  //      3 - record length in unit bytes = 2048
  //      4 - length of data description (words)
  //  - data description (second part of record 1, minimum 6 words),
  //    integer*2 words:
  //      1 - data type:   1=lines      2=polygons/lines
  //      2 - data format: 1=integer*2
  //      3 - no. of words describing each line (in front of each line)
  //      4 - coordinate type: 1=latitude,longitude
  //      5 - scaling of coordinates (real_value = 10.**scale * file_value)
  //      6 - 0 (not used)
  //  - data follows after file header and data description.
  //  - each line has a number of words describing it in front of the
  //    coordinate pairs.
  //  - line description (minimum 1 word):
  //      1 - no. of positions (coordinate pairs) on the line.
  //          0 = end_of_data
  //      2 - possible description of the line (for plot selection):
  //            1=land/sea  2=borders  3=lakes  4=rivers (values 1-100).
  //      3 - line type:  1=line  2=polygon
  //      restriction: a line desciption is never split into two records
  //                   (i.e. possibly unused word(s) at end_of_record)
  //  - line coordinates:
  //          latitude,longitude, latitude,longitude, ......
  //      restriction: a coordinate pair is never split into two records
  //                   (i.e. possibly one unused word at end_of_record)
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  //  current version is not plotting polygons (only plotting the borders)
  //
  //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  //
  //---------------------------------------------------------------------
  //  DNMI/FoU  29.07.1992  Anstein Foss
  //  DNMI/FoU  29.08.1995  Anstein Foss
  //  DNMI/FoU  24.09.1995  Anstein Foss ... utskrift for UNIRAS (ascii)
  //  DNMI/FoU  30.01.1996  Anstein Foss ... version 2 file (sorted)
  //  DNMI/FoU  09.08.1999  Anstein Foss ... C/C++ ... diMapPlot (diana)
  //  DNMI/FoU  17.02.2001  Anstein Foss ... glMapPlot
  //---------------------------------------------------------------------
    
  const int nwrec  = 1024;
  const int maxpos = 2000;
  const int mlevel1 = 36*18;
  const int mlevel2 = 10*10;
    
  short indata[nwrec];
  float x[maxpos], y[maxpos];

  int irec, jrec, nwr, nw, nd, err, nwdesc, npos, n,  version;
  int iscale2, nlevel1, nlevel2, np, npi, npp, i, j, ibgn, ierror;
  int n1, n2, nn, nwx1, nwx2, nlines, nl;
    
  float scale, slat2, slon2, x1, y1, x2, y2, dx, dy, dxbad;
  float glon, glat, glonmin, glonmax, glatmin, glatmax, reflon, reflat;
    
  float box1[4], box2[4];
    
  short int ilevel1[mlevel1][5];
  short int ilevel2[mlevel2][5];
    
  FILE *pfile;
  long offset;

  // a geographic grid that matches longitude,latitude coordinates
  int  igeogrid = 2;
  float geogrid[6] = {1., 1., 1., 1., 0., 0.};

  // colour
  glColor4f(colour.fR(),colour.fG(),colour.fB(),colour.fA());
  // linewidth
  glLineWidth(linewidth);
  // linetype
  //if (linetype!=0xFFFF){
  //  glLineStipple(1,linetype);
  //  glEnable(GL_LINE_STIPPLE);
  //}

  if ( (pfile = fopen(filename, "rb")) == NULL)
    {
      return 1;
    }
    
  irec = 1;
  nwr = fread(indata, 2, nwrec, pfile);
  if (nwr != nwrec)
    {
      fclose(pfile);
      return 2;
    }
    
  // check file header
  err = 0;
  if(indata[0] != 104) err = 1;
  if(indata[1] != 1 && indata[1] != 2) err = 1;
  if(indata[2] != 2048) err = 1;
  if(indata[3] <  6) err = 1;
  version = indata[1];
  if(version == 2 && indata[3] < 18) err = 1;
  nw = 3;
  nd = indata[3];
  if(nw+nd >= nwrec) err = 1;
  // data description
  if(indata[nw+1] != 1 && indata[nw+1] != 2) err = 1;
  if(indata[nw+2] != 1) err = 1;
  nwdesc = indata[nw+3];
  if(nwdesc<1) err = 1;
  if(indata[nw+4] != 1) err = 1;
    
  if (err != 0)
    {
      fclose(pfile);
      return 3;
    }

  // for version 1 this is the scaling of all values (lat,long)
  // for version 2 this is the scaling of reference values (lat,long)
  scale = indata[nw+5];
  scale = powf(10., scale);
  if(version==2)
    {
      iscale2 = indata[nw+6];
      nlevel1 = indata[nw+7];
      box1[0] = indata[nw+11]*scale;
      box1[1] = indata[nw+12]*scale;
      box1[2] = indata[nw+13]*scale;
      box1[3] = indata[nw+14]*scale;
      box2[0] = indata[nw+15]*scale;
      box2[1] = indata[nw+16]*scale;
      box2[2] = indata[nw+17]*scale;
      box2[3] = indata[nw+18]*scale;
    }
      
  nw = nw + nd;

  if(gridtype != 1 && gridtype != 4)
    {
      x[0]=-170.;
      x[1]=+170.;
      y[0]=60.;
      y[1]=60.;
      npos=2;
      xyconvert(npos,x,y,igeogrid,geogrid,
		gridtype,gridparam,&ierror);
      dxbad=fabsf(x[0]-x[1]);
    }
  else dxbad=1.e+35;


  if (version == 1)
    {

      // version 1 (not sorted)

      npos = 1;

      while (npos>0)
	{
      
          // get line description
          if(nw+nwdesc >= nwrec)
	    {
	      irec++;
	      nwr = fread(indata, 2, nwrec, pfile);
	      if (nwr != nwrec)
		{
		  fclose(pfile);
		  return 2;
		}
	      nw = -1;
	    }

          npos = indata[nw+1];

          // current version: not using more than first word of line description
          nw = nw + nwdesc;

          np  = 0;
          npp = 0;

          while (npp < npos)
	    {
	      if(nw+2 >= nwrec)	{
		irec++;
		nwr = fread(indata, 2, nwrec, pfile);
		if (nwr != nwrec)
		  {
		      fclose(pfile);
		      return 2;
		    }
		  nw = -1;
		}
	      npi = npos - npp;
	      if(npi*2 > nwrec-nw-1) npi = (nwrec-nw-1)/2;
	      if(npi > maxpos-np)    npi = maxpos-np;
	      
	      for (n=0; n<npi; ++n,  nw+=2)
		{
		  //  y[] = latitude  = scale * indata[i]
		  //  x[] = longitude = scale * indata[i+1]
		  y[np+n] = scale*indata[nw+1];
		  x[np+n] = scale*indata[nw+2];
		}
	       
	      npp = npp + npi;
	      np  = np  + npi;
	    
              if ((npp==npos || np==maxpos) && np>1)
		{
		  x1 = x[np-1];
		  y1 = y[np-1];

		  // convert coordinates from longitude,latitude to x,y
		  xyconvert(np,x,y,igeogrid,geogrid,
			    gridtype,gridparam,&ierror);

	          if (gridtype==1 || gridtype==4)
	            xyclip(np,&x[0],&y[0],&xylim[0]);                         
		  else
		    {
		      // some problems handled here (but not good, may loose lines)
		      // skip lines crossing the 180 degrees meridian
		      ibgn = 0;
		      for (i=1; i<np; ++i)
			{
			  if (fabsf(x[i-1]-x[i])>dxbad)
			    {
			      // plot (part of line inside xylim area)
			      if(i-ibgn>1) xyclip(i-ibgn,&x[ibgn],&y[ibgn],&xylim[0]);                
			      ibgn=i;
			    }
			}
		      // plot (part of line inside xylim area)
                      if(np-ibgn>1) xyclip(np-ibgn,&x[ibgn],&y[ibgn],&xylim[0]);
		    }
		
		  x[0] = x1;
		  y[0] = y1;
		  np = 1;
		}
            }	       
	}
    }
  else
    {
      // version 2 (two level sorted)
	
      if (nlevel1>mlevel1) return 4;

      if(box2[0] > box2[1]) slat2 = box2[0]/float(iscale2);
      else                  slat2 = box2[1]/float(iscale2);
      if(box2[2] > box2[3]) slon2 = box2[2]/float(iscale2);
      else                  slon2 = box2[3]/float(iscale2);

      for (n1=0; n1<nlevel1; ++n1)
	{
	  for (i=0; i<5; ++i)
	    {
	      nw++;
	      if(nw>=nwrec)
		{
		  irec++;
		  nwr = fread(indata, 2, nwrec, pfile);
		  if (nwr != nwrec)
		    {
		      fclose(pfile);
		      return 2;
		    }
		  nw = 0;
		}
	      ilevel1[n1][i] = indata[nw];
	    }
	}

      // first simple attempt
      nn = int(sqrtf(float(maxpos)));
      if (nn>16) nn=16;
      x1 = xylim[0];
      x2 = xylim[1];
      y1 = xylim[2];
      y2 = xylim[3];
      dx = (x2-x1)/float(nn-1);
      dy = (y2-y1)/float(nn-1);
      n=0;
      for (j=0; j<nn; ++j)
	{
	  for (i=0; i<nn; ++i, ++n)
	    {
	      x[n] = x1 + i * dx;
	      y[n] = y1 + j * dy;
	    }
        }
      nn = n;
      xyconvert(nn,x,y,gridtype,gridparam,
		igeogrid,geogrid,&ierror);
      glonmin = x[0];
      glonmax = x[0];
      glatmin = y[0];
      glatmax = y[0];
      for (n=1; n<nn; ++n)
	{
	  if (glonmin > x[n]) glonmin = x[n];
	  if (glonmax < x[n]) glonmax = x[n];
	  if (glatmin > y[n]) glatmin = y[n];
	  if (glatmax < y[n]) glatmax = y[n];
        }
      glonmin = glonmin - 1.;
      glonmax = glonmax + 1.;
      glatmin = glatmin - 1.;
      glatmax = glatmax + 1.;
      if (gridtype != 2 && glonmax-glonmin>300)
	{
	  glonmin = -180.;
	  glonmax = +180.;
	  if (gridtype==1 && gridparam[4]>=0.) glatmax = +90.;
	  if (gridtype==4 && gridparam[4]>=0.) glatmax = +90.;
	  if (gridtype==3 && gridparam[5]>=0.) glatmax = +90.;
	  if (gridtype==1 && gridparam[4]< 0.) glatmin = -90.;
	  if (gridtype==4 && gridparam[4]< 0.) glatmin = -90.;
	  if (gridtype==3 && gridparam[5]< 0.) glatmin = -90.;
        }

      for (n1=0; n1<nlevel1; ++n1)
	{
	  glat    = ilevel1[n1][0]*scale;
	  glon    = ilevel1[n1][1]*scale;
	  nlevel2 = ilevel1[n1][2];
	    
	  if (nlevel2>mlevel2) return 5;

	  if (glat+box1[1]>=glatmin &&
      	      glat+box1[0]<=glatmax &&
     	      glon+box1[3]>=glonmin &&
              glon+box1[2]<=glonmax && nlevel2>0)
	    {
	      jrec = irec;
	      nwx1 = ilevel1[n1][3];
	      nwx2 = ilevel1[n1][4];
	      irec = (nwx1+nwx2*32767+nwrec-1)/nwrec;
	      nw   = (nwx1+nwx2*32767)-(irec-1)*nwrec-2;
	
	      if (irec != jrec)
		{
		  offset = (irec-1) * nwrec * 2;
		  if (fseek(pfile, offset, SEEK_SET) != 0)
		    {
		      fclose(pfile);
		      return 2;
		    }
	    
		  nwr = fread(indata, 2, nwrec, pfile);
		  if (nwr != nwrec)
		    {
		      fclose(pfile);
		      return 2;
		    }
		}
	    
	      for (n2=0; n2<nlevel2; ++n2)
		{
		  for (i=0; i<5; ++i)
		    {
		      nw++;
		      if(nw>=nwrec)
			{
			  irec++;
			  nwr = fread(indata, 2, nwrec, pfile);
			  if (nwr != nwrec)
			    {
			      fclose(pfile);
			      return 2;
			    }
			  nw = 0;
			}
		      ilevel2[n2][i] = indata[nw];
		    }
		}

	      for (n2=0; n2<nlevel2; ++n2)
		{
		  reflat = ilevel2[n2][0]*scale;
		  reflon = ilevel2[n2][1]*scale;
		  nlines = ilevel2[n2][2];

		  if (reflat+box2[1]>=glatmin &&
		      reflat+box2[0]<=glatmax &&
		      reflon+box2[3]>=glonmin &&
		      reflon+box2[2]<=glonmax && nlines>0)
		    {
		      jrec = irec;
		      nwx1 = ilevel2[n2][3];
		      nwx2 = ilevel2[n2][4];
		      irec = (nwx1+nwx2*32767+nwrec-1)/nwrec;
		      nw   = (nwx1+nwx2*32767)-(irec-1)*nwrec-2;

		      if (irec != jrec)
			{
			  offset = (irec-1) * nwrec * 2;
			  if (fseek(pfile, offset, SEEK_SET) != 0)
			    {
			      fclose(pfile);
			      return 2;
			    }
	    
			  nwr = fread(indata, 2, nwrec, pfile);
			  if (nwr != nwrec)
			    {
			      fclose(pfile);
			      return 2;
			    }
			}

		      for (nl=0; nl<nlines; ++nl)
			{
			  // get line description
			  if(nw+nwdesc >= nwrec)
			    {
			      irec++;
			      nwr = fread(indata, 2, nwrec, pfile);
			      if (nwr != nwrec)
				{
				  fclose(pfile);
				  return 2;
				}
			      nw = -1;
			    }

			  npos = indata[nw+1];

			  // current version:
			  // not using more than first word of line description
			  nw = nw + nwdesc;

			  np  = 0;
			  npp = 0;

			  while (npp < npos)
			    {
			      if(nw+2 >= nwrec)
				{
				  irec++;
				  nwr = fread(indata, 2, nwrec, pfile);
				  if (nwr != nwrec)
				    {
				      fclose(pfile);
				      return 2;
				    }
				  nw = -1;
				}
			      npi = npos - npp;
			      if(npi*2 > nwrec-nw-1) npi = (nwrec-nw-1)/2;
			      if(npi > maxpos-np)    npi = maxpos-np;
		    
			      for (n=0; n<npi; ++n, nw+=2)
				{
                                  // y[] = latitude
                                  // x[] = longitude
                                  y[np+n] = reflat + slat2 * indata[nw+1];
                                  x[np+n] = reflon + slon2 * indata[nw+2];
				}
	       
			      npp = npp + npi;
                              np  = np  + npi;
		    
                              if ((npp==npos || np==maxpos) && np>1)
				{
				  x1 = x[np-1];
				  y1 = y[np-1];

				  // convert coordinates from longitude,latitude to x,y
				  xyconvert(np,x,y,igeogrid,geogrid,
				            gridtype,gridparam,&ierror);

	                          if (gridtype==1 || gridtype==4)
		                    xyclip(np,&x[0],&y[0],&xylim[0]);
				  else
				    {
				      // some problems handled here (but not good, may loose lines)
				      // skip lines crossing the 180 degrees meridian
				      ibgn = 0;
				      for (i=1; i<np; ++i)
					{
					  if (fabsf(x[i-1]-x[i])>dxbad)
					    {
					      // plot (part of line inside xylim area)
					      if(i-ibgn>1) xyclip(i-ibgn,&x[ibgn],&y[ibgn],&xylim[0]);
					      ibgn=i;
					    }
					}
				      // plot (part of line inside xylim area)
                                      if(np-ibgn>1) xyclip(np-ibgn,&x[ibgn],&y[ibgn],&xylim[0]);
				    }
	       
			          x[0] = x1;
			          y[0] = y1;
			          np = 1;
			        }
			    }
		        }
		    }
	        }
            }
        }
    }

  fclose(pfile);

  //glDisable(GL_LINE_STIPPLE);

  return 0;
}


void MapPlot::xyclip(int npos, float x[], float y[], float xylim[])
{
  //  plotter del(er) av sammenhengende linje som er innenfor gitt
  //  omraade, ogsaa linjestykker mellom 'nabopunkt' som begge er
  //  utenfor omraadet.
  //  (farge, linje-type og -tykkelse maa vaere satt paa forhaand)
  //  
  //  grafikk: OpenGL
  //  
  //  input:
  //  ------
  //  x(npos),y(npos): linje med 'npos' punkt (npos>1)
  //  xylim(1-4):      x1,x2,y1,y2 for aktuelt omraade
  
  int   nint, nc, n, i, k1, k2;
  float xa, xb, ya, yb, xint, yint, x1, x2, y1, y2;
  float xc[4], yc[4];
  
  if (npos<2) return;
  
  xa = xylim[0];
  xb = xylim[1];
  ya = xylim[2];
  yb = xylim[3];
  
  if (x[0]<xa || x[0]>xb || y[0]<ya || y[0]>yb)
    {
      k2 = 0;
    }
  else
    {
      k2 = 1;
      nint = 0;
      xint = x[0];
      yint = y[0];
    }
  
  for (n=1; n<npos; ++n)
    {
      k1=k2;
      k2=1;
      
      if (x[n]<xa || x[n]>xb || y[n]<ya || y[n]>yb) k2=0;
      
      // sjekk om 'n' og 'n-1' er innenfor
      if (k1+k2==2) continue;
      
      // k1+k2=1: punkt 'n' eller 'n-1' er utenfor
      // k1+k2=0: sjekker om 2 nabopunkt som begge er utenfor omraadet
      //          likevel har en del av linja innenfor.
      
      x1 = x[n-1];
      y1 = y[n-1];
      x2 = x[n];
      y2 = y[n];
      
      // sjekker om 'n-1' og 'n' er utenfor paa samme side
      if (k1+k2==0  && 
	  ((x1<xa && x2<xa) || (x1>xb && x2>xb) ||
	   (y1<ya && y2<ya) || (y1>yb && y2>yb))) continue;
      
      // sjekker alle skjaerings-muligheter
      nc = -1;
      if (x1 != x2)
	{
	  nc++;
	  xc[nc] = xa;
	  yc[nc] = y1+(y2-y1)*(xa-x1)/(x2-x1);
	  if (yc[nc]<ya || yc[nc]>yb || (xa-x1)*(xa-x2)>0.) nc--;
	  nc++;
	  xc[nc] = xb;
	  yc[nc] = y1+(y2-y1)*(xb-x1)/(x2-x1);
	  if (yc[nc]<ya || yc[nc]>yb || (xb-x1)*(xb-x2)>0.) nc--;
        }
      if(y1 != y2)
	{
	  nc++;
	  yc[nc] = ya;
	  xc[nc] = x1+(x2-x1)*(ya-y1)/(y2-y1);
	  if (xc[nc]<xa || xc[nc]>xb || (ya-y1)*(ya-y2)>0.) nc--;
	  nc++;
	  yc[nc] = yb;
	  xc[nc] = x1+(x2-x1)*(yb-y1)/(y2-y1);
	  if (xc[nc]<xa || xc[nc]>xb || (yb-y1)*(yb-y2)>0.) nc--;
        }
      
      if(k2==1)
	{
	  // foerste punkt paa linjestykke innenfor
	  nint = n-1;
	  xint = xc[0];
	  yint = yc[0];
	}  
      else if (k1==1)
	{
	  // siste punkt paa linjestykke innenfor
	  glBegin(GL_LINE_STRIP);
	  glVertex2f(xint, yint);
	  for (i=nint+1; i<n; i++) glVertex2f(x[i], y[i]);
	  glVertex2f(xc[0], yc[0]);
	  glEnd();
	}
      else if (nc>0)
	{
	  // to 'nabopunkt' utenfor, men del av linja innenfor
	  glBegin(GL_LINE_STRIP);
	  glVertex2f(xc[0], yc[0]);
	  glVertex2f(xc[1], yc[1]);
	  glEnd();
        }
    }
  
  if(k2==1)
    {
      // siste punkt er innenfor omraadet
      glBegin(GL_LINE_STRIP);
      glVertex2f(xint, yint);
      for (i=nint+1; i<npos; i++) glVertex2f(x[i], y[i]);
      glEnd();
    }
}
