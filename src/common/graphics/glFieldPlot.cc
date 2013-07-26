#include <glFieldPlot.h>
#include <diPlotOptions.h>
#include <glpfile.h>
#include <diArea.h>
#include <diContouring.h>

#include <GL/gl.h>

#include <vector>

bool plotContour(int nx, int ny, float *field,
		 const miString& specifications,
		 FontManager *fp, float *xymap)
{
/***********************************************************************
 * ROUTINE:   plotContour
 * PURPOSE:   plot scalar field as contour lines
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
  cerr << "++ Plotter konturert felt.." << endl;
#endif

  int ipart[4];

  const int mmm = 2;
  const int mmmUsed = 100;

  float xylim[4], chxlab, chylab;
  int ismooth, labfmt[3], ibcol;
  int idraw, nlines, nlim;
  int ncol, icol[mmmUsed], ntyp, ityp[mmm], nwid, iwid[mmm];
  float zrange[2], zstep, zoff, rlines[mmmUsed], rlim[mmm];
  int idraw2, nlines2, nlim2;
  int ncol2, icol2[mmm], ntyp2, ityp2[mmm], nwid2, iwid2[mmm];
  float zrange2[2], zstep2, zoff2, rlines2[mmm], rlim2[mmm];
  int   ibmap, lbmap, kbmap[mmm], nxbmap, nybmap;
  float rbmap[4];

  int   mapconvert, ix1, ix2, iy1, iy2;
  //float cvfield2map[6], cvmap2field[6];
  float cvfield2map[6];

  mapconvert= 0;
  ix1= int(xymap[0]);
  ix2= int(xymap[1]+1.);
  iy1= int(xymap[2]);
  iy2= int(xymap[3]+1.);
  if (ix1<0)    ix1=0;
  if (ix2>nx-1) ix2=nx-1;
  if (iy1<0)    iy1=0;
  if (iy2>ny-1) iy2=ny-1;

  if (ix1>ix2 || iy1>iy2) return false;

  PlotOptions poptions;

  vector<miString> tokens= specifications.split(' ',true);
  vector<miString> parts,values;
  miString key,value;

  int n= tokens.size();

  for (int i=0; i<n; i++) {

    parts= tokens[i].split('=');

    if (parts.size()==2) {

      key=   parts[0].downcase();
      value= parts[1];

      if (key=="lineinterval") {
	poptions.lineinterval= atof(value.cStr());
      } else if (key=="linewidth") {
	poptions.linewidth= atoi(value.cStr());
      } else if (key=="linevalues") {
	values= value.split(',');
	int m= values.size();
	for (int j=0; j<m; j++)
	  poptions.linevalues.push_back(atof(values[j].cStr()));
      } else if (key=="loglinevalues") {
	values= value.split(',');
	int m= values.size();
	for (int j=0; j<m; j++)
	  poptions.loglinevalues.push_back(atof(values[j].cStr()));
      } else if (key=="valuerange") {
	values= value.split(',');
	int m= values.size();
	for (int j=0; j<m; j++)
	  poptions.valuerange.push_back(atof(values[j].cStr()));
      } else if (key=="valuelabel") {
	poptions.valueLabel= atoi(value.cStr());
      } else if (key=="linesmooth") {
	poptions.lineSmooth= atoi(value.cStr());
      } else if (key=="linecolour") {
	poptions.linecolour= Colour(value);
      } else if (key=="labelsize") {
	poptions.labelSize= atof(value.cStr());
      }
    }
  }



  float fontsize= 12. * poptions.labelSize;

  fp->set(poptions.fontname,poptions.fontface,fontsize);
  fp->getCharSize('0',chxlab,chylab);

  // the real height for numbers 0-9 (width is ok)
  chylab *= 0.75;

  ipart[0] = ix1;
  ipart[1] = ix2;
  ipart[2] = iy1;
  ipart[3] = iy2;

  xylim[0] = xymap[0];
  xylim[1] = xymap[1];
  xylim[2] = xymap[2];
  xylim[3] = xymap[3];

  zstep= poptions.lineinterval;
  zoff = poptions.base;

  if (poptions.linevalues.size()>0) {
    nlines = poptions.linevalues.size();
    for (int ii=0; ii<nlines; ii++){
      rlines[ii]= poptions.linevalues[ii];
    }
    idraw = 3;
  } else if (poptions.loglinevalues.size()>0) {
    nlines = poptions.loglinevalues.size();
    for (int ii=0; ii<nlines; ii++){
      rlines[ii]= poptions.loglinevalues[ii];
    }
    idraw = 4;
  } else {
    nlines= 0;
    idraw= 1;
    if (poptions.zeroLine==0) {
      idraw= 2;
      zoff= 0.;
    }
  }

  zrange[0]= +1.;
  zrange[1]= -1.;

  if (idraw==1 || idraw==2) {
    if (poptions.valuerange.size()==1) {
      zrange[0]= poptions.valuerange[0];
      zrange[1]= fieldUndef*0.95;
    } else if (poptions.valuerange.size()==2 &&
	       poptions.valuerange[0]<=poptions.valuerange[1]) {
      zrange[0]= poptions.valuerange[0];
      zrange[1]= poptions.valuerange[1];
    }
  }

  ncol = 1;
  icol[0] = -1; // -1: set colour below
		// otherwise index in poptions.colours[]
  ntyp = 1;
  ityp[0] = -1;
  nwid = 1;
  iwid[0] = -1;
  nlim = 0;
  rlim[0] = 0.;

  idraw2 = 0;
  zrange2[0] = +1.;
  zrange2[1] = -1.;
  zstep2 = 5000.;
  zoff2 = 0.;
  nlines2 = 0;
  ncol2 = 1;
  icol2[0] = -1;
  ntyp2 = 1;
  ityp2[0] = -1;
  nwid2 = 1;
  iwid2[0] = -1;
  nlim2 = 0;
  rlim2[0] = 0.;

  ismooth = poptions.lineSmooth;
  if (ismooth<0) ismooth=0;

  if (poptions.valueLabel==0) labfmt[0] = 0;
  else                        labfmt[0] =-1;
  labfmt[1] = 0;
  labfmt[2] = 0;
  ibcol = -1;

  ibmap  = 0;
  lbmap  = 0;
  nxbmap = 0;
  nybmap = 0;

  if (idraw==1 || idraw==2) {
    if (poptions.colours.size()>1) {
      ncol=poptions.colours.size();
      for (int i=0; i<ncol; ++i) icol[i]= i;
    }
  }

  if (ncol<2)
    glColor3ubv(poptions.linecolour.RGB());

  glLineWidth(poptions.linewidth);
  //if (poptions.linetype.bmap!=0xFFFF){
  //  glLineStipple(1,poptions.linetype.bmap);
  //  glEnable(GL_LINE_STIPPLE);
  //}

  float *x= 0;
  float *y= 0;

  Area fieldArea;
  GLPfile* psoutput;

  bool res =
	 contour(nx, ny, field, x, y,
		 ipart, mapconvert, cvfield2map, xylim,
		 idraw, zrange, zstep, zoff,
		 nlines, rlines,
		 ncol, icol, ntyp, ityp,
		 nwid, iwid, nlim, rlim,
		 idraw2, zrange2, zstep2, zoff2,
		 nlines2, rlines2,
		 ncol2, icol2, ntyp2, ityp2,
		 nwid2, iwid2, nlim2, rlim2,
		 ismooth, labfmt, chxlab, chylab,
		 ibcol,
		 ibmap, lbmap, kbmap,
		 nxbmap, nybmap, rbmap,
		 fp, poptions, psoutput, fieldArea);

  // UpdateOutput();

  if (!res) cerr << "Contour error" << endl;

#ifdef DEBUGPRINT
  cerr << "++ Returning from FieldPlot::plotContour() ++" << endl;
#endif
  //glDisable(GL_LINE_STIPPLE);
  return true;
}


void plotAlpha_shade(int nx, int ny, float *field, float *xymap)
{
  glShadeModel(GL_SMOOTH);
  glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  int ix1= int(xymap[0])   ; if (ix1<0)  ix1=0;
  int ix2= int(xymap[1])+2 ; if (ix2>nx) ix2=nx;
  int iy1= int(xymap[2])   ; if (iy1<0)  iy1=0;
  int iy2= int(xymap[3])+2 ; if (iy2>ny) iy2=ny;

  iy2--;

  float flim[8]= { 3., 10., 30., 100., 300., 1000., 3000., 10000. };
  float atab[8]= { 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1. };

//float red=1.0, green=1.0, blue=0., alpha, fval;
  float red=1.0, green=0.7, blue=0., alpha, fval;
  int ix,iy,ij,n;

  for (iy=iy1; iy<iy2; iy++) {
      glBegin(GL_QUAD_STRIP);
      for (ix=ix1; ix<ix2; ix++) {
      	  ij= iy*nx+ix;

	  fval= field[ij+nx];
	  n= 0;
	  while (n<8 && fval>flim[n]) n++;
	  n--;
	  if (n>=0) alpha= atab[n];
	  else      alpha= 0.;
	  glColor4f(red, green, blue, alpha);
	  glVertex2f(float(ix), float(iy+1));

	  fval= field[ij];
	  n= 0;
	  while (n<8 && fval>flim[n]) n++;
	  n--;
	  if (n>=0) alpha= atab[n];
	  else      alpha= 0.;
	  glColor4f(red, green, blue, alpha);
	  glVertex2f(float(ix), float(iy));

      }
      glEnd();
  }

  //UpdateOutput();

  glDisable(GL_BLEND);
  glShadeModel(GL_FLAT);
}

