#include <string.h>
#include <milib.h>

/* 
------ MILIB EXTERNAL FORTRAN ROUTINES --------
*/

extern void gridpar_(int *icall, int *ldata, short *idata,
		     int *igtype, int *nx, int *ny,
		     float *grid, int *ierror);

extern void mrfelt_(int *mode, const char *filename, int *iunit,
		    short *in, int *ipack, int *lfield,
		    float *field, float *fscale, int *ldata,
		    short *idata, int *ierror, int len1);

extern void mwfelt_(int *mode, const char *filename, int *iunit,
		    int *ipack, int *lfield,
		    float *field, float *fscale, int *ldata,
		    short *idata, int *ierror, int len1);

extern void qfelt_(int *iunit, int *ireq, int *iexist, int *nin, short *in,
		   int *ifound, int *nfound,
 		   int *iend, int *ierror, int *ioerr);

extern void crefelt_(const char *filename, int *iunit, int *itype,
		     int *ltime, int *itime,
		     int *icode, int *lspec, short *ispec,
		     int *lopt, int *iopt, int *ierror, int len1);

extern void xyconvert_(int *npos, float *x, float *y,
		       int *igtypa, float *ga,
		       int *igtypr, float *gr, int *ierror);

extern void uvconvert_(int *npos, float *xr, float *yr,
		       float *u, float *v,
		       int *igtypa, float *ga,
		       int *igtypr, float *gr, float *udef, int *ierror);

extern void mapfield_(int *imapr, int *icori, int *igtype, float *grid,
		      int *nx, int *ny, float *xm, float *ym, float *fc,
		      float *hx, float *hy, int *ierror);

extern void movegrid_(int *igtype, float *grid, float *dx, float *dy,
		      float *gridmv, int *ierror);

extern void xyconst_(int *igtyp1, float *grid1, int *igtyp2, float *grid2,
		     float *cxy1, float *cxy2, int *ierror);

/* 
------ C-WRAPPERS -----------------------------
*/

void gridpar(int icall, int ldata, short *idata,
	     int *igtype, int *nx, int *ny,
	     float *grid, int *ierror){
  gridpar_(&icall, &ldata, idata, igtype, nx, ny, grid, ierror);
}


void mrfelt(int mode, const char *filename, int iunit,
	    short in[16], int ipack, int lfield,
	    float *field, float fscale, int ldata,
	    short *idata, int *ierror){
  int flen=strlen(filename)+1;
  
  mrfelt_(&mode, filename, &iunit, in, &ipack, &lfield,
	  field, &fscale, &ldata, idata, ierror, flen);
}


void mwfelt(int mode, const char *filename, int iunit,
	    int ipack, int lfield,
	    float *field, float fscale, int ldata,
	    short *idata, int *ierror){
  int flen=strlen(filename)+1;

  mwfelt_(&mode, filename, &iunit, &ipack, &lfield,
	  field, &fscale, &ldata, idata, ierror, flen);
}


void qfelt(int iunit, int ireq, int iexist, int nin, short *in,
	   int *ifound, int *nfound,
 	   int *iend, int *ierror, int *ioerr){

  qfelt_(&iunit, &ireq, &iexist, &nin, in,
	 ifound, nfound, iend, ierror, ioerr);
}


void crefelt(const char *filename, int iunit, int itype,
	     int ltime, int *itime,
	     int icode, int lspec, short *ispec,
	     int lopt, int *iopt, int *ierror){
  int flen=strlen(filename)+1;

  crefelt_(filename, &iunit, &itype, &ltime, itime,
	   &icode, &lspec, ispec, &lopt, iopt, ierror, flen);
}


void xyconvert(int npos, float *x, float *y,
	       int igtypa, float *ga,
	       int igtypr, float *gr, int *ierror){

  xyconvert_(&npos, x, y, &igtypa, ga, &igtypr, gr, ierror);
}


void uvconvert(int npos, float *xr, float *yr,
	       float *u, float *v,
	       int igtypa, float *ga,
	       int igtypr, float *gr, float udef, int *ierror){

  uvconvert_(&npos, xr, yr, u, v,
	     &igtypa, ga, &igtypr, gr, &udef, ierror);
}


void mapfield(int imapr, int icori, int igtype, float *grid,
	      int nx, int ny, float *xm, float *ym, float *fc,
	      float *hx, float *hy, int *ierror){

  mapfield_(&imapr, &icori, &igtype, grid,
	    &nx, &ny, xm, ym, fc, hx, hy, ierror);
}


void movegrid(int igtype, float *grid, float dx, float dy,
	      float *gridmv, int *ierror){

  movegrid_(&igtype, grid, &dx, &dy, gridmv, ierror);
}


void xyconst(int igtyp1, float *grid1, int igtyp2, float *grid2,
	     float *cxy1, float *cxy2, int *ierror){

  xyconst_(&igtyp1, grid1, &igtyp2, grid2, cxy1, cxy2, ierror);
}
