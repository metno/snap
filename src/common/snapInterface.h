#ifndef _snapinterface_h_
#define _snapinterface_h_

#ifdef __cplusplus
extern "C" {
#endif

extern "C" {
  extern void c2fgetarg_(int *iarg, const char *carg, int len1);

  extern void snap_(int *iaction, int *iexit);

  extern void getmapspec_(int *gridtype, float *gridspec, float *gridpart);

  extern void getdrawspec_(int *nfields, int *nx, int *ny, int *mpos,
			   int *istep, int *nstep,
			   int *year, int *month, int *day,
			   int *hour, int *minute,
			   int *numreleased, int *numrunning,
			   int *savePNG, int *saveXPM, int *saveBMP);

  extern void getfield_(int *nfield, int *fieldid, float *field);

  extern void getposis_(int *mpos, int *npos,
			float *xpos, float *ypos, int *ipos);
}

#ifdef __cplusplus
}
#endif

#endif
