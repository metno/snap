c-----------------------------------------------------------------------
c snapfld.inc
c
c..include file  -  common for fields
c
c
c..1:      input time step 1
c..2:      input time step 2
c..u,v:    horizontal wind components
c..w:      sigma_dot
c..t:      potential temperature (level 1, surface: abs. temp. 2m)
c..ps:     surface pressure
c..bl:     boudary layer top in sigma coordinate
c..hbl:    boudary layer depth in meters
c..xm:     map ratio in x direction
c..ym:     map ratio in y direction
c..garea:  grid square area (m**2)
c..field*: work arrays
c..pmsl:   mean sea level pressure (mslp, not used in computations)
c	   for graphics and/or output (possible if nxad=nx and nyad=ny)
c..precip: hourly precipitation intensity (mm/hour)
c..nprecip: no. of steps stored
c..iprecip: the current precipitation field
c
      REAL(kind=4), DIMENSION(:,:,:), POINTER ::
     +   u1, v1, w1, t1, hlevel1, hlayer1,
     +   u2, v2, w2, t2, hlevel2, hlayer2,
     +   precip

      REAL(kind=4), DIMENSION(:,:), POINTER ::
     +  ps1, bl1, hbl1,
     +  ps2, bl2, hbl2,
     +  xm, ym, garea, field1, field2, field3,
     +  field4, pmsl1, pmsl2

      integer nprecip,iprecip
c
      common/fieldi/u1,v1,w1,t1,ps1,bl1,hbl1,hlevel1,hlayer1
     -             ,u2,v2,w2,t2,ps2,bl2,hbl2,hlevel2,hlayer2
     -             ,xm,ym,garea
     -             ,field1,field2,field3,field4
     -             ,pmsl1,pmsl2
     -             ,precip,nprecip,iprecip
c
c
c..dgarea:  grid square area (m**2) ... double precision
c..depdry:  dry deposition
c..depwet:  wet deposition
c..accdry:  accumulated dry deposition
c..accwet:  accumulated wet deposition
c..concacc: accumulated/integrated concentration
c..avghbl:  average height of boundary layer (accumulation)
c..avgprec: precipitation  (accumulation)
c..accprec: accumulation of precipitation from start of run
c..avgbq1:  average Bq (per square area) in boundary layer (accum.)
c..avgbq2:  average Bq (per square area) above boundary layer (accum.)
c..avgbq:   average Bq (per square area) in each layer (accum.)
c..	    only used if (nxmc=nx, nymc=ny and imodlevel=1)
c
      REAL(kind=8), DIMENSION(:,:), POINTER ::
     +  dgarea, avghbl, avgprec, accprec
      REAL(kind=8), DIMENSION(:,:,:), POINTER ::
     +  depdry, depwet, accdry, accwet,
     +  concen, concacc, avgbq1, avgbq2
      REAL(kind=8), DIMENSION(:,:,:,:), POINTER ::
     +  avgbq
c
      common/fieldo/dgarea
     -             ,depdry,depwet,accdry,accwet
     -             ,concen,concacc
     -         ,avghbl,avgprec,accprec
     -         ,avgbq1,avgbq2
     -         ,avgbq
c-----------------------------------------------------------------------
