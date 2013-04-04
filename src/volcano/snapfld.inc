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
      real    u1(nx,ny,nk),v1(nx,ny,nk),w1(nx,ny,nk),t1(nx,ny,nk)
     -       ,ps1(nx,ny),bl1(nx,ny),hbl1(nx,ny)
     -       ,hlevel1(nx,ny,nk),hlayer1(nx,ny,nk)
     -       ,u2(nx,ny,nk),v2(nx,ny,nk),w2(nx,ny,nk),t2(nx,ny,nk)
     -       ,ps2(nx,ny),bl2(nx,ny),hbl2(nx,ny)
     -       ,hlevel2(nx,ny,nk),hlayer2(nx,ny,nk)
     -       ,xm(nx,ny),ym(nx,ny),garea(nx,ny)
     -       ,field1(nx,ny),field2(nx,ny),field3(nx,ny)
     -       ,field4(nx,ny)
     -       ,pmsl1(nxad,nyad),pmsl2(nxad,nyad)
     -       ,precip(nx,ny,mprecip)
      integer nprecip,iprecip
c
      common/fieldi/u1,v1,w1,t1,ps1,bl1,hbl1,hlevel1,hlayer1
     -             ,u2,v2,w2,t2,ps2,bl2,hbl2,hlevel2,hlayer2
     -             ,xm,ym,garea
     -		   ,field1,field2,field3,field4
     -		   ,pmsl1,pmsl2
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
      double precision dgarea(nx,ny)
     -                ,depdry(nx,ny,mcomp),depwet(nx,ny,mcomp)
     -                ,accdry(nx,ny,mcomp),accwet(nx,ny,mcomp)
     -                ,concen(nx,ny,mcomp),concacc(nx,ny,mcomp)
     -		      ,avghbl(nx,ny),avgprec(nx,ny),accprec(nx,ny)
     -		      ,avgbq1(nx,ny,mcomp),avgbq2(nx,ny,mcomp)
     -		      ,avgbq(nxmc,nymc,nk-1,mcomp)
c
      common/fieldo/dgarea
     -             ,depdry,depwet,accdry,accwet
     -             ,concen,concacc
     -		   ,avghbl,avgprec,accprec
     -		   ,avgbq1,avgbq2
     -		   ,avgbq
c-----------------------------------------------------------------------
