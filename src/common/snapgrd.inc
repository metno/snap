c-----------------------------------------------------------------------
c snapgrd.inc
c
c..include file  -  common for grid specifications etc.
c
c
c..iprod:         input producer no.
c..igrid:         input grid no.
c..klevel(nk):    model level no.
c                 sequence: bottom to top (kk,kk-1,....1)
c                 level no.  1 should be 0    (surface fields here)
c                 level no.  2 should be 'kk' (no. of levels)
c                 level no. nk should be 1    (current SNAP version)
c..iprodr:        output producer no. (usually as input)
c..igridr:        output grid no. (may be different from input)
c..ixbase,iybase: lower left corner in input fields
c                 (size is set by nx,ny at compilation time)
c..ixystp:        step in x and y direction
c                 (ixystp>1 means lower resolution fields than input,
c                  to decrease memory size)
c..inprecip:      0=not read precipitation  1=read precipitation
c..		  -1=read precipitation (but there is no wet deposition)
c..imslp:         0=not read mslp  -1=read mslp  1=read and write mslp
c		  (mslp not used in computations)
c..imodlevel:     0=not output of concentrations in model levels
c		  1=output (possible if nxmc=nx and nymc=ny)
c..modleveldump: remove particles from model after at least that many steps
c..itotcomp:      0=not output of total of all components
c		  (e.g. when each component is released with a mass unit)
c                 1=output of total of all components
c
      integer iprod,igrid
c klevel needs to get allocated in main program while reading parameters
      INTEGER, DIMENSION(:), POINTER ::  klevel
      integer iprodr,igridr,ixbase,iybase,ixystp
      integer inprecip,imslp,imodlevel,itotcomp
      real modleveldump
c
      common/cgrid1/iprod,igrid,klevel
     -             ,iprodr,igridr,ixbase,iybase,ixystp
     -		   ,inprecip,imslp,imodlevel,itotcomp,modleveldump
c
c
c..gparam:  grid parameters (depending on the grid type)
c..  polarstereographic (igtype=1,4):
c..    gparam(1): xp     - x position of north pole
c..    gparam(2): yp     - y position of north pole
c..    gparam(3): an     - no. grid units between the pole and equator
c..    gparam(4): fi     - grid rotation angle
c                          (y axis compared to longitude 0, +/- = e/w)
c..    gparam(5): 60.    - projection latitude (usually 60., i.e. 60N)
c..    gparam(6): 0.     - (not used)
c..    gparam(7): dxgrid - grid resolution in meters, x direction
c..    gparam(8): dygrid - grid resolution in meters, y direction
c..  geographic and spherical rotated (igtype=2,3):
c..    gparam(1): blon   - west  longitude (x)
c..    gparam(2): blat   - south latitude  (y)
c..    gparam(3): dlon   - longitude resolution
c..    gparam(4): dlat   - latitude  resolution
c..    gparam(5): clon   - centre longitude (0 if not rotated)
c..    gparam(6): clat   - centre latitude  (0 if not rotated)
c..    gparam(7): dxgrid - grid resolution in meters in x direction
c                          (longitude)
c..    gparam(8): dygrid - grid resolution in meters in y direction
c                          (latitude)
c..alevel:    eta a_level (sigma levels: alevel=ptop*(1.-sigma))
c..blevel:    eta b_level (sigma levels: blevel=sigma)
c..vlevel:    vertical level (sigma or eta)
c..igtype:    grid type, 1=polarstereographic
c..                      2=geographic
c..                      3=spherical (rotated)
c..ivcoor:    vertical coordinate,  2=sigma (Norlam)
c..                                10=eta   (Hirlam,...))
c..kadd:      levels added at the top (when missing upper model levels)
c..	      (u,v copied up, w reduced, pot.temp. const.)
c..ivlevel:   tabel of level numbers for interpolation
c..	      (key is vlevel*10000)
c..ivlayer:   tabel of layer numbers for concentration in model levels
c..	      (key is vlevel*10000)
c
      real    gparam(8)
c nk dependent arrays
      REAL, DIMENSION(:), POINTER :: alevel,blevel,vlevel
      REAL, DIMENSION(:), POINTER :: ahalf,bhalf,vhalf
      integer igtype,ivcoor,kadd
      integer ivlevel(0:10000),ivlayer(0:10000)
c
      common/cgrid2/gparam
     -             ,alevel,blevel,vlevel
     -             ,ahalf,bhalf,vhalf
     -             ,igtype,ivcoor,kadd
     -             ,ivlevel,ivlayer
c-----------------------------------------------------------------------
