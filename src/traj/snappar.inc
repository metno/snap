c-----------------------------------------------------------------------
c snappar.inc
c
c..include file  -  common for particles
c
c
c..ndefcomp:  no. of components defined (aerosol,gas,noble.gas,...)
c..nplume:    no. of released plumes
c..npart:     total no. of particles (in all released plumes)
c..nparnum:   counter for unique particle identifier 
c..iplume:    pointers to first and last particle in each plume
c.            (0,-1 means no particles left in the grid domain)
c..kdrydep:   for each component: 0=dry deposition off  1=dry dep. on
c..kwetdep:   for each component: 0=wet deposition off  1=wet dep. on
c..kdecay:    for each component: 0=radioactive decay off  1=decay on
c..kgravity:  gravity type: 0=off
c                           1=fixed
c                           2=computed
c..drydephgt: max height above ground for dry deposition
c..drydeprat: dry deposition rate
c..wetdeprat: wet deposition rate
c..halftime:  radioactive half lifetime (hours)
c..decayrate: radioactive decay (rate)
c..gravityms:   fixed gravity in unit m/s
c..radiusmym:   radius in unit micrometer (for gravity computation)
c..densitygcm3: density in unit g/cm3     (for gravity computation)
c..vgtable:     table of gravity in m/s
c		(temperature as first index, pressure second)
c..idcomp:    an identifier used in the field identification
c	      (stored as level_1 for single level fields,
c	       model level fields adds this number to a 'basic'
c	       parameter no., 0 is used for the total if more than
c	       one component present)
c..iruncomp:  run comp. (1,...ncomp) or 0 if not used
c..totalbq:   total release in unit Bq, accumulated during run
c..numtotal:  total no. of particles released, accumulated during run
c..compname:  a component name (not much used, really)
c
      integer      ndefcomp,nplume,npart,nparnum
      integer      iplume(2,mplume)
      integer      kdrydep(mdefcomp),kwetdep(mdefcomp),kdecay(mdefcomp)
      integer      kgravity(mdefcomp)
      real         drydephgt(mdefcomp)
     -            ,drydeprat(mdefcomp),wetdeprat(mdefcomp)
     -            ,halftime(mdefcomp),decayrate(mdefcomp)
     -            ,gravityms(mdefcomp)
     -            ,radiusmym(mdefcomp),densitygcm3(mdefcomp)
     -            ,vgtable(numtempvg,numpresvg,mdefcomp)
     -		  ,tbasevg,tincrvg,pbasevg,pincrvg
     -            ,totalbq(mdefcomp)
      integer      numtotal(mdefcomp)
      integer      idcomp(mdefcomp),iruncomp(mdefcomp)
      character*32 compname(mdefcomp),compnamemc(mdefcomp)
c
      common/plume/ndefcomp,nplume,npart,nparnum
     -            ,iplume
     -		  ,kdrydep,kwetdep,kdecay,kgravity
     -		  ,drydephgt,drydeprat,wetdeprat
     -            ,halftime,decayrate
     -		  ,idcomp,iruncomp
     -            ,gravityms,radiusmym,densitygcm3
     -            ,vgtable,tbasevg,tincrvg,pbasevg,pincrvg
     -            ,totalbq,numtotal
     -		  ,compname,compnamemc
c
c
c..itprof:  time profile type:
c             1= constant
c             2= bomb (only one initial release)
c             3= linear (between specified timesteps)
c             4= steps  (constant between the specified timesteps)
c..ntprof:  no. of timesteps in the release profiles
c..ncomp:   no. of components used in the run
c..mprel:   max no. of particles released in each plume
c              (scaled according to max mass released
c               and divided between components according to mass)
c..component(n):    component name
c..idefcomp(n):     component no. as defined in input file sequence
c..irelhour(i):     specified release hours in time profile
c..relradius(i):    release radius in unit meter
c..relupper(i):     release upper height in unit meter
c..rellower(i):     release lower height in unit meter
c..relstemradius(i): release radius in unit meter for a mushroom stem
c..relbqsec(i,n):   radioactive release in unit Bq/sec
c..      i=1,ntprof
c..      n=1,ncomp
c
      integer       itprof,ntprof,ncomp,mprel
      integer       idefcomp(mcomp)
      integer       irelhour(mtprof)
      real          relradius(mtprof),relupper(mtprof),rellower(mtprof)
      real          relstemradius(mtprof)
      real          relbqsec(mtprof,mcomp)
      character*32  component(mcomp)
c
      common/relcom/itprof,ntprof,ncomp,mprel
     -             ,idefcomp
     -             ,irelhour
     -             ,relradius,relupper,rellower
     -             ,relstemradius
     -             ,relbqsec
     -             ,component
c
c
      integer npdata
      parameter (npdata=10)
c
c
c..pdata:   pdata(1,n) - x position in grid
c           pdata(2,n) - y position in grid
c           pdata(3,n) - sigma/eta position (vertical)
c           pdata(4,n) - sigma/eta at top of boundary layer
c           pdata(5,n) - height of boundary layer
c           pdata(6,n) - map ratio in x direction
c           pdata(7,n) - map ratio in y direction
c           pdata(8,n) - precipitation intensity (mm/hour)
c           pdata(9,n) - radioactive content (Bq)
c           pdata(10,n) - gravity in m/s (fixed or computed)
c..pwork:   work array, currently also used to transfer wind components
c                       (u,v) for 'forwrd' to 'rwalk'
c..icomp:   component no. in particle
c..iparnum: unique identifier for each particle
c..ipwork:  work array
c
      real    pdata(npdata,mpart),pwork(mpart,2)
      integer icomp(mpart),iparnum(mpart),ipwork(mpart)
c
      common/part/pdata,pwork
     -           ,icomp,iparnum,ipwork
c
c-----------------------------------------------------------------------
