c-----------------------------------------------------------------------
c snaptab.inc
c
c..include file  - fixed tables and constants
c                  (independant of input data)
c
c
c..pmult:  multiply pressure by this value to get index in pitab
c..pitab:  Exner function, pitab(0:130) for p=0,10,20,...1300 hPa
c..g    :  ...
c..r    :  ...
c..cp   :  ...
c
      real    pmult,pitab(0:130),g,r,cp
c
      common/table1/pmult,pitab
      parameter (g=9.81, r=287., cp=1004.)
c
c
c..premult: multiply precipitation intensity (mm/hour) by this value
c	    to get index in pretab
c..pretab:  precipitation parobability table (for wet depositions)
c..nprepro:     no. of steps in input precipitation probability table
c..prepro(1,n): precipitation intensity (mm/hour)
c..prepro(2,n): probability for precipitation (0. - 1.)
c
      real    premult,pretab(0:mpretab)
      integer nprepro
      real    prepro(2,mprepro+1)
c
      common/table2/premult,pretab
     -             ,nprepro,prepro
c
c-----------------------------------------------------------------------
