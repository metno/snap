c-----------------------------------------------------------------------
c snappos.inc
c
c..include file  -  common for release positions and profiles
c
c
c..irelpos:     the chosen release position
c..nrelpos:     no. of release positions (in list)
c..relpos(1,n): geographic latitude  for release positions
c..relpos(2,n): geographic longitude for release positions
c..relpos(3,n): grid x coordinat for release positions
c..relpos(4,n): grid y coordinat for release positions
c..relnam(n)  : name of release positions
c..srelnam    : name of selected release position
c
      integer      irelpos,nrelpos
      real         relpos(4,mrelpos)
      character*40 relnam(mrelpos),srelnam
c
      common/poscom/irelpos,nrelpos
     -             ,relpos
     -             ,relnam,srelnam
c
c-----------------------------------------------------------------------
