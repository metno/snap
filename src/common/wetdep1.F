      subroutine wetdep1
c
c  Purpose:  Compute wet deposition for each particle and each component
c            and store depositions in nearest gridpoint in a field
c  Method:   J.Saltbones 1994
c
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
      include 'snaptab.inc'
c
      integer m,n,itab,i,j,mm
      real    precint,probab,prand,dep
c
c..for random number functions
      integer irand,iseed
c
#if defined(linux)
      real rand
#else
      double precision rand
#endif
c
c
c..for random number generation
      iseed=irand()
      call srand(iseed)
c
      do n=1,npart
       m= icomp(n)
       if(kwetdep(m).eq.1 .and. pdata(8,n).gt.0.0) then
c..find particles with wet deposition and
c..reset precipitation to zero if not wet deposition
          precint=pdata(8,n)
          itab=nint(precint*premult)
          itab=min(itab,mpretab)
          probab=pretab(itab)
c..the rand function returns random real numbers between 0.0 and 1.0
          prand=rand()
          if(prand.gt.probab) then
           pdata(8,n)=0.0
         else
            dep=wetdeprat(m)*pdata(9,n)
            pdata(9,n)=pdata(9,n)-dep
           i=nint(pdata(1,n))
           j=nint(pdata(2,n))
           mm=iruncomp(m)
            depwet(i,j,mm)=depwet(i,j,mm)+dble(dep)
         end if
       end if
      end do
c
      return
      end
