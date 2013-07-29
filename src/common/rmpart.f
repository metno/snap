      subroutine rmpart(rmlimit)
c
c  Purpose: Remove particles which have left the grid domain
c           or has lost (almost) all mass, in the last case the
c           remaining mass is transferred to to the other particles
c           in the same plume (or to the next plume if none left).
c
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snappar.inc'
c
      real    rmlimit
c
      integer nkeep,idep,m,n,npl,i,i1,i2,keep,iredist,j
      real    xmin,xmax,ymin,ymax,vmin,vmax
c
      integer npkeep(mdefcomp)
      real    pbqmin(mdefcomp),pbqtotal(mdefcomp),pbqlost(mdefcomp)
      real    pbqdist(mdefcomp)
c
c
      xmin=1.
      ymin=1.
      xmax=float(nx)
      ymax=float(ny)
      vmin=vlevel(nk)
      vmax=vlevel( 1)
c
c..rmlimit is now input, used to be 0.02 (2%)
      idep=0
      do n=1,ncomp
       m=idefcomp(n)
        pbqmin(m)=0.
        if(kdrydep(m).eq.1 .or. kwetdep(m).eq.1
     +			   .or. kdecay(m).eq.1) then
          if(numtotal(m).gt.0)
     +	    pbqmin(m)=(totalbq(m)/numtotal(m))*rmlimit
          idep=1
        end if
        pbqlost(m)=0.
      end do
c
      n=0
c
      do npl=1,nplume
c
        i1=iplume(1,npl)
        i2=iplume(2,npl)
c
c..redistribution of lost mass (within one plume)
        if(idep.eq.1 .and. i1.gt.0) then
         do m=1,ncomp
            pbqtotal(m)=0.
           npkeep(m)=0
         end do
         nkeep=0
          do i=i1,i2
           m=icomp(i)
           if(pdata(9,i).gt.pbqmin(m)) then
             pbqtotal(m)=pbqtotal(m)+pdata(9,i)
             npkeep(m)=npkeep(m)+1
           else
             pbqlost(m)=pbqlost(m)+pdata(9,i)
             pdata(9,i)=0.
              pdata(1,i)=0.
              pdata(2,i)=0.
           end if
          end do
         iredist=0
         do m=1,ncomp
           pbqdist(m)=0.
            if(pbqlost(m).gt.0. .and. npkeep(m).gt.0) then
             pbqdist(m)=pbqlost(m)/float(npkeep(m))
             pbqlost(m)=0.
             iredist=1
           end if
         end do
          if(iredist.eq.1) then
            do i=i1,i2
             if(pdata(9,i).gt.0.0) then
               m=icomp(i)
               pdata(9,i)= pdata(9,i)+pbqdist(m)
             end if
           end do
          end if
        end if
c
        iplume(1,npl)=n+1
c
        do i=i1,i2
          if(pdata(1,i).gt.xmin .and. pdata(1,i).lt.xmax .and.
     +       pdata(2,i).gt.ymin .and. pdata(2,i).lt.ymax) then
            pdata(3,i)=min(pdata(3,i),vmax)
            pdata(3,i)=max(pdata(3,i),vmin)
            n=n+1
            if(n.ne.i) then
             do j=1,npdata
                pdata(j,n)=pdata(j,i)
             end do
             icomp(n)=  icomp(i)
             iparnum(n)=iparnum(i)
            end if
          end if
        end do
c
        iplume(2,npl)=n
        if(iplume(1,npl).gt.iplume(2,npl)) then
          iplume(1,npl)=0
          iplume(2,npl)=-1
        end if
c
      end do
c
      npart=n
c
c..note: if pmlost>0 we lost mass inside the grid area
c..      (no later plumes to take the mass).
c
      return
      end
