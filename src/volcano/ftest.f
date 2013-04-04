      subroutine ftest(name,k1,k2,nx,ny,nk,field,iundef)
c
c  Purpose: Test field, print min,mean,max values.
c
c
      implicit none
c
      integer       k1,k2,nx,ny,nk,iundef
      real          field(nx,ny,nk)
      character*(*) name
c
      integer kstep,i,j,k,ndef
      real    undef,ud,f,fmin,fmax,fmean
c
      double precision fsum
c
      if(k1.lt.1 .or. k1.gt.nk) k1=1
      if(k2.lt.1 .or. k2.gt.nk) k2=nk
      kstep=+1
      if(k1.gt.k2) kstep=-1
c
      undef=+1.e+35
      ud=undef*0.9
c
      do k=k1,k2,kstep
        fmin=+undef
        fmax=-undef
        fsum=0.
        if(iundef.eq.0) then
          do j=1,ny
            do i=1,nx
              fmin=min(fmin,field(i,j,k))
              fmax=max(fmax,field(i,j,k))
              fsum=fsum+field(i,j,k)
            end do
          end do
          ndef=nx*ny
        else
          ndef=0
          do j=1,ny
            do i=1,nx
              if(field(i,j,k).lt.ud) then
                fmin=min(fmin,field(i,j,k))
                fmax=max(fmax,field(i,j,k))
                fsum=fsum+field(i,j,k)
                ndef=ndef+1
              end if
            end do
          end do
        end if
        if(ndef.gt.0) then
          fmean=fsum/dble(ndef)
        else
          fmin=0.
          fmax=0.
          fmean=0.
        end if
        if(k1.ne.k2) then
          write(9,fmt='(5x,a8,1x,i3,3(1x,e13.5))')
     -                     name,k,fmin,fmean,fmax
        else
          write(9,fmt='(5x,a8,1x,3x,3(1x,e13.5))')
     -                     name,  fmin,fmean,fmax
        end if
      end do
c
      return
      end
