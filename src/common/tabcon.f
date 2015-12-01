      subroutine tabcon
c
c  Purpose:  Define fixed tables and constans
c            (independant of input data)
c
      implicit none
c
      include 'snapdim.inc'
      include 'snaptab.inc'
c
      integer i,j,n
      real    rcp,p,prestep,precint,probab
c
      rcp=r/cp
c
c..Exner function, pitab(0:130) for p=0,10,20,...1300 hPa
c
      pmult=0.1
      do i=0,130
        p=i*10.
        pitab(i)=1004.*(p/1000.)**rcp
      end do
c
c..precipitation probability for wet depositions
c
      if(nprepro.gt.0) then
        prestep=prepro(1,nprepro)/float(mpretab)
        premult=1./prestep
        i=2
        pretab(0)=0.0
        do n=1,mpretab
          precint=prestep*n
          do while (precint.gt.prepro(1,i) .and. i.lt.nprepro)
            i=i+1
          end do
          probab=( prepro(2,i-1)*(prepro(1,i)-precint)
     *            +prepro(2,i)  *(precint-prepro(1,i-1)))
     *           /(prepro(1,i)-prepro(1,i-1))
          pretab(n)=probab
        end do
        pretab(mpretab)=prepro(2,nprepro)
c######################################################################
        do n=1,nprepro
          write(9,*) '...n,prepro: ',n,prepro(1,n),prepro(2,n)
        end do
        write(9,*) '...premult: ',premult
        do n=0,mpretab,20
          precint=prestep*n
          write(9,*) '...n,pretab: ',n,precint,pretab(n)
        end do
c######################################################################
      else
        premult=0.
        do n=1,mpretab
          pretab(n)=0.
        end do
      end if
c
      return
      end
