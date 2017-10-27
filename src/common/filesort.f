! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
! 
! This file is part of SNAP. SNAP is free software: you can 
! redistribute it and/or modify it under the terms of the 
! GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!
      subroutine filesort(iunit,ierror)
c
c       check and sort felt file contents
c
c       unsorted list of files and timesteps with data:
c         iavail(n)%aYear: year    )
c         iavail(n)%aMonth: month   ) Time of analysis
c         iavail(n)%aDay: day     ) (not valid time of forecast)
c         iavail(n)%aHour: hour    )
c         iavail(n)%fcHour: forecast hour
c         iavail(n)%fileNo: file no. (in filename array)
c         iavail(n)%fileType: 1=model level  2=surface  3=both
c         iavail(n)%oHour: offset in hours from first (sorted) timestep
c         iavail(n)%nAvail: pointer to next forward  (time) data
c         iavail(n)%pAvail: pointer to next backward (time) data
c                   n=1,navail
c
c       pointers to lists in iavail:
c         kavail(1): pointer to first forward  sorted timestep
c         kavail(2): pointer to first backward sorted timestep
c
      use fileInfoML
      implicit none
c
      include 'snapdim.inc'
      include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snapdebug.inc'
c
c..input/output
      integer   iunit,ierror
c
c..local
      integer   maxinh
      parameter (maxinh=64*8)
c
      integer*2 inh(16,maxinh),idfile(32)
      integer   ifound(maxinh),itime(5),itimev(5),itimeref(5)
      integer*2 i2dum
c
      integer lenstr
      integer nf,i,mhdiff,minhfc,maxhfc,nbegin,iend,nfound,ioerr
      integer n,ihdiff,ierr1,ierr2,iaforw,iaback,laforw,laback
      integer k,ihrmin,iprmin,nmin,ihrmax,iprmax,nmax,ktest,modsurf,j
c
      if(idebug.eq.1) then
        write(9,*)
        write(9,*) 'FILESORT------------------------------------------'
        write(9,*) 'iprod,igrid:   ',iprod,igrid
        write(9,*) 'FILES.    nfilef= ',nfilef
        do nf=1,nfilef
          write(9,*) nf,'  ',filef(nf)(1:lenstr(filef(nf),1))
          write(9,*) '      min,max fc:',(limfcf(i,nf),i=1,2)
        end do
      end if
c
      navailt1=0
      navailt2=0
c
      navail=0
      mhdiff=0
c
      do nf = 1,nfilef
c
c..open the (standard/archive/cyclic_archive) felt file
c
ccc     call mrfelt  (1,filef(nf),iunit,i2dum,0,0,0.,0.,
ccc  +		      32,idfile,ierror)
        call mrfturbo(1,filef(nf),iunit,i2dum,0,0,0.,0.,
     +		      32,idfile,ierror)
c
        if(ierror.eq.0) then
c
c..min,max forecast length in hours for model level data
          minhfc=limfcf(1,nf)
          maxhfc=limfcf(2,nf)
c
         ktest=klevel(nk-kadd)
c
          nbegin=navail
          iend=0
c
          do while (iend.eq.0)
c
            do i=1,16
              inh(i,1)=-32767
            end do
c..search for u component of wind in upper input model level
            inh( 1,1)=iprod
            inh( 2,1)=igrid
c
            call qfelt(iunit,1,1,maxinh,inh(1,1),ifound(1),nfound,
     +                 iend,ierror,ioerr)
c
            if(nfound.gt.0 .and. ierror.eq.0) then
c
              do n = 1,nfound
c
           if(inh(10,n).ge.minhfc .and. inh(10,n).le.maxhfc) then
             modsurf=0
             if(inh(11,n).eq.ivcoor .and. inh(12,n).eq.2
     +					 .and. inh(13,n).eq.ktest) then
               modsurf=1
             elseif(inh(11,n).eq.2 .and. inh(12,n).eq.33
     +					.and. inh(13,n).eq.1000) then
               modsurf=2
             end if
             if(modsurf.gt.0) then
c..year,month,day,hour,forecast_hour
                    itime(1)=inh(3,n)
                    itime(2)=inh(4,n)/100
                    itime(3)=inh(4,n)-(inh(4,n)/100)*100
                    itime(4)=inh(5,n)/100
                    itime(5)=inh(10,n)
                   do i=1,5
                 itimev(i)=itime(i)
               end do
                    call vtime(itimev(1),ierror)
                   if(ierror.eq.0) then
                 k=0
                 j=0
                 do while (k.eq.0 .and. j.lt.navail)
                   j=j+1
               if(iavail(j)%aYear.eq.itime(1) .and.
     +			   iavail(j)%aMonth.eq.itime(2) .and.
     +			   iavail(j)%aDay.eq.itime(3) .and.
     +			   iavail(j)%aHour.eq.itime(4) .and.
     +			   iavail(j)%fcHour.eq.itime(5) .and.
     +			   iavail(j)%fileNo.eq.nf)  k=j
                   end do
                 if(k.eq.0) then
                        navail=navail+1
                        if(navail.le.mavail) then
                     if(navail.eq.1) then
                           do i=1,5
                         itimeref(i)=itimev(i)
                       end do
                       ihdiff=0
                     else
                       call hrdiff(0,0,itimeref(1),itimev(1),
     +				        ihdiff,ierr1,ierr2)
                       mhdiff=min(mhdiff,ihdiff)
                     end if
                     iavail(navail)%aYear=itime(1)
                     iavail(navail)%aMonth=itime(2)
                     iavail(navail)%aDay=itime(3)
                     iavail(navail)%aHour=itime(4)
                     iavail(navail)%fcHour=itime(5)
                          iavail(navail)%fileNo=nf
                          iavail(navail)%fileType=modsurf
                          iavail(navail)%oHour=ihdiff
                          iavail(navail)%nAvail=0
                          iavail(navail)%pAvail=0
                   end if
                 else
                   if(iavail(k)%fileType.eq.1 .and. modsurf.eq.2)
     &                iavail(k)%fileType=3
                   if(iavail(k)%fileType.eq.2 .and. modsurf.eq.1)
     &                iavail(k)%fileType=3
                 end if
                    end if
             end if
           end if
c
c.............end do n = 1,nfound
              end do
c
            end if
c
c.........end do while (iend.eq.0)
          end do
c
c..qfelt cleanup
          call qfelt(0,0,1,maxinh,inh(1,1),ifound(1),nfound,
     +               iend,ierror,ioerr)
c
c..close the (standard/archive/cyclic_archive) felt file
c
ccc       call mrfelt  (3,filef(nf),iunit,i2dum,0,0,0.,0.,
ccc  +			1,idfile,ierror)
          call mrfturbo(3,filef(nf),iunit,i2dum,0,0,0.,0.,
     +			1,idfile,ierror)
c
        end if
c
c.....end do nf = 1,nfilef
      end do
c
      do n=1,navail
       iavail(n)%oHour=iavail(n)%oHour+mhdiff
      end do
c
      if(navail.gt.mavail) then
        write(9,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
        write(9,*) '          no.,max(MAVAIL): ',navail,mavail
        write(9,*) '    CONTINUING WITH RECORDED DATA'
        write(6,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
        write(6,*) '          max (MAVAIL): ',mavail
        write(6,*) '    CONTINUING WITH RECORDED DATA'
        navail=mavail
      end if
c
c#############################################################################
c      if(idebug.eq.1) then
c        write(9,*) 'ALL STEPS UNSORTED AVAILABLE    navail=',navail
c        do n=1,navail
c          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
c     +				n,(iavail(i,n),i=1,10)
c        end do
c      end if
c#############################################################################
      n=0
      do while (n.lt.navail)
        n=n+1
       if(iavail(n)%fileType.eq.2) then
         k=0
         j=0
         do while (k.eq.0 .and. j.lt.navail)
           j=j+1
           if(iavail(j)%aYear.eq.iavail(n)%aYear .and.
     +	       iavail(j)%aMonth.eq.iavail(n)%aMonth .and.
     +	       iavail(j)%aDay.eq.iavail(n)%aDay .and.
     +	       iavail(j)%aHour.eq.iavail(n)%aHour .and.
     +	       iavail(j)%fcHour.eq.iavail(n)%fcHour .and.
     +	       iavail(j)%fileType.ne.2) k=j
         end do
         if(k.eq.0) then
           navail=navail-1
           do j=n,navail
             iavail(j)=iavail(j+1)
           end do
           n=n-1
         end if
       end if
      end do
c
      if(idebug.eq.1) then
        write(9,*) 'UNSORTED AVAILABLE    navail=',navail
        do n=1,navail
          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
     +				n,(iavail(n))
        end do
      end if
c
      if(navail.lt.2) then
        write(9,*) 'NOT ENOUGH AVAILABLE DATA'
        write(9,*) '  Total no. of recorded timesteps: ',navail
        write(6,*) 'NOT ENOUGH AVAILABLE DATA'
        write(6,*) '  Total no. of recorded timesteps: ',navail
        ierror=1
        return
      end if
c
c..make the forward and backward (time) pointers,
c..always shortest forecast length first if equal time difference,
c..which means that one of the pointer lists is not the other reversed
c..(if everything equal, the file input sequence is kept)
c
      iaforw=0
      iaback=0
      laforw=0
      laback=0
c
      do k=1,navail
c
        ihrmin=+999999999
        iprmin=+999999999
        nmin=0
        ihrmax=-999999999
        iprmax=+999999999
        nmax=0
c
        do n=1,navail
c..forward
          if(iavail(n)%nAvail.eq.0 .and. (iavail(n)%oHour.lt.ihrmin
     +      .or.
     +       (iavail(n)%oHour.eq.ihrmin
     +         .and.iavail(n)%fcHour.lt.iprmin))) then
            ihrmin=iavail(n)%oHour
            iprmin=iavail(n)%fcHour
            nmin  =n
          end if
c..backward (note that shortest forecast length is recorded first)
          if(iavail(n)%pAvail.eq.0 .and. (iavail(n)%oHour.gt.ihrmax
     +       .or.
     +       (iavail(n)%oHour.eq.ihrmax
     +          .and. iavail(n)%fcHour.lt.iprmax))) then
            ihrmax=iavail(n)%oHour
            iprmax=iavail(n)%fcHour
            nmax  =n
          end if
c
        end do
c
c..start of forward list
        if(iaforw.eq.0) iaforw=nmin
c..forward pointer from previous
        if(laforw.gt.0) iavail(laforw)%nAvail=nmin
        laforw=nmin
       iavail(laforw)%nAvail=-1
c
c..start of backward list
        if(iaback.eq.0) iaback=nmax
c..backward pointer from previous
        if(laback.gt.0) iavail(laback)%pAvail=nmax
        laback=nmax
       iavail(laback)%pAvail=-1
c
c.....end do k=1,navail
      end do
c
      iavail(laforw)%nAvail=0
      iavail(laback)%pAvail=0
c
      kavail(1)=iaforw
      kavail(2)=iaback
c
c..set time difference in hours equal zero for first timestep
      ihdiff=-iavail(iaforw)%oHour
      do n = 1,navail
        iavail(n)%oHour=iavail(n)%oHour+ihdiff
      end do
c
      if(idebug.eq.1) then
        write(9,*) 'FORWARD SORTED AVAILABLE    navail=',navail
        n=iaforw
        do while (n.gt.0)
          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
     +				n,(iavail(n))
          n=iavail(n)%nAvail
        end do
        write(9,*) 'BACKWARD SORTED AVAILABLE    navail=',navail
        n=iaback
        do while (n.gt.0)
          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
     +				n,(iavail(n))
          n=iavail(n)%pAvail
        end do
        write(9,*) '--------------------------------------------------'
        write(9,*)
      end if
c
c..time range
      itimer(1,1)=iavail(iaforw)%aYear
      itimer(2,1)=iavail(iaforw)%aMonth
      itimer(3,1)=iavail(iaforw)%aDay
      itimer(4,1)=iavail(iaforw)%aHour
      itimer(5,1)=iavail(iaforw)%fcHour
      itimer(1,2)=iavail(iaback)%aYear
      itimer(2,2)=iavail(iaback)%aMonth
      itimer(3,2)=iavail(iaback)%aDay
      itimer(4,2)=iavail(iaback)%aHour
      itimer(5,2)=iavail(iaback)%fcHour
c..get valid time (with forecast=0)
      call vtime(itimer(1,1),ierror)
      call vtime(itimer(1,2),ierror)
c
      ierror=0
c
      return
      end
