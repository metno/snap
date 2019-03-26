! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

module filesortML
  implicit none
  private

  public filesort

  contains

subroutine filesort(iunit,ierror)

!       check and sort felt file contents

!       unsorted list of files and timesteps with data:
!         iavail(n)%aYear: year    )
!         iavail(n)%aMonth: month   ) Time of analysis
!         iavail(n)%aDay: day     ) (not valid time of forecast)
!         iavail(n)%aHour: hour    )
!         iavail(n)%fcHour: forecast hour
!         iavail(n)%fileNo: file no. (in filename array)
!         iavail(n)%fileType: 1=model level  2=surface  3=both
!         iavail(n)%oHour: offset in hours from first (sorted) timestep
!         iavail(n)%nAvail: pointer to next forward  (time) data
!         iavail(n)%pAvail: pointer to next backward (time) data
!                   n=1,navail

!       pointers to lists in iavail:
!         kavail(1): pointer to first forward  sorted timestep
!         kavail(2): pointer to first backward sorted timestep

  USE fileInfoML
  USE snapfldML
  USE snapfilML
  USE snapdebugML
  USE snapgrdML

  implicit none

        

!..input/output
  integer ::   iunit,ierror

!..local
  integer ::   maxinh
  parameter (maxinh=64*8)

  integer*2 :: inh(16,maxinh),idfile(32)
  integer ::   ifound(maxinh),itime(5),itimev(5),itimeref(5)
  integer*2 :: i2dum

  integer :: lenstr
  integer :: nf,i,mhdiff,minhfc,maxhfc,nbegin,iend,nfound,ioerr
  integer :: n,ihdiff,ierr1,ierr2,iaforw,iaback,laforw,laback
  integer :: k,ihrmin,iprmin,nmin,ihrmax,iprmax,nmax,ktest,modsurf,j

  if(idebug == 1) then
    write(9,*)
    write(9,*) 'FILESORT------------------------------------------'
    write(9,*) 'iprod,igrid:   ',iprod,igrid
    write(9,*) 'FILES.    nfilef= ',nfilef
    do nf=1,nfilef
      write(9,*) nf,'  ',filef(nf)(1:lenstr(filef(nf),1))
      write(9,*) '      min,max fc:',(limfcf(i,nf),i=1,2)
    end do
  end if

  navailt1=0
  navailt2=0

  navail=0
  mhdiff=0

  do nf = 1,nfilef
  
  !..open the (standard/archive/cyclic_archive) felt file
  
  ! c     call mrfelt  (1,filef(nf),iunit,i2dum,0,0,0.,0.,
  ! c  +		      32,idfile,ierror)
    call mrfturbo(1,filef(nf),iunit,i2dum,0,0,0.,0., &
    & 		      32,idfile,ierror)
  
    if(ierror == 0) then
    
    !..min,max forecast length in hours for model level data
      minhfc=limfcf(1,nf)
      maxhfc=limfcf(2,nf)
    
      ktest=klevel(nk-kadd)
    
      nbegin=navail
      iend=0
    
      do while (iend == 0)
      
        do i=1,16
          inh(i,1)=-32767
        end do
      !..search for u component of wind in upper input model level
        inh( 1,1)=iprod
        inh( 2,1)=igrid
      
        call qfelt(iunit,1,1,maxinh,inh(1,1),ifound(1),nfound, &
        iend,ierror,ioerr)
      
        if(nfound > 0 .AND. ierror == 0) then
        
          do n = 1,nfound
          
            if(inh(10,n) >= minhfc .AND. inh(10,n) <= maxhfc) then
              modsurf=0
              if(inh(11,n) == ivcoor .AND. inh(12,n) == 2 &
               .AND. inh(13,n) == ktest) then
                modsurf=1
              elseif(inh(11,n) == 2 .AND. inh(12,n) == 33 &
                 .AND. inh(13,n) == 1000) then
                modsurf=2
              end if
              if(modsurf > 0) then
              !..year,month,day,hour,forecast_hour
                itime(1)=inh(3,n)
                itime(2)=inh(4,n)/100
                itime(3)=inh(4,n)-(inh(4,n)/100)*100
                itime(4)=inh(5,n)/100
                itime(5)=inh(10,n)
                do i=1,5
                  itimev(i)=itime(i)
                end do
                call vtime(itimev(1),ierror)
                if(ierror == 0) then
                  k=0
                  j=0
                  do while (k == 0 .AND. j < navail)
                    j=j+1
                    if(iavail(j)%aYear == itime(1) .AND. &
                    iavail(j)%aMonth == itime(2) .AND. &
                    iavail(j)%aDay == itime(3) .AND. &
                    iavail(j)%aHour == itime(4) .AND. &
                    iavail(j)%fcHour == itime(5) .AND. &
                    iavail(j)%fileNo == nf)  k=j
                  end do
                  if(k == 0) then
                    navail=navail+1
                    if(navail <= mavail) then
                      if(navail == 1) then
                        do i=1,5
                          itimeref(i)=itimev(i)
                        end do
                        ihdiff=0
                      else
                        call hrdiff(0,0,itimeref(1),itimev(1), &
                        ihdiff,ierr1,ierr2)
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
                    if(iavail(k)%fileType == 1 .AND. modsurf == 2) &
                    iavail(k)%fileType=3
                    if(iavail(k)%fileType == 2 .AND. modsurf == 1) &
                    iavail(k)%fileType=3
                  end if
                end if
              end if
            end if
          
          !.............end do n = 1,nfound
          end do
        
        end if
      
      !.........end do while (iend.eq.0)
      end do
    
    !..qfelt cleanup
      call qfelt(0,0,1,maxinh,inh(1,1),ifound(1),nfound, &
      iend,ierror,ioerr)
    
    !..close the (standard/archive/cyclic_archive) felt file
    
    ! c       call mrfelt  (3,filef(nf),iunit,i2dum,0,0,0.,0.,
    ! c  +			1,idfile,ierror)
      call mrfturbo(3,filef(nf),iunit,i2dum,0,0,0.,0., &
      & 			1,idfile,ierror)
    
    end if
  
  !.....end do nf = 1,nfilef
  end do

  do n=1,navail
    iavail(n)%oHour=iavail(n)%oHour+mhdiff
  end do

  if(navail > mavail) then
    write(9,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
    write(9,*) '          no.,max(MAVAIL): ',navail,mavail
    write(9,*) '    CONTINUING WITH RECORDED DATA'
    write(6,*) 'WARNING : TOO MANY AVAILABLE TIME STEPS'
    write(6,*) '          max (MAVAIL): ',mavail
    write(6,*) '    CONTINUING WITH RECORDED DATA'
    navail=mavail
  end if

!#############################################################################
!      if(idebug.eq.1) then
!        write(9,*) 'ALL STEPS UNSORTED AVAILABLE    navail=',navail
!        do n=1,navail
!          write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)')
!     +				n,(iavail(i,n),i=1,10)
!        end do
!      end if
!#############################################################################
  n=0
  do while (n < navail)
    n=n+1
    if(iavail(n)%fileType == 2) then
      k=0
      j=0
      do while (k == 0 .AND. j < navail)
        j=j+1
        if(iavail(j)%aYear == iavail(n)%aYear .AND. &
        iavail(j)%aMonth == iavail(n)%aMonth .AND. &
        iavail(j)%aDay == iavail(n)%aDay .AND. &
        iavail(j)%aHour == iavail(n)%aHour .AND. &
        iavail(j)%fcHour == iavail(n)%fcHour .AND. &
        iavail(j)%fileType /= 2) k=j
      end do
      if(k == 0) then
        navail=navail-1
        do j=n,navail
          iavail(j)=iavail(j+1)
        end do
        n=n-1
      end if
    end if
  end do

  if(idebug == 1) then
    write(9,*) 'UNSORTED AVAILABLE    navail=',navail
    do n=1,navail
      write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)') &
      n,(iavail(n))
    end do
  end if

  if(navail < 2) then
    write(9,*) 'NOT ENOUGH AVAILABLE DATA'
    write(9,*) '  Total no. of recorded timesteps: ',navail
    write(6,*) 'NOT ENOUGH AVAILABLE DATA'
    write(6,*) '  Total no. of recorded timesteps: ',navail
    ierror=1
    return
  end if

!..make the forward and backward (time) pointers,
!..always shortest forecast length first if equal time difference,
!..which means that one of the pointer lists is not the other reversed
!..(if everything equal, the file input sequence is kept)

  iaforw=0
  iaback=0
  laforw=0
  laback=0

  do k=1,navail
  
    ihrmin=+999999999
    iprmin=+999999999
    nmin=0
    ihrmax=-999999999
    iprmax=+999999999
    nmax=0
  
    do n=1,navail
    !..forward
      if(iavail(n)%nAvail == 0 .AND. (iavail(n)%oHour < ihrmin &
       .OR. &
      (iavail(n)%oHour == ihrmin &
       .AND. iavail(n)%fcHour < iprmin))) then
        ihrmin=iavail(n)%oHour
        iprmin=iavail(n)%fcHour
        nmin  =n
      end if
    !..backward (note that shortest forecast length is recorded first)
      if(iavail(n)%pAvail == 0 .AND. (iavail(n)%oHour > ihrmax &
       .OR. &
      (iavail(n)%oHour == ihrmax &
       .AND. iavail(n)%fcHour < iprmax))) then
        ihrmax=iavail(n)%oHour
        iprmax=iavail(n)%fcHour
        nmax  =n
      end if
    
    end do
  
  !..start of forward list
    if(iaforw == 0) iaforw=nmin
  !..forward pointer from previous
    if(laforw > 0) iavail(laforw)%nAvail=nmin
    laforw=nmin
    iavail(laforw)%nAvail=-1
  
  !..start of backward list
    if(iaback == 0) iaback=nmax
  !..backward pointer from previous
    if(laback > 0) iavail(laback)%pAvail=nmax
    laback=nmax
    iavail(laback)%pAvail=-1
  
  !.....end do k=1,navail
  end do

  iavail(laforw)%nAvail=0
  iavail(laback)%pAvail=0

  kavail(1)=iaforw
  kavail(2)=iaback

!..set time difference in hours equal zero for first timestep
  ihdiff=-iavail(iaforw)%oHour
  do n = 1,navail
    iavail(n)%oHour=iavail(n)%oHour+ihdiff
  end do

  if(idebug == 1) then
    write(9,*) 'FORWARD SORTED AVAILABLE    navail=',navail
    n=iaforw
    do while (n > 0)
      write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)') &
      n,(iavail(n))
      n=iavail(n)%nAvail
    end do
    write(9,*) 'BACKWARD SORTED AVAILABLE    navail=',navail
    n=iaback
    do while (n > 0)
      write(9,fmt='(i4,'':'',7(1x,i4),1x,i6,2i5)') &
      n,(iavail(n))
      n=iavail(n)%pAvail
    end do
    write(9,*) '--------------------------------------------------'
    write(9,*)
  end if

!..time range
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
!..get valid time (with forecast=0)
  call vtime(itimer(1,1),ierror)
  call vtime(itimer(1,2),ierror)

  ierror=0

  return
end subroutine filesort
end module filesortML
