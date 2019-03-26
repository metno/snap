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

module c2fgetvarML
  implicit none
  private

  contains

subroutine c2fgetvar(nvar,var,nilarg,ilarg,iprint,ierror)

!  PURPOSE: Search for Environment variables and Command line arguments
!           in text strings and replace them with actual values.
!           The character ~ is interpreted as $HOME.

!  INPUT:   nvar          - integer       - no. of character strings
!           var(nvar)     - character*(*) - the character strings
!           nilarg        - integer       - no. of illegal command line
!                                           arguments
!           ilarg(nilarg) - integer       - the illegal command line
!                                           arguments (none illegal:
!                                           nilarg=1 and ilarg(1)=-1)
!           iprint        - integer       - 0 = do not print error
!                                               messages
!                                           1 = print error messages

!  OUTPUT:  var(nvar) - character*(*) - the character strings
!           ierror    - integer       - 0 = no error
!                                       1 = error

!  DESCRIPTION:
!    The character ~ is replaced with the value of $HOME.
!    Environment variables  starts with a $ character
!    followed by the variable name.
!    Command line arguments starts with a # character
!    followed by the argument number.
!    Both types are terminated by one these characters:
!    <space> / . $ # ? , ; \
!    If \ is used, it will be removed from the text string.

!    One text string can contain any combinations of fixed text,
!    environment variables and command line arguments.
!    The ~ is interpreted as $HOME any place in the text string.

!  LIMITATIONS:
!    Maximum length of one 'variable name' and it's actual value,
!    is 256 characters.

!-----------------------------------------------------------------------
!  DNMI/FoU  09.10.1992  Anstein Foss
!  DNMI/FoU  04.10.1993  Anstein Foss
!  DNMI/FoU  02.09.1994  Anstein Foss .... added ~ as $HOME
!  DNMI/FoU  27.01.1998  Anstein Foss .... length of "#N" incl. spaces
!  DNMI/FoU  17.02.2001  Anstein Foss .... c2f version (C/C++ -> Fortran)
!-----------------------------------------------------------------------

!..input/output:
  integer, intent(in) :: nvar,iprint,nilarg
  integer, intent(out) :: ierror
  integer, intent(in) :: ilarg(nilarg)
  character*(*), intent(out) :: var(nvar)

!..local:
  integer ::       lvarx,nbeg,nend
  parameter     (lvarx=256,nbeg=3,nend=9)
  character(256) :: var2,var3
  character(1) ::   cbeg(nbeg),cend(nend)
  logical ::       search
  integer :: i,iarg,ibeg,iend,k1,k2,km,kr,kv,k
  integer :: lenvar,lvar,n,narg,nv

! ccc integer       iargc
  integer ::       c2fiargc

!.................1.....2.....3..
  data cbeg/ '$' , '#' , '~'/
!.................1.....2.....3.....4.....5.....6.....7.....8.....9...
  data cend/ ' ' , '.' , '/' , '$' , '#' , '?' , ',' , ';' , '\' /
!      data cend/ ' ' , '.' , '/' , '$' , '#' , '?' , ',' , ';' , '\\' /
!..'\\' = '\' when read by the SGI (MIPS) f77 compiler

  ierror=0
  narg=-1
  lenvar=len(var(1))

  do nv=1,nvar
  
    lvar=0
  
    search= .TRUE. 
  
    do while (search)
    
    !..search for 'variable'
      ibeg=0
      k1=lenvar+1
      do n=1,nbeg
        k=index(var(nv),cbeg(n))
        if(k > 0 .AND. k < k1) then
          k1=k
          ibeg=n
        end if
      end do
    
      if(ibeg > 0 .AND. lvar == 0) then
        do k=1,lenvar
          if(var(nv)(k:k) /= ' ') lvar=k
        end do
      end if
      k2=lvar
    
      if(ibeg == 0) then
      
        search= .FALSE. 
      
      else
      
      !..search for end of 'variable' unless ~
      
        iend=0
        if(ibeg == 3) then
          ibeg=1
          iend=1
          k2=k1
          var2='HOME'
        else
          kv=min0(lvar-k1,lvarx)
          var2=var(nv)(k1+1:k1+kv)
          k2=lvarx
          do n=1,nend
            k=index(var2,cend(n))
            if(k > 0 .AND. k <= k2) then
              k2=k
              iend=n
            end if
          end do
          k2=k1+k2
          if(iend == 0 .OR. k2 == k1+1) goto 210
        !..keep all 'end' characters unless it is \
          if(iend /= nend) k2=k2-1
          var2=var(nv)(k1+1:k2)
        end if
        var3=' '
        if(ibeg == 1) then
        !..$name (environment variable)
          call getenv(var2,var3)
          if(var3(1:1) == ' ') goto 220
        elseif(ibeg == 2) then
        !..#n (command line argument no. n)
        ! ccc         if(narg.lt.0) narg=iargc()
          if(narg < 0) narg=c2fiargc()
          read(var2,*,err=230,end=230) iarg
          if(iarg < 1 .OR. iarg > narg) goto 230
          do i=1,nilarg
            if(iarg == ilarg(i)) goto 240
          end do
        ! ccc         call getarg(iarg,var3)
          call c2fgetarg(iarg,var3)
        end if
      
        kv=0
        do k=1,lvarx
          if(var3(k:k) /= ' ') kv=k
        end do
        km=kv-(k2-k1+1)
        kr=lvar-k2
        if(km > 0) then
          lvar=lvar+km
          if(lvar > lenvar) goto 250
          do k=kr,1,-1
            var(nv)(k2+km+k:k2+km+k)=var(nv)(k2+k:k2+k)
          end do
        elseif(km < 0) then
          do k=1,kr
            var(nv)(k2+km+k:k2+km+k)=var(nv)(k2+k:k2+k)
          end do
          do k=km+1,0
            var(nv)(k2+kr+k:k2+kr+k)=' '
          end do
        end if
        var(nv)(k1:k1+kv-1)=var3(1:kv)
      
      end if
    
    end do
  
    goto 290
  
    210 ierror=ierror+1
    if(iprint > 0) then
      if(k2 < k1)     k2=k1
      if(k2 > lenvar) k2=lenvar
      write(6,*) '** Error in string:'
      write(6,*) '** ',var(nv)(1:lvar)
      write(6,*) '** Error found in the substring:'
      write(6,*) '** ',var(nv)(k1:k2)
      write(6,*) '**----------------------------------------*'
    end if
    goto 290
  
    220 ierror=ierror+1
    if(iprint > 0) then
      write(6,*) '** Undefined environment variable in string:'
      write(6,*) '** ',var(nv)(1:lvar)
      write(6,*) '** Environment variable: ',var(nv)(k1+1:k2)
      write(6,*) '**----------------------------------------*'
    end if
    goto 290
  
    230 ierror=ierror+1
    if(iprint > 0) then
      write(6,*) '** Not existing command line argument no.', &
      ' in string:'
      write(6,*) '** ',var(nv)(1:lvar)
      write(6,*) '** Command line argument no: ',var(nv)(k1+1:k2)
      write(6,*) '** No. of command line arguments: ',narg
      write(6,*) '**----------------------------------------*'
    end if
    goto 290
  
    240 ierror=ierror+1
    if(iprint > 0) then
      write(6,*) '** Illegal command line argument no. in string:'
      write(6,*) '** ',var(nv)(1:lvar)
      write(6,*) '** Command line argument no: ',var(nv)(k1+1:k2)
      write(6,*) '** Illegal command line argument numbers:'
      write(6,*) '** ',(ilarg(i),i=1,nilarg)
      write(6,*) '**----------------------------------------*'
    end if
    goto 290
  
    250 ierror=ierror+1
    if(iprint > 0) then
      write(6,*) '** String will become too long:'
      write(6,*) '** ',var(nv)
      write(6,*) '** Variable:         ',var(nv)(k1+1:k2)
      write(6,*) '** The actual value: ',var3(1:kv)
      write(6,*) '**----------------------------------------*'
    end if
    goto 290
  
    290 continue
  
  end do

  if(ierror /= 0) ierror=1

  return
end subroutine c2fgetvar
end module c2fgetvarML
