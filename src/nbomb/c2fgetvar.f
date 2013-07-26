      subroutine c2fgetvar(nvar,var,nilarg,ilarg,iprint,ierror)
c
c  PURPOSE: Search for Environment variables and Command line arguments
c           in text strings and replace them with actual values.
c           The character ~ is interpreted as $HOME.
c
c  INPUT:   nvar          - integer       - no. of character strings
c           var(nvar)     - character*(*) - the character strings
c           nilarg        - integer       - no. of illegal command line
c                                           arguments
c           ilarg(nilarg) - integer       - the illegal command line
c                                           arguments (none illegal:
c                                           nilarg=1 and ilarg(1)=-1)
c           iprint        - integer       - 0 = do not print error
c                                               messages
c                                           1 = print error messages
c
c  OUTPUT:  var(nvar) - character*(*) - the character strings
c           ierror    - integer       - 0 = no error
c                                       1 = error
c
c  DESCRIPTION:
c    The character ~ is replaced with the value of $HOME.
c    Environment variables  starts with a $ character
c    followed by the variable name.
c    Command line arguments starts with a # character
c    followed by the argument number.
c    Both types are terminated by one these characters:
c    <space> / . $ # ? , ; \
c    If \ is used, it will be removed from the text string.
c
c    One text string can contain any combinations of fixed text,
c    environment variables and command line arguments.
c    The ~ is interpreted as $HOME any place in the text string.
c
c  LIMITATIONS:
c    Maximum length of one 'variable name' and it's actual value,
c    is 256 characters.
c
c-----------------------------------------------------------------------
c  DNMI/FoU  09.10.1992  Anstein Foss
c  DNMI/FoU  04.10.1993  Anstein Foss
c  DNMI/FoU  02.09.1994  Anstein Foss .... added ~ as $HOME
c  DNMI/FoU  27.01.1998  Anstein Foss .... length of "#N" incl. spaces
c  DNMI/FoU  17.02.2001  Anstein Foss .... c2f version (C/C++ -> Fortran)
c-----------------------------------------------------------------------
c
c..input/output:
      integer       nvar,iprint,ierror,nilarg
      integer       ilarg(nilarg)
      character*(*) var(nvar)
c
c..local:
      integer       lvarx,nbeg,nend
      parameter     (lvarx=256,nbeg=3,nend=9)
      character*256 var2,var3
      character*1   cbeg(nbeg),cend(nend)
      logical       search
c
ccccc integer       iargc
      integer       c2fiargc
c
c.................1.....2.....3..
      data cbeg/ '$' , '#' , '~'/
c.................1.....2.....3.....4.....5.....6.....7.....8.....9...
      data cend/ ' ' , '.' , '/' , '$' , '#' , '?' , ',' , ';' , '\' /
c      data cend/ ' ' , '.' , '/' , '$' , '#' , '?' , ',' , ';' , '\\' /
c..'\\' = '\' when read by the SGI (MIPS) f77 compiler
c
      ierror=0
      narg=-1
      lenvar=len(var(1))
c
      do nv=1,nvar
c
        lvar=0
c
        search=.true.
c
        do while (search)
c
c..search for 'variable'
          ibeg=0
          k1=lenvar+1
          do n=1,nbeg
            k=index(var(nv),cbeg(n))
            if(k.gt.0 .and. k.lt.k1) then
              k1=k
              ibeg=n
            end if
          end do
c
          if(ibeg.gt.0 .and. lvar.eq.0) then
            do k=1,lenvar
              if(var(nv)(k:k).ne.' ') lvar=k
            end do
          end if
          k2=lvar
c
          if(ibeg.eq.0) then
c
            search=.false.
c
          else
c
c..search for end of 'variable' unless ~
c
            iend=0
            if(ibeg.eq.3) then
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
                if(k.gt.0 .and. k.le.k2) then
                  k2=k
                  iend=n
                end if
              end do
              k2=k1+k2
              if(iend.eq.0 .or. k2.eq.k1+1) goto 210
c..keep all 'end' characters unless it is \
              if(iend.ne.nend) k2=k2-1
              var2=var(nv)(k1+1:k2)
            end if
	    var3=' '
            if(ibeg.eq.1) then
c..$name (environment variable)
              call getenv(var2,var3)
              if(var3(1:1).eq.' ') goto 220
            elseif(ibeg.eq.2) then
c..#n (command line argument no. n)
ccccc         if(narg.lt.0) narg=iargc()
              if(narg.lt.0) narg=c2fiargc()
              read(var2,*,err=230,end=230) iarg
              if(iarg.lt.1 .or. iarg.gt.narg) goto 230
              do i=1,nilarg
                if(iarg.eq.ilarg(i)) goto 240
              end do
ccccc         call getarg(iarg,var3)
              call c2fgetarg(iarg,var3)
            end if
c
	    kv=0
	    do k=1,lvarx
	      if(var3(k:k).ne.' ') kv=k
            end do
            km=kv-(k2-k1+1)
            kr=lvar-k2
            if(km.gt.0) then
              lvar=lvar+km
              if(lvar.gt.lenvar) goto 250
              do k=kr,1,-1
                var(nv)(k2+km+k:k2+km+k)=var(nv)(k2+k:k2+k)
              end do
            elseif(km.lt.0) then
              do k=1,kr
                var(nv)(k2+km+k:k2+km+k)=var(nv)(k2+k:k2+k)
              end do
              do k=km+1,0
                var(nv)(k2+kr+k:k2+kr+k)=' '
              end do
            end if
            var(nv)(k1:k1+kv-1)=var3(1:kv)
c
          end if
c
        end do
c
        goto 290
c
  210   ierror=ierror+1
        if(iprint.gt.0) then
          if(k2.lt.k1)     k2=k1
          if(k2.gt.lenvar) k2=lenvar
          write(6,*) '** Error in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Error found in the substring:'
          write(6,*) '** ',var(nv)(k1:k2)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  220   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** Undefined environment variable in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Environment variable: ',var(nv)(k1+1:k2)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  230   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** Not existing command line argument no.',
     +               ' in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Command line argument no: ',var(nv)(k1+1:k2)
          write(6,*) '** No. of command line arguments: ',narg
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  240   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** Illegal command line argument no. in string:'
          write(6,*) '** ',var(nv)(1:lvar)
          write(6,*) '** Command line argument no: ',var(nv)(k1+1:k2)
          write(6,*) '** Illegal command line argument numbers:'
          write(6,*) '** ',(ilarg(i),i=1,nilarg)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  250   ierror=ierror+1
        if(iprint.gt.0) then
          write(6,*) '** String will become too long:'
          write(6,*) '** ',var(nv)
          write(6,*) '** Variable:         ',var(nv)(k1+1:k2)
          write(6,*) '** The actual value: ',var3(1:kv)
          write(6,*) '**----------------------------------------*'
        end if
        goto 290
c
  290   continue
c
      end do
c
      if(ierror.ne.0) ierror=1
c
      return
      end
