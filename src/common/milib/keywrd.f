c
c  milib
c  
c  $Id: keywrd.f 523 2007-11-29 10:48:21Z martinr $
c
c  Copyright (C) 2006 met.no
c
c  Contact information:
c  Norwegian Meteorological Institute
c  Box 43 Blindern
c  0313 OSLO
c  NORWAY
c  email: diana@met.no
c  
c  This library is free software; you can redistribute it and/or
c  modify it under the terms of the GNU Lesser General Public
c  License as published by the Free Software Foundation; either
c  version 2.1 of the License, or (at your option) any later version.
c
c  This library is distributed in the hope that it will be useful,
c  but WITHOUT ANY WARRANTY; without even the implied warranty of
c  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
c  Lesser General Public License for more details.
c  
c  You should have received a copy of the GNU Lesser General Public
c  License along with this library; if not, write to the Free Software
c  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c
      subroutine keywrd(ntext,text,cend,csep,mkey,kwhere,nkey,ierror)
c
c  PURPOSE:  Search for keywords and their value in text string(s).
c            Keywords are converted to lowercase letters.
c
c  METHOD:   If a keyword ends with the character 'cend' (not space)
c            the following string is assumed to be it's value.
c            Otherwise this is a keyword without a value.
c            If more than one kyyword and value on the same line,
c            they are separated with the character 'csep'.
c            Lowercase conversion stops at 'cend' and starts again
c            after the following 'csep'.
c            When the place of keyword and value is recorded, this
c            is without leading and trailing spaces and also without
c            the 'cend' and 'csep' characters.
c
c  EXAMPLE:  Input:  ntext   = 2
c                    text(1) = 'FILE=test1.dat ; VAR=1,2,3 abc'
c                    text(2) = 'FILE = TEST2.dat ; VAR = 4, 5, 6'
c                    text(3) = 'Sorting.On ; PRINT.Off'
c                    cend    = '='
c                    csep    = ';'
c                    mkey    = 10
c            Output: text(1) = 'file=test1.dat ; var=1,2,3 abc'
c                    text(2) = 'file = TEST2.dat ; var = 4, 5, 6'
c                    text(3) = 'sorting.on ; print.off'
c                    nkey    = 6
c                    kwhere(1:5,1) will show where 1'st 'file' and
c                                            the value 'test1.dat' are.
c                    kwhere(1:5,2) will show where 1'st 'var'  and
c                                            the value '1,2,3 abc' are.
c                    kwhere(1:5,3) will show where 2'nd 'file' and
c                                            the value 'TEST2.dat' are.
c                    kwhere(1:5,4) will show where 2'nd 'var'  and
c                                            the value '4, 5, 6' are.
c                    kwhere(1:5,5) will show where 'sorting.on' is.
c                    kwhere(1:5,6) will show where 'print.off' is.
c                    ierror  = 0  (as nkey is not larger than mkey, and
c                                  'cend' and 'csep' are not space).
c
c-----------------------------------------------------------------------
c  DNMI/FoU  20.10.1992  Anstein Foss
c  DNMI/FoU  04.10.1993  Anstein Foss  ... lowercase keywords output
c-----------------------------------------------------------------------
c
      integer       ntext,mkey,nkey,ierror
      integer       kwhere(5,mkey)
      character*(*) text(ntext)
      character*1   cend,csep
c
      ltext=len(text(1))
c
      nkey=0
      ierror=-1
      if(cend.eq.' ' .or. csep.eq.' ' .or. csep.eq.cend) return
c
      ierror=0
c
      do n=1,ntext
        kk=1
        do while (kk.le.ltext)
          k1=0
          k2=0
          k3=0
          k4=0
          do k=kk,ltext
            if(k1.gt.0 .and. text(n)(k:k).eq.cend) goto 10
            if(k1.gt.0 .and. text(n)(k:k).eq.csep) goto 20
            if(text(n)(k:k).ne.' ') then
              if(k1.eq.0) then
                k1=k
                k2=k
              else
                k2=k
              end if
            end if
            ichr=ichar(text(n)(k:k))
c..from lowercase to uppercase (a-z)
ccc         if(ichr.gt.96 .and. ichr.lt.123) text(n)(k:k)=char(ichr-32)
c..from uppercase to lowercase (A-Z)
            if(ichr.gt.64 .and. ichr.lt.91) text(n)(k:k)=char(ichr+32)
          end do
          k=ltext
   10     kk=k+1
          do k=kk,ltext
            if(text(n)(k:k).eq.csep) goto 20
            if(text(n)(k:k).ne.' ') then
              if(k3.eq.0) then
                k3=k
                k4=k
              else
                k4=k
              end if
            end if
          end do
          k=ltext
   20     kk=k+1
          if(k1.gt.0) then
            if(nkey.lt.mkey) then
              nkey=nkey+1
              kwhere(1,nkey)=n
              kwhere(2,nkey)=k1
              kwhere(3,nkey)=k2
              kwhere(4,nkey)=k3
              kwhere(5,nkey)=k4
            else
              ierror=ierror+1
            end if
          end if
        end do
      end do
c
      return
      end
