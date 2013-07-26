#ifndef _milib_h_
#define _milib_h_

#ifdef __cplusplus
extern "C" {
#endif
  
  /*==================== GRIDPAR ====================
    c
    c  NAME:
    c     gridpar
    c
    c  PURPOSE:
    c     Convertion between integer*2 field identification and
    c     variables used in programs. Note that some gridtypes also
    c     has extended (geometry) identification behind the field data.
    c
    c  SYNOPSIS:
    c     subroutine gridpar(icall,ldata,idata,igtype,nx,ny,grid,ierror)
    c     integer   icall,ldata,igtype,nx,ny,ierror
    c     integer*2 idata(ldata)
    c     real      grid(6)
    c
    c  INPUT:
    c     icall  - +1 : from idata to igtype,nx,ny,grid
    c              -1 : from igtype,nx,ny,grid to idata
    c     ldata  - length of the idata array
    c
    c  INPUT/OUTPUT:
    c     idata  - field identification, field data (not touched)
    c              and possibly extra geometry specifications
    c              (max 20 words in this version)
    c     igtype - grid type, 1 = polarstereographic grid (true at 60N)
    c                         2 = geographic
    c                         3 = spherical rotated grid
    c                         4 = polarstereographic grid
    c                         5 = mercator grid (unrotated)
    c                         * = unknown grid type (-32767 to +32 accepted)
    c              (note that 'codes' are used in the field identification
    c               when extra geometry identification is stored after
    c               the field data, the reason for the +32 limit)
    c     nx     - no. of gridpoints in x direction (1 - 32767)
    c     ny     - no. of gridpoints in y direction (1 - 32767)
    c     grid   - grid parameters
    c                igtype=1,4, polarstereographic grid:
    c                          grid(1) = x position of north pole (xp)
    c                          grid(2) = y position of north pole (yp)
    c                          grid(3) = no. of grid units between
    c                                    North Pole and Equator
    c                          grid(4) = grid rotation angle (degrees),
    c                                    positive east, negative west
    c                          grid(5) = projection latitude
    c                                    (degrees) standard is 60 (60 deg. N)
    c                          grid(6) = 0.  (not used)
    c                igtype=2,3, geographic or spherical rotated grid:
    c                          grid(1) = western boundary (degrees)
    c                                    (longitude for x=1)
    c                          grid(2) = southern boundary (degrees)
    c                                    (latitude for y=1)
    c                          grid(3) = longitude increment (degrees)
    c                          grid(4) = latitude  increment (degrees)
    c                          grid(5) = longitude position of rotated equator
    c                                    (degrees)  (0 if geographic grid)
    c                          grid(6) = latitude  position of rotated equator
    c                                    (degrees)  (0 if geographic grid)
    c                igtype=5, mercator (unrotated) grid:
    c                          grid(1) = western boundary (degrees)
    c                                    (longitude for x=1)
    c                          grid(2) = southern boundary (degrees)
    c                                    (latitude for y=1)
    c                          grid(3) = x (longitude) increment (km)
    c                          grid(4) = y (latitude)  increment (km)
    c                          grid(5) = reference (construction) latitude
    c                                    (degrees)
    c                          grid(6) = 0.  (not used)
    c                igtype=*, unknown grid type,
    c                          only use grid type less than 1 if the
    c                          grid parameters have no meaning:
    c                          grid(1:6) : unknown grid parameters
    c
    c  OUTPUT:
    c     ierror - error status: 0 = no error
    c                            1 = bad value in input identification
    c                                or in input grid parameters etc.
    c                            2 = ldata too small
    c                            3 = unknown icall
    c                            4 = ldata too small for needed extra
    c                                geometry identification (icall=-1),
    c                                but the best possible identification
    c                                is done (see NOTES below)
    c
    c  DOCUMENTATION:
    c     Basic document on felt files:
    c          FILE STRUKTUR FOR "SANNTIDS" LAGRING AV GRID-DATA
    c               Forskningsavdeling DNMI, oktober 1982
    c     See also /usr/local/doc/felt.doc (at DNMI)
    c
    c  NOTES:
    c   - This routine maintain compability with old formats
    c     (see comments in the source code),
    c     new formats added when required for some reason.
    c   - Specyfing ldata too short to hold extra geometry identification
    c     (for icall=-1) will restrict this routine from making this even
    c     when it seems appropriate. In order to be compatible with
    c     old unextended (or less extended) formats you then may
    c     disregard the returned ierror=4 (in special cases).
    c   - Avoid calling this routine more often than necessary,
    c     i.e. only after reading the first field or only before output
    c     of the first field.
    c     In models with several calling routines you may do a 'setup' call
    c     and keep (part of) the first 20 words of identification and
    c     possibly extra geometry specification locally (see note below).
    c   - The value of nx and ny (in idata if icall=+1) does not influence
    c     conversion of grid parameters.
    c     You may then use this routine to convert between field
    c     identification and grid parameters with nx=ny=1 and possibly
    c     find extra identification in word 22,23,... in idata.
    c     (see DOCUMENTATION to find format description).
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  05.05.1995  Anstein Foss
    c  DNMI/FoU  09.06.1995  Anstein Foss
    c  DNMI/FoU  14.05.1996  Anstein Foss ... mercator (unrotated)
    c  DNMI/FoU  02.09.1996  Anstein Foss ... gridtype 2012 -> 2 (bad test)
    c  DNMI/FoU  15.10.1996  Anstein Foss ... even better scaling when needed
    c  DNMI/FoU  17.02.1997  Anstein Foss ... and again (for 'image' fields)
    c-----------------------------------------------------------------------*/

  extern void gridpar(int icall, int ldata, short *idata,
		      int *igtype, int *nx, int *ny,
		      float *grid, int *ierror);

  /*==================== MRFELT ====================
    c
    c  NAME:
    c     mrfelt
    c
    c  PURPOSE:
    c     Master read field routine.
    c     Open file, read field(s) from Felt file, scale data and close file.
    c     Using the standard routine rfelt to read fields.
    c
    c  SYNOPSIS:
    c     subroutine mrfelt(mode,filnam,iunit,in,ipack,lfield,field,fscale,
    c    +                  ldata,idata,ierror)
    c     integer       mode,iunit,ipack,lfield,ldata,ierror
    c     real          fscale
    c     real          field(lfield)
    c     integer*2     in(16),idata(ldata)
    c     character*(*) filnam
    c
    c  INPUT:
    c     mode    -  0 = combination of 1, 2 and 3 below
    c                1 = open Felt file
    c                2 = read field
    c                3 = close Felt file
    c		     10,11,12,13 are the same as 0,1,2,3 except that no
    c		                 error messages are printed
    c     filnam  -  file name (if filename(1:1)='*' the file is opened
    c			    without 'file=filnam', only with unit no.)
    c     iunit   -  file unit no.
    c     in      -  parameters describing the field ('innholdsfortegnelse'):
    c                  in( 1) = producer no.
    c                  in( 2) = grid no.
    c                  in( 3) = -32767  or  year
    c                  in( 4) = -32767  or  month*100+day
    c                  in( 5) = -32767  or  hour*100+minute
    c                  in( 9) = data type
    c                  in(10) = forecast length (hours)
    c                  in(11) = vertical coordinate
    c                  in(12) = parameter no.
    c                  in(13) = level 1
    c                  in(14) = level 2 (usually 0)
    c                if at least one of the in(3:5) equals -32767 then
    c                date/time is not checked, always set date/time in
    c                in(3:5) if the file is an Archive or Cyclic Archive
    c                Felt file.
    c     ipack   -  0 = return the field in the idata array (no unpacking)
    c                1 = return the field in the field array and the
    c                    field identification in the first part of
    c                    the idata array, the field will be scaled using
    c                    scaling exponent in idata(20) and fscale,
    c                    not checking undefined/missing values
    c                2 = as 1 above, but will now also check
    c                    undefined/missing values, i.e. input elements
    c                    with the value -32767 will be set to +1.e+35 .
    c     lfield  -  maximum length of the field (nx*ny), for ipack=1,2
    c     fscale  -  extra scaling of field, for ipack=1,2 (usually 1.)
    c     ldata   -  maximum length of identification and field (20+nx*ny),
    c		 for mode=1,11 and ldata>1:
    c		     returning the first ldata words (max. 1024) in the
    c		     idata array from record 1 in the file, and if
    c		     mode=1,11 and ldata>1024 also returning ldata-1024
    c		     words (max. 1024) from record 2
    c
    c  OUTPUT:
    c     field   -  the field, if ipack=1,2
    c     idata   -  identification and field (all ipack values),
    c                grid dimensions are found in the identification
    c                (i.e. the 20 first elements of idata):
    c                       nx=idata(10)
    c                       ny=idata(11)
    c		 for mode=1,11: see ldata above
    c     ierror  -  exit status
    c                  0 = no error
    c                  1 = some error (see printed message)
    c
    c  NOTES:
    c     +  Note the use of ldata and idata for mode=1,11 if ldata>1,
    c	     usually it is enough to return 32 words if used at all.
    c     +  If the Felt file are opened elsewhere, then a call with
    c        mode=1 is not necessary.
    c     +  If the Felt file are closed elsewhere, then a call with
    c        mode=3 is not necessary.
    c     +  As rfelt really is a simple routine to use, you can keep
    c        more than one file open when using mrfelt.
    c     +  Use mode=0 only when reading one or a few fields,
    c        otherwise this will cause unnecessary overhead.
    c     +  Call sequence:
    c           call mrfelt(1,filnam,iunit,in,ipack,1,1,1,1,1,ierror)
    c           do while <more fields to read>
    c             call mrfelt(2,filnam,iunit,in,ipack,lfield,field,fscale,
    c        -                  ldata,idata,ierror)
    c           end do
    c           call mrfelt(3,filnam,iunit,in,ipack,1,1,1,1,1,ierror)
    c     +  When the field array is not used (ipack=0):
    c           call mrfelt(mode,filnam,iunit,in,0,1,1,1,ldata,idata,ierror)
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  01.10.1993  Anstein Foss
    c  DNMI/FoU  02.12.1993  Anstein Foss
    c  DNMI/FoU  15.03.1995  Anstein Foss ... silent modes
    c  DNMI/FoU  14.02.1996  Anstein Foss ... ldata,idata for mode=1,11
    c  DNMI/FoU  10.11.2000  Anstein Foss ... automatic byte swap
    c-----------------------------------------------------------------------*/

  extern void mrfelt(int mode, const char *filename, int iunit,
		     short in[16], int ipack, int lfield,
		     float *field, float fscale, int ldata,
		     short *idata, int *ierror);

  /*==================== MWFELT ====================
    c
    c  NAME:
    c     mwfelt
    c
    c  PURPOSE:
    c     Master write field routine.
    c     Open file, scale data, write field(s) to Felt file and close file.
    c     Using the standard routine wfelt to write fields.
    c
    c  SYNOPSIS:
    c     subroutine mwfelt(mode,filnam,iunit,ipack,lfield,field,fscale,
    c    +                  ldata,idata,ierror)
    c     integer       mode,iunit,ipack,lfield,ldata,ierror
    c     real          fscale
    c     real          field(lfield)
    c     integer*2     idata(ldata)
    c     character*(*) filnam
    c
    c  INPUT:
    c     mode    -  0 = combination of 1, 2 and 3 below
    c                1 = open Felt file and initialize
    c                2 = write field
    c                3 = update record 1 and close Felt file
    c		 10,11,12,13 are the same as 0,1,2,3 except that no
    c		             error messages are printed
    c     filnam  -  file name (if filename(1:1)='*' the file is opened
    c			    without 'file=filnam', only with unit no.)
    c     iunit   -  file unit no.
    c     ipack   -  0 = the field is in the idata array (no packing)
    c                1 = the field is in the field array and the
    c                    field identification is in the first part of
    c                    the idata array, the field will be scaled using
    c                    scaling exponent in idata(20) and fscale,
    c		     if idata(20)=-32767 then the best possible
    c		     scaling will be found and used,
    c                    not checking undefined/missing values
    c                2 = as 1 above, but will now also check
    c                    undefined/missing values, i.e. elements with
    c                    the value +1.e+35 (-32767 in output field)
    c     lfield  -  length of the field (nx*ny), for ipack=1,2
    c     field   -  field to be packed, for ipack=1,2
    c     fscale  -  extra scaling of field, for ipack=1,2 (usually 1.)
    c     ldata   -  maximum length of identification and field (20+nx*ny),
    c		 in some cases there also is extra geometry
    c		 identification after the field data,
    c		 for mode=1,11,3,13 and ldata>1:
    c		     returning the first ldata words (max. 1024) in the
    c		     idata array from record 1 in the file, and if
    c		     mode=1,11 and ldata>1024 also returning ldata-1024
    c		     words (max. 1024) from record 2 (for mode=1,11 this
    c		     is contents before any update and for mode=3,13
    c		     after last update of the file header)
    c     idata   -  identification and field if ipack=0,
    c                identification and space for output field
    c                if ipack=1,2
    c
    c  OUTPUT:
    c     idata   -  for mode=1,11,3,13: see ldata above
    c     ierror  -  exit status
    c                  0 = no error
    c                  1 = some error (see printed message)
    c
    c  NOTES:
    c     +  Note the use of ldata and idata for mode=1,11,3,13 if ldata>1,
    c	 usually it is enough to return 32 words if used at all.
    c     +  If field exists on a Felt file, it is overwritten.
    c     +  One cannot write to a second file without closing the
    c        first file.
    c     +  Use mode=0 only when writing one or a few fields,
    c        otherwise this will cause unnecessary overhead.
    c     +  Call sequence:
    c           call mwfelt(1,filnam,iunit,ipack,1,1,1,1,1,ierror)
    c           do while <more fields to write>
    c             call mwfelt(2,filnam,iunit,ipack,lfield,field,fscale,
    c        -                  ldata,idata,ierror)
    c           end do
    c           call mwfelt(3,filnam,iunit,ipack,1,1,1,1,1,ierror)
    c     +  When the field array is not used (ipack=0):
    c           call mwfelt(mode,filnam,iunit,0,1,1,1,ldata,idata,ierror)
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  01.10.1993  Anstein Foss
    c  DNMI/FoU  12.03.1994  Anstein Foss
    c  DNMI/FoU  15.03.1995  Anstein Foss ... silent modes
    c  DNMI/FoU  31.03.1995  Anstein Foss ... automatic scaling
    c  DNMI/FoU  09.06.1995  Anstein Foss ... extra ident, gridtype 3,4
    c  DNMI/FoU  14.02.1996  Anstein Foss ... ldata,idata for mode=1,11,3,13
    c-----------------------------------------------------------------------*/

  extern void mwfelt(int mode, const char *filename, int iunit,
		     int ipack, int lfield,
		     float *field, float fscale, int ldata,
		     short *idata, int *ierror);

  /*==================== QFELT ====================
    c
    c  NAME:
    c     qfelt
    c
    c  PURPOSE:
    c     Inquire felt file contents
    c
    c  SYNOPSIS:
    c     subroutine qfelt(iunit,ireq,iexist,nin,in,ifound,nfound,
    c    +                 iend,ierror,ioerr)
    c     integer    iunit,ireq,iexist,nin,nfound,iend,ierror,ioerr
    c     integer    ifound(nin)
    c     integer*2  in(16,nin)
    c
    c  INPUT:
    c     iunit         - felt file unit,
    c                     if qfelt is called with several files, using
    c                     the same file unit no., reset qfelt by a call
    c                     with iunit=0.
    c     ireq          - request type:
    c                       1 = return all occurrences of in(1:16,1),
    c                           in(i,1)=-32767 means any value
    c                       2 = return different occurrences of one of the
    c                           elements in in(1:16,1), the selected
    c                           element is the first with value equal
    c                           -32766 , while -32767 means any value
    c                       3 = return first occurrence of in(1:16,1:nin),
    c                           where in(i,n)=-32767 means any value
    c                       0 = reset when stopping requests of type
    c                           1 or 2 before end is reached and the
    c                           next request is identical, but should
    c                           start from top of the file index
    c                       11,12,13: as 1,2,3 above, but see output ifound
    c     iexist        -  control of data existence:
    c                      0 = return all innh.fort. found
    c                      1 = return only innh.fort. with existing data
    c                          (for request types 1 and 2)
    c                      2 = return only innh.fort. without existing data
    c                          (for request types 1 and 2)
    c     nin           - max. no. of innh.fort. to be returned
    c     in(16,nin)    - see requirements for each request type (ireq)
    c                     and the notes below
    c
    c  OUTPUT:
    c     in(16,...)    - the innh.fort. found
    c     ifound(...)   - length of the fields if they exist,
    c                     for ireq=1,2,3 the length of the integer*2 buffer
    c                     for ireq=11,12,13 the field size (nx*ny)
    c                      0 if an innh.fort. is found without data,
    c                     -1 if innh.fort. is not found
    c     nfound        - no. of innh.fort. found, for request type 1 and 2
    c                     (for request type 3 nfound=nin unless some
    c     iend          - completion status:
    c                      0 = more calls to qfelt is necessary to
    c                          return all requested information,
    c                          for request types 1 and 2
    c                      1 = all requested information is returned
    c                          (if same request is repeated in next call,
    c                           qfelt starts from top of the file index)
    c     ierror        -  0 = no error
    c                      1 = read error (file error)
    c                      2 = unknown ireq or iexist specified
    c                      3 = error in input in array
    c     ioerr         -  read error (iostat, 0 = no error)
    c
    c  NOTES:
    c     - qfelt always returns iend=1 and nfound=0 if ierror =/= 0.
    c     - a continued call (after iend=0) is identified by unchanged
    c       file (iunit), request type (ireq) and in(1:16,1) .
    c     - for request type 2 does a continued call not mean that qfelt
    c       returns innh.fort. elements different from the previous call(s),
    c       but all element values will be returned (possibly more than
    c       once)
    c     - iend=1 doesn't mean there are more information, rather that
    c       the 'in' array is full before the complete file index has neen
    c       searched
    c     - remeber to reset in(1:16,1) before continued calls.
    c     - input date and time, in(3:5,n), are used, set these elements
    c       to -32767 if they are unimportant
    c     - input in(6:8,n) and in(16,n) are not used
    c     - in(1:16,n) means in(i,n) for i= 1 to 16  (fortran90 style)
    c     - for performance (i/o) reasons, nin should not be a very small
    c       number for request type 1 and 2, rather N*64 (if a small number
    c	means many calls to qfelt)
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  30.09.1993  Anstein Foss
    c  DNMI/FoU  29.05.1996  Anstein Foss ... iexist=2
    c  DNMI/FoU  10.11.2000  Anstein Foss ... automatic byte swap
    c  DNMI/FoU  01.02.2001  Anstein Foss ... ireq= 11,12,13 (ifound()=nx*ny)
    c-----------------------------------------------------------------------*/

  extern void qfelt(int iunit, int ireq, int iexist, int nin, short *in,
		    int *ifound, int *nfound,
 		    int *iend, int *ierror, int *ioerr);

  /*==================== CREFELT ====================
    c
    c  NAME:
    c     crefelt
    c
    c  PURPOSE:
    c     Create a DNMI felt file.
    c     For simple creation of felt files from a program without
    c     having to run the standard program NYFELT.
    c     If the file exists, it is removed first.
    c     The file is closed before exit.
    c
    c  SYNOPSIS:
    c     subroutine crefelt(filename,iunit,itype,ltime,itime,
    c    +                   icode,lspec,ispec,lopt,iopt,ierror)
    c     integer       iunit,itype,ltime,lopt,icode,lspec,ierror
    c     integer       itime(ltime),iopt(lopt)
    c     integer*2     ispec(lspec)
    c     character*(*) filename
    c
    c  INPUT:
    c     filename  -  the felt file name
    c     iunit     -  file unit no. for felt file
    c     itype     -  felt file type,
    c                      itype=999 : standard felt file
    c                      itype=998 : archive  felt file
    c                      itype=997 : cyclic archive felt file
    c     ltime     -  length of the itime array,
    c                      ltime=5 if itype=999
    c                      ltime=8 if itype=998,997
    c     itime     -  date,time (utc)
    c                  for standard felt file (itype=999):
    c                      itime(1) : year
    c                      itime(2) : month
    c                      itime(3) : day
    c                      itime(4) : hour
    c                      itime(5) : minute
    c                  for (cyclic) archive felt file (itype=998,997):
    c                      itime(1) : first year
    c                      itime(2) : first month
    c                      itime(3) : first day
    c                      itime(4) : first hour
    c                      itime(5) : first minute
    c                      itime(6) : no. of time steps
    c                      itime(7) : time step type (in itime(8))
    c                                   1=years  2=months  3=days
    c                                   4=hours  5=minutes
    c                      itime(8) : time step
    c     icode     -  format code for the ispec array (see ispec)
    c     lspec     -  length of the ispec array
    c     ispec     -  felt file index specifications,
    c                  icode=0:
    c                     then mostly 'unspecified' indecies are created,
    c                     all with the same producer and grid, lspec=3 and
    c                        ispec(1) : producer no. (88=DNMI)
    c                        ispec(2) : grid no.
    c                        ispec(3) : no. of indecies to be created
    c                                   with the above specifications
    c                                   and date/time(s) in itime.
    c                  icode=1:
    c                     consider ispec an array of type ispec(8,nindex),
    c                     then lspec=8*nindex, where nindex is the number
    c                     of field indecies to be created, then
    c                        ispec(1,n) : producer no. (88=DNMI)
    c                        ispec(2,n) : grid no.
    c                        ispec(3,n) : data type (1=analysis,...)
    c                        ispec(4,n) : forecast hour (0 if analysis)
    c                        ispec(5,n) : vertical coordinate
    c                        ispec(6,n) : parameter
    c                        ispec(7,n) : level
    c                        ispec(8,n) : second level (usually 0)
    c                     if the felt file type is (cyclic) archive,
    c                     all indecies will be created for each time step.
    c                  other icode's and ispec formats may be added later...
    c     lopt      -  length of the iopt array
    c                  (possibly required by icode, see icode above)
    c     iopt      -  options
    c                      iopt(1)=0 : data gaps allowed (default)
    c                             =1 : data gaps not allowed
    c                  other options may be added later...
    c
    c  OUTPUT:
    c     ierror    -  error exit status
    c                       0 = no error
    c                       1 = some input specification error
    c                           (see printed message),
    c                           file not created.
    c                       2 = some I/O error (see printed message),
    c                           file not created.
    c
    c  RELATED DOCUMENTATION:
    c     Basic document on FELT files:
    c          FILE STRUKTUR FOR "SANNTIDS" LAGRING AV GRID-DATA
    c               Forskningsavdeling DNMI, oktober 1982
    c     Latest update:
    c          /usr/local/doc/felt.doc (at DNMI)
    c     About the program NYFELT (and felt file features):
    c          /usr/local/doc/nyfelt.doc (at DNMI)
    c
    c  NOTES:
    c     - producer numbers are from 1 to 99 (WMO model identifiers),
    c       and if more than one used, they must be in increasing order.
    c     - the maximum no. of field indecies in one felt file is 32767.
    c     - only one producer allowed in (cyclic) archive felt file.
    c     - unlike NYFELT, this routine handles minutes.
    c     - unlike NYFELT, for cyclic archive felt file the specified
    c       date,time will be used as the first in the file.
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  26.01.1997  Anstein Foss
    c-----------------------------------------------------------------------*/

  extern void crefelt(const char *filename, int iunit, int itype,
		      int ltime, int *itime,
		      int icode, int lspec, short *ispec,
		      int lopt, int *iopt, int *ierror);

  /*==================== XYCONVERT ====================
    c     subroutine xyconvert(npos,x,y,igtypa,ga,igtypr,gr,ierror)
    c
    c****************************************************************
    c
    c     xyconvert - convert coordinates from one grid to another
    c
    c  purpose:
    c
    c     coordinate conversion between different grids; spherical,
    c     spherical rotated, polar stereographic or mercator.
    c     (you may use uvconvert to turn vector (velocity) components.)
    c
    c  input/output parameters:
    c
    c     npos   - no. of positions (in x,y)
    c     x      - input/output x position
    c     y      - input/output y position
    c     igtypa - input  grid type
    c     ga     - input  grid description
    c     igtypr - output grid type
    c     gr     - output grid description
    c     ierror - output error status, 0=no error
    c
    c  description of g = ga and gr (for igtype = igtypa and igtypr):
    c
    c  for spherical (rotated) grid, igtype=2,3:
    c
    c     g(1) - western boundary (degrees)
    c     g(2) - southern boundary (degrees)
    c     g(3) - longitude increment (degrees)
    c     g(4) - latitude increment (degrees)
    c     g(5) - xcen: longitude position of rotated equator (degrees)
    c     g(6) - ycen: latitude  position of rotated equator (degrees)
    c            (lamda,theta)=(xcen,ycen) at (lamda',theta')=(0,0),
    c            where (lamda,theta) are usual spherical coord. and
    c            (lamda',theta') are rotated spherical coord.
    c            xcen = ycen = 0 for usual spherical coord.
    c
    c     (note: if the coordinates are geographic longitude(x),latitude(y)
    c            set igtype=2 and g(1:6) = 1.,1.,1.,1.,0.,0. )
    c
    c  for polar stereographic grid, igtype=1,4:
    c
    c     g(1) - x-position of north pole
    c     g(2) - y-position of north pole
    c     g(3) - number of grid distances between pole and equator
    c     g(4) - rotation angle of the grid (degrees)
    c     g(5) - projection latitude (degrees)
    c            (60 degrees north for igtype=1)
    c     g(6) - 0. (not used)
    c
    c  for mercator (unrotated) grid, igtype=5:
    c
    c     g(1) - western boundary (longitude for x=1) (degrees)
    c     g(2) - southern boundary (latitude for y=1) (degrees)
    c     g(3) - x (longitude) increment (km)
    c     g(4) - y (latitude)  increment (km)
    c     g(5) - reference (construction) latitude (degrees)
    c     g(6) - 0.  (not used)
    c
    c  externals:
    c
    c     pol2sph - polar sterographic <-> spherical coordinates
    c     sph2rot - spherical <-> spherical rotated coordinates
    c     mer2sph - mercator (unrotated) <-> spherical coordinates
    c
    c  history:
    c
    c     j.e. haugen/dnmi      nov -94 ... grd2grd
    c     a.   foss   dnmi   02.02.1995 ... no size limits
    c     a.   foss   dnmi   25.08.1995 ... xyconvert
    c     a.   foss   dnmi   15.05.1996 ... mercator (unrotated)
    c
    c****************************************************************/

  extern void xyconvert(int npos, float *x, float *y,
			int igtypa, float *ga,
			int igtypr, float *gr, int *ierror);

  /*==================== UVCONVERT ====================
    c      subroutine uvconvert(npos,xr,yr,u,v,
    c     +                     igtypa,ga,igtypr,gr,udef,ierror)
    c
    c****************************************************************
    c
    c     uvconvert - turn vector (velocity) components between grids
    c
    c  purpose:
    c
    c     turn vector (velocity) components between different grids;
    c     spherical, spherical rotated, polar stereographic or mercator.
    c     (you may use xyconvert to convert the positions first.)
    c
    c  input/output parameters:
    c
    c     npos   - no. of positions (in xr,yr,u,v)
    c     xr     - input x position in the output grid
    c     yr     - input y position in the output grid
    c     u      - input/output vector (velocity) component in x-direction
    c     v      - input/output vector (velocity) component in y-direction
    c     igtypa - input  grid type
    c     ga     - input  grid description
    c     igtypr - output grid type
    c     gr     - output grid description
    c     ierror - output error status, 0=no error
    c
    c  description of g = ga and gr (for igtype = igtypa and igtypr):
    c
    c  for spherical (rotated) grid (igtype=2,3):
    c
    c     g(1) - western boundary (degrees)
    c     g(2) - southern boundary (degrees)
    c     g(3) - longitude increment (degrees)
    c     g(4) - latitude increment (degrees)
    c     g(5) - xcen: longitude position of rotated equator (degrees)
    c     g(6) - ycen: latitude  position of rotated equator (degrees)
    c            (lamda,theta)=(xcen,ycen) at (lamda',theta')=(0,0),
    c            where (lamda,theta) are usual spherical coord. and
    c            (lamda',theta') are rotated spherical coord.
    c            xcen = ycen = 0 for usual spherical coord.
    c
    c     (note: if the coordinates are geographic longitude(x),latitude(y)
    c            set igtype=2 and g(1:6) = 1.,1.,1.,1.,0.,0. )
    c
    c  for polar stereographic grid (igtype=1,4):
    c
    c     g(1) - x-position of north pole
    c     g(2) - y-position of north pole
    c     g(3) - number of grid distances between pole and equator
    c     g(4) - rotation angle of the grid (degrees)
    c     g(5) - projection latitude (degrees)
    c            (60 degrees north for igtype=1)
    c     g(6) - 0. (not used)
    c
    c  for mercator (unrotated) grid, igtype=5:
    c
    c     g(1) - western boundary (longitude for x=1) (degrees)
    c     g(2) - southern boundary (latitude for y=1) (degrees)
    c     g(3) - x (longitude) increment (km)
    c     g(4) - y (latitude)  increment (km)
    c     g(5) - reference (construction) latitude (degrees)
    c     g(6) - 0.  (not used)
    c
    c  externals:
    c
    c     pol2sph - polar sterographic <-> spherical coordinates
    c     sph2rot - spherical <-> spherical rotated coordinates
    c     mer2sph - mercator (unrotated) <-> spherical coordinates
    c
    c  history:
    c
    c     j.e. haugen/dnmi      nov -94 ... grv2grv
    c     a.   foss   dnmi   02.02.1995 ... no size limits
    c     a.   foss   dnmi   25.08.1995 ... uvconvert
    c     a.   foss   dnmi   15.05.1996 ... mercator (unrotated)
    c     a.   foss   dnmi   22.02.2000 ... correct polarst. "s.pole"
    c
    c****************************************************************/

  extern void uvconvert(int npos, float *xr, float *yr,
			float *u, float *v,
			int igtypa, float *ga,
			int igtypr, float *gr, float udef, int *ierror);

  /*==================== MAPFIELD ====================
    c
    c  NAME:
    c     mapfield
    c
    c  PURPOSE:
    c     Compute parameters (fields) dependant of the map projection,
    c     map ratio (in x and y direction) and coriolis parameter.
    c
    c  SYNOPSIS:
    c     subroutine mapfield(imapr,icori,igtype,grid,nx,ny,xm,ym,fc,
    c    +			  hx,hy,ierror)
    c     integer imapr,icori,igtype,nx,ny,ierror
    c     real    grid(6),xm(nx,ny),ym(nx,ny),fc(nx,ny),hx,hy
    c
    c  INPUT:
    c     imapr  - 0 : not compute map ratio
    c              1 : compute map ratio
    c              2,... : compute map ratio divided by grid resolution
    c		       in meter times imapr-1
    c     icori  - 0 : not compute coriolis parameter
    c              1 : compute coriolis parameter
    c     igtype - grid type, 1 = polarstereographic grid (true at 60 deg. N)
    c			  2 = geographic
    c			  3 = spherical rotated grid
    c			  4 = polarstereographic grid
    c			  5 = mercator grid (unrotated)
    c     grid   - grid parameters
    c                igtype=1,4, polarstereographic grid:
    c			   grid(1) = x position of north pole (xp)
    c			   grid(2) = y position of north pole (yp)
    c			   grid(3) = no. of grid units between
    c				     North Pole and Equator
    c			   grid(4) = grid rotation angle (degrees),
    c				     positive east, negative west
    c			   grid(5) = projection latitude
    c				     (degrees) standard is 60 (60 deg. N)
    c			   grid(6) = 0.  (not used)
    c                igtype=2,3, geographic or spherical rotated grid:
    c			   grid(1) = western boundary (degrees)
    c				     (longitude for x=1)
    c			   grid(2) = southern boundary (degrees)
    c				     (latitude for y=1)
    c			   grid(3) = longitude increment (degrees)
    c			   grid(4) = latitude  increment (degrees)
    c			   grid(5) = longitude position of rotated equator
    c				     (degrees)  (0 if geographic grid)
    c			   grid(6) = latitude  position of rotated equator
    c				     (degrees)  (0 if geographic grid)
    c                igtype=5, mercator (unrotated) grid:
    c			   grid(1) = western boundary (degrees)
    c				     (longitude for x=1)
    c			   grid(2) = southern boundary (degrees)
    c				     (latitude for y=1)
    c			   grid(3) = x (longitude) increment (km)
    c			   grid(4) = y (latitude)  increment (km)
    c			   grid(5) = reference (construction) latitude
    c                                    (degrees)
    c			   grid(6) = 0.  (not used)
    c     nx     - no. of gridpoints in x direction
    c     ny     - no. of gridpoints in y direction
    c
    c  OUTPUT:
    c     xm     - map ratio in x direction (if imapr>0)
    c     ym     - map ratio in x direction (if imapr>0)
    c     fc     - coriolis parameter (if icori>0)
    c     hx     - grid resolution in meter in x direction (map ratio = 1)
    c     hy     - grid resolution in meter in y direction (map ratio = 1)
    c     ierror - error status: 0 = no error
    c			     1 = bad value in input grid array
    c			     2 = unknown grid type (igtype)
    c                        3 = internal conversion error
    c
    c  NOTES:
    c     To avoid division by zero in calling routines, some uncorrect
    c     values may be returned:
    c     1) Coriolis parameter (all grid types):
    c	 The minimum value is computed 1/100 grid unit from equator.
    c	 Correct value is 0 at equator.
    c     2) Map ratio (x or longitude direction), geographic and spherical
    c	 rotated grids:
    c	 The minimum value is computed 1/100 grid unit from the pole.
    c	 Correct value is infinite at the pole.
    c     Example of map ratio usage, xm,ym computed with imapr=1:
    c        x_gradient = xm(i,j)*(field(i+1,j)-field(i-1,j))/(hx*2.)
    c        y_gradient = ym(i,j)*(field(i,j+1)-field(i,j-1))/(hy*2.)
    c     Example of map ratio usage, xm,ym computed with imapr=3:
    c        x_gradient = xm(i,j)*(field(i+1,j)-field(i-1,j))
    c        y_gradient = ym(i,j)*(field(i,j+1)-field(i,j-1))
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  09.06.1995  Anstein Foss
    c  DNMI/FoU  31.10.1995  Anstein Foss
    c  DNMI/FoU  04.06.1996  Anstein Foss ... mercator (unrotated)
    c-----------------------------------------------------------------------*/

  extern void mapfield(int imapr, int icori, int igtype, float *grid,
		       int nx, int ny, float *xm, float *ym, float *fc,
		       float *hx, float *hy, int *ierror);

  /*==================== MOVEGRID ====================
    c
    c  NAME:
    c     movegrid
    c
    c  PURPOSE:
    c     Adjust grid parameters to a new origo.
    c     May be used when only a part of a field is handled and other
    c     routines can't handle a sub area and need the grid parameters.
    c     The origo (or rather position x=1,y=1) is moved dx,dy grid units
    c     in x and y direction.
    c
    c  SYNOPSIS:
    c     subroutine movegrid(igtype,grid,dx,dy,gridmv,ierror)
    c     integer igtype,ierror
    c     real    grid(6),dx,dy,gridmv(6)
    c
    c  INPUT:
    c     igtype - grid type, 1 = polarstereographic grid (true at 60 deg. N)
    c			  2 = geographic
    c			  3 = spherical rotated grid
    c			  4 = polarstereographic grid
    c			  5 = mercator (unrotated) grid
    c     grid   - grid parameters
    c                igtype=1,4, polarstereographic grid:
    c			   grid(1) = x position of north pole (xp)
    c			   grid(2) = y position of north pole (yp)
    c			   grid(3) = no. of grid units between
    c				     North Pole and Equator
    c			   grid(4) = grid rotation angle (degrees),
    c				     positive east, negative west
    c			   grid(5) = projection latitude
    c				     (degrees) standard is 60 (60 deg. N)
    c			   grid(6) = 0.  (not used)
    c                igtype=2,3, geographic or spherical rotated grid:
    c			   grid(1) = western boundary (degrees)
    c				     (longitude for x=1)
    c			   grid(2) = southern boundary (degrees)
    c				     (latitude for y=1)
    c			   grid(3) = longitude increment (degrees)
    c			   grid(4) = latitude  increment (degrees)
    c			   grid(5) = longitude position of rotated equator
    c				     (degrees)  (0 if geographic grid)
    c			   grid(6) = latitude  position of rotated equator
    c				     (degrees)  (0 if geographic grid)
    c                igtype=5, mercator (unrotated) grid:
    c			   grid(1) = western boundary (degrees)
    c				     (longitude for x=1)
    c			   grid(2) = southern boundary (degrees)
    c				     (latitude for y=1)
    c			   grid(3) = x (longitude) increment (km)
    c			   grid(4) = y (latitude)  increment (km)
    c			   grid(5) = reference (construction) latitude
    c                                    (degrees)
    c			   grid(6) = 0.  (not used)
    c     dx     - no. of gridpoints move in x direction
    c     dy     - no. of gridpoints move in y direction
    c
    c  OUTPUT:
    c     gridmv - grid parameters after move
    c	       (same description as for input grid)
    c     ierror - error status: 0 = no error
    c			     1 = unknown grid type (igtype)
    c
    c-----------------------------------------------------------------------
    c  DNMI/FoU  04.06.1996  Anstein Foss
    c-----------------------------------------------------------------------*/

  extern void movegrid(int igtype, float *grid, float dx, float dy,
		       float *gridmv, int *ierror);

  /*==================== XYCONST ====================
    c
    c      subroutine xyconst(igtyp1,grid1,igtyp2,grid2,cxy1,cxy2,ierror)
    c
    c        hvis mulig beregnes konstanter for omregning av
    c        kooordinater mellom 2 kart-projeksjoner.
    c
    c        grid typer:  1=polarstereografisk
    c                     2=geografisk
    c                     3=sfaerisk (rotert)
    c                     4=polarstereografisk
    c                     5=merkator (urotert)
    c
    c        x2 = cxy1(1) + cxy1(2)*x1 + cxy1(3)*y1
    c        y2 = cxy1(4) + cxy1(5)*x1 + cxy1(6)*y1
    c
    c        x1 = cxy2(1) + cxy2(2)*x2 + cxy2(3)*y2
    c        y1 = cxy2(4) + cxy2(5)*x2 + cxy2(6)*y2
    c
    c        input:  igtyp1,grid1(6)
    c                igtyp2,grid2(6)
    c        output: cxy1(6)
    c                cxy2(6)
    c                ierror : 0 = ok, cxy1,cxy2 er beregnet
    c                         1 = umulig konvertering
    c                         2 = ukjent grid type
    c
    c-------------------------------------------------------------------
    c  DNMI/FoU  xx.xx.1990  Anstein Foss ... polarstereografisk
    c  DNMI/FoU  06.06.1996  Anstein Foss ... gridtype 1,2,3,4,5
    c-------------------------------------------------------------------*/

 extern void xyconst(int igtyp1, float *grid1, int igtyp2, float *grid2,
		     float *cxy1, float *cxy2, int *ierror);

#ifdef __cplusplus
}
#endif

#endif
