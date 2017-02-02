c-----------------------------------------------------------------------
c snapfil.inc
c
c..include file  -  common for input felt files
c
c
c..nhfmin: min. step in hours between field input
c..nhfmax: max. step in hours between field input
c..nfilef: no. of input FELT files
c..navail: no. of available timesteps with data, plus 'other' files
c..iavail: unsorted list of timesteps with data as defined in fileInfo
c        plus files with 'other' data
c..kavail: pointers etc. to lists in iavail:
c          kavail(1): pointer to first forward  sorted timestep
c          kavail(2): pointer to first backward sorted timestep
c  navailt1 : data used for model level fieldset 1 (u1,v1,t1,...)
c  navailt2 : data used for model level fieldset 2 (u2,v2,t2,...)
c..itimer: time range, first and last possible input time
c..limfcf: limitation of used forecast lengths in each file
c             limfcf(1,n): min forecast length in hours
c             limfcf(2,n): max forecast length in hours
c..filef:  file names
c..nctype: type of meteorology, used for different nc-inputs (emep,hirlam12)
c
      integer      nhfmin,nhfmax,nfilef,navail,navailt1,navailt2
      integer      kavail(2)
      TYPE(fileInfo) ::  iavail(mavail)
      integer      itimer(5,2),limfcf(2,mfilef)
c fdata(maxsiz), idata(ldata)
      real, pointer ::  fdata(:)
      integer*2, pointer :: idata(:)
      character*1024 filef(mfilef)
      character*72 nctype
      CHARACTER(LEN=19)  :: simulation_start
c
      common/cfiles/nhfmin,nhfmax,nfilef,navail
     -             ,iavail,kavail,navailt1,navailt2
     -             ,itimer,limfcf
     -             ,fdata,idata
     -             ,filef,nctype,simulation_start
c-----------------------------------------------------------------------
