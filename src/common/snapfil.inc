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
c..iavail: unsorted list of timesteps with data:
c..        plus files with 'other' data
c          iavail( 1,n): year    )
c          iavail( 2,n): month   ) Time of analysis
c          iavail( 3,n): day     ) (not valid time of forecast)
c          iavail( 4,n): hour    )
c          iavail( 5,n): forecast hour
c          iavail( 6,n): file no. (in filename array)
c          iavail( 7,n): 1=model levels 2=surface data 3=both
c          iavail( 8,n): offset in hours from first (sorted) timestep
c          iavail( 9,n): pointer to next forward  (time) data
c          iavail(10,n): pointer to next backward (time) data
c                    n=1,navail
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
      integer      iavail(10,mavail),kavail(2)
      integer      itimer(5,2),limfcf(2,mfilef)
c fdata(maxsiz), idata(ldata)
      real, pointer ::  fdata(:)
      integer*2, pointer :: idata(:)
      character*1024 filef(mfilef)
      character*72 nctype
c
      common/cfiles/nhfmin,nhfmax,nfilef,navail
     -             ,iavail,kavail,navailt1,navailt2
     -             ,itimer,limfcf
     -             ,fdata,idata
     -             ,filef,nctype
c-----------------------------------------------------------------------
