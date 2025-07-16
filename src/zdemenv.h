c----------------------------------------------------------------------
c    @(#)zdemenv.h   version 1.2   created 02/16/99 09:46:39
c    Last modified:    4-Jan-1999 16:07:34   klf 
c
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This is the common block for those variables which are determined
c   by the device dependent routine getenvvr.f
c
c  Modifications:
c    08/12/97, MLK, Changed inpfile variable to allow 100 characters
c                   Added base_filename variable to allow storage of
c                   filename without the extension for DOS (otherwise
c                   it is the full filename)
c    06/09/95, MLK, Commented out use of sub1-sub4, zfile, date_time
c                   uname, host, device
c 2008-07-16 RBS: Cleaned up entire file during gFortran conversion
c 2015-03-26 RBS: Changed char*100 to char*80 to match other places
c 2015-03-31 RBS: Removed inpfile as it is only used in getenvvr.f and
c                 zdemenv.h is included in other places. It was not used
c                 in common.
c
c---------------------------------------------------------------------
c
      character*80  base_filename
      character*30  scr_parmfile
      common /env_var/ base_filename, scr_parmfile
