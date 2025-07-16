      subroutine get_env_var(inpfile,clen)
c
c-----Description-------------------------------------------------------
c
c Author/Date: Kelley Fugelso, 1265 (SEA)  07/89
c      Mac mods February 19, 1993;     hnw
c      More mods May 14, 1993;    kws
c      Modifications:
c        03/07/95, MLK, format statement 945 exceeded 72 characters
c        06/09/95, MLK, converted some statements back to lowercase
c                       and commented out use of sub1,sub2,sub3,sub4
c        08/21/95, MLK, Moved opening of input file to this routine
c                       and made it system dependent to allow MAC
c                       version to open a file browser. UNIX and PC
c                       versions are identical. Included format
c                       statement number 5 directly. Also, removed
c                       READONLY option from open statement -- not
c                       standard Fortran.
c        02/14/97, MLK, Added machine dependencies for MS Powerstation
c                       version
c        08/13/97, MLK, Added defining the string base_filename to
c                       store the filename without the extension for
c                       DOS (otherwise, it is the full filename). This
c                       is used to name the LOG file. Also removed
c                       MAC_ABSOFT 'pause' statement -- apparently it
c                       is not needed.
c        03/31/98, MLK, Replaced all UNIX system dependencies and
c                       replaced with generic UNIX
c  2008-12-07 RBS Error message for bad input file name fixed.
c             Clean up of legacy coments
c 2015-03-31 RBS: Removed dlfile from the character definition - unused
c 2015-03-31 RBS: Defined passed variable inpfile as character*80, will
c                 remove the definition of inpfile from zdemenv.h.
c ----------------------------------------------------------------------
c
c Purpose: This subroutines contains ALL of the device dependent
c          library calls in SCREAMER. These calls are used to obtain
c          the values of various "environmental" variables which are
c          needed by other subroutines in SCREAMER. The values are
c          stored in the common block contained in zdemenv.h. 
c     
c Called by: Program ZDEM
c
c Calls:  Subroutine CONV_TO_LCASE
c
c  Modifications:
c    
c 2012-04-10 RBS: Fixed the extra logfile text and cleaned up routine
c 2015-03-30 RBS: Removed dlfile*80 in char def. Not used.
c
c
c-----Include Files-----------------------------------------------------
c
      include 'zdemenv.h'
c
c-----Passed variables
c
      character inpfile*(*)
c
      integer clen
c
c-----Define internal Variables-----------------------------------------
c
      integer    istart
      character  templog*80, logfile*80
      character  period*1
      parameter (period = '.')

c----Variables used in C++ wrapper JAM----------------------------------
      istart=1
      iend=clen

      open (unit=4, file=inpfile(istart:iend),
     &      err=999, status='old')

      base_filename(1:iend-istart+1) = inpfile(istart:iend)
c
c Create the log file based on the input file name
c
      call strip (inpfile, i_1st, i_last)
c
c     Strip off the extension
c
      call strip_name (inpfile(i_1st:i_last),templog,lentmp)
      logfile = templog(1:lentmp)//'.log'
      open (unit= 9, file=logfile, status='UNKNOWN')
c
c  Set a name for the temporary SCREAMER output parameter file
c
      scr_parmfile='scrtmpfl'

      return
c
c Error found when opening the data file, so print a message and stop.
c
 999  continue
      write(6,'(A)')
     & ' ### GetEnvVr - Unable to open the input file, exection halted.'

      stop
      end
