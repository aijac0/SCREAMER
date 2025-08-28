!----------------------------------------------------------------------
!    @(#)zdemenv.f90   version 1.2   created 02/16/99 09:46:39
!    Last modified:    4-Jan-1999 16:07:34   klf 
!
!----------------------------------------------------------------------
!----------------------------------------------------------------------
!
!  This is the common block for those variables which are determined
!   by the device dependent routine getenvvr.f
!
!  Modifications:
!    08/12/97, MLK, Changed inpfile variable to allow 100 characters
!                   Added base_filename variable to allow storage of
!                   filename without the extension for DOS (otherwise
!                   it is the full filename)
!    06/09/95, MLK, Commented out use of sub1-sub4, zfile, date_time
!                   uname, host, device
! 2008-07-16 RBS: Cleaned up entire file during gFortran conversion
! 2015-03-26 RBS: Changed char*100 to char*80 to match other places
! 2015-03-31 RBS: Removed inpfile as it is only used in getenvvr.f and
!                 zdemenv.h is included in other places. It was not used
!                 in common.
! 2025-08-28 AJC: Moved file into a module
!
!---------------------------------------------------------------------
!

module zdemenv

      character*80 :: base_filename
      character*30 :: scr_parmfile

end module zdemenv