      subroutine open_outfile (iunit, status, ierr)
c
c-----Description------------------------------------------------------- 
c 
c Purpose: This subroutine opens the SCREAMER output parameter file. If
c          the calling routine specifies that this is a new file 
c          (status=1), the file is opened as new; if the calling routine
c          specified that this is an existing file (status=2), the
c          file is opened as old.
c Author:  K.L.Fugelso, 1265 (SEA), 12/89
c
c Calls:     None
c Called by: ZDEM$MAIN, FILVALS, IDRVALS, PLTVALS, PRTVALS, 
c            TABVALS, UFOVALS
c
c Modifications:
c   08/12/97, KWS, modified open statement for DOS-Leahy compiler so that
c                  existing file (from a previous run) will be overwritten.
c                  This caused this module to become machine-dependent.
c   03/31/98, MLK, Replaced all UNIX system dependencies and replaced with
c                  generic UNIX
c  2014-02-06 RBS: Changed real*4 to real
c
c-----Include Files-----------------------------------------------------
c
      use zdemenv
c
c-----Input Parameters--------------------------------------------------
c
      integer iunit,  ! Logical unit # of SCREAMER output param file
     +        status  ! Val. of 1 indicates new file; 2, old file
c
c-----Output Parameters-------------------------------------------------
c
      integer ierr    ! Error flag
c
c-----Local Variables---------------------------------------------------
c
      integer new,    ! Parameter corresponding to status of new file
     +        old     ! Parameter corresponding to status of old file
      real   dummy    ! Dummy variable used to read 1st 2 recs in file
      parameter (new=1, old=2)
c
c-----Subroutine Body---------------------------------------------------
c
      if (status .eq. new) then
c Why repeat the open here? if there is no existing file then the first
c open creates a new file with that file name. If a file with that name
c exists then the open fails. With replace, an existing file with that
c file name is deleted and a new file created. No error is possible.
c
         open (unit=iunit,file=scr_parmfile,form='unformatted',
     +         status='new', iostat=ierr)
         open (unit=iunit,file=scr_parmfile,form='unformatted',
     +         status='replace', iostat=ierr)
      elseif (status .eq. old) then
         open (unit=iunit,file=scr_parmfile,form='unformatted',
     +         status='old',iostat=ierr)
         read (iunit,iostat=ierr) dummy
         read (iunit,iostat=ierr) dummy
c
      endif
c
c-----Return to Calling Routine-----------------------------------------
c
      return
      end
