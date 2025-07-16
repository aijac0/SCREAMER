      subroutine read_outfile (iunit, newrec, time_flag, ipoint,
     +                         ibufsize, ttime, value, ierr)
c
c-----Description-------------------------------------------------------
c
c Purpose: This subroutine reads one record from the SCREAMER output
c          parameter file. Based on the time flag (timeflag) and 
c          the pointer to the appropriate parameter (ipoint), the
c          appropriate ttime and value are returned to the calling
c          routine.
c Author:  K.L.Fugelso, 1265 (SEA), 12/89
c
c Calls:     None
c Called by: FILVALS, IDRVALS, PLTVALS, PRTVALS, TABVALS, UFOVALS
c
c Modified 3/7/95, Mark Kiefer, change if test on newrec to be
c                               F77 compliant
c 2014-02-06 RBS: Changed real*4 to real
c 2015-06-23 RBS: Fixed typo on passed time_flag,was timeflag. This did
c                 matter as it was passed but once half_step and
c                 whole_step were removed from zdemparm it failed
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
c-----Include Files-----------------------------------------------------
c
      use zdemmax
      include 'zdemparm.h'
c
c-----Input Parameters--------------------------------------------------
c
      integer iunit,     ! Logical unit number of SCREAMER param file
     +        ipoint,    ! Pointer to specific parameter
     +        ibufsize   ! Size of record to read (# of words)
      integer newrec
c
c ***** Time flag parameters ******
c

      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)

c
c-----Output Parameters-------------------------------------------------
c
      real    ttime,     ! Time value of record read
     +        value      ! Value of parameter specified by ipoint
      integer ierr       ! Error flag
c
c-----Local Variables---------------------------------------------------
c
      real       buffer(maxout*2+2)
c
c-----Subroutine Body---------------------------------------------------
c
      if (newrec .ne. 0) then
         read (iunit,iostat=ierr) (buffer(i),i=1,ibufsize)
      endif

      if (ierr .eq. 0) then
         if (time_flag .eq. half_step) then
            ttime = buffer(1)
            value = buffer(ipoint*2+1)
         elseif (time_flag .eq. whole_step) then
            ttime = buffer(2)
            value = buffer(ipoint*2+2)
         endif
      endif
c
c-----Return to Calling Routine-----------------------------------------
c
      return
      end
