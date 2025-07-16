      subroutine close_outfile (iunit,ierr)
c
c-----Description-------------------------------------------------------
c
c Purpose: This subroutine closes the SCREAMER output parameter file
c Author:  K.L.Fugelso, 1265 (SEA), 12/89
c
c Calls:     None
c Called by: zdem, csvvals, filvals, pffvals, sfcvals, tabvals,
c            ufovals
c
c-----Define Passed variables-------------------------------------------
c
      integer iunit   ! Logical unit # of SCREAMER output parameter file
      integer ierr    ! Condition returned from the FORTRAN CLOSE
c
c-----Subroutine Body---------------------------------------------------
c
c  Close the file and return the iostat condition
c
      close (iunit,iostat=ierr)
c
c-----Return to Calling Routine-----------------------------------------
c
      return
      end
