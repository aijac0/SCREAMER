      subroutine banner_end_run (ifile,clen,bgin)
c
c-----Description-------------------------------------------------------
c
c Author/Date: Mathias Bavay  11/04
c      Modifications:
c
c   2008-11-07 RBS: Cleaned up writes and variable definitions
c              Now log file now records date and time correctly
c   2012-04-11 RBS: Fixed Run time and cleaned up routine.
c   2014-02-06 RBS: Changed real*4 to real
c   2014-05-02 RBS: Explicitly defined bgin as real
c   2015-12-28 RBS: Increased final to include hours for longer runs
c
c ----------------------------------------------------------------------
c
c Purpose: This subroutines prints a banner at the end of a run. 
c     
c Called by: Program ZDEM
c
c Calls:  Subroutine BANNER, TIME, DATE_AND_TIME
c
c Define passed variables
c
      character*(*) ifile
      integer clen
      real bgin
c
c Include files
c
      include 'version.h'
c
c Define internal variables
c
      real        final
      character   endln*80
      character   date*8
      character   time*10
      character   zone*5
      integer     values(8)
c
c ----------------------------------------------------------------------
c
      endln = 'End of Simulation'
c
c  Set delta time in real seconds (final) knowing initial time bgin
c
      call date_and_time(date,time,zone,values)
      final = (values(5)*60 + values(6))*60 + values(7) + values(8)*1e-3
     &         - bgin
      write (6,'(A,f10.3,A)') ' Run time:',final,' seconds'
c
c  Finish up with summary
c
      call banner(endln)
c
      write(9,'(/A,A)') 'The SCREAMER input file used: ',ifile(1:clen)
      write(9,'(A,i2.2,A,i2.2)')
     & ' Current time: ',values(5),':',values(6)
c      write (6,'(A,i2.2,A,i2.2,A,i4)')
c     & ' Current date: ',values(2),'/',values(3),'/',values(1)
       write(9,'(A,f8.3,A)')
     & ' Run time: ',final,' seconds'
      write(9,'(A)') screamer_version
      close(unit=9)

      write(6,'(A/)') ' Done'

      return
      end
