      subroutine banner_start_run (ifile,clen,bgin)
c
c-----Description-------------------------------------------------------
c
c Author/Date: Mathias Bavay  11/04
c      Modifications:
c
c 2008-07-16 RBS gFortran fix, remove Time, leave Date_and_time
c 2008-11-07 RBS Split off Banner_start from start_run.f
c 2008-11-07 RBS Write & include clean up
c 2012-04-11 RBS: Fixed bgin and start time issues
c 2015-12-28 RBS: Extended bgin to include hours for long runs
c 2019-02-06 RBS: Added ifile, clen as passed variables
c                 Added writes for computer name and user name
c                 Added write for input file name
c                 Added write for current working directory
c ----------------------------------------------------------------------
c
c Purpose: This subroutine prints a banner at the begining of a run.
c     
c Called by: Program ZDEM
c
c Calls:  Subroutines DATE_AND_TIME
c
c Define passed variables
c
      real bgin
      integer clen
      character*(*) ifile
c
c Include files
c
      include 'version.h'
c
c Define internal variables
c
      character   date*8
      character   time*10
      character   zone*5
      character*120 host_name, login_name, path
      integer     status
      integer     values(8)
c
c ----------------------------------------------------------------------
c Write out current version number and date and time to the screen
c
      write (6,'(/A,A,A)') ' *****  ',screamer_version,'  *****'
c
c  Get the current date and time
c
      call date_and_time(date,time,zone,values)
c
      write (6,'(A,i2.2,A,i2.2,A,i2.2,A,i3.3,A)')
     & ' *****  The current time is: '
     & ,values(5),':',values(6),':',values(7),':',values(8), '  *****'
      write (6,'(A,i2.2,A,i2.2,A,i4.4,A)')
     & ' *****  The current date is: ',
     & values(2),'/',values(3),'/',values(1),'  *****'
      bgin = values(5)*3600 + values(6)*60 + values(7) + values(8)*1e-3

      CALL HostNm(host_name, status)
      CALL GetLog(login_name)
      Call getcwd(path)
      write (6,'(/A,A)') '   Host computer = ', trim(host_name)
      write (6,'(A,A)') '   User name = ', trim(login_name)
      write (6,'(A/,A)') '   Current Working Directory = ', trim(path)

      write (6,'(/A,A)') '   Screamer input file name = ', trim(ifile)
      return
      end
