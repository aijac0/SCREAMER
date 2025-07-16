      subroutine banner (line)
c
c Define passed variables
c
      character*(*)  line
c
c
c Puts out a banner in log file with the input line written 3 times.
c
c ----------------------------------------------------------------------
c Modifications:
c   06/27/95, MLK, Modified to just print input line
c                  with a lines of stars above and below it
c                  and blank lines above and below those.
c 2008-11-07, RBS, Cleaned up write statements for simplicity
c ----------------------------------------------------------------------
c
      character  star20*20, star80*80
c
      star20 = '********************'
      star80 = star20//star20//star20//star20
c
c Write 5 lines (blank, stars, input, stars, blank).
c
      write(9,'(A)') ' '
      write(9,'(A)') star80
      write(9,'(A)') line
      write(9,'(A)') star80
      write(9,'(A)') ' '
c
      return
      end
