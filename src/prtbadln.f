      subroutine print_bad_line (currline, nlines, numerr)
c
c define passed variables
c
      character  currline*(*)
      integer    nlines, numerr
c
c Echoes an input line if there was an error detected in it.
c
      numerr = numerr + 1
      write(9,6) nlines, currline
    6 format ('0', '### ERROR  ### in line ', i3, '. \\', a80)

      return
      end
