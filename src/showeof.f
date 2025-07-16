      subroutine show_end_of_file (nlines, numerr)
c
c Prints a message telling that an end of file was found when more input
c was expected.
c
c Define passed variables
c
      integer  nlines, numerr
c
    5 format ('0', 'Error, end-of-file encountered after line ', I3,
     &             '.  More input lines were expected!')
      write(9,5) nlines
      numerr = numerr + 1
c
      return
      end
