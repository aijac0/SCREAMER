      subroutine get_next_line
     &           (currline, currline_lc, field, nlines, eofflg,
     &            max_fields)
c
c **********************************************************************
c     This subroutine reads in a line from a SCREAMER data file
c 2008-12-11 RBS: read format for currline was 80 not 120
c 2014-05-01 RBS: Changed integer*4 to integer
c 2014-05-01 RBS: field vector now passes length and size
c 2014-05-01 RBS: currline length now passed
c 2014-05-07 RBS: All internal variables explicitly defined
c 2017-02-24 RBS: Added a new variable currline_lc to carry a mixed
c                 upper and lower case version of currline.
c **********************************************************************
c
c     Max_fields defined here sets the maximum distinguishable fields
c     per line of input text.
c
c Define passed variables
c
      character*(*) currline, currline_lc
      character*(*) field(*)
      integer  nlines, eofflg, max_fields
c
c Get the next line in the file.
c Here all inputs are converted to UPPER CASE.
c Convert to uppercase, change tabs to blanks.
c If the first character is a "!", then it is a comment line.
c If it is not a comment line or a blank line, then 
c separate it into fields and return, else get another line.
c
c Define internal variables
c
      integer    error,       noerr
      parameter (error   = 1, noerr=0)
      integer    notext
      parameter (notext  = 0)
      integer    lunit
      parameter (lunit   = 4)
c
c  100 format (a80)
c
  200 continue
      read (lunit, '(A120)', end=1000) currline
      nlines = nlines + 1
      currline_lc = currline
c      write(6,*)'currline = ',currline
c      write(6,*)'currline_lc = ',currline_lc
c      write(6,*)

      call tab2blnk (currline_lc)
      call strip_blanks_commas (currline_lc, istart, iend)

      call conv_to_ucase (currline)
      call tab2blnk (currline)
      call strip_blanks_commas (currline, istart, iend)
      if (istart .ne. notext) then
        if (currline(istart:istart) .ne. '!') then
          call get_field (currline(istart:iend), field, max_fields)
          eofflg = noerr
          return
        end if
      end if
      go to 200
c
 1000 continue
      eofflg = error
      return
c
      end
