      subroutine strip_blanks_commas (text, start, end)
c **********************************************************************
c     This subroutine removes leading and trailing spaces and commas
c  Modifications:
c 2012-12-11 RBS: text*(*) deprecated in F95
c 2014-05-01 RBS: integer*4 to integer
c
c **********************************************************************
c
c Define passed variables
c
      integer    start, end
      character  text*(*)
c
c Define internal variables
c
      character  blank*1,     comma*1
      parameter (blank = ' ', comma = ',')
      parameter (no_text = 0)
c
c Strips off left- and right-most blanks and commas in TEXT by returning
c the position of the first (START) and last (END) non-blank and non_comma
c characters. If all blanks and commas, or if no text passed, START=END=0.

      lentext = len(text)
c
c Strip left blanks and commas.
c
      start = 1
      do while (((text(start:start) .eq. blank)
     &   .or .   (text(start:start) .eq. comma))
     &   .and.  (start .le. lentext))
c         print '(a,i3)', ' start= ', start
        start = start + 1
      end do
c
c Return if all blanks and commas.
c
      if  (start .gt. lentext) then
        start = no_text
        end   = no_text
        return
      end if
c
c Strip right blanks and commas.
c
      end = lentext
      do while ((text(end:end) .eq. blank)
     &    .or.  (text(end:end) .eq. comma))
        end = end - 1
      end do

      return
      end
