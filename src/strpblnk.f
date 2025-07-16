      subroutine strip (text, start, end)
c
c Modifications:
c
c 2014-05-01 RBS: integer*4 to integer
c 2016-03-16 RBS: Cleaned up a two write format statements
c
c-----------------------------------------------------------------------
c
c Define passed variables
c
      integer    start, end
      character  text*(*)
c
c Define internal variables
c
      character  blank*1
      parameter (blank = ' ')
      parameter (no_text = 0)
c
c Strips off left- and right-most blanks in TEXT by returning
c the position of the first (START) and last (END) non-blank
c characters. If all blanks or if no text passed, START=END=0.
c

      lentext = len(text)
c
c Strip left blanks
c
      start = 1
      do while ((text(start:start) .eq. blank)
     & .and.    (start .le. lentext))
        start = start + 1
      end do
c
c Return if all blanks
c
      if  (start .gt. lentext) then
        start = no_text
        end   = no_text
       return
      end if
c
c Strip right blanks
c
      end = lentext
      do while  (text(end:end) .eq. blank)
        end = end - 1
      end do
c
c      write(6,'(A,i2,5x,A,i2,5x,A,i2)')
c     & ' substring length = ', lentext,'start = ', start, 'end = ', end
c      write(6,*)text(start:end)
c      do i=start,end
c      write(6,'(A,i2,A,i3)') '   text(', i, ') = ', ichar(text(i:i))
c      end do
c
      return
      end
