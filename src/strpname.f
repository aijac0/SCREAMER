      subroutine strip_name (text, name, start)
c
c Define passed variables
c
      character  text*(*),name*(*)
      integer start
c
c Define internal variables
c
      character  period*1
      parameter (period = '.')
      parameter (no_text = 0)
c
c Finds the period in a file name.
c If all blanks and commas, or if no text passed, START=END=0.
c
      lentext = len(text)
c
c Strip left blanks and commas.
c
      start = 1
      do i = 1,lentext
        if (text(i:i) .eq. period) start = i-1
      end do
c
c Return if no periods
c
      if  (start .ge. lentext) then
        name = text
        return
      end if
c
c Strip off text right of period
c
      name = text(1:start)

      return
      end
