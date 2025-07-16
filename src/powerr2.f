      function   fpr2 (inod, ibrn, time_flag)
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
c Declare passed variables
c
      integer  inod, ibrn

c
c Declare time flag parameters
c

      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)

c
c Calculates the power dissipated in R2 (series).
c
      include 'zdemparm.h'
      include 'zdemmax.h'
      include 'zdemwork.h'
c
c    *If (time on the half step was requested)
      if (time_flag .eq. half_step) then
c
c       *Average value from last whole step and value from this whole step
         curr = 0.5 * (zir(inod,ibrn) + zirn(inod,ibrn))
c
c    *Else (time on the whole step was requested)
      else
c
c       *Return value on the current whole step
        curr = zirn(inod,ibrn)
c
      endif
c
      fpr2 = (curr * curr) * rr(inod,ibrn)
c
      return
      end
