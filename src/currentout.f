      function   fiout (inod, ibrn, time_flag)
c
c Needed to calculate ir2, il2, ql2
c
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
c Calculates the current out of a block.
c
c
c Include files
c
      use zdemmax
      use zdemwork
      use zdemparm
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
c Define internal variables
c
c     NONE
c
c    *If (time on the half step was requested)
      if (time_flag .eq. half_step) then
c
c       *Average time from last whole step and time from this whole step
         fiout = 0.5 * (zir(inod,ibrn) + zirn(inod,ibrn))
c
c    *Else (time on the whole step was requested)
      else
c
c       *Return time on the current whole step
         fiout = zirn(inod,ibrn)
c
      endif
c
      return
      end
