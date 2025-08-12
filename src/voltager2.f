      function   fvr2 (inod, ibrn, time_flag)
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
c Calculates the voltage across R2 (series).
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
      integer    inod, ibrn

c
c Declare time flag parameters
c

      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)
c
c    *If (time on the half step was requested)
      if (time_flag .eq. half_step) then
c
c       *Average value from last whole step and value from this whole step
         fvr2= 0.5 * (zir(inod,ibrn)+zirn(inod,ibrn)) * rr(inod,ibrn)
c
c    *Else (time on the whole step was requested)
      else
c
c       *Return value on the current whole step
         fvr2 = zirn(inod,ibrn) * rr(inod,ibrn)
c
      endif
c
      return
      end
