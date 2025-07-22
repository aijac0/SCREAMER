      function   fiin (inod, ibrn, time_flag)
c
c Need to calculate the current into any block
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
c Calculates the current into a block.
c
      use zdemmax
      use zdemwork
      include 'zdemparm.h'

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
      integer jnod
c
c Go back a node unless it is the first one.
c
      if (inod .eq. 1) then
        jnod = 1
      else
        jnod = inod - 1
      end if
c
c    *If (time on the half step was requested)
      if (time_flag .eq. half_step) then
c
c       *Average value from last whole step and value from this whole step
         fiin = 0.5 * (zir(jnod,ibrn) + zirn(jnod,ibrn))
c
c    *Else (time on the whole step was requested)
      else
c
c       *Return value on the current whole step
         fiin = zirn(jnod,ibrn)
c
      endif
c
      return
      end
