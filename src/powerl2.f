      function   fpl2 (inod, ibrn, time_flag)
c
c Used to calculate pl2 and el2
c
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
      use zdemmax
      include 'zdemparm.h'
      include 'zdemwork.h'
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
c Calculates the 0.5(dL/dt)I**2 power put into an inductor.
c
      curr = fiout (inod,ibrn,time_flag)
      fpl2 = 0.5 * zlrdot(inod,ibrn) * curr * curr
c
      return
      end

