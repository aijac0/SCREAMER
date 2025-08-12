      function   fel2 (inod, ibrn, time_flag)
c
c Used to calculate el2 and pl2
c
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
      use zdemmax
      use zdemwork
      use zdemparm

c Declare passed variables
c
      integer    inod, ibrn

c
c Declare time flag parameters
c

      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)


c
c Calculates the 0.5LI**2 energy stored in an inductor.
c
c
      curr = fiout (inod,ibrn, time_flag)
      fel2 = 0.5 * zlrechk(inod,ibrn) * curr * curr
c
      return
      end
