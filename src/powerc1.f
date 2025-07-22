      function   fpc1 (inod, ibrn, time_flag)
c
c Used to calculate pc1, and pc3
c
c
c Modifications
c
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c
      use zdemmax
      use zdemwork
      include 'zdemparm.h'
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
c Calculates the 0.5(dC/dt)V**2 power put into a capacitor.
c
      volt = fvblk (inod,ibrn,time_flag)
      fpc1 = 0.5 * cdot(inod,ibrn) * volt * volt
c
      return
      end
