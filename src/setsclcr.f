      subroutine set_sclcurr (time, volt, index, current)
c
c  Change log
c
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-02 RBS: Changed integer*4 to integer
c
c
      use zdemmax
      include 'zdemparm.h'
      include 'zdemcomm.h'
      include 'zdemwork.h'
c
c Define passed variables
c
      real       time, volt, current
      integer    index
c
c SCL current source as a function of time.
c
c Use the table lookup routine to find the current, given the delayed time and
c voltage produced.
c Scale the voltage (input) down since the stored function is scaled down,
c then scale the resulting current up since it is stored scaled down.
c
      scale    = currf_parms(1,index)
      rvscale  = currf_parms(2,index)
      delay    = currf_parms(3,index)
      npts     = num_currf_parms(index) - 3
      current  = ftabcond (time-delay, volt*rvscale, npts, 
     &                     currf_parms(4,index), lastcurrf_time(index))
      current  = current * scale
c
      return
      end
