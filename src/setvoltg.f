      subroutine set_voltage (time, index, voltage)
c
c  Change log
c
c  2014-02-06 RBS: Changed real*4 to real
c  2014-05-04 RBS: Changed integer*4 to integer
c
c Voltage source as a function of time.
c
c
c Include files
c
      use zdemmax
      use zdemwork
      include 'zdemparm.h'
      include 'zdemcomm.h'

c
c Define passed variables
c
      real       time, voltage
      integer    index
c
c Sort out the function describing the source.
c
      ifunction = ivoltf(index)
      if (ifunction .eq. sinsquared) then
        voltage = fsinsquared (time, voltf_parms(1,index),
     &                               voltf_parms(2,index),
     &                               voltf_parms(3,index))
      else if (ifunction .eq. sinfun) then
        voltage = fsinfun (time, voltf_parms(1,index),
     &                           voltf_parms(2,index),
     &                           voltf_parms(3,index))
      else if (ifunction .eq. leastsquares) then
        voltage = fleastsquares (time, num_voltf_parms(index),
     &                           voltf_parms(1,index))
      else if (ifunction .eq. piecewiselinear) then
        voltage = fpiecewiselinear (time, num_voltf_parms(index),
     &               voltf_parms(1,index), lastvoltf_time(index))
      else if (ifunction .eq. table) then
        scale   = voltf_parms(1,index)
        delay   = voltf_parms(2,index)
        num_tpoints = num_voltf_parms(index) - 2
        voltage = fpiecewiselinear (time-delay, num_tpoints,
     &               voltf_parms(3,index), lastvoltf_time(index))
        voltage = voltage * scale
      end if
c
      return
      end
