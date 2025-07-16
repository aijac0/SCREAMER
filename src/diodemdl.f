      subroutine wasdiode_model
c Read-only arguments
     & (time, voltage,
c Read and write arguments
     &  var_parms,
c Write-only arguments
     &  conductance, capacitance)
c
c ----------------------------------------------------------------------------
c
c Created 2020-09-02 RBS: New high-voltage diode model
c
c   Diode model of W. Stygar with breakdown at high reverse voltage. The
c   model will include a variable capacitance in a later version.
c   This is only an RCGround and PI-block model. This will not work as an
c   RLSeries element but can be mimicked using a top branch.
c
c ----------------------------------------------------------------------------
c
c Declare passed variables
c
      real       time
      real       voltage
      real       var_parms(*)
      real       conductance
      real       capacitance
c
c Declare internal variables
c
      real       V1, I1, V2, I2, V3, I3, V4, I4, V5, I5, V6, I6
      real       slope, yint
c
c ----------------------------------------------------------------------------
c
c Parameters used in this diode model:
c   voltage is the voltage at the last time step and will be used to define
c   the conductance (and later) the capacitance..
c
c ----------------------------------------------------------------------------
c
c first input the 6 pairs of points from var_parms
c

      V1   = var_parms(1)
      I1   = var_parms(2)
      V2   = var_parms(3)
      I2   = var_parms(4)
      V3   = var_parms(5)
      I3   = var_parms(6)
      V4   = var_parms(7)
      I4   = var_parms(8)
      V5   = var_parms(9)
      I5   = var_parms(10)
      V6   = var_parms(11)
      I6   = var_parms(12)

c
c The 6 pairs of points with 0,0 define 6 line line segments
c

c       print *, V1, I1, V2, I2, V3, I3
c       print *, V4, I4, V5, I5, V6, I6

      If ( voltage .le. V2 ) then
         slope = (I2 - I1)/(V2 - V1)
         yint  = I2 - slope * V2
         conductance = slope + yint/voltage
c         print *, slope, yint, voltage, conductance
      else if (voltage .gt. V2 .and. voltage .le. V3 ) then
         slope = (I3 - I2)/(V3 - V2)
         yint  = I3 - slope * V3
         conductance = slope + yint/voltage
      else if (voltage .gt. V3 .and. voltage .le. 0.0 ) then
         slope = (0.0 - I3)/(0.0 - V3)
         yint = 0.0
         conductance = slope
      else if (voltage .gt. 0.0 .and. voltage .le. V4 ) then
         slope = (I4 - 0.0)/(V4 - 0.0)
         yint = 0.0
         conductance = slope
      else if (voltage .gt. V4 .and. voltage .le. V5 ) then
         slope = (I5 - I4)/(V5 - V4)
         yint  = I5 - slope * V5
         conductance = slope + yint/voltage
      else
         slope = (I6 - I5)/(V6 - V5)
         yint  = I6 - slope * V6
         conductance = slope + yint/voltage
      end if

c      print *, time, voltage, slope, yint, conductance

c
c -----------------------------------------------------------------------------
c
      return
      end
