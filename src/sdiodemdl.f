      subroutine ssdiode_model
c Read-only arguments
     & (time, voltage,
c Read and write arguments
     &  var_parms,
c Write-only arguments
     &  conductance)
c
c ----------------------------------------------------------------------------
c
c   December 10, 1992;     hnw
c  2014-02-06 RBS: Changed real*4 to real
c
c   Diode model of S.A.Slutz with gap closure and setting resistance to an 
c   open circuit value for negative voltage.
c   time is the problem time at the half-step,
c   voltage is the voltage across the diode at the last time step.
c    (the voltage actually used the the voltage at the last half-step).
c   conductance is the returned value of the diode conductance (1/R).
c
c ----------------------------------------------------------------------------
c
c Define passed variables
c
      real       time
      real       voltage
      real       var_parms(*)
      real       conductance
c
c Define internal variables
c
      real       voltlast, volthalf, tdelay, gmin, gmax, area, gap,
     &           gapmin, velocity, pmass_ratio, effgap, effgap2
      real       p
      parameter (p = 5.5e-8)
      real       condvneg
      parameter (condvneg = 1.0e-6)
c
c ----------------------------------------------------------------------------
c
c Parameters used in this diode model:
c   tdelay is the time delay before the gap begins to close.
c   gmin is the minimum conductance the diode may obtain (starts with this
c     value typically).
c   gmax is the maximum conductance the diode may obtain (ends with this value
c     typically).
c   area is the effective diode area (include enhancement factors here).
c   gap is the initial diode gap (before closure).
c   velocity is the gap closure velocity.
c   gapmin is the minimum allowable effective gap.
c   pmass_ratio is the ratio of the proton mass to the subject mass
c   voltlast is the voltage at the last time step.
c
c Enhancement factors are entered by multiplying the area value (user does it).
c
c ----------------------------------------------------------------------------
c
c If the voltage is negative, set the conductance for an open circuit
c (so it really is a diode).
c
      voltlast = var_parms(9)
      volthalf = 0.5 * (voltlast + voltage)
c
      if (volthalf .lt. 0.0) then
        conductance = condvneg
c
c If the voltage is nonnegative, set the gap and find conductance.
c
      else
        tdelay = var_parms(1)
        gmin   = var_parms(2)
c
c If the gap has not started to close yet, set the diode conductance to
c its minimum value (largest resistance with a positive voltage).
c
        if (time .le. tdelay) then
          conductance = gmin
c
c Otherwise, find the effective gap.  If it is too small, set it
c to its minimum value.  Then calculate the conductance
c and check to see if it is in the bounds of gmin and gmax.
c
        else
          gmax        = var_parms(3)
          area        = var_parms(4)
          gap         = var_parms(5)
          velocity    = var_parms(6)
          gapmin      = var_parms(7)
          pmass_ratio = var_parms(8)
c
          effgap   = gap  -  velocity * (time-tdelay)
          effgap   = amax1 (effgap, gapmin)
c
          effgap2    = effgap * effgap
          conductance = (p * area * sqrt(volthalf)) / effgap2
          conductance = conductance * sqrt(pmass_ratio)
          if (conductance .gt. gmax) then
            conductance = gmax
          else if (conductance .lt. gmin) then
            conductance = gmin
          end if
c
        end if
c
      end if
c
c Reset the voltage at the last time step.
c
      var_parms(9) = voltage
c
c -----------------------------------------------------------------------------
c
      return
      end
