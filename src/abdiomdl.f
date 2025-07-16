      subroutine sabdiode_model
c Read-only arguments
     & (vt2,
c Read and write arguments
     &  var_parms,
c Write-only arguments
     &  conductance)
c
c ----------------------------------------------------------------------------
c
c    December 10, 1992;     hnw
c
c    Applied-B Diode model of M.P.DesJarlais.
c
c   'vt2' is the voltage across the diode at the last full time step (t=i-1).
c
c   'conductance' is the returned value of the diode conductance at
c   t=i-1/2.
c     The maximum growth rate of the conductance is never allowed to exceed
c   ratemax.
c     The derivative of the conductance is never allowed to change
c   sign in one time step.  If the calculated value of the conductance would
c   entail a sign change, then the conductance is set to the value at the
c   last time step.
c
c   This model sets conductance to an open circuit value for negative voltage.
c
c ----------------------------------------------------------------------------
c
c Declare passed variables
c
      real       vt2
      real       var_parms(*)
      real       conductance
c
c Declare internal parameters/constants
c
      real       pi
      parameter (pi = 3.14159)
      real       enhancemin
      parameter (enhancemin = 9.0 * pi * pi / 16.0)
      real       s0
      parameter (s0 = 1.6806)
      real       nu
      parameter (nu = 0.6416)
      real       mu
      parameter (mu = 0.2982)
      real       condvneg
      parameter (condvneg = 1.0e-6)
      real       smallv
      parameter (smallv = 0.01)      !small voltage relative to vstar
      real       nearly1
      parameter (nearly1 = 0.99)     !voltage near, relatively, to vstar
c
c Declare internal variables
c
      real       gap       !diode gap
      real       mconst    !calculational constant
      real       sconst    !calculational constant
      real       iclconst  !calculational constant
      real       vt3       !voltage at t=i-3/2
      real       vt4       !voltage at t=i-2
      real       gt5       !conductance at t=i-3/2 (from last time step)
      real       x0        !distance from cathode corner to gas cell foil
      real       gmax      !max. conductance
      real       gmin      !min. conductance
      real       vstar     !voltage at which current diverges
      real       ionfrac   !ion current fraction of the total beam current
      real       derivt4   !the value of gt3-gt5
      real       derivt6   !the value of gt5-gt7
      real       ratemax   !the max growth rate
      real       enhance
      real       s
      real       gs
      real       rate
c
c Set model parameters to understandable names.
c
      gap      = var_parms(1)
      mconst   = var_parms(2)
      sconst   = var_parms(3)
      iclconst = var_parms(4)
      vt4      = var_parms(5)
      gt5      = var_parms(6)
      x0       = var_parms(7)
      gmax     = var_parms(8)
      gmin     = var_parms(9)
      vstar    = var_parms(10)
      ionfrac  = var_parms(11)
      derivt6  = var_parms(12)
      ratemax  = var_parms(13)
c
c ----------------------------------------------------------------------------
c
c Set vt3 to average of vt2 and vt4.  Use this to calculate conductance.
c Note that this is one time step behind.
c
      vt3 = 0.5*(vt2+vt4)
c
c If the voltage is negative, set the conductance for an open circuit
c (so it really is a diode).
c
      if (vt3 .lt. 0.0) then
        conductance = condvneg
c
c If the voltage is nonnegative and less than vstar*smallv, set the
c enhancement factor to enhancemin, then calculate conductance.
c
      else if (vt3 .lt. vstar*smallv) then
        enhance     = enhancemin
        conductance = enhance * iclconst * sqrt(vt3) / ionfrac
        if (conductance .lt. gmin) then
          conductance = gmin
        else if (conductance .gt. gmax) then
          conductance = gmax
        end if
c
c Otherwise calculate the enhancement factor normally, then calculate
c conductance.
c
      else
        s           = sconst / vt3
        gs          = (abs(s-s0))**nu
        gs          = exp(gs)
        gs          = (gs - 1.0) / (gs - mu)
        enhance     = (x0 + gs*gap) / gs
        enhance     = mconst * enhance * enhance
        conductance = enhance * iclconst * sqrt(vt3) / ionfrac
c
c .. Now limit the growth rate if necessary.
c
        rate = (conductance - gt5) / abs(0.5*(conductance + gt5))
        if (abs(rate) .gt. ratemax) then
          conductance = gt5 * (1.0 + ratemax) * sign(1.0,rate)
        end if
        write (88,*) 'growth-rate: ', rate
c
c .. Check against absolute limits
c
        if (conductance .lt. gmin) then
          conductance = gmin
        else if (conductance .gt. gmax) then
          conductance = gmax
        end if
c
      end if
c
c Now test the derivative of the conductance to see if it changed sign
c from the last time step.  If it did set it to the conductance at the
c last time step and set the derivative to zero.
c
      derivt4 = conductance - gt5
      if (derivt6 .ne. 0.0) then
        if (derivt4*derivt6 .lt. 0.0) then
          conductance = gt5
          derivt4 = 0.0
        end if
      end if
      write (88,*) 'gt3,gt5,dgt4: ',
     &              conductance, gt5, derivt4
c
c Reset any model parameters needed for the next time-step iteration.
c
      var_parms(5)  = vt2
      var_parms(6)  = conductance
      var_parms(12) = derivt4
c
c -----------------------------------------------------------------------------
c
      return
      end
