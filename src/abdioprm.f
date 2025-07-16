      subroutine abdiodeparm (parms, nparms)
c
c  Created
c  1992-12-10 hnw
c
c Sets up the parameters needed for the applied-B diode model and
c returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is sent with the basic parameters needed to rearrange and fill
c the actual parms array.
c
c  Modifications
c  2014-02-06 RBS: removed real*4 replaced with real
c
c Define passed variables
c
      real     parms(*)
      integer  nparms
c
c Define internal variables
c
      real     temp(10)
c
c Set internal parameters
c
      real       pi
      parameter (pi = 3.14159265)
      real       eps0
      parameter (eps0 = 8.8542e-12)
      real       cspeed
      parameter (cspeed = 2.9979e+8)
      real       qelectron
      parameter (qelectron = 1.602e-19)
      real       massproton
      parameter (massproton = 1.67262e-27)
      real       q2m
      parameter (q2m = qelectron / massproton)
      real       enhancemin
      parameter (enhancemin = 9.0 * pi * pi / 16.0)
      real       s0
      parameter (s0 = 1.6806)
      real       const
      parameter (const = 4.0 * eps0 / 9.0)
c
c Fill temp with parms, then rearrange parms and fill it in.
c
      do i = 1, nparms
         temp(i) = parms(i)
       end do
c
c In parms we want:
c  1: gap       (does not change)
c       = diode gap
c  2: mconst    (does not change)
c       = constant for simplifying calculations
c  3: sconst     (does not change)
c       = constant for simplifying calculations
c  4: iclconst        (does not change)
c       = constant for simplifying calculations
c  5: vt4
c       = voltage at the time step t=i-2
c  6: gt5
c       = conductance at the time step t=i-3/2
c  7: x0       (does not change)
c       = distance from cathode corner to gas cell foil
c  8: gmax        (does not change)
c       = maximum possible conductance of diode
c  9: gmin        (does not change)
c       = minimum possible conductance of diode
c 10: vstar        (does not change)
c       = voltage at which current diverges theoretically
c 11: ionfrac      (does not change)
c       = ion current as a fraction of the total current
c 12: derivt6
c       = the value of gt5-gt7
c 13: ratemax      (does not change)
c       = maximum growth rate for the conductance per time step
c
c In temp:
c  1: gap
c  2: area
c  3: units of electron charge on ion (basically an integer)
c  4: amu of ion (basically an integer)
c  5. b0
c  6. x0
c  7. gmax
c  8. gmin
c  9. ionfrac
c 10. ratemax
c
c     total number of parameters for model
      nparms    = 13
c
c   --gap
      parms(1)  = temp(1)
c
c   --mconst
      parms(2)  = enhancemin / ((temp(6)+ temp(1)) **2)
c
c   --sconst
      parms(3)  = cspeed * temp(5) * temp(1)
c
c   --iclconst
      parms(4)  = const * temp(2) * sqrt(2.0*q2m*temp(3)/temp(4))
     &            / (temp(1)**2)
c
c   --vt4
      parms(5)  = 0.0
c
c   --gt5
      parms(6)  = temp(8)
c
c   --x0
      parms(7)  = temp(6)
c
c   --gmax
      parms(8)  = temp(7)
c
c   --gmin
      parms(9)  = temp(8)
c
c   --vstar
      parms(10) = parms(3) / s0
c
c   --ionfrac
      parms(11) = temp(9)
c
c   --derivt6
      parms(12) = 0.0
c
c   --ratemax
      parms(13) = temp(10)
c
      return
      end
