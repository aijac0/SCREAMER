      subroutine gaspuffparm (parms, nparms)
c
c Sets up the parameters needed for the gaspuff implosion model and
c returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is sent with the basic parameters needed to rearrange and fill
c the actual parms array.
c
c
c  December 10, 1992;     hnw
c  2014-02-06 RBS: Changed real*4 to real
c  2014-05-02 RBS: Changed interger*4 to integer
c
c Define passed variables
c
      real       parms(*)
      integer    nparms
c
c Include the common block for plotting so that we can set some
c initial plotting values.
c
      include   'zdemmax.h'
      include   'zdemout.h'
c
c Define internal variables
c
c Note carefully the value of mu
c
      real       temp(6)
      real       mu
      parameter (mu = 2.0e-7)
      real       pi
      parameter (pi = 3.14159)
c
c Fill temp with parms, then rearrange parms and fill it in.
c
      do i = 1, nparms
        temp(i) = parms(i)
      end do
c
c In parms we want:
c  1: initrad       (does not change)
c       = initial gas puff radius
c  2: innerrad      (does not change)
c       = gas puff radius at which we turn off drag term
c  3: minrad        (does not change)
c       = minimum gas puff radius - radius of stagnation
c  4: massri        (does not change)
c       = gas puff mass for drag term when radius=innerrad
c  5: masst3
c       = gas puff mass for drag term from last half time step
c  6: density       (does not change)
c       = gas puff density
c  7: aconst        (does not change)
c       = -(mu * length) / 2
c  8: dconst        (does not change)
c       = 2 * pi * length * density 
c  9: lconst        (does not change)
c       = mu * length
c 10: mconst        (does not change)
c       = pi * length * density
c 11: lminrad       (does not change)
c       = lconst * ln(initrad/minrad)
c 12: lt3           (initially = 0)
c       = inductance from last half time step
c 13: radt3         (initially = initrad)
c       = radius from last half time step
c 14: velt3         (initially = 0)
c       = velocity from last half time step
c 15: testimpl      (initially = 0, which is imploding, set to 1 when radius
c                                   shrinks to minrad)
c       = value to test to see if foil is still imploding or has stagnated at
c         the minimum radius
c 16: initmass     (does not change)
c       = initial mass term
c
c In temp:
c 1: initrad
c 2: length
c 3: density
c 4: minrad
c 5. innerrad
c 6. initmass
c
      nparms    = 16
      parms(1)  = temp(1)
      parms(2)  = temp(5)
      parms(3)  = temp(4)
      parms(4)  = pi * temp(2) * temp(3)
     &               * (temp(1)*temp(1) - temp(5)*temp(5))
      parms(5)  = temp(6)
      parms(6)  = temp(3)
      parms(7)  = - (mu * temp(2) * 0.5)
      parms(8)  = 2.0 * pi * temp(2) * temp(3)
      parms(9)  = mu * temp(2)
      parms(10) = pi * temp(2) * temp(3)
      parms(11) = mu * temp(2) * log(temp(1)/temp(4))
      parms(12) = 0.0      !lt3=initial inductance
      parms(13) = temp(1)  !radt3=initrad
      parms(14) = 0.0      !initial velocity
      parms(15) = 0.0      !set status as "imploding"
      parms(16) = temp(6)
c
c Set initial plotting values for GAS PUFF radius, velocity,
c acceleration, kinetic energy.
c
      gasrad   = parms(1)
      gasvel   = 0.0
      gasacc   = 0.0
      gaske    = 0.0
c
      return
      end
