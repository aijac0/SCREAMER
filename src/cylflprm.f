      subroutine cylfoilparm (parms, nparms)
c
c  December 10, 1992;     hnw
c  2014-02-06 RBS: Changed real*4 to real
c
c Include files
c Include the common block for plotting so that we can set some
c initial plotting values.
c
      use zdemmax
      include   'zdemout.h'
c
c Define passed variables
c
      real       parms(*)
      integer    nparms
c
c
c Sets up the parameters needed for the foil implosion model and
c returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is sent with the basic parameters needed to rearrange and fill
c the actual parms array.
c
c Define internal variables
c
      real       temp(4)
      real       mu
      parameter (mu = 2.0e-7)
c
c Fill temp with parms, then rearrange parms and fill it in.
c
      do i = 1, nparms
        temp(i) = parms(i)
      end do
c **********************************************************************
c In parms we want:
c  1: initrad       (does not change)
c       = initial foil radius
c  2: minrad        (does not change)
c       = minimum foil radius
c  3: mass          (does not change)
c       = foil mass
c  4: aconst        (does not change)
c       = -(mu * length) / (2 * mass)
c  5: lconst        (does not change)
c       = mu * length
c  6: lminrad       (does not change)
c       = lconst * ln(initrad/minrad)
c  7: lt3           (initially = 0)
c       = inductance from last half time step
c  8: radt3         (initially = initrad)
c       = radius from last half time step
c  9: velt3         (initially = 0)
c       = velocity from last half time step
c 10: testimpl      (initially = 0, which is imploding, set to 1 when
c                    radius shrinks to minrad)
c       = value to test to see if foil is still imploding or has
c         stagnated at the minimum radius
c
c In temp:
c 1: initrad
c 2: length
c 3: mass
c 4: minrad
c **********************************************************************
c
      nparms    = 13
      parms(1)  = temp(1)
      parms(2)  = temp(4)
      parms(3)  = temp(3)
      parms(4)  = - (mu * temp(2)) / (2.0 * temp(3))
      parms(5)  = mu * temp(2)
      parms(6)  = (mu * temp(2))  *  log(temp(1)/temp(4))
      parms(7)  = 0.0
      parms(8)  = temp(1)
      parms(9)  = 0.0
      parms(10) = 0.0
      parms(11) = temp(2)       !foil length in m
      parms(12) = temp(3)*1.0e3      !mass in grams
      parms(13) = 0.0           !initial liner velocity
c
c Set initial plotting values for foil radius, velocity,
c acceleration, kinetic energy.
c
      foilrad   = parms(1)
      foilvel   = 0.0
      foilacc   = 0.0
      foilke    = 0.0
c
      return
      end
