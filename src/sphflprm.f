      subroutine sphfoilparm (parms, nparms)
c ----------------------------------------------------------------------
c
c  December 10, 1992;     hnw
c  2014-02-06 RBS: Changed real*4 to real
c  2014-05-04 RBS: Changed integer*4 to integer
c
c Sets up the parameters needed for the SPHERICAL foil implosion model
c and returns them in parms(i).
c nparms is the number of parameters in parms.
c
c parms is sent with the basic parameters needed to rearrange and fill
c the actual parms array.
c
c Include the common block for plotting so that we can set some
c initial plotting values.
c
      use zdemmax
      use zdemout
c
c Define passed variables
c
      real       parms(*)
      integer    nparms
c
c ----------------------------------------------------------------------
c
      real       temp(4)
c
c ----------------------------------------------------------------------
c Fill temp with parms.
c
      do i = 1, nparms
        temp(i) = parms(i)
      end do
c
c ----------------------------------------------------------------------
c In parms we want:
c  1: initrad       (does not change)
c       = initial foil radius
c  2: angl        (does not change)
c       = foil included angle
c  3: mass          (does not change)
c       = foil mass
c  4: minrad        (does not change)
c       = minimum foil radius
c  5: lt3           (initially = 0)
c       = inductance from last half time step
c  6: radt3         (initially = initrad)
c       = radius from last half time step
c  7: velt3         (initially = 0)
c       = velocity from last half time step
c  8: testimpl      (initially = 0, which is imploding, set to 1 when
c                                   radius shrinks to minrad)
c       = value to test to see if foil is still imploding or has
c         stagnated at the minimum radius
c
c In temp:
c 1: initrad
c 2: angl
c 3: mass
c 4: minrad
c
c ----------------------------------------------------------------------
c
      nparms    = 8
      parms(1)  = temp(1)                       !  initrad
      parms(2)  = temp(2)                       !  angl
      parms(3)  = temp(3)                       !  mass
      parms(4)  = temp(4)                       !  minrad
c
      parms(5)  = 0.0                           !  lt3
      parms(6)  = temp(1)                       !  radt3
      parms(7)  = 0.0                           !  velt3
      parms(8)  = 0.0                           !  testimpl
c
c ----------------------------------------------------------------------
c Set initial plotting values for foil radius, velocity,
c acceleration, kinetic energy.
c
      foilrad   = parms(1)
      foilvel   = 0.0
      foilacc   = 0.0
      foilke    = 0.0
c
c ----------------------------------------------------------------------
c
      return
      end
