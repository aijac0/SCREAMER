      subroutine gaspuff_model
c read only arguments
     & (time, dt, rdt, ilt2,
c read and write arguments
     &  parms,
c write only arguments
     &  ldott2, lt1)
c ------------------------------------------------------------------------------
c
c Summary:
c
c Screamer gas puff implosion subroutine.
c
c   Define:
c     the next               full time step as t=i      (variable suffix t0)
c     the current            half time step as t=i-1/2  (variable suffix t1)
c     the previous           full time step as t=i-1    (variable suffix t2)
c     the previous           half time step as t=i-3/2  (variable suffix t3)
c     the previous, previous full time step as t=i-2    (variable suffix t4)
c     the previous, previous half time step as t=i-5/2  (variable suffix t5)
c
c Calculates the following values for an imploding gas puff:
c   Passed via argument list:
c     inductance (=lt1) at t=i-1/2
c     time rate of change of inductance (=ldott2) at t=i-1
c   Passed via 'zdemout.h' common blocks:
c     gas puff kinetic energy (=gaske) at t=i-1/2
c     gas puff radius (=gasrad)        at t=i-1/2
c     gas puff velocity (=gasvel)      at t=i-1/2
c     gas puff acceleration (=gasacc)  at t=i-1
c When the calculated gas puff radius becomes less than or equal to the
c minimum radius, the implosion stops and this routine sets:
c   radt1    = (gpuffrad =) minrad 
c   lt1      = mu * length * ln (initrad/minrad) (=lminrad)
c   ldott2   = 0
c   gaske    = 0
c   gasvel   = 0
c   gasacc   = 0
c
c ------------------------------------------------------------------------------
c
c Modifications:
c   MLK, 3/23/95, Moved printing of 999 format message to point where
c                 masst1 was defined. Also, fixed 999 message so that
c                 it is not misleading -- achieving min-radius does not
c                 imply max kinetic energy, nor max velocity.
c
c   MLK, 4/29/95, write(5,999) changed to write (6,999)
c                 unit 5 is stdin, unit 6 is stdout
c  2014-02-06 RBS: Changed real*4 to real
c  2014-10-22 RBS: Error noted in mass for r<innerrad, initmass not
c                  included. Fixed in line 203 masst1 = massri + initmass
c                  Likely not detected since most gas puff runs don't
c                  have a significant initmass.
c
c ------------------------------------------------------------------------------
c
c Include the file which has a common block for storing some unusual
c parameters from this model
c
      include   'zdemmax.h'   !parameters
      include   'zdemout.h'   !common blocks
c
c ------------------------------------------------------------------------------
c
c Read only passed arguments
c
      real       time     !simulation time, t=i
      real       dt       !time-step
      real       rdt      !1/dt
      real       ilt2     !current through this inductor at t=i-1
c
c Read and write passed arguments
c
      real       parms(*) !model parameters for this circuit element
c
c Write only passed arguments
c
      real       ldott2  !time rate of change of inductance at t=i-1/2
      real       lt1     !inductance at t=i-1/2
c
c ------------------------------------------------------------------------------
c
c Set some internal parameters
c
      real       implode,       end_implode
      parameter (implode = 0.0, end_implode = 1.0)
c
c Set internal variables
c
      real       initrad   !initial radius
      real       minrad    !minimum radius
      real       innerrad  !inner radius
      real       massri    !mass when radius=rinner
      real       masst3    !mass at t=i-3/2
      real       masst1    !calculated mass at t=i-1/2
      real       density   !density
      real       aconst    !-(mu*length)/2
      real       dconst    !(2*pi*length*density)
      real       lconst    !(mu*length)
      real       mconst    !(pi*length*density)
      real       lminrad   !inductance at minimum radius
      real       lt3       !inductance from t=i-3/2
      real       radt1     !calculated radius,   t=i-1/2
      real       radt3     !radius from t=i-3/2
      real       velt1     !calculated velocity, t=i-1/2
      real       velt3     !velocity from t=i-3/2
      real       accilt2   !calculated acceleration term due to current, t=i-1
      real       dragt2    !calculated acceleration term due to drag, t=i-1
      real       acct2     !calculated total acceleration term, t=i-1
      real       testimpl  !calculated current state of foil
                           ! (imploding or stagnated)
      real       initmass  !initial mass term
c
c ------------------------------------------------------------------------------
c
c Set the model parameters to understandable names.
c
      initrad   = parms(1)
      innerrad  = parms(2)
      minrad    = parms(3)
      massri    = parms(4)
      masst3    = parms(5)
      density   = parms(6)
      aconst    = parms(7)
      dconst    = parms(8)
      lconst    = parms(9)
      mconst    = parms(10)
      lminrad   = parms(11)
      lt3       = parms(12)
      radt3     = parms(13)
      velt3     = parms(14)
      testimpl  = parms(15)
      initmass  = parms(16)
c
c ------------------------------------------------------------------------------
c
c Calculate the acceleration, velocity and radius of the gas puff.
c From these, calculate L, dL/dt, d(LI)/dI.
c  Note: in this calculation mu = 2x10^-7 NOT 4pix10-7
c
c   The acceleration at t=i-1 is defined by the sum of:
c       accilt2 = P*A/masst3
c       accilt2 = - ( mu * ilt2**2 *length) / (2 * masst3 * radt3)
c        masst3 is calculated at the end of this time step
c Drag can be thought of as the force of gas moving a velocity velt3
c striking the sheath per time step.
c     dragt2 = F / m = [(Area * velt3 * density)] * velt3 / masst3
c            = [(2 * pi * length * density) * radt3 * velt3]
c                                                        * velt3/ masst3
c     dragt2 = (2 * pi * length * density) * radt3 * velt3**2  /  masst3
c
c   The time centered velocity is defined by:
c       (velt1 + velt3) / dt  = acct2
c
c   The time centered radius is defined by:
c       (radt1 - radt3) / dt = 0.5 * (velt1 + velt3)
c
c   L (lt1) is defined by:
c       (mu * length) * ln(initrad/radt1)
c
c   dL/dt (ldott2) is defined by:
c       (lt1 - lt3) / dt
c
c ------------------------------------------------------------------------------
c
c For an imploding gas puff:
c
      if (testimpl .eq. implode) then
c
c   a. Gas puff acceleration terms
c
        if (radt3 .gt. innerrad) then
          accilt2 = (ilt2 * ilt2 * aconst) / (radt3 * masst3)
          dragt2  = (dconst * radt3 * velt3 * velt3 ) / (masst3)
          acct2   = accilt2 + dragt2
        else
          accilt2 = (ilt2 * ilt2 * aconst) / (radt3 * masst3)
          acct2   = accilt2
        end if
c
c   b. New gas puff velocity
c
        velt1 = velt3 + acct2 * dt
c
c   c. New gas puff radius, check to see if less than minimum radius, if
c        so, print a message that this has occurred and set values
c        appropriately.  But keep going with the calculation since
c        the gas puff radius has changed this time step.
c
        radt1 = radt3  +  ((velt1+velt3) * dt * 0.5)
        if (radt1 .lt. minrad) then
          radt1    = minrad
          testimpl = end_implode
        end if
c
c   d. Gas puff inductance
c
        lt1 = lconst * log(initrad/radt1)
c
c   e. Time rate of change of inductance
c
        ldott2 = (lt1 - lt3) * rdt
c
c
c   f. New gas puff mass for acceleration terms.  Stop increasing it
c      if radius drops below inner radius allowed.
c
        if (radt1 .gt. innerrad) then
          masst1 = mconst * (initrad * initrad  -  radt1 * radt1)
     &             + initmass
        else
          masst1 = massri
        end if
c
c   g. Put some values into the common block so that they will be
c      available for plotting.
c
        gaske  = 0.5 * masst1 * velt1 * velt1
        gasrad = radt1
        gasvel = velt1
        gasacc = acct2
c
c Print out an informational message about reaching the min-radius
c
        if (testimpl .eq. end_implode) then
      write(9,
     &  '(/A/,A/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A/)')
     &  ' ------------------------------------------------------------',
     &  ' Foil radius has reached the minimum value.',
     &  '   time:                   ',time,
     &  '   minimum radius:         ',minrad,
     &  '   KE at min-radius:       ',gaske,
     &  '   velocity at min-radius: ',velt1,
     &  ' ------------------------------------------------------------'

      write(6,'(A)')
     &  ' ------------------------------------------------'
      write(6,'(A)')
     &  ' Gas puff radius has reached the minimum value.'
      write(6,'(A,1pe10.3)') '  time:                   ',time
      write(6,'(A,1pe10.3)') '  minimum radius:         ',minrad
      write(6,'(A,1pe10.3)') '  KE at min-radius:       ',gaske
      write(6,'(A,1pe10.3)') '  velocity at min-radius: ',velt1
      write(6,'(A)')
     &  ' ------------------------------------------------'
      end if
c
c   h. Save all values required for next time step.
c
        parms(5)  = masst1
        parms(12) = lt1
        parms(13) = radt1
        parms(14) = velt1
        parms(15) = testimpl
c
c For stagnant gas puff (constant radius and inductance):
c 
      else
c
c   d. Gas puff inductance
c
        lt1 = lminrad
c
c   e. Time rate of change of inductance
c
        ldott2 = 0.0
c
c   f. Put some values into a common block so that they will be
c      available for plotting.
c
        gaske  = 0.0
        gasrad = minrad
        gasvel = 0.0
        gasacc = 0.0
      end if
c
c ------------------------------------------------------------------------------
c
      return
      end
