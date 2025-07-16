      subroutine dynamichohlraum_model
c read only arguments
     & (time, dt, rdt, ilt2,
c read and write arguments
     &  parms,
c write only arguments
     &  ldott2, lt1)
c ----------------------------------------------------------------------
c
c Summary:
c
c Screamer dynamic hohlraum implosion subroutine.
c
c Define:
c   the next             full time step as t=i      (variable suffix t0)
c   the current          half time step as t=i-1/2  (variable suffix t1)
c   the previous         full time step as t=i-1    (variable suffix t2)
c   the previous         half time step as t=i-3/2  (variable suffix t3)
c   the 2X previous      full time step as t=i-2    (variable suffix t4)
c   the 2X previous      half time step as t=i-5/2  (variable suffix t5)
c
c Calculates the following values for an imploding dynamic hohlraum:
c   Passed via argument list:
c     inductance (=lt1) at t=i-1/2
c     time rate of change of inductance (=ldott2) at t=i-1
c   Passed via 'zdemout.h' common blocks:
c     dynamic hohlraum kinetic energy (=gaske) at t=i-1/2
c     dynamic hohlraum radius (=gasrad)        at t=i-1/2
c     dynamic hohlraum velocity (=gasvel)      at t=i-1/2
c     dynamic hohlraum acceleration (=gasacc)  at t=i-1
c When the calculated dynamic hohlraum radius becomes less than or equal
c to the minimum radius, the implosion stops and this routine sets:
c   radt1    = (gpuffrad =) minrad 
c   lt1      = mu * length * ln (rinit/minrad) (=lminrad)
c   ldott2   = 0
c   gaske    = 0
c   gasvel   = 0
c   gasacc   = 0
c
c ----------------------------------------------------------------------
c
c Modifications:
c  2014-10-11 RBS: Created from a copy of gas puff model
c  2014-10-11 RBS: Changed order of the internal variables real list to
c                  agree with the order of the parameter list for
c                  clarity
c  2014-10-11 RBS: Add needed variables massf2, radf2, massf3, router
c  2014-10-22 RBS: Finished adding changes to the acceleration, mass
c                  accretion, and implosion flags in parms. We test for
c                  impact with liner 2 & 3 for vel change and new mass.
c                  This should be a working version. Some cleanup may be
c                  needed before final release.
c  2014-10-22 RBS: All subroutine characters are inside the 72 character
c                  limit for f77 compatibility.
c  2014-11-03 RBS: Included peakcur to the output list.
c  2015-03-30 RBS: Error found in line 252, router3 used instead of
c                  router. Error found in line 256 in which a type added
c                  an extra 'and' appended to router. Fixed.
c
c ----------------------------------------------------------------------
c
c Include the file which has a common block for storing some unusual
c parameters from this model
c
      use zdemmax             !parameters
      include   'zdemout.h'   !common blocks
c
c ----------------------------------------------------------------------
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
c ----------------------------------------------------------------------
c
c Set some internal parameters
c
      real       implode,       end_implode
      parameter (implode = 0.0, end_implode = 1.0)
c
c Set internal variables
c
      real       rinit     !initial radius of outer liner
      real       rinner    !inner radius dynamic hohlraum foam
      real       router    !fixed outer radius of foam and liner3
      real       minrad    !minimum radius of implosion
      real       massri    !total mass when radius=rinner
      real       masst3    !mass at t=i-3/2
      real       density   !density of foam
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
      real       testimpl  !calculated current state of foil
                           ! (imploding or stagnated)
      real       initmass  !initial mass term of outer liner
      real       accilt2   !calculated acceleration term due to current,
                           ! t=i-1
      real       dragt2    !calculated acceleration term due to drag,
                           ! t=i-1
      real       acct2     !calculated total acceleration term, t=i-1
      real       masst1    !calculated mass at t=i-1/2
      real       rliner2   !fixed radius of second liner
      real       mliner2   !fixed mass of second liner
      real       mliner3   !fixed mass of third liner
      real       flagl2    !flag for implosion at second liner
      real       flagl3    !flag for implosion at third liner
      real       flagl3m   !flag for mass of third liner added to total
      real       peakcur   !peak liner current
c
c ----------------------------------------------------------------------
c
c Set the model parameters to understandable names.
c
      rinit     = parms(1)
      rinner    = parms(2)
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
      rliner2   = parms(17)
      mliner2   = parms(18)
      router    = parms(19)
      mliner3   = parms(20)
      flagl2    = parms(21)
      flagl3    = parms(22)
      flagl3m   = parms(23)
      peakcur   = parms(24)
c
c ----------------------------------------------------------------------
c
c Calculate the acceleration, velocity and radius of the gas puff.
c From these, calculate L, dL/dt, d(LI)/dI.
c
c   The acceleration at t=i-1 is defined by the sum of:
c      accilt2 = - (mu * ilt2**2 *length) / (2 * masst3 * radt3)
c      dragt2  = (2 * pi * length * density) * radt3 * velt3**2 / masst3
c
c   The time centered velocity is defined by:
c       (velt1 - velt3) / dt  = acct2
c
c   The time centered radius is defined by:
c       (radt1 - radt3) / dt = 0.5 * (velt1 + velt3)
c
c   L (lt1) is defined by:
c       (mu * length) * ln(rinit/radt1)
c
c   dL/dt (ldott2) is defined by:
c       (lt1 - lt3) / dt
c
c ----------------------------------------------------------------------
      if (abs(ilt2) .gt. peakcur) then
         peakcur = abs(ilt2)
         endif
c
c For an imploding dynamic hohlraum:
c
      if (testimpl .eq. implode) then
c
c   0. Shell stagnation with momentum conservation
c
        if (radt3 .le. rliner2 .and. flagl2 .eq. 0.0) then
          flagl2 = 1.0
          velt3 = velt3 * (initmass/(initmass + mliner2))
          end if
        if (radt3 .le. router .and. flagl3 .eq. 0.0) then
          flagl3 = 1.0
          velt3 = velt3 * ((initmass + mliner2)
     &                     /(initmass + mliner2 + mliner3))
          end if
c
c   a. Dynamic hohlraum acceleration terms. The drag term only shows up
c      In the time when the radius includes the foam volume.
c
        if (radt3 .gt. rliner2) then
          accilt2 = (ilt2 * ilt2 * aconst) / (radt3 * initmass)
          acct2   = accilt2
          else if (radt3 .gt. router .and. radt3 .le. rliner2) then
          accilt2 = (ilt2 * ilt2 * aconst) / (radt3 *
     &              (initmass + mliner2))
          acct2   = accilt2
          else if (radt3 .lt. router .and. radt3 .gt. rinner) then
          accilt2 = (ilt2 * ilt2 * aconst) / (radt3 * masst3)
          dragt2  = (dconst * radt3 * velt3 * velt3 ) / (masst3)
          acct2   = accilt2 + dragt2
          else
          xmass = initmass + mliner2 + mliner3 + massri
          accilt2 = (ilt2 * ilt2 * aconst) / (radt3 * xmass)
          acct2   = accilt2
          end if
c
c   b. Dynamic hohlraum velocity
c
        velt1 = velt3 + acct2*dt
c
c   c. Dynamic hohlraum radius, check to see if less than minimum
c        radius, if so, print a message that this has occurred and set
c        values appropriately.  But keep going with the calculation
c        since the dynamic hohlraum radius has changed this time step.
c
        radt1 = radt3  +  ((velt1+velt3) * dt * 0.5)
        if (radt1 .lt. minrad) then
          radt1    = minrad
          testimpl = end_implode
          end if
c
c   d. Dynamic hohlraum inductance
c
        lt1 = lconst * log(rinit/radt1)
c
c   e. Time rate of change of inductance
c
        ldott2 = (lt1 - lt3) * rdt
c
c
c   f. New dynamic hohlraum mass for acceleration terms. Add in the
c      inner shells as they accrete. Stop increasing it
c      if radius drops below inner radius allowed.
c
        if (radt1 .gt. rliner2) then
          masst1 = initmass
          else if (radt1 .le. rliner2 .and. radt1 .gt. router) then
          masst1 = initmass + mliner2
          else if (radt1 .le. router .and. flagl3m .eq. 0.0) then
          flagl3m = 1.0
          masst1 = initmass + mliner2 + mliner3
          else if (radt1 .lt. router .and. radt1 .gt. rinner) then
          masst1 = mconst * (router * router  -  radt1 * radt1)
     &             + initmass + mliner2 + mliner3
          else
          masst1 = massri + initmass + mliner2 + mliner3
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
     &  '(/A/,A/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,A,1pe10.3/,
     &    A,1pe10.3/,A/)')
     &  ' ------------------------------------------------------------',
     &  ' Implosion radius has reached the minimum value.',
     &  '   time:                   ',time,
     &  '   minimum radius:         ',minrad,
     &  '   peak current:           ',peakcur,
     &  '   KE at min-radius:       ',gaske,
     &  '   velocity at min-radius: ',velt1,
     &  ' ------------------------------------------------------------'

      write(6,'(A)')
     &  ' ------------------------------------------------'
      write(6,'(A)')
     &  ' Implosion radius has reached the minimum value.'
      write(6,'(A,1pe10.3)') '  time:                   ',time
      write(6,'(A,1pe10.3)') '  minimum radius:         ',minrad
      write(6,'(A,1pe10.3)') '  peak current:           ',peakcur
      write(6,'(A,1pe10.3)') '  KE at min-radius:       ',gaske
      write(6,'(A,1pe10.3)') '  velocity at min-radius: ',velt1
      write(6,'(A)')
     &  ' ------------------------------------------------'
      end if
c
c   h. Save all values including implosion flags required for next time
c      step to parms.
c
        parms(5)  = masst1
        parms(12) = lt1
        parms(13) = radt1
        parms(14) = velt1
        parms(15) = testimpl
        parms(21) = flagl2
        parms(22) = flagl3
        parms(23) = flagl3m
        parms(24) = peakcur
c
c For stagnant dynamic hohlraum (constant radius and inductance):
c 
      else
c
c   d. Dynamic hohlraum inductance
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
c ----------------------------------------------------------------------
c
      return
      end
