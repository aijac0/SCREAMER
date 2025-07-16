      subroutine sphfoil_model
c read only arguments
     & (time, dt, rdt, ilt2,
c read and write arguments
     &  parms,
c write only arguments
     &  ldott2, lt1)
c ----------------------------------------------------------------------   
c  Modifications:
c    08/21/95, MLK, removed use of tand intrinsic and replaced with tan
c                   by converting degree argument to radians
c  2014-02-06 RBS: Changed real*4 to real
c ----------------------------------------------------------------------
c
c Summary:
c
c Screamer SPHERICAL foil implosion subroutine.
c   Treats r2 and l2 as variable elements (r2 is dl2/dt).
c
c   Define:
c     the next               full time step as t=i      (variable suffix t0)
c     the current            half time step as t=i-1/2  (variable suffix t1)
c     the previous           full time step as t=i-1    (variable suffix t2)
c     the previous           half time step as t=i-3/2  (variable suffix t3)
c     the previous, previous full time step as t=i-2    (variable suffix t4)
c     the previous, previous half time step as t=i-5/2  (variable suffix t5)
c
c Calculates the following values for an imploding foil:
c   Passed via argument list:
c     inductance (=lt1) at t=i-1/2
c     time rate of change of inductance (=ldott2) at t=i-1
c   Passed via 'zdemout.h' common blocks:
c     foil kinetic energy (=foilke) at t=i-1/2
c     foil radius (=foilrad)        at t=i-1/2
c     foil velocity (=foilvel)      at t=i-1/2
c     foil acceleration (=foilacc)  at t=i-1
c When the calculated foil radius becomes less than or equal to the
c minimum radius, the implosion stops and this routine sets:
c   radt1   = (foilrad =) minrad 
c   lt1     =  lminrad 
c   ldott2  = 0
c   foilke  = 0
c   foilvel = 0
c   foilacc = 0
c
c ------------------------------------------------------------------------------
c
c Include the file which has a common block for storing some unusual
c parameters from this model
c
      use zdemmax             !parameters
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
c Internal parameter
c
      real       mu, pi
      parameter (mu = 2.0e-7)
      parameter (pi = 3.14159265359)
c
      real       implode,       end_implode
      parameter (implode = 0.0, end_implode = 1.0)
c
c Set internal variables
c
      real       initrad   !initial radius
      real       angl      !foil included angle
      real       mass      !mass
      real       minrad    !minimum radius
      real       arc       !arc length
      real       lminrad   !inductance at minimum radius
      real       lt3       !inductance from t=i-3/2
      real       radt1     !calculated radius,   t=i-1/2
      real       radt3     !radius from t=i-3/2
      real       velt1     !calculated velocity, t=i-1/2
      real       velt3     !velocity from t=i-3/2
      real       acct2     !calculated acceleration, t=i-1
      real       testimpl  !calculated current state of foil 
                           !(imploding or stagnated)
      real       theta     !angl/2
      real       beta      !(theta + 90)/2
c
c ------------------------------------------------------------------------------
c
c Set the model parameters to understandable names.
c
      initrad   = parms(1)
      angl      = parms(2)
      mass      = parms(3)
      minrad    = parms(4)
c
      lt3       = parms(5)
      radt3     = parms(6)
      velt3     = parms(7)
      testimpl  = parms(8)
c
c ------------------------------------------------------------------------------
c
c Calculate the acceleration, velocity and radius of the foil.
c From these, calculate L and dL/dt (=R)
c
c   The acceleration at t=i-1 is defined by:
c       acct2 = -(mu * ilt2**2 * arc ) / ( 2 * mass * radt3 )
c
c   The time centered velocity is defined by:
c       (velt1 - velt3) / dt  = acct2
c
c   The time centered radius is defined by:
c       (radt1 - radt3) / dt = 0.5 * (velt1 + velt3)
c
c   L (lt1) is defined by:
c      theta = angl/2
c      beta = (theta + 90)/2
c     2*mu*(initrad-radt1)*(log(tan(beta*pi/180.0)))     
c
c   dL/dt (ldott2) is defined by:
c       (lt1 - lt3) / dt
c
c ------------------------------------------------------------------------------
c
c For an SPHERICAL imploding foil:
c
        theta = angl/2
        beta = (theta + 90)/2
        lminrad = 2*mu*(initrad-minrad)*(log(tan(beta*pi/180.0)))        
c
c ------------------------------------------------------------------------------
      if (testimpl .eq. implode) then
c
c   a. Foil acceleration
c
        arc = pi * radt3 * (angl/180.0)
        acct2 = -(mu * ilt2**2 * arc ) / ( 2 * mass * radt3 )
c
c   b. Foil velocity
c
        velt1 = velt3 + acct2*dt
c
c   c. Foil radius, check to see if less than minimum radius, if
c        so, print a message that this has occurred and set values
c        appropriately.  But keep going with the calculation since
c        the foil radius has changed this time step.
c
        radt1 = radt3  +  ((velt1+velt3) * dt * 0.5)
        if (radt1 .lt. minrad) then
          radt1    = minrad
          testimpl = end_implode
          write(9,999) time, minrad
        end if
c
c   d. Foil inductance
c
        lt1 = 2*mu*(initrad-radt1)*(log(tan(beta*pi/180.0)))   
         
c   e. Time rate of change of inductance
c
        ldott2 = (lt1 - lt3) * rdt
c
c   f. Put some values into a common block so that they will be
c      available for plotting.
c
        foilke  = 0.5 * mass * velt1 * velt1
        foilrad = radt1
        foilvel = velt1
        foilacc = acct2
c
c   g. Save all values required for next time step.
c
        parms(5)  = lt1
        parms(6)  = radt1
        parms(7)  = velt1
        parms(8)  = testimpl
c
c ------------------------------------------------------------------------------
c For stagnant foil (constant radius and inductance):
c 
      else
c
c   d. Foil inductance
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
        foilke  = 0.0
        foilrad = minrad
        foilvel = 0.0
        foilacc = 0.0
c
      end if
c
c ------------------------------------------------------------------------------
  999 format
     & ('1 '/
     & ' ------------------------------------------------------------'/
     & ' Foil radius has reached the minimum value.'/
     & '   time:           ', 1pe10.3/
     & '   minimum radius: ', 1pe10.3/
     & ' ------------------------------------------------------------'/
     & //////////)
c ------------------------------------------------------------------------------
      return
      end
