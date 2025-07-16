      Subroutine rwall (dt,time,parms,rvar)
c
c
c     Resistive Wall Model
c
c      Written by Ken Struve, March 7, 1994. 
c
c      Modified 5/12/98 to add coax resistance
c
c      Modified 7/27/98 to use the Stygar formula
c
c      Modified 1/6/99 to use the newer Stygar formula
c
c      Modified 10/6/06 to use most recent Stygar formula based on
c      Stygar memo dated 29 Sept 2005.
c
c 2008-09-06 RBS:Change the varible dimension from
c      dimension(1) to dimension(*), fixed a code warning
c 2015-06-23 RBS: Added dttmp = dt to eliminate a compiler warning based
c                 on an unused passed variable.
c
c**********************************************************************
c
c      This routine estimates the resistive losses in the wall at very
c      high current densities.  It is based on a model by Knoepfl, in
c      the book, Pulsed High Magnetic Fields, but modified by Bill
c      Stygar.  This is only an estimate at this time since it does not
c      use the actual current profile to determine the magnetic
c      diffusion into the conductor, but rather uses a square root of t
c      dependence.  Also, this routine does not consider current that is
c      carried in a plasma sheath, unless that sheath is produced by
c      vaporized conductor.  The resistance R for a disk transmission
c      line is described as
c       
c       R[Ohms]  =   sqrt(rho muo / pi^3 t) (sum(ln(router/rinner)) +
c                                                 sum(lcyl/rcyl))
c
c      where rho is 7.2e-7 ohm-m for stainless steel, t is the time from
c      the start of the current, diskinner and diskouter are the inner
c      and outer radii of the disk mitls, and lcyl and rcyl are the
c      lengths and radii of each individual coaxial section.  All units
c      are MKS.  NOTE:  router should only extend to the post hole
c      convolute for Z or ZX.  This formula is only valid for (B/Bo)^2 >
c      1, where Bo is sqrt(2 muo / beta), and beta is the resistivity
c      heat coefficient.  rho = rho(To) * (1 + beta Q), where Q = cp
c      delta T
c
c      The calculation can be simplified to
c
c      R[Ohms] = 1.708e-7 / sqrt(t) * geometry factor * scaling factor
c
c      The variables diskfact and cylfact have already been used to
c      create the variable geomfact, and are therefore not used in this
c      routine.  The variable g (which does not change) allows the user
c      to adjust the conductivity to account for temperature.  The value
c      of this constant is normally 1.  For higher assumed higher
c      temperatures it could be 2 or 3.
c
c Define passed variables
c
      real dt, time, parms(*), rvar
c
c Define internal variables
c
      real factor, t_in, g, diskfact, cylfact, geomfact, tau
c
      parameter (factor=1.708e-7)
c
      t_in     = parms(1)
      g        = parms(2)
      diskfact = parms(3)
      cylfact  = parms(4)
      geomfact = parms(5)
c
c dt used to avoid compiler errors
c
      dttmp = dt
c
c
c     Calculate the resistance
c
c
      if(time .le. t_in) then
         rvar=0.0
      else
         tau      = time-t_in+1.0e-10
         rvar     = factor * geomfact * g / sqrt(tau)
      endif
c
c
c
      return
      end

