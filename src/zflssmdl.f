      Subroutine zflowloss (time, node, ib, parms, gvar)
c
c      Changes for version 2.1 (KWS):
c        Allows both forward and reverse direction of current.
c        Time-averaged current and voltage values are now used.
c        Zflow turn-on loss model improved.
c
c      Written by Ken Struve, April 20, 1995
c
c Modification log
c
c 2016-03-16 RBS: Cleaned up a single write format statement
c
c-----------------------------------------------------------------------
c
c      This routine forces a plasma loss current in a magnetically
c      insulated transmission line consistent with the zflow definition
c      and the user specified zflow.  The loss is only turned on after
c      Child-Langmuir emission is turned off.  The criterion is
c      consistent with the calculation in the MITL model.
c
c
c      Inputs to the model are:
c
c      zflow:   The zflow impedance in Ohms
c      gap:     The gap separation in meters, either disk or coaxial
c      radius:  The cathode radius (coaxial), or disk radius, in meters
c      gmin:    The minimum conductance, units of mhos (inverse ohms)
c      gmax:    The maximum conductance, units of mhos
c      xni:     The number of parallel lines being calculated
c
c      Outputs are:
c
c      gvar       The time-varying conductance in mhos
c      calczflow  The calculated zflow in ohms, available in user output
c
c Include files
c
      use zdemmax
      include 'zdemwork.h'
      include 'zdemcomm.h'
c
c Define passed variables
c
      real       time, parms(*), gvar
      integer    node, ib
c
c Define internal variables
c
      real zflow, gap, radius, gmin, gmax, emissionflag, xni, forward
      real cup, cdn, vsw, vx, efield, tintegral, calczflow
      real bs, es, critval, turnon, gx, f, xx, tail, factor
c
      real       pi, cvac, zmuo, con1, bm, xy, emin, tthreshold, xmcsqd
      parameter (pi         = 3.1415927)
      parameter (cvac       = 2.9979e8)
      parameter (zmuo       = 4.0e-7 * pi)
      parameter (con1       = zmuo * zmuo)
      parameter (bm         = 0.5)
      parameter (xy         = 0.9 * bm)
      parameter (emin       = 3.0e7)
      parameter (tthreshold = 5.0e-9)
      parameter (xmcsqd     = 0.511e6)
c
c Set variable values
c
       zflow        = parms(1)
       gap          = parms(2)
       radius       = parms(3)
       gmin         = parms(4)
       gmax         = parms(5)
       emissionflag = parms(8)
       tintegral    = parms(9)
       xni          = parms(10)
       forward      = parms(11)

c set temptime = time to remove compiler warning
      temptime = time
c
c
c      Get the currents and voltage
c
       if (forward .eq. 1.0) then
          cup    = 0.5*(zir(node-1,ib)+zirold(node-1,ib))
          cdn    = 0.5*(zir(node,ib)+zirold(node,ib))
       else
          cup    = 0.5*(zir(node,ib)+zirold(node,ib))
          cdn    = 0.5*(zir(node-1,ib)+zirold(node-1,ib))
       endif
       cdn    = 0.999999 * cdn
       csw    = cup-cdn
       vsw    = 0.5*(v(node,ib)+vold(node,ib))
       vx     = abs (vsw) + 1.0
       efield = vx/gap
c
c
c      First find the Child-Langmuir emission threshold.  Don't bother
c      calculating zflow losses until the C-L emission threshold is
c      exceeded.
c      The criterion for emission is that the field exceeds the minimum
c      field for a time tthreshold.  Usually emin is 300 kV/cm and
c      tthreshold is 5 ns.
c
       if (emissionflag .eq. 0.0) then
         if (efield .ge. emin) then
            tintegral = tintegral+ht
         endif
         if (tintegral .ge. tthreshold) then
            emissionflag = 1.0
c            write(*,*) 'Emission threshold exceeded at t = ', time
            go to 100
         else
            gvar = gmin
            calczflow = 1.0/gmin
            go to 200
         endif
       endif
c
c
c
c      Calculate the zflow loss turn on criterion.  This is based on the
c      magnetic insulation of the Child-Langmuir emission. This uses the
c      same formulism of the MITL model.  It is:
c
c      c**2 B**2 / E**2  >  1  +  2 mc**2 / eV      
c
c
 100   continue
       bs      =  (zmuo*cup/(2.0*xni*pi*radius))**2  + 1.0e-6
       es      =  efield * efield + 1.0
       critval =  1.0 + 2.0 * xmcsqd / vx
       turnon  =  cvac * cvac * bs / (es * critval)
c
c
c
c      exponential with no attenuated tail - nominal parameters
c
       gx  = (1.21 * turnon) ** 2.5
c
c      super exponential g model with attenuated tail
c
       if (gx .gt. 15.0) then
         f = 0.0
       else if (gx .lt. -15.0) then
         f = 3.269e6
       else
         f = exp(-gx)
       endif
c
       xx    = bm * turnon
       if (xx .gt. xy) then
          if ((xx-xy).gt. 4.0) then
            tail = 0.0
          else
            tail = exp(-10.0*(xx-xy))
          endif
       else
         tail = 1.0
       end if
c
       factor = 1.0 - f * tail
c
c
c
c      Calculate zflow
c
       if ((vsw. gt. 0.0) .and. (abs(cup).gt.abs(cdn))) then
          gvar      = abs(csw/sqrt(cup**2-cdn**2)) / zflow
          calczflow = vsw/(sqrt(cup**2-cdn**2)+ 1.0e-12)
          if (factor .ge. 0.0) then
             gvar = gvar * factor
          else
             gvar = gmin
          endif
       else
          gvar = gmin
          calczflow = 1.0 / gmin
       endif
c
c
c      Bound the switch conductance by gmin and gmax
c
c
       gvar = min(gvar,gmax)
       gvar = max(gvar,gmin)
c
c      Save the output values
c
 200   continue
c       if(((time.ge. 110.0e-9).and.(time.le.110.1e-9)).or.
c     &    ((time.ge. 120.0e-9).and.(time.le.120.1e-9)).or.
c     &    ((time.ge. 130.0e-9).and.(time.le.130.1e-9)).or.
c     &    ((time.ge. 140.0e-9).and.(time.le.140.1e-9)))
c     & then
c       write(*,'(20e10.4)')
c     &                 time,cup,cdn,vsw,turnon,gx,xx,f,tail,factor,gvar
c       endif
       parms(6) = calczflow
       parms(7) = gvar
       parms(8) = emissionflag
       parms(9) = tintegral
c
       return
c
       end
