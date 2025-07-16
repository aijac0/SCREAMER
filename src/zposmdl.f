       subroutine zpos_model (time, node, ib, parms, gvar)
c
c      Changes for version 2.1 (KWS):
c        Allows both forward and reverse direction of current.
c        Time-averaged current and voltage values are now used.
c
c      Changes for version 2.0 (KWS):
c        Changed gvar calculation algorithm to use iup and idown
c        instead of VSW.
c        Fixed model so it can be used many times in an input deck.
c
c      Modification
c  1989-03-07 MLK: Added END statement
c  2014-02-06 RBS: Changed real*4 to real
c 2015-06-23 RBS: cbtest removed from real declaration - not used
c 2015-06-23 RBS: caclzflow initialized to 0.0 to remove compiler
c                 warning
c
c     POS Zflow switch subroutine for screamer.  To use, set up RCGROUND
c     followed by the Format statement;    VARIABLE   R1   POS_MODEL
c     the next line  will have:
c     tsw, cursw, topen, zflowmax, gswmin, gswmax.
c
c      tsw = start time for triggered POS opening (seconds);
c      cursw =  current (amperes) into POS node for current switching;
c        NOTE; current into POS node NOT current to ground.
c      topen = opening time in seconds.
c      zflowmax =  maximum zflow in ohms.
c      gswmin = the minimum switch conductance in mhos.
c      gswmax = the maximum switch conductance in mhos.
c      CBswitch = 1.0, the switch crowbars when the voltage reverses,
c        anything else it doesn't
c
c
c      IMPORTANT:  This model will switch the POS with either current or
c      time depending on how you set TSW and CURSW.  The switch opens
c      when BOTH TSW and CURSW are exceeded!
c      For example, if you set the current to a high value, but still
c      less than the maximum expected current, and set the switch time
c      to a small value, the POS will open when the switch current
c      exceeds CURSW.  The switch time will be the time that CURSW is
c      first exceeded for any t > TSW. Conversely, if CURSW is set to a
c      small value that is easily exceeded, the switch will open when
c      t = TSW.
c
c      zv is the calculated opening rate, zv = zflowmax / topen.
c      The MINIMUM resistance of the switch is set to be 1/gswmax 
c
c      A note on convention:  Any variable starting with a "g" is a
c      conductance, and any variable starting with a "z" is a resistance
c      or impedance.
c 
c Define passed variables
c
      real time, parms(*), gvar
      integer node, ib
c
c Include common variable definitions
c
      include   'zdemmax.h'   !parameters
      include   'zdemout.h'   !common blocks
      include   'zdemwork.h'  !working arrays in Zdem
c
c Define internal variables
c
      real vmin
      real ttest, cursw, topen, zflowmax, gswmin, gswmax, CBswitch
      real skip, tsw, CBflag, forward
c
c Set internal variables
c
      parameter (vmin=1.0e3)
      calczflow = 0.0
c
c
c
      ttest     = parms(1)
      cursw     = parms(2)
      topen     = parms(3)
      zflowmax  = parms(4)
      gswmin    = parms(5)
      gswmax    = parms(6)
      CBswitch  = parms(7)
c
c     The following are saved parameters from one time-step to another
c
      skip      = parms(8)
      tsw       = parms(9)
      ctest     = parms(10)
      CBflag    = parms(11)
      forward   = parms(14)
c
c
      if (skip .ne. 1.0) then
         ctest=cursw
        endif
c
      if (gswmax .eq. 0.0 ) then
         zmin=5.0e-5
        else
         zmin=1/gswmax
        endif
c
c     Find the average current and voltage
c
      if (forward .eq. 1.0) then
         node1 = node - 1
         node2 = node
         cup = 0.5*(zir(node1,ib)+zirold(node1,ib))
         cdn = 0.5*(zir(node2,ib)+zirold(node2,ib))
        else
         node1 = node
         node2 = node - 1
         cup = 0.5*(zir(node1,ib)+zirold(node1,ib))
         cdn = 0.5*(zir(node2,ib)+zirold(node2,ib))
        endif
      csw=cup-cdn
c
      vsw=0.5*(v(node,ib)+vold(node,ib))
c
c     Calculate zflow(t)
c
      if ((time.ge.ttest).and.(abs(cup).ge.ctest)) then
         if (skip.ne.1.0) then
            tsw=time
            ctest=0.0
            skip=1.0
           endif
         if (topen .eq. 0.0) then
            zv = zflowmax/1.0e-10
           else
            zv = zflowmax/topen
           endif
         z1=zv*(time-tsw)
         z2=max(z1,zmin)
         zflow=min(z2,zflowmax)
        else
         gvar=gswmax
         goto 200
        endif
c
c     Find the equivalent conductance of the switch consistent with the
c     specified zflow(t).
c     Set crowbarflag to one (CBflag := 1.0) if voltage reverses and the
c     crowbarswitch equals 1.0  (CBswitch .eq. 1.0)
c
      if (CBswitch .eq. 1.0) then
         if ((vsw .ge. -vmin) .and. (CBflag .ne. 1.0)) then
            if (abs(cup) .gt. abs(cdn)) then
               gvar=abs(csw/sqrt(cup**2-cdn**2)) / zflow
               calczflow=vsw/(sqrt(cup**2-cdn**2)+ 1.0e-12)
              else
               gvar=gswmax
               calczflow=0.0
              endif
            goto 200
           else
            gvar=gswmax
            CBflag=1.0
            calczflow=0.0
            goto 200
           endif
        else
         goto 100
        endif
c
c
c
100   continue
      if (abs(cup) .gt. abs(cdn)) then
         gvar=abs(csw/sqrt(cup**2-cdn**2)) / zflow
         calczflow=vsw/(sqrt(cup**2-cdn**2)+ 1.0e-12)
        else
         gvar=gswmin
         calczflow=0.0
        endif
c
c     Bound the switch conductance by gswmin and gswmax
c
200   continue
      gvar=min(gvar,gswmax)
      gvar=max(gvar,gswmin)
c
      parms(8)  = skip
      parms(9)  = tsw
      parms(10) = ctest
      parms(11) = CBflag
      parms(12) = calczflow
      parms(13) = gvar
c
c       write(9,'(e12.4,4i6,6x,11e12.4)')
c     & time,node,node1,node2,ib,cup,zir(node1,ib),zirold(node1,ib),
c     & cdn,zir(node2,ib),zirold(node2,ib),vsw,gvar,zflow,calczflow,
c     & forward
c
      return
      end
      
