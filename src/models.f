      subroutine get_model
c
c ----------------------------------------------------------------------
c
c 2004-11-01 JAM: Created get_model subroutine from zdem.f for more
c                 modularity
c
c -------------------------Modifications--------------------------------
c
c 2012-04-05 RBS: Included the new RWALL model, R2WALL based on Stygar's
c                 latest paper.
c 2012-04-10 RBS: Caught the incorrect use of the jth node in the rr
c                 value of resistance used in the older RWALL model.
c 2014-03-11 RBS: Added a variable inductor in the variable resistor
c                 table model
c 2015-06-18 RBS: Moved the variable declarations that were in the
c                 common block /misc for models/ in zdemvars.h into this
c                 subroutine. Defined internal variables.
c 2015-06-18 RBS: Moved the variable declarations that were in the
c                 common block /mdl_vars/ in zdemvars.h into this
c                 subroutine. Defined internal variables.
c 2016-04-16 RBS: Added the DPF Model
c 2016-06-25 RBS: Added the ebeam model
c 2017-01-02 RBS: Added time-domain skin depth variable resistor model
c 2020-01-22 RBS: Added cold conductor model RCOND
c 2020-09-01 RBS: Changed the name of the active or Slutz diode to sdiode
c 2020-09-02 RBS: Added a new idealized diode model named diode
c ----------------------------------------------------------------------
c
c Include files
c
      include 'zdemparm.h'
      include 'zdemmax.h'
      include 'zdemwork.h'
      include 'zdemout.h'
      include 'zdemcomm.h'
      include 'zdemvars.h'
c
c Declare internal variables
c
      real
     & anodecurrent, blockvoltage, ccath, cplas,
     & currentbr, currentin, currentl2, currentout, currentr2,
     & dcdt, dcvdv, dldt, dlidi,
c     & dummyp1, dummyp2, dummyp3, dummyp4,
     & dvoltage, fcurrent,
     & pscurrent, psvoltage,
     & swcurrent, zmeas, zvac,
     & voltagec1, voltagec3, voltager1, voltager2, voltager3,
     & voltagein, voltageout

      real
     & lvar, cvar, rvar, gvar, drdt

      integer
     & iblock, iblock_num, ielement, imodel, inode,
     & ivar, jnode

c
c
c      write(*,*) 'nvar = ', nvar
      do 106 ivar = 1, nvar
c write(*,*) 'Starting do loop...'
        inode=indexv(1,ivar)
        ielement=indexv(2,ivar)
        ibranch=indexv(3,ivar)
        imodel=ivar_type(ivar)
        iblock=ivar_block(ivar)
        iblock_num=ivar_block_num(ivar)

c write(*,*) 'get_model: imodel = ', imodel
c User supplied model, different calls depending on element type.
c

          if      ((imodel .eq. user_model)  .or.
     +             (imodel .eq. user1_model) .or.
     +             (imodel .eq. user2_model) .or.
     +             (imodel .eq. user3_model) .or.
     +             (imodel .eq. user4_model))     then
c
            if      (ielement .eq. r1_var) then
              currentin        = zir(inode-1,ibranch)
              currentout       = zir(inode,ibranch)
              currentbr        = zib(inode,ibranch)
              voltager1        = v(inode,ibranch)
              if (imodel .eq. user_model) then
                 call user (ht,timehalf,currentin,currentout,currentbr,
     &                      voltager1, gvar, dummyv1, dummyv2)
              elseif (imodel .eq. user1_model) then
                 call user1 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager1, gvar, dummyv1, dummyv2)
              elseif (imodel .eq. user2_model) then
                 call user2 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager1, gvar, dummyv1, dummyv2)
              elseif (imodel .eq. user3_model) then
                 call user3 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager1, gvar, dummyv1, dummyv2)
              elseif (imodel .eq. user4_model) then
                 call user4 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager1, gvar, dummyv1, dummyv2)
              endif
              g(inode,ibranch) = gvar
c
            else if (ielement .eq. c1_var) then
              currentin            = zir(inode-1,ibranch)
              currentout           = zir(inode,ibranch)
              currentbr            = zib(inode,ibranch)
              voltagec1            = v(inode,ibranch)
              if (imodel .eq. user_model) then
                 call user (ht,timehalf,currentin,currentout,currentbr,
     &                      voltagec1, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user1_model) then
                 call user1(ht,timehalf,currentin,currentout,currentbr,
     &                      voltagec1, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user2_model) then
                 call user2 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec1, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user3_model) then
                 call user3 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec1, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user4_model) then
                 call user4 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec1, cvar, dcdt, dcvdv)
              endif
              c(inode,ibranch)     = dcvdv
              cdot(inode,ibranch)  = dcdt
              cechk(inode,ibranch) = cvar
c
            else if (ielement .eq. r2_var) then
              voltagein         = v(inode,ibranch)
              voltageout        = v(inode+1,ibranch)
              currentr2         = zir(inode,ibranch)
              if (imodel .eq. user_model) then
                 call user (ht,timehalf,voltagein,voltageout,currentr2,
     &                      dummyp4, rvar, dummyv2, dummyv3)
              elseif (imodel .eq. user1_model) then
                 call user1 (ht,timehalf,voltagein,voltageout,currentr2,
     &                       dummyp4, rvar, dummyv2, dummyv3)
              elseif (imodel .eq. user2_model) then
                 call user2 (ht,timehalf,voltagein,voltageout,currentr2,
     &                       dummyp4, rvar, dummyv2, dummyv3)
              elseif (imodel .eq. user3_model) then
                 call user3 (ht,timehalf,voltagein,voltageout,currentr2,
     &                       dummyp4, rvar, dummyv2, dummyv3)
              elseif (imodel .eq. user4_model) then
                 call user4 (ht,timehalf,voltagein,voltageout,currentr2,
     &                       dummyp4, rvar, dummyv2, dummyv3)
              endif
              rr(inode,ibranch) = rvar
c
            else if (ielement .eq. l2_var) then
              voltagein              = v(inode,ibranch)
              voltageout             = v(inode+1,ibranch)
              currentl2              = zir(inode,ibranch)
              if (imodel .eq. user_model) then
                 call user (ht,timehalf,voltagein,voltageout,currentl2,
     &                      dummyp4, lvar, dldt, dlidi)
              elseif (imodel .eq. user1_model) then
                 call user1(ht,timehalf,voltagein,voltageout,currentl2,
     &                      dummyp4, lvar, dldt, dlidi)
              elseif (imodel .eq. user2_model) then
                 call user2 (ht,timehalf,voltagein,voltageout,currentl2,
     &                       dummyp4, lvar, dldt, dlidi)
              elseif (imodel .eq. user3_model) then
                 call user3 (ht,timehalf,voltagein,voltageout,currentl2,
     &                       dummyp4, lvar, dldt, dlidi)
              elseif (imodel .eq. user4_model) then
                 call user4 (ht,timehalf,voltagein,voltageout,currentl2,
     &                       dummyp4, lvar, dldt, dlidi)
              endif
              zlr(inode,ibranch)     = dlidi
              zlrdot(inode,ibranch)  = dldt
              zlrechk(inode,ibranch) = lvar
c
            else if (ielement .eq. r3_var) then
              jnode            = inode + 1
              currentin        = zir(inode,ibranch)
              currentout       = zir(jnode,ibranch)
              currentbr        = zib(jnode,ibranch)
              voltager3        = v(jnode,ibranch)
              if (imodel .eq. user_model) then
                 call user (ht,timehalf,currentin,currentout,currentbr,
     &                      voltager3, gvar, dummyv2, dummyv3)
              elseif (imodel .eq. user1_model) then
                 call user1 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager3, gvar, dummyv2, dummyv3)
              elseif (imodel .eq. user2_model) then
                 call user2 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager3, gvar, dummyv2, dummyv3)
              elseif (imodel .eq. user3_model) then
                 call user3 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager3, gvar, dummyv2, dummyv3)
              elseif (imodel .eq. user4_model) then
                 call user4 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltager3, gvar, dummyv2, dummyv3)
              endif
              g(jnode,ibranch) = gvar
c
            else if (ielement .eq. c3_var) then
              jnode                = inode + 1
              currentin            = zir(inode,ibranch)
              currentout           = zir(jnode,ibranch)
              currentbr            = zib(jnode,ibranch)
              voltagec3            = v(jnode,ibranch)
              if (imodel .eq. user_model) then
                 call user (ht,timehalf,currentin,currentout,currentbr,
     &                      voltagec3, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user1_model) then
                 call user1 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec3, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user2_model) then
                 call user2 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec3, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user3_model) then
                 call user3 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec3, cvar, dcdt, dcvdv)
              elseif (imodel .eq. user4_model) then
                 call user4 (ht,timehalf,currentin,currentout,currentbr,
     &                       voltagec3, cvar, dcdt, dcvdv)
              endif
              c(jnode,ibranch)     = dcvdv
              cdot(jnode,ibranch)  = dcdt
              cechk(jnode,ibranch) = cvar
            end if
c  
c  The code which follows deals with the USER variables. Values for user
c   variables are passed to this main program via the /uservar/ common
c   block
c
c       Save the values of the user variables for this time step 
c        into the semi-static uservars array
c
            uservars(iblock_num,ibranch,1)  = u1
            uservars(iblock_num,ibranch,2)  = u2
            uservars(iblock_num,ibranch,3)  = u3
            uservars(iblock_num,ibranch,4)  = u4
            uservars(iblock_num,ibranch,5)  = u5
            uservars(iblock_num,ibranch,6)  = u6
            uservars(iblock_num,ibranch,7)  = u7
            uservars(iblock_num,ibranch,8)  = u8
            uservars(iblock_num,ibranch,9)  = u9
            uservars(iblock_num,ibranch,10) = u10
c
c------------------------------------------------------------------------
c  Variable Resistor Table model -- Added 4/6/89, KLF
c  Variable Resistor/Inductor Table Model Added 2014-03-11 RBS
c  Valid for R1 (G1), R2, or R3 (G3), or L2.
c  I am leaving the table value as rvar even though it is lvar
c  in the variable inductance case.
c
          else if (imodel .eq. tab_model) then
c
c      Increment counter for table models, and get resistance from
c        appropriate table
c
            itab_counter = itab_counter+1
            call get_tablem_value (timehalf, itab_counter, rvar, drdt)
c
            if      (ielement .eq. r1_var) then
              g(inode,ibranch) = 1.0 / (abs(rvar)+1.0e-20)
            else if (ielement .eq. r2_var) then
              rr(inode,ibranch) = rvar
            else if (ielement .eq. r3_var) then
              jnode            = inode + 1
              g(jnode,ibranch) = 1.0 / (abs(rvar)+1.0e-20)
            else if (ielement .eq. l2_var) then
c   L for circuit solver
c
              zlr(inode,ibranch)     = rvar
c
c   dL/dt for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable resistor
c   and what we really need is (+0.5*dldt*I*I) dissipated.
c
             zlrdot(inode,ibranch)  = -1.0*drdt
c
c   L for energy check
c
             zlrechk(inode,ibranch) = rvar
c
c   R(=dL/dt) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
             rr(inode,ibranch)      = drdt

            end if
c
c------------------------------------------------------------------------
c Gas switch model.
c Valid for R1 (G1), R2, or R3 (G3).
c
          else if (imodel .eq. exp_model) then
c       write(*,*) 'Exponential switch model'
            call sexp_model (timehalf, var_model(1,ivar), rvar)

            if      (ielement .eq. r1_var) then
              g(inode,ibranch)    = 1.0 / (rvar+1.0e-20)
            else if (ielement .eq. r2_var) then
              rr(inode,ibranch)   = rvar
            else if (ielement .eq. r3_var) then
              g(inode+1,ibranch)  = 1.0 / (rvar+1.0e-20)
            end if

c       write(*,*)'Exited exponential switch model'
c
c------------------------------------------------------------------------
c Decay switch model.
c Valid for R1 (G1), R2, or R3 (G3).
c
          else if (imodel .eq. decay_model) then
c           write(*,*) 'Decay switch model'

            call sdecay_model (timehalf, var_model(1,ivar), rvar)

            if      (ielement .eq. r1_var) then
              g(inode,ibranch)    = 1.0 / (rvar+1.0e-20)
            else if (ielement .eq. r2_var) then
              rr(inode,ibranch)   = rvar
            else if (ielement .eq. r3_var) then
              g(inode+1,ibranch)  = 1.0 / (rvar+1.0e-20)
            end if
c
c-----------------------------------------------------------------------
c Rise switch model.
c Valid for R1 (G1), R2, or R3 (G3).
c
          else if (imodel .eq. rise_model) then

            call srise_model (timehalf, var_model(1,ivar), rvar)

            if      (ielement .eq. r1_var) then
              g(inode,ibranch)    = 1.0 / (rvar+1.0e-20)
            else if (ielement .eq. r2_var) then
              rr(inode,ibranch)   = rvar
            else if (ielement .eq. r3_var) then
              g(inode+1,ibranch)  = 1.0 / (rvar+1.0e-20)
            end if
c
c-----------------------------------------------------------------------
c
c Tom's lossy switch model, sw_model
c Valid for  R2 in a PIEsection or a RLSeries block.
c
          else if (imodel .eq. sw_model) then

            if (ielement .eq. r2_var) then
              voltagein         = v(inode,ibranch)
              voltageout        = v(inode+1,ibranch)
              currentr2         = zir(inode,ibranch)
            end if

          call tsw_model (ht,timehalf,var_model(1,ivar),
     &            rvar,currentr2)
          rr(inode,ibranch)   = rvar
c
c
c-----------------------------------------------------------------------
c
c PEOS switch model 1 or 2.
c Valid for R1 (G1) or R3 (G3).
c
          else if ((imodel .eq. ps1_model) .or.
     &             (imodel .eq. ps2_model)) then

            if      (ielement .eq. r1_var) then
              jnode = inode
            else if (ielement .eq. r3_var) then
              jnode = inode + 1
            end if

            pscurrent = zir(jnode-1,ibranch)
            psvoltage = v(jnode,ibranch)

            if (imodel .eq. ps1_model) then
              call sps1_model (ht, timehalf, pscurrent, psvoltage,
     &                         var_model(1,ivar), gvar)
            else if (imodel .eq. ps2_model) then
              call sps2_model (ht, pscurrent, psvoltage,
     &                         var_model(1,ivar), gvar)
            end if

            g(jnode,ibranch)    = gvar
c
c-----------------------------------------------------------------------
c
c Z-flow POS switch model .
c Valid for R1 (G1).
c
          else if (imodel .eq. pos_model) then
            if  (ielement .eq. r1_var) then
            jnode = inode
            end if
c                             
            call zpos_model
     &        (timehalf,jnode,ibranch,var_model(1,ivar),gvar)
c
            g(jnode,ibranch)    = gvar
c
c
c-----------------------------------------------------------------------
c
c KWS  Z-flow Plasma Loss Current Model .
c Valid for R1 (G1).
c
          else if (imodel .eq. zflow_model) then
            if  (ielement .eq. r1_var) then
            jnode = inode
            end if
c                             
            call zflowloss
     &        (timehalf, jnode, ibranch, var_model(1,ivar), gvar)
c
            g(jnode,ibranch)    = gvar
c
c
c-----------------------------------------------------------------------
c
c MFI Insulator Crowbar model .
c Valid for R1 (G1).
c
          else if (imodel .eq. mfi_model) then
            if  (ielement .eq. r1_var) then
              jnode = inode
              currentin = zir(jnode-1,ibranch)
              currentout= zir(jnode,ibranch)
              voltager1 = v(jnode,ibranch)
            end if
            call zmfi_model (timehalf,currentin,currentout,
     &                         voltager1,var_model(1,ivar),gvar)
            g(jnode,ibranch) = gvar

c
c-----------------------------------------------------------------------
c
c RWALL Resistive Wall Model,  March 7, 1994  KWS
c Valid for R2 only
c
          else if (imodel .eq. rwall_model) then
            call rwall (ht,timehalf,var_model(1,ivar),rvar)
            rr(inode,ibranch)    = rvar
c
c-----------------------------------------------------------------------
c
c R2WALL New Stygar Wall Model,  March 8, 2012  RBS
c Valid for R2 only
c
          else if (imodel .eq. r2wall_model) then
            current = zir(inode,ibranch)
            call r2wall (timehalf, current, var_model(1,ivar), rvar)
            rr(inode,ibranch)    = rvar
c
c-----------------------------------------------------------------------
c
c RSKIN New skin depth resistance, Jan 2, 2017  RBS
c Valid for R2 only
c
          else if (imodel .eq. rskin_model) then
            current = zir(inode,ibranch)
            call rskin (ht, timehalf, current, var_model(1,ivar),
     &                  max_skin_points, delta_i(1,ivar), H_field, rvar)
            rr(inode,ibranch)    = rvar

c
c-----------------------------------------------------------------------
c
c RCOND Conductor heating Model,  January 1, 2020  RBS
c Valid for R1, R2 only
c
          else if (imodel .eq. rcond_model) then
c     Get values for current and rvar from the prior time step
            current = zir(inode,ibranch)
            rvar    = rr(inode,ibranch)

            call rcond (ht, timehalf, current, var_model(1,ivar), rvar)

c            write(6,*) 'Element type = ', ielement

            if (ielement .eq. r1_var) then
               g(inode,ibranch)  = 1.0 / (rvar+1.0e-20)
            else if (ielement .eq. r2_var) then
               rr(inode,ibranch) = rvar
            end if
c
c-----------------------------------------------------------------------
c E beam diode model.
c Valid for R1 (G1).
c Voltage on the node is to ground
c
          else if (imodel .eq. ediode_model) then
            voltager1 = v(inode,ibranch)
            call ediode (timehalf, voltager1, var_model(1,ivar), gvar)
            g(inode,ibranch) = abs( gvar )
c
c-----------------------------------------------------------------------
c
c ZMIP ( aka CTOPS ) POS model, Feb 26, 1993   hnw
c Valid for R1 (G1).
c
C          else if (imodel .eq. zmip_model) then
C            if  (ielement .eq. r1_var) then
C            jnode = inode
c
C            currentin = zir(jnode-1,ibranch)
C            currentout= zir(jnode,ibranch)
C            voltager1 = v(jnode,ibranch)
C            call zzmip_model (ht,timehalf,currentin,currentout,
C     &         voltager1,var_model(1,ivar),gvar)
C            end if
c                             
C            g(jnode,ibranch)    = gvar
c
c-----------------------------------------------------------------------
c
c                         MEASURE ZFLOW block
c                            Feb. 24, 1998
c                                KWS
c
c The is a null block that only calculates cathode and plasma current,
c and the actual zflow using the Cliff Mendel pressure balance equation.
c The data is accessed by inserting output calls anywhere after the block.
c This block does not change the circuit.
c
          else if (iblock .eq. measurezflow) then
            mzflowblock  = mzflowblock + 1
c Inputs
            anodecurrent = 0.5*(zir(inode,ibranch)+zirn(inode,ibranch))
            blockvoltage = 0.5*(v(inode,ibranch)+vn(inode,ibranch))
            zvac = zofmzflow(mzflowblock)
c Calculate currents and zflow
            call FindZflow
     &      (timehalf,anodecurrent,blockvoltage,zvac,ccath,cplas,zmeas)
c Store values
            ccathode(mzflowblock)   = ccath
            cplasma(mzflowblock)    = cplas
            measdzflow(mzflowblock) = zmeas
c
c
c-----------------------------------------------------------------------
c
c Imploding CYLINDRICAL foil model block.
c The series inductance and the series resistance are variable.
c Note: R = dL/dt.
c
          else if (iblock .eq. cylfoilblock) then
            fcurrent = zir(inode,ibranch)
            call cylfoil_model (timehalf, ht, rht, fcurrent,
     &                        var_model(1,ivar), dldt, lvar)
c
c   L for circuit solver
c
            zlr(inode,ibranch)     = lvar
c
c   dL/dt for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable resistor
c   and what we really need is (+0.5*dldt*I*I) dissipated.
c
            zlrdot(inode,ibranch)  = -1.0*dldt
c
c   L for energy check
c
            zlrechk(inode,ibranch) = lvar
c
c   R(=dL/dt) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
            rr(inode,ibranch)      = dldt
c
c-----------------------------------------------------------------------
c
c Imploding N SHELL model block.
c The series inductance and the series resistance are variable.
c Note: R = dL/<DT>.
c
          else if (iblock .eq. nshellblock) then
            fcurrent = zir(inode,ibranch)
            voltager2 = v(inode,ibranch)
            call nshell_model (timehalf, ht, rht, fcurrent,voltager2,
     &                        var_model(1,ivar), dldt, lvar)
c
c   L for circuit solver
c
            zlr(inode,ibranch)     = lvar
c
c   dL/<DT> for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable
c   resistor and what we really need is (+0.5*dldt*I*I) dissipated.
c
            zlrdot(inode,ibranch)  = -1.0*dldt
c
c   L for energy check
c
            zlrechk(inode,ibranch) = lvar
c
c   R(=dL/<DT>) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
            rr(inode,ibranch)      = dldt
c
c-----------------------------------------------------------------------
c
c Imploding SPHERICAL foil model block.
c The series inductance and the series resistance are variable.
c Note: R = dL/dt.
c
          else if (iblock .eq. sphfoilblock) then
            fcurrent = zir(inode,ibranch)
            call sphfoil_model (timehalf, ht, rht, fcurrent,
     &                        var_model(1,ivar), dldt, lvar)
c
c   L for circuit solver
c
            zlr(inode,ibranch)     = lvar
c
c   dL/dt for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable resistor
c   and what we really need is (+0.5*dldt*I*I) dissipated.
c
            zlrdot(inode,ibranch)  = -1.0*dldt
c
c   L for energy check
c
            zlrechk(inode,ibranch) = lvar
c
c   R(=dL/dt) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
            rr(inode,ibranch)      = dldt
c
c-----------------------------------------------------------------------
c
c Slutz Diode model.
c Valid for R1 (G1) or R3 (G3).
c
          else if (imodel .eq. sdiode_model) then
            if      (ielement .eq. r1_var) then
              jnode = inode
            else if (ielement .eq. r3_var) then
              jnode = inode + 1
            end if
            dvoltage = v(jnode,ibranch)
            call ssdiode_model (timehalf, dvoltage, var_model(1,ivar),
     &                         gvar)
            g(jnode,ibranch)    = gvar
c
c-----------------------------------------------------------------------
c
c Classical Electrical Diode model.
c Valid for R1 (G1) or R3 (G3).
c
          else if (imodel .eq. diode_model) then
            if      (ielement .eq. r1_var) then
              jnode = inode
            else if (ielement .eq. r3_var) then
              jnode = inode + 1
            end if
            dvoltage = v(jnode,ibranch)
            call wasdiode_model (timehalf, dvoltage, var_model(1,ivar),
     &                         gvar, cvar)
c model returns the conductance and capacitance to the problem
            g(jnode,ibranch)    = gvar
c             c(jnode,ibranch)    = cvar
c
c------------------------------------------------------------------------
c
c Applied-B Diode model.
c Valid for R1 (G1) or R3 (G3).
c
          else if (imodel .eq. abdiode_model) then
            if      (ielement .eq. r1_var) then
              jnode = inode
            else if (ielement .eq. r3_var) then
              jnode = inode + 1
            end if
            dvoltage = v(jnode,ibranch)
            call sabdiode_model
     &        (dvoltage, var_model(1,ivar), gvar)
            g(jnode,ibranch)    = gvar
c
c------------------------------------------------------------------------
c
c Saturable core inductor magnetic switch.
c Valid for L2 only.
c
          else if (imodel .eq. magsw_model) then
            swcurrent = zir(inode,ibranch)
            call smagsw_model (rht, swcurrent, var_model(1,ivar),
     &                         dldt, dlidi, lvar)
            zlr(inode,ibranch)     = dlidi
            zlrdot(inode,ibranch)  = dldt
            zlrechk(inode,ibranch) = lvar
c
c------------------------------------------------------------------------
c
c Imploding dynamic hohlraum model block.
c The series inductance and the series resistance are variable.
c Note: R = dL/dt.
c
          else if (iblock .eq. dyhohlblock) then
            fcurrent = zir(inode,ibranch)
            call dynamichohlraum_model (timehalf, ht, rht, fcurrent,
     &                                  var_model(1,ivar), dldt, lvar)
c
c   L for circuit solver
c
            zlr(inode,ibranch)     = lvar
c
c   dL/dt for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable
c   resistor and what we really need is (+0.5*dldt*I*I) dissipated.
c
            zlrdot(inode,ibranch)  = -1.0*dldt
c
c   L for energy check
c
            zlrechk(inode,ibranch) = lvar
c
c   R(=dL/dt) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
            rr(inode,ibranch)      = dldt
c
c-----------------------------------------------------------------------
c
c Imploding gas puff model block.
c The series inductance and the series resistance are variable.
c Note: R = dL/dt.
c
          else if (iblock .eq. gaspuffblock) then
            fcurrent = zir(inode,ibranch)
            call gaspuff_model (timehalf, ht, rht, fcurrent,
     &                          var_model(1,ivar), dldt, lvar)
c
c   L for circuit solver
c
            zlr(inode,ibranch)     = lvar
c
c   dL/dt for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable resistor
c   and what we really need is (+0.5*dldt*I*I) dissipated.
c
            zlrdot(inode,ibranch)  = -1.0*dldt
c
c   L for energy check
c
            zlrechk(inode,ibranch) = lvar
c
c   R(=dL/dt) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
            rr(inode,ibranch)      = dldt
c
c-----------------------------------------------------------------------
c
c DPF model block.
c The series inductance and the series resistance are variable.
c Note: R = dL/dt.
c
          else if (iblock .eq. dpfblock) then
            fcurrent = zir(inode,ibranch)
            call dpf_model (timehalf, ht, rht, fcurrent,
     &                      var_model(1,ivar), dldt, lvar)
c
c   L for circuit solver
c
            zlr(inode,ibranch)     = lvar
c
c   dL/dt for energy check (0.5*zlrdot*I*I=power dissipated)
c   Note that we are subtracting this quantity since we will add
c   2 times this amount into the power dissipated in the variable resistor
c   and what we really need is (+0.5*dldt*I*I) dissipated.
c
            zlrdot(inode,ibranch)  = -1.0*dldt
c
c   L for energy check
c
            zlrechk(inode,ibranch) = lvar
c
c   R(=dL/dt) for circuit solver which will also be used to dissipate
c   power at the rate of R*I*I=(dldt*I*I) in the energy check (and we
c   cannot override that calculation).
c
            rr(inode,ibranch)      = dldt
c
          end if
c      write(*,*)'Exiting model do loop...'
c

  106   continue
c         write(*,*)'Finished 106 do loop....'
c
c------------------------------------------------------------------------
c      write(*,*)'Return from models subroutine'
      return
      end
