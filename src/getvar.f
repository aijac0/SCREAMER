      subroutine get_variable (vtype, branch, node, lastnode, blktype, 
     &                         dt, rdt, time, savev, savev2, value, 
     &                         time_flag, block, ipinblk)
c
c Note:time_flag defined as integer in zdemparam
c
c ---------------------------------------------------------------------
c  Modifications:
c     KWS, 03/07/94, Fixed outputs for MFI and SW models to
c                    allow multiple calls to those routines.
c                    Earlier fixes involved adding callouts for
c                    radiation yields in the cylindrical foil model.
c     MLK, 03/07/95, Change include filenames to be 8 characters or less
c     KWS, 05/30/95, Added output parameters due to addition of Zflow
c                    plasma loss model
c     MLK, 01/15/96, Added additional argument, ipinblk, the block
c                    number to be used to ref the "pin" array when it is
c                    used to store calculations this routine needs
c     MLK, 02/14/97, Changed `type` variable to `vtype` so as not to
c                    conflict with Fortran 90 TYPE statement
c     KWS, 08/12/97, Added Zflow and Cathode Current diagnostics
c 2014-02-06 RBS: Changed real*4 to real
c 2014-05-02 RBS: Changed integer*4 to integer
c 2014-05-02 RBS: Split the real and integer declarations to reflect
c                 called and local variables.
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c 2018-02-22 RBS: Added a new output parameter in the MITL model, closs,
c                 needed to output closs from rstmitl.
c ----------------------------------------------------------------------
c
c value is the returned value of the requested variable.
c vtype is variable requested.
c branch, block and node specify its location in the circuit.
c node is the first block node, lastnode is the lastblock node.
c blktype is type of circuit block it is for.
c ipinblk is the block number to used to reference the "pin" array
c   (2nd index in that array)
c dt is the timestep.
c rdt is 1/dt.
c time is the problem time (only needed for some t=0 calculations).
c savev is some saved value from the last time step
c   for energy calculations (all energy requests) or for
c   power calculations (capacitors and inductors if variable).
c   This is updated and returned if necessary.
c
c
c Include files
c
      use zdemmax
      use zdemwork
      use zdemcomm
      use zdemparm
      use zdemout
      use zdempprm

c
c Define passed variables
c
      integer    vtype, branch, node, lastnode, blktype
      real       dt, rdt, time, savev, savev2, value
      integer    block, ipinblk

c
c ***** Time flag parameters ******
c

      integer time_flag, half_step,     whole_step
      parameter         (half_step = 1, whole_step = 2)
c
c Define internal variables
c
      real cvalue, ivalue

c
c Resistor circuit element (series R2, shunt R1 and R3)
c
      if ((vtype .eq. ip_vr1) .or. (vtype .eq. ip_vr3)) then
        value = fvblk (node, branch, time_flag)
      else if (vtype .eq. ip_vr2) then
        value = fvr2 (node, branch, time_flag)
      else if ((vtype .eq. ip_ir1) .or. (vtype .eq. ip_ir3)) then
        value = fir1 (node, branch, time_flag)
      else if (vtype .eq. ip_ir2) then
        value = fiout (node, branch, time_flag)
      else if ((vtype .eq. ip_pr1) .or. (vtype .eq. ip_pr3)) then
        value = fpr1 (node, branch, time_flag)
      else if (vtype .eq. ip_pr2) then
        value = fpr2 (node, branch, time_flag)
      else if ((vtype .eq. ip_er1) .or. (vtype .eq. ip_er3)) then
        value = savev  +  fpr1 (node, branch, time_flag) * dt
        savev = value
      else if (vtype .eq. ip_er2) then
        value = savev  +  fpr2 (node, branch, time_flag) * dt
        savev = value
      else if ((vtype .eq. ip_r1) .or. (vtype .eq. ip_r3)) then
        value = 1.0 / (g(node,branch) + 1.0e-20)
      else if (vtype .eq. ip_r2) then
        value = rr(node,branch)
      else if ((vtype .eq. ip_qr1) .or. (vtype .eq. ip_qr3)) then
        value = savev  +  fir1 (node, branch, time_flag) * dt
        savev = value
      else if (vtype .eq. ip_qr2) then
        value = savev  +  fiout (node, branch, time_flag) * dt
        savev = value
c
c Capacitor circuit element (shunt).
c   Watch for initial state for current or power.
c
      else if ((vtype .eq. ip_vc1) .or. (vtype .eq. ip_vc3)) then
        value = fvblk (node, branch, time_flag)
      else if ((vtype .eq. ip_ic1) .or. (vtype .eq. ip_ic3)) then
        if (time .gt. 0.0) then
          charg = fqc1 (node, branch, time_flag)
          value = (charg - savev) * rdt
          savev = charg
        else
          savev = fqc1 (node, branch, time_flag)
          value = 0.0
        end if
      else if ((vtype .eq. ip_pc1) .or. (vtype .eq. ip_pc3)) then
        if (time .gt. 0.0) then
          epowr = fec1 (node, branch, time_flag)
          value = fpc1 (node, branch, time_flag)  +
     &            (epowr - savev) * rdt
          savev = epowr
        else
          savev = fec1 (node, branch, time_flag)
          value = 0.0
        end if
      else if ((vtype .eq. ip_ec1) .or. (vtype .eq. ip_ec3)) then
        savev = savev  +  fpc1 (node, branch, time_flag) * dt
        value = fec1 (node, branch, time_flag)  +  savev
      else if ((vtype .eq. ip_c1) .or. (vtype .eq. ip_c3)) then
        value = cechk(node,branch)
      else if ((vtype .eq. ip_qc1) .or. (vtype .eq. ip_qc3)) then
        value = fqc1 (node, branch, time_flag)
      else if ((vtype .eq. ip_c1e) .or. (vtype .eq. ip_c3e)) then
        value = c(node,branch)
c
c Inductor circuit element (series).
c   Watch for initial voltage and power.
c
      else if (vtype .eq. ip_vl2) then   !V=d(flux)/dt
        if (time .gt. 0.0) then
          fluxl = ffl2 (node, branch, time_flag)
          value = (fluxl - savev) * rdt
          savev = fluxl
        else
          savev = ffl2 (node, branch, time_flag)
          value = 0.0
        end if
      else if (vtype .eq. ip_il2) then
        value = fiout (node, branch, time_flag)
      else if (vtype .eq. ip_pl2) then  !P=0.5*(d(L*I*I)/dt + I*I*dL/dt))
        if (time .gt. 0.0) then
          epowr = fel2 (node, branch, time_flag)
          value = fpl2 (node, branch, time_flag)  +
     &           (epowr - savev) * rdt
          savev = epowr
        else
          savev = fel2 (node, branch, time_flag)
          value = 0.0
        end if
      else if (vtype .eq. ip_el2) then  !E=0.5*(L*I*I + sum(I*I*dL/dt))
        savev = savev  +  fpl2 (node, branch, time_flag) * dt
        value = fel2 (node, branch, time_flag)  +  savev
      else if (vtype .eq. ip_l2) then
        value = zlrechk (node, branch)
      else if (vtype .eq. ip_fl2) then
        value = ffl2 (node, branch, time_flag)
      else if (vtype .eq. ip_ql2) then
        value = savev  +  fiout (node, branch, time_flag) * dt
        savev = value
      else if (vtype .eq. ip_l2e) then
        value = zlr(node, branch)
c
c Source block.
c
      else if (vtype .eq. ip_vsrc) then
        if ((blktype .eq. voltsource) .or.
     &      (blktype .eq. cendsource) .or.
     &      (blktype .eq. csclsource)) then
          value = fvblk (node, branch, time_flag)
c          write(9,*) 'time = ',time,
c     & '     node = ',node,'     branch = ',branch,
c     & '     time_flag = ', time_flag,
c     & '     v(node,branch) = ',v(node,branch),
c     & '     vn(node,branch) = ',vn(node,branch)
        else if ((blktype .eq. currsource) .or.
     &           (blktype .eq. vendsource)) then
          value = fvblk (node+1, branch, time_flag)
        end if
      else if (vtype .eq. ip_isrc) then
        value = fiout (node, branch, time_flag)
      else if (vtype .eq. ip_psrc) then
        if (blktype .eq. voltsource) then
          value = fiout (node,branch, time_flag) * 
     &            fvblk (node, branch, time_flag)
        else if ((blktype .eq. cendsource) .or.
     &           (blktype .eq. csclsource)) then
          value = -1.0 * fiout (node,branch, time_flag) * 
     &                   fvblk (node, branch, time_flag)
        else if (blktype .eq. currsource) then
          value = fiout (node,branch, time_flag) * 
     &            fvblk (node+1, branch, time_flag)
        else if (blktype .eq. vendsource) then
          value = -1.0 * fiout (node,branch, time_flag) * 
     &                   fvblk (node+1, branch, time_flag)
        end if
      else if (vtype .eq. ip_esrc) then
        if (blktype .eq. voltsource) then
          value = savev + 
     &            fiout (node,branch, time_flag) * 
     &            fvblk (node, branch, time_flag) * dt
          savev = value
        else if ((blktype .eq. cendsource) .or.
     &           (blktype .eq. csclsource)) then
          value = savev -
     &            fiout (node,branch, time_flag) * 
     &            fvblk (node, branch, time_flag) * dt
          savev = value
        else if (blktype .eq. currsource) then
          value = savev +
     &            fiout (node,branch, time_flag) * 
     &            fvblk (node+1, branch, time_flag) * dt
          savev = value
        else if (blktype .eq. vendsource) then
          value = savev -
     &            fiout (node,branch, time_flag) * 
     &            fvblk (node+1, branch, time_flag) * dt
          savev = value
        end if
      else if (vtype .eq. ip_qsrc) then
        value = savev   +  fiout (node, branch, time_flag) * dt
        savev = value
c
c Generic input to blocks.
c
      else if (vtype .eq. ip_vin) then
        value = fvblk (node, branch, time_flag)
      else if (vtype .eq. ip_iin) then
        value = fiin (node, branch, time_flag)
      else if (vtype .eq. ip_pin) then
        value = fvblk (node, branch, time_flag) * 
     &          fiin (node, branch, time_flag)
      else if (vtype .eq. ip_ein) then
        value = savev +
     &          fvblk (node, branch, time_flag) * 
     &          fiin (node, branch, time_flag) * dt
        savev = value
      else if (vtype .eq. ip_qin) then
        value = savev + fiin (node, branch, time_flag) * dt
        savev = value
c
c Generic electrical output from blocks.
c
      else if (vtype .eq. ip_vout) then
        value = fvblk (node, branch, time_flag)
      else if (vtype .eq. ip_iout) then
        value = fiout (node, branch, time_flag)
      else if (vtype .eq. ip_pout) then
        value = fvblk (node, branch, time_flag) * 
     &          fiout (node, branch, time_flag)
      else if (vtype .eq. ip_eout) then
        value = savev +
     &          fvblk (node, branch, time_flag) * 
     &          fiout (node, branch, time_flag) * dt
        savev = value
      else if (vtype .eq. ip_qout) then
        value = savev + fiout (node, branch, time_flag) * dt
        savev = value
c
c Foil implosion parameters not available above.
c Values passed through common block FOILPARM.
c
      else if (vtype .eq. ip_frad) then
        value = foilrad
      else if (vtype .eq. ip_fvel) then
        value = foilvel
      else if (vtype .eq. ip_facc) then
        value = foilacc
      else if (vtype .eq. ip_fke) then
        value = foilke
      else if (vtype .eq. ip_yw_al) then
        value = yw_al
      else if (vtype .eq. ip_ym_al) then
        value = ym_al
      else if (vtype .eq. ip_yw_ar) then
        value = yw_ar
      else if (vtype .eq. ip_ym_ar) then
        value = ym_ar
      else if (vtype .eq. ip_yw_cu) then
        value = yw_cu
      else if (vtype .eq. ip_ym_cu) then
        value = ym_cu
      else if (vtype .eq. ip_yw_kr) then
        value = yw_kr
      else if (vtype .eq. ip_ym_kr) then
        value = ym_kr
      else if (vtype .eq. ip_yw_xe) then
        value = yw_xe
      else if (vtype .eq. ip_ym_xe) then
        value = ym_xe
c
      else if (vtype .eq. ip_srad) then
        value = shellrad
      else if (vtype .eq. ip_svel) then
        value = shellvel
      else if (vtype .eq. ip_sacc) then
        value = shellacc
      else if (vtype .eq. ip_ske) then
        value = shellke
      else if (vtype .eq. ip_smass) then
        value = shellm
      else if (vtype .eq. ip_srad1) then
        value = shellradius(1)
      else if (vtype .eq. ip_srad2) then
        value = shellradius(2)
      else if (vtype .eq. ip_srad3) then
        value = shellradius(3)
      else if (vtype .eq. ip_srad4) then
        value = shellradius(4)
      else if (vtype .eq. ip_srad5) then
        value = shellradius(5)
      else if (vtype .eq. ip_scur1) then
        value = shellcurr(1)
      else if (vtype .eq. ip_scur2) then
        value = shellcurr(2)
      else if (vtype .eq. ip_scur3) then
        value = shellcurr(3)
      else if (vtype .eq. ip_scur4) then
        value = shellcurr(4)
      else if (vtype .eq. ip_scur5) then
        value = shellcurr(5)

c
c MFI CB parameters not available above.
c
      else if (vtype .eq. ip_efld) then
        value = var_model(9,invarl(inumout))
      else if (vtype .eq. ip_bfld) then
        value = var_model(10,invarl(inumout))
      else if (vtype .eq. ip_xmfi) then
        value = var_model(11,invarl(inumout))
c
c Toms sw parameter, channel radius not available above.
c
      else if (vtype .eq. ip_fc) then
c        value = var_model(8,invarl(inumout))
        value = radch1
c
c Gas puff implosion parameters not available above.
c Values passed through common block FIPARM.
c
      else if (vtype .eq. ip_grad) then
        value = gasrad
      else if (vtype .eq. ip_gvel) then
        value = gasvel
      else if (vtype .eq. ip_gacc) then
        value = gasacc
      else if (vtype .eq. ip_gke) then
        value = gaske
c
c Zflow model parameters available for plotting
c
      else if (vtype .eq. ip_zflow) then
        value = var_model(12,invarl(inumout))   
      else if (vtype .eq. ip_gzflow) then
        value = var_model(13,invarl(inumout))
c
c ---------------------------------------------------------------------
c KWS - new parameters for Zflow Plasma Loss Current Model
c ---------
c
c
c Zflow Plasma Loss Current Model parameters available for plotting
c
      else if (vtype .eq. ip_zloss) then
        value = var_model(6,invarl(inumout))   
      else if (vtype .eq. ip_gloss) then
        value = var_model(7,invarl(inumout))   
c
c
c MITL Model parameters available for plotting
c
      else if (vtype .eq. ip_aloss) then
        value = pin(10,ipinblk,branch) 
      else if (vtype .eq. ip_closs) then
        value = pin(11,ipinblk,branch)
c
c ---------------------------------------------------------------------
c
c Outputs for the Measure Zflow and Cathode Current Block
c
c ---------------------------------------------------------------------
c
c Measure Zflow and Cathode Current block.
c
      else if (vtype .eq. ip_icathode) then
        value = ccathode(mzflowblock)
      else if (vtype .eq. ip_iplasma) then
        value = cplasma(mzflowblock)
      else if (vtype .eq. ip_zot) then
        value = measdzflow(mzflowblock)
c
c ---------------------------------------------------------------------
c
c  User variables. The values are stored in the uservars array based
c   on block and branch, rather than node and branch as in the other
c   variable arrays.
c
      else if (vtype .eq. ip_u1) then
         value = uservars(block,branch,1)
      else if (vtype .eq. ip_u2) then
         value = uservars(block,branch,2)
      else if (vtype .eq. ip_u3) then
         value = uservars(block,branch,3)
      else if (vtype .eq. ip_u4) then
         value = uservars(block,branch,4)
      else if (vtype .eq. ip_u5) then
         value = uservars(block,branch,5)
      else if (vtype .eq. ip_u6) then        
         value = uservars(block,branch,6)
      else if (vtype .eq. ip_u7) then
         value = uservars(block,branch,7)
      else if (vtype .eq. ip_u8) then
         value = uservars(block,branch,8)
      else if (vtype .eq. ip_u9) then
         value = uservars(block,branch,9)
      else if (vtype .eq. ip_u10) then
         value = uservars(block,branch,10)
c
      else if (vtype .eq. ip_pdline) then
        psum = 0.0
c      *Sum the power dissipated on each resistor in the line
        do i = node, lastnode
           psum = fpr1 (i, branch, time_flag) + psum
        enddo
        value = psum
c
      elseif (vtype .eq. ip_edline) then
         esum = 0.0
c      *Sum the energy dissipated on each resistor in the line
         do i = node, lastnode
            esum = fpr1(i,branch,time_flag)*dt + esum
         enddo
         value = savev + esum
         savev = value
c
      else if (vtype .eq. ip_eline) then
c
c   First get the energy stored in all of the capacitors in the line
c
         psum = 0.0
         do i = node, lastnode
            psum = fpc1(i, branch, time_flag)*dt + psum
         enddo
         savev = savev  +  psum
         esum = 0.0
         do i = node, lastnode
            esum = fec1(i, branch, time_flag) + esum
         enddo
         cvalue = esum  +  savev
c
c  Then get the energy stored in all of the the inductors in the line
c
         psum = 0.0
         do i = node, lastnode
            psum = fpl2(i, branch, time_flag)*dt + psum
         enddo
         savev2 = savev2  +  psum
         esum = 0.0
         do i = node, lastnode
            esum = fel2(i, branch, time_flag) + esum
         enddo
         ivalue = esum  +  savev2
c
c  Add the energy stored in the capacitors to the energy stored in the
c   inductors to get the total energy stored in the line
c
         value = cvalue + ivalue
c
      else if (vtype .eq. ip_pline) then
c
c   First get the power stored in all of the capacitors in the line
c
         esum = 0.0
         do i = node, lastnode
            esum = fec1(i, branch, time_flag) + esum
         enddo

         if (time .gt. 0.0) then
            psum = 0.0
            do i = node, lastnode
               psum = fpc1(i, branch, time_flag) + psum
            enddo
            cvalue = psum  +  (esum - savev) * rdt
         else
            cvalue = 0.0
         endif
         savev = esum
c
c  Then get the power stored in all the inductors in the line
c
         esum = 0.0
         do i = node, lastnode
            esum = fel2(i, branch, time_flag) + esum
         enddo

         if (time .gt. 0.0) then
            psum = 0.0
            do i = node, lastnode
               psum = fpl2(i, branch, time_flag) + psum
            enddo
            ivalue = psum  +  (esum - savev2) * rdt
         else
            ivalue = 0.0
         endif
         savev2 = esum
c
c  Add the power stored in the capacitors to the power stored in the
c   inductors to get the total power stored in the line
c
         value = cvalue + ivalue
c
      end if
c
      return
      end
