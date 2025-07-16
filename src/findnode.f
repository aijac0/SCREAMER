      integer function ifnodreq (otype, lastnode, numnodes)
c
c Define passed variables
c
      integer  otype, lastnode, numnodes
c
c  Modifications:
c 1995-03-07 MLK: Change include filenames to be 8 characters or less
c 1997-02-14 MLK: Changed `type` variable to `otype` so as not to
c                   conflict with Fortran 90 TYPE statement
c 2015-06-23 RBS: Initialized ifnodreq to remove a compiler warning
c
c Find the node associated with this output request.
c
c
c Include variable definitions
c
      include 'zdemmax.h'
      include 'zdempprm.h'
      include 'zdemout.h'
c
c Define internal variables
c
      integer  firstnode
c
c Initialize
c
      ifnodreq = 0
      firstnode = lastnode - numnodes + 1
c
c
c Set the output variable node based on plot type.
c
      if (otype .eq. ip_vr1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_vr2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_vr3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_ir1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_ir2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_ir3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_pr1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pr2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pr3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_er1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_er2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_er3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_r1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_r2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_r3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_qr1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_qr2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_qr3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_vc1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_vc3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_ic1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_ic3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_pc1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pc3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_ec1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_ec3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_c1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_c3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_qc1) then
        ifnodreq = firstnode
      else if (otype .eq. ip_qc3) then
        ifnodreq = lastnode
      else if (otype .eq. ip_c1e) then
        ifnodreq = firstnode
      else if (otype .eq. ip_c3e) then
        ifnodreq = lastnode
      else if (otype .eq. ip_vl2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_il2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pl2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_el2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_l2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_fl2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_ql2) then
        ifnodreq = firstnode
      else if (otype .eq. ip_l2e) then
        ifnodreq = firstnode
      else if (otype .eq. ip_vsrc) then
        ifnodreq = firstnode
      else if (otype .eq. ip_isrc) then
        ifnodreq = firstnode
      else if (otype .eq. ip_psrc) then
        ifnodreq = firstnode
      else if (otype .eq. ip_esrc) then
        ifnodreq = firstnode
      else if (otype .eq. ip_qsrc) then
        ifnodreq = firstnode
      else if (otype .eq. ip_vin) then
        ifnodreq = firstnode
      else if (otype .eq. ip_iin) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pin) then
        ifnodreq = firstnode
      else if (otype .eq. ip_ein) then
        ifnodreq = firstnode
      else if (otype .eq. ip_qin) then
        ifnodreq = firstnode
      else if (otype .eq. ip_vout) then
        ifnodreq = lastnode
      else if (otype .eq. ip_iout) then
        ifnodreq = lastnode
      else if (otype .eq. ip_pout) then
        ifnodreq = lastnode
      else if (otype .eq. ip_eout) then
        ifnodreq = lastnode
      else if (otype .eq. ip_qout) then
        ifnodreq = lastnode
c
c  If this is a user variable, set the node to the first
c    node. We really do not need the node number to process
c    user variables, but we get it anyways to keep everything
c    consistant.
c
      else if (otype .eq. ip_u1) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u2) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u3) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u4) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u5) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u6) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u7) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u8) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u9) then
         ifnodreq = firstnode
      else if (otype .eq. ip_u10) then
         ifnodreq = firstnode
c
c  If this is a transmission line energy or power check (stored or
c    dissipated), set the node to the first
c    node. 
c
      else if (otype .eq. ip_edline) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pdline) then
        ifnodreq = firstnode
      else if (otype .eq. ip_eline) then
        ifnodreq = firstnode
      else if (otype .eq. ip_pline) then
        ifnodreq = firstnode
      end if
c
      return
      end
