c----------------------------------------------------------------------
c    @(#)zdemwork.h   version 1.2   created 02/16/99 09:52:42
c    Last modified:    4-Jan-1999 16:07:34   klf 
C_Groups @(#) screamer
c----------------------------------------------------------------------
c *****************************************************************************
c  This is the common block used for the ZDEM working arrays.
c  To get this into the program, use
c    INCLUDE  'zdemwork.h'   .
c *****************************************************************************
c   file zdemwork.h   kws   Mar. 2, 1994
c 2014-02-06 RBS: Reordered the integer variable to the last location in
c             the circuit common. This is to make 64 bit padding work.
c 2014-04-11 RBS: I changed the /circuit/ dimension call to explicit
c             real and explicit integer
c 2014-06-10 YG: Create a new real*8 array, am_band(max_bb,max_bb),
c 2014-06-10 YG: Create a new real*8 array, rhs_band(max_bb)
c 2014-12-12 RBS: Changed the size of am_band and rhs to max_am
c 2015-03-25 RBS: DP x vector removed
c 2015-03-25 RBS: DP a vector removed from common and placed explicitly
c                 in main loop
c 2015-03-25 RBS: DP aa and bb vectors removed from common. They are no
c                 longer used in Screamer.
c 2015-03-25 RBS: DP am_band matrix and lhs_band vector removed from
c                 common. They are explicitly defined in main_loop.f.
c 2015-12-25 YG:  Put solver common blocks in this file, i.e. 
c                 a, am_band and rhs_band common block back in this file.
c 2019-08-19 YG:  Added new solver array for parallel version, a_prl
c
c ----------------------------------------------------------------------
c
c Voltages, currents, circuit elements, etc.
c
      real
     & v(max_nodes,max_branches),
     & vold(max_nodes,max_branches),
     & vn(max_nodes,max_branches),
     & vsour(max_branches),
     & zir(max_nodes,max_branches),
     & zirn(max_nodes,max_branches),
     & zirold(max_nodes,max_branches),
     & zib(max_nodes,max_branches),
     & g(max_nodes,max_branches),
     & zlr(max_nodes,max_branches),
     & c(max_nodes,max_branches),
     & rr(max_nodes,max_branches),
     & gdot(max_nodes,max_branches),
     & zlrdot(max_nodes,max_branches),
     & cdot(max_nodes,max_branches),
     & rrdot(max_nodes,max_branches),
     & zlrechk(max_nodes,max_branches),
     & cechk(max_nodes,max_branches),
     & uservars(max_blocks,max_branches,max_uservars)

      integer nr(max_branches)
c
      common /circuit/
     & v, vold, vn, vsour, zir, zirn, zirold, zib, g, zlr, c, rr,
     & gdot, zlrdot, cdot, rrdot, zlrechk, cechk, uservars,
     & nr
c
c Various indexing arrays.
c
      integer
     & iflg(max_nodes,max_branches),
     & indexb(2,max_branches),
     & indexv(3,max_var_elem),
     & indexvs(2,max_volt_source),
     & indexcs(2,max_curr_source),
     & indexmitl(5,max_mitl),
     & indextrnf(5,max_transformer),
     & lastvoltf_time(max_volt_source),
     & lastcurrf_time(max_curr_source),
     & lasttabm_time(max_var_elem),
     & nbv(0:max_branches),
     & nbe(max_branches),
     & nadd_array(max_branches)
c
      common /indexing/
     & iflg, indexb, indexv, indexvs, indexcs, indexmitl, indextrnf,
     & lastvoltf_time, lastcurrf_time, lasttabm_time, nbv, nbe, 
     & nadd_array
     
C     
C  Solver common blocks
C

      real*8 a(max_a),a_prl(max_a),am_band(max_am, max_am),
     &       rhs_band(max_am)
      common/solver/ a,a_prl, am_band, rhs_band
     
