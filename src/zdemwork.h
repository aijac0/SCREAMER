!----------------------------------------------------------------------
!    @(#)zdemwork.h   version 1.2   created 02/16/99 09:52:42
!    Last modified:    4-Jan-1999 16:07:34   klf 
!_Groups @(#) screamer
!----------------------------------------------------------------------
! *****************************************************************************
!  This is the common block used for the ZDEM working arrays.
!  To get this into the program, use
!    INCLUDE  'zdemwork.h'   .
! *****************************************************************************
!   file zdemwork.h   kws   Mar. 2, 1994
! 2014-02-06 RBS: Reordered the integer variable to the last location in
!             the circuit common. This is to make 64 bit padding work.
! 2014-04-11 RBS: I changed the /circuit/ dimension call to explicit
!             real and explicit integer
! 2014-06-10 YG: Create a new real*8 array, am_band(max_bb,max_bb),
! 2014-06-10 YG: Create a new real*8 array, rhs_band(max_bb)
! 2014-12-12 RBS: Changed the size of am_band and rhs to max_am
! 2015-03-25 RBS: DP x vector removed
! 2015-03-25 RBS: DP a vector removed from common and placed explicitly
!                 in main loop
! 2015-03-25 RBS: DP aa and bb vectors removed from common. They are no
!                 longer used in Screamer.
! 2015-03-25 RBS: DP am_band matrix and lhs_band vector removed from
!                 common. They are explicitly defined in main_loop.f.
! 2015-12-25 YG:  Put solver common blocks in this file, i.e. 
!                 a, am_band and rhs_band common block back in this file.
! 2019-08-19 YG:  Added new solver array for parallel version, a_prl
! 2025-07-11 AJC: Made circuit arrays dynamically allocated 
!                 and removed them from /circuit/ common block
!
! ----------------------------------------------------------------------
!
! Voltages, currents, circuit elements, etc.
! Arrays allocated in zdemalloc

      REAL, ALLOCATABLE :: v(:,:), vold(:,:), vn(:,:), zir(:,:)
      REAL, ALLOCATABLE :: zirn(:,:), zirold(:,:), zib(:,:), g(:,:)
      REAL, ALLOCATABLE :: zlr(:,:), c(:,:), rr(:,:), gdot(:,:)
      REAL, ALLOCATABLE :: zlrdot(:,:), cdot(:,:), rrdot(:,:)
      REAL, ALLOCATABLE :: zlrechk(:,:), cechk(:,:)

      REAL :: vsour(max_branches)
      REAL :: uservars(max_blocks,max_branches,max_uservars)

      integer :: nr(max_branches)
!
      common /circuit/ vsour, uservars, nr
!
! Various indexing arrays.
!
      INTEGER :: iflg(max_nodes,max_branches)
      INTEGER :: indexb(2,max_branches)
      INTEGER :: indexv(3,max_var_elem)
      INTEGER :: indexvs(2,max_volt_source)
      INTEGER :: indexcs(2,max_curr_source)
      INTEGER :: indexmitl(5,max_mitl)
      INTEGER :: indextrnf(5,max_transformer)
      INTEGER :: lastvoltf_time(max_volt_source)
      INTEGER :: lastcurrf_time(max_curr_source)
      INTEGER :: lasttabm_time(max_var_elem)
      INTEGER :: nbv(0:max_branches)
      INTEGER :: nbe(max_branches)
      INTEGER :: nadd_array(max_branches)
!
      common /indexing/ iflg, indexb, indexv, indexvs, indexcs, indexmitl, indextrnf, lastvoltf_time, lastcurrf_time, lasttabm_time, nbv, nbe, nadd_array
     
!     
!  Solver common blocks
!

      real*8 a(max_a)
      real*8 a_prl(max_a)
      real*8 am_band(max_am, max_am)
      real*8 rhs_band(max_am)
      common /solver/ a, a_prl, am_band, rhs_band
     
