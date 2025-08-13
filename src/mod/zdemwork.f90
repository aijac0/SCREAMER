!----------------------------------------------------------------------
!    @(#)zdemwork.f90   version 1.2   created 02/16/99 09:52:42
!    Last modified:    4-Jan-1999 16:07:34   klf 
!_Groups @(#) screamer
!----------------------------------------------------------------------
! *****************************************************************************
!  This is the common block used for the ZDEM working arrays.
!  To get this into the program, use
!    use zdemwork   .
! *****************************************************************************
!   file zdemwork.f90   kws   Mar. 2, 1994
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
! 2025-07-14 AJC: Arrays now dynamically allocated
! 2025-07-21 AJC: Moved file into a module, removed common blocks
! ----------------------------------------------------------------------
module zdemwork
      use zdemsolve
      use zdemcomm
      implicit none

!
! Whether or not arrays in module are allocated
!
      logical, private :: is_allocated = .false.

!
! These parameters reflect actual size of problem
! Initialized with upper bounds set by zdemmax
! Recalculated after reading input
!
      integer, private :: n_branches = max_branches
      integer, private :: n_blocks   = max_blocks
      integer, private :: n_nodes    = max_nodes
      integer, private :: n_vars     = max_vars


! Voltages, currents, circuit elements, etc.
      real, allocatable    :: v(:,:), vold(:,:), vn(:,:), zir(:,:)
      real, allocatable    :: zirn(:,:), zirold(:,:), zib(:,:), g(:,:)
      real, allocatable    :: zlr(:,:), c(:,:), rr(:,:), gdot(:,:)
      real, allocatable    :: zlrdot(:,:), cdot(:,:), rrdot(:,:)
      real, allocatable    :: zlrechk(:,:), cechk(:,:)
      real, allocatable    :: vsour(:), uservars(:,:,:)
      integer, allocatable :: nr(:)
!
! Various indexing arrays.
!
      integer, allocatable :: iflg(:,:)
      integer, allocatable :: indexb(:,:)
      integer, allocatable :: indexv(:,:)
      integer, allocatable :: indexvs(:,:)
      integer, allocatable :: indexcs(:,:)
      integer, allocatable :: indexmitl(:,:)
      integer, allocatable :: indextrnf(:,:)
      integer, allocatable :: lastvoltf_time(:)
      integer, allocatable :: lastcurrf_time(:)
      integer, allocatable :: lasttabm_time(:)
      integer, allocatable :: nbv(:)
      integer, allocatable :: nbe(:)
      integer, allocatable :: nadd_array(:)

end module zdemwork