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

contains

! -----------------------------------------------------------------------------
! Get values for parameters in module based on actual problem size
! Defaults to upper bound when input has not been read yet
! result := {n_branches, n_blocks, n_nodes, n_vars}
!
      function calcparm_zdemwork() result(arr)
            integer :: i
            integer :: nb = 5 ! REMOVE AFTER MOVING ZDEMCOMM TO MODULE
            integer, dimension(4) :: arr

! Check if required arrays are allocated
! nb     := total number of branches
! nbr    := total number of nodes
! nbk(i) := number of blocks in ith branch
            if (nb > 0) then
                  arr(1) = nb                          ! n_branches
                  arr(2) = 0                           ! n_blocks
                  arr(3) = 0 !nbr                         ! n_nodes
                  arr(4) = 0                           ! n_vars

! Aggregate number of blocks in all branches
                  do i=1, nb
                        arr(2) = arr(2) + nbk(i)                ! n_blocks
                  end do

! Default to upper bound
            else
                  arr(1) = max_branches
                  arr(2) = max_blocks
                  arr(3) = max_nodes
                  arr(4) = max_vars
            end if

      end function

! -----------------------------------------------------------------------------
! Change value of parameters in module based on actual problem size
! Defaults to upper bound when input has not been read yet
! result := whether or not parameters have changed
!
      function updateparm_zdemwork() result(has_changed)
            logical :: has_changed
            integer, dimension(4) :: arr

! Get new values for parameters
            arr = calcparm_zdemwork()

! Update parameters
            if (arr(1) .ne. n_branches) then
                  n_branches = arr(1)
                  has_changed = .true.
            endif
            if (arr(2) .ne. n_blocks) then
                  n_blocks = arr(2)
                  has_changed = .true.
            endif
            if (arr(3) .ne. n_nodes) then
                  n_nodes = arr(3)
                  has_changed = .true.
            endif
            if (arr(4) .ne. n_vars) then
                  n_vars = arr(4)
                  has_changed = .true.
            end if

      end function

! -----------------------------------------------------------------------------
! Allocate memory for arrays declared ALLOCATABLE in module 
!
      subroutine allocarry_zdemwork
            is_allocated = .true.
            if (.not. allocated(v))               allocate(v(max_nodes, max_branches))
            if (.not. allocated(vold))            allocate(vold(max_nodes, max_branches))
            if (.not. allocated(vn))              allocate(vn(max_nodes, max_branches))
            if (.not. allocated(zir))             allocate(zir(max_nodes, max_branches))
            if (.not. allocated(zirn))            allocate(zirn(max_nodes, max_branches))
            if (.not. allocated(zirold))          allocate(zirold(max_nodes, max_branches))
            if (.not. allocated(zib))             allocate(zib(max_nodes, max_branches))
            if (.not. allocated(g))               allocate(g(max_nodes, max_branches))
            if (.not. allocated(zlr))             allocate(zlr(max_nodes, max_branches))
            if (.not. allocated(c))               allocate(c(max_nodes, max_branches))
            if (.not. allocated(rr))              allocate(rr(max_nodes, max_branches))
            if (.not. allocated(gdot))            allocate(gdot(max_nodes, max_branches))
            if (.not. allocated(zlrdot))          allocate(zlrdot(max_nodes, max_branches))
            if (.not. allocated(cdot))            allocate(cdot(max_nodes, max_branches))
            if (.not. allocated(rrdot))           allocate(rrdot(max_nodes, max_branches))
            if (.not. allocated(zlrechk))         allocate(zlrechk(max_nodes, max_branches))
            if (.not. allocated(cechk))           allocate(cechk(max_nodes, max_branches))
            if (.not. allocated(vsour))           allocate(vsour(max_branches))
            if (.not. allocated(uservars))        allocate(uservars(max_blocks, max_branches, max_uservars))
            if (.not. allocated(nr))              allocate(nr(max_branches))
            if (.not. allocated(iflg))            allocate(iflg(max_nodes, max_branches))
            if (.not. allocated(indexb))          allocate(indexb(2, max_branches))
            if (.not. allocated(indexv))          allocate(indexv(3, max_var_elem))
            if (.not. allocated(indexvs))         allocate(indexvs(2, max_volt_source))
            if (.not. allocated(indexcs))         allocate(indexcs(2, max_curr_source))
            if (.not. allocated(indexmitl))       allocate(indexmitl(5, max_mitl))
            if (.not. allocated(indextrnf))       allocate(indextrnf(5, max_transformer))
            if (.not. allocated(lastvoltf_time))  allocate(lastvoltf_time(max_volt_source))
            if (.not. allocated(lastcurrf_time))  allocate(lastcurrf_time(max_curr_source))
            if (.not. allocated(lasttabm_time))   allocate(lasttabm_time(max_var_elem))
            if (.not. allocated(nbv))             allocate(nbv(0:max_branches))
            if (.not. allocated(nbe))             allocate(nbe(max_branches))
            if (.not. allocated(nadd_array))      allocate(nadd_array(max_branches))
      end subroutine

! -----------------------------------------------------------------------------
! Reallocate memory for arrays declared ALLOCATABLE in module 
!
      subroutine reallocarry_zdemwork
! TODO
      end subroutine

! -----------------------------------------------------------------------------
! Deallocate memory for arrays declared ALLOCATABLE in module 
!
      subroutine deallocarry_zdemwork
            is_allocated = .false.
            if (allocated(v))               deallocate(v)
            if (allocated(vold))            deallocate(vold)
            if (allocated(vn))              deallocate(vn)
            if (allocated(zir))             deallocate(zir)
            if (allocated(zirn))            deallocate(zirn)
            if (allocated(zirold))          deallocate(zirold)
            if (allocated(zib))             deallocate(zib)
            if (allocated(g))               deallocate(g)
            if (allocated(zlr))             deallocate(zlr)
            if (allocated(c))               deallocate(c)
            if (allocated(rr))              deallocate(rr)
            if (allocated(gdot))            deallocate(gdot)
            if (allocated(zlrdot))          deallocate(zlrdot)
            if (allocated(cdot))            deallocate(cdot)
            if (allocated(rrdot))           deallocate(rrdot)
            if (allocated(zlrechk))         deallocate(zlrechk)
            if (allocated(cechk))           deallocate(cechk)
            if (allocated(vsour))           deallocate(vsour)
            if (allocated(uservars))        deallocate(uservars)
            if (allocated(nr))              deallocate(nr)
            if (allocated(iflg))            deallocate(iflg)
            if (allocated(indexb))          deallocate(indexb)
            if (allocated(indexv))          deallocate(indexv)
            if (allocated(indexvs))         deallocate(indexvs)
            if (allocated(indexcs))         deallocate(indexcs)
            if (allocated(indexmitl))       deallocate(indexmitl)
            if (allocated(indextrnf))       deallocate(indextrnf)
            if (allocated(lastvoltf_time))  deallocate(lastvoltf_time)
            if (allocated(lastcurrf_time))  deallocate(lastcurrf_time)
            if (allocated(lasttabm_time))   deallocate(lasttabm_time)
            if (allocated(nbv))             deallocate(nbv)
            if (allocated(nbe))             deallocate(nbe)
            if (allocated(nadd_array))      deallocate(nadd_array)
      end subroutine
     
end module zdemwork