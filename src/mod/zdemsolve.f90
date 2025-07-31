
!----------------------------------------------------------------------
!    @(#)zdemsolve.f90
!----------------------------------------------------------------------
! *****************************************************************************

module zdemsolve
      use zdemmax       ! Set dimensions of scratch arrays (dependent on zdemmax).
      implicit none

!     
!  Solver arrays
! Dynamically allocated arrays are initialized in zdemalloc.f90 
!

      REAL(KIND=8), ALLOCATABLE :: a(:)
      REAL(KIND=8), ALLOCATABLE :: a_prl(:)
      REAL(KIND=8), ALLOCATABLE :: am_band(:,:)
      REAL(KIND=8), ALLOCATABLE :: rhs_band(:)

end module zdemsolve