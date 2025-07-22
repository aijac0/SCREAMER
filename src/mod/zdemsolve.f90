
!----------------------------------------------------------------------
!    @(#)zdemsolve.f90
!----------------------------------------------------------------------
! *****************************************************************************

module zdemsolve
      use zdemmax       ! Set dimensions of scratch arrays (dependent on zdemmax).
      implicit none

!     
!  Solver arrays
!

      real*8 a(max_a)
      real*8 a_prl(max_a)
      real*8 am_band(max_am, max_am)
      real*8 rhs_band(max_am)

end module zdemsolve