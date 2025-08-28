!----------------------------------------------------------------------
!    @(#)zdemvars.f90   version 1.2.1   created 11/19/04 17:50:07
!    Last modified:   19-Nov-2004 17:50:07   MB 
!C_Groups @(#)
!----------------------------------------------------------------------
! *****************************************************************************
!  This is the common block used for sharing data between zdem.f and models.f
! *****************************************************************************
! 
!  file zdemvars.h    
!
! ----------------------------------------------------------------------
!  Modified:
!
! 2004-11-19 MB:  Created by MB when separated from zdemcomm.
! 2012-04-13 RBS: zdemvars was mislabeled in the first line. Corrected.
! 2014-02-06 RBS: Change real*4 to real
!                 Define real for variables in misc_for_models
!                 Reorder misc_for_models 64-bit first
! 2014-04-04 RBS: Added mdl common drdt variable and real definition
! 2014-04-11 RBS: Noticed that htd2 had not been defined real - added
! 2014-04-11 RBS: Defined reals and integer in last common blocks
! 2015-06-18 RBS: Common block /misc for models/ does not get used in
!                 any subroutine but models. We do not need this common.
! 2015-06-18 RBS: Common block /mdl_vars/ does not get used in
!                 any subroutine but models. We do not need this common.
! 2015-06-18 RBS: Common block /misc_energy_checks/  reals do not get
!                 used in any subroutine but energy_checks.
!                 We do not need the reals in this common.
! 2025-08-28 AJC: Moved file into a module
! ----------------------------------------------------------------------
!      

module zdemvars

      integer :: ibranch, itab_counter
      
      real :: rht, timehalf, htd2

!
!  User variables, user must also include in his
!  user subroutine(s) if he wishes to use the user variable feature
!
      real :: u1, u2, u3, u4, u5, u6, u7, u8, u9, u10
      
!
! Variables needed for the energy check in main_loop, start_run,
! energy_checks
!
      real :: econ, eres, esour, elosscap, elossind, error, ecap, eind

!
! Misc variables node1 and node2 are only used in energy_checks and
! could be moved to energy_checks.f.
!
      integer :: i, ib, node, node1, node2, nrx

end module zdemvars