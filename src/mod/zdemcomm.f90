!----------------------------------------------------------------------
!    @(#)zdemcomm.f90   version 1.2   created 02/16/99 09:46:07
!    Last modified:   19-Jan-1999 13:54:50   klf 
!_Groups @(#)
!----------------------------------------------------------------------
! **********************************************************************
!  This is the common block used for entering data into zdem when using
!  read_screamer_data  To get this into the program, use include 'zdemcomm.h'
! **********************************************************************
!  file zdemcomm.f90  
!
! ----------------------------------------------------------------------
!  Modified:
! 1994-07-03 KWS: Added pointer variables for finding
!                   variable element parameters.
! 1995-07-19 MLK: Added variable to store status of detail for
!                   log file printing (detail_prints)
! 2014-02-06 RBS: Changed real*4 to real
!                  Explicit definition of real, integer common/input/
!                  Reorder of variables in common/input/ 64-bit first
! 2014-05-01 RBS: Create a new real vector for switch table data values
!                  the vector name is switch_time
! 2014-05-04 RBS: Changed integer*4 to integer
! 2015-06-22 RBS: iswitch defined in integer but not placed in common -
!                 added to common
! 2017-01-02 RBS: Added dcurr, H_field for skin depth routine
! 2018-07-20 RBS: Add itabnum in integer declaration and in common.
! 2025-08-11 AJC: Moved file into a module
! ----------------------------------------------------------------------

module zdemcomm
      use zdemmax
      implicit none

!
! Input arrays
!
      real :: ht, res_time, tmax
      real :: currf_parms(max_curr_func_parms, max_curr_source)
      real :: delta_i(max_skin_timepts,max_var_elem)
      real :: H_field(max_skin_points)
      real :: pin(max_elem_parms,max_blocks,max_branches)
      real :: switch_time(max_switch_points)
      real :: tablem_vals (max_tablem_vals, max_var_elem)
      real :: var_model(max_var_parms, max_var_elem)
      real :: value_init(max_init_cond)
      real :: voltf_parms(max_volt_func_parms, max_volt_source)

      integer :: iset, iswitch, itabnum, nb, nbr, ncurrsource, ninit_cond, nmitline
      integer :: nprint, ntransformer, ntransline, nvar, nvoltsource
      integer :: icbranch_end(max_branches)
      integer :: icurrf(max_curr_source)
      integer :: iin(max_iin_specs,max_blocks,max_branches)
      integer :: imitl_type(max_mitl)
      integer :: itrl_type(max_trline)
      integer :: itypcend(max_branches)
      integer :: ivar_block(max_var_elem)
      integer :: ivar_block_num(max_var_elem)
      integer :: ivar_type(max_var_elem)
      integer :: ivbranch_end(max_branches)
      integer :: ivoltf(max_volt_source)
      integer :: nbk(max_branches)
      integer :: num_currf_parms(max_curr_source)
      integer :: num_tablem_vals(max_var_elem)
      integer :: num_var_parms(max_var_elem)
      integer :: num_voltf_parms(max_volt_source)

      character :: title*80

!
! Some pointers for output control
!
      integer :: nsteps, plot_grid, maxfpts, echoset, detail_prints, inumout
      integer :: invarl(maxout)

! Initial energy in inductors and capacitors due to initial current or voltage
      real :: ecapsource, eindsource

end module zdemcomm