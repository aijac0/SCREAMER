!----------------------------------------------------------------------
!    @(#)zdemmax.f90   version 1.2   created 02/16/99 09:49:17
!    Last modified:    4-Jan-1999 16:07:34   klf 
!_Groups @(#) screamer
!----------------------------------------------------------------------
! *********************************************************************
! This is a file giving the array dimensions for ZDEM.
!    file zdemmax.f90
!
! ---------------------------------------------------------------------
!
! Modifications:
! 05/30/95, Reduced max_branches to 15 from 20
! 2012-04-11 RBS: Increased max_branches to 65 as per Stygar request
! 2012-04-11 RBS: Increased max_var_elem to 65 as per Stygar request
! 2014-05-01 RBS: Create a new max parameter, max_switch_points
! 2014-06-10 YG: Create a new max parameter, max_band
! 2014-06-10 YG: Create a new max parameter, max_band_matrix
! 2014-06-10 YG: Create a new max parameter, max_band_rhs
! 2014-12-12 RBS: Created a new max parameter, max_am = 2*max_nodes
! 2016-06-07 RBS: Increased max_plot_points to 20,001
! 2017-01-02 RBS: Added the max points for skin depth
! 2018-02-22 RBS: Increased max_elem_parms to 12 for MITL cur output
! 2018-05-29 RBS: Increased max_branches to 250 as per Stygar request
! 2019-01-23 RBS: Increased max_iin_specs from 5 to 12 to match
!                 max_elem_parms - used in zdemcomm.h
! 2025-07-14 AJC: Moved file into a module
!
! ---------------------------------------------------------------------
!

module zdemmax
      implicit none

      integer, parameter :: max_branches         =   25
      integer, parameter :: max_blocks           =   300
      integer, parameter :: max_nodes            =  40000
      integer, parameter :: max_vars             =     5
      integer, parameter :: max_cols             = 3*max_vars + 1
      integer, parameter :: max_elem_parms       =    12
      integer, parameter :: max_iin_specs        =    12
      integer, parameter :: maxout               =   400
      integer, parameter :: max_plot_points      = 20001
      integer, parameter :: max_table_points     =  1001
      integer, parameter :: max_var_elem         =    65
      integer, parameter :: max_switch_points    =    40
      integer, parameter :: max_skin_points      =  1001
      integer, parameter :: max_skin_timepts     = 20001
!      integer, parameter :: max_band            =    40
!
! Don't reduce max_var_parms below 15
!
      integer, parameter :: max_var_parms       =   40
      integer, parameter :: max_init_cond       =  max_branches + 1
      integer, parameter :: max_volt_source     =  max_branches + 1
      integer, parameter :: max_volt_func_parms =  2*max_table_points + 2
      integer, parameter :: max_curr_source     =  max_branches + 1
      integer, parameter :: max_curr_func_parms =  2*max_table_points + 2
      integer, parameter :: max_tablem_vals     =  2*max_table_points + 2
      integer, parameter :: max_trline          =  6000
      integer, parameter :: max_mitl            =  100
      integer, parameter :: max_trline_nodes    =  6001
      integer, parameter :: max_transformer     =    5
      integer, parameter :: max_uservars        =   10
      integer, parameter :: max_mzflowblocks    =   20
!
! Set (dependent on above) dimensions of scratch arrays.
!
      integer, parameter :: max_bb           = max_nodes * max_vars * max_branches
      integer, parameter :: max_cc           = max_bb
      integer, parameter :: max_a            = max_bb * max_cols
      integer, parameter :: max_x            = max_bb
      integer, parameter :: max_am           = max_nodes * 2
!      integer, parameter :: max_band_matrix  = max_bb*max_band
!      integer, parameter :: max_band_rhs     = max_bb

! Parameters for the multiple shell model
!
      integer, parameter :: max_shells = 20

end module zdemmax