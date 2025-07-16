c----------------------------------------------------------------------
c    @(#)zdemmax.h   version 1.2   created 02/16/99 09:49:17
c    Last modified:    4-Jan-1999 16:07:34   klf 
C_Groups @(#) screamer
c----------------------------------------------------------------------
c *********************************************************************
c This is a file giving the array dimensions for ZDEM.
c    file zdemmax.h
c
c ---------------------------------------------------------------------
c
c Modifications:
c 05/30/95, Reduced max_branches to 15 from 20
c 2012-04-11 RBS: Increased max_branches to 65 as per Stygar request
c 2012-04-11 RBS: Increased max_var_elem to 65 as per Stygar request
c 2014-05-01 RBS: Create a new max parameter, max_switch_points
c 2014-06-10 YG: Create a new max parameter, max_band
c 2014-06-10 YG: Create a new max parameter, max_band_matrix
c 2014-06-10 YG: Create a new max parameter, max_band_rhs
c 2014-12-12 RBS: Created a new max parameter, max_am = 2*max_nodes
c 2016-06-07 RBS: Increased max_plot_points to 20,001
c 2017-01-02 RBS: Added the max points for skin depth
c 2018-02-22 RBS: Increased max_elem_parms to 12 for MITL cur output
c 2018-05-29 RBS: Increased max_branches to 250 as per Stygar request
c 2019-01-23 RBS: Increased max_iin_specs from 5 to 12 to match
c                 max_elem_parms - used in zdemcomm.h
c
c ---------------------------------------------------------------------
c
      parameter (max_branches         =   250)
      parameter (max_blocks           =   300)
      parameter (max_nodes            = 40000)
      parameter (max_vars             =     5)
      parameter (max_cols             = 3*max_vars + 1)
      parameter (max_elem_parms       =    12)
      parameter (max_iin_specs        =    12)
      parameter (maxout               =   400)
      parameter (max_plot_points      = 20001)
      parameter (max_table_points     =  1001)
      parameter (max_var_elem         =    65)
      parameter (max_switch_points    =    40)
      parameter (max_skin_points      =  1001)
      parameter (max_skin_timepts     = 20001)
c      parameter (max_band            =    40)
c
c Don't reduce max_var_parms below 15
c
      parameter (max_var_parms       =   40)
      parameter (max_init_cond       =  max_branches + 1)
      parameter (max_volt_source     =  max_branches + 1)
      parameter (max_volt_func_parms =  2*max_table_points + 2)
      parameter (max_curr_source     =  max_branches + 1)
      parameter (max_curr_func_parms =  2*max_table_points + 2)
      parameter (max_tablem_vals     =  2*max_table_points + 2)
      parameter (max_trline          =  6000)
      parameter (max_mitl            =  100)
      parameter (max_trline_nodes    =  6001)
      parameter (max_transformer     =    5)
      parameter (max_uservars        =   10)
      parameter (max_mzflowblocks    =   20)
c
c Set (dependent on above) dimensions of scratch arrays.
c
      parameter (max_bb           = max_nodes * max_vars * max_branches)
      parameter (max_cc           = max_bb)
      parameter (max_a            = max_bb * max_cols)
      parameter (max_x            = max_bb)
      parameter (max_am           = max_nodes * 2)
c      parameter (max_band_matrix  = max_bb*max_band)
c      parameter (max_band_rhs     = max_bb)
c
c Parameters for the multiple shell model
c
      parameter (max_shells = 20)
