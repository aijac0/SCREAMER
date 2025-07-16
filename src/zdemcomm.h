c----------------------------------------------------------------------
c    @(#)zdemcomm.h   version 1.2   created 02/16/99 09:46:07
c    Last modified:   19-Jan-1999 13:54:50   klf 
C_Groups @(#)
c----------------------------------------------------------------------
c **********************************************************************
c  This is the common block used for entering data into zdem when using
c  read_screamer_data  To get this into the program, use include 'zdemcomm.h'
c **********************************************************************
c  file zdemcomm.h    
c
c ----------------------------------------------------------------------
c  Modified:
c 1994-07-03 KWS: Added pointer variables for finding
c                   variable element parameters.
c 1995-07-19 MLK: Added variable to store status of detail for
c                   log file printing (detail_prints)
c 2014-02-06 RBS: Changed real*4 to real
c                  Explicit definition of real, integer common/input/
c                  Reorder of variables in common/input/ 64-bit first
c 2014-05-01 RBS: Create a new real vector for switch table data values
c                  the vector name is switch_time
c 2014-05-04 RBS: Changed integer*4 to integer
c 2015-06-22 RBS: iswitch defined in integer but not placed in common -
c                 added to common
c 2017-01-02 RBS: Added dcurr, H_field for skin depth routine
c 2018-07-20 RBS: Add itabnum in integer declaration and in common.
c ----------------------------------------------------------------------
c
c Input arrays
c
      real
     & currf_parms(max_curr_func_parms, max_curr_source),
     & delta_i(max_skin_timepts,max_var_elem), ht,
     & H_field(max_skin_points),
     & pin(max_elem_parms,max_blocks,max_branches), res_time,
     & switch_time(max_switch_points),
     & tablem_vals (max_tablem_vals, max_var_elem), tmax,
     & var_model(max_var_parms, max_var_elem),
     & value_init(max_init_cond),
     & voltf_parms(max_volt_func_parms, max_volt_source)

      integer
     & icbranch_end(max_branches),
     & icurrf(max_curr_source),
     & iin(max_iin_specs,max_blocks,max_branches),
     & imitl_type(max_mitl), iset,
     & iswitch, itabnum,
     & itrl_type(max_trline), itypcend(max_branches),
     & ivar_block(max_var_elem), ivar_block_num(max_var_elem),
     & ivar_type(max_var_elem),
     & ivbranch_end(max_branches),
     & ivoltf(max_volt_source),
     & nb, nbk(max_branches), ncurrsource, ninit_cond, nmitline,
     & nprint, ntransformer, ntransline,
     & num_currf_parms(max_curr_source), num_tablem_vals(max_var_elem),
     & num_var_parms(max_var_elem), num_voltf_parms(max_volt_source),
     & nvar, nvoltsource

      common /input/
     & currf_parms,
     & delta_i, ht,
     & H_field,
     & pin, res_time,
     & switch_time,
     & tablem_vals, tmax,
     & var_model,
     & value_init,
     & voltf_parms,
     & icbranch_end,
     & icurrf,
     & iin,
     & imitl_type, iset,
     & iswitch, itabnum,
     & itrl_type, itypcend,
     & ivar_block, ivar_block_num,
     & ivar_type,
     & ivbranch_end,
     & ivoltf,
     & nb, nbk, ncurrsource, ninit_cond, nmitline,
     & nprint, ntransformer, ntransline,
     & num_currf_parms, num_tablem_vals,
     & num_var_parms, num_voltf_parms,
     & nvar, nvoltsource

      character  title*80
      common /title_block/ title
c
c Some pointers for output control.
c
      integer
     & nsteps, plot_grid, maxfpts, echoset, detail_prints,
     & invarl(maxout), inumout

      common /output/
     & nsteps, plot_grid, maxfpts, echoset, detail_prints,
     & invarl, inumout
c
c Initial energy in inductors and capacitors due
c to initial current or voltage.
c
      real              ecapsource, eindsource
      common /einitial/ ecapsource, eindsource
