c **********************************************************************
c  Set various parameters for read_screamer_data and use INCLUDE'ZDEMPARM.H'
c  Have various keywords (for the interface to the user) and integers (for
c  the code itself's use).
c
c ----------------------------------------------------------------------
c
c  Modifications:
c  KWS, 10/14/93, Added MFI CB parameters
c  KWS, 03/07/94, Added rwall model, deleted references to multiple
c                 copies of the lossy switch model.
c  KWS, 05/30/95, Added parameters for Zflow Plasma Loss Model
c  MLK, 07/19/95, Added parameters for detail of printing on log file
c  KWS, 06/06/97, Added NShell model parameters
c  KWS, 08/12/97, Added parameters for Measure Zflow Block
c  RBS, 2012/03/28, Added R2wall and incremented Zflow to param 25
c  RBS, 2012-04-05, Replaced fixed *3 char length with (keyword_len)
c 2014-05-01 RBS: Added new switch_time variable in setup
c 2014-05-04 RBS: Changed integer*4 to integer
c 2014-10-21 RBS: Added the block ID dyhohlblock to both char and int
c 2014-10-23 RBS: Changed integer*4 to integer
c 2014-11-20 RBS: Added the block ID Lossyline to both char and int
c 2015-01-08 RBS: Include the variable lossyline in the parameter list
c 2016-04-01 RBS: Added block ID dpfblock char and character parameter
c 2016-04-01 RBS: Include ID dpfblock in int and integer parameter list
c 2016-06-25 RBS: Added ID ediode_model char and character parameter
c 2016-06-25 RBS: Included ediode_model in int and integer parameter
c 2017-01-02 RBS: Added rskin_model to char and character parameter
c 2017-01-02 RBS: Added rskin_model to integer and integer parameter
c 2019-12-22 RBS: Added rcond_model to char and character parameter
c 2019-12-22 RBS: Added rcond_model to integer and integer parameter
c 2020-09-01 RBS: Renamed the diode model to the sdiode model
c 2020-09-02 RBS: Add a new diode model with char and int parameters
c
c ----------------------------------------------------------------------
c
c  file zdemparm.h   
c
c ***** Time flag parameters ******
c
c       integer time_flag, half_step,     whole_step
c       parameter         (half_step = 1, whole_step = 2)
c
c ***** Set length of KEYWORD. *****
c
      integer    keyword_len
      parameter (keyword_len = 3)
c
c ***** Setup parameters *****
c
      character*(keyword_len)
     & k_time_step, 
     & k_end_time, 
     & k_num_prints,
     & k_num_cycles, 
     & k_res_time,
     & k_switch_time,
     & k_maxfpts
                                                                       
      parameter (k_time_step =   'TIM')
      parameter (k_end_time =    'END')
      parameter (k_num_prints =  'NUM')
      parameter (k_num_cycles =  'EXE')
      parameter (k_res_time =    'RES')
      parameter (k_switch_time = 'SWI')
      parameter (k_maxfpts =     'MAX')
c
c ***** How many cycles to execute. *****
c
      character*1
     & k_one_cycle,
     & k_all_cycles
      parameter (k_one_cycle    = 'O')
      parameter (k_all_cycles   = 'A')
c
      integer*4
     & one_cycle,
     & all_cycles
      parameter (one_cycle  = 0)
      parameter (all_cycles = 1)
c
c ***** Grids on plots *****
c
      character*(keyword_len)
     & k_plot_grid
      character*1
     & k_yes_grid,
     & k_no_grid
      parameter (k_plot_grid = 'GRI')
      parameter (k_yes_grid =  'Y')
      parameter (k_no_grid =   'N')
c
      integer    yes_grid,     no_grid
      parameter (yes_grid = 0, no_grid = 1)
c
c ***** Echo setup and indicies *****
c
      character*(keyword_len)
     & k_echo
      character*1
     & k_yes_echo,
     & k_no_echo
      parameter (k_echo = 'ECH')
      parameter (k_yes_echo =  'Y')
      parameter (k_no_echo =   'N')
c
      integer    yes_echo,     no_echo
      parameter (yes_echo = 0, no_echo = 1)
c
c ***** Detail of printing to log file at specified time steps *****
c
      character*(keyword_len)
     & k_detail_prints,
     & k_detail_prints_full,
     & k_detail_prints_min
      parameter (k_detail_prints      = 'DET')
      parameter (k_detail_prints_full = 'FUL')
      parameter (k_detail_prints_min  = 'MIN')
c
      integer    detail_prints_min,     detail_prints_full
      parameter (detail_prints_min = 0, detail_prints_full = 1)
c
c ***** Circuit blocks. *****
c
      integer
     & transline,
     & outputreq,
     & rcground,
     & pisection,
     & voltsource,
     & mitline,
     & adder,
     & vendsource,
     & pmitline,
     & rlseries,
     & currsource,
     & cendsource,
     & csclsource,
     & transformer,
     & cylfoilblock,
     & gaspuffblock,
     & sphfoilblock,
     & measurezflow,
     & nshellblock,
     & dyhohlblock,
     & lossyline,
     & dpfblock

      parameter (transline    =  0)
      parameter (outputreq    =  1)
      parameter (rcground     =  2)
      parameter (pisection    =  3)
      parameter (voltsource   =  5)
      parameter (mitline      =  6)
      parameter (adder        =  7)
      parameter (vendsource   =  8)
      parameter (pmitline     =  9)
      parameter (rlseries     = 10)
      parameter (currsource   = 11)
      parameter (cendsource   = 12)
      parameter (csclsource   = 13)
      parameter (transformer  = 14)
c
c Set foil block and gaspuff block so that these numbers do not conflict
c with the user#_model parameters (which currently extend to 20).
c This is because the foil block and gaspuff block are at times treated
c as models instead of blocks.
c If new block types are added, start them at the next number.
c
      parameter (cylfoilblock = 31)
      parameter (gaspuffblock = 32)
      parameter (sphfoilblock = 33)
      parameter (measurezflow = 34)
      parameter (nshellblock  = 35)
      parameter (dyhohlblock  = 36)
      parameter (lossyline    = 37)
      parameter (dpfblock     = 38)

      character*(keyword_len)
     & k_transline,
     & k_rcground,
     & k_pisection,
     & k_voltsource,
     & k_mitline,
     & k_adder,
     & k_vendsource,
     & k_pmitline,
     & k_rlseries,
     & k_currsource,
     & k_cendsource,
     & k_csclsource,
     & k_transformer,
     & k_cylfoilblock,
     & k_gaspuffblock,
     & k_sphfoilblock,
     & k_measurezflow,
     & k_nshellblock,
     & k_dyhohlblock,
     & k_lossyline,
     & k_dpfblock

      parameter (k_transline    = 'TRL')
      parameter (k_rcground     = 'RCG')
      parameter (k_pisection    = 'PIS')
      parameter (k_voltsource   = 'VOL')
      parameter (k_mitline      = 'MIT')
      parameter (k_adder        = 'ADD')
      parameter (k_vendsource   = 'VEN')
      parameter (k_pmitline     = 'PMI')
      parameter (k_rlseries     = 'RLS')
      parameter (k_currsource   = 'CUR')
      parameter (k_cendsource   = 'CEN')
      parameter (k_csclsource   = 'CSC')
      parameter (k_transformer  = 'TRA')
      parameter (k_cylfoilblock = 'CYL')
      parameter (k_gaspuffblock = 'GAS')
      parameter (k_sphfoilblock = 'SPH')
      parameter (k_measurezflow = 'MZF')
      parameter (k_nshellblock  = 'NSH')
      parameter (k_dyhohlblock  = 'DYH')
      parameter (k_lossyline    = 'LOS')
      parameter (k_dpfblock     = 'DPF')

c
c ***** Branches. *****
c
      character*(keyword_len)
     & k_branch,
     & k_topbranch,
     & k_endbranch
      parameter (k_branch =      'BRA')
      parameter (k_topbranch =   'TOP')
      parameter (k_endbranch =   'END')
c
      integer
     & topbranch,
     & endbranch
      parameter (topbranch = 1)
      parameter (endbranch = 2)
c
c ***** Type of transmission lines. *****
c
      character*(keyword_len)
     & k_linearz,
     & k_exponentialz
      parameter (k_linearz      = 'LIN')
      parameter (k_exponentialz = 'EXP')
c
      integer
     & linearz,
     & exponentialz
      parameter (linearz      = 1)
      parameter (exponentialz = 2)
c
c ***** Type of functions for voltage and current sources *****
c
      character*(keyword_len)
     & k_sinsquared,
     & k_piecewiselinear,
     & k_leastsquares,
     & k_table,
     & k_sinfun
      parameter (k_sinsquared      = 'SSQ')
      parameter (k_piecewiselinear = 'PWL')
      parameter (k_leastsquares    = 'LSF')
      parameter (k_table           = 'TAB')
      parameter (k_sinfun          = 'SIN')
c
      integer
     & sinsquared,
     & piecewiselinear,
     & leastsquares,
     & table,
     & sinfun
      parameter (sinsquared      = 1)
      parameter (piecewiselinear = 2)
      parameter (leastsquares    = 3)
      parameter (table           = 4)
      parameter (sinfun          = 5)
c
c ***** Variable elements for blocks. *****
c
      character*(keyword_len)
     & k_variable
      parameter (k_variable = 'VAR')
c
c ***** Switched variable elements for blocks. *****
c
      character*(keyword_len)
     & k_svariable
      parameter (k_svariable = 'SVA')
c
c ***** Which element is variable. *****
c
      character*(keyword_len)
     & k_r1_var,
     & k_c1_var,
     & k_r2_var,
     & k_l2_var,
     & k_r3_var,
     & k_c3_var
      parameter (k_r1_var = 'R1 ')
      parameter (k_c1_var = 'C1 ')
      parameter (k_r2_var = 'R2 ')
      parameter (k_l2_var = 'L2 ')
      parameter (k_r3_var = 'R3 ')
      parameter (k_c3_var = 'C3 ')
c
      integer
     & r1_var,
     & c1_var,
     & r2_var,
     & l2_var,
     & r3_var,
     & c3_var
      parameter (r1_var = 1)
      parameter (c1_var = 2)
      parameter (r2_var = 3)
      parameter (l2_var = 4)
      parameter (r3_var = 5)
      parameter (c3_var = 6)
c
c ***** Variable element models. *****
c
      integer
     & user_model,
     & lsf_model,
     & pwl_model,
     & exp_model,
     & decay_model,
     & rise_model,
     & magsw_model,
     & ps1_model,
     & ps2_model,
     & sdiode_model,
     & abdiode_model,
     & tab_model,
     & user1_model,
     & user2_model,
     & user3_model,
     & user4_model,
     & sw_model,
     & pos_model,
     & zmip_model,
     & mfi_model,
     & rwall_model,
     & r2wall_model,
     & rskin_model,
     & rcond_model,
     & zflow_model,
     & ediode_model,
     & diode_model

      parameter (user_model    = 0)
      parameter (lsf_model     = 1)
      parameter (pwl_model     = 2)
      parameter (exp_model     = 3)
      parameter (decay_model   = 4)
      parameter (rise_model    = 5)
      parameter (magsw_model   = 6)
      parameter (ps1_model     = 7)
      parameter (ps2_model     = 8)
      parameter (sdiode_model  = 9)
      parameter (abdiode_model = 10)
      parameter (tab_model     = 11)
      parameter (user1_model   = 12)
      parameter (user2_model   = 13)
      parameter (user3_model   = 14)
      parameter (user4_model   = 15)
      parameter (sw_model      = 16)
      parameter (pos_model     = 20)
      parameter (zmip_model    = 21)
      parameter (mfi_model     = 22)
      parameter (rwall_model   = 23)
      parameter (r2wall_model  = 24)
      parameter (rskin_model   = 25)
      parameter (rcond_model   = 26)
      parameter (zflow_model   = 27)
      parameter (ediode_model  = 28)
      parameter (diode_model   = 29)

      character*(keyword_len)
     & k_user_model,
     & k_lsf_model,
     & k_pwl_model,
     & k_exp_model,
     & k_decay_model,
     & k_rise_model,
     & k_magsw_model,
     & k_ps1_model,
     & k_ps2_model,
     & k_sdiode_model,
     & k_abdiode_model,
     & k_tab_model,
     & k_user1_model,
     & k_user2_model,
     & k_user3_model,
     & k_user4_model,
     & k_sw_model,
     & k_sw1_model,
     & k_sw2_model,
     & k_sw3_model,
     & k_sw4_model,
     & k_pos_model,
     & k_mfi_model,
     & k_zmip_model,
     & k_rwall_model,
     & k_r2wall_model,
     & k_rskin_model,
     & k_rcond_model,
     & k_zflow_model,
     & k_ediode_model,
     & k_diode_model

      parameter (k_user_model    = 'USE')
      parameter (k_lsf_model     = 'LSF')
      parameter (k_pwl_model     = 'PWL')
      parameter (k_exp_model     = 'EXP')
      parameter (k_decay_model   = 'DEC')
      parameter (k_rise_model    = 'RIS')
      parameter (k_magsw_model   = 'MSW')
      parameter (k_ps1_model     = 'PS1')
      parameter (k_ps2_model     = 'PS2')
      parameter (k_sdiode_model  = 'SDI')
      parameter (k_abdiode_model = 'ABD')
      parameter (k_tab_model     = 'TAB')
      parameter (k_user1_model   = 'US1')
      parameter (k_user2_model   = 'US2')
      parameter (k_user3_model   = 'US3')
      parameter (k_user4_model   = 'US4')
      parameter (k_sw_model      = 'SWI')
      parameter (k_sw1_model     = 'SW1')
      parameter (k_sw2_model     = 'SW2')
      parameter (k_sw3_model     = 'SW3')
      parameter (k_sw4_model     = 'SW4')
      parameter (k_pos_model     = 'POS')
      parameter (k_mfi_model     = 'MFI')
      parameter (k_zmip_model    = 'MIP')
      parameter (k_rwall_model   = 'RWA')
      parameter (k_r2wall_model  = 'R2W')
      parameter (k_rskin_model   = 'RSK')
      parameter (k_rcond_model   = 'RCO')
      parameter (k_zflow_model   = 'ZLO')
      parameter (k_ediode_model  = 'EDI')
      parameter (k_diode_model   = 'DIO')

c
c *****  Last entry seen in long parameter list *****
c
      character*(keyword_len)
     & k_last_entry

      parameter (k_last_entry = 'LAS')
c
c ***** Setting initial voltages or currents *****
c
      character*(keyword_len)
     & k_initial,
     & k_vcapacitor1,
     & k_vcapacitor3,
     & k_vtrline,
     & k_cinductor,
     & k_ctrline

      parameter (k_initial     = 'INI')
      parameter (k_vcapacitor1 = 'VC1')
      parameter (k_vcapacitor3 = 'VC3')
      parameter (k_vtrline     = 'VTR')
      parameter (k_cinductor   = 'IL2')
      parameter (k_ctrline     = 'ITR')
c
      integer
     & vcapacitor1,
     & vcapacitor3,
     & vtrline,
     & cinductor,
     & ctrline

      parameter (vcapacitor1 = 1)
      parameter (vcapacitor3 = 2)
      parameter (vtrline     = 3)
      parameter (cinductor   = 4)
      parameter (ctrline     = 5)
c
c   User variable label
c
      character*(keyword_len)
     & k_ulabel
c
      parameter (k_ulabel = 'ULA')
