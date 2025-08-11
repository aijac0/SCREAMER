! **********************************************************************
!  Set various parameters for read_screamer_data and use INCLUDE'ZDEMPARM.H'
!  Have various keywords (for the interface to the user) and integers (for
!  the code itself's use).
!
! ----------------------------------------------------------------------
!
!  Modifications:
!  KWS, 10/14/93, Added MFI CB parameters
!  KWS, 03/07/94, Added rwall model, deleted references to multiple
!                 copies of the lossy switch model.
!  KWS, 05/30/95, Added parameters for Zflow Plasma Loss Model
!  MLK, 07/19/95, Added parameters for detail of printing on log file
!  KWS, 06/06/97, Added NShell model parameters
!  KWS, 08/12/97, Added parameters for Measure Zflow Block
!  RBS, 2012/03/28, Added R2wall and incremented Zflow to param 25
!  RBS, 2012-04-05, Replaced fixed *3 char length with (keyword_len)
! 2014-05-01 RBS: Added new switch_time variable in setup
! 2014-05-04 RBS: Changed integer*4 to integer
! 2014-10-21 RBS: Added the block ID dyhohlblock to both char and int
! 2014-10-23 RBS: Changed integer*4 to integer
! 2014-11-20 RBS: Added the block ID Lossyline to both char and int
! 2015-01-08 RBS: Include the variable lossyline in the parameter list
! 2016-04-01 RBS: Added block ID dpfblock char and character parameter
! 2016-04-01 RBS: Include ID dpfblock in int and integer parameter list
! 2016-06-25 RBS: Added ID ediode_model char and character parameter
! 2016-06-25 RBS: Included ediode_model in int and integer parameter
! 2017-01-02 RBS: Added rskin_model to char and character parameter
! 2017-01-02 RBS: Added rskin_model to integer and integer parameter
! 2019-12-22 RBS: Added rcond_model to char and character parameter
! 2019-12-22 RBS: Added rcond_model to integer and integer parameter
! 2020-09-01 RBS: Renamed the diode model to the sdiode model
! 2020-09-02 RBS: Add a new diode model with char and int parameters
! 2025-08-11 AJC: Moved file into module
!
! ----------------------------------------------------------------------
!
!  file zdemparm.f90   


module zdemparm
      implicit none
!
! ***** Time flag parameters ******
!
      integer, parameter :: time_flag = 0
      integer, parameter :: half_step = 1
      integer, parameter :: whole_step = 2
!
! ***** Set length of KEYWORD. *****
!
      integer, parameter :: keyword_len = 3

!
! ***** Setup parameters *****
!                                                                       
      character(len=keyword_len), parameter :: k_time_step =   'TIM'
      character(len=keyword_len), parameter :: k_end_time =    'END'
      character(len=keyword_len), parameter :: k_num_prints =  'NUM'
      character(len=keyword_len), parameter :: k_num_cycles =  'EXE'
      character(len=keyword_len), parameter :: k_res_time =    'RES'
      character(len=keyword_len), parameter :: k_switch_time = 'SWI'
      character(len=keyword_len), parameter :: k_maxfpts =     'MAX'
!
! ***** How many cycles to execute. *****
!
      character(len=1), parameter :: k_one_cycle = 'O'
      character(len=1), parameter :: k_all_cycles = 'A'

      integer*4, parameter :: one_cycle = 0
      integer*4, parameter :: all_cycles = 1

!
! ***** Grids on plots *****
!
      character(len=keyword_len), parameter :: k_plot_grid = 'GRI'
      character(len=1), parameter :: k_yes_grid = 'Y'
      character(len=1), parameter :: k_no_grid = 'N'

      integer, parameter :: yes_grid = 0
      integer, parameter :: no_grid = 1
!
! ***** Echo setup and indicies *****
!
      character(len=keyword_len), parameter :: k_echo = 'ECH'
      character(len=1), parameter :: k_yes_echo = 'Y'
      character(len=1), parameter :: k_no_echo = 'N'

      integer, parameter :: yes_echo = 0
      integer, parameter :: no_echo = 1
!
! ***** Detail of printing to log file at specified time steps *****
!
      character(len=keyword_len), parameter :: k_detail_prints = 'DET'
      character(len=keyword_len), parameter :: k_detail_prints_full = 'FUL'
      character(len=keyword_len), parameter :: k_detail_prints_min = 'MIN'

      integer, parameter :: detail_prints_min = 0
      integer, parameter :: detail_prints_full = 1
!
! ***** Circuit blocks. *****
!
      integer, parameter :: transline = 0
      integer, parameter :: outputreq = 1
      integer, parameter :: rcground = 2
      integer, parameter :: pisection = 3
      integer, parameter :: voltsource = 5
      integer, parameter :: mitline = 6
      integer, parameter :: adder = 7
      integer, parameter :: vendsource = 8
      integer, parameter :: pmitline = 9
      integer, parameter :: rlseries = 10
      integer, parameter :: currsource = 11
      integer, parameter :: cendsource = 12
      integer, parameter :: csclsource = 13
      integer, parameter :: transformer = 14

!
! Set foil block and gaspuff block so that these numbers do not conflict
! with the user#_model parameters (which currently extend to 20).
! This is because the foil block and gaspuff block are at times treated
! as models instead of blocks.
! If new block types are added, start them at the next number.
!
      integer, parameter :: cylfoilblock = 31
      integer, parameter :: gaspuffblock = 32
      integer, parameter :: sphfoilblock = 33
      integer, parameter :: measurezflow = 34
      integer, parameter :: nshellblock  = 35
      integer, parameter :: dyhohlblock  = 36
      integer, parameter :: lossyline    = 37
      integer, parameter :: dpfblock     = 38

      character(len=keyword_len), parameter :: k_transline    = 'TRL'
      character(len=keyword_len), parameter :: k_rcground     = 'RCG'
      character(len=keyword_len), parameter :: k_pisection    = 'PIS'
      character(len=keyword_len), parameter :: k_voltsource   = 'VOL'
      character(len=keyword_len), parameter :: k_mitline      = 'MIT'
      character(len=keyword_len), parameter :: k_adder        = 'ADD'
      character(len=keyword_len), parameter :: k_vendsource   = 'VEN'
      character(len=keyword_len), parameter :: k_pmitline     = 'PMI'
      character(len=keyword_len), parameter :: k_rlseries     = 'RLS'
      character(len=keyword_len), parameter :: k_currsource   = 'CUR'
      character(len=keyword_len), parameter :: k_cendsource   = 'CEN'
      character(len=keyword_len), parameter :: k_csclsource   = 'CSC'
      character(len=keyword_len), parameter :: k_transformer  = 'TRA'
      character(len=keyword_len), parameter :: k_cylfoilblock = 'CYL'
      character(len=keyword_len), parameter :: k_gaspuffblock = 'GAS'
      character(len=keyword_len), parameter :: k_sphfoilblock = 'SPH'
      character(len=keyword_len), parameter :: k_measurezflow = 'MZF'
      character(len=keyword_len), parameter :: k_nshellblock  = 'NSH'
      character(len=keyword_len), parameter :: k_dyhohlblock  = 'DYH'
      character(len=keyword_len), parameter :: k_lossyline    = 'LOS'
      character(len=keyword_len), parameter :: k_dpfblock     = 'DPF'

!
! ***** Branches. *****
!
      character(len=keyword_len), parameter :: k_branch =      'BRA'
      character(len=keyword_len), parameter :: k_topbranch =   'TOP'
      character(len=keyword_len), parameter :: k_endbranch =   'END'

      integer, parameter :: topbranch = 1
      integer, parameter :: endbranch = 2

!
! ***** Type of transmission lines. *****
!
      character(len=keyword_len), parameter :: k_linearz      = 'LIN'
      character(len=keyword_len), parameter :: k_exponentialz = 'EXP'

      integer, parameter :: linearz      = 1
      integer, parameter :: exponentialz = 2

!
! ***** Type of functions for voltage and current sources *****
!
      character(len=keyword_len), parameter :: k_sinsquared      = 'SSQ'
      character(len=keyword_len), parameter :: k_piecewiselinear = 'PWL'
      character(len=keyword_len), parameter :: k_leastsquares    = 'LSF'
      character(len=keyword_len), parameter :: k_table           = 'TAB'
      character(len=keyword_len), parameter :: k_sinfun          = 'SIN'

      integer, parameter :: sinsquared      = 1
      integer, parameter :: piecewiselinear = 2
      integer, parameter :: leastsquares    = 3
      integer, parameter :: table           = 4
      integer, parameter :: sinfun          = 5

!
! ***** Variable elements for blocks. *****
!
      character(len=keyword_len), parameter :: k_variable = 'VAR'

!
! ***** Switched variable elements for blocks. *****
!
      character(len=keyword_len), parameter :: k_svariable = 'SVA'

!
! ***** Which element is variable. *****
!
      character(len=keyword_len), parameter :: k_r1_var = 'R1 '
      character(len=keyword_len), parameter :: k_c1_var = 'C1 '
      character(len=keyword_len), parameter :: k_r2_var = 'R2 '
      character(len=keyword_len), parameter :: k_l2_var = 'L2 '
      character(len=keyword_len), parameter :: k_r3_var = 'R3 '
      character(len=keyword_len), parameter :: k_c3_var = 'C3 '

      integer, parameter :: r1_var = 1
      integer, parameter :: c1_var = 2
      integer, parameter :: r2_var = 3
      integer, parameter :: l2_var = 4
      integer, parameter :: r3_var = 5
      integer, parameter :: c3_var = 6

!
! ***** Variable element models. *****
!

      integer, parameter :: user_model    = 0
      integer, parameter :: lsf_model     = 1
      integer, parameter :: pwl_model     = 2
      integer, parameter :: exp_model     = 3
      integer, parameter :: decay_model   = 4
      integer, parameter :: rise_model    = 5
      integer, parameter :: magsw_model   = 6
      integer, parameter :: ps1_model     = 7
      integer, parameter :: ps2_model     = 8
      integer, parameter :: sdiode_model  = 9
      integer, parameter :: abdiode_model = 10
      integer, parameter :: tab_model     = 11
      integer, parameter :: user1_model   = 12
      integer, parameter :: user2_model   = 13
      integer, parameter :: user3_model   = 14
      integer, parameter :: user4_model   = 15
      integer, parameter :: sw_model      = 16
      integer, parameter :: pos_model     = 20
      integer, parameter :: zmip_model    = 21
      integer, parameter :: mfi_model     = 22
      integer, parameter :: rwall_model   = 23
      integer, parameter :: r2wall_model  = 24
      integer, parameter :: rskin_model   = 25
      integer, parameter :: rcond_model   = 26
      integer, parameter :: zflow_model   = 27
      integer, parameter :: ediode_model  = 28
      integer, parameter :: diode_model   = 29

      character(len=keyword_len), parameter :: k_user_model    = 'USE'
      character(len=keyword_len), parameter :: k_lsf_model     = 'LSF'
      character(len=keyword_len), parameter :: k_pwl_model     = 'PWL'
      character(len=keyword_len), parameter :: k_exp_model     = 'EXP'
      character(len=keyword_len), parameter :: k_decay_model   = 'DEC'
      character(len=keyword_len), parameter :: k_rise_model    = 'RIS'
      character(len=keyword_len), parameter :: k_magsw_model   = 'MSW'
      character(len=keyword_len), parameter :: k_ps1_model     = 'PS1'
      character(len=keyword_len), parameter :: k_ps2_model     = 'PS2'
      character(len=keyword_len), parameter :: k_sdiode_model  = 'SDI'
      character(len=keyword_len), parameter :: k_abdiode_model = 'ABD'
      character(len=keyword_len), parameter :: k_tab_model     = 'TAB'
      character(len=keyword_len), parameter :: k_user1_model   = 'US1'
      character(len=keyword_len), parameter :: k_user2_model   = 'US2'
      character(len=keyword_len), parameter :: k_user3_model   = 'US3'
      character(len=keyword_len), parameter :: k_user4_model   = 'US4'
      character(len=keyword_len), parameter :: k_sw_model      = 'SWI'
      character(len=keyword_len), parameter :: k_sw1_model     = 'SW1'
      character(len=keyword_len), parameter :: k_sw2_model     = 'SW2'
      character(len=keyword_len), parameter :: k_sw3_model     = 'SW3'
      character(len=keyword_len), parameter :: k_sw4_model     = 'SW4'
      character(len=keyword_len), parameter :: k_pos_model     = 'POS'
      character(len=keyword_len), parameter :: k_mfi_model     = 'MFI'
      character(len=keyword_len), parameter :: k_zmip_model    = 'MIP'
      character(len=keyword_len), parameter :: k_rwall_model   = 'RWA'
      character(len=keyword_len), parameter :: k_r2wall_model  = 'R2W'
      character(len=keyword_len), parameter :: k_rskin_model   = 'RSK'
      character(len=keyword_len), parameter :: k_rcond_model   = 'RCO'
      character(len=keyword_len), parameter :: k_zflow_model   = 'ZLO'
      character(len=keyword_len), parameter :: k_ediode_model  = 'EDI'
      character(len=keyword_len), parameter :: k_diode_model   = 'DIO'

!
! *****  Last entry seen in long parameter list *****
!
      character(len=keyword_len), parameter :: k_last_entry = 'LAS'

!
! ***** Setting initial voltages or currents *****
!
      character(len=keyword_len), parameter :: k_initial = 'INI'
      character(len=keyword_len), parameter :: k_vcapacitor1 = 'VC1'
      character(len=keyword_len), parameter :: k_vcapacitor3 = 'VC3'
      character(len=keyword_len), parameter :: k_vtrline     = 'VTR'
      character(len=keyword_len), parameter :: k_cinductor   = 'IL2'
      character(len=keyword_len), parameter :: k_ctrline     = 'ITR'

      integer, parameter :: vcapacitor1 = 1
      integer, parameter :: vcapacitor3 = 2
      integer, parameter :: vtrline     = 3
      integer, parameter :: cinductor   = 4
      integer, parameter :: ctrline     = 5

!
!   User variable label
!
      character(len=keyword_len), parameter :: k_ulabel = 'ULA'

end module zdemparm