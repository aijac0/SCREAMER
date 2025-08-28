! *****************************************************************************
!  Parameters for output selections.
!  use:  include 'zdempprm.h'
!     file zdempprm.h
! ---------------------------------------------------------------------
!  Modifications:
!    KWS, 10/14/93, Added MFI CB parameters.
!                   Added callouts for the radiation yield in the
!                   cylindrical foil model.
!    MLK, 03/23/95, Added line: parameter (ip_gke   = 71) to fix
!                   someone's previous programming error
!    KWS, 06/06/95, Added keywords and indices for Zflow Plasma
!                   current loss model outputs. Also added a new
!                   output for MITLs.
!    MLK, 06/07/95, Added keywords and indices for CSV output types
!    KWS, 08/12/97, Added keywords and indices for Zflow Block outputs
!    MLK, 12/23/97, Added keywords and indices for SFC output types
! 2014-05-05 RBS: Changed integer*4 to integer
! 2014-05-07 RBS: All integer variables explicitly defined
! 2016-04-02 RBS: Reordered all integer parameters
! 2018-02-22 RBS: Added a new output CLOSS to the MITL outputs
! 2019-01-27 RBS: Added two parameters: halfstep and scale
! 2019-04-05 RBS: Added TXT parameter
! 2025-08-11 AJC: Moved file into module
!
! ---------------------------------------------------------------------

module zdempprm

      integer, parameter :: pkeyword_len = 3

!
! ***** Output Format Keywords *****
!
      character(len=pkeyword_len), parameter :: k_pltpts = 'PLO'
      character(len=pkeyword_len), parameter :: k_prtpts = 'PRI'
      character(len=pkeyword_len), parameter :: k_filpts = 'FIL'
      character(len=pkeyword_len), parameter :: k_tabpts = 'TAB'
      character(len=pkeyword_len), parameter :: k_ufopts = 'UFO'
      character(len=pkeyword_len), parameter :: k_idrpts = 'IDR'
      character(len=pkeyword_len), parameter :: k_pffpts = 'PFF'
      character(len=pkeyword_len), parameter :: k_csvpts = 'CSV'
      character(len=pkeyword_len), parameter :: k_sfcpts = 'SFC'
      character(len=pkeyword_len), parameter :: k_txtpts = 'TXT'

      integer, parameter :: ip_pltpts = 1
      integer, parameter :: ip_prtpts = 2
      integer, parameter :: ip_filpts = 3
      integer, parameter :: ip_tabpts = 4
      integer, parameter :: ip_ufopts = 5
      integer, parameter :: ip_idrpts = 6
      integer, parameter :: ip_pffpts = 7
      integer, parameter :: ip_csvpts = 8
      integer, parameter :: ip_sfcpts = 9
      integer, parameter :: ip_txtpts = 10
!
      integer, parameter :: oplot  = 1
      integer, parameter :: oprint = 2
      integer, parameter :: ofile  = 3
      integer, parameter :: otable = 4
      integer, parameter :: oufo   = 5
      integer, parameter :: oidr   = 6
      integer, parameter :: opff   = 7
      integer, parameter :: ocsv   = 8
      integer, parameter :: osfc   = 9
      integer, parameter :: otxt   = 10
!
      character(len=pkeyword_len), parameter :: k_wholestep = 'WHO'
      character(len=pkeyword_len), parameter :: k_halfstep = 'HAL'
      character(len=pkeyword_len), parameter :: k_scale = 'SCA'

! ***** Keywords and indicies for PI, RCG, RLS,
!                                          source resistor outputs *****
!
      character(len=pkeyword_len), parameter :: k_vr1 = 'VR1'
      character(len=pkeyword_len), parameter :: k_vr2 = 'VR2'
      character(len=pkeyword_len), parameter :: k_vr3 = 'VR3'
      character(len=pkeyword_len), parameter :: k_ir1 = 'IR1'
      character(len=pkeyword_len), parameter :: k_ir2 = 'IR2'
      character(len=pkeyword_len), parameter :: k_ir3 = 'IR3'
      character(len=pkeyword_len), parameter :: k_pr1 = 'PR1'
      character(len=pkeyword_len), parameter :: k_pr2 = 'PR2'
      character(len=pkeyword_len), parameter :: k_pr3 = 'PR3'
      character(len=pkeyword_len), parameter :: k_er1 = 'ER1'
      character(len=pkeyword_len), parameter :: k_er2 = 'ER2'
      character(len=pkeyword_len), parameter :: k_er3 = 'ER3'
      character(len=pkeyword_len), parameter :: k_r1  = 'R1 '
      character(len=pkeyword_len), parameter :: k_r2  = 'R2 '
      character(len=pkeyword_len), parameter :: k_r3  = 'R3 '
      character(len=pkeyword_len), parameter :: k_qr1 = 'QR1'
      character(len=pkeyword_len), parameter :: k_qr2 = 'QR2'
      character(len=pkeyword_len), parameter :: k_qr3 = 'QR3'

      integer, parameter :: ip_vr1 = 1
      integer, parameter :: ip_vr2 = 2
      integer, parameter :: ip_vr3 = 3
      integer, parameter :: ip_ir1 = 4
      integer, parameter :: ip_ir2 = 5
      integer, parameter :: ip_ir3 = 6
      integer, parameter :: ip_pr1 = 7
      integer, parameter :: ip_pr2 = 8
      integer, parameter :: ip_pr3 = 9
      integer, parameter :: ip_er1 = 10
      integer, parameter :: ip_er2 = 11
      integer, parameter :: ip_er3 = 12
      integer, parameter :: ip_r1  = 13
      integer, parameter :: ip_r2  = 14
      integer, parameter :: ip_r3  = 15
      integer, parameter :: ip_qr1 = 16
      integer, parameter :: ip_qr2 = 17
      integer, parameter :: ip_qr3 = 18

!
! ***** Keywords and indicies for PI, RCG,
!                                   source block capacitor outputs *****
!
      character(len=pkeyword_len), parameter :: k_vc1 = 'VC1'
      character(len=pkeyword_len), parameter :: k_vc3 = 'VC3'
      character(len=pkeyword_len), parameter :: k_ic1 = 'IC1'
      character(len=pkeyword_len), parameter :: k_ic3 = 'IC3'
      character(len=pkeyword_len), parameter :: k_pc1 = 'PC1'
      character(len=pkeyword_len), parameter :: k_pc3 = 'PC3'
      character(len=pkeyword_len), parameter :: k_ec1 = 'EC1'
      character(len=pkeyword_len), parameter :: k_ec3 = 'EC3'
      character(len=pkeyword_len), parameter :: k_c1  = 'C1 '
      character(len=pkeyword_len), parameter :: k_c3  = 'C3 '
      character(len=pkeyword_len), parameter :: k_qc1 = 'QC1'
      character(len=pkeyword_len), parameter :: k_qc3 = 'QC3'
      character(len=pkeyword_len), parameter :: k_c1e = 'C1E'
      character(len=pkeyword_len), parameter :: k_c3e = 'C3E'
      character(len=pkeyword_len), parameter :: k_pcdot = 'PCD'
      character(len=pkeyword_len), parameter :: k_ecdot = 'ECD'
      character(len=pkeyword_len), parameter :: k_pccon = 'PCC'
      character(len=pkeyword_len), parameter :: k_eccon = 'ECC'
!
      integer, parameter :: ip_vc1   = 19
      integer, parameter :: ip_vc3   = 20
      integer, parameter :: ip_ic1   = 21
      integer, parameter :: ip_ic3   = 22
      integer, parameter :: ip_pc1   = 23
      integer, parameter :: ip_pc3   = 24
      integer, parameter :: ip_ec1   = 25
      integer, parameter :: ip_ec3   = 26
      integer, parameter :: ip_c1    = 27
      integer, parameter :: ip_c3    = 28
      integer, parameter :: ip_qc1   = 29
      integer, parameter :: ip_qc3   = 30
      integer, parameter :: ip_c1e   = 53
      integer, parameter :: ip_c3e   = 54
      integer, parameter :: ip_pcdot = 60
      integer, parameter :: ip_ecdot = 61
      integer, parameter :: ip_pccon = 62
      integer, parameter :: ip_eccon = 63
!
! ***** Keywords and indicies for PI, RLS,
!                                and source block inductor outputs *****
!
      character(len=pkeyword_len), parameter :: k_vl2   = 'VL2'
      character(len=pkeyword_len), parameter :: k_il2   = 'IL2'
      character(len=pkeyword_len), parameter :: k_pl2   = 'PL2'
      character(len=pkeyword_len), parameter :: k_el2   = 'EL2'
      character(len=pkeyword_len), parameter :: k_ql2   = 'QL2'
      character(len=pkeyword_len), parameter :: k_l2    = 'L2 '
      character(len=pkeyword_len), parameter :: k_fl2   = 'FL2'
      character(len=pkeyword_len), parameter :: k_l2e   = 'L2E'
      character(len=pkeyword_len), parameter :: k_pldot = 'PLD'
      character(len=pkeyword_len), parameter :: k_eldot = 'ELD'
      character(len=pkeyword_len), parameter :: k_plcon = 'PLC'
      character(len=pkeyword_len), parameter :: k_elcon = 'ELC'
!
      integer, parameter :: ip_vl2   = 31
      integer, parameter :: ip_il2   = 32
      integer, parameter :: ip_pl2   = 33
      integer, parameter :: ip_el2   = 34
      integer, parameter :: ip_ql2   = 37
      integer, parameter :: ip_l2    = 35
      integer, parameter :: ip_fl2   = 36
      integer, parameter :: ip_l2e   = 55
      integer, parameter :: ip_pldot = 64
      integer, parameter :: ip_eldot = 65
      integer, parameter :: ip_plcon = 66
      integer, parameter :: ip_elcon = 67
!
! ***** Keywords and indicies for the various source types *****
!
      character(len=pkeyword_len), parameter :: k_vsrc = 'VSR'
      character(len=pkeyword_len), parameter :: k_isrc = 'ISR'
      character(len=pkeyword_len), parameter :: k_psrc = 'PSR'
      character(len=pkeyword_len), parameter :: k_esrc = 'ESR'
      character(len=pkeyword_len), parameter :: k_qsrc = 'QSR'
!
      integer, parameter :: ip_vsrc = 38
      integer, parameter :: ip_isrc = 39
      integer, parameter :: ip_psrc = 40
      integer, parameter :: ip_esrc = 41
      integer, parameter :: ip_qsrc = 42
!
! ***** Keywords and indicies for all block type outputs *****
!
      character(len=pkeyword_len), parameter :: k_vin = 'VIN'
      character(len=pkeyword_len), parameter :: k_iin = 'IIN'
      character(len=pkeyword_len), parameter :: k_pin = 'PIN'
      character(len=pkeyword_len), parameter :: k_ein = 'EIN'
      character(len=pkeyword_len), parameter :: k_qin = 'QIN'
      character(len=pkeyword_len), parameter :: k_vout = 'VOU'
      character(len=pkeyword_len), parameter :: k_iout = 'IOU'
      character(len=pkeyword_len), parameter :: k_pout = 'POU'
      character(len=pkeyword_len), parameter :: k_eout = 'EOU'
      character(len=pkeyword_len), parameter :: k_qout = 'QOU'
!
      integer, parameter :: ip_vin = 43
      integer, parameter :: ip_iin = 44
      integer, parameter :: ip_pin = 45
      integer, parameter :: ip_ein = 46
      integer, parameter :: ip_qin = 47
      integer, parameter :: ip_vout = 48
      integer, parameter :: ip_iout = 49
      integer, parameter :: ip_pout = 50
      integer, parameter :: ip_eout = 51
      integer, parameter :: ip_qout = 52

!
! ***** Keywords and indicies for foil implosion outputs *****
!
      character(len=pkeyword_len), parameter :: k_frad  = 'FRA'
      character(len=pkeyword_len), parameter :: k_fvel  = 'FVE'
      character(len=pkeyword_len), parameter :: k_facc  = 'FAC'
      character(len=pkeyword_len), parameter :: k_fke   = 'FKE'
!
      integer, parameter :: ip_frad  = 56
      integer, parameter :: ip_fvel  = 57
      integer, parameter :: ip_facc  = 58
      integer, parameter :: ip_fke   = 59
!
! ***** Keywords and indicies for gas puff implosion outputs *****
!
      character(len=pkeyword_len), parameter :: k_grad = 'GRA'
      character(len=pkeyword_len), parameter :: k_gvel = 'GVE'
      character(len=pkeyword_len), parameter :: k_gacc = 'GAC'
      character(len=pkeyword_len), parameter :: k_gke  = 'GKE'
!
      integer, parameter :: ip_grad  = 68
      integer, parameter :: ip_gvel  = 69
      integer, parameter :: ip_gacc  = 70
      integer, parameter :: ip_gke   = 71
!
! ***** Keywords and indicies user variable outputs *****
!
      character(len=pkeyword_len), parameter :: k_u1    = 'U1'
      character(len=pkeyword_len), parameter :: k_u2    = 'U2'
      character(len=pkeyword_len), parameter :: k_u3    = 'U3'
      character(len=pkeyword_len), parameter :: k_u4    = 'U4'
      character(len=pkeyword_len), parameter :: k_u5    = 'U5'
      character(len=pkeyword_len), parameter :: k_u6    = 'U6'
      character(len=pkeyword_len), parameter :: k_u7    = 'U7'
      character(len=pkeyword_len), parameter :: k_u8    = 'U8'
      character(len=pkeyword_len), parameter :: k_u9    = 'U9'
      character(len=pkeyword_len), parameter :: k_u10   = 'U10'
!
      integer, parameter :: ip_u1   = 72
      integer, parameter :: ip_u2   = 73
      integer, parameter :: ip_u3   = 74
      integer, parameter :: ip_u4   = 75
      integer, parameter :: ip_u5   = 76
      integer, parameter :: ip_u6   = 77
      integer, parameter :: ip_u7   = 78
      integer, parameter :: ip_u8   = 79
      integer, parameter :: ip_u9   = 80
      integer, parameter :: ip_u10  = 81
!
! ***** Keywords and indicies for transmission line outputs *****
!
      character(len=pkeyword_len), parameter :: k_edline    = 'EDL'
      character(len=pkeyword_len), parameter :: k_pdline    = 'PDL'
      character(len=pkeyword_len), parameter :: k_eline     = 'ELI'
      character(len=pkeyword_len), parameter :: k_pline     = 'PLI'
!
      integer, parameter :: ip_edline   = 82
      integer, parameter :: ip_pdline   = 83
      integer, parameter :: ip_eline    = 84
      integer, parameter :: ip_pline    = 85
!
! ***** Keywords and indicies for Toms sw outputs *****
!
      character(len=pkeyword_len), parameter :: k_fc     = 'FCH'
      character(len=pkeyword_len), parameter :: k_fc1    = 'FC1'
      character(len=pkeyword_len), parameter :: k_fc2    = 'FC2'
      character(len=pkeyword_len), parameter :: k_fc3    = 'FC3'
      character(len=pkeyword_len), parameter :: k_fc4    = 'FC4'
!
      integer, parameter :: ip_fc    = 86

!
! ***** Note: integer parameters 87-89 unused *****
!
!
! ***** Keywords and indicies for MFI CB outputs *****
!
      character(len=pkeyword_len), parameter :: k_efld    = 'EFL'
      character(len=pkeyword_len), parameter :: k_bfld    = 'BFL'
      character(len=pkeyword_len), parameter :: k_xmfi    = 'XMF'
!
      integer, parameter :: ip_efld   = 90
      integer, parameter :: ip_bfld   = 91
      integer, parameter :: ip_xmfi   = 92
!
! ***** Keywords and indicies for Zflow POS model outputs *****
!
      character(len=pkeyword_len), parameter :: k_zflow    = 'ZFL'
      character(len=pkeyword_len), parameter :: k_gzflow   = 'GZF'
!
      integer, parameter :: ip_zflow   = 93
      integer, parameter :: ip_gzflow  = 94
!
!
! ***** Keywords and indicies for Zflow Current Loss Model outputs *****
!
      character(len=pkeyword_len), parameter :: k_zloss    = 'CZL'
      character(len=pkeyword_len), parameter :: k_gloss    = 'GLO'
!
      integer, parameter :: ip_zloss   = 95
      integer, parameter :: ip_gloss   = 96
!
! ***** Additional Keywords and indicies for the MITL Model *****
!
      character(len=pkeyword_len), parameter :: k_aloss    = 'ALO'
      character(len=pkeyword_len), parameter :: k_closs    = 'CLO'
!
      integer, parameter :: ip_aloss   = 97
      integer, parameter :: ip_closs   = 116
!
! ***** Keywords and indicies for Measure Zflow Block outputs *****
!
      character(len=pkeyword_len), parameter :: k_icathode = 'ICA'
      character(len=pkeyword_len), parameter :: k_iplasma  = 'IPL'
      character(len=pkeyword_len), parameter :: k_zot      = 'ZOT'
!
      integer, parameter :: ip_icathode   = 98
      integer, parameter :: ip_iplasma    = 99
      integer, parameter :: ip_zot        = 100

!
! ***** Keywords and indicies for k shell yield outputs *****
!
      character(len=pkeyword_len), parameter :: k_yw_al = 'YWL'
      character(len=pkeyword_len), parameter :: k_ym_al = 'YML'
      character(len=pkeyword_len), parameter :: k_yw_ar = 'YWA'
      character(len=pkeyword_len), parameter :: k_ym_ar = 'YMA'
      character(len=pkeyword_len), parameter :: k_yw_cu = 'YWC'
      character(len=pkeyword_len), parameter :: k_ym_cu = 'YMC'
      character(len=pkeyword_len), parameter :: k_yw_kr = 'YWK'
      character(len=pkeyword_len), parameter :: k_ym_kr = 'YMK'
      character(len=pkeyword_len), parameter :: k_yw_xe = 'YWX'
      character(len=pkeyword_len), parameter :: k_ym_xe = 'YMX'
!
      integer, parameter :: ip_yw_al  = 101
      integer, parameter :: ip_ym_al  = 102
      integer, parameter :: ip_yw_ar  = 103
      integer, parameter :: ip_ym_ar  = 104
      integer, parameter :: ip_yw_cu  = 105
      integer, parameter :: ip_ym_cu  = 106
      integer, parameter :: ip_yw_kr  = 107
      integer, parameter :: ip_ym_kr  = 108
      integer, parameter :: ip_yw_xe  = 109
      integer, parameter :: ip_ym_xe  = 110

!
! ***** Keywords and indicies for spherical foil & NSHELL outputs *****
!
      character(len=pkeyword_len), parameter :: k_srad  = 'SRA'
      character(len=pkeyword_len), parameter :: k_svel  = 'SVE'
      character(len=pkeyword_len), parameter :: k_sacc  = 'SAC'
      character(len=pkeyword_len), parameter :: k_ske   = 'SKE'
      character(len=pkeyword_len), parameter :: k_smass = 'SMA'
!
      integer, parameter :: ip_srad   = 111
      integer, parameter :: ip_svel   = 112
      integer, parameter :: ip_sacc   = 113
      integer, parameter :: ip_ske    = 114
      integer, parameter :: ip_smass  = 115
!
! ***** Keywords and indices for NSHELL outputs *****
!
      character(len=pkeyword_len), parameter :: k_srad1 = 'SR1'
      character(len=pkeyword_len), parameter :: k_srad2 = 'SR2'
      character(len=pkeyword_len), parameter :: k_srad3 = 'SR3'
      character(len=pkeyword_len), parameter :: k_srad4 = 'SR4'
      character(len=pkeyword_len), parameter :: k_srad5 = 'SR5'
      character(len=pkeyword_len), parameter :: k_scur1 = 'SC1'
      character(len=pkeyword_len), parameter :: k_scur2 = 'SC2'
      character(len=pkeyword_len), parameter :: k_scur3 = 'SC3'
      character(len=pkeyword_len), parameter :: k_scur4 = 'SC4'
      character(len=pkeyword_len), parameter :: k_scur5 = 'SC5'
!
      integer, parameter :: ip_srad1  = 201
      integer, parameter :: ip_srad2  = 202
      integer, parameter :: ip_srad3  = 203
      integer, parameter :: ip_srad4  = 204
      integer, parameter :: ip_srad5  = 205
      integer, parameter :: ip_scur1  = 301
      integer, parameter :: ip_scur2  = 302
      integer, parameter :: ip_scur3  = 303
      integer, parameter :: ip_scur4  = 304
      integer, parameter :: ip_scur5  = 305
!
! ********************************************************
!

end module zdempprm