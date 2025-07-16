c *****************************************************************************
c  Parameters for output selections.
c  use:  include 'zdempprm.h'
c     file zdempprm.h
c ---------------------------------------------------------------------
c  Modifications:
c    KWS, 10/14/93, Added MFI CB parameters.
c                   Added callouts for the radiation yield in the
c                   cylindrical foil model.
c    MLK, 03/23/95, Added line: parameter (ip_gke   = 71) to fix
c                   someone's previous programming error
c    KWS, 06/06/95, Added keywords and indices for Zflow Plasma
c                   current loss model outputs. Also added a new
c                   output for MITLs.
c    MLK, 06/07/95, Added keywords and indices for CSV output types
c    KWS, 08/12/97, Added keywords and indices for Zflow Block outputs
c    MLK, 12/23/97, Added keywords and indices for SFC output types
c 2014-05-05 RBS: Changed integer*4 to integer
c 2014-05-07 RBS: All integer variables explicitly defined
c 2016-04-02 RBS: Reordered all integer parameters
c 2018-02-22 RBS: Added a new output CLOSS to the MITL outputs
c 2019-01-27 RBS: Added two parameters: halfstep and scale
c 2019-04-05 RBS: Added TXT parameter
c
c ---------------------------------------------------------------------
      integer    pkeyword_len
      parameter (pkeyword_len = 3)
c
c ***** Output Format Keywords *****
c
      character*(pkeyword_len)
     & k_pltpts,
     & k_prtpts,
     & k_filpts,
     & k_tabpts,
     & k_ufopts,
     & k_idrpts,
     & k_pffpts,
     & k_csvpts,
     & k_sfcpts,
     & k_txtpts

      parameter (k_pltpts = 'PLO')
      parameter (k_prtpts = 'PRI')
      parameter (k_filpts = 'FIL')
      parameter (k_tabpts = 'TAB')
      parameter (k_ufopts = 'UFO')
      parameter (k_idrpts = 'IDR')
      parameter (k_pffpts = 'PFF')
      parameter (k_csvpts = 'CSV')
      parameter (k_sfcpts = 'SFC')
      parameter (k_txtpts = 'TXT')

      integer
     & ip_pltpts, ip_prtpts, ip_filpts, ip_tabpts, ip_ufopts, ip_idrpts,
     & ip_pffpts, ip_csvpts, ip_sfcpts, ip_txtpts

      parameter (ip_pltpts = 1)
      parameter (ip_prtpts = 2)
      parameter (ip_filpts = 3)
      parameter (ip_tabpts = 4)
      parameter (ip_ufopts = 5)
      parameter (ip_idrpts = 6)
      parameter (ip_pffpts = 7)
      parameter (ip_csvpts = 8)
      parameter (ip_sfcpts = 9)
      parameter (ip_txtpts = 10)
c
      integer
     & oplot, oprint, ofile, otable, oidr, oufo, opff, ocsv, osfc, otxt
c
      parameter (oplot  = 1)
      parameter (oprint = 2)
      parameter (ofile  = 3)
      parameter (otable = 4)
      parameter (oufo   = 5)
      parameter (oidr   = 6)
      parameter (opff   = 7)
      parameter (ocsv   = 8)
      parameter (osfc   = 9)
      parameter (otxt   = 10)
c
      character*(pkeyword_len)
     & k_wholestep, k_halfstep, k_scale

      parameter (k_wholestep = 'WHO')
      parameter (k_halfstep = 'HAL')
      parameter (k_scale = 'SCA')
c
c ***** Keywords and indicies for PI, RCG, RLS,
c                                          source resistor outputs *****
c
      character*(pkeyword_len)
     & k_vr1,
     & k_vr2,
     & k_vr3,
     & k_ir1,
     & k_ir2,
     & k_ir3,
     & k_pr1,
     & k_pr2,
     & k_pr3,
     & k_er1,
     & k_er2,
     & k_er3,
     & k_r1,
     & k_r2,
     & k_r3,
     & k_qr1,
     & k_qr2,
     & k_qr3

      parameter (k_vr1 = 'VR1')
      parameter (k_vr2 = 'VR2')
      parameter (k_vr3 = 'VR3')
      parameter (k_ir1 = 'IR1')
      parameter (k_ir2 = 'IR2')
      parameter (k_ir3 = 'IR3')
      parameter (k_pr1 = 'PR1')
      parameter (k_pr2 = 'PR2')
      parameter (k_pr3 = 'PR3')
      parameter (k_er1 = 'ER1')
      parameter (k_er2 = 'ER2')
      parameter (k_er3 = 'ER3')
      parameter (k_r1  = 'R1 ')
      parameter (k_r2  = 'R2 ')
      parameter (k_r3  = 'R3 ')
      parameter (k_qr1 = 'QR1')
      parameter (k_qr2 = 'QR2')
      parameter (k_qr3 = 'QR3')

      integer ip_vr1, ip_vr2, ip_vr3, ip_ir1, ip_ir2, ip_ir3,
     &        ip_pr1, ip_pr2, ip_pr3, ip_er1, ip_er2, ip_er3,
     &        ip_r1, ip_r2, ip_r3, ip_qr1, ip_qr2, ip_qr3

      parameter (ip_vr1 = 1)
      parameter (ip_vr2 = 2)
      parameter (ip_vr3 = 3)
      parameter (ip_ir1 = 4)
      parameter (ip_ir2 = 5)
      parameter (ip_ir3 = 6)
      parameter (ip_pr1 = 7)
      parameter (ip_pr2 = 8)
      parameter (ip_pr3 = 9)
      parameter (ip_er1 = 10)
      parameter (ip_er2 = 11)
      parameter (ip_er3 = 12)
      parameter (ip_r1  = 13)
      parameter (ip_r2  = 14)
      parameter (ip_r3  = 15)
      parameter (ip_qr1 = 16)
      parameter (ip_qr2 = 17)
      parameter (ip_qr3 = 18)

c
c ***** Keywords and indicies for PI, RCG,
c                                   source block capacitor outputs *****
c
      character*(pkeyword_len)
     & k_vc1,
     & k_vc3,
     & k_ic1,
     & k_ic3,
     & k_pc1,
     & k_pc3,
     & k_ec1,
     & k_ec3,
     & k_c1,
     & k_c3,
     & k_qc1,
     & k_qc3,
     & k_c1e,
     & k_c3e,
     & k_pcdot,
     & k_ecdot,
     & k_pccon,
     & k_eccon
c
      parameter (k_vc1   = 'VC1')
      parameter (k_vc3   = 'VC3')
      parameter (k_ic1   = 'IC1')
      parameter (k_ic3   = 'IC3')
      parameter (k_pc1   = 'PC1')
      parameter (k_pc3   = 'PC3')
      parameter (k_ec1   = 'EC1')
      parameter (k_ec3   = 'EC3')
      parameter (k_c1    = 'C1 ')
      parameter (k_c3    = 'C3 ')
      parameter (k_qc1   = 'QC1')
      parameter (k_qc3   = 'QC3')
      parameter (k_c1e   = 'C1E')
      parameter (k_c3e   = 'C3E')
      parameter (k_pcdot = 'PCD')
      parameter (k_ecdot = 'ECD')
      parameter (k_pccon = 'PCC')
      parameter (k_eccon = 'ECC')
c
      integer ip_vc1, ip_vc3, ip_ic1, ip_ic3, ip_pc1, ip_pc3,
     &        ip_ec1, ip_ec3, ip_c1, ip_c3, ip_qc1, ip_qc3,
     &        ip_c1e, ip_c3e, ip_pcdot, ip_ecdot, ip_pccon, ip_eccon
c
      parameter (ip_vc1   = 19)
      parameter (ip_vc3   = 20)
      parameter (ip_ic1   = 21)
      parameter (ip_ic3   = 22)
      parameter (ip_pc1   = 23)
      parameter (ip_pc3   = 24)
      parameter (ip_ec1   = 25)
      parameter (ip_ec3   = 26)
      parameter (ip_c1    = 27)
      parameter (ip_c3    = 28)
      parameter (ip_qc1   = 29)
      parameter (ip_qc3   = 30)
      parameter (ip_c1e   = 53)
      parameter (ip_c3e   = 54)
      parameter (ip_pcdot = 60)
      parameter (ip_ecdot = 61)
      parameter (ip_pccon = 62)
      parameter (ip_eccon = 63)
c
c ***** Keywords and indicies for PI, RLS,
c                                and source block inductor outputs *****
c
      character*(pkeyword_len)
     & k_vl2,
     & k_il2,
     & k_pl2,
     & k_el2,
     & k_ql2,
     & k_l2,
     & k_fl2,
     & k_l2e,
     & k_pldot,
     & k_eldot,
     & k_plcon,
     & k_elcon
c
      parameter (k_vl2   = 'VL2')
      parameter (k_il2   = 'IL2')
      parameter (k_pl2   = 'PL2')
      parameter (k_el2   = 'EL2')
      parameter (k_ql2   = 'QL2')
      parameter (k_l2    = 'L2 ')
      parameter (k_fl2   = 'FL2')
      parameter (k_l2e   = 'L2E')
      parameter (k_pldot = 'PLD')
      parameter (k_eldot = 'ELD')
      parameter (k_plcon = 'PLC')
      parameter (k_elcon = 'ELC')
c
      integer ip_vl2, ip_il2, ip_pl2, ip_el2, ip_ql2, ip_l2, ip_fl2,
     &        ip_l2e, ip_pldot, ip_eldot, ip_plcon, ip_elcon
c
      parameter (ip_vl2   = 31)
      parameter (ip_il2   = 32)
      parameter (ip_pl2   = 33)
      parameter (ip_el2   = 34)
      parameter (ip_ql2   = 37)
      parameter (ip_l2    = 35)
      parameter (ip_fl2   = 36)
      parameter (ip_l2e   = 55)
      parameter (ip_pldot = 64)
      parameter (ip_eldot = 65)
      parameter (ip_plcon = 66)
      parameter (ip_elcon = 67)
c
c ***** Keywords and indicies for the various source types *****
c
      character*(pkeyword_len)
     & k_vsrc,
     & k_isrc,
     & k_psrc,
     & k_esrc,
     & k_qsrc
c
      parameter (k_vsrc = 'VSR')
      parameter (k_isrc = 'ISR')
      parameter (k_psrc = 'PSR')
      parameter (k_esrc = 'ESR')
      parameter (k_qsrc = 'QSR')
c
      integer ip_vsrc, ip_isrc, ip_psrc, ip_esrc, ip_qsrc
c
      parameter (ip_vsrc = 38)
      parameter (ip_isrc = 39)
      parameter (ip_psrc = 40)
      parameter (ip_esrc = 41)
      parameter (ip_qsrc = 42)
c
c ***** Keywords and indicies for all block type outputs *****
c
      character*(pkeyword_len)
     & k_vin,
     & k_iin,
     & k_pin,
     & k_ein,
     & k_qin,
     & k_vout,
     & k_iout,
     & k_pout,
     & k_eout,
     & k_qout
c
      parameter (k_vin  = 'VIN')
      parameter (k_iin  = 'IIN')
      parameter (k_pin  = 'PIN')
      parameter (k_ein  = 'EIN')
      parameter (k_qin  = 'QIN')
      parameter (k_vout = 'VOU')
      parameter (k_iout = 'IOU')
      parameter (k_pout = 'POU')
      parameter (k_eout = 'EOU')
      parameter (k_qout = 'QOU')
c
      integer ip_vin, ip_iin, ip_pin, ip_ein, ip_qin,
     &        ip_vout, ip_iout, ip_pout, ip_eout, ip_qout
c
      parameter (ip_vin  = 43)
      parameter (ip_iin  = 44)
      parameter (ip_pin  = 45)
      parameter (ip_ein  = 46)
      parameter (ip_qin  = 47)
      parameter (ip_vout = 48)
      parameter (ip_iout = 49)
      parameter (ip_pout = 50)
      parameter (ip_eout = 51)
      parameter (ip_qout = 52)

c
c ***** Keywords and indicies for foil implosion outputs *****
c
      character*(pkeyword_len)
     & k_frad,
     & k_fvel,
     & k_facc,
     & k_fke
c
      parameter (k_frad  = 'FRA')
      parameter (k_fvel  = 'FVE')
      parameter (k_facc  = 'FAC')
      parameter (k_fke   = 'FKE')
c
      integer ip_frad, ip_fvel, ip_facc, ip_fke
c
      parameter (ip_frad  = 56)
      parameter (ip_fvel  = 57)
      parameter (ip_facc  = 58)
      parameter (ip_fke   = 59)
c
c ***** Keywords and indicies for gas puff implosion outputs *****
c
      character*(pkeyword_len)
     & k_grad,
     & k_gvel,
     & k_gacc,
     & k_gke
c
      parameter (k_grad  = 'GRA')
      parameter (k_gvel  = 'GVE')
      parameter (k_gacc  = 'GAC')
      parameter (k_gke   = 'GKE')
c
      integer ip_grad, ip_gvel, ip_gacc, ip_gke
c
      parameter (ip_grad  = 68)
      parameter (ip_gvel  = 69)
      parameter (ip_gacc  = 70)
      parameter (ip_gke   = 71)
c
c ***** Keywords and indicies user variable outputs *****
c
      character*(pkeyword_len) 
     & k_u1,
     & k_u2,
     & k_u3,
     & k_u4,
     & k_u5,
     & k_u6,
     & k_u7,
     & k_u8,
     & k_u9,
     & k_u10
c
      parameter (k_u1    = 'U1')
      parameter (k_u2    = 'U2')
      parameter (k_u3    = 'U3')
      parameter (k_u4    = 'U4')
      parameter (k_u5    = 'U5')
      parameter (k_u6    = 'U6')
      parameter (k_u7    = 'U7')
      parameter (k_u8    = 'U8')
      parameter (k_u9    = 'U9')
      parameter (k_u10   = 'U10')
c
      integer ip_u1, ip_u2, ip_u3, ip_u4, ip_u5,
     &        ip_u6, ip_u7, ip_u8, ip_u9, ip_u10
c
      parameter (ip_u1   = 72)
      parameter (ip_u2   = 73)
      parameter (ip_u3   = 74)
      parameter (ip_u4   = 75)
      parameter (ip_u5   = 76)
      parameter (ip_u6   = 77)
      parameter (ip_u7   = 78)
      parameter (ip_u8   = 79)
      parameter (ip_u9   = 80)
      parameter (ip_u10  = 81)
c
c ***** Keywords and indicies for transmission line outputs *****
c
      character*(pkeyword_len) 
     & k_edline,
     & k_pdline,
     & k_eline,
     & k_pline
c
      parameter (k_edline    = 'EDL')
      parameter (k_pdline    = 'PDL')
      parameter (k_eline     = 'ELI')
      parameter (k_pline     = 'PLI')
c
      integer ip_edline, ip_pdline, ip_eline, ip_pline
c
      parameter (ip_edline   = 82)
      parameter (ip_pdline   = 83)
      parameter (ip_eline    = 84)
      parameter (ip_pline    = 85)
c
c ***** Keywords and indicies for Toms sw outputs *****
c
      character*(pkeyword_len) k_fc, k_fc1, k_fc2, k_fc3, k_fc4
c
      parameter (k_fc     = 'FCH')
      parameter (k_fc1    = 'FC1')
      parameter (k_fc2    = 'FC2')
      parameter (k_fc3    = 'FC3')
      parameter (k_fc4    = 'FC4')
c
      integer    ip_fc
c
      parameter (ip_fc    = 86)
c
c ***** Note: integer parameters 87-89 unused *****
c
c
c ***** Keywords and indicies for MFI CB outputs *****
c
      character*(pkeyword_len) 
     & k_efld,
     & k_bfld,
     & k_xmfi
c
      parameter (k_efld    = 'EFL')
      parameter (k_bfld    = 'BFL')
      parameter (k_xmfi    = 'XMF')
c
      integer ip_efld, ip_bfld, ip_xmfi
c
      parameter (ip_efld   = 90)
      parameter (ip_bfld   = 91)
      parameter (ip_xmfi   = 92)
c
c ***** Keywords and indicies for Zflow POS model outputs *****
c
      character*(pkeyword_len) 
     & k_zflow,
     & k_gzflow
c
      parameter (k_zflow    = 'ZFL')
      parameter (k_gzflow   = 'GZF')
c
      integer    ip_zflow, ip_gzflow
c
      parameter (ip_zflow   = 93)
      parameter (ip_gzflow  = 94)
c
c
c ***** Keywords and indicies for Zflow Current Loss Model outputs *****
c
      character*(pkeyword_len) 
     & k_zloss,
     & k_gloss
c
      parameter (k_zloss    = 'CZL')
      parameter (k_gloss    = 'GLO')
c
      integer ip_zloss, ip_gloss
c
      parameter (ip_zloss   = 95)
      parameter (ip_gloss   = 96)
c
c
c ***** Additional Keywords and indicies for the MITL Model *****
c
      character*(pkeyword_len)
     & k_aloss,
     & k_closs
c
      parameter (k_aloss    = 'ALO')
      parameter (k_closs    = 'CLO')
c
      integer    ip_aloss,
     &           ip_closs
c
      parameter (ip_aloss   = 97)
      parameter (ip_closs   = 116)
c
c
c
c ***** Keywords and indicies for Measure Zflow Block outputs *****
c
      character*(pkeyword_len) 
     & k_icathode,
     & k_iplasma,
     & k_zot
c
      parameter (k_icathode = 'ICA')
      parameter (k_iplasma  = 'IPL')
      parameter (k_zot      = 'ZOT')
c
      integer ip_icathode, ip_iplasma, ip_zot
c
      parameter (ip_icathode   = 98)
      parameter (ip_iplasma    = 99)
      parameter (ip_zot        = 100)
c
c
c ***** Keywords and indicies for k shell yield outputs *****
c
      character*(pkeyword_len)
     & k_yw_al,
     & k_ym_al,
     & k_yw_ar,
     & k_ym_ar,
     & k_yw_cu,
     & k_ym_cu,
     & k_yw_kr,
     & k_ym_kr,
     & k_yw_xe,
     & k_ym_xe
c
      parameter (k_yw_al = 'YWL')
      parameter (k_ym_al = 'YML')
      parameter (k_yw_ar = 'YWA')
      parameter (k_ym_ar = 'YMA')
      parameter (k_yw_cu = 'YWC')
      parameter (k_ym_cu = 'YMC')
      parameter (k_yw_kr = 'YWK')
      parameter (k_ym_kr = 'YMK')
      parameter (k_yw_xe = 'YWX')
      parameter (k_ym_xe = 'YMX')
c
      integer ip_yw_al, ip_ym_al, ip_yw_ar, ip_ym_ar,
     &        ip_yw_cu, ip_ym_cu, ip_yw_kr, ip_ym_kr, ip_yw_xe,
     &        ip_ym_xe
c
      parameter (ip_yw_al  = 101)
      parameter (ip_ym_al  = 102)
      parameter (ip_yw_ar  = 103)
      parameter (ip_ym_ar  = 104)
      parameter (ip_yw_cu  = 105)
      parameter (ip_ym_cu  = 106)
      parameter (ip_yw_kr  = 107)
      parameter (ip_ym_kr  = 108)
      parameter (ip_yw_xe  = 109)
      parameter (ip_ym_xe  = 110)
c
c ***** Keywords and indicies for spherical foil & NSHELL outputs *****
c
      character*(pkeyword_len)
     & k_srad,
     & k_svel,
     & k_sacc,
     & k_ske,
     & k_smass
c
      parameter (k_srad  = 'SRA')
      parameter (k_svel  = 'SVE')
      parameter (k_sacc  = 'SAC')
      parameter (k_ske   = 'SKE')
      parameter (k_smass = 'SMA')
c
      integer ip_srad, ip_svel, ip_sacc, ip_ske, ip_smass
c
      parameter (ip_srad   = 111)
      parameter (ip_svel   = 112)
      parameter (ip_sacc   = 113)
      parameter (ip_ske    = 114)
      parameter (ip_smass  = 115)
c
c ***** Keywords and indices for NSHELL outputs *****
c
      character*(pkeyword_len)
     & k_srad1,
     & k_srad2,
     & k_srad3,
     & k_srad4,
     & k_srad5,
     & k_scur1,
     & k_scur2,
     & k_scur3,
     & k_scur4,
     & k_scur5
c
      parameter (k_srad1 = 'SR1')
      parameter (k_srad2 = 'SR2')
      parameter (k_srad3 = 'SR3')
      parameter (k_srad4 = 'SR4')
      parameter (k_srad5 = 'SR5')
      parameter (k_scur1 = 'SC1')
      parameter (k_scur2 = 'SC2')
      parameter (k_scur3 = 'SC3')
      parameter (k_scur4 = 'SC4')
      parameter (k_scur5 = 'SC5')
c
      integer ip_srad1, ip_srad2, ip_srad3, ip_srad4, ip_srad5,
     &        ip_scur1, ip_scur2, ip_scur3, ip_scur4, ip_scur5
c
      parameter (ip_srad1  = 201)
      parameter (ip_srad2  = 202)
      parameter (ip_srad3  = 203)
      parameter (ip_srad4  = 204)
      parameter (ip_srad5  = 205)
      parameter (ip_scur1  = 301)
      parameter (ip_scur2  = 302)
      parameter (ip_scur3  = 303)
      parameter (ip_scur4  = 304)
      parameter (ip_scur5  = 305)
c
c ********************************************************
c
