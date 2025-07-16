c----------------------------------------------------------------------
c    @(#)rdscrouts.f   version 1.0   created 06/15/2005 by Mathias Bavay
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This subroutine reads the output requests from the input deck.
c
c  Modifications:
c 2008-11-05 RBS: Warnings cleaned up by placing error status and
c                 returns in the if statments
c 2015-06-23 RBS: Declared time_flag, half_step, whole_step internal to
c                 function to eliminate compiler warnings
c 2015-06-23 RBS: Indexout initialized, zdemfmt.h removed as not needed
c 2017-02-24 RBS: Changed the lblout definition from using currline to
c                 using currline_lc
c 2018-02-22 RBS: Added a new output parameter closs in Zflow/MITL
c 2019-01-27 RBS: Added scale option for time_steps test
c 2019_01-27 RBS: Added scale keyword test, and inputted scale_out
c 2019-04-05 RBS: Added TXT output option, changed log file to write -
c                 TXT output requested, UFO call will be deprecated
c---------------------------------------------------------------------

      subroutine readscreameroutputs(status)
      
c Include the common blocks which are to be filled by the
c this subroutine.
c

      include 'zdemmax.h'
      include 'zdemcomm.h'
      include 'zdemout.h'
c
c Include the files with the keywords and the integer flags as
c parameters.
c
      include 'zdemparm.h'
      include 'zdempprm.h'
      include 'zdemenv.h'
      include 'rdscrdat.h'
c
c Define passed variables
c
      integer status

c
c ***** Time flag parameters ******
c

      integer half_step, whole_step, scale, time_flag, scale_flag
      parameter (half_step = 1, whole_step = 2, scale = 3)

c
      status = 0
c
c Initialize internal variables
c
      indexout = 0
      scale_flag = 0
      tscale = 1.0

c
c If keyword (first field) is an output request then
c
      if ((keyword .eq. k_pltpts) .or.
     &         (keyword .eq. k_prtpts) .or.
     &         (keyword .eq. k_filpts) .or.
     &         (keyword .eq. k_tabpts) .or.
     &         (keyword .eq. k_ufopts) .or.
     &         (keyword .eq. k_idrpts) .or.
     &         (keyword .eq. k_pffpts) .or.
     &         (keyword .eq. k_csvpts) .or.
     &         (keyword .eq. k_sfcpts) .or.
     &         (keyword .eq. k_txtpts)) then

c This is an output request.     
c Save this keyword and set the keyword for the type of output variable.
c
        keywordsave = keyword
        keyword     = field(2)(1:keyword_len)
c
c Set the output variable type based on the keyword.
c
        if (keyword .eq. k_vr1) then
          labelout = 'Voltage across R1'
          labelplt = 'VR1_volts'
          labelidr = 'VR1'
          indextyp = ip_vr1
        else if (keyword .eq. k_vr2) then
          labelout = 'Voltage across R2'
          labelplt = 'VR2_volts'
          labelidr = 'VR2'
          indextyp = ip_vr2
        else if (keyword .eq. k_vr3) then
          labelout = 'Voltage across R3'
          labelplt = 'VR3_volts'
          labelidr = 'VR3'
          indextyp = ip_vr3
        else if (keyword .eq. k_ir1) then
          labelout = 'Current in R1'
          labelplt = 'IR1_amps'
          labelidr = 'IR1'
          indextyp = ip_ir1
        else if (keyword .eq. k_ir2) then
          labelout = 'Current in R2'
          labelplt = 'IR2_amps'
          labelidr = 'IR2'
          indextyp = ip_ir2
        else if (keyword .eq. k_ir3) then
          labelout = 'Current in R3'
          labelplt = 'IR3_amps'
          labelidr = 'IR3'
          indextyp = ip_ir3
        else if (keyword .eq. k_pr1) then
          labelout = 'Power Dissipated in R1'
          labelplt = 'PR1_watts'
          labelidr = 'PR1'
          indextyp = ip_pr1
        else if (keyword .eq. k_pr2) then
          labelout = 'Power Dissipated in R2'
          labelplt = 'PR2_watts'
          labelidr = 'PR2'
          indextyp = ip_pr2
        else if (keyword .eq. k_pr3) then
          labelout = 'Power Dissipated in R3'
          labelplt = 'PR3_watts'
          labelidr = 'PR3'
          indextyp = ip_pr3
        else if (keyword .eq. k_er1) then
          labelout = 'Energy Dissipated in R1'
          labelplt = 'ER1_joules'
          labelidr = 'ER1'
          indextyp = ip_er1
        else if (keyword .eq. k_er2) then
          labelout = 'Energy Dissipated in R2'
          labelplt = 'ER2_joules'
          labelidr = 'ER2'
          indextyp = ip_er2
        else if (keyword .eq. k_er3) then
          labelout = 'Energy Dissipated in R3'
          labelplt = 'ER3_joules'
          labelidr = 'ER3'
          indextyp = ip_er3
        else if (keyword .eq. k_r1) then
          labelout = 'Value of R1'
          labelplt = 'R1_ohms'
          labelidr = 'R1'
          indextyp = ip_r1
        else if (keyword .eq. k_r2) then
          labelout = 'Value of R2'
          labelplt = 'R2_ohms'
          labelidr = 'R2'
          indextyp = ip_r2
        else if (keyword .eq. k_r3) then
          labelout = 'Value of R3'
          labelplt = 'R3_ohms'
          labelidr = 'R3'
          indextyp = ip_r3
        else if (keyword .eq. k_qr1) then
          labelout = 'Charge through R1'
          labelplt = 'QR1_coul'
          labelidr = 'QR1'
          indextyp = ip_qr1
        else if (keyword .eq. k_qr2) then
          labelout = 'Charge through R2'
          labelplt = 'QR2_coul'
          labelidr = 'QR2'
          indextyp = ip_qr2
        else if (keyword .eq. k_qr3) then
          labelout = 'Charge through R3'
          labelplt = 'QR3_coul'
          labelidr = 'QR3'
          indextyp = ip_qr3
        else if (keyword .eq. k_vc1) then
          labelout = 'Voltage across C1'
          labelplt = 'VC1_volts'
          labelidr = 'VC1'
          indextyp = ip_vc1
        else if (keyword .eq. k_vc3) then
          labelout = 'Voltage across C3'
          labelplt = 'VC3_volts'
          labelidr = 'VC3'
          indextyp = ip_vc3
        else if (keyword .eq. k_ic1) then
          labelout = 'Current in C1'
          labelplt = 'IC1_amps'
          labelidr = 'IC1'
          indextyp = ip_ic1
        else if (keyword .eq. k_ic3) then
          labelout = 'Current in C3'
          labelplt = 'IC3_amps'
          labelidr = 'IC3'
          indextyp = ip_ic3
        else if (keyword .eq. k_pc1) then
          labelout = 'Power Stored in C1'
          labelplt = 'PC1_watts'
          labelidr = 'PC1'
          indextyp = ip_pc1
        else if (keyword .eq. k_pc3) then
          labelout = 'Power Stored in C3'
          labelplt = 'PC3_watts'
          labelidr = 'PC3'
          indextyp = ip_pc3
        else if (keyword .eq. k_ec1) then
          labelout = 'Energy Stored in C1'
          labelplt = 'EC1_joules'
          labelidr = 'EC1'
          indextyp = ip_ec1
        else if (keyword .eq. k_ec3) then
          labelout = 'Energy Stored in C3'
          labelplt = 'EC3_joules'
          labelidr = 'EC3'
          indextyp = ip_ec3
        else if (keyword .eq. k_c1) then
          labelout = 'Value of C1'
          labelplt = 'C1_farads'
          labelidr = 'C1'
          indextyp = ip_c1
        else if (keyword .eq. k_c3) then
          labelout = 'Value of C3'
          labelplt = 'C3_farads'
          labelidr = 'C3'
          indextyp = ip_c3
        else if (keyword .eq. k_qc1) then
          labelout = 'Charge on C1'
          labelplt = 'QC1_coul'
          labelidr = 'QC1'
          indextyp = ip_qc1
        else if (keyword .eq. k_qc3) then
          labelout = 'Charge on C3'
          labelplt = 'QC3_coul'
          labelidr = 'QC3'
          indextyp = ip_qc3
        else if (keyword .eq. k_c1e) then
          labelout = 'Value of C1eff'
          labelplt = 'C1eff_farad'
          labelidr = 'C1EF'
          indextyp = ip_c1e
        else if (keyword .eq. k_c3e) then
          labelout = 'Value of C3eff'
          labelplt = 'C3eff_farad'
          labelidr = 'C3EF'
          indextyp = ip_c3e
        else if (keyword .eq. k_pcdot) then
          labelout = 'Value of 1/2*V*V*dC/dt'
          labelplt = 'PCdot_watts'
          labelidr = 'PCD'
          indextyp = ip_pcdot
        else if (keyword .eq. k_ecdot) then
          labelout = 'Sum of 1/2*V*V*dC/dt'
          labelplt = 'ECdot_joule'
          labelidr = 'ECD'
          indextyp = ip_ecdot
        else if (keyword .eq. k_pccon) then
          labelout = 'Term: d(1/2*C*V*V)/dt'
          labelplt = 'PCcon_watts'
          labelidr = 'PCC'
          indextyp = ip_pccon
        else if (keyword .eq. k_eccon) then
          labelout = 'Value of 1/2*C*V*V'
          labelplt = 'ECcon_joule'
          labelidr = 'ECC'
          indextyp = ip_eccon
        else if (keyword .eq. k_vl2) then
          labelout = 'Voltage across L2'
          labelplt = 'VL2_volts'
          labelidr = 'VL2'
          indextyp = ip_vl2
        else if (keyword .eq. k_il2) then
          labelout = 'Current in L2'
          labelplt = 'IL2_amps'
          labelidr = 'IL2'
          indextyp = ip_il2
        else if (keyword .eq. k_pl2) then
          labelout = 'Power Stored in L2'
          labelplt = 'PL2_watts'
          labelidr = 'PL2'
          indextyp = ip_pl2
        else if (keyword .eq. k_el2) then
          labelout = 'Energy Stored in L2'
          labelplt = 'EL2_joules'
          labelidr = 'EL2'
          indextyp = ip_el2
        else if (keyword .eq. k_l2) then
          labelout = 'Value of L2'
          labelplt = 'L2_henrys'
          labelidr = 'L2'
          indextyp = ip_l2
        else if (keyword .eq. k_fl2) then
          labelout = 'Flux in L2'
          labelplt = 'FL2_webers'
          labelidr = 'FL2'
          indextyp = ip_fl2
        else if (keyword .eq. k_ql2) then
          labelout = 'Charge through L2'
          labelplt = 'QL2_coul'
          labelidr = 'QL2'
          indextyp = ip_ql2
        else if (keyword .eq. k_l2e) then
          labelout = 'Value of L2eff'
          labelplt = 'L2eff_henry'
          labelidr = 'L2EF'
          indextyp = ip_l2e
        else if (keyword .eq. k_pldot) then
          labelout = 'Value of 1/2*I*I*dL/dt'
          labelplt = 'PLdot_watts'
          labelidr = 'PLD'
          indextyp = ip_pldot
        else if (keyword .eq. k_eldot) then
          labelout = 'Sum of 1/2*I*I*dL/dt'
          labelplt = 'ELdot_joule'
          labelidr = 'ELD'
          indextyp = ip_eldot
        else if (keyword .eq. k_plcon) then
          labelout = 'Term: d(1/2*L*I*I)/dt'
          labelplt = 'PLcon_watts'
          labelidr = 'PLC'
          indextyp = ip_plcon
        else if (keyword .eq. k_elcon) then
          labelout = 'Value of 1/2*L*I*I'
          labelplt = 'ELcon_joule'
          labelidr = 'ELC'
          indextyp = ip_elcon
        else if (keyword .eq. k_vsrc) then
          labelout = 'Source Voltage'
          labelplt = 'VSRC_volts'
          labelidr = 'VSRC'
          indextyp = ip_vsrc
        else if (keyword .eq. k_isrc) then
          labelout = 'Source Current'
          labelplt = 'ISRC_amps'
          labelidr = 'ISRC'
          indextyp = ip_isrc
        else if (keyword .eq. k_psrc) then
          labelout = 'Source Power'
          labelplt = 'PSRC_watts'
          labelidr = 'PSRC'
          indextyp = ip_psrc
        else if (keyword .eq. k_esrc) then
          labelout = 'Source Energy'
          labelplt = 'ESRC_joules'
          labelidr = 'ESRC'
          indextyp = ip_esrc
        else if (keyword .eq. k_qsrc) then
          labelout = 'Source Charge Produced'
          labelplt = 'QSRC_coul'
          labelidr = 'QSRC'
          indextyp = ip_qsrc
        else if (keyword .eq. k_vin) then
          labelout = 'Input Voltage'
          labelplt = 'VIN_volts'
          labelidr = 'VIN'
          indextyp = ip_vin
        else if (keyword .eq. k_iin) then
          labelout = 'Input Current'
          labelplt = 'IIN_amps'
          labelidr = 'IIN'
          indextyp = ip_iin
        else if (keyword .eq. k_pin) then
          labelout = 'Input Power'
          labelplt = 'PIN_watts'
          labelidr = 'PIN'
          indextyp = ip_pin
        else if (keyword .eq. k_ein) then
          labelout = 'Input Energy'
          labelplt = 'EIN_joules'
          labelidr = 'EIN'
          indextyp = ip_ein
        else if (keyword .eq. k_qin) then
          labelout = 'Input Charge'
          labelplt = 'QIN_coul'
          labelidr = 'QIN'
          indextyp = ip_qin
        else if (keyword .eq. k_vout) then
          labelout = 'Output Voltage'
          labelplt = 'VOUT_volts'
          labelidr = 'VOUT'
          indextyp = ip_vout
        else if (keyword .eq. k_iout) then
          labelout = 'Output Current'
          labelplt = 'IOUT_amps'
          labelidr = 'IOUT'
          indextyp = ip_iout
        else if (keyword .eq. k_pout) then
          labelout = 'Output Power'
          labelplt = 'POUT_watts'
          labelidr = 'POUT'
          indextyp = ip_pout
        else if (keyword .eq. k_eout) then
          labelout = 'Output Energy'
          labelplt = 'EOUT_joules'
          labelidr = 'EOUT'
          indextyp = ip_eout
        else if (keyword .eq. k_qout) then
          labelout = 'Output Charge'
          labelplt = 'QOUT_coul'
          labelidr = 'QOUT'
          indextyp = ip_qout
        else if (keyword .eq. k_facc) then
          labelout = 'Foil Acceleration'
          labelplt = 'A-FOIL_m/ss'
          labelidr = 'AFOI'
          indextyp = ip_facc
        else if (keyword .eq. k_fvel) then
          labelout = 'Foil Velocity'
          labelplt = 'V-FOIL_m/s'
          labelidr = 'VFOI'
          indextyp = ip_fvel
        else if (keyword .eq. k_frad) then
          labelout = 'Foil Radius'
          labelplt = 'RAD-FOIL_m'
          labelidr = 'RFOI'
          indextyp = ip_frad
        else if (keyword .eq. k_fke) then
          labelout = 'Foil Kinetic Energy'
          labelplt = 'KE-FOIL_j'
          labelidr = 'KFOI'
          indextyp = ip_fke
        else if (keyword .eq. k_yw_al) then
          labelout = 'Al K-line Energy/w'
          labelplt = 'E_Al/w_j'
          labelidr = 'E_Al'
          indextyp = ip_yw_al
          radyields=.true.
        else if (keyword .eq. k_ym_al) then
          labelout = 'Al K-line Energy/m'
          labelplt = 'E_Al/m_j'
          labelidr = 'E_Al'
          indextyp = ip_ym_al
          radyields=.true.
        else if (keyword .eq. k_yw_ar) then
          labelout = 'Ar K-line Energy/w'
          labelplt = 'E_Ar/w_j'
          labelidr = 'E_Ar'
          indextyp = ip_yw_ar
          radyields=.true.
        else if (keyword .eq. k_ym_ar) then
          labelout = 'Ar K-line Energy/m'
          labelplt = 'E_Ar/m_j'
          labelidr = 'E_Ar'
          indextyp = ip_ym_ar
          radyields=.true.
        else if (keyword .eq. k_yw_cu) then
          labelout = 'Cu K-line Energy/w'
          labelplt = 'E_Cu/w_j'
          labelidr = 'E_Cu'
          indextyp = ip_yw_cu
          radyields=.true.
        else if (keyword .eq. k_ym_cu) then
          labelout = 'Cu K-line Energy/m'
          labelplt = 'E_Cu/m_j'
          labelidr = 'E_Cu'
          indextyp = ip_ym_cu
          radyields=.true.
        else if (keyword .eq. k_yw_kr) then
          labelout = 'Kr K-line Energy/w'
          labelplt = 'E_Kr/w_j'
          labelidr = 'E_Kr'
          indextyp = ip_yw_kr
          radyields=.true.
        else if (keyword .eq. k_ym_kr) then
          labelout = 'Kr K-line Energy/m'
          labelplt = 'E_Kr/m_j'
          labelidr = 'E_Kr'
          indextyp = ip_ym_kr
          radyields=.true.
        else if (keyword .eq. k_yw_xe) then
          labelout = 'Xe K-line Energy/w'
          labelplt = 'E_Xe/w_j'
          labelidr = 'E_Xe'
          indextyp = ip_yw_xe
          radyields=.true.
        else if (keyword .eq. k_ym_xe) then
          labelout = 'Xe K-line Energy/m'
          labelplt = 'E_Xe/m_j'
          labelidr = 'E_Xe'
          radyields=.true.
          indextyp = ip_ym_xe
c
        else if (keyword .eq. k_gacc) then
          labelout = 'Gas Puff Acceleration'
          labelplt = 'A-GAS_m/ss'
          labelidr = 'AGAS'
          indextyp = ip_gacc
        else if (keyword .eq. k_gvel) then
          labelout = 'Gas Puff Velocity'
          labelplt = 'V-GAS_m/s'
          labelidr = 'VGAS'
          indextyp = ip_gvel
        else if (keyword .eq. k_grad) then
          labelout = 'Gas Puff Radius'
          labelplt = 'RAD-GAS_m'
          labelidr = 'RGAS'
          indextyp = ip_grad
        else if (keyword .eq. k_gke) then
          labelout = 'Gas Puff Kinetic Energy'
          labelplt = 'KE-GAS_j'
          labelidr = 'KGAS'
          indextyp = ip_gke
c
        else if (keyword .eq. k_sacc) then
          labelout = 'Shell Acceleration'
          labelplt = 'm/ss'
          labelidr = 'S_ACC'
          indextyp = ip_sacc
        else if (keyword .eq. k_svel) then
          labelout = 'Shell Velocity'
          labelplt = 'm/s'
          labelidr = 'S_VEL'
          indextyp = ip_svel
        else if (keyword .eq. k_srad) then
          labelout = 'Shell Radius'
          labelplt = 'm'
          labelidr = 'S_RAD'
          indextyp = ip_srad
        else if (keyword .eq. k_srad1) then
          labelout = 'Shell Radius 1'
          labelplt = 'm'
          labelidr = 'S_RAD1'
          indextyp = ip_srad1
        else if (keyword .eq. k_srad2) then
          labelout = 'Shell Radius 2'
          labelplt = 'm'
          labelidr = 'S_RAD2'
          indextyp = ip_srad2
        else if (keyword .eq. k_srad3) then
          labelout = 'Shell Radius 3'
          labelplt = 'm'
          labelidr = 'S_RAD3'
          indextyp = ip_srad3
        else if (keyword .eq. k_srad4) then
          labelout = 'Shell Radius 4'
          labelplt = 'm'
          labelidr = 'S_RAD4'
          indextyp = ip_srad4
        else if (keyword .eq. k_srad5) then
          labelout = 'Shell Radius 5'
          labelplt = 'm'
          labelidr = 'S_RAD5'
          indextyp = ip_srad5
        else if (keyword .eq. k_ske) then
          labelout = 'Shell Kinetic Energy'
          labelplt = 'J'
          labelidr = 'S_KE'
          indextyp = ip_ske
        else if (keyword .eq. k_smass) then
          labelout = 'Shell Mass'
          labelplt = 'kg'
          labelidr = 'S_MASS'
          indextyp = ip_smass
        else if (keyword .eq. k_scur1) then
          labelout = 'Shell Cur1'
          labelplt = 'A'
          labelidr = 'S_CUR1'
          indextyp = ip_scur1
        else if (keyword .eq. k_scur2) then
          labelout = 'Shell Cur2'
          labelplt = 'A'
          labelidr = 'S_CUR2'
          indextyp = ip_scur2
        else if (keyword .eq. k_scur3) then
          labelout = 'Shell Cur3'
          labelplt = 'A'
          labelidr = 'S_CUR3'
          indextyp = ip_scur3
        else if (keyword .eq. k_scur4) then
          labelout = 'Shell Cur4'
          labelplt = 'A'
          labelidr = 'S_CUR4'
          indextyp = ip_scur4
        else if (keyword .eq. k_scur5) then
          labelout = 'Shell Cur5'
          labelplt = 'A'
          labelidr = 'S_CUR5'
          indextyp = ip_scur5

        else if ( (keyword .eq. k_fc) 
     &    .or.    (keyword .eq. k_fc1) 
     &    .or.    (keyword .eq. k_fc2)
     &    .or.    (keyword .eq. k_fc3)
     &    .or.    (keyword .eq. k_fc4) ) then
          labelout = 'Gas Ch Radius'
          labelplt = 'radch'
          labelidr = 'fc'
          indextyp = ip_fc

        else if (keyword .eq. k_zflow) then
          labelout = 'Zflow (Ohms)'
          labelplt = 'zflow'
          labelidr = 'ZFL'
          indextyp = ip_zflow
        else if (keyword .eq. k_gzflow) then
          labelout = 'G Zflow (Mhos)'
          labelplt = 'gzflow'
          labelidr = 'GZF'
          indextyp = ip_gzflow
c
c --- KWS Zflow Plasma Loss Model ---
c
        else if (keyword .eq. k_zloss) then
          labelout = 'Zflow (Ohms)'
          labelplt = 'zflow'
          labelidr = 'ZLO'
          indextyp = ip_zloss
        else if (keyword .eq. k_gloss) then
          labelout = 'G Zflow (Mhos)'
          labelplt = 'gzflow'
          labelidr = 'GLO'
          indextyp = ip_gloss
c
c -----------------------------------
c
        else if (keyword .eq. k_efld) then
          labelout = 'E Field (V/m)'
          labelplt = 'efld'
          labelidr = 'EFL'
          indextyp = ip_efld
        else if (keyword .eq. k_bfld) then
          labelout = 'B Field (wb/m2)'
          labelplt = 'bfld'
          labelidr = 'BFL'
          indextyp = ip_bfld
        else if (keyword .eq. k_xmfi) then
          labelout = 'MFI Criterion'
          labelplt = 'xmfi'
          labelidr = 'XMF'
          indextyp = ip_xmfi
c
c --- KWS Zflow Plasma Loss Model MITL Model ---
c
        else if (keyword .eq. k_aloss) then
          labelout = 'Anode Loss Current Density'
          labelplt = 'aloss'
          labelidr = 'ALO'
          indextyp = ip_aloss
        else if (keyword .eq. k_closs) then
          labelout = 'Anode Loss Current'
          labelplt = 'closs'
          labelidr = 'CLO'
          indextyp = ip_closs
c
c --- KWS Measure Zflow and Cathode Current Block ---
c
        else if (keyword .eq. k_icathode) then
          labelout = 'Cathode Current'
          labelplt = 'I Cathode'
          labelidr = 'ICA'
          indextyp = ip_icathode
c
        else if (keyword .eq. k_iplasma) then
          labelout = 'Plasma Current'
          labelplt = 'I Plasma'
          labelidr = 'IPL'
          indextyp = ip_iplasma
        else if (keyword .eq. k_zot) then
          labelout = 'Zflow of T'
          labelplt = 'Zflow of T'
          labelidr = 'ZOT'
          indextyp = ip_zot
c
c -----------------------------------
c
        else if (keyword .eq. k_u1) then  ! User Variable #1
          labelout = 'User Variable #1'
          labelplt = 'U1_Unknown'
          labelidr = 'U1'
          indextyp = ip_u1
        else if (keyword .eq. k_u2) then  ! User Variable #2
          labelout = 'User Variable #2'
          labelplt = 'U2_Unknown'
          labelidr = 'U2'
          indextyp = ip_u2
        else if (keyword .eq. k_u3) then  ! User Variable #3
          labelout = 'User Variable #3'
          labelplt = 'U3_Unknown'
          labelidr = 'U3'
          indextyp = ip_u3
        else if (keyword .eq. k_u4) then  ! User Variable #4
          labelout = 'User Variable #4'
          labelplt = 'U4_Unknown'
          labelidr = 'U4'
          indextyp = ip_u4
        else if (keyword .eq. k_u5) then  ! User Variable #5
          labelout = 'User Variable #5'
          labelplt = 'U5_Unknown'
          labelidr = 'U5'
          indextyp = ip_u5
        else if (keyword .eq. k_u6) then  ! User Variable #6
          labelout = 'User Variable #6'
          labelplt = 'U6_Unknown'
          labelidr = 'U6'
          indextyp = ip_u6
        else if (keyword .eq. k_u7) then  ! User Variable #7
          labelout = 'User Variable #7'
          labelplt = 'U7_Unknown'
          labelidr = 'U7'
          indextyp = ip_u7
        else if (keyword .eq. k_u8) then  ! User Variable #8
          labelout = 'User Variable #8'
          labelplt = 'U8_Unknown'
          labelidr = 'U8'
          indextyp = ip_u8
        else if (keyword .eq. k_u9) then  ! User Variable #9
          labelout = 'User Variable #9'
          labelplt = 'U9_Unknown'
          labelidr = 'U9'
          indextyp = ip_u9
        else if (keyword .eq. k_u10) then  ! User Variable #10
          labelout = 'User Variable #10'
          labelplt = 'U10_Unknown'
          labelidr = 'U10'
          indextyp = ip_u10
        else if (keyword .eq. k_edline) then
          labelout = 'Energy Lost in Line'
          labelplt = 'EDLINE_joul'
          labelidr = 'EDL'
          indextyp = ip_edline
        else if (keyword .eq. k_pdline) then
          labelout = 'Power Lost in Line'
          labelplt = 'PDLINE_watt'
          labelidr = 'PDL'
          indextyp = ip_pdline
        else if (keyword .eq. k_eline) then
          labelout = 'Energy Stored in Line'
          labelplt = 'ELINE_joule'
          labelidr = 'ELI'
          indextyp = ip_eline
        else if (keyword .eq. k_pline) then
          labelout = 'Power Stored in Line'
          labelplt = 'PLINE_watts'
          labelidr = 'PLI'
          indextyp = ip_pline
        else
          call print_bad_line (currline, nlines, numerr)
        status = 305
        return
        end if
c
c First, test to see if a time_flag value has been entered. Then
c test to see if Tstart and Tstop have been entered, if not, then set
c them to 0 and end time, respectively. Here we have the option that if
c no time_flag has been entered tstart an tstop can still be entered
c
c First field is output type, second field is output parameter
c Third field is the (time_flag_ or scale option

        jj = 3
        call strip (field(jj), istart, iend)
        if (istart .ne. notext) then
c Try to read a number, else err
          call text_to_real (field(jj), tstart, flag)
c If text_to_real gives an error field(jj) must be characters
          if (flag .eq. err) then
             flag = noerr
             keyword = field(jj)(1:keyword_len)
c
c Check if the third field is an allowed character
c
             if (keyword .eq. k_wholestep) then
                time_flag = whole_step
             else if (keyword .eq. k_halfstep) then
                time_flag = half_step
             else if (keyword .eq. k_scale) then
                scale_flag = scale
             end if

c Look at the contents of the fourth field and strip blanks
             jj = jj + 1
               call strip (field(jj), istart, iend)
c
c If there is an entry in the fourth field and the entry is not scale
c  read in tstart and tstop

             if ((istart .ne. notext) .and. (scale_flag .ne. 3)) then
                call text_to_real (field(jj), tstart, flag)
                jj = jj + 1
                call text_to_real (field(jj), tstop, flag2)
             else
                tstart = 0.0
                tstop = tmax
                flag = noerr
                flag2 = noerr
             endif

          else
             jj = jj + 1

             if ((keywordsave .ne. k_idrpts) .and.
     +           (keywordsave .ne. k_sfcpts) .and. 
     +           (keywordsave .ne. k_pffpts))        then
               time_flag = half_step
             else
               time_flag = whole_step
             endif

             call text_to_real (field(jj), tstop,  flag2)
          endif

        else

          if ((keywordsave .ne. k_idrpts) .and.
     +        (keywordsave .ne. k_sfcpts) .and. 
     +        (keywordsave .ne. k_pffpts))        then
            time_flag = half_step
          else
            time_flag = whole_step
          endif

          tstart = 0.0
          tstop  = tmax
          flag   = noerr
          flag2  = noerr
        end if

c
c Test to see if Ymin and Ymax have been entered, if not, then set them
c both to zero to enable automatic scaling when plotting.
c
        jj = jj + 1
        call strip (field(jj), istart, iend)

        if (istart .ne. notext) then
          call text_to_real (field(jj), ymin, flag3)
          jj = jj + 1
          call text_to_real (field(jj), ymax, flag4)
        else
          ymin  = 0.0
          ymax  = 0.0
          flag3 = noerr
          flag4 = noerr
        end if

        if ((flag+flag2+flag3+flag4) .ne. noerr) then
          call print_bad_line (currline, nlines, numerr)
          status = 305
          return
        end if
c
c increment number of output calls and limit to maxout
c
        numout = numout + 1
        if (numout .gt. maxout) then
           status = 1000
           return
        end if
c
c Read in the scale factor only if scale_flag = scale
c
        scale_out(numout) = tscale
        if ( scale_flag .eq. scale ) then
          call text_to_real (field(4), tscale, flag2)
          scale_out(numout) = tscale
        end if
c        print '(i2,2x,1pe10.3,1pe10.3)',numout,tscale,scale_out(numout)
c
c Write log file info for 
c
        if (keywordsave .eq. k_pltpts) then
          numplt = numplt + 1
          write(9,
     &     '(A,i3,A,a23/A,1pe10.3,A,1pe10.3,A,1pe10.3,A,1pe10.3)') 
     &     ' PLOT Output Request for block ',nublks,
     &     '.    Variable: ',labelout,
     &     '  Tstart=',tstart,'   Tstop=',tstop,
     &     '   Ymin=',ymin, '   Ymax=',ymax
          iouttype(numout) = oplot
          indexout         = ip_pltpts
        else if (keywordsave .eq. k_prtpts) then
          numprt = numprt + 1
          write(9,'(A,i3,A,a23/A,1pe10.3,A,1pe10.3)')
     &     ' PRINT Output Request for block ',nublks,
     &     '.    Variable: ',labelout,
     &     '  Tstart=',tstart,'   Tstop=',tstop
          iouttype(numout) = oprint
          indexout         = ip_prtpts
        else if (keywordsave .eq. k_filpts) then
          numfil = numfil + 1
          write(9,'(A,i3,A,a23/A,1pe10.3,A,1pe10.3)')
     &     ' FILE Output Request for block ',nublks,
     &     '.    Variable: ',labelout,
     &     '  Tstart=',tstart, '   Tstop=',tstop
          iouttype(numout) = ofile
          indexout         = ip_filpts
        else if (keywordsave .eq. k_tabpts) then
          numtab = numtab + 1
          write(9,'(A,i3,A,a23/A,1pe10.3,A,1pe10.3)')
     &     ' TABLE Output Request for block ',nublks,
     &     '.    Variable: ',labelout,
     &     '  Tstart=',tstart,'   Tstop=',tstop
          iouttype(numout) = otable
          indexout         = ip_tabpts
        else if (keywordsave .eq. k_ufopts) then
          numufo = numufo + 1
          write(9,'(A,i3,A,a23)')
     &     ' TXT Output Request for block ',nublks,
     &     '.    Variable: ',labelout
          iouttype(numout) = oufo
          indexout         = ip_ufopts
          tstart           = 0.0
          tstop            = tmax
        else if (keywordsave .eq. k_txtpts) then
          numufo = numufo + 1
          write(9,'(A,i3,A,a23)')
     &     ' TXT Output Request for block ',nublks,
     &     '.    Variable: ',labelout
          iouttype(numout) = oufo
          indexout         = ip_ufopts
          tstart           = 0.0
          tstop            = tmax
        else if (keywordsave .eq. k_idrpts) then
          numidr = numidr + 1
          write(9,'(A,i3,A,a23)')
     &     ' IDR Output Request for block ',nublks,
     &     '.    Variable: ',labelout
          iouttype(numout) = oidr
          indexout         = ip_idrpts
          tstart           = 0.0
          tstop            = tmax
        else if (keywordsave .eq. k_pffpts) then
          numpff = numpff + 1
          write(9,'(A,i3,A,a23)')
     &     ' PFF Output Request for block ',nublks,
     &     '.    Variable: ',labelout
          iouttype(numout) = opff
          indexout         = ip_pffpts
          tstart           = 0.0
          tstop            = tmax
        else if (keywordsave .eq. k_csvpts) then
          numcsv = numcsv + 1
          write(9,'(A,i3,A,a23)')
     &     ' CSV Output Request for block ',nublks,
     &     '.    Variable: ',labelout
          iouttype(numout) = ocsv
          indexout         = ip_csvpts
          tstart           = 0.0
          tstop            = tmax
        else if (keywordsave .eq. k_sfcpts) then
          numsfc = numsfc + 1
          write(9,'(A,i3,A,a23)')
     &     ' SFC Output Request for block ',nublks,
     &     '.    Variable: ',labelout
          iouttype(numout) = osfc
          indexout         = ip_sfcpts
          tstart           = 0.0
          tstop            = tmax
        end if
c
c Set so UFO, TXT, and CSV are always half time steps.
c
        if ((time_flag .eq. whole_step) .and.
     &      (keywordsave .ne. k_ufopts) .and.
     &      (keywordsave .ne. k_txtpts) .and.
     &      (keywordsave .ne. k_csvpts)) then
          write(9,'(A)') '  Values on the whole time step will be used.'
c
c kls added the following else if structure because SFC always uses
c whole time step
c
        else if ((time_flag .eq. half_step) .and.
     &           (keywordsave .eq. k_sfcpts)) then
          write(9,'(A)') '  Values on the whole time step will be used.'
        else
          write(9,'(A)') '  Values on the half time step will be used.'
        endif
c
        tbegout(numout)  = tstart
        tendout(numout)  = tstop
        yminout(numout)  = ymin
        ymaxout(numout)  = ymax
        ixbrnout(numout) = nbrns
cc
cc --- KWS  Zflow Plasma Loss Model ---
cc        ixblkout(numout) = nublks
c        ixblkout(numout) = nblks
c
c --- MLK, set KWS change back to original since it causes
c          errors in plotting variables from user subroutines
c
        ixblkout(numout) = nublks
        iblkout(numout)  = lcirblk
        lastblk          = outputreq
        itypout(numout)  = indextyp
        itimeflg(numout) = time_flag
        lblout(numout)   = ' '
        ylblout(numout)  = labelplt
        lblout_temp(numout) = labelout
        invarl(numout)   = nvarl
c
c         create label for output
c
        write (cbranch,'(i2)') nbrns
        iblank_pointer = index(cbranch,' ')

        if (iblank_pointer .eq. 1) then
          cbranch = '0' // cbranch(2:2)
        endif

        write (cblock,'(i2)')  nublks
        iblank_pointer = index(cblock,' ')

        if (iblank_pointer .eq. 1) then
          cblock = '0' // cblock(2:2)
        endif

        branch_block = cbranch // cblock
        call strip(labelidr,istart,iend)
        tagout(numout) = labelidr(1:iend) // branch_block
c
        nblks              = nblks + 1
        iin(1,nblks,nbrns) = outputreq
        iin(5,nblks,nbrns) = indexout
c
c Comment line.
c
      else if (keyword(1:1) .eq. '!') then
        continue
c
c Label for output request (if last block was an output request, else it
c is a comment).
c
      else if ( (keyword(1:1) .eq. diaglabel_key) .or.
     +          (keyword(1:1) .eq. diaglabel_altkey) ) then

        if (lastblk .eq. outputreq) then
c
c Strip blanks from contents of currline
c Here we want to use currline_lc
c
          call strip (currline_lc, icl1, icl2)
c Note currline is *120 not *80

          if (icl1 .eq. 80) then
            currline_lc = ' '
            icl1     = 1
          else
            icl1     = icl1 + 1
          end if

c Note currline is *120 not *80
c Why :80 and not :icl2?
          lblout(numout) = currline_lc(icl1:80)
        else
          continue
        end if

c
c Label for user variable (if last block was an output request, else it
c is ignored).
c
      else if (keyword .eq. k_ulabel) then
c

        if (lastblk .eq. outputreq) then
          il1 = index(currline,'ULABEL')
          currline = currline(il1+7:80)
          call strip (currline, ilbl_start, ilbl_end)
          max_length = ilbl_start + 10
          ulabel = currline(ilbl_start:min(max_length,ilbl_end))
          ylblout(numout) = ulabel
        else
          continue
        end if

c
c Unknown keyword.
c
      else
        call print_bad_line (currline, nlines, numerr)
      end if

c
c Get another line.
c

      status = 305
      
      return
      end
