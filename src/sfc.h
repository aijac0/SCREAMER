c This is a Fortran header file: sfc.h
c  Modifications:
c    1/6/98, MLK: Original version
c 2015-03-28 RBS: Increased length of sfc_len to *25
c
c Character strings for SFC data files
c   sfc_titcol: default TITCOL string to be used when the user did not specify
c               an output request title
c   sfc_labcol: LABCOL (type of measurement) string
c   sfc_unicol: UNICOL (unit of measurement) string
c
      integer    sfc_len
      parameter (sfc_len = 25)
c
      character*(sfc_len) sfc_tlab, sfc_tuni
      character*(*) sfc_vlab, sfc_vuni
      character*(*) sfc_ilab, sfc_iuni
      character*(*) sfc_plab, sfc_puni
      character*(*) sfc_elab, sfc_euni
      character*(*) sfc_qlab, sfc_quni
      character*(*) sfc_rlab, sfc_runi
      character*(*) sfc_glab, sfc_guni
      character*(*) sfc_llab, sfc_luni
      character*(*) sfc_clab, sfc_cuni
      character*(*) sfc_flab, sfc_funi
      character*(*) sfc_celab
      character*(*) sfc_pcdlab, sfc_ecdlab
      character*(*) sfc_pcclab, sfc_ecclab
      character*(*) sfc_lelab
      character*(*) sfc_pldlab, sfc_eldlab
      character*(*) sfc_plclab, sfc_elclab
      character*(*) sfc_srclab
      character*(*) sfc_inlab, sfc_outlab
      character*(*) sfc_rdlab, sfc_lenuni
      character*(*) sfc_vellab, sfc_veluni
      character*(*) sfc_alab, sfc_auni
      character*(*) sfc_klab
      character*(*) sfc_foillab, sfc_gaslab
      character*(*) sfc_kllab, sfc_ylab
      character*(*) sfc_allab,sfc_arlab,sfc_culab,sfc_krlab,sfc_xelab
      character*(*) sfc_moshlab, sfc_whitlab
      character*(*) sfc_ulab, sfc_uuni
      character*(*) sfc_tllab, sfc_strlab, sfc_lstlab
      character*(*) sfc_gchlab
      character*(*) sfc_stklab, sfc_xmlab, sfc_eflab, sfc_bflab
      character*(*) sfc_nonuni, sfc_efuni, sfc_bfuni
      character*(*) sfc_zflab, sfc_gzflab
      character*(*) sfc_alslab
      character*(*) sfc_cdlab, sfc_cduni
      character*(*) sfc_iclab, sfc_iplab
c
c ******************** Translation Section ******************************
c To translate from English to French, edit the following:
c
      parameter (sfc_tlab = 'Time', sfc_tuni = 'Seconds')
      parameter (sfc_vlab = 'Voltage', sfc_vuni = 'Volts')
      parameter (sfc_ilab = 'Current', sfc_iuni = 'Amps')
      parameter (sfc_plab = 'Power', sfc_puni = 'Watts')
      parameter (sfc_elab = 'Energy', sfc_euni = 'Joules')
      parameter (sfc_qlab = 'Charge', sfc_quni = 'Coulombs')
      parameter (sfc_rlab = 'Resistance', sfc_runi = 'Ohms')
      parameter (sfc_glab = 'Conductance', sfc_guni = 'Mhos')
      parameter (sfc_llab = 'Inductance', sfc_luni = 'Henrys')
      parameter (sfc_clab = 'Capacitance', sfc_cuni = 'Farads')
      parameter (sfc_flab = 'Flux', sfc_funi = 'Webers')
      parameter (sfc_celab = 'dCV/dV')
      parameter (sfc_pcdlab = 'V*V*dC/dt/2')
      parameter (sfc_ecdlab = 'Sum V*V*dC/dt/2')
      parameter (sfc_pcclab = 'd(C*V*V/2)/dt')
      parameter (sfc_ecclab = '(C*V*V)/2')
      parameter (sfc_lelab = 'dLI/dI')
      parameter (sfc_pldlab = 'I*I*dL/dt/2')
      parameter (sfc_eldlab = 'Sum I*I*dL/dt/2')
      parameter (sfc_plclab = 'd(L*I*I/2)/dt')
      parameter (sfc_elclab = '(L*I*I)/2')
      parameter (sfc_srclab = 'Source')
      parameter (sfc_inlab = 'Input')
      parameter (sfc_outlab = 'Output')
      parameter (sfc_rdlab = 'Radius', sfc_lenuni = 'Meters')
      parameter (sfc_vellab = 'Velocity', sfc_veluni = 'Meters/sec')
      parameter (sfc_alab = 'Acceleration')
      parameter (sfc_auni = 'Meters/sec/sec')
      parameter (sfc_klab = 'Kinetic')
      parameter (sfc_foillab = 'Foil', sfc_gaslab = 'Gas Puff')
      parameter (sfc_kllab = 'K-line', sfc_ylab = 'X-ray Yield')
      parameter (sfc_allab = 'Al', sfc_arlab = 'Ar', sfc_culab = 'Cu')
      parameter (sfc_krlab = 'Kr', sfc_xelab = 'Xe')
      parameter (sfc_moshlab = '(M)', sfc_whitlab = '(W)')
      parameter (sfc_ulab = 'User', sfc_uuni = 'Undefined')
      parameter (sfc_tllab = 'T-Line', sfc_strlab = 'Stored')
      parameter (sfc_lstlab = 'Lost')
      parameter (sfc_gchlab = 'Gas Channel')
      parameter (sfc_stklab = 'Stack')
      parameter (sfc_xmlab = 'MFI Criterion')
      parameter (sfc_eflab = 'Electric Field')
      parameter (sfc_bflab = 'Magnetic Field')
      parameter (sfc_nonuni = 'No Units')
      parameter (sfc_efuni = 'Volts/meter')
      parameter (sfc_bfuni = 'Tesla')
      parameter (sfc_zflab = 'Zflow', sfc_gzflab = '1/Zflow')
      parameter (sfc_alslab = 'Loss to Anode')
      parameter (sfc_cdlab = 'Current Density')
      parameter (sfc_cduni = 'Amps/m*m')
      parameter (sfc_iclab = 'Cathode', sfc_iplab = 'Plasma')
c
c **************  End of translation section *********************
c
      character sfc_titcol(200)*(sfc_len)
      character sfc_labcol(200)*(sfc_len)
      character sfc_unicol(200)*(sfc_len)
c
      sfc_titcol(  1) = sfc_vlab//' R1'
      sfc_titcol(  2) = sfc_vlab//' R2'
      sfc_titcol(  3) = sfc_vlab//' R3'
      sfc_titcol(  4) = sfc_ilab//' R1'
      sfc_titcol(  5) = sfc_ilab//' R2'
      sfc_titcol(  6) = sfc_ilab//' R3'
      sfc_titcol(  7) = sfc_plab//' R1'
      sfc_titcol(  8) = sfc_plab//' R2'
      sfc_titcol(  9) = sfc_plab//' R3'
      sfc_titcol( 10) = sfc_elab//' R1'
      sfc_titcol( 11) = sfc_elab//' R2'
      sfc_titcol( 12) = sfc_elab//' R3'
      sfc_titcol( 13) = sfc_rlab//' R1'
      sfc_titcol( 14) = sfc_rlab//' R2'
      sfc_titcol( 15) = sfc_rlab//' R3'
      sfc_titcol( 16) = sfc_qlab//' R1'
      sfc_titcol( 17) = sfc_qlab//' R2'
      sfc_titcol( 18) = sfc_qlab//' R3'
      sfc_titcol( 19) = sfc_vlab//' C1'
      sfc_titcol( 20) = sfc_vlab//' C3'
      sfc_titcol( 21) = sfc_ilab//' C1'
      sfc_titcol( 22) = sfc_ilab//' C3'
      sfc_titcol( 23) = sfc_plab//' C1'
      sfc_titcol( 24) = sfc_plab//' C3'
      sfc_titcol( 25) = sfc_elab//' C1'
      sfc_titcol( 26) = sfc_elab//' C3'
      sfc_titcol( 27) = sfc_clab//' C1'
      sfc_titcol( 28) = sfc_clab//' C3'
      sfc_titcol( 29) = sfc_qlab//' C1'
      sfc_titcol( 30) = sfc_qlab//' C3'
      sfc_titcol( 53) = sfc_celab//' C1'
      sfc_titcol( 54) = sfc_celab//' C3'
      sfc_titcol( 60) = sfc_pcdlab
      sfc_titcol( 61) = sfc_ecdlab
      sfc_titcol( 62) = sfc_pcclab
      sfc_titcol( 63) = sfc_ecclab
      sfc_titcol( 31) = sfc_vlab//' L2'
      sfc_titcol( 32) = sfc_ilab//' L2'
      sfc_titcol( 33) = sfc_plab//' L2'
      sfc_titcol( 34) = sfc_elab//' L2'
      sfc_titcol( 35) = sfc_llab//' L2'
      sfc_titcol( 36) = sfc_flab//' L2'
      sfc_titcol( 37) = sfc_qlab//' L2'
      sfc_titcol( 55) = sfc_lelab//' L2'
      sfc_titcol( 64) = sfc_pldlab
      sfc_titcol( 65) = sfc_eldlab
      sfc_titcol( 66) = sfc_plclab
      sfc_titcol( 67) = sfc_elclab
      sfc_titcol( 38) = sfc_srclab//' '//sfc_vlab
      sfc_titcol( 39) = sfc_srclab//' '//sfc_ilab
      sfc_titcol( 40) = sfc_srclab//' '//sfc_plab
      sfc_titcol( 41) = sfc_srclab//' '//sfc_elab
      sfc_titcol( 42) = sfc_srclab//' '//sfc_qlab
      sfc_titcol( 43) = sfc_inlab//' '//sfc_vlab
      sfc_titcol( 44) = sfc_inlab//' '//sfc_ilab
      sfc_titcol( 45) = sfc_inlab//' '//sfc_plab
      sfc_titcol( 46) = sfc_inlab//' '//sfc_elab
      sfc_titcol( 47) = sfc_inlab//' '//sfc_qlab
      sfc_titcol( 48) = sfc_outlab//' '//sfc_vlab
      sfc_titcol( 49) = sfc_outlab//' '//sfc_ilab
      sfc_titcol( 50) = sfc_outlab//' '//sfc_plab
      sfc_titcol( 51) = sfc_outlab//' '//sfc_elab
      sfc_titcol( 52) = sfc_outlab//' '//sfc_qlab
      sfc_titcol( 56) = sfc_foillab//' '//sfc_rdlab
      sfc_titcol( 57) = sfc_foillab//' '//sfc_vellab
      sfc_titcol( 58) = sfc_foillab//' '//sfc_alab
      sfc_titcol( 59) = sfc_gaslab//' '//sfc_klab//' '//sfc_elab
      sfc_titcol( 68) = sfc_gaslab//' '//sfc_rdlab
      sfc_titcol( 69) = sfc_gaslab//' '//sfc_vellab
      sfc_titcol( 70) = sfc_gaslab//' '//sfc_alab
      sfc_titcol( 71) = sfc_gaslab//' '//sfc_klab//' ' //sfc_elab
      sfc_titcol(101) = sfc_allab//' '//sfc_kllab//' '//sfc_whitlab
      sfc_titcol(102) = sfc_allab//' '//sfc_kllab//' '//sfc_moshlab
      sfc_titcol(103) = sfc_arlab//' '//sfc_kllab//' '//sfc_whitlab
      sfc_titcol(104) = sfc_arlab//' '//sfc_kllab//' '//sfc_moshlab
      sfc_titcol(105) = sfc_culab//' '//sfc_kllab//' '//sfc_whitlab
      sfc_titcol(106) = sfc_culab//' '//sfc_kllab//' '//sfc_moshlab
      sfc_titcol(107) = sfc_krlab//' '//sfc_kllab//' '//sfc_whitlab
      sfc_titcol(108) = sfc_krlab//' '//sfc_kllab//' '//sfc_moshlab
      sfc_titcol(109) = sfc_xelab//' '//sfc_kllab//' '//sfc_whitlab
      sfc_titcol(110) = sfc_xelab//' '//sfc_kllab//' '//sfc_moshlab
      sfc_titcol( 72) = sfc_ulab//' 1'
      sfc_titcol( 73) = sfc_ulab//' 2'
      sfc_titcol( 74) = sfc_ulab//' 3'
      sfc_titcol( 75) = sfc_ulab//' 4'
      sfc_titcol( 76) = sfc_ulab//' 5'
      sfc_titcol( 77) = sfc_ulab//' 6'
      sfc_titcol( 78) = sfc_ulab//' 7'
      sfc_titcol( 79) = sfc_ulab//' 8'
      sfc_titcol( 80) = sfc_ulab//' 9'
      sfc_titcol( 81) = sfc_ulab//' 10'
      sfc_titcol( 82) = sfc_tllab//' '//sfc_lstlab//' '//sfc_elab
      sfc_titcol( 83) = sfc_tllab//' '//sfc_lstlab//' '//sfc_plab
      sfc_titcol( 84) = sfc_tllab//' '//sfc_strlab//' '//sfc_elab
      sfc_titcol( 85) = sfc_tllab//' '//sfc_strlab//' '//sfc_plab
      sfc_titcol( 86) = sfc_gchlab//' '//sfc_rlab
      sfc_titcol( 90) = sfc_stklab//' '//sfc_eflab
      sfc_titcol( 91) = sfc_stklab//' '//sfc_bflab
      sfc_titcol( 92) = sfc_stklab//' '//sfc_xmlab
      sfc_titcol( 93) = sfc_zflab
      sfc_titcol( 94) = sfc_gzflab
      sfc_titcol( 95) = sfc_zflab
      sfc_titcol( 96) = sfc_gzflab
      sfc_titcol( 97) = sfc_alslab
      sfc_titcol( 98) = sfc_iclab//' '//sfc_ilab
      sfc_titcol( 99) = sfc_iplab//' '//sfc_ilab
      sfc_titcol(100) = sfc_zflab
c
      sfc_labcol(  1) = sfc_vlab
      sfc_labcol(  2) = sfc_vlab
      sfc_labcol(  3) = sfc_vlab
      sfc_labcol(  4) = sfc_ilab
      sfc_labcol(  5) = sfc_ilab
      sfc_labcol(  6) = sfc_ilab
      sfc_labcol(  7) = sfc_plab
      sfc_labcol(  8) = sfc_plab
      sfc_labcol(  9) = sfc_plab
      sfc_labcol( 10) = sfc_elab
      sfc_labcol( 11) = sfc_elab
      sfc_labcol( 12) = sfc_elab
      sfc_labcol( 13) = sfc_rlab
      sfc_labcol( 14) = sfc_rlab
      sfc_labcol( 15) = sfc_rlab
      sfc_labcol( 16) = sfc_qlab
      sfc_labcol( 17) = sfc_qlab
      sfc_labcol( 18) = sfc_qlab
      sfc_labcol( 19) = sfc_vlab
      sfc_labcol( 20) = sfc_vlab
      sfc_labcol( 21) = sfc_ilab
      sfc_labcol( 22) = sfc_ilab
      sfc_labcol( 23) = sfc_plab
      sfc_labcol( 24) = sfc_plab
      sfc_labcol( 25) = sfc_elab
      sfc_labcol( 26) = sfc_elab
      sfc_labcol( 27) = sfc_clab
      sfc_labcol( 28) = sfc_clab
      sfc_labcol( 29) = sfc_qlab
      sfc_labcol( 30) = sfc_qlab
      sfc_labcol( 53) = sfc_celab
      sfc_labcol( 54) = sfc_celab
      sfc_labcol( 60) = sfc_pcdlab
      sfc_labcol( 61) = sfc_ecdlab
      sfc_labcol( 62) = sfc_pcclab
      sfc_labcol( 63) = sfc_ecclab
      sfc_labcol( 31) = sfc_vlab
      sfc_labcol( 32) = sfc_ilab
      sfc_labcol( 33) = sfc_plab
      sfc_labcol( 34) = sfc_elab
      sfc_labcol( 35) = sfc_llab
      sfc_labcol( 36) = sfc_flab
      sfc_labcol( 37) = sfc_qlab
      sfc_labcol( 55) = sfc_lelab
      sfc_labcol( 64) = sfc_pldlab
      sfc_labcol( 65) = sfc_eldlab
      sfc_labcol( 66) = sfc_plclab
      sfc_labcol( 67) = sfc_elclab
      sfc_labcol( 38) = sfc_vlab
      sfc_labcol( 39) = sfc_ilab
      sfc_labcol( 40) = sfc_plab
      sfc_labcol( 41) = sfc_elab
      sfc_labcol( 42) = sfc_qlab
      sfc_labcol( 43) = sfc_vlab
      sfc_labcol( 44) = sfc_ilab
      sfc_labcol( 45) = sfc_plab
      sfc_labcol( 46) = sfc_elab
      sfc_labcol( 47) = sfc_qlab
      sfc_labcol( 48) = sfc_vlab
      sfc_labcol( 49) = sfc_ilab
      sfc_labcol( 50) = sfc_plab
      sfc_labcol( 51) = sfc_elab
      sfc_labcol( 52) = sfc_qlab
      sfc_labcol( 56) = sfc_rdlab
      sfc_labcol( 57) = sfc_vellab
      sfc_labcol( 58) = sfc_alab
      sfc_labcol( 59) = sfc_klab//' '//sfc_elab
      sfc_labcol( 68) = sfc_rdlab
      sfc_labcol( 69) = sfc_vellab
      sfc_labcol( 70) = sfc_alab
      sfc_labcol( 71) = sfc_klab//' '//sfc_elab
      sfc_labcol(101) = sfc_ylab
      sfc_labcol(102) = sfc_ylab
      sfc_labcol(103) = sfc_ylab
      sfc_labcol(104) = sfc_ylab
      sfc_labcol(105) = sfc_ylab
      sfc_labcol(106) = sfc_ylab
      sfc_labcol(107) = sfc_ylab
      sfc_labcol(108) = sfc_ylab
      sfc_labcol(109) = sfc_ylab
      sfc_labcol(110) = sfc_ylab
      sfc_labcol( 72) = sfc_ulab//' 1'
      sfc_labcol( 73) = sfc_ulab//' 2'
      sfc_labcol( 74) = sfc_ulab//' 3'
      sfc_labcol( 75) = sfc_ulab//' 4'
      sfc_labcol( 76) = sfc_ulab//' 5'
      sfc_labcol( 77) = sfc_ulab//' 6'
      sfc_labcol( 78) = sfc_ulab//' 7'
      sfc_labcol( 79) = sfc_ulab//' 8'
      sfc_labcol( 80) = sfc_ulab//' 9'
      sfc_labcol( 81) = sfc_ulab//' 10'
      sfc_labcol( 82) = sfc_elab
      sfc_labcol( 83) = sfc_elab
      sfc_labcol( 84) = sfc_elab
      sfc_labcol( 85) = sfc_elab
      sfc_labcol( 86) = sfc_rlab
      sfc_labcol( 90) = sfc_eflab
      sfc_labcol( 91) = sfc_bflab
      sfc_labcol( 92) = sfc_xmlab
      sfc_labcol( 93) = sfc_zflab
      sfc_labcol( 94) = sfc_gzflab
      sfc_labcol( 95) = sfc_zflab
      sfc_labcol( 96) = sfc_gzflab
      sfc_labcol( 97) = sfc_cdlab
      sfc_labcol( 98) = sfc_ilab
      sfc_labcol( 99) = sfc_ilab
      sfc_labcol(100) = sfc_zflab
c
      sfc_unicol(  1) = sfc_vuni
      sfc_unicol(  2) = sfc_vuni
      sfc_unicol(  3) = sfc_vuni
      sfc_unicol(  4) = sfc_iuni
      sfc_unicol(  5) = sfc_iuni
      sfc_unicol(  6) = sfc_iuni
      sfc_unicol(  7) = sfc_puni
      sfc_unicol(  8) = sfc_puni
      sfc_unicol(  9) = sfc_puni
      sfc_unicol( 10) = sfc_euni
      sfc_unicol( 11) = sfc_euni
      sfc_unicol( 12) = sfc_euni
      sfc_unicol( 13) = sfc_runi
      sfc_unicol( 14) = sfc_runi
      sfc_unicol( 15) = sfc_runi
      sfc_unicol( 16) = sfc_quni
      sfc_unicol( 17) = sfc_quni
      sfc_unicol( 18) = sfc_quni
      sfc_unicol( 19) = sfc_vuni
      sfc_unicol( 20) = sfc_vuni
      sfc_unicol( 21) = sfc_iuni
      sfc_unicol( 22) = sfc_iuni
      sfc_unicol( 23) = sfc_puni
      sfc_unicol( 24) = sfc_puni
      sfc_unicol( 25) = sfc_euni
      sfc_unicol( 26) = sfc_euni
      sfc_unicol( 27) = sfc_cuni
      sfc_unicol( 28) = sfc_cuni
      sfc_unicol( 29) = sfc_quni
      sfc_unicol( 30) = sfc_quni
      sfc_unicol( 53) = sfc_cuni
      sfc_unicol( 54) = sfc_cuni
      sfc_unicol( 60) = sfc_puni
      sfc_unicol( 61) = sfc_euni
      sfc_unicol( 62) = sfc_puni
      sfc_unicol( 63) = sfc_euni
      sfc_unicol( 31) = sfc_vuni
      sfc_unicol( 32) = sfc_iuni
      sfc_unicol( 33) = sfc_puni
      sfc_unicol( 34) = sfc_euni
      sfc_unicol( 35) = sfc_luni
      sfc_unicol( 36) = sfc_funi
      sfc_unicol( 37) = sfc_quni
      sfc_unicol( 55) = sfc_luni
      sfc_unicol( 64) = sfc_puni
      sfc_unicol( 65) = sfc_euni
      sfc_unicol( 66) = sfc_puni
      sfc_unicol( 67) = sfc_euni
      sfc_unicol( 38) = sfc_vuni
      sfc_unicol( 39) = sfc_iuni
      sfc_unicol( 40) = sfc_puni
      sfc_unicol( 41) = sfc_euni
      sfc_unicol( 42) = sfc_quni
      sfc_unicol( 43) = sfc_vuni
      sfc_unicol( 44) = sfc_iuni
      sfc_unicol( 45) = sfc_puni
      sfc_unicol( 46) = sfc_euni
      sfc_unicol( 47) = sfc_quni
      sfc_unicol( 48) = sfc_vuni
      sfc_unicol( 49) = sfc_iuni
      sfc_unicol( 50) = sfc_puni
      sfc_unicol( 51) = sfc_euni
      sfc_unicol( 52) = sfc_quni
      sfc_unicol( 56) = sfc_lenuni
      sfc_unicol( 57) = sfc_veluni
      sfc_unicol( 58) = sfc_auni
      sfc_unicol( 59) = sfc_euni
      sfc_unicol( 68) = sfc_lenuni
      sfc_unicol( 69) = sfc_veluni
      sfc_unicol( 70) = sfc_auni
      sfc_unicol( 71) = sfc_euni
      sfc_unicol(101) = sfc_euni
      sfc_unicol(102) = sfc_euni
      sfc_unicol(103) = sfc_euni
      sfc_unicol(104) = sfc_euni
      sfc_unicol(105) = sfc_euni
      sfc_unicol(106) = sfc_euni
      sfc_unicol(107) = sfc_euni
      sfc_unicol(108) = sfc_euni
      sfc_unicol(109) = sfc_euni
      sfc_unicol(110) = sfc_euni
      sfc_unicol( 72) = sfc_uuni
      sfc_unicol( 73) = sfc_uuni
      sfc_unicol( 74) = sfc_uuni
      sfc_unicol( 75) = sfc_uuni
      sfc_unicol( 76) = sfc_uuni
      sfc_unicol( 77) = sfc_uuni
      sfc_unicol( 78) = sfc_uuni
      sfc_unicol( 79) = sfc_uuni
      sfc_unicol( 80) = sfc_uuni
      sfc_unicol( 81) = sfc_uuni
      sfc_unicol( 82) = sfc_euni
      sfc_unicol( 83) = sfc_euni
      sfc_unicol( 84) = sfc_euni
      sfc_unicol( 85) = sfc_euni
      sfc_unicol( 86) = sfc_lenuni
      sfc_unicol( 90) = sfc_efuni
      sfc_unicol( 91) = sfc_bfuni
      sfc_unicol( 92) = sfc_nonuni
      sfc_unicol( 93) = sfc_runi
      sfc_unicol( 94) = sfc_guni
      sfc_unicol( 95) = sfc_runi
      sfc_unicol( 96) = sfc_guni
      sfc_unicol( 97) = sfc_cduni
      sfc_unicol( 98) = sfc_iuni
      sfc_unicol( 99) = sfc_iuni
      sfc_unicol(100) = sfc_runi
