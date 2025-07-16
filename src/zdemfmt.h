c *****************************************************************************
c  Various format statements for read_screamer_data output.
c  To get these into use:  INCLUDE  'ZDEMFMT.H'  .
c *****************************************************************************
c
c  Modifications:
c    03/07/95, MLK, made format 72 to be 72 characters
c                   for each line
c    04/28/95, MLK, corrected spelling errors in format statements 48, 85
c    05/30/95, KWS, added statement 162 for Zflow Plasma Loss Model
c    06/07/95, MLK, added format 101 for CSV output type
c    08/18/95, MLK, fixed more lines longer than 72 characters
c    08/12/97, KWS, added format for Measure Zflow Block and forward/
c                   reverse current directions in Zflow plasma loss
c                   and Zflow POS models
c    12/23/97, MLK, added format 103 for SFC output type
c 2008-12-5 RBS: Format 15,16,17,18,20,27,62 removed from all code calls
c
c ---------------------------------------------------------------------------
c
c 3    format (' ', 'Input file read with no errors.')
c 4    format (' ', '### Errors found in input file, execution halted.')
c 5    format (' ', '### Unable to open input file, exection halted.')
c 7    format (' '/ ' ', 6x, a80 /)
c 8    format (' ', a16, ': ', 1pe10.3)
c 9    format (' ', a16, ': ', i10)
c 10   format (' ', 'Grids on plots')
c 11   format (' ', 'No grids on plots')
c 60   format (' ', 'Write files containing the plotted points')
c 61   format (' ', 'Do not write files containing the plotted points')
c 13   format (' ', 'Execute all cycles')
c 14   format (' ', 'Execute only one cycle')
c 90   format (' ', 'Echo the setup parameters and indicies')
c 91   format (' ', 'Do not echo the setup parameters and indicies')
c 16   format (' '/ '************ Branch ', i2, ' ************')
c 24   format (' ', '***** Block ', i3, ' has ', a6, ' branch ',
c     &             'exitting it (branch ', i2, ') *****')
c 17   format (' ', 'Block ', i3, ' : transmission line with impedance ',
c     &             'varying ', a13/
c     &        ' ', 2x, 'Tau=', 1pe10.3,  '   Zin=', 1pe10.3,
c     &             '   Zout=', 1pe10.3, '   Tres=', 1pe10.3)
c 18   format (' ', 'Block ', i3, ' : MITL (geometric setup)'/
c     &        ' ', 2x, 'Circumference=', 1pe10.3, '   Gap=', 1pe10.3,
c     &               '   Tau=',    1pe10.3,   '   Z=', 1pe10.3,
c     &               '   Tres=',   1pe10.3,'   Eturnon=',    1pe10.3)
c 15   format (' ', 'Block ', i3, ' : MITL (perveance setup)'/
c     &        ' ', 2x, 'Perveance=', 1pe10.3,  '   Tau=', 1pe10.3,
c     &                      '   Z=', 1pe10.3, '   Tres=', 1pe10.3)
c 19   format (' ', 'Block ', i3, ' : resistor and capacitor to ground.'/
c     &        ' ', 2x, 'R1=', 1pe10.3, '   C1=', 1pe10.3)
c 62   format (' ', 'Block ', i3, ' : resistor and inductor in series.'/
c     &        ' ', 2x, 'R2=', 1pe10.3, '   L2=', 1pe10.3)
c 20   format (' ', 'Block ', i3, ' : pi section. '/
c     &        ' ', 2x, 'R1=', 1pe10.3, '   C1=', 1pe10.3,
c     &              '   R2=', 1pe10.3, '   L2=', 1pe10.3,
c     &              '   R3=', 1pe10.3, '   C3=', 1pe10.3)
c 27   format (' ', 'Block ', i3, ' : adder block.')
c 94   format (' ', 'Block ', i3, ' : transformer block.'/
c     &        ' ', 2x, 'Lp=', 1pe10.3, '   Ls=', 1pe10.3,
c     &                '   M=', 1pe10.3)
c102   format (' ', 'Block ', i3, ' : Measure Zflow Block'/
c     &        ' ', 2x, 'Line Impedance = ', 1pe10.3,
c     &        ' ', 2x, 'Measure Zflow Block Number = ',i2)
c 40   format (' ', 'Block ', i3, ' : end-of-branch voltage source ',
c     &             'described as a function of time.'/
c     &        ' ', 2x, 'R2=', 1pe10.3, '   L2=', 1pe10.3)
c 45   format (' ', 'Block ', i3, ' : voltage source described as a ',
c     &             'function of time.'/
c     &        ' ', 2x, 'R2=', 1pe10.3, '   L2=', 1pe10.3)
c 67   format (' ', 'Block ', i3, ' : end-of-branch current source ',
c     &             'described as a function of time.'/
c     &        ' ', 2x, 'R1=', 1pe10.3, '   C1=', 1pe10.3)
c 87   format (' ', 'Block ', i3, ' : end-of-branch SCL current source ',
c     &             'described as a function of time.'/
c     &        ' ', 2x, 'R1=', 1pe10.3, '   C1=', 1pe10.3)
c 66   format (' ', 'Block ', i3, ' : current source described as a ',
c     &             'function of time.'/
c     &        ' ', 2x, 'R3=', 1pe10.3, '   C3=', 1pe10.3)
c 53   format (' ', 'Block ', i3, ' : imploding CYLfoil model.'/
c     &        ' ', 2x,  'Initrad=',    1pe10.3, '   Length=', 1pe10.3,
c     &                  '   Mass=',    1pe10.3, '   Minrad=', 1pe10.3)
c 54   format (' ', 'Block ', i3, ' : imploding NShell model.'/
c     &        ' ', 2x,  'shell length =',    1pe10.3,
c     &          '   min radius =', 1pe10.3, '  A-K gap = ',1pe10.3,
c     &          '   trapped field time = ',1pe10.3)
c153   format (' ', 'Block ', i3, ' : imploding SPHfoil model.'/
c     &        ' ', 2x,  'Initrad=',    1pe10.3, '   ANGLE=', 1pe10.3,
c     &                  '   Mass=',    1pe10.3, '   Minrad=', 1pe10.3)
c 96   format (' ', 'Block ', i3, ' : imploding gas puff model.'/
c     &        ' ', 2x,  'Initrad=',    1pe10.3, '   Length=',   1pe10.3,
c     &               '   Density=',    1pe10.3, '   Minrad=',   1pe10.3,
c     &              '   Innerrad=',    1pe10.3, '   InitMass=', 1pe10.3)
c 46   format (' ', 4x, 'Sin-squared function:'/
c     &        ' ', 6x, 'Magnitude=',  1pe10.3, '   Tpulse=', 1pe10.3,
c     &                 '   Tdelay=',  1pe10.3)
c 98   format (' ', 4x, 'Sin function:'/
c     &        ' ', 6x, 'Magnitude=',  1pe10.3, '   Period=', 1pe10.3,
c     &                 '   Tdelay=',  1pe10.3)
c
c---- remaining calls in rdscrelem
c
 47   format (' ', 4x, 'Least squares polynomial:'/
     &      7(' ', 6x, 'A', i1, '=', 1pe10.3, :/))
 48   format (' ', 4x, 'Tabular function:'/
     &        ' ', 6x, 'Point', 6x, a10, 5x, a10/
     &    400(' ', 8x, i3, 5x, 1pe10.3, 5x, 1pe10.3, :/))
 49   format (' ', 4x, 'Scale=', 1pe10.3, '   Tdelay=', 1pe10.3)
 85   format (' ', 4x, 'Tabular conditional function:'/
     &        ' ', 6x, 'Point', 6x, a10, 5x, a10, 5x, a10/
     &    400(' ', 8x, i3, 6x, 1pe10.3, 5x, 1pe10.3, 5x, 1pe10.3:/))
 86   format (' ', 4x, 'Scale=', 1pe10.3, '   Cond-scale=', 1pe10.3,
     &             '   Tdelay=', 1pe10.3)

c 23   format (' ', 4x, a2, ' is a variable element in block ', i3)
c 64   format (' ', 6x, '### ERROR ###  element ', a2, ' may not be 
c     &                 used with this model.')
c 65   format (' ', 6x, 'User supplied model.')
c 22   format (' ', 6x, 'Exponential model of a resistive switch:'/
c     &        ' ', 8x,      'Ropen=', 1pe10.3, '   Rclose=', 1pe10.3,
c     &                 '   Tswitch=', 1pe10.3, '   Tau=',    1pe10.3,
c     &                 '   Zswitch=', 1pe10.3)
c 30   format (' ', 6x, 'Decay model of a resistive switch:'/
c     &        ' ', 8x,      'Ropen=', 1pe10.3, '   Rclose=', 1pe10.3,
c     &                 '   Tswitch=', 1pe10.3, '   Tau=',    1pe10.3)
c 32   format (' ', 6x, 'Rise model of a resistive switch:'/
c     &        ' ', 8x,      'Ropen=', 1pe10.3, '   Rclose=', 1pe10.3,
c     &                 '   Tswitch=', 1pe10.3, '   Tau=',    1pe10.3)
c 52   format (' ', 6x, 'PEOS model 1:'/
c     &        ' ', 8x, 'Tswitch=', 1pe10.3, '   Constant=', 1pe10.3,
c     &                 '   Rmax=', 1pe10.3,     '   Rmin=', 1pe10.3)
c152   format (' ', 6x, ' Z FLOW POS model  :'/
c     & ' ', 2x, 'tsw =', 1pe10.3,' cursw =',1pe10.3,' topen=', 1pe10.3,
c     &                 ' zflow=', 1pe10.3,     ' gswmin=', 1pe10.3,
c     &                 ' gswmax=',1pe10.3,' CBflag=',1pe10.3,
c     &                 '  forward = ', 1pe10.3)
c 92   format (' ', 6x, 'PEOS model 2:'/
c     &        ' ', 8x, 'Qswitch=', 1pe10.3, '   Constant=', 1pe10.3,
c     &                 '   Rmax=', 1pe10.3,     '   Rmin=', 1pe10.3)
c154   format (' ', 6x, 'SW1 model  :'/
c     &        ' ', 6x, 'DIEL1=',   a3, 2x,    '  TBD1=',  1pe10.3,
c     &                 ' DM1=', 1pe10.3,      '  ATM1=', 1pe10.3,
c     &                 ' XSW1=', 1pe10.3,      '  XCH1=',  1pe10.3)
c158   format (' ', 6x, ' MIP POS ( CTOPS ) model:'/
c     & ' ', 2x, 'length  =',1pe10.3,' gap0    =', 1pe10.3, 
c     &          'gapmin =',1pe10.3,/
c     & ' ', 2x, 'radius  =',1pe10.3,' gmax    =', 1pe10.3, 
c     &          ' pitch  =',1pe10.3,/
c     & ' ', 2X, 'massnum =',1pe10.3,'  numden =', 1pe10.3, 
c     &          ' econst =',1pe10.3,/  
c     & ' ', 2X, 'ibigpo  =',1pe10.3)
c160   format (' ', 6x, ' MFI Insulator CB model  :'/
c     & ' ', 2x, 'radius =', 1pe10.3,' dgap =',1pe10.3,
c     &                 ' gmin =', 1pe10.3,
c     &                 ' gmax =', 1pe10.3,     ' xni =', 1pe10.3,
c     &                 ' flash =',1pe10.3)
c161   format (' ', 6x, ' RWALL resistive wall model  :'/
c     & ' ', 2x, 't_in =', 1pe10.3,
c     & ' ', 2x, 'gvalue =', 1pe10.3/
c     & ' ', 2x, 'disk1inner =', 1pe10.3,
c     & ' ', 2x, 'disk1outer =', 1pe10.3/
c     & ' ', 2x, 'disk2inner =', 1pe10.3,
c     & ' ', 2x, 'disk2outer =', 1pe10.3/
c     & ' ', 2x, 'cyl_radius_1 =', 1pe10.3,
c     & ' ', 2x, 'cyl_length_1 =', 1pe10.3/
c     & ' ', 2x, 'cyl_radius_2 =', 1pe10.3,
c     & ' ', 2x, 'cyl_length_2 =', 1pe10.3)
c162   format (' ', 6x, ' Z FLOW Plasma Loss Current model  :'/
c     & ' ', 2x, 'zflow =', 1pe10.3,' gap =',1pe10.3,' radius=', 1pe10.3,
c     &                 ' gmin=', 1pe10.3,     ' gmax=', 1pe10.3,
c     &'  xni=', 1pe10.3, '  forward = ', 1pe10.3)
c 63   format (' ', 6x, 'Diode model:'/
c     &        ' ', 8x, 'Tdelay=', 1pe10.3,' Rmax=', 1pe10.3,
c     &                 'Rmin=',   1pe10.3,' Area=', 1pe10.3,/,
c     &             8x, 'Gap=',    1pe10.3,' Vel.=', 1pe10.3,
c     &                 'Min-Gap=',1pe10.3,' Pmass_ratio=',1pe10.3)
c 97   format (' ', 6x, 'Applied-B Diode model:'/
c     &        ' ', 8x,    'Gap=', 1pe10.3, '  Area=',    1pe10.3,
c     &           '  Ion Charge=', 1pe10.3, '  Ion AMU=', 1pe10.3,
c     &                   '  B0=', 1pe10.3, '  X0=',      1pe10.3/
c     &        ' ', 8x,   'Rmin=', 1pe10.3, '  Rmax=',    1pe10.3,
c     &          '  IonFraction=', 1pe10.3, '  Ratemax=', 1pe10.3)
c 51   format (' ', 6x, 'Saturable core inductor switch model:'/
c     &        ' ', 8x,  'PkFrac=',    1pe10.3, '   Rinner=', 1pe10.3,
c     &               '   Router=',    1pe10.3, '   Width=',  1pe10.3/
c     &        ' ', 8x,      'H1=',    1pe10.3, '   Hsat=',   1pe10.3,
c     &                 '   Hrev=',    1pe10.3, '   Bsat=',   1pe10.3)
c 70   format (' ', 2x, 'Initial condition: ', a18, 1pe10.3)
c 81   format (' ', 'PLOT Output Request for block ', i3,
c     &             '.    Variable: ', a23,/
c     &        ' ', 2x, 'Tstart=', 1pe10.3, '   Tstop=', 1pe10.3,
c     &                '   Ymin=', 1pe10.3,  '   Ymax=', 1pe10.3)
c 82   format (' ', 'PRINT Output Request for block ', i3,
c     &             '.    Variable: ', a23,/
c     &        ' ', 2x, 'Tstart=', 1pe10.3, '   Tstop=', 1pe10.3)
c 83   format (' ', 'FILE Output Request for block ', i3,
c     &             '.    Variable: ', a23,/
c     &        ' ', 2x, 'Tstart=', 1pe10.3, '   Tstop=', 1pe10.3)
c 84   format (' ', 'TABLE Output Request for block ', i3,
c     &             '.    Variable: ', a23,/
c     &        ' ', 2x, 'Tstart=', 1pe10.3, '   Tstop=', 1pe10.3)
c 93   format (' ', 'PFF Output Request for block ', i3,
c     &             '.    Variable: ', a23)
c 95   format (' ', 'UFO Output Request for block ', i3,
c     &             '.    Variable: ', a23)
c 99   format (' ', 'IDR Output Request for block ', i3,
c     &             '.    Variable: ', a23)
c101   format (' ', 'CSV Output Request for block ', i3,
c     &             '.    Variable: ', a23)
c103   format (' ', 'SFC Output Request for block ', i3,
c     &             '.    Variable: ', a23)
c 88   format (' ', 2x,a)
c 31   format (' ', '### ERROR ### ', a16, ' not specified!')
c 42   format (' ', '### ERROR ### at least one block must precede a ',
c     &             'print or plot request in this branch!')
c 44   format (' ', '### ERROR ### two plot cards in a row are not ',
c     &             'allowed!')
c
c ***
c
c 71   format('  No user subroutines were included in this ',
c     &       'SCREAMER run'/)
c 72   format('  The following user subroutines were linked into ',
c     &       'SCREAMER for this run:'/)
c 73   format(' ',a)
