c----------------------------------------------------------------------
c    @(#)rdscrdat.h   version 1.0   created 06/15/2005 by Mathias Bavay
c----------------------------------------------------------------------
c---------------------------------------------------------------------
c
c  This is the common block for those variables used by the processing
c  of the input deck.
c
c  Modifications:
c  2014-02-06 RBS: Changed real*4 to real
c                  Reorder common /rdscrnumbers/ 64 bit first
c  2014-10-23 RBS: Parameter (max_fields) increased to 12
c  2014-10-23 RBS: Changed integer*4 to integer
c  2015-03-28 RBS: Material changed from *3 to *80 to match field
c                  Length of labelout increased to *26
c  2017-02-24 RBS: Added char*120 currline_lc and placed it in common
c  2020-10-19 RBS: Increased the number of flag calls to 12 to match
c                   max_fields used in field
c---------------------------------------------------------------------

c
c Various data types for this subroutine.
c
      parameter (max_fields = 12)
      
      character  field(max_fields)*80, label2*2, keychar*1,
     &           cbranch*2, cblock*2, branch_block*4
      character  trllabel*20, label18*18, labelout*26, labelplt*11,
     &           labelidr*8, ulabel*11, material*80
      character  currline*120, currline_lc*120,
     &           currdate*9, hostmach*80, date_mach*80
      character  keyword*(keyword_len), keywordsave*(keyword_len)
      
      common /rdscrstring1/ field, label2, keychar,
     &          cbranch, cblock, branch_block
      common /rdscrstring2/ trllabel, label18, labelout, labelplt,
     &          labelidr, ulabel, material
      common /rdscrstring3/ currline, currline_lc, currdate, hostmach,
     &          date_mach, keyword, keywordsave
      
      real       l2, mode, massnum, numden, ibigpo
      integer    flag, flag2, flag3, flag4, flag5, flag6,
     &           flag7, flag8, flag9, flag10, flag11, flag12,
     &           tstepflg, endtflg, nprtflg,
     &           ncyclflg, restflg, eofflg, lcirblk

      common /rdscrnumbers/ l2, mode, massnum, numden, ibigpo,
     &           flag, flag2, flag3, flag4, flag5, flag6
     &           flag7, flag8, flag9, flag10, flag11, flag12,
     &           tstepflg, endtflg, nprtflg,
     &           ncyclflg, restflg, eofflg, lcirblk
     
      integer    nblks, nbrns, nublks, nvarl, nlines,
     &           numerr, nsaverr, nsecbrn, lastblk
      
      common /rdscrcounters/ nblks, nbrns, nublks, nvarl, nlines,
     &           numerr, nsaverr, nsecbrn, lastblk
     
c
c Set various other parameters
c
      character comment_key*1, diaglabel_key*1, diaglabel_altkey*1
      parameter (comment_key = '!')
      parameter (diaglabel_key = '$')
      parameter (diaglabel_altkey = '%')
      integer    err,       noerr,     k_found
      parameter (err   = 1, noerr = 0, k_found = 10)
      parameter (notext = 0)
      parameter (lunit = 4)
      parameter (abdratemax = 0.05)  !max growth rate for applied-B diode

