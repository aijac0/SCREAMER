      subroutine echo_setup
c
c Echo the set up parameters and circuit element blocks.
c Sort of cryptic, but useful.
c
c -----------------------------------------------------------------------
c  Modified:
c    07/19/95, MLK, Redo format statements
c 2019-11-29 RBS: Adjusted the output formation for the blocks
c -----------------------------------------------------------------------
      use zdemmax
      use zdemcomm
      use zdemout
c
      character  setupln*80
      parameter (setupln = 'SCREAMER Setup Parameters (for debugging)')
c
c Put up a banner page and another page with the title.
c
      call banner (setupln)
c      call print_title
c
c Write out the circuit block parameters.
c
      write(9,14)
      write(9,42)
      write(9,43) ht, res_time, tmax, nprint, nb
      do ibranch = 1, nb
        write(9,11)
        nblocks = nbk(ibranch)
        do iblock = 1, nblocks
          write(9,12) ibranch, iblock, (iin(i,iblock,ibranch),i=1,5),
     &                               (pin(j,iblock,ibranch),j=1,8)
        end do
      end do
c
   14 format (/'      Set-up Parameters For Screamer Code'/)
   42 format (10x, 'HT', 10x, 'Tres', 9x, 'Tmax', 9x, 'Nprint',
     &        2x, 'NB')
   43 format (5x, 3(e10.3,1x), 2(i5,1x))
   11 format (//' BR BL BT  IV  IB  I C IO', 6x, 'P1', 10x, 'P2', 10x,
     &        'P3', 10x, 'P4', 10x, 'P5', 10x,
     &          'P6', 10x, 'P7', 10x, 'P8')
   12 format (7(i3,2x), (1p8e10.3,1x))
c
c Transmission line, MITL types, and voltage source function information.
c
c      if (ntransline+nmitline+nvoltsource .gt. 0) then
c        call print_title
c      end if
c
      if (ntransline .gt. 0) then
        write(9,810)
      end if
      do i = 1, ntransline
        write(9,811) i, itrl_type(i)
      end do
c
      if (nmitline .gt. 0) then
        write(9,814)
      end if
      do i = 1, nmitline
        write(9,811) i, imitl_type(i)
      end do
c
      if (nvoltsource .gt. 0) then
        write(9,812)
      end if
      do i = 1, nvoltsource
        write(9,813) i, ivoltf(i)
        write(9,822)
        do j = 1, num_voltf_parms(i)
          write(9,823) j, voltf_parms(j,i)
        end do
      end do
c
      if (ncurrsource .gt. 0) then
        write(9,815)
      end if
      do i = 1, ncurrsource
        write(9,813) i, icurrf(i)
        write(9,822)
        do j = 1, num_currf_parms(i)
          write(9,823) j, currf_parms(j,i)
        end do
      end do
c
  810 format (/'Transmission line types'/
     &        ' ', ' I  IT')
  814 format (/'MITL types'/
     &        ' ', ' I  I_type')
  811 format (i3, 2x, i3)
  812 format (/'Voltage source function types and parameters')
  815 format (/'Current source function types and parameters')
  813 format ('I=', i3, '     IT=', i3)
c
c Variable elements.
c
      if (nvar .gt. 0) then
c        call print_title
        write(9,820)
      end if
      do i = 1, nvar
        write(9,821) i, ivar_type(i), ivar_block(i)
        write(9,822)
        do j = 1, num_var_parms(i)
          write(9,823) j, var_model(j,i)
        end do
      end do
c
  820 format (/'Variable element information')
  821 format ('Var. elem: ', i2, '   Model type: ', i2,
     &        '   Block type: ', i2)
  822 format ('   I ', '  Parameter ')
  823 format (1x, i3, 2x, 1pe10.3)
c
c Initial conditions.
c
      if (ninit_cond .gt. 0) then
c        call print_title
        write(9,827)
        write(9,828)
      end if
      do i = 1, ninit_cond
        write(9,829) i, value_init(i)
      end do
c
  827 format (/'Initial conditions information')
  828 format ('   I ', '  Initial_value')
  829 format (1x, i3, 4x, 1pe10.3)
c
c ***** Output requests.
c
c Plots:
c
c      if (numplt .gt. 0) then
c        call print_title
c        write(9,924) 'Plot requests '
c        write(9,925)
c      end if
c      do i = 1, numplt
c        write(9,926) i,
c     &             tbegplt(i), tendplt(i), yminplt(i), ymaxplt(i),
c     &             itypplt(i), iblkplt(i)
c      end do
c
c Prints:
c
c      if (numprt .gt. 0) then
c        call print_title
c        write(9,924) 'print requests'
c        write(9,927)
c      end if
c      do i = 1, numprt
c        write(9,928) i,
c     &             tbegprt(i), tendprt(i),
c     &             itypprt(i), iblkprt(i)
c      end do
c
c Files:
c
c      if (numfil .gt. 0) then
c        call print_title
c        write(9,924) 'File requests '
c        write(9,927)
c      end if
c      do i = 1, numfil
c        write(9,928) i,
c     &             tbegfil(i), tendfil(i),
c     &             itypfil(i), iblkfil(i)
c      end do
c
c Tables:
c
c      if (numtab .gt. 0) then
c        call print_title
c        write(9,924) 'Table requests'
c        write(9,927)
c      end if
c      do i = 1, numtab
c        write(9,928) i,
c     &             tbegtab(i), tendtab(i),
c     &             ityptab(i), iblktab(i)
c      end do
c
c UFO:
c
c      if (numufo .gt. 0) then
c        call print_title
c        write(9,924) 'UFO requests '
c        write(9,927)
c      end if
c      do i = 1, numufo
c        write(9,928) i,
c     &             tbegufo, tendufo,
c     &             itypufo(i), iblkufo(i)
c      end do
c
c  924 format (/a14)
c  925 format (' I', 2x, 'Tstart', 6x, 'Tstop',
c     &        6x, 'Ymin', 7x, 'Ymax', 5x, 'Typ', 1x, 'Blk')
c  927 format (' I', 2x, 'Tstart', 6x, 'Tstop',
c     &              4x, 'Typ', 1x, 'Blk')
c  926 format (i2, 4(1x,1pe10.3), 2(1x,i3))
c  928 format (i2, 2(1x,1pe10.3), 2(1x,i3))
c
      return
      end
