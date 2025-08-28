      subroutine echo_indicies
c
c     Echo the various indexing arrays.
c     Sort of cryptic, but useful.
c
c -----------------------------------------------------------------------
c     Modified:
c       03/07/95, MLK, fix write statements that are
c                      longer than 72 characters
c       07/19/95, MLK, redo format statements
c -----------------------------------------------------------------------
c
      use zdemmax
      use zdemwork
      use zdemcomm
      use zdemout
c
      character  indexln*80
      parameter (indexln = 'SCREAMER Setup Indicies (for debugging)')
c
c
c Write out a banner page and another page with the title.
c
      call banner (indexln)
c      call print_title
c
c Branch node indicies.
c
      if (nb .gt. 1) then
        write(9,32)
        write(9,35)
      end if
      do i = 1, nb-1
        ip = i + 1
        write(9,31) ip, (indexb(j,i),j=1,2)
      end do
c
c Variable element node indicies.
c
      if (nvar .gt. 0) then
        write(9,34)
        write(9,36)
      end if
      do i = 1, nvar
        write(9,31) i, (indexv(j,i),j=1,3)
      end do
c
c Energy source node indicies.
c
      if (nvoltsource .gt. 0) then
        write(9,833)
        write(9,834)
      end if
      do i = 1, nvoltsource
        write(9,31) i, (indexvs(j,i),j=1,2)
      end do
c
      if (ncurrsource .gt. 0) then
        write(9,837)
        write(9,838)
      end if
      do i = 1, ncurrsource
        write(9,31) i, (indexcs(j,i),j=1,2)
      end do
c
      write(9,20)
      write(9,21)
      do i = 1, nb
        write(9,31) i, ivbranch_end(i)
      end do
c
      write(9,22)
      write(9,23)
      do i = 1, nb
        write(9,31) i, icbranch_end(i), itypcend(i)
      end do
c
c MITL node indicies.
c
      if (nmitline .gt. 0) then
        write(9,835)
        write(9,836)
      end if
      do i = 1, nmitline
        write(9,31) i, (indexmitl(j,i),j=1,5)
      end do
c
   20 format (/'End-of-branch voltage source index parameters')
   22 format (/'End-of-branch current source index parameters')
   21 format (3x, 'BR     I')
   23 format (3x, 'BR     I   TYP')
   32 format (/'Branch index parameters')
   35 format (3x, 'BR     I    IB')
   34 format (/'Variable element index parameters')
   36 format (3x, 'IS     I    IV    IB')
  833 format (/'Voltage source index parameters')
  837 format (/'Current source index parameters')
  834 format (2x, 'IVS     I    IB')
  838 format (2x, 'ICS     I    IB')
  835 format (/'MITL index parameters')
  836 format (1x, 'MITL    BR    IB    N1    N2    SW')
   31 format (6(i5,1x))
c
c
c ***** Output requests.
c
c Plots:
c
c      if (numplt .gt. 0) then
c        write(9,824) 'Plot requests '
c        write(9,825)
c      end if
c      do i = 1, numplt
c        write(9,826) i, ixbrnplt(i), ixblkplt(i), ixnodplt(i),
c     &               nskipplt(i)
c      end do
c
c Prints:
c
c      if (numprt .gt. 0) then
c        write(9,824) 'print requests'
c        write(9,825)
c      end if
c      do i = 1, numprt
c        write(9,826) i, ixbrnprt(i), ixblkprt(i), ixnodprt(i),
c     &               nskipprt(i)
c      end do
c
c Files:
c
c      if (numfil .gt. 0) then
c        write(9,824) 'File requests '
c        write(9,825)
c      end if
c      do i = 1, numfil
c        write(9,826) i, ixbrnfil(i), ixblkfil(i), ixnodfil(i),
c     &               nskipfil(i)
c      end do
c
c Tables:
c
c      if (numtab .gt. 0) then
c        write(9,824) 'Table requests'
c        write(9,825)
c      end if
c      do i = 1, numtab
c        write(9,826) i, ixbrntab(i), ixblktab(i), ixnodtab(i),
c     &               nskiptab(i)
c      end do
c
c UFO:
c
c      if (numufo .gt. 0) then
c        write(9,824) 'UFO requests '
c        write(9,825)
c      end if
c      do i = 1, numufo
c        write(9,826) i, ixbrnufo(i), ixblkufo(i), ixnodufo(i),
c     &               nskipufo
c      end do
c
c  824 format (/a14)
c  825 format (' I', 2x,
c     &        'IXbrn', 2x, 'IXblk', 2x, 'IXnod', 2x, 'Nskip')
c  826 format (i2, 4(2x,i5))
c
      return
      end
