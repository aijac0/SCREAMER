      subroutine cycle_print (icycle, time, esour, eind, ecap,
     &                        econ, eres, elossind, elosscap, error )
c
c print out current status of the solution.
c
c --------------------------------------------------------
c Modifications:
c  1995-06-27 MLK: Only print out energy balance by default
c                  Print out circuit details if requested
c  2014-02-06 RBS: Changed real*4 to real
c  2020-11-04 RBS: Changed the Cycle format to i7
c  2020-22-04 RBS: Changed the output energy formats to 1pe12.4
c --------------------------------------------------------
c
c
c Include files
c
      use zdemmax
      include 'zdemcomm.h'
      include 'zdemwork.h'
      include 'zdemparm.h'
c
c Define passed variables
c
      integer    icycle
      real       time, esour, eind, ecap, econ, eres,
     &           elossind, elosscap, error
c
c Define internal variables
c
c     NONE
c
c Call print_title
c
      write(9,26) time, icycle
      write(9,1) esour
      write(9,2) eind
      write(9,3) ecap
      write(9,4) econ
      write(9,5) eres
      write(9,6) elossind
      write(9,7) elosscap
      write(9,8) error
c
c Print out circuit details if detail-prints level is set appropriately
c
      if (detail_prints .eq. detail_prints_full) then
        do ib = 1, nb
          write(9,74) ib
          write(9,73)
          nrx = nr(ib)
          do i = 1, nrx
            halfstepv = 0.5 * (  vn(i,ib) +   v(i,ib))
            halfstepi = 0.5 * (zirn(i,ib) + zir(i,ib))
            write(9,20) i, iflg(i,ib), halfstepv, halfstepi, g(i,ib),
     &                cechk(i,ib), zlrechk(i,ib), rr(i,ib),
     &                cdot(i,ib), zlrdot(i,ib),
     &                c(i,ib), zlr(i,ib)
          end do
        end do
      end if
c
   26 format(/'Time = ', 1pe10.3, 5x, 'Cycle = ', i7)
    1 format ('Energy from all sources:                         ',
     &         1pe12.4)
    2 format ('L*I*I/2    energy stored in inductors:           ',
     &         1pe12.4)
    3 format ('C*V*V/2    energy stored in capacitors:          ',
     &         1pe12.4)
    4 format ('G*V*V      energy dissipated in shunt resistors: ',
     &         1pe12.4)
    5 format ('R*I*I      energy dissipated in series resistors:',
     &         1pe12.4)
    6 format ('Ldot*I*I/2 energy in variable inductors:         ',
     &         1pe12.4)
    7 format ('Cdot*V*V/2 energy in variable capacitors:        ',
     &         1pe12.4)
    8 format ('Relative error in energy sum:                    ',
     &         1pe12.4)
   74 format (/'Branch ', i2)
   73 format (3x, 'I', 5x, 'Flags', 6x, 'V', 10x, 'I', 10x, 'G',
     &             10x, 'C', 10x, 'L', 10x, 'R', 8x, 'dC/dt',
     &             6x, 'dL/dt', 5x, 'd(CV)/dV', 3x,
     &             'd(LI)/dI'/)
   20 format (i4, 1x, i10, 1x, 10(1pe10.3,1x))
c
      return
      end
