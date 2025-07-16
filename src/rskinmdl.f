      Subroutine rskin
     &          (ht, time, curr, parms, j_max, dcurr, H, rvar)
c
c     Cold Skin Depth Resistive Wall Model
c
c Author/Date: Rick Spielman 2017-01-02
c     Modifications: Yury Gryazin 2017-03-29, Included explicit
c                    finite difference calculation of H
c
c
c This routine calculates the average skin depth as a function of
c  time using the solution to the magnetic diffusion equation.
c This subroutine is based on many published solutions.
c
c
c     we assume µ is set to µ0
c
c ***************************************************************
c Declare acceleration variables. Y.Gryazin 03/29/2017

      real*8   aa, crnt, HN, dt, coef, dd, HL, HM, HR, tm, tswitch, htt
      integer  nc, ic


c ***************************************************************
c
c Declare passed variables
c
      real ht, time, curr, parms(*), dcurr(*), H(*), rvar
      integer j_max
c
c Declare internal variables
c
      integer itime

c
      real       pi
      real*8     dpi
      real*8     zmu0
c
      real*8 sigma, depth, cyl_len, cyl_rout, cyl_rin,
     &       disk_len, disk_rout, iold

      real*8 deltai, H_ave, skin_depth

c
c Values in the parms array are placed into internal variables
c
      sigma     = dble(parms(1))
      depth     = dble(parms(2))
      cyl_len   = dble(parms(3))
      cyl_rout  = dble(parms(4))
      cyl_rin   = dble(parms(5))
      disk_len  = dble(parms(6))
      disk_rout = dble(parms(7))

      iold      = dble(parms(10))
c
c Define pi real*4 and real*8
c
      pi = 4.0 * atan(1.0)
      dpi = 4.0d0 * datan(1.0d0)
c
c Define material µ
c zmu0 and dpi are double precision
c
      zmu0 = 4.0d0 * dpi * 1.0d-7
c
c redefine depth as the depth increment
c depth is double precision
c
      depth = depth / dble(j_max-1)
c
c Calculate the delta I for this time step
c curr is single precision, deltai and iold are double precision
c
      deltai = ( dble(curr) - iold )
c
c Calculate the time index
c time and ht are single precision
c
      itime = int( time / ht )
c
c Place the delta I in the correct location in the delta I vector
c dcurr is single precision and deltai is double precision
c
      dcurr(itime) = sngl( deltai )


c   *********************
c Solution of the diffusion equation on the next time step by
c the explicit scheme first order approximation in time and
c second order (central difference) approximation in space.
c

        htt = dble(ht)
        if(itime .eq. 1) htt = dble(time) - dble(itime) * dble(ht)


        aa = 1.0d0 / sigma / zmu0
        crnt = depth * depth / 2.0d0 / aa

        nc = ceiling( htt/crnt )
        HN = 0.0d0

        if( itime .gt. 0 ) then
            HN = dble( H(1) ) +
     &           dble( dcurr(itime) )/(2.0d0 * dpi * cyl_rin)
        endif

        n = 20
        if ( crnt .lt. htt ) then
            n = max(n,nc)
        endif

        dt = htt / dble(n)
        coef = aa * dt / depth / depth
        dd = 1.0d0 - 2.0d0 * coef
        tswitch = dble(itime) * dble(ht)

        if( itime .gt. 0 ) then

            do ic  = 1 , n

                HL = dble( H(1) )
                HM = dble( H(2) )
                HR = dble( H(3) )
                tm = dt * ic + dble(time) - htt

                if( tm .gt. tswitch ) HL = HN

                do j = 2, j_max-2
                    H(j) = sngl( dd * HM + coef * (HL + HR) )
                    HL = HM
                    HM = HR
                    HR = dble( H(j+2) )
                enddo
                H(j_max-1) = sngl( dd * HM + coef * (HL + HR) )
            enddo

        endif

        HR = 0.0d0
        do i = 1, itime
            HR =  HR +
     &            ( dble(dcurr(i)) / (2.0d0 * dpi * cyl_rin) ) *
     &            (1.0d0 - derf( (dble(j_max-1) * depth / 2.0d0 ) *
     &            dsqrt( sigma * zmu0 /
     &            ( dble(time) - (dble(i) * dble(ht) ) ) ) ) )
        enddo

        H(1)     = sngl( HN )
        H(j_max) = sngl( HR )

c        print '( "time = " (e10.3) "  ht = " (e10.3))', time, ht
c        print '( "itime*ht = " (e10.3))', (itime)*ht

c        print '(11e10.3)', H(1), H(2), H(3), H(4), H(5), H(6),
c     &                     H(7), H(8), H(9), H(10), H(11)

c *********************************************

c
c Calculate H as a function of position for all values of deltai up to
c  itime
c First zero the array
c The array H is then filled up with the values of magnetic field for
c  each depth j for all past current impulses itime-1.
c
c        do j = 1, j_max
c            H(j) = 0.0
c        end do
c
c        do j = 1, j_max
c
c        do i = 1, itime
c            H(j) = H(j) + ( dcurr(i) / (2.0 * pi * cyl_rin) ) *
c     &           (1.0 - erf( (real(j-1) * depth / 2.0 ) *
c     &           sqrt( sigma * zmu0 / ( time - real(i) * ht ) ) ) )
c        enddo
c
c        enddo
c
c        print '(11e10.3)', H(1), H(2), H(3), H(4), H(5), H(6),
c     &                   H(7), H(8), H(9), H(10), H(11)
cc      print '(10e10.3)', H(12), H(13), H(14), H(15), H(16), H(17),
cc     &                   H(18), H(19), H(20), H(21)

c        write(*,'(''paused, type [enter] to continue'')')
c        read (*,*)


c
c Calculate average H
c
      H_ave = 0.0d0

      H_ave = 0.5d0 * H(1)

      do j = 2, j_max-1
        H_ave = H_ave + dble( H(j) )
      enddo

      H_ave = H_ave + 0.5d0 * dble( H(j_max) )

      H_ave = H_ave / dble(j_max)

c      print '(A,1pE10.3)', ' H_ave= ', H_ave
c
c Calculate the skin depth where H = H_ave
c We assume that H monotonically decreases into the metal
c Don't count j=1 in the depth real(j-1) but add back one since we
c overshot the value by testing from the lowest to the highest.
c
      do j = j_max, 1, -1

       if ( H(j) .ge. H_ave ) then
         exit
       endif

      enddo

      if ( H_ave .eq. 0.0d0 ) j = 2

      skin_depth = dble(j) * depth

c      print '(A,I4,A,1pE10.3)', 'j=',j, ' skin depth= ', skin_depth
c
c Calculate the resistance
c Assume that the radius is >>>> skin_depth
c
c First calculate the coaxial terms
c
      if ( sigma .le. 1.0d0 ) sigma = 1.0d0

      if ( cyl_len .gt. 0.0d0 ) then
        rvar = ( 1.0 / sngl(sigma) ) * sngl(cyl_len) /
     &         ( pi * 2.0 * sngl(cyl_rin) * sngl(skin_depth) )
        rvar = rvar + ( ( 1.0 / sngl(sigma) ) * sngl(cyl_len) /
     &         ( pi * 2.0 * sngl(cyl_rout) * sngl(skin_depth) ) )
      endif
c
c Then calculate the disk terms
c
      if ( disk_len .gt. 0.0 ) then
        rvar = ( 1.0 / sngl(sigma) ) * sngl(disk_len) /
     &         ( pi * 2.0 * sngl(disk_rout) * sngl(skin_depth) )
        rvar = rvar * 2.0
      endif
c
c
c Place the present value of current into parms(10) for this element
c  to be passed back to model.f.
c
      parms(10) = curr

      return
      end
