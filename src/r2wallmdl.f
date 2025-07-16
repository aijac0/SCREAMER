      Subroutine r2wall (time, curr, parms, rvar)
c
c     Stygar Resistive Wall Model
c
c Author/Date: Rick Spielman 2012-03-08
c     Modifications:
c
c 2012-04-02 RBS: Changed the name of the subroutine to r2wall to make
c                 it easier to use a shortened name in the input deck
c 2013-12-07 RBS: Changed the subroutine to arbitrary upper and lower
c                 disks and separate cylindrical elements
c 2014-05-07 RBS: All internal reals declared explicitly
c
c     Version 1 - one disk (2 identical elements, i.e. X2 below) and
c                 one coax (two identical elements)
c
c     Version 2 – one disk (2 separate elements) and one coax (two
c                 separate cylindrical elements)
c
c     This routine estimates the resistive losses in the wall at very high
c     current densities.  This subroutine is based on the Phys. Rev. ST
c     Accel Beams 11, 120401 (2008).
c     In Eq. 35, the resistance R for a transmission line is described as
c       
c     Reff(t)[Ohms] = 2piCHI1(1/t^.5)[SUMi(li/ai) + SUMj(ln(cj/bj)] +
c                     2piCHI2(t^.25)I^2[SUMi(li/ai^3) + .5SUMj(1/bj^2
c                     - 1/cj^2)]
c
c     where CHI1 = 3.36E-08, CHI2 = 3.7548E-22, li - length of a cylindrical
c     line, ai - radius of a cylindrical line, bj - inner disk radius,
c     cj - outer disk radius of each disk segment.
c     The values of CHI are for Z-like SS conductor geometries.
c     The resistance can be made up of the sum of the disk and cylindrical
c     pieces. This is EACH side of the elements.
c
c Define passed variables
c
      real time, curr, parms(*), rvar
c
c Define internal variables
c
      real       CHI1,          CHI2,            pi
      parameter (CHI1=3.36E-08, CHI2=3.7548E-22, pi=3.14159)
c
      real t_in, cyl_lin, cyl_lo, cyl_rin, cyl_ro,
     &     d_urin, d_urout, d_lrin, d_lrout,
     &     t, t1, t2
c
      t_in     = parms(1)
      cyl_lin  = parms(2)
      cyl_lo   = parms(3)
      cyl_rin  = parms(4)
      cyl_ro   = parms(5)
      d_urin   = parms(6)
      d_urout  = parms(7)
      d_lrin   = parms(8)
      d_lrout  = parms(9)

c
c     Calculate the resistance
c
c
      if(time .le. t_in) then
         rvar=0.0
        else
         t = time-t_in+1e-10
         t1 = 1.0/t**.5
         t2 = t**.25
c t is adusted from zero to prevent problems with divide by zero.
c Initial tests use a constant resistance but all else is included
c
         rvar = 2*pi*(CHI1*t1*
     &       (cyl_lin/cyl_rin + cyl_lo/cyl_ro +
     &       log(d_urout/d_urin) + log(d_lrout/d_lrin)) +
     &       CHI2*t2*curr**2*(cyl_lin/cyl_rin**3 + cyl_lo/cyl_ro**3 +
     &       .5*(1/d_urin**2 - 1/d_urout**2 +
     &       1/d_lrin**2 - 1/d_lrout**2)))

        endif
      return
      end