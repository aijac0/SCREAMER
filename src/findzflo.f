      Subroutine FindZflow (time, ianode, V, Z,icathode,iplasma,zflow)
c
c     KWS  10/31/95.  Calculates the cathode current and zflow using the 
c     Cliff Mendel pressure balance equation
c
c Modifications:
c
c 2015-03-30 RBS: Double precision math fixed at many locations, still
c                 instances of mixed precision.
c 2015-03-30 RBS: Dummy call to time because time is a passed variable
c                 that is only used in the diagnostic write, which is
c                 commented out at this time.
c
c-----------------------------------------------------------------------
      use zdemmax
      use zdemout

c Define passed variables
c
      real time, ianode, V, Z, icathode, iplasma, zflow
c
c Define internal variables
c
      complex root1, root2, root3
      double precision a, a1, a2, a3, d
      real r(3), root, cur2diff, Z2, Ve, Ve2, zflowlim
c
c
c
      parameter (Ve = 2.5549953e5)
c     Ve = mc^2/2e
      parameter (Ve2 = 6.5280010e10)
      parameter (zflowlim = 1.0e3)
c
c Dummy call to time to remove a compiler warning
c
      zflowtime = time
c
c     Set up coefficients
c
      a = ianode * ianode
      Z2 = Z*Z
      a1 = (V*V - 2.0*V*Ve + Ve2 - a*Z2)/Z2
      a2 = 2.0*Ve*a*(V - Ve)/Z2
      a3 = (Ve2*a*a)/Z2
c
c
      if((a .gt. 1.) .and. (v .gt. 1.)) then
        call CubicRoots(a1,a2,a3,d,root1,root2,root3)
c
c
c     Find the right root.  Note that the first root is
c     the real root for d>0 and d=0
c
        r(1) = real(root1)
        r(2) = real(root2)
        r(3) = real(root3)
        root = 0.0

        if ( d .ge. 0.0 ) then
          if ( r(1) .ge. 0.0 ) then
            root = r(1)
          else
            root = real(a)
          endif
        else
          do i = 1,3
            if ( r(i) .ge. 0.0 ) then
              if(( r(i) .le. a ) .and. ( r(i) .gt. root )) root = r(i)
            endif
          end do
          if ( root .eq. 0.0 ) root = real(a)
        endif
c
c     Find the plasma current and zflow
c
        icathode = sqrt(root)
        iplasma = abs(ianode) - icathode
        cur2diff = real(a) - icathode*icathode
        if ( cur2diff .gt. 1.0 ) then
           zflow = V / sqrt(cur2diff)
        else
           zflow = zflowlim
        endif
      else
        icathode=ianode
        iplasma=0.0
        zflow=zflowlim
        a1=0.
        a2=0.
        a3=0.
        d=0.
        root1=0.
        root2=0.
        root3=0.
      endif
c
c
c      write(9,600) time,ianode,icathode,iplasma,zflow,v,z,a1,a2,a3,d,
c     &             root1,root2,root3
c 600  format(6x,'t',11x,'Ia',10x,'Ic',10x,'Ip',10x,'zf',10x,'V'/
c     &       6e12.4/
c     &       18x,'Z',11x,'a1',10x,'a2',10x,'a3',10x,'d'/
c     &       12x,5e12.4/
c     &       16x,'root1',19x,'root2',19x,'root3'/
c     &       12x,6e12.4//)
      return
      end
c
c
c
c
c
c     **************************************************
      Subroutine CubicRoots (da1,da2,da3,d,z1,z2,z3)
c     **************************************************
c
c                         Ken Struve
c                         Nov. 8, 1995
c Modifications:
c
c 2016-03-16 RBS: Parameter pi not used - removed
c
c-----------------------------------------------------------------------
c
c     This routine calculates the roots of the cubic equation
c
c        x**3 + a1*x**2 + a2*x + a3 = 0.0
c
c     where a1, a2, and a3 are real.
c     As a check, the roots should obey the following:
c
c        z1 + z2 + z3 = -a1
c
c        z1*z2 + z2*z3 + z3*z1 = a2
c
c        z1 * z2 * z3 = -a3
c
c     where z1, z2, and z3 are the three roots.
c
c
c Define passed variables
c
      double precision da1, da2, da3, d
      complex          z1, z2, z3
c
c Define internal variables
c
      double precision q, r, s, t, s3, t3
      double precision da13, z, theta, dpi
c      double precision check1, check2, check3

      parameter (dpi = 3.1415926535D0)
c
c     Calculate the intermediate values
c
      q = (3.0*da2 - da1*da1)/9.0
      r = (9.0*da1*da2 - 27.0*da3 -2.0*da1*da1*da1)/54.0
      d = q*q*q + r*r
      da13 = da1/3.0
c
c     Find the roots for the three cases
c
c     *******************************************
c     Case 1 and Case 2:  d greater or equal to 0
c     *******************************************
c
      if (d .ge. 0.0) then
         s3 = r + dsqrt(d)
         if (s3 .ge. 0.0) then
            s = s3**(1.0/3.0)
         else
            s = -1.0*(-1.0*s3)**(1.0/3.0)
         endif
         t3 = r - dsqrt(d)
         if (t3 .ge. 0.0) then
            t = t3**(1.0/3.0)
         else
            t = -1.0*(-1.0*t3)**(1.0/3.0)
         endif
         z1 = real(s + t - da13)
         z2 = real(-0.5D0 * (s + t) - da13) +
     &             0.5 * (0.0,1.0) * sqrt(3.0) * real(s - t)
         z3 = conjg(z2)
c
c     *******************************************
c     Case 3:  d less than 0
c     *******************************************
c
      else
         theta = dacos(r/dsqrt(-1.0D0*q*q*q))
         z = 2.0D0 * dsqrt(-q)
         z1 = real(z * dcos(theta/3.0D0) - da13)
         z2 = real(z * dcos((theta + 2.0D0*dpi)/3.0D0) - da13)
         z3 = real(z * dcos((theta - 2.0D0*dpi)/3.0D0) - da13)
      endif
c
c     *******************************************
c     Error Check
c     *******************************************
c
c      check1 = -1.0 * (z1 + z2 +z3)
c      check2 = z1*z2 + z2*z3 + z3*z1
c      check3 = -1.0 * z1 * z2 * z3
c      write(9,*)
c      write(9,*) 'Check1 = ',check1,'   It should be ', da1
c      write(9,*) 'Check2 = ',check2,'   It should be ', da2
c      write(9,*) 'Check3 = ',check3,'   It should be ', da3
c
      return
      end
