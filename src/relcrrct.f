      function hrelf (u)
c
c  Change log
c 2014-02-06 RBS: Changed real*4 to real
c 2015-06-22 RBS: Error in real declaration, typo, missing comma between
c                 b6 and b7. Fixed.
c
c Function from LXS and DHM SCEPTRE model of perveance specified MITLs.
c Relativistic correction to electron motion.
c
c
c Define passed variables
c
      real     u
c
c Define internal variables
c
      real a, s, scon
      real a0, a1, a2, a3, a4, a5, a6, a7,
     &     b0, b1, b2, b3, b4, b5, b6, b7, b8,
     &     c1, c2, uswitch
c
      parameter (a0 = 9.99992673e-1)
      parameter (a1 = -1.07000372e-1)
      parameter (a2 = 2.35920406e-2)
      parameter (a3 = -5.98692605e-3)
      parameter (a4 = 1.33650594e-3)
      parameter (a5 = -2.1510132e-4)
      parameter (a6 = 2.08370041e-5)
      parameter (a7 = -8.91792265e-7)
      parameter (b0 = 1.0)
      parameter (b1 = -1.0 / 3.0)
      parameter (b2 = 1.0 / 3.0)
      parameter (b3 = -4.28571429e-1)
      parameter (b4 = 6.19047619e-1)
      parameter (b5 = -9.56709957e-1)
      parameter (b6 = 1.54545455)
      parameter (b7 = -2.57575758)
      parameter (b8 = 4.39393939)
      parameter (c1 = -0.847213)
      parameter (c2 = 3.181980515)
      parameter (uswitch = 5.0)
c
      if (u .le. uswitch) then
        hrelf = a0 + u *
     &         (a1 + u *
     &         (a2 + u *
     &         (a3 + u *
     &         (a4 + u *
     &         (a5 + u *
     &         (a6 + u * a7))))))
c
      else
        a = 1.0 / u
        s = u + b0 + a *
     &         (b1 + a *
     &         (b2 + a *
     &         (b3 + a *
     &         (b4 + a *
     &         (b5 + a *
     &         (b6 + a *
     &         (b7 + a * b8)))))))
        scon  = sqrt (u*u + u + u)
        s     = s / sqrt (scon)  +  c1
        hrelf = c2 * s * s / (u * sqrt(u))
c
      end if
c
      return
      end
