      function fleastsquares (time, num_coeff, coeff)
c
c  Change Log
c
c  2014-02-06 RBS: Changed real*4 to real
c  2014-05-02 RBS: Changed integer*4 to integer
c  2014-05-07 RBS: Explicitly defined sum as real
c
c Define passed variables
c
      integer   num_coeff
      real      time, coeff(num_coeff)
c
c Define internal variables
c
      real sum
c
c Calulates the function using nested multiplication:
c
c                                    2          3
c    f(time) = a0 + a1*time + a2*time  + a3*time  + ...
c
c       where:  ai is coeff(i+1)
c         and:  num_coeff is from 1 to 10 (a0 to a9)
c
      sum = coeff(num_coeff)
      do i = num_coeff-1, 1, -1
        sum = coeff(i) + time*sum
      end do
c
      fleastsquares = sum
c
      return
      end
