      SUBROUTINE PRLBackSub_MainBRANCH_NP(a,nrx)

!$    use omp_lib

C This SUBROUTINE PRLBackSub_MainBRANCH executes the backsubsitution step
C in the Gaussian elimination process (starting from the first row)
C for a lower triangular matrix in Screamer format for the main branch.
C The diagonal terms are expect to be 1s except of first entry.
C=======================================================================
C input: a           - the coefficient matrix in the Screamer format
C        nrx         - the total number of nodes in the branch.
C
C output:a           - the solution vector in the last a(ny + 6) column.
C=======================================================================
C
C Written by Dr. Yury Gryazin, 07/20/2019, ISU, Pocatello, ID
C
C
c     Define passed variables
c

      REAL*8  a(*)
      INTEGER nrx
c
c Define internal variables
c

      REAL*16 AM(3,3), B(3), Y(3), RES(3)
      INTEGER k, nrow, ny,ny1,ny1M,nr2,nrm,n,nnn,i,j
c
c Loop from the first node to the last
c
        k    = 2
        nrow = 3*k + 1
        nr2 = 2 * nrx
        nrm = nrx - 1

        ny  = 0
        ny1 = ny  + nrow
        ny2 = ny1 + nrow
        ny3 = ny2 + nrow
C
C  Find solution for the first three equations.
C
        n = 3
! matrix A
        AM(1,1)= a(ny + 3)
        AM(1,2)= a(ny + 4)
        AM(1,3)= 0.0

        AM(2,1)= a(ny1 + 3)
        AM(2,2)= a(ny1 + 4)
        AM(2,3)= a(ny1 + 5)

        AM(3,1)= 0.0
        AM(3,2)= a(ny2 + 2)
        AM(3,3)= a(ny2 + 3)
! vector b
        B(1)= a(ny  + 7)
        B(2)= a(ny1 + 7)
        B(3)= a(ny2 + 7)


        call gauss_2(AM,B,Y,n)




        a(ny  + 6) = Y(1)
        a(ny1 + 6) = Y(2)
        a(ny2 + 6) = Y(3)
        a(ny3 + 6) = ( a(ny3 + 7) - a(ny3+ 2)*a(ny1 + 6) -
     &                 a(ny3 + 3)*a(ny2 + 6) )/a(ny3 + 4)

c
c Loop over the nodes from 2 to nrm.
c

        DO i = 3, nrm

            ny  = nrow*(i-1)*2
            ny1 = ny + nrow
            nyM1= ny - nrow

            a(ny + 6) = (a(ny + 7) - a(ny + 2)*a(nyM1 + 6))/a(ny + 3)
            a(ny1+ 6) = ( a(ny1+ 7) - a(ny1+ 2)*a(nyM1 + 6) -
     &                    a(ny1+ 3)*a(ny + 6) )/a(ny1 + 4)

        ENDDO

c
c Last node.
c

        ny  = (nr2-2)*nrow
        ny1 = ny + nrow
        nyM1= ny - nrow
        nyM2= nyM1 - nrow

        a(ny  + 6)  =  (a(ny  + 7)- a(ny + 1)*a(nyM2 + 6)
     &                   - a(ny +2)*a(nyM1 + 6))/a(ny +3)
        a(ny1 + 6)  =  a(ny1 + 7)

      RETURN
      END



