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


      INTEGER k, nrow,nr2,nrm,ny,ny1,ny1M,i
c
c Loop from the first node to the last
c
        k    = 2
        nrow = 3*k + 1
        nr2 = 2 * nrx
        nrm = nrx - 1

C
C  Find solution for the first two equations.
C
        ny  = 0
        ny1 = nrow

c        a(ny  + 6) = a(ny  + 7)/a(ny + 3)
c        a(ny1 + 6) = ( a(ny1 + 7) - a(ny1 + 3)*a(ny  + 6))/a(ny1 + 4)

        a(ny  + 6) = a(ny  + 7)
        a(ny1 + 6) = a(ny1 + 7)

c
c Loop over the nodes from 2 to nrm.
c

        DO i = 2, nrm

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

        a(ny  + 6)  =  (a(ny  + 7) - a(ny +2)*a(nyM1 + 6))/a(ny +3)
        a(ny1 + 6)  =  a(ny1 + 7)/a(ny1 + 4)

      RETURN
      END



