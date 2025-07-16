      SUBROUTINE bandPRL3diag_MN_BRANCH(a,nrx)

!$    use omp_lib

C This SUBROUTINE bandPRL3diag_BRANCH executes the forward step in the
C Gaussian elimination process (starting from the last row) for a general
C three diagonal matrix ( with possible connection terms in the lower
C triangular part which are left unchanged) stored in the Screamer Format.
C The diagonal terms can be zero, so at every node two equations are
C switched. The second equation of the last node has only one
C nonzero diagonal element. The equations corresponding to the first and
C last nodes are not switched (with assumed nonzero diagonal terms).
C This solver is designed as a sequential solver, so it can be used in the
C algorithms with parallelization at the upper level. The diagonal terms
C in the resulting lower triangular matrix are not 1s!!!!
C=======================================================================
C input:    a      - the coefficient matrix in the Screamer format
C           nrx    - the total number of nodes in the branch.
C
C output:   a      - the coefficient matrix in the Screamer format
C                    that includes the resulting lower triangular matrix
C                    with diagonal elements are not 1s (echelon form).
C=======================================================================
C
C Written by Dr. Yury Gryazin, 07/01/2019, ISU, Pocatello, ID
C
C
c     Define passed variables
c

      REAL*8  a(*)
      INTEGER nrx
c
c Define internal variables
c
      INTEGER k,nrow,ny,ny1,nr2,i,nrm
      REAL*8  coef, a02, a03, a07
      REAL*8 b03, b04,b07, b13, b14, b17, d1, d2
c
c It is assumed that all connection terms (in the upper triangular part)
c were already eliminated. The parallelization in the algorithm requires
c forward step of the Gaussian elmination is implemented from the last
c row and the substitution step starts from the first row.
c
        nzz = 0
        k    = 2
        nrow = 3*k + 1

c
c Do the last node of each branch first, separately because of
c boundary conditions and the possibility of sources.
c
        nr2 = 2 * nrx

c
c Second row for the last node, voltage equation. a(ny+4) can not be
c zero (Usually it is 1). Stay the same.
c
c First row for the last node.  Current equation.  a(ny+3) may not fall
c below 1e-6.
c
            ny = (nr2-2)*nrow + nzz
            ny1 = ny + nrow

            a(ny+7) = a(ny+7) - a(ny+4)*a(ny1+7)/a(ny1+4)
            a02 = a(ny+2)
            a03 = a(ny+3)
            a07 = a(ny+7)

c
c Loop over the nodes, from the next-to-last node to node 2 using the
c voltage and current equations. The equations are switched to garantee
c the nonzero diagonal elements.
c


            nrm = nrx - 1

c
c Normalize the Current equation, second row.
c
!$OMP PARALLEL DO PRIVATE(ny1,nrow,nzz)
            DO i = nrm, 2, -1
                ny1  = nrow*i*2 - nrow + nzz
                a(ny1+2) = a(ny1+2)/a(ny1+4)
                a(ny1+3) = a(ny1+3)/a(ny1+4)
                a(ny1+7) = a(ny1+7)/a(ny1+4)
                a(ny1+4) = 1.0
            ENDDO
!$OMP END PARALLEL DO

            DO i = nrm, 2, -1
c
c Voltage equation, first row.  a(ny+3) is the diagonal element and is
c never zero.
c
                ny  = nrow*(i-1)*2 + nzz
                ny1 = ny + nrow
                coef = a(ny+5)/a03

                a(ny+4) = a(ny+4) - coef*a02
                a(ny+7) = a(ny+7) - coef*a07

                a(ny+2) = - a(ny1+2)*a(ny+4)
                a(ny+3) = a(ny+3) - a(ny1+3)*a(ny+4)
                a(ny+7) = a(ny+7) - a(ny1+7)*a(ny+4)

                a02 = a(ny+2)
                a03 = a(ny+3)
                a07 = a(ny+7)

            ENDDO

!$OMP PARALLEL DO PRIVATE(ny,ny1,nrow,nzz,coef)
            DO i = nrm, 2, -1
                ny  = nrow*(i-1)*2 + nzz
                ny1 = ny + nrow
                coef = a(ny1+3)/a(ny+3)

                a(ny1+2) = a(ny1+2)- coef*a(ny+2)
                a(ny1+7) = a(ny1+7)- coef*a(ny+7)
                a(ny1+3) = 0.0
            ENDDO
!$OMP END PARALLEL DO

c
c First node.
c First row.  Current equation.  a(ny+3) is the diagonal element and
c should not be zero.
c Second row (voltage equation) for first node. a(ny1+4) is the diagonal
c element and may not fall below 1e-6.
c
        ny  = nzz
        ny1 = ny + nrow

        coef = a(ny1 + 5)/a03

        b13 = a(ny1 + 3)

        b14 = a(ny1 + 4)
        b14 = b14 - a02*coef

        b17 = a(ny1 + 7)
        b17 = b17 - a07*coef

    
        b03 = a(ny + 3)
        b04 = a(ny + 4)
        b07 = a(ny + 7)

        d1 = 1/sqrt(b03*b03 + b04*b04)
        d2 = 1/sqrt(b13*b13 + b14*b14)

        b03 = b03*d1
        b04 = b04*d1
        b07 = b07*d1

        b13 = b13*d2
        b14 = b14*d2
        b17 = b17*d2



        coef = b04/b14

        b03 = b03 - b13*coef

        b07 = b07 - b17*coef
        b07 = b07/b03

        b17 = b17 - b13*b07
        b17 = b17/b14


        a(ny + 7)= b07
        a(ny1 +7)= b17
c
c In the first row the diagonal term is not 1. The goal is to leave
c the connection terms in the lower triangular matrix as 1s.
c


      RETURN
      END



