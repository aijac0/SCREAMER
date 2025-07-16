      SUBROUTINE bandPRL3diag(a,nzz,nrx)

!$    use omp_lib

C This SUBROUTINE bandPRL3diag executes the forward step in the
C Gaussian elimination process (starting from the last row) for a
C three diagonal matrix ( with possible connection terms in the lower
c triangular part which are left unchanged) stored in the Screamer
c Format. The diagonal terms can be zero, so at every node two equations
c are switched. The second equation of the last node has only one
C nonzero diagonal element. The equations corresponding to the first and
C last nodes are not switched (with assumed nonzero diagonal terms).
C=======================================================================
C input:    a      - the coefficient matrix in the Screamer format
C           nzz    - the number of rows in the matrix prior to the row
C                    corresponding to the first node of the branch.
C           nrx    - the total number of nodes in the branch.
C
C output:   a      - the coefficient matrix in the Screamer format
C                    that includes the resulting lower triangular matrix
C                    with diagonal elements all 1s(reduced echelon form)
C=======================================================================
C
C Written by Dr. Yury Gryazin, 07/01/2019, ISU, Pocatello, ID
C
C
c     Define passed variables
c

      REAL*8  a(*)
      INTEGER nzz, nrx
c
c Define internal variables
c
      INTEGER k, nrow, ny, nr2, i, nrm
      REAL*8  coef, a02, a03, a07, a17
c
c It is assumed that all connection terms (in the upper triangular part)
c were already eliminated. The parallelization in the algorithm requires
c forward step of the Gaussian elmination is implemented from the last
c row and the substitution step starts from the first row.
c

        k    = 2
        nrow = 3*k + 1

c
c Do the last node of each branch first, separately because of
c boundary conditions and the possibility of sources.
c
        nr2 = 2 * nrx

c
c Second row for the last node, voltage equation. a(ny+4) can not be
c zero (Usually it is 1).
c
            ny = (nr2-1)*nrow + nzz

            a(ny+7) = a(ny+7)/a(ny+4)
            a17 = a(ny+7)
c
c First row for the last node.  Current equation.  a(ny+3) may not fall
c below 1e-6.
c
            ny = (nr2-2)*nrow + nzz

            a(ny+7) = a(ny+7) - a(ny+4)*a17
            a02 = a(ny+2)
            a03 = a(ny+3)
            a07 = a(ny+7)

c
c Loop over the nodes, from the next-to-last node to node 2 using the
c voltage and current equations. The equations are switched to garantee
c the nonzero diagonal elements.
c

c
c First, we start with the easily parallelizable step of normalizing the
c second equation at every node (Making diagonal term 1).
c Current equation, second row.  a(ny+4) is the diagonal element
c and is never zero.
c

            nrm = nrx - 1
            DO i = nrm, 2, -1
                ny  = nrow*((i-1)*2 + 1) + nzz

                a(ny+3) = a(ny+3)/a(ny+4)
                a(ny+2) = a(ny+2)/a(ny+4)

                a(ny+7) = a(ny+7)/a(ny+4)

            ENDDO



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

                a(ny+2) = -a(ny1+2)*a(ny+4)
                a(ny+3) = a(ny+3) - a(ny1+3)*a(ny+4)
                a(ny+7) = a(ny+7) - a(ny1+7)*a(ny+4)

                a02 = a(ny+2)
                a03 = a(ny+3)
                a07 = a(ny+7)

            ENDDO
c
c Parallel step. Scaling of the first equation and eliminate the
c a(ny + 3) term in the second equation.
c
            ny  = nrow*(nrx-1)*2 + nzz

            a(ny+2) = a(ny+2)/a(ny+3)
            a(ny+7) = a(ny+7)/a(ny+3)

            DO i = nrm, 2, -1
                ny  = nrow*(i-1)*2 + nzz
                ny1 = ny + nrow

                a(ny+2) = a(ny+2)/a(ny+3)
                a(ny+7) = a(ny+7)/a(ny+3)

                a(ny1+2) = a(ny1+2) - a(ny+2)*a(ny1+3)
                a(ny1+7) = a(ny1+7) - a(ny+7)*a(ny1+3)

            ENDDO

            a02 = a(ny+2)
            a07 = a(ny+7)


c
c First node.
c First row.  Current equation.  a(ny+3) is the diagonal element and
c should not be zero.
c Second row (voltage equation) for first node. a(ny1+4) is the diagonal
c element and may not fall below 1e-6.
c
        ny  = nzz
        ny1 = ny + nrow

        coef = a(ny1 + 4) - a02*a(ny1 + 5)
        a(ny1 + 7) = a(ny1 + 7) - a07*a(ny1 + 5)

        a(ny1 + 3) = a(ny1 + 3)/coef
        a(ny1 + 7) = a(ny1 + 7)/coef

        a(ny + 3) = a(ny + 3) - a(ny1 + 3)*a(ny + 4)
        a(ny + 7) = a(ny + 7) - a(ny1 + 7)*a(ny + 4)

c
c In the first row the diagonal term is not 1. The goal is to leave
c the connection terms in the lower triangular matrix as 1s.
c


      RETURN
      END



