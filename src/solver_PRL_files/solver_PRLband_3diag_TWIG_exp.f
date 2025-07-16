      SUBROUTINE bandPRL3diag_TWIG_exp(a,nzz,nrx)

!$    use omp_lib

C This SUBROUTINE bandPRL3diag executes the forward step in the
C Gaussian elimination process (starting from the last row) for a
C three diagonal matrix and -.5 and .5 on at the subdiagonal
C ( with possible connection terms in the lower triangular part which
C are left unchanged) stored in the Screamer Format. The diagonal terms
C can be zero, so at every node two equations are switched.
C The second equation of the last node has only one
C nonzero diagonal element. The equations corresponding to the first and
C last nodes are not switched (with assumed nonzero diagonal terms).
C This solver is designed as a sequential solver, so it can be used in the
C algorithms with parallelization at the upper level. The diagonal terms
C in the resulting lower triangular matrix are not 1s!!!!
C=======================================================================
C input:    a      - the coefficient matrix in the Screamer format
C           nzz    - the number of rows in the matri prior to the row
C                    corresponding to the first node of the branch.
C           nrx    - the total number of nodes in the branch.
C
C output:   a      - the coefficient matrix in the Screamer format
C                    that includes the resulting lower triangular matrix
C                    with diagonal elements are not 1s(echelon form).
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
      INTEGER k,nrow,nr2,ny,ny1,ny2,i,nrm
      REAL*8  coef, a02, a03, a07, amx1, amx2, amx3

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
c zero (Usually it is 1). Stay the same.
c
c First row for the last node.  Current equation.  a(ny+3) may not fall
c below 1e-6.
c
            ny = (nr2-2)*nrow + nzz
            ny1 = (nr2-1)*nrow + nzz

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

                coef = a(ny+4)/a(ny1+4)

                a(ny+2) = -a(ny1+2)*coef
                a(ny+3) = a(ny+3) - a(ny1+3)*coef
                a(ny+7) = a(ny+7) - a(ny1+7)*coef

                a02 = a(ny+2)
                a03 = a(ny+3)
                a07 = a(ny+7)

            ENDDO

c
c First node.
c First row.  Current equation.  a(ny+3) is the diagonal element and
c should not be zero.
c Second row (voltage equation) for first node. a(ny1+4) is the diagonal
c element and may not fall below 1e-6.
c
        ny  = nzz
        ny1 = ny + nrow
        ny2 = ny1 + nrow

        a(ny + 5)=0

        amx1 = max ( dabs( a(ny + 3) ),dabs( a(ny + 4) ) )
        amx2 = max ( dabs( a(ny1 +3) ),dabs( a(ny1 +4) ) )
        coef = max ( amx2,dabs( a(ny1 + 5)) )
        amx2 = coef

        if (dabs(a(ny1 + 3))/amx2.gt.dabs(a(ny + 3))/amx1) then
            coef = a(ny1 + 3)
                a(ny1 + 3) = a(ny + 3)
                a(ny + 3)  = coef
            coef = a(ny1 + 4)
                a(ny1 + 4) = a(ny + 4)
                a(ny + 4)  = coef
            coef = a(ny1 + 5)
                a(ny1 + 5) = a(ny + 5)
                a(ny + 5)  = coef
            coef = a(ny1 + 7)
                a(ny1 + 7) = a(ny + 7)
                a(ny + 7)  = coef
        endif


        coef = a(ny1 + 3)/a(ny + 3)

        a(ny1 + 4) = a(ny1 + 4) - a(ny + 4)*coef
        a(ny1 + 5) = a(ny1 + 5) - a(ny + 5)*coef
        a(ny1 + 7) = a(ny1 + 7) - a(ny + 7)*coef

        amx1 = max ( dabs( a(ny1 + 4) ),dabs( a(ny1 + 5) ) )
        amx2 = max ( dabs( a02 ),dabs( a03 ) )

        if (dabs(a02)/amx2.gt.dabs(a(ny1 + 4))/amx1) then
            coef = a(ny1 + 4)
              a(ny1 + 4) = a02
              a02  = coef
            coef = a(ny1 + 5)
              a(ny1 + 5) = a03
              a03  = coef
            coef = a(ny1 + 7)
                a(ny1 + 7) = a07
                a07  = coef
        endif

        coef = a02/a(ny1 + 4)

        a(ny2+5) = a03 - a(ny1 + 5)*coef
        a(ny2+7) = a07 - a(ny1 + 7)*coef

c
c In the first row the diagonal term is not 1. The goal is to leave
c the connection terms in the lower triangular matrix as 1s.
c


      RETURN
      END



