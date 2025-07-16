      SUBROUTINE solver_PRLConElim(a,node_num,iexit_type,topbranch,
     &                             a03,a07,a13, a14,a17)

!$    use omp_lib

C This SUBROUTINE bandPRL3diag modified the matrix in Screamer format to
C eliminate the connection terms in the upper part of the matrix.
C=======================================================================
C input: a           - the coefficient matrix in the Screamer format
C        node_num    - the node number of the connection of the branch.
C                      The number is in absolute numbering.
C        iexit_type  - The connection type.
C        a03,ao7     - The diagonal and rhs terms in the part of the lower
C        a13,a14,a17   triangular matrix corresponding to the 1st node of
C                      the branch (after forward step of the Gaussian
C                      elimination in the branch0).
C        topbranch   - the indicator of the topbranch

C output: a          - the coefficient matrix in the Screamer format
C                      where the connection terms were eliminated.
C=======================================================================
C
C Written by Dr. Yury Gryazin, 07/15/2019, ISU, Pocatello, ID
C
C
c     Define passed variables
c

      REAL*8  a(*), a03, a07, a13, a14, a17
      INTEGER node_num, iexit_type, topbranch
c
c Define internal variables
c
      INTEGER icx,icy, nrow, ny, ny1, ny0, nyM1
      REAL*8 coef, coef1, coef2, coef3
c
c First, find the location of the connection terms.
c
        nrow       = 7
        icx        = (node_num-2)*2 + 1
        icy        = (node_num-1)*2 + 1
c        print *, 'icx =', 7*icx, 'icy=', 7*icy

c
c Elimination of the connection terms in icx and icy and
c presenting the corresponding rows in the three diagonal form.
c
        ny = nrow*icy

        if (iexit_type .eq. topbranch) then
c
c The location of the terms corresponding to the branch
c connection terms.

            ny1 = ny - nrow
            ny0 = ny1 - nrow
            nyM1 = ny0 - nrow

c            print *,'EL ny =',ny,'ny1 =',ny1,'ny0 =',ny0,'nyM1 =',nyM1
c            print *, 'a03 =', a03
c            print *, 'a13 =', a13, 'a14=', a14

            coef  = .5/a14
            coef1 = coef*a17

c            print *,'EL coef =', coef,'coef1 =',coef1


            a(ny0 + 7) = a(ny0 + 7) - coef1
            a(ny  + 7) = a(ny  + 7) + coef1

            coef1 = coef*a13/a03
            coef2 = coef1*a07

c            print *,'EL coef1 =', coef1,'coef2 =',coef2
c            print *,'EL a03 =', a03,'a07 =',a07

            a(ny0 + 7) = a(ny0 + 7) + coef2
            a(ny  + 7) = a(ny  + 7) - coef2

            coef = 2*coef1
            coef2 = coef*a(nyM1 + 4)
            a(ny0 + 4) = a(ny0 + 4) - coef2
            a(ny  + 2) = a(ny  + 2) + coef2

            coef2 = coef*a(nyM1 + 7)
            a(ny0 + 7) = a(ny0 + 7) - coef2
            a(ny  + 7) = a(ny  + 7) + coef2

c            print *,'EL a_0(2,3,4,7) =',a(ny0+2),a(ny0+3),
c     &                                  a(ny0+4),a(ny0+7)
c
c            print *,'EL a(2,3,4,7) =',a(ny + 2),a(ny + 3),
c    &                                a(ny + 4),a(ny + 7)

        else

            coef  = .5/a14

            a(ny + 7) = a(ny + 7) - coef*a17

            coef1 = coef*a13/a03

            a(ny + 3) = a(ny + 3) - coef1
            a(ny + 7) = a(ny + 7) + coef1*a07

        end if


      RETURN
      END



