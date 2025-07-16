      SUBROUTINE bandmatrix(a,am,rhs,nk,nr,indexb,nadd_array,nb,
     &                      topbranch,max_am)

C This SUBROUTINE bandmatrix converts a matrix in the Screamer format into
C the full coefficient matrix and the right hand side of the discretized
C linear system .


C========================================================================
C input:    a             - the coefficient matrix in the Screamer format
C           nk            - the size of the square matrix (nk by nk), i.e.
C                           the total number of nodes times two.
C           nr(ib)        - the total number of nodes in ib-th branch
C           nb            - the total number of branches
C           indexb(1,ib)  - the connection node of the ib-th branch to
C                           the main branch
C           indexb(2,ib)  - the type of the ib-th branch
C           nadd_array(ib)- the total number of nodes in the main branch
C                           and branches from 1 to (ib-1)st.
C           topbranch=1   - the indicator of the top branch
C           max_am        - the maximum dimensioned size of am and rhs
C
C output:   am            - the nk by nk coefficient matrix
C           rhs           - the right hand side in the system
C output
C files :   MTRX          - contains the print out of the coefficient
C                           matrix am
C           RHS           - contains the print out of the right hand
C                           side rhs
C parameter:
C           ktime = 1     - the execution of the program without saving
C                           am and rhs in the files MTRX and RHS
C           ktime = 0     - the execution of the program with saving am
C                           and rhs in the files and termination of the
C                           program
C=======================================================================
C
C Written by Dr. Yury Gryazin, 06/19/2014, ISU, Pocatello, ID
C
C Modifications:
C 2014-12-12 RBS: Changed the array size passed variable from max_bb
C                 to max_am, changed all references to max_bb to max_am
C 2015-06-18 RBAS: Clean up passed and internal variables
C
c     Define passed variables
c

      REAL*8  a(*),am(max_am,*),rhs(*)
      INTEGER nk, nr(*),indexb(2,*),nadd_array(*), nb, topbranch,
     &        max_am
c
c Define internal variables
c
      INTEGER icb, ib, icx, icy, iexit_type, jadd, k, ktime, nbm,
     &        node_num, nr2, nrow, nrm, nrx, ny, nzz
      CHARACTER*5 NAME,NAME1
c
c First, loop over the branches. Initialize am array.
c
      k    = 2
      nrow = 3*k + 1

      nbm = nb - 1
        DO icb = 1, nbm
            ib = icb + 1
            node_num   = indexb(1,icb)
            j          = k * nadd_array(ib) + 1
 
            DO I=1,j
                am(I,j+1) = 0.0
                am(j,I  ) = 0.0
            END DO

        ENDDO


      DO ib = 1, nb

          nzz       = nrow * k * nadd_array(ib)
          jadd      = k * nadd_array(ib)
          nrx       = nr(ib)
          nrm       = nr(ib)-1
c
c Loop over the nodes, from node 2 to the next-to-last using the general
c voltage and current equations.
c
          DO i = 2, nrm
c
c Current equation, second row.  a(ny+4) is the diagonal element
c and is never zero.
c
            j   = (i-1)*2 + 1 + jadd
            ny  = nrow*((i-1)*2 + 1) + nzz

               am(j+1,j+2)  = 0.0
               am(j+1,j+1)  = 2*a(ny+4)
               am(j+1,j  )  = 2*a(ny+3)
               am(j+1,j-1)  = 2*a(ny+2)

               rhs(j+1)     = 2*a(ny+7)

c
c Voltage equation, first row.  a(ny+3) is the diagonal element and is
c never zero.
c

            ny  = ny - nrow
                am(j,j  )  = -2*a(ny+3)
                am(j,j+1)  = -2*a(ny+4)
                am(j,j+2)  = -2*a(ny+5)

                rhs(j)   = -2*a(ny+7)

          ENDDO
c
c Do the first and last nodes of each branch separately because of
c boundary conditions and the possibility of sources.
c
            nr2 = 2 * nrx

c
c First row.  Current equation.  a(ny+3) is the diagonal element and
c should not be zero.
c
            ny  = nzz
            j   = 1 + jadd

            am(j,j  )  = a(ny+3)
            am(j,j+1)  = a(ny+4)

            rhs(j)     = a(ny+7)

c
c
c

c Second row (voltage equation) for first node. a(ny+4) is the diagonal
c element and may not fall below 1e-6.

            ny  = nrow + nzz
            j   = 2 + jadd

            am(j,j-1)  = a(ny+3)
            am(j,j  )  = a(ny+4)
            am(j,j+1)  = a(ny+5)

            rhs(j)     = a(ny+7)

c
c First row for the last node.  Current equation.  a(ny+3) may not fall below
c 1e-6.  Check for voltage or current source at end or normal termination.
c
            ny = (nr2-2)*nrow + nzz
            j  =  nr2-1 + jadd

            am(j,j-1)  = a(ny+2)
            am(j,j  )  = a(ny+3)
            am(j,j+1)  = a(ny+4)

            rhs(j)     = a(ny+7)

c
c Second row for the last node, voltage equation. a(ny+4) can not be zero.
c Set the current flowing in or out of the branch.
c
            ny = (nr2-1)*nrow + nzz
            j  =  nr2 + jadd

            am(j,j)    = a(ny+4)

            rhs(j)     = a(ny+7)


        ENDDO

c
c branch connections
c

      nbm = nb - 1
        DO icb = 1, nbm
            ib = icb + 1

            node_num   = indexb(1,icb)
            iexit_type = indexb(2,icb)
c        write(6,'(I2,A,I4)') icb,' branch connection node:',node_num
c        write(6,'(I2,A,I4)') icb,' branch type ',iexit_type

            jadd      = k * nadd_array(ib)
            j         = 1 + jadd

            icx        = (node_num-2)*k + 1
            icy        = (node_num-1)*k + 1


c
c Fill for top or end branch.
c
c icy is wrong for L3 in L2 but j is right. The coefficients are placed
c in the correct jth row or jth + 1 column.
c
            if (iexit_type .eq. topbranch) then
                    am(icx+1,j+1)     = +1.0
                    am(icy+1,j+1)     = -1.0

                    am(j,icx)         = -1.0
                    am(j,icy)         = +1.0
            else
                    am(icx+1,j+1)     =  0.0
                    am(icy+1,j+1)     = +1.0

                    am(j,icx)         =  0.0
                    am(j,icy)         = -1.0
            end if
        ENDDO


c
c To save the coefficient matrix and the right handside for diagnostics
c in the files MTRX and RHS assign ktime = 0. After writing the files
c to the disk the program will be terminated.
c
      ktime = 0

      if(ktime .EQ. 0) then


        NAME ='MTRX'
        NAME1='RHS'

        OPEN(57,FILE=NAME)

        DO i = 1,nk
            WRITE(57,*) 'Row number=',i
            WRITE(57,*)  (am(i,j),j=1,nk)
        ENDDO

        CLOSE(57)


        OPEN(58,FILE=NAME1)

        DO I = 1,nk

            WRITE(58,*) I,rhs(I)

        ENDDO

        CLOSE(58)
        PRINT *, 'THE COEFFICIENT MATRIX AND RHS ARE STORED in
     & MTRX and RHS files.'
        STOP

      end if

      RETURN
      END


