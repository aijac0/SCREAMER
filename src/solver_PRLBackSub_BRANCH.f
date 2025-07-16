      SUBROUTINE PRLBackSub_BRANCH(a,jadd,nrx,node_num,iexit_type,
     &                             topbranch)


!$    use omp_lib

C This SUBROUTINE PRLBackSub_BRANCH executes the backsubsitution step
C in the Gaussian elimination process (starting from the first row)
C for a lower triangular matrix in Screamer format.
C=======================================================================
C input: a           - the coefficient matrix in the Screamer format
C        jadd        - the number of rows in the matrix prior to the row
C                      corresponding to the first node of the branch.
C        nrx         - the total number of nodes in the branch.
C        node_num    - the node number of the connection of the branch.
C                      The number is in absolute numbering.
C        iexit_type  - The connection type of the branch.
C        topbranch   - the indicator of the topbranch
C
C output:a           - the solution vector in the last a(ny +7) column.
C=======================================================================
C
C Written by Dr. Yury Gryazin, 07/20/2019, ISU, Pocatello, ID
C
C
c     Define passed variables
c

      REAL*8  a(*)
      INTEGER jadd,nrx,node_num,iexit_type,topbranch
c
c Define internal variables
c
      INTEGER k,nrow,ny,ny1,nyM1,nr2,i,nrm,nzz

c
c It is assumed that the solution in the connected to branch are already
c calculated. First, y(jadd+1) is calculated by using these values.
c
        k    = 2
        nrow = 3*k + 1
        nr2 = 2 * nrx
        nzz = jadd*nrow
        nrm = nrx - 1

        ny  = nzz
        ny1 = ny + nrow
        nyM1= ny - nrow

        icx        = (node_num-2)*2
        icy        = (node_num-1)*2

        ny_icx = icx*nrow
        ny_icy = icy*nrow


        if (iexit_type .eq. topbranch) then

            a(ny + 6)=(a(ny + 7)+a(ny_icx+6)-a(ny_icy+6))/a(ny+3)

        else

            a(ny + 6)=( a(ny + 7) + a(ny_icy+6) )/a(ny + 3)

        end if

        a(ny1 + 6)= ( a(ny1 + 7) - a(ny1 + 3)*a(ny + 6) )/a(ny1 + 4)

c
c Loop over the nodes from 2 to nrm.
c

        DO i = 2, nrm

            ny  = nrow*(i-1)*2 + nzz
            ny1 = ny + nrow
            nyM1 = ny - nrow

            a(ny+6)=( a(ny+7) - a(ny +2)*a(nyM1+6))/a(ny + 3)
            a(ny1+6)=(a(ny1+7)-a(ny1+3)*a(ny+6)-a(ny1+2)*a(nyM1+6)
     &                )/a(ny1+4)

            ENDDO
c
c Last node.
c
        ny   =  (nr2-2)*nrow + nzz
        ny1  = ny + nrow
        nyM1 = ny - nrow

        a(ny +6) = ( a(ny +7) - a(ny +2)*a(nyM1 +6) )/a(ny + 3)
        a(ny1+6) =   a(ny1+7)/a(ny1+4)


      RETURN
      END
