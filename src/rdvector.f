      subroutine read_vector (vals,num_vals,eofflg,nlines,numerr)
c
c-------Description--------------------------------------------------
c
c Author/Date:  Rick Spielman   2014-05-01
c
c Purpose: This subroutine reads in a series of values from an input
c          list using the switch_time SWI option
c
c Called by: Subroutine READSCREAMERPARAM
c
c Calls:  Subroutine GET_NEXT_LINE
c         Subroutine TEXT_TO_REAL
c         Subroutine PRINT_BAD_LINE
c
c Modification
c 2017-02-24 RBS: Added currline_lc to the p[assed variables of
c                 get_next_line. Declared currline_lc*120
c
c-------Include Files---------------------------------------------------
c
      use zdemmax
      include  'zdemparm.h'    ! Contains keywords
c
c-------Input Parameters------------------------------------------------
c
      integer   numerr         ! Current number of error while reading*/
c                              ! SCREAMER input deck                  */
c
c-------Output Parameters-----------------------------------------------
c
      real       vals(*)       ! Vector which holds data values       */
      integer    num_vals,     ! # of values in array VALS            */
     +           eofflg,       ! End-of-file flag                     */
     +           nlines        ! Current line # in SCREAMER input deck*/
c
c-------Constants-------------------------------------------------------
c
      integer    noerr,          error,     notext
      parameter (noerr      = 0, error = 1, notext = 0)
      integer    max_fields
      parameter (max_fields    = 10)
c
c-------Local Variables-------------------------------------------------
c
      character  currline*120  ! Text of current line from SCR inp.dck*/
      character  currline_lc*120  ! Mixed case copy of currline       */
      integer    flag1         ! Error flag for TEXT_TO_REAL call     */
      character  field(max_fields)*80, ! Fields from current line of  */
     +                                 !  text of SCREAMER input deck */
     +           keyword*(keyword_len) ! First non-numeric entry after*/
     +                                 !  list of values in table     */
c
c-------Subroutine Body-------------------------------------------------
c
c Set the counter for the number of elements read and the limit check to
c no error
c
      num_vals  = 0
      limit     = noerr
c
c Get the line.
c
  200 continue
      call get_next_line
     &     (currline, currline_lc, field, nlines, eofflg, max_fields)
      if (eofflg .eq. error) return
c
      if (num_vals .lt. max_switch_points) then
c
c Attempt to convert each field into a real number.
c
         call text_to_real (field(1), vals(num_vals+1), flag1)
c
c If we failed to convert a number, see if we had the last entry keyword.
c If so, then set the number of parameters entered, else signal an error.
c If we did fail, this will cause the subroutine to exit.
c
        if (flag1 .eq. noerr) then
           num_vals = num_vals + 1
         else
           keyword = field(1)(1:keyword_len)
           if (keyword .ne. k_last_entry) then
              call print_bad_line (currline, nlines, numerr)
              end if
           go to 1000
         end if
c
c If we have read in the maximum number of data, check this line
c for LAST keyword.  If not LAST do not use anything on the line.
c
      else
        keyword = field(1)(1:keyword_len)
c
        if (keyword .ne. k_last_entry) then
           numerr = numerr + 1
           limit      = error
          else
           go to 1000
        end if
c
      end if
      go to 200
c
c If we were over the limit, send a message and tell what the current limit
c is.
c
 1000 continue
      eofflg = noerr
c
      if (limit .eq. error) then
         write(9,'(A/A,i3,A)')
     &   '0', 'Error, too many table values entered, only ',
     &    max_switch_points, ' points allowed!'
      end if
c Test routine
c      print '(6F10.5)'
c     &      , vals(1), vals(2), vals(3),vals(4), vals(5),vals(6)
c
      return
      end
