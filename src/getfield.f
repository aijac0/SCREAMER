      subroutine get_field (text, field, max_fields)
c
c **********************************************************************
c Subroutine to take the character string TEXT and extract the first
c MAX_FIELDS character fields and put them in FIELD.
c The character fields are assumed to be any string of characters not
c containing a blank or comma since it is assumed that blanks and commas
c separate the character fields.
c All lowercase letters are changed to uppercase after they are placed
c in FIELD.
c The fields in FIELD are left justified and padded on the right with
c blanks to fill the characters in each array element.
c
c For example, if:
c   TEXT is ' TRLINE 2.0e3 , 3.1E1 DATA'
c   NUM_FIELDS is 3
c then,
c   FIELD(1) is 'TRLINE'
c   FIELD(2) is '2.0E3'
c   FIELD(3) is '3.1E1' .
c
c Modifications:
c 2014-05-01 RBS: The passed character vector "field" has been changed
c                 to pass the vector size. The character length is
c                 passed via *(*).
c                 The parameter definition for max_fields is no longer
c                 needed to define the size of the vector.
c **********************************************************************
c
c Define passed variables
c
c
      character  text*(*)
      character*(*)  field(*)
      integer    max_fields
c
c Define internal variables
c
      character  blank*1,     comma*1
      parameter (blank = ' ', comma = ',')
      parameter (no_text = 0)
c
      character  curr_char*1
c
c Find the character length of TEXT and FIELD, then set all fields
c to blanks.
c
      lentext  = len (text)
      do j = 1, max_fields
        field(j) = blank
        end do
c
c Check to see that NUM_FIELDS is within prescribed limits.
c
      if (max_fields .lt. 1) then
        return
      end if
c
c Strip leading and trailing blanks and commas in TEXT, then make sure
c there are some characters
c
      call strip_blanks_commas (text, istart, iend)
      if (istart .eq. no_text) then
        return
      end if
c
c Fill the FIELD array by looking for the first blank or comma which
c signals the end of one field and the first character other than a
c blank or comma which signals the beginning of the next field.
c J is the current FIELD element index.
c I is the current TEXT character (byte) position.
c
      j = 0
      i = istart
c
c At this point, I points to the character in TEXT which
c corresponds to first character of character field J.
c Note that I could equal IEND.
c
      do while ((j .lt. max_fields) .and. (i .le. iend))
        j = j + 1
c
c Look until we find a blank or a comma.
c
        istart_temp = i
        curr_char = text(i:i)
        do while ((curr_char .ne. blank)
     &      .and. (curr_char .ne. comma)
     &      .and. (i         .lt. iend))
          i = i + 1
          curr_char = text(i:i)
        end do
c
c I is set to IEND if this is the last field or to the position of
c the first blank or comma encountered after the field. So reset I,
c if not the last field, to signal the end of the character field.
c
        if (i .lt. iend) then
          i = i - 1
        end if
c
c Fill the FIELD array element and convert it to uppercase.
c
        do k = istart_temp, i
          kfield = k - istart_temp + 1
          field(j)(kfield:kfield) = text(k:k)
        end do
        call conv_to_ucase (field(j))
c
c Look for the next character which is not a blank or comma.
c Increment I by 1 because we decremented it above by one and
c so I currently points to the position of the last character in the
c current field.  (Note: I does not equal IEND-1 at this point.)
c
        i = i + 1
c
c At this point, I could be greater than IEND, so check for TEXT(I:I)
c bounds. (Note: I does not equal IEND because I points to a blank or
c comma or is greater than IEND.)
c
        if (i .lt. iend) then
          curr_char = text(i:i)
          do while ((curr_char .eq. blank) .or. (curr_char .eq. comma))
            i = i + 1
            curr_char = text(i:i)
          end do
        end if
c
c End of J loop.
c
      end do
c
      return
      end
