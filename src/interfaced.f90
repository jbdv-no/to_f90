Module interfaced
  ! Module containing formely external text processing procedures with explicit
  ! interfaces declared in main program to_f90.

  Implicit None

Contains

  Subroutine mark_text(text, n_marks, pos1, pos2, continuation)

    ! Look for exclamation marks or quotes to find any text which must be
    ! protected from case changes.
    ! It is assumed that strings are NOT continued from one line to the next.
    Implicit None

    Character (Len=*), Intent (In) :: text
    Logical, Intent (In) :: continuation
    Integer, Intent (Out) :: n_marks, pos1(:), pos2(:)

    ! Local variables
    Integer :: mark, start, pos_exclaim, pos_sngl_quote, pos_dbl_quote, pos, &
      endpos
    Character (Len=1), Save :: quote
    Logical, Save :: protect = .False.

    mark = 1
    start = 1
    If (continuation .And. protect) Then
      pos1(mark) = 1
      pos = 0
      Go To 110
    End If

    ! Find next opening quote or exclamation mark

   100 protect = .False.
    pos_exclaim = index(text(start:80), '!')
    pos_sngl_quote = index(text(start:80), '''')
    pos_dbl_quote = index(text(start:80), '"')
    If (pos_exclaim==0) pos_exclaim = 81
    If (pos_sngl_quote==0) pos_sngl_quote = 81
    If (pos_dbl_quote==0) pos_dbl_quote = 81
    pos1(mark) = min(pos_exclaim, pos_sngl_quote, pos_dbl_quote)

    If (pos1(mark)==81) Then       ! No more protected regions
      n_marks = mark - 1
      Return
    Else If (pos_exclaim==pos1(mark)) Then ! Rest of line is a comment
      pos1(mark) = pos1(mark) + start - 1
      pos2(mark) = 80
      n_marks = mark
      Return
    End If

    pos = start - 1 + pos1(mark)
    pos1(mark) = pos
    quote = text(pos:pos)

    ! Search for matching quote

   110 endpos = index(text(pos+1:), quote)
    If (endpos>0) Then
      pos2(mark) = pos + endpos
      start = pos2(mark) + 1
      mark = mark + 1
      Go To 100
    End If

    ! No matching end quote - it should be on the next line

    pos2(mark) = 80
    n_marks = mark
    protect = .True.

    Return
  End Subroutine mark_text


  Subroutine convert_text(text, n_marks, pos1, pos2)

    ! Convert unprotected text to upper case if it is a FORTRAN word,
    ! otherwise convert to lower case.
    Implicit None

    Character (Len=*), Intent (Inout) :: text
    Integer, Intent (In) :: n_marks
    Integer, Intent (Inout) :: pos1(:), pos2(:)

    ! Local variables

    Integer :: length, inc = ichar('A') - ichar('a'), pos, mark, i, i1, j, j1, &
      j2, ptr
    Logical :: matched
    Character (Len=11) :: fortran_word(186) = [ 'ABS        ', 'ACCESS     ', &
      'ACOS       ', 'AIMAG      ', 'AINT       ', 'ALOG       ', &
      'ALOG10     ', 'AMAX0      ', 'AMAX1      ', 'AMIN0      ', &
      'AMIN1      ', 'AMOD       ', 'AND        ', 'ANINT      ', &
      'APPEND     ', 'ASIN       ', 'ASSIGN     ', 'ATAN       ', &
      'ATAN2      ', 'BACKSPACE  ', 'BLANK      ', 'BLOCK      ', &
      'BLOCKDATA  ', 'BLOCKSIZE  ', 'CALL       ', 'CCOS       ', &
      'CDABS      ', 'CDCOS      ', 'CDEXP      ', 'CDLOG      ', &
      'CDSIN      ', 'CDSQRT     ', 'CEXP       ', 'CHAR       ', &
      'CHARACTER  ', 'CLOG       ', 'CLOSE      ', 'CMPLX      ', &
      'COMMON     ', 'COMPLEX    ', 'CONJG      ', 'CONTINUE   ', &
      'COS        ', 'COSH       ', 'CSIN       ', 'CSQRT      ', &
      'DABS       ', 'DACOS      ', 'DASIN      ', 'DATA       ', &
      'DATAN      ', 'DATAN2     ', 'DBLE       ', 'DCMPLX     ', &
      'DCONJG     ', 'DCOS       ', 'DCOSH      ', 'DELETE     ', &
      'DEXP       ', 'DIMAG      ', 'DINT       ', 'DIRECT     ', &
      'DLOG       ', 'DLOG10     ', 'DMAX1      ', 'DIMENSION  ', &
      'DMIN1      ', 'DMOD       ', 'DNINT      ', 'DO         ', &
      'DOUBLE     ', 'DSIGN      ', 'DSIN       ', 'DSINH      ', &
      'DSQRT      ', 'DTAN       ', 'DTANH      ', 'ELSE       ', &
      'ELSEIF     ', 'END        ', 'ENDFILE    ', 'ENDIF      ', &
      'ENTRY      ', 'EQ         ', 'EQUIVALENCE', 'EQV        ', &
      'ERR        ', 'EXIST      ', 'EXIT       ', 'EXP        ', &
      'EXTERNAL   ', 'FILE       ', 'FLOAT      ', 'FMT        ', &
      'FORM       ', 'FORMAT     ', 'FORMATTED  ', 'FUNCTION   ', &
      'GE         ', 'GOTO       ', 'GO         ', 'GT         ', &
      'IABS       ', 'IAND       ', 'ICHAR      ', 'IDINT      ', &
      'IDNINT     ', 'IEOR       ', 'IF         ', 'IFIX       ', &
      'IMPLICIT   ', 'INCLUDE    ', 'INDEX      ', 'INPUT      ', &
      'INQUIRE    ', 'INT        ', 'INTEGER    ', 'INTRINSIC  ', &
      'IOSTAT     ', 'ISIGN      ', 'KEEP       ', 'LE         ', &
      'LEN        ', 'LGE        ', 'LGT        ', 'LLE        ', &
      'LLT        ', 'LOG        ', 'LOG10      ', 'LOGICAL    ', &
      'LT         ', 'MAX        ', 'MAX0       ', 'MAX1       ', &
      'MIN        ', 'MIN0       ', 'MIN1       ', 'MOD        ', &
      'NAME       ', 'NAMELIST   ', 'NAMED      ', 'NE         ', &
      'NEQV       ', 'NEW        ', 'NEXTREC    ', 'NONE       ', &
      'NOT        ', 'NUMBER     ', 'OLD        ', 'OPEN       ', &
      'OPENED     ', 'OR         ', 'PARAMETER  ', 'PAUSE      ', &
      'POSITION   ', 'PRECISION  ', 'PRINT      ', 'PROGRAM    ', &
      'READ       ', 'REAL       ', 'REC        ', 'RECL       ', &
      'RETURN     ', 'REWIND     ', 'SAVE       ', 'SCRATCH    ', &
      'SEQUENTIAL ', 'SIGN       ', 'SIN        ', 'SINH       ', &
      'SNGL       ', 'SPACE      ', 'SQRT       ', 'STATUS     ', &
      'STOP       ', 'SUBROUTINE ', 'TAN        ', 'TANH       ', &
      'THEN       ', 'TO         ', 'TYPE       ', 'UNFORMATTED', &
      'UNIT       ', 'UNKNOWN    ', 'WHILE      ', 'WRITE      ' ]
    Character (Len=4) :: compare(6) = [ '.LT.', '.LE.', '.EQ.', '.GE.', &
      '.GT.', '.NE.' ]
    Character (Len=2) :: replacement(6) = [ '< ', '<=', '==', '>=', '> ', &
      '/=' ]

    ! A   B   C   D   E   F   G    H    I    J    K    L    M    N    O
    ! P    Q    R    S    T    U    V    W    X    Y    Z
    Integer, Parameter :: indx(27) = [ 1, 20, 25, 47, 78, 92, 99, 103, 103, &
      121, 121, 122, 132, 139, 149, 153, 159, 159, 165, 177, 182, 185, 185, &
      187, 187, 187, 187 ]

    If (pos1(1)==1 .And. pos2(1)==80) Return ! Entire line protected

    pos = 1
    mark = 1
    length = len_trim(text)
    Do                             ! Convert to upper case
      If (n_marks>=mark .And. pos==pos1(mark)) Then
        pos = pos2(mark) + 1
        mark = mark + 1
        If (pos>=length) Exit
      End If
      If (text(pos:pos)>='a' .And. text(pos:pos)<='z') text(pos:pos) &
        = char(ichar(text(pos:pos))+inc)
      pos = pos + 1
      If (pos>length) Exit
    End Do

    ! Search for `words' in text.
    ! Convert to lower case if they are not FORTRAN words.
    i1 = 1
    pos = 1
    mark = 1
    Do
      If (pos>length) Exit
      If (n_marks>=mark .And. pos>=pos1(mark)) Then
        pos = pos2(mark) + 1
        i1 = pos
        mark = mark + 1
        If (pos>=length) Exit
      End If

      Do
        If ((text(pos:pos)>='A' .And. text(pos:pos)<='Z') .Or. (text( &
          pos:pos)>='0' .And. text(pos:pos)<='9') .Or. text(pos:pos)=='_') &
          Then
          pos = pos + 1
          Cycle
        Else
          Exit
        End If
      End Do

      pos = pos - 1
      ! Now i1 & pos = positions of 1st & last characters of current string

      If (pos<i1) Then             ! Single non-alphanumeric character
        pos = i1 + 1
        i1 = pos
        Cycle
      End If

      ptr = ichar(text(i1:i1)) - ichar('A') + 1
      If (ptr<1 .Or. ptr>26) Then
        pos = pos + 1
        If (pos>length) Exit
        i1 = pos
        Cycle
      End If

      matched = .False.
      If (pos>i1) Then
        j1 = indx(ptr)
        j2 = indx(ptr+1) - 1
        Do j = j1, j2
          If (text(i1:pos)==fortran_word(j)) Then
            matched = .True.
            Exit
          End If
        End Do
      End If

      ! Replace .LT. with <, etc.
      If (matched .And. i1>1) Then
        If (text(i1-1:i1-1)=='.') Then
          Do j = 1, 6
            If (text(i1-1:pos+1)==compare(j)) Then
              text(i1-1:pos+1) = ' ' // replacement(j) // ' '
              Exit
            End If
          End Do
          Do                       ! Remove excess blanks
            i1 = max(i1, 3)
            j1 = index(text(i1-2:pos+2), '  ')
            If (j1==0) Exit
            j1 = j1 + i1 - 3
            text(j1:) = text(j1+1:)
            pos2(mark) = pos2(mark) - 1 ! Adjust mark positions
            Do i = mark + 1, n_marks
              pos1(i) = pos1(i) - 1
              pos2(i) = pos2(i) - 1
            End Do
            pos = pos - 1
          End Do
        End If
      End If

      ! Output line of text to screen if it contains SUBROUTINE or FUNCTION.
      ! Convert ENDIF to END IF, ELSEIF to ELSE IF, and GOTO to GO TO.
      If (matched) Then
        If (text(i1:pos)=='SUBROUTINE' .Or. text(i1:pos)=='FUNCTION') Then
          Write (*, '(1x, a)') text(1:length)
        Else If (text(i1:pos)=='ENDIF') Then
          text(i1:) = 'END IF' // text(pos+1:)
          pos = pos + 1
        Else If (text(i1:pos)=='ELSEIF') Then
          text(i1:) = 'ELSE IF' // text(pos+1:)
          pos = pos + 1
        Else If (text(i1:pos)=='GOTO') Then
          text(i1:) = 'GO TO' // text(pos+1:)
          pos = pos + 1
        End If
      End If

      ! If text is not matched, convert to lower case, if necessary.
      If (.Not. matched) Then
        Do j = i1, pos
          If (text(j:j)>='A' .And. text(j:j)<='Z') text(j:j) &
            = char(ichar(text(j:j))-inc)
        End Do
      End If

      pos = pos + 1
      If (pos>length) Exit
      i1 = pos
    End Do

    Return
  End Subroutine convert_text


  Subroutine remove_data_blanks(text)
    ! Remove any blanks embedded between numerical digits in DATA statements

    Implicit None
    Character (Len=*), Intent (Inout) :: text

    ! Local variables
    Integer :: length, pos, i1
    Character (Len=10) :: numbers = '1234567890'

    length = len_trim(text)
    i1 = 2
    Do
      pos = index(text(i1:length), ' ')
      If (pos==0) Exit
      i1 = i1 + pos - 1
      If (scan(text(i1-1:i1-1),numbers)>0 .And. scan(text(i1+1:i1+ &
        1),numbers)>0) Then
        text = text(:i1-1) // text(i1+1:length)
        length = length - 1
      End If
      i1 = i1 + 2
      If (i1>length) Exit
    End Do

    Return
  End Subroutine remove_data_blanks


  Function last_char(text) Result (ch)
    ! Return the last character on a line
    Implicit None

    Character (Len=*), Intent (In) :: text
    Character (Len=1) :: ch

    ! Local variable
    Integer :: last

    last = len_trim(text)
    If (last==0) Then
      ch = ' '
    Else
      ch = text(last:last)
    End If

    Return
  End Function last_char


  Function find_delimited_name(text, name) Result (pos)
    ! Find a name in a character string with delimiters either side of it,
    ! or after it if it starts at position 1.
    ! An extended version of the intrinsic INDEX.
    ! pos = the position of the first character of name in text (= 0 if not
    ! found).
    ! N.B. When the name is short (e.g. i or n) it could occur as part of some
    ! other name.

    Implicit None
    Character (Len=*), Intent (In) :: text, name
    Integer :: pos

    ! Local variables
    Integer :: i1, ltext, lname

    i1 = 1
    ltext = len_trim(text)
    lname = len_trim(name)
    Do
      pos = index(text(i1:ltext), trim(name))
      If (pos==0) Return
      pos = pos + i1 - 1
      If (pos>1) Then
        If (scan(text(pos-1:pos-1),' <=+-/*,')>0) Then
          If (scan(text(pos+lname:pos+lname),' >=(+-/*,')>0) Return
        End If
      Else
        If (scan(text(pos+lname:pos+lname),' >=(+-/*,')>0) Return
      End If
      i1 = pos + lname
      If (i1+lname>ltext) Exit
    End Do

    pos = 0

    Return
  End Function find_delimited_name

End Module interfaced
