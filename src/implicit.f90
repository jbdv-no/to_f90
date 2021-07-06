Module implicit
  ! Module to set and reset implicit variable types for use by to_f90.

  Implicit None
  Integer, Save :: var_type(26) = [ 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, &
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ]
  ! a b c d e f g h i j k l m n o p q r s t u v w x y z
  Character (Len=24), Save :: vt(0:7) = [ 'NO TYPE                 ', &
    'REAL                    ', 'INTEGER                 ', &
    'DOUBLE PRECISION        ', 'LOGICAL                 ', &
    'COMPLEX                 ', 'CHARACTER               ', &
    'OTHER TYPE              ' ]

Contains


  Subroutine reset_defaults()

    var_type(1:8) = 1              ! REAL (A-H)
    var_type(9:14) = 2             ! INTEGER (I-N)
    var_type(15:26) = 1            ! REAL (O-Z)

    Return
  End Subroutine reset_defaults



  Subroutine set_implicit_types(text)
    ! Read in implicit statement and interpret.

    Character (Len=*), Intent (Inout) :: text

    ! Local variables
    Integer :: ivt, length, start, i, j, pos, left, right
    Logical :: first

    i = index(text, 'IMPLICIT')
    If (i>0) text = text(i+8:)
    text = adjustl(text)

    Do
      If (text(1:4)=='NONE') Then
        var_type = 0
        Return
      Else If (text(1:4)=='REAL') Then
        ivt = 1
      Else If (text(1:7)=='INTEGER') Then
        ivt = 2
      Else If (text(1:24)=='DOUBLE PRECISION COMPLEX') Then
        ivt = 7
        vt(7) = 'DOUBLE PRECISION COMPLEX'
      Else If (text(1:16)=='DOUBLE PRECISION') Then
        ivt = 3
      Else If (text(1:7)=='LOGICAL') Then
        ivt = 4
      Else If (text(1:7)=='COMPLEX') Then
        ivt = 5
      Else If (text(1:9)=='CHARACTER') Then
        ivt = 6
      Else
        ivt = 7
        i = index(text, ' ')
        vt(7) = text(1:i-1)
      End If

      ! Interpret the part in brackets, e.g. (a - h, o - z)

      length = len_trim(text)
      start = 5
      left = index(text(start:length), '(') + start - 1
      If (left<start) Return
      right = index(text(start:length), ')') + start - 1
      If (right<left) Return
      ! Interpret text(left+1:right-1)
      first = .True.
      Do pos = left + 1, right
        Select Case (text(pos:pos))
        Case (' ')
          Cycle
        Case ('-')
          first = .False.
        Case (',', ')')
          If (first) Then
            var_type(i) = ivt
          Else
            var_type(i:j) = ivt
            first = .True.
          End If
        Case Default
          If (first) Then
            i = ichar(text(pos:pos)) - ichar('a') + 1
            If (i<1) Then
              i = ichar(text(pos:pos)) - ichar('A') + 1
            End If
          Else
            j = ichar(text(pos:pos)) - ichar('a') + 1
            If (j<1) Then
              j = ichar(text(pos:pos)) - ichar('A') + 1
            End If
          End If
        End Select
      End Do

      start = right + 1
      If (start>=length) Return
      text = text(start:length)
      Do
        If (text(1:1)==',' .Or. text(1:1)==' ') Then
          text = text(2:)
        Else
          Exit
        End If
      End Do
    End Do

    Return
  End Subroutine set_implicit_types



  Function implicit_type(ch) Result (vtype)
    ! Return the variable type given the first character of its name.
    ! The first character is expected to be lower case, but just in case ..

    Character (Len=1), Intent (In) :: ch
    Character (Len=24) :: vtype

    ! Local variable
    Integer :: i, j

    i = ichar(ch) - ichar('a') + 1
    If (i>=1 .And. i<=26) Then
      j = var_type(i)
      vtype = vt(j)
    Else
      i = ichar(ch) - ichar('A') + 1
      If (i>=1 .And. i<=26) Then
        j = var_type(i)
        vtype = vt(j)
      Else
        vtype = ' '
      End If
    End If

    Return
  End Function implicit_type

End Module implicit
