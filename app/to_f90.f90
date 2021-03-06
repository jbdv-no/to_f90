Program to_f90

  Use implicit
  ! JBdV: replaced the explicit interface block within, and external
  ! procedures at the end of this program, with a module called `interfaced`.
  ! `use`'ing the module here. May be a glaring oversight, but I don't see any
  ! reason for not doing so?
  Use interfaced

  Implicit None

  Type :: code
    Character (Len=140) :: text
    Character (Len=5) :: label
    Type (code), Pointer :: next
  End Type code

  Type :: argument
    Character (Len=10) :: name
    Integer :: intention           ! IN = 1, OUT = 2, IN OUT = 3
    Character (Len=24) :: var_type ! Room for DOUBLE PRECISION COMPLEX
    Integer :: dim                 ! DIM = 0 for scalars
    Character (Len=24) :: dimensions ! Not used if DIM = 0
    Type (argument), Pointer :: next
  End Type argument

  Character (Len=60) :: f77_name, f90_name
  Character (Len=1) :: tab = char(9), ch
  Character (Len=50) :: prog_unit_name = ' ', blank = ' ', case_expr
  Character (Len=9) :: delimiters = ' =+-*/,()'
  Character (Len=10) :: numbers = '1234567890'
  Character (Len=5) :: lab
  Character (Len=30) :: text, vtype
  Character (Len=140) :: statement
  Character (Len=8) :: date
  Character (Len=10) :: time
  Integer :: iostatus, pos, count, last, n_marks, pos1(20), pos2(20), &
    lab_length, indent, i, i1, i2, length, numb_arg, i3, i4
  Type (code), Pointer :: head, current, tail, last_line, next_line, &
    first_decl, last_decl, start_prog_unit, end_prog_unit
  Logical :: asterisk, ok, data_stmnt, first_arg, continuation
  Type (argument), Pointer :: arg_start, arg, last_arg


  Do
    Write (*, '(a)', Advance='NO') ' Enter name of Fortran source file: '
    Read (*, '(a)', Iostat=iostatus) f77_name
    If (iostatus<0) Stop           ! Halts gracefully when the names are
    ! read from a file and the end is reached

    If (len_trim(f77_name)==0) Cycle
    If (index(f77_name,'.')==0) Then
      last = len_trim(f77_name)
      f77_name(last+1:last+4) = '.for'
    End If
    Open (8, File=f77_name, Status='old', Iostat=iostatus)
    If (iostatus/=0) Then
      Write (*, *) '** Unable to open file: ', f77_name
      Cycle
    End If

    pos = index(f77_name, '.', back=.True.) ! Added BACK=.TRUE. for Unix
    ! names e.g. prog.test.f
    f90_name = f77_name(1:pos) // 'f90'
    Open (9, File=f90_name)

    ! Set up a linked list containing the lines of code

    Nullify (head, tail)
    Allocate (head)
    tail => head
    Read (8, '(a)') head%text
    If (head%text(1:1)=='C' .Or. head%text(1:1)=='c' .Or. head%text(1:1)=='*') &
      Then
      head%text(1:1) = '!'
    Else If (head%text(1:1)==tab) Then
      head%text = '      ' // head%text(2:)
    End If
    head%label = ' '
    count = 1

    Do
      Nullify (current)
      Allocate (current)
      Read (8, '(a)', Iostat=iostatus) current%text
      If (iostatus/=0) Exit

      ! Change C, c or * in column 1 to !
      If (current%text(1:1)=='C' .Or. current%text(1:1)=='c' .Or. &
        current%text(1:1)=='*') Then
        If (len_trim(current%text)>1) Then
          current%text(1:1) = '!'
        Else
          current%text = ' '       ! Leave blank if nothing else on line
        End If
        current%label = ' '
      Else
        current%label = adjustl(current%text(1:5))
      End If

      count = count + 1
      If (current%label(1:1)==tab) Then ! Expand tabs
        current%label = ' '
        current%text = '      ' // current%text(2:)
      Else If (current%label(1:1)=='!') Then
        current%label = ' '
      Else
        current%label = adjustl(current%label)
      End If

      Nullify (current%next)
      tail%next => current
      tail => current
    End Do

    Write (*, *) 'No. of lines read =', count

    ! ---------------------------------------------------------------------------

    current => head
    Nullify (last_line)
    data_stmnt = .False.

    Do
      ! Look for blanks in columns 1-5 followed by non-blank in column 6.
      ! If found, add an ampersand at the end of the previous line.

      If (current%label=='     ' .And. current%text(6:6)/=' ' .And. &
        current%text(1:1)/='!') Then
        last = len_trim(last_line%text)
        last_line%text(last+3:last+3) = '&'
        current%text(6:6) = ' '
        continuation = .True.
      Else
        data_stmnt = .False.
        continuation = .False.
      End If

      ! Replace tabs with single spaces
      Do
        pos = index(current%text, tab)
        If (pos==0) Exit
        current%text(pos:pos) = ' '
      End Do

      ! Remove leading blanks
      current%text = adjustl(current%text)

      ! Mark regions of text which must not have their case changed.
      Call mark_text(current%text, n_marks, pos1, pos2, continuation)

      ! Convert cases of regions which are not protected.
      Call convert_text(current%text, n_marks, pos1, pos2)

      ! If line is start of a program unit, record its name
      If (current%text(1:7)=='PROGRAM') Then
        prog_unit_name = current%text(1:50)
      Else If (current%text(1:10)=='SUBROUTINE') Then
        pos = index(current%text, '(') - 1
        If (pos<0) pos = len_trim(current%text)
        prog_unit_name = current%text(1:pos)
      Else If (current%text(1:9)=='BLOCKDATA') Then
        prog_unit_name = current%text(1:50)
      Else
        ! N.B. 'FUNCTION' could be part of a comment
        pos = index(current%text, 'FUNCTION')
        If (pos>0 .And. index(current%text,'!')==0 .And. &
          index(current%text,'''')==0) Then
          last = index(current%text, '(') - 1
          If (last<0) last = len_trim(current%text)
          prog_unit_name = current%text(pos:last)
        End If
      End If

      ! If first word is one of INTEGER, REAL, DOUBLE PRECISION, CHARACTER ,
      ! LOGICAL or COMPLEX, add :: unless FUNCTION appears on the same line
      ! or next non-blank character is '*' as in REAL*8.
      If (index(current%text,'FUNCTION')==0) Then
        pos = 0
        If (index(current%text,'INTEGER')==1) Then
          pos = 9
        Else If (index(current%text,'REAL')==1) Then
          pos = 6
        Else If (index(current%text,'DOUBLE PRECISION')==1) Then
          pos = 18
        Else If (index(current%text,'CHARACTER')==1) Then
          pos = 11
        Else If (index(current%text,'COMPLEX')==1) Then
          pos = 9
        Else If (index(current%text,'LOGICAL')==1) Then
          pos = 9
        End If

        If (pos>0) Then
          asterisk = index(current%text(pos-1:pos), '*') > 0
          If (.Not. asterisk) Then
            If (pos/=11) Then
              current%text = current%text(1:pos-1) // ':: ' // &
                adjustl(current%text(pos:))
            Else                   ! CHARACTER type, default length = 1
              current%text = 'CHARACTER (LEN=1) :: ' // &
                adjustl(current%text(pos:))
            End If
          Else
            If (pos==11) Then      ! CHARACTER * found
              i1 = index(current%text, '*') + 1
              length = len_trim(current%text)
              ! Get length, could be (*)
              Do
                If (current%text(i1:i1)/=' ') Exit
                If (i1>=length) Exit
                i1 = i1 + 1
              End Do
              If (current%text(i1:i1)=='(') Then
                i1 = i1 + 1
                i2 = index(current%text, ')') - 1
              Else
                i2 = index(current%text(i1:), ' ') + i1 - 2
              End If
              current%text = 'CHARACTER (LEN=' // current%text(i1:i2) // &
                ') :: ' // adjustl(current%text(i2+2:))
            End If
          End If
          ! Check for 2 or more lengths in CHARACTER declaration.
          ! e.g. CHARACTER a, b, c*10, d
          ! Put 2nd (& later) declarations on separate lines:
          ! CHARACTER*10 c
          ! But check for CHARACTER*10 a(*) where last * is not a
          ! length but a dimension
          If (pos==11) Then
            pos = index(current%text, '::') + 2
            Do
              i = index(current%text(pos:), '*')
              If (i==0) Exit
              i = i + pos - 1
              length = len_trim(current%text)
              i1 = index(current%text(:i-1), ',', back=.True.)
              i1 = max(pos, i1)
              i2 = index(current%text(i+1:), ',')
              If (i2==0) Then
                i2 = length + 1
              Else
                i2 = i2 + i
              End If
              ! i1, i2 mark commas at beginning & end of `, name*xx,'
              ! but we could have `name(xx, *), '
              ! Test for * after ( , or ) before ,
              i3 = index(current%text(i1:i2), '(')
              i4 = index(current%text(i1:), ')')
              If (i3>0) Then
                i4 = i4 + i1 - 1
                i2 = index(current%text(i4+1:), ',')
                If (i2==0) Then
                  i2 = length + 1
                Else
                  i2 = i2 + i4
                End If
                pos = i2 + 1
                Cycle
              Else If (i4>0) Then
                i4 = i4 + i1 - 1
                i2 = index(current%text(i4+1:), ',')
                If (i2==0) Then
                  i2 = length + 1
                Else
                  i2 = i2 + i4
                End If
                pos = i2 + 1
                Cycle
              End If

              If (i1==pos .And. i2==length+1) Then
                ! Only one declaration left on line, e.g.
                ! CHARACTER :: name*50
                current%text = 'CHARACTER (LEN=' // current%text(i+1:length) &
                  // ') :: ' // adjustl(current%text(i1:i-1))
                Exit
              End If

              Allocate (next_line)
              next_line%next => current%next
              current%next => next_line
              next_line%text = 'CHARACTER' // current%text(i:i2-1) // ' ' // &
                current%text(i1+1:i-1)
              If (i2<length) Then
                current%text = current%text(:i1) // current%text(i2+1:length)
              Else
                current%text = current%text(:i1-1)
              End If
            End Do
          End If
        End If
      End If

      ! If this is in a DATA statement, eliminate any blanks within numbers
      If (data_stmnt .Or. current%text(1:4)=='DATA') Then
        Call remove_data_blanks(current%text)
        last = len_trim(current%text)
        data_stmnt = .True.
      End If

      ! If line only contains 'END', add the program unit name
      If (len_trim(current%text)==3 .And. current%text(1:3)=='END') Then
        current%text = current%text(1:3) // ' ' // prog_unit_name
        prog_unit_name = ' '

        ! Convert `enddo' to 'END DO'
      Else If (current%text(1:5)=='enddo') Then
        current%text = 'END DO' // current%text(6:)
      End If

      last_line => current
      If (associated(current,tail)) Exit
      If (.Not. associated(current)) Exit
      current => current%next
    End Do

    ! -------------------------------------------------------------------------

    ! Now convert Do-loops

    current => head
    Write (*, *) '      Converting DO-loops, 3-way IFs, & computed GO TOs'
    Do
      If (current%text(1:1)/='!' .And. current%text(1:1)/=' ') Then
        pos = index(current%text, 'DO')
        If (pos>0 .And. (current%text(pos+2:pos+2)==' ' .Or. current%text(pos+ &
          2:pos+2)==',')) Then
          If (current%text(pos+2:pos+2)==',') current%text(pos+2:pos+2) = ' '
          If (pos==1) Then
            ok = .True.
          Else If (scan(current%text(pos-1:pos-1),delimiters)>0) Then
            ok = index(current%text(:pos-1), 'END ') == 0
          Else
            ok = .False.
          End If
          If (ok) Then
            text = adjustl(current%text(pos+3:))
            last = index(text, ' ')
            lab = text(:last-1)
            If (scan(lab(1:1),numbers)==0) lab = ' '
            lab_length = len_trim(lab)
            If (lab_length>0) Then
              pos = index(lab, ',') ! Check for a comma after label
              If (pos>0) Then
                lab(pos:) = ' '
                i = index(current%text, ',')
                current%text(i:i) = ' '
                lab_length = pos - 1
              End If
              Call do_loop_fixup(current, lab)
            End If
          End If

          ! Test for computed GO TO
        Else If (index(current%text,'GO TO')>0) Then
          i1 = index(current%text, 'GO TO')
          statement = adjustl(current%text(i1+5:))
          ! Test for a `('
          If (statement(1:1)=='(') Then
            ok = .True.
            ! If current line is continued, try appending
            ! the next line
            If (last_char(statement)=='&') Then
              next_line => current%next
              length = len_trim(statement) + len_trim(next_line%text)
              ok = (length<=141) .And. (last_char(next_line%text)/='&')
              If (ok) Then
                pos = len_trim(statement)
                statement = trim(statement(:pos-1)) // trim(next_line%text)
                current%next => next_line%next
                Deallocate (next_line)
              End If
            End If

            If (ok) Then
              ! Check for comma between ( and )
              pos = index(statement, ')')
              If (index(statement(2:pos-1),',')>0) Then
                ! We could have something like:
                ! IF (condition) GO TO (100, 200, 300) ivar
                ! Before doing any more, split into 3 lines:
                ! IF (condition) THEN
                ! GO TO (100, 200, 300) ivar
                ! END IF
                If (current%text(1:2)=='IF') Then
                  If (current%text(3:3)==' ' .Or. current%text(3:3)=='(') Then
                    current%text = current%text(:i1-1) // 'THEN'
                    i1 = 2
                    Call insert_and_moveto_newline(current)
                    current%text = ' '
                    next_line => current
                    Call insert_and_moveto_newline(next_line)
                    next_line%text = 'END IF'
                  End If
                End If
                ! Get the CASE variable or expression
                case_expr = adjustl(statement(pos+1:))
                If (case_expr(1:1)==',') case_expr = adjustl(case_expr(2:))
                current%text = current%text(:i1-1) // 'SELECT CASE ( ' // &
                  trim(case_expr) // ' )'
                ! Put in pairs of lines:  CASE ( i )
                ! GO TO i-th label
                Call goto_cases(statement(2:pos-1))
              End If
            End If
          End If

          ! Look for IF, then a number as last non-blank character
        Else
          pos = index(current%text, 'IF')
          If (pos>0) Then
            last = len_trim(current%text)
            If (scan(current%text(last:last),numbers)>0) Then
              Call fix_3way_if(current)
            End If
          End If
        End If
      End If

      If (associated(current,tail)) Exit
      If (.Not. associated(current)) Exit
      current => current%next
    End Do

    ! -------------------------------------------------------------------------

    ! Determine INTENTs for dummy arguments

    Write (*, *) '      Determining INTENTs of dummy arguments'

    ! Search for either FUNCTION or SUBROUTINE.
    ! Extract name of program unit.

    current => head
    Nullify (last_line)
    outer_loop: Do
      Do
        If (current%text(1:1)/='!' .And. current%text(1:1)/=' ') Then
          If (current%text(1:10)=='SUBROUTINE') Then
            pos = index(current%text, '(') - 1
            If (pos<0) pos = len_trim(current%text)
            prog_unit_name = current%text(1:pos)
            Exit
          Else
            pos = index(current%text, 'FUNCTION')
            If (pos>0) Then
              last = index(current%text, '(') - 1
              If (last<0) last = len_trim(current%text)
              prog_unit_name = current%text(pos:last)
              Exit
            End If
          End If
        End If

        last_line => current
        current => current%next
        If (associated(current,tail)) Exit outer_loop
      End Do

      ! If there is no blank line between this program unit and the previous
      ! one, then insert one.

      If (associated(last_line)) Then
        If (len_trim(last_line%text)>0) Then
          Call insert_and_moveto_newline(last_line)
          last_line%text = ' '
        End If
      End If

      Allocate (start_prog_unit)
      start_prog_unit => current

      ! Find end of program unit

      Do
        current => current%next
        If (current%text(1:1)/='!' .And. current%text(1:1)/=' ') Then
          If (current%text(1:3)=='END') Then
            If (index(current%text(5:),prog_unit_name)>0) Then
              Allocate (end_prog_unit)
              end_prog_unit => current
              Exit
            End If
          End If
        End If
        If (associated(current,tail)) Exit outer_loop
      End Do

      ! Find first & last declarations

      Allocate (first_decl, last_decl)
      Call find_declarations(start_prog_unit, end_prog_unit, first_decl, &
        last_decl)
      If (.Not. associated(last_decl)) Go To 100

      ! Extract list of dummy arguments

      Call get_arg_list()
      If (numb_arg==0) Go To 100

      ! See if the declarations contain any IMPLICIT statements

      Call reset_defaults()
      current => first_decl
      Do
        If (current%text(1:8)=='IMPLICIT') Then
          statement = current%text(10:)
          Call set_implicit_types(statement)
        End If
        If (associated(current,last_decl)) Exit
        current => current%next
      End Do

      ! Search through the declarations for variable types & dimensions

      Call get_var_types()

      ! Search through rest of code to try to determine the INTENTs

      Call get_intents()

      ! Insert INTENT statements

      statement = first_decl%text
      first_decl%text = ' '
      current => first_decl
      arg => arg_start
      Do
        Call insert_and_moveto_newline(current)
        current%text = arg%var_type
        Select Case (arg%intention)
        Case (0, 3)
          current%text = trim(current%text) // ', INTENT(IN OUT)'
        Case (1)
          current%text = trim(current%text) // ', INTENT(IN)'
        Case (2)
          current%text = trim(current%text) // ', INTENT(OUT)'
        End Select
        current%text = current%text(:41) // ':: ' // arg%name
        If (arg%dim>0) current%text = trim(current%text) // arg%dimensions

        If (associated(arg,last_arg)) Exit
        arg => arg%next
      End Do
      Call insert_and_moveto_newline(current)
      current%text = statement

      ! Search for, and convert, any PARAMETER statements

      current => first_decl
      Do
        If (current%text(1:9)=='PARAMETER') Then
          Call convert_parameter(current)
        End If
        If (associated(current,last_decl)) Exit
        current => current%next
      End Do

      ! Insert a blank line after the last declaration if there is not one
      ! there already, or a comment.

      next_line => last_decl%next
      If (next_line%text(1:1)/=' ' .And. next_line%text(1:1)/='!') Then
        Call insert_and_moveto_newline(last_decl)
        last_decl%text = ' '
      End If

      ! Move onto the next SUBROUTINE or FUNCTION

     100   current => end_prog_unit
      If (associated(current,tail)) Exit
      last_line => current
      current => current%next
      If (associated(current,tail)) Exit
    End Do outer_loop

    ! -------------------------------------------------------------------------

    ! Indenting and writing output file

    ! Output header line & any continuation lines

    current => head
    continuation = .False.
    Do
      If (continuation) Then
        Write (9, '(t9, a)') trim(current%text)
      Else
        Write (9, '(a)') trim(current%text)
      End If
      ch = last_char(current%text)
      current => current%next
      If (ch/='&') Exit
      continuation = .True.
    End Do
    ! Date & time stamp
    Call date_and_time(date, time)
    If (ch/=' ') Write (9, *)
    Write (9, '("! Code converted using TO_F90 by Alan Miller")')
    Write (9, '("! Date: ", a4, "-", a2, "-", a2, "  Time: ", a2, &
      &":", a2,      ":", a2)') date(1:4), date(5:6), date(7:8), time(1:2), &
      time(3:4), time(5:6)
    If (len_trim(current%text)>0) Write (9, *)

    indent = 0
    continuation = .False.
    Write (*, *) '      Writing file: ', f90_name

    Do
      If (current%text(1:1)/='!') Then
        If (index(current%text,'END ')>0) Then
          If (index(current%text,'END SELECT')==0) indent = max(indent-2, 0)
          Write (9, '(a)') blank(:indent) // trim(current%text)
          continuation = (last_char(current%text)=='&')
        Else If (index(current%text,'DO ')>0) Then
          Write (9, '(a)') blank(:indent) // trim(current%text)
          continuation = (last_char(current%text)=='&')
          indent = indent + 2
          ! Temporary reduction in
          ! indentation for `ELSE'
        Else If (index(current%text,'ELSE')>0) Then
          last = max(0, indent-2)
          Write (9, '(a)') blank(:last) // trim(current%text)
          continuation = (last_char(current%text)=='&')
          ! Indent increased if `IF'
          ! is followed by `THEN'
        Else If (index(current%text,'IF ')>0 .Or. index(current%text,'IF(')>0) &
            Then
          current%text = blank(:indent) // trim(current%text)
          ! If IF statement runs onto
          ! next line, try joining
          last = len_trim(current%text)
          If (current%text(last:last)=='&') Then
            next_line => current%next
            If (last+len_trim(next_line%text)<80) Then
              current%text(last:last) = ' '
              current%text = trim(current%text) // ' ' // trim(next_line%text)
              current%next => next_line%next
            End If
          End If

          Write (9, '(a)') trim(current%text)
          continuation = (last_char(current%text)=='&')
          next_line => current
          Do
            If (index(next_line%text,' THEN')>0 .Or. &
              index(next_line%text,')THEN')>0) Then
              indent = indent + 2
              Exit
            Else
              If (last_char(next_line%text)/='&') Exit
            End If
            next_line => next_line%next
          End Do
        Else

          ! If line ends with '&', attempt to join on the next line if it is
          ! short.

          last = len_trim(current%text)
          If (last>0) Then
            If (current%text(last:last)=='&') Then
              last = len_trim(current%text(:last-1))
              next_line => current%next
              If (last+indent+len_trim(next_line%text)<78) Then
                current%text = current%text(:last) // ' ' // &
                  trim(next_line%text)
                current%next => next_line%next
                Deallocate (next_line)
              End If
            End If
          End If

          If (continuation) Then
            Write (9, '(a)') blank(:indent+4) // trim(current%text)
          Else
            Write (9, '(a)') blank(:indent) // trim(current%text)
          End If
          continuation = (last_char(current%text)=='&')
        End If
        ! Comment line (unchanged)
      Else
        Write (9, '(a)') trim(current%text)
        continuation = .False.
      End If
      If (associated(current,tail)) Exit
      If (.Not. associated(current)) Exit
      current => current%next
    End Do

    Close (8)
    Close (9)
  End Do

  Stop


Contains


  Subroutine do_loop_fixup(start, lab)

    ! Convert DO-loops from:    DO xxx i=1,n    To:   DO i=1,n
    ! xxx CONTINUE              END DO

    ! `start' points to the first line of the DO loop
    ! `lab' is the label

    Type (code), Pointer :: start
    Character (Len=*), Intent (In) :: lab

    ! Local variables

    Type (code), Pointer :: current, end_loop
    Integer :: i, j, level, nmult, nl_length
    Logical :: continued, jump_from_inner, referenced
    Character (Len=5) :: label(20), next_label, text
    Character (Len=10) :: loop_name

    ! -------------------------------------------------------------------
    ! PASS 1. Analysis
    ! Find end of loop (end_loop)
    ! Test for multiple loops using same label
    ! Test for jumps to end of this loop from this DO loop (referenced)
    ! or from inner loops (jump_from_inner)
    ! Find if label is on a statement other than CONTINUE
    ! Find if next executable line beyond loop is labelled (for EXIT)

    current => start%next
    nmult = 1
    level = 0
    jump_from_inner = .False.
    referenced = .False.
    Do
      If (current%label==lab) Then
        continued = (index(current%text,'CONTINUE')>0)
        Exit
      End If

      ! Check for nested DO loop or multiple use of current loop

      If (current%text(1:1)=='!' .Or. current%text(1:1)==' ') Go To 100
      i = index(current%text, 'DO ')
      If (i>0 .And. index(current%text,'END DO')==0) Then
        text = adjustl(current%text(i+3:))
        If (scan(text(1:1),numbers)>0) Then
          If (text(:lab_length)==lab) Then
            nmult = nmult + 1
          Else
            level = level + 1
            i = scan(text, ' ,')
            If (i>0) text = text(:i-1)
            label(level) = text
          End If
        End If
      End If

      ! Check for end of nested loop

      If (current%label/='     ' .And. level>0) Then
        Do
          If (current%label==label(level)) Then
            level = level - 1
            If (level<=0) Exit
          Else
            Exit
          End If
        End Do
      End If

      ! Test for GO TO current loop label

      i = index(current%text, 'GO TO')
      If (i>0) Then
        text = adjustl(current%text(i+5:))
        If (text(:lab_length)==lab) Then
          If (level>0) Then
            jump_from_inner = .True.
          Else
            referenced = .True.
          End If
        End If
      End If

      ! Get next line

     100   If (.Not. associated(current)) Return
      current => current%next
    End Do

    end_loop => current

    ! Find label of next executable line.
    ! First advance past any continuation lines after the end of the DO loop.

    next_label = ' '
    Do
      If (last_char(current%text)/='&') Exit
      If (.Not. associated(current)) Go To 110
      current => current%next
    End Do

    Do
      current => current%next
      If (current%text(1:1)/='!') Exit
      If (.Not. associated(current)) Go To 110
    End Do
    next_label = current%label
    nl_length = len_trim(next_label)

    ! -------------------------------------------------------------------
    ! PASS 2. Transform beginning & end of loop

   110 current => start

    ! Remove label from DO line
    ! There may be a comma after the label, if so, remove it.

    i = index(current%text, lab(:lab_length))
    current%text = current%text(:i-1) // current%text(i+lab_length:)
    length = len_trim(current%text)
    Do j = i, length
      If (current%text(j:j)==' ') Cycle
      If (current%text(j:j)==',') current%text(j:j) = ' '
      Exit
    End Do

    ! Jump out of inner loop detected, set up DO construct.

    If (jump_from_inner) Then
      loop_name = 'loop' // lab
      current%text = trim(loop_name) // ':  ' // current%text
      current%label = ' '
    End If

    ! Insert `END DO' at end of loop

    current => end_loop
    If (continued) Then
      current%text = 'END DO'
      current%label = ' '
    Else
      If (.Not. referenced) Then
        current%label = ' '
        i = index(current%text, lab(:lab_length))
        If (i>0) current%text = adjustl(current%text(i+lab_length:))
      End If
      ! If there are continuation lines, advance to last one
      Do
        If (last_char(current%text)=='&') Then
          current => current%next
        Else
          Exit
        End If
      End Do
      Call insert_and_moveto_newline(current)
      end_loop => current
      current%text = 'END DO'
    End If
    If (jump_from_inner) current%text = trim(current%text) // ' ' // loop_name

    ! Insert multiple CONTINUE's if necessary

    If (nmult>1) Then
      Call insert_and_moveto_newline(current)
      end_loop => current
      current%text = lab // ' CONTINUE'
      current%label = lab
    End If

    ! -------------------------------------------------------------------
    ! PASS 3. Change GO TOs to CYCLE or EXIT where appropriate

    current => start%next
    If (continued) Then
      Do
        If (current%text(1:1)=='!' .Or. current%text(1:1)==' ') Go To 120
        i = index(current%text, 'GO TO')
        If (i>0) Then
          text = adjustl(current%text(i+5:))
          If (text(:5)==lab) Then
            current%text(i:) = 'CYCLE'
            If (jump_from_inner) current%text = trim(current%text) // ' ' // &
              loop_name
          Else If (nl_length>0 .And. text(:nl_length)==next_label) Then
            current%text(i:) = 'EXIT'
            If (jump_from_inner) current%text = trim(current%text) // ' ' // &
              loop_name
          End If
        End If

        ! Get next line

     120     current => current%next
        If (associated(current,end_loop)) Exit
        If (.Not. associated(current)) Exit
      End Do
    End If

    Return
  End Subroutine do_loop_fixup


  Subroutine fix_3way_if(start)
    ! Convert 3-way IFs to IF () THEN .. ELSE IF () THEN .. ELSE

    Type (code), Pointer :: start

    ! Local variables

    Type (code), Pointer :: current
    Integer :: pos1, count, length, pos2, i, lab1, lab2, lab3, lenq, &
      next_label, lenz
    Character (Len=1) :: ch
    Character (Len=128) :: quantity
    Character (Len=3) :: zero_txt

    current => start
    length = len_trim(current%text)

    ! Find closing bracket to match the opening bracket.
    ! Only cases with the closing bracket on the same line are converted.

    pos1 = index(current%text, 'IF')

    ! Check that next non-blank character after 'IF' is '('.
    i = pos1 + 2
    Do
      ch = current%text(i:i)
      If (ch/=' ') Exit
      i = i + 1
      If (i>length) Return
    End Do
    If (ch/='(') Return

    pos1 = i
    count = 1
    pos2 = pos1 + 1
    Do
      i = scan(current%text(pos2:length), '()')
      If (i==0) Return
      pos2 = i + pos2 - 1
      If (current%text(pos2:pos2)=='(') Then
        count = count + 1
      Else
        count = count - 1
      End If
      If (count==0) Exit
      pos2 = pos2 + 1
    End Do

    ! See if there are 3 labels after the closing bracket.

    Read (current%text(pos2+1:), *, Err=100) lab1, lab2, lab3

    ! As it is probably very old code, the first alphabetic character in the
    ! expression should tell us whether the quantity is REAL or INTEGER.

    Do i = pos1 + 1, pos2 - 1
      ch = current%text(i:i)
      If (ch>='i' .And. ch<='n') Then
        zero_txt = '0'
        lenz = 1
        Exit
      Else If (ch>='a' .And. ch<='z') Then
        zero_txt = '0.0'
        lenz = 3
        Exit
      Else If (i==pos2-1) Then
        Return
      End If
    End Do

    quantity = current%text(pos1:pos2)
    lenq = len_trim(quantity)

    ! Find the next executable line to see if it is labelled.
    next_label = 0
    Do
      If (.Not. associated(current)) Exit
      current => current%next
      If (current%text(1:1)=='!' .Or. len_trim(current%text)==0) Cycle
      If (len_trim(current%label)>0) Read (current%label, *) next_label
      Exit
    End Do
    current => start

    If (lab1==lab2) Then
      current%text = current%text(:pos2-1) // ' > ' // zero_txt(:lenz) // &
        ') THEN'
      Call insert_and_moveto_newline(current)
      current%text = ' '
      Write (current%text, '(a, i5)') 'GO TO ', lab3
      If (lab1/=next_label) Then
        Call insert_and_moveto_newline(current)
        current%text = 'ELSE'
        Call insert_and_moveto_newline(current)
        current%text = ' '
        Write (current%text, '(a, i5)') 'GO TO ', lab1
      End If
      Call insert_and_moveto_newline(current)
      current%text = 'END IF'

    Else If (lab2==lab3) Then
      current%text = current%text(:pos2-1) // ' < ' // zero_txt(:lenz) // &
        ') THEN'
      Call insert_and_moveto_newline(current)
      current%text = ' '
      Write (current%text, '(a, i5)') 'GO TO ', lab1
      If (lab2/=next_label) Then
        Call insert_and_moveto_newline(current)
        current%text = 'ELSE'
        Call insert_and_moveto_newline(current)
        current%text = ' '
        Write (current%text, '(a, i5)') 'GO TO ', lab2
      End If
      Call insert_and_moveto_newline(current)
      current%text = 'END IF'

    Else If (lab1==lab3) Then
      current%text = current%text(:pos2-1) // ' == ' // zero_txt(:lenz) // &
        ') THEN'
      Call insert_and_moveto_newline(current)
      current%text = ' '
      Write (current%text, '(a, i5)') 'GO TO ', lab2
      If (lab1/=next_label) Then
        Call insert_and_moveto_newline(current)
        current%text = 'ELSE'
        Call insert_and_moveto_newline(current)
        current%text = ' '
        Write (current%text, '(a, i5)') 'GO TO ', lab1
      End If
      Call insert_and_moveto_newline(current)
      current%text = 'END IF'

    Else
      current%text = current%text(:pos2-1) // ' < ' // zero_txt(:lenz) // &
        ') THEN'
      Call insert_and_moveto_newline(current)
      current%text = ' '
      Write (current%text, '(a, i5)') 'GO TO ', lab1
      Call insert_and_moveto_newline(current)
      current%text = 'ELSE IF ' // quantity(1:lenq-1) // ' == ' // &
        zero_txt(:lenz) // ') THEN'
      Call insert_and_moveto_newline(current)
      current%text = ' '
      Write (current%text, '(a, i5)') 'GO TO ', lab2
      If (lab3/=next_label) Then
        Call insert_and_moveto_newline(current)
        current%text = 'ELSE'
        Call insert_and_moveto_newline(current)
        current%text = ' '
        Write (current%text, '(a, i5)') 'GO TO ', lab3
      End If
      Call insert_and_moveto_newline(current)
      current%text = 'END IF'

    End If

   100 Return
  End Subroutine fix_3way_if


  Subroutine insert_and_moveto_newline(current)
    ! Insert a new line AFTER the current line, and move `current' to point to
    ! it.

    Type (code), Pointer :: current

    ! Local variable
    Type (code), Pointer :: new_line

    Allocate (new_line)
    new_line%next => current%next
    current%next => new_line
    current => new_line

    Return
  End Subroutine insert_and_moveto_newline


  Subroutine find_declarations(start, tail, first_decl, last_decl)
    ! Find the first & last declaration lines in a program unit.

    Type (code), Pointer :: start, tail
    Type (code), Pointer :: first_decl, last_decl

    ! Local variables
    Character (Len=9), Parameter :: declaration(13) = [ 'IMPLICIT ', &
      'INTEGER  ', 'REAL     ', 'DOUBLE   ', 'LOGICAL  ', 'COMPLEX  ', &
      'DIMENSION', 'EXTERNAL ', 'DATA     ', 'COMMON   ', 'PARAMETER', &
      'SAVE     ', 'CHARACTER' ]
    Type (code), Pointer :: current
    Integer :: pos, length, i

    Nullify (first_decl, last_decl)

    ! Search for first declaration
    current => start%next
    search1: Do
      If (current%text(1:1)/='!' .And. current%text(1:1)/=' ') Then
        pos = scan(current%text(1:13), delimiters)
        If (pos>0) Then
          length = min(9, pos-1)
          If (length>=4) Then
            Do i = 1, 13
              If (current%text(:length)==declaration(i)(:length)) Then
                first_decl => current
                Exit search1
              End If
            End Do
          End If
        End If
      End If

      current => current%next
      If (associated(current,tail)) Return
    End Do search1

    ! Search for last declaration

    last_decl => first_decl
    Do
      If (current%text(1:1)/='!' .And. current%text(1:1)/=' ') Then
        pos = index(current%text, '=')
        If (pos>0) Then
          If (pos<12) Return
          If (current%text(1:9)/='PARAMETER' .And. current%text(1:9)/= &
            'CHARACTER') Return
        End If

        If (current%text(1:4)=='CALL') Return

        If (current%text(1:2)=='IF') Then
          If (current%text(3:3)==' ') Return
          If (current%text(3:3)=='(') Return
        End If

        If (current%text(1:3)=='DO ') Return

        ! Skip continuation lines

        Do
          If (last_char(current%text)/='&') Exit
          current => current%next
        End Do

        last_decl => current
      End If

      current => current%next
      If (associated(current,tail)) Return
    End Do

    Return
  End Subroutine find_declarations


  Subroutine get_arg_list()
    ! Extract list of dummy arguments

    ! Local variables
    Integer :: pos, last

    current => start_prog_unit
    numb_arg = 0
    Do                             ! Find '(' if there are any arguments
      pos = index(current%text, '(')
      If (pos==0) Then
        If (last_char(current%text)/='&') Return
        current => current%next
      Else
        Exit
      End If
    End Do
    pos = pos + 1

    Nullify (arg_start)
    Allocate (arg_start)
    first_arg = .True.
    Do                             ! Loop through lines of arguments
      last = scan(current%text(pos:), ',)')
      If (last==0) Then
        If (last_char(current%text)/='&') Exit
        current => current%next
        pos = 1
      Else
        last = last + pos - 1
        Nullify (arg)
        Allocate (arg)
        If (first_arg) Then
          If (len_trim(current%text(pos:last-1))==0) Exit
          arg_start => arg
          first_arg = .False.
          Nullify (last_arg)
          Allocate (last_arg)
        Else
          last_arg%next => arg
        End If
        numb_arg = numb_arg + 1
        last_arg => arg

        arg%name = adjustl(current%text(pos:last-1))
        arg%intention = 0
        arg%var_type = ' '
        arg%dim = 0
        pos = last + 1
      End If
    End Do

    Return
  End Subroutine get_arg_list


  Subroutine get_var_types()
    ! Search thru the declarations for the types of dummy arguments

    current => first_decl
    Do
      text = current%text(:30)
      If (text(:4)=='REAL' .Or. text(:7)=='INTEGER' .Or. &
        text(:6)=='DOUBLE' .Or. text(:9)=='CHARACTER' .Or. &
        text(:7)=='LOGICAL' .Or. text(:7)=='COMPLEX') Then
        ! Copy the variable type to vtype
        last = index(text, ' ::') - 1
        If (last<0) Then
          last = index(text, '*')
          If (last==0) Then
            last = 24
          Else
            last = index(text(last+2:), ' ') + last
          End If
          i1 = last + 2
        Else
          i1 = last + 4
        End If
        vtype = text(:last)
        Call extract_declarations(i1)

      Else If (text(:9)=='DIMENSION') Then
        i1 = 11
        vtype = ' '
        Call extract_declarations(i1)
      End If

      If (associated(current,last_decl)) Exit
      current => current%next
    End Do

    ! If there are any arguments for which the type has not been determined,
    ! use the implicit types

    arg => arg_start
    Do
      If (arg%var_type==' ') arg%var_type = implicit_type(arg%name(1:1))
      If (associated(arg,last_arg)) Exit
      arg => arg%next
    End Do

    Return
  End Subroutine get_var_types


  Subroutine get_intents()
    ! Search thru the body of the current program unit to try to determine
    ! the intents of dummy arguments.

    Character (Len=80) :: last_part
    Integer :: j, nbrac

    Do
      If (current%text(1:1)/='!' .And. current%text(1:1)/=' ') Then
        statement = current%text
        If (statement(1:3)=='IF ' .Or. statement(1:3)=='IF(') Then
          ! Split line into two parts
          ! IF (condition) | last_part
          i = index(statement, '(')
          length = len_trim(statement)
          nbrac = 1
          Do j = i + 1, length - 1
            If (statement(j:j)==')') Then
              nbrac = nbrac - 1
              If (nbrac==0) Exit
            Else If (statement(j:j)=='(') Then
              nbrac = nbrac + 1
            End If
          End Do
          If (j<length) Then
            last_part = statement(j+1:)
          Else
            last_part = ' '
          End If
          statement = statement(:j)
          ! It is assumed that a variable inside
          ! an IF-expression cannot be altered
          arg => arg_start
          Do
            i = find_delimited_name(statement, arg%name)
            If (i>0) Then
              If (arg%intention==0) arg%intention = 1
            End If
            If (associated(arg,last_arg)) Exit
            arg => arg%next
          End Do
          statement = last_part
        End If

        pos = index(statement, '=', back=.True.)
        If (pos>0) Then
          If (statement(pos-1:pos-1)/='=' .And. statement(pos-1:pos-1)/='/' &
            .And. statement(pos-1:pos-1)/='<' .And. statement(pos-1:pos-1)/= &
            '>') Then

            ! Look for each argument name;
            ! is it before or after '='?
            arg => arg_start
            Do
              i = find_delimited_name(statement, arg%name)
              If (i>0) Then
                If (i<pos) Then
                  arg%intention = ior(arg%intention, 2)
                Else
                  If (arg%intention==0) arg%intention = 1
                End If
              End If
              If (associated(arg,last_arg)) Exit
              arg => arg%next
            End Do
          End If
        End If
      End If

      If (associated(current,end_prog_unit)) Exit
      current => current%next
    End Do

    Return
  End Subroutine get_intents


  Subroutine goto_cases(text)
    ! Inserts pairs:
    ! CASE (i)
    ! GO TO i-th label
    ! Terminated with:
    ! END SELECT

    Character (Len=*), Intent (Inout) :: text

    Integer :: case_number, pos, i2

    case_number = 1

    Do
      pos = index(text, ',')
      If (pos>0) Then
        i2 = pos - 1
      Else
        i2 = len_trim(text)
      End If
      Call insert_and_moveto_newline(current)
      Write (current%text, '("  CASE (", i5, ")")') case_number
      Call insert_and_moveto_newline(current)
      current%text = '    GO TO ' // trim(text(:i2))
      If (pos==0) Exit
      text = text(pos+1:)
      case_number = case_number + 1
    End Do

    Call insert_and_moveto_newline(current)
    current%text = 'END SELECT'

    Return
  End Subroutine goto_cases


  Subroutine extract_declarations(start_pos)
    ! Take the current line, and any continuations, look for dummy variables,
    ! and remove them, after storing any relevant type & dimension info.

    Integer, Intent (In) :: start_pos

    ! Local variables

    Integer :: i, i1, j, ndim
    Character (Len=70) :: text

    i1 = start_pos
    Do
      i = scan(current%text(i1:), '(,') ! Find next ( or ,
      ndim = 0
      If (i==0) Then               ! No comma or ( on this line
        If (last_char(current%text)=='&') Then
          current => current%next
          i1 = 1
          Cycle
        Else
          text = adjustl(current%text(i1:))
          ! Just in case there is an in-line
          pos = index(text, '!')   ! comment (though illegal in F77)
          If (pos>0) text = text(:pos-1)

          If (len_trim(text)==0) Return
          pos = len_trim(current%text)
        End If
      Else
        pos = i + i1 - 1
        If (current%text(pos:pos)==',') Then ! Comma found
          text = current%text(i1:pos-1)
        Else                       ! ( found; find matching )
          count = 1
          ndim = 1
          pos = pos + 1
          Do
            j = scan(current%text(pos:), '(,)')
            If (j==0) Then         ! No bracket or comma
              If (last_char(current%text)=='&') Then
                length = len_trim(current%text)
                next_line => current%next
                current%text = trim(current%text(:length-1)) // ' ' // &
                  adjustl(next_line%text)
                If (associated(next_line,last_decl)) last_decl => current
                current%next => next_line%next
                Cycle
              Else
                Return
              End If
            End If

            pos = pos + j - 1
            Select Case (current%text(pos:pos))
            Case ('(')
              count = count + 1
            Case (')')
              count = count - 1
              If (count<=0) Then
                text = current%text(i1:pos)
                Exit
              End If
            Case (',')
              ndim = ndim + 1
            End Select
            pos = pos + 1
          End Do                   ! End matching ) search
        End If
      End If

      ! Variable name isolated, with ndim dimensions
      ! Now see if it matches a dummy argument

      arg => arg_start
      text = adjustl(text)
      If (ndim<=0) Then
        length = len_trim(text)
      Else
        length = index(text, '(') - 1
      End If
      Do
        If (text(:length)==arg%name) Then ! Argument matched
          ! Insert variable type
          If (arg%var_type==' ') arg%var_type = vtype
          If (ndim>arg%dim) Then
            arg%dim = ndim
            i = index(text, '(')
            arg%dimensions = text(i:)
          End If
          ! Remove variable ( & comma)
          text = adjustl(current%text(pos+1:))
          If (len_trim(text)==0) Then
            If (i1>1) Then
              current%text(i1-1:) = ' '
            Else
              current%text = ' '
            End If
            If (i1==start_pos) current%text = ' '
            Return
          Else
            If (text(1:1)==',') text = adjustl(text(2:))
            If (text(1:1)=='&') Then
              next_line => current%next
              If (i1==start_pos) Then
                current%text = current%text(:i1-1) // ' ' // &
                  adjustl(next_line%text)
                If (associated(next_line,last_decl)) last_decl => current
                current%next => next_line%next
              Else
                current%text = current%text(:i1-1) // '  &'
                current => next_line
                i1 = 1
              End If
            Else
              current%text = current%text(:i1-1) // ' ' // text
            End If
          End If
          Exit
        End If

        If (associated(arg,last_arg)) Then
          i1 = pos + 1             ! Skip over comma, if present
          Exit
        End If
        arg => arg%next
      End Do
    End Do

    Return
  End Subroutine extract_declarations


  Subroutine convert_parameter(start)

    ! Convert PARAMETER statements from:
    ! PARAMETER (name1 = value1, name2 = value2, ... )
    ! to:
    ! TYPE1, PARAMETER :: name1 = value1
    ! TYPE2, PARAMETER :: name2 = value2

    Type (code), Pointer :: start

    ! Local variables

    Type (code), Pointer :: current, next_line
    Integer :: count, i, j, length, pos
    Character (Len=10) :: text
    Character (Len=30) :: vtype

    current => start

    ! Replace opening ( with ::

    i = index(current%text, '(')
    If (i==0) Return
    current%text = trim(current%text(:i-1)) // ' :: ' // &
      adjustl(current%text(i+1:))
    i = index(current%text, '::') + 3
    Do
      j = index(current%text(i:), '=')
      If (j==0) Then
        If (last_char(current%text)/='&') Return
        next_line => current%next
        j = len_trim(current%text)
        current%text = trim(current%text(:j-1)) // next_line%text
        current%next => next_line%next
        j = index(current%text(i:), '=')
        If (j==0) Return
      End If
      j = i + j - 1
      text = adjustl(current%text(i:j-1))
      Call find_type(text, vtype, first_decl, start)

      current%text = trim(vtype) // ', ' // current%text
      j = j + 2 + len_trim(vtype)

      ! Is there another value set in this statement?
      ! Find end of the expression for the value, which may involve brackets
      ! and commas.

     100   length = len_trim(current%text)
      count = 0
      Do i = j + 1, length
        Select Case (current%text(i:i))
        Case ('(')
          count = count + 1
        Case (')')
          count = count - 1
          If (count<0) Then
            ! Remove final ) and return
            current%text = current%text(:i-1)
            Return
          End If
        Case (',')
          ! If count = 0, there is another declaration
          If (count==0) Then
            ! Break line, check for '&' as first character
            text = adjustl(current%text(i+1:))
            If (text(1:1)=='&') Then
              current%text = current%text(:i-1)
              current => current%next
              current%text = 'PARAMETER :: ' // adjustl(current%text)
            Else
              Allocate (next_line)
              next_line%next => current%next
              current%next => next_line
              next_line%text = 'PARAMETER :: ' // adjustl(current%text(i+1:))
              current%text = current%text(:i-1)
              If (associated(current,last_decl)) last_decl => next_line
              current => next_line
              start => start%next
            End If
            Exit
          End If
        Case ('&')
          ! Expression continued on next line, merge lines
          next_line => current%next
          pos = len_trim(current%text(:i-1))
          current%text = current%text(:pos) // next_line%text
          current%next => next_line%next
          Go To 100
        End Select
      End Do

      If (i>length) Exit
      i = 14
    End Do

    Return
  End Subroutine convert_parameter


  Subroutine find_type(vname, vtype, first_decl, last_decl)

    ! Find the type of variable 'vname'

    Character (Len=*), Intent (In) :: vname
    Character (Len=*), Intent (Out) :: vtype
    Type (code), Pointer :: first_decl, last_decl

    ! Local variables

    Type (code), Pointer :: current
    Character (Len=30) :: text
    Integer :: i1, last, length, pos

    current => first_decl
    length = len_trim(vname)
    If (length==0) Return
    Do
      text = current%text(:30)
      If (text(:4)=='REAL' .Or. text(:7)=='INTEGER' .Or. &
        text(:6)=='DOUBLE' .Or. text(:9)=='CHARACTER' .Or. &
        text(:7)=='LOGICAL' .Or. text(:7)=='COMPLEX') Then
        ! Copy the variable type to vtype
        last = index(text, ' ::') - 1
        If (last<0) Then
          last = index(text, '*')
          If (last==0) Then
            last = 24
          Else
            last = index(text(last+2:), ' ') + last
          End If
          i1 = last + 2
        Else
          i1 = last + 4
        End If
        vtype = text(:last)

        ! See if variable is declared on this line (& any continuation)

        Do
          pos = find_delimited_name(current%text(i1:), vname(:length))
          If (pos==0) Then
            If (last_char(current%text)=='&') Then
              current => current%next
              i1 = 1
              Cycle
            End If
          End If
          Exit
        End Do

        ! Variable name found if pos > 0.

        If (pos>0) Then            ! Remove variable name
          pos = pos + i1 - 1
          current%text = current%text(:pos-1) // current%text(pos+length:)
          ! Delete line if only TYPE :: remains
          If (last_char(current%text)==':') Then
            current%text = ' '
            Return
          End If
          ! Remove any following comma
          i = pos
          length = len_trim(current%text)
          Do
            If (i>length) Then
              Return
            Else If (current%text(i:i)==',') Then
              current%text = current%text(:i-1) // current%text(i+1:)
              Return
            Else If (current%text(i:i)/=' ') Then
              Return
            End If
            i = i + 1
          End Do
        End If

      End If

      ! If last declaration has been reached, return default type.
      ! Otherwise proceed to next line.

      If (associated(current,last_decl)) Then
        vtype = implicit_type(vname(1:1))
        Exit
      Else
        current => current%next
      End If
    End Do

    Return
  End Subroutine find_type

End Program to_f90
