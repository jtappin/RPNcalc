! Copyright (C) 2011
! James Tappin

! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.

! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.

module handlers
  ! Event handlers.

  use iso_c_binding
  use gtk, only: gtk_statusbar_push, gtk_main_quit, gtk_entry_get_text_length, &
       & gtk_entry_get_text, gtk_editable_get_chars, gtk_editable_insert_text, &
       & gtk_editable_get_chars, gtk_editable_delete_text, &
       & gtk_about_dialog_new, gtk_about_dialog_set_program_name, &
       & gtk_about_dialog_set_license, gtk_about_dialog_set_comments, &
       & gtk_about_dialog_set_authors, gtk_main,&
       & gtk_window_set_transient_for, gtk_about_dialog_set_website

  use g, only: g_object_set_property, g_signal_stop_emission_by_name

  use gtk_sup
  use gtk_hl
  use widgets
  use utils

  implicit none

  ! Constants (these are used both in main and some handlers)
  character, parameter :: linefeed=char(10)
  real(kind=c_double), parameter :: pi = 3.1415926535897932384626433832795029_c_double
  real(kind=c_double), parameter :: dtor = pi/180._c_double

contains

  subroutine my_destroy (widget, gdata) bind(c)
    ! Destroy the heirarchy.

    use iso_c_binding, only: c_ptr
    type(c_ptr), value :: widget, gdata
    call gtk_widget_destroy(win)
    call gtk_main_quit ()
  end subroutine my_destroy

  subroutine enter_value(widget, gdata) bind(c)
    ! Enter key or <Enter> in the input window, transfer the value to the stack
    type(c_ptr), value :: widget, gdata

    real(kind=c_double) :: val
    logical :: status
    integer(kind=c_int) :: mid

    mid = gtk_statusbar_push(fstatus, 0, cnull)
    call read_entry(val, status, push=.true.)
    if (.not. status) return
    call gtk_entry_set_text(fentry, cnull)
    call set_result(val)
  end subroutine enter_value

  subroutine char_entered(widget, ctext, nchars, ppos, gdata) bind(c)
    ! A few basic sanity checks on numeric entry
    type(c_ptr), value :: widget, ctext, ppos, gdata
    integer(kind=c_int), value :: nchars

    character(len=40) :: itext, etext, otext, wtext
    integer(kind=c_int) :: nentry
    type(c_ptr) :: cetext
    integer(kind=c_int), pointer :: ipos
    integer :: i, j, iloc
    integer(kind=c_int) :: mid
    logical :: dflag, eflag

    call convert_c_string(ctext, nchars, itext)
    call c_f_pointer(ppos, ipos)
    if (ipos == 0) itext=adjustl(itext)

    nentry = gtk_entry_get_text_length(widget)
    cetext = gtk_entry_get_text(widget)
    call convert_c_string(cetext, nentry, etext)

    eflag=.false.
    dflag=.false.

    j=1
    do i = 1, nchars
       select case(itext(i:i))
       case('0':'9') ! A number, always OK
          otext(j:j) = itext(i:i)
          j = j+1

       case('.') ! A decimal point (OK if there's not already one present
          ! and it's not after an exponent
          if (decimal_present) cycle
          if (exponent_present) then
             if (i > 1) then
                if (scan(itext(:i-1), "EeDd") > 0) cycle
             end if
             if (ipos > 0) then
                if (scan(itext(:ipos),"EeDd") > 0) cycle
             end if
          end if
          otext(j:j) = itext(i:i)
          j = j+1
          dflag = .true.

       case("E","D","e","d") ! An exponent (OK if there isn't already one
          ! and it's not before a decimal
          if (exponent_present) cycle
          if (decimal_present) then
             if (i < len_trim(itext)) then
                if (index(itext(i+1:),'.') > 0) cycle
             end if
             if (ipos < nchars-1) then
                if (index(etext(ipos+1:),'.') > 0) cycle
             end if
          end if
          otext(j:j) = itext(i:i)
          j = j+1
          eflag = .true.

       case("+","-") ! A sign (OK at the start or immediately after
          ! an exponent
          if (i > 1) then
             if (scan(itext(i-1:i-1), "EeDd") == 0) cycle
          else if (ipos == 0) then
             if (scan(etext(1:1), "+-") /= 0) cycle
          end if
          if (ipos > 0) then
             if (scan(etext(ipos:ipos), "EeDd") == 0) cycle
             if (scan(etext(ipos+1:), "+-") /= 0) cycle
          end if
          otext(j:j) = itext(i:i)
          j = j+1

       case default  ! Anything else is invalid
       end select
    end do

    if (j <= nchars) then ! We had to exclude some chars otherwise let the
       ! default handler do it.
       mid = gtk_statusbar_push(fstatus, 0, &
            & "Entered text includes invalid characters -- excluded"//cnull)
       if (j > 1) then
          if (ipos > 0) then
             wtext = etext(:ipos)//otext(:j-1)
          else
             wtext = otext(:j-1)
          endif
          if (ipos < nchars-1) then
             wtext = trim(wtext)//trim(etext(ipos+1:))
          end if
          call gtk_entry_set_text(widget, trim(wtext)//cnull)
       endif
       if (dflag) decimal_present = .true.
       if (eflag) exponent_present = .true.

       call g_signal_stop_emission_by_name(widget, "insert-text")
    else
       if (dflag) decimal_present = .true.
       if (eflag) exponent_present = .true.
    end if
  end subroutine char_entered

  subroutine char_deleted(widget, istart, iend, gdata) bind(c)
    ! When characters are deleted, check what's deleted and clear
    ! flags if needed
    type(c_ptr), value :: widget, gdata
    integer(kind=c_int), value :: istart, iend

    type(c_ptr) :: cetext
    integer(kind=c_int) :: nchars
    character(len=40) :: etext
    integer :: iloc, ilocs

    nchars = gtk_entry_get_text_length(widget)
    cetext = gtk_entry_get_text(widget)

    if (iend < 0) iend = nchars-1
    call convert_c_string(cetext, nchars, etext)

    ! Was there an exponent marker in the deleted segment?
    iloc = scan(etext(istart+1:iend+1), "EeDd")
    if (iloc > 0) exponent_present = .false.

    iloc = index(etext(istart+1:iend+1), '.')
    if (iloc > 0) decimal_present = .false.

  end subroutine char_deleted

  subroutine numpress(widget, gdata) bind(c)
    ! Keypad number entry -- gdata will contain the number
    type(c_ptr), value :: widget, gdata

    character, pointer :: fdata
    integer(kind=c_int) :: mid

    mid = gtk_statusbar_push(fstatus, 0, cnull)

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       call append_char_entry(fdata//cnull)
    end if
  end subroutine numpress

  subroutine dppress(widget, gdata) bind(c)
    ! Keypad decimal point
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: mid

    if (decimal_present .or. exponent_present) then
       mid = gtk_statusbar_push(fstatus, 0, &
            & "Decimal point not permitted here"//cnull)
    else
       call append_char_entry("."//cnull)
       decimal_present = .TRUE.
    end if
  end subroutine dppress

  subroutine chspress(widget, gdata) bind(c)
    ! The change sign key (+/-)
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: ctext
    character :: ftext
    integer(kind=c_int), target :: pos
    character(len=40) :: alltext
    integer(kind=c_int) :: nchars, mid
    integer :: idx
    real(kind=c_double) :: x
    logical :: status

    mid = gtk_statusbar_push(fstatus, 0, cnull)
    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (exponent_present) then
       ctext = gtk_entry_get_text(fentry)
       call convert_c_string(ctext, nchars, alltext)
       idx = max(index(alltext, 'E'), index(alltext,'e'), &
            & index(alltext, 'D'), index(alltext, 'd'))
       if (idx == 0) then
          mid = gtk_statusbar_push(fstatus, 0, "D'Oh"//cnull)
       else
          idx=idx+1
          select case(alltext(idx:idx))
          case ('+')
             alltext(idx:idx) = '-'
          case('-')
             alltext = alltext(:idx-1)//alltext(idx+1:)
          case default
             alltext = alltext(:idx-1)//'-'//alltext(idx:)
          end select
          call gtk_entry_set_text(fentry, trim(alltext)//cnull)
       end if
    else if (nchars > 0) then
       ctext = gtk_editable_get_chars(fentry, 0, 1)
       call convert_c_string(ctext, 1, ftext)
       pos = 0
       select case(ftext)
       case ('+')
          call gtk_editable_delete_text(fentry, 0, 1)
          call gtk_editable_insert_text(fentry, '-'//cnull, 1, c_loc(pos))
       case ('-')
          call gtk_editable_delete_text(fentry, 0, 1)
       case default  ! no sign present
          call gtk_editable_insert_text(fentry, '-'//cnull, 1, c_loc(pos))
       end select
    else
       call set_result()
       call pop_stack(x, status)
       if (.not. status) return
       x = -x
       call push_stack(x)
    end if
  end subroutine chspress

  subroutine eepress(widget, gdata) bind(c)
    ! Exponent entry key (EE)
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: mid, nchars
    integer(kind=c_int), target :: pos

    if (exponent_present) then
       mid = gtk_statusbar_push(fstatus, 0, "Exponent already present"//cnull)
    else
       nchars = int(gtk_entry_get_text_length(fentry), c_int)
       if (nchars == 0) then
          mid = gtk_statusbar_push(fstatus, 0, &
               & "Must have a mantissa before an exponent."//cnull)
       else
          pos = nchars
          call gtk_editable_insert_text(fentry, 'E'//cnull, 1, c_loc(pos))
          exponent_present = .true.
       end if
    end if
  end subroutine eepress

  subroutine pipress(widget, gdata) bind(c)
    ! The PI key
    type(c_ptr), value :: widget, gdata

    call push_stack(pi)
    call gtk_entry_set_text(fentry, cnull)
  end subroutine pipress

  subroutine delpress(widget, gdata) bind(c)
    ! The del key -- delete the last character in the entry window
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: nchars
    character :: last
    type(c_ptr) :: clast

    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars == 0) return

    clast = gtk_editable_get_chars(fentry, nchars-1, nchars)
    call convert_c_string(clast, 1, last)
    call gtk_editable_delete_text(fentry, nchars-1, nchars)

    select case(last)
    case('.')
       decimal_present = .false.
    case('E','e','D','d')
       exponent_present = .false.
    end select

  end subroutine delpress

  subroutine duppress(widget, gdata) bind(c)
    ! The dup key. If there is a value in the entry window, copy it to the
    ! stack; if not, duplicate the top entry on the stack.
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: nchars, mid
    real(kind=c_double) :: val
    logical :: status

    mid = gtk_statusbar_push(fstatus, 0, cnull)
    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars == 0) then
       call pop_stack(val, status, readonly=.TRUE.)
       if (.not. status) return
       call push_stack(val)
    else
       call read_entry(val, status, push=.true.)
       if (.not. status) return
    end if
    call set_result(val)
  end subroutine duppress

  subroutine oppress(widget, gdata) bind(c)
    ! One of the operators (including ATAN2).

    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: opcode
    integer(kind=c_int) :: mid, nchars
    real(kind=c_double) :: x, y, z
    logical :: status

    call set_result()
    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars > 0) then  ! Have a value in the entry window (will be X)
       call read_entry(x, status, push=.false.)
       if (.not. status) return

       call pop_stack(y, status)
       if (.not. status) return

       call gtk_entry_set_text(fentry, cnull)
    else
       call pop_stack(x, status)
       if (.not. status) return
       call pop_stack(y, status)
       if (.not. status) then
          call push_stack(x, show_result=.false.)
          return
       end if
    end if

    call c_f_pointer(gdata, opcode)
    select case(opcode)
    case(OP_PLUS)
       z = y+x
    case (OP_MINUS)
       z = y-x
    case (OP_TIMES)
       z = y*x
    case (OP_DIVIDE)
       z = y/x
    case (OP_POWER)
       if (x == int(x)) then  ! integer power safe for all signs
          z = y**int(x)

       else if (y < 0._c_double) then ! real power of a negative value (not allowed)
          mid = gtk_statusbar_push(fstatus, 0, &
               & "Cannot raise a negative value to a real power"//cnull)
          call push_stack(y, show_result=.false.)
          call push_stack(x, show_result=.false.)
          return
       else
          z = y**x
       end if
    case(FUN_ATAN2)
       z = atan2(y, x)
       select case(trigunit)
       case(1)
          z = z * 180._c_double/pi
       case(2)
          z = z * 200._c_double/pi
       case(0)
       end select
    end select
    call push_stack(z)
    call set_result(z)
    mid = gtk_statusbar_push(fstatus, 0, cnull)
  end subroutine oppress

  subroutine cepress(widget, gdata) bind(c)
    ! Clear entry: Delete the entry window or the top element of the stack
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int16_t) :: nchars
    real(kind=c_double) :: xjunk
    logical :: status
    integer(kind=c_int) :: mid

    nchars = gtk_entry_get_text_length(fentry)
    if (nchars > 0) then
       call gtk_entry_set_text(fentry, cnull)
       call clear_entry_flags
    else
       call pop_stack(xjunk, status)
    end if
    call set_result()
    mid = gtk_statusbar_push(fstatus, 0, cnull)
  end subroutine cepress

  subroutine capress(widget, gdata) bind(c)
    ! Clear all
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int16_t) :: nchars
    integer(kind=c_int) :: mid

    call set_result()

    nchars = gtk_entry_get_text_length(fentry)
    if (nchars > 0) then
       call gtk_entry_set_text(fentry, cnull)
       call clear_entry_flags
    endif

    call hl_gtk_listn_rem(fstack)
    mid = gtk_statusbar_push(fstatus, 0, cnull)
    if (dynamic_stats) call stack_stats(0._c_double, clear=.true.)

  end subroutine capress

  subroutine uppress(widget, gdata) bind(c)
    ! Move the current selection up the stack
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: nchars, mid
    type(c_ptr) :: ctext
    character(len=40) :: ftext
    character(len=80) :: iom
    integer :: ios
    real(kind=c_double) :: x, y
    logical :: status, sflag

    sflag = .true.
    if (stack_selected <= 0) then ! top element or none
       ! (swap with the entry box)
       nchars = int(gtk_entry_get_text_length(fentry), c_int)
       if (nchars == 0) then ! just pop the top of the stack to the box
          call pop_stack(x, status)
          sflag=.false.
          if (.not. status) return
          write(ftext,*) x
          call gtk_entry_set_text(fentry, trim(adjustl(ftext))//cnull)
       else
          call read_entry(x, status, push=.false.)
          if (.not. status) return
          call pop_stack(y, status)
          if (.not. status) return
          call push_stack(x)
          write(ftext,*) y
          call gtk_entry_set_text(fentry, trim(adjustl(ftext))//cnull)
       end if
       call hl_gtk_listn_set_selection(fstack)
    else
       call hl_gtk_listn_swap_rows(fstack, stack_selected, stack_selected-1)
       stack_selected = stack_selected-1
    end if
    if (sflag) then
       call pop_stack(x, status, readonly=.true.)
       if (status) call set_result(x)
    end if
  end subroutine uppress

  subroutine downpress(widget, gdata) bind(c)
    ! Move the current selection down the stack
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: nchars, mid
    type(c_ptr) :: ctext
    character(len=40) :: ftext
    character(len=80) :: iom
    integer :: ios
    real(kind=c_double) :: x, y
    integer(kind=c_int) :: isel, nrows
    logical :: status

    if (stack_selected < 0) then ! no selection
       nchars = int(gtk_entry_get_text_length(fentry), c_int)
       if (nchars == 0) return ! No action of entry is empty

       ctext = gtk_entry_get_text(fentry)
       call convert_c_string(ctext, nchars, ftext)
       read(ftext, *, iostat=ios, iomsg=iom) x
       if (ios /= 0) then
          mid = gtk_statusbar_push(fstatus, 0, trim(iom)//cnull)
          return
       end if
       call pop_stack(y, status)
       if (.not. status) return
       call push_stack(x)
       write(ftext,*) y
       call gtk_entry_set_text(fentry, trim(adjustl(ftext))//cnull)
       call hl_gtk_listn_set_selection(fstack)
    else
       nrows = hl_gtk_listn_get_n_rows(fstack)
       if (nrows <= 1) return
       isel = min(stack_selected, nrows-2)
       call hl_gtk_listn_swap_rows(fstack, isel, isel+1)
       if (isel == stack_selected) then
          stack_selected = stack_selected+1
       else
          stack_selected = stack_selected-1
       end if
    end if
    call pop_stack(x, status, readonly=.true.)
    if (status) call set_result(x)
  end subroutine downpress

  subroutine rollpress(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata

    real(kind=c_double) :: x
    logical :: status

    integer(kind=c_int), dimension(:), allocatable :: idx
    integer :: i
    integer(kind=c_int) :: nrows

    nrows = hl_gtk_listn_get_n_rows(fstack)
    if (nrows <= 1) return  ! Empty or 1 row can't roll

    allocate(idx(nrows))
    idx = (/ (i-1, i=1,nrows) /)
    idx = cshift(idx, -1)
    call hl_gtk_listn_reorder(fstack, idx)

    call pop_stack(x, status, readonly=.true.)
    if (status) call set_result(x)
  end subroutine rollpress

  subroutine funpress(widget, gdata) bind(c)
    ! The 1-argument functions
    type(c_ptr), value :: widget, gdata

    real(kind=c_double) :: x, z, acf
    integer(kind=c_int), pointer :: funcode
    integer(kind=c_int) :: nchars, mid
    logical :: status
    integer :: i

    call set_result()
    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars > 0) then
       call read_entry(x, status, push=.false.)
       if (.not. status) return
       call gtk_entry_set_text(fentry, cnull)
    else
       call pop_stack(x, status)
       if (.not. status) return
    end if

    select case(trigunit)
    case(0)
       acf = 1._c_double
    case(1)
       acf = pi/180._c_double
    case(2)
       acf = pi/200._c_double
    end select

    call c_f_pointer(gdata, funcode)

    select case(funcode)
    case(FUN_SIN)
       if (isinv) then
          if (abs(x) > 1._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Asin argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = asin(x)/acf
       else
          z = sin(x*acf)
       end if
    case(FUN_COS)
       if (isinv) then
          if (abs(x) > 1._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Acos argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = acos(x)/acf
       else
          z = cos(x*acf)
       end if
    case(FUN_TAN)
       if (isinv) then
          z = atan(x)/acf
       else
          z = tan(x*acf)
       end if
    case(FUN_LN)
       if (isinv) then
          z = exp(x)
       else
          if (x <= 0._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Ln argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = log(x)
       end if
    case(FUN_SQRT)
       if (isinv) then
          z = x**2
       else
          if (x <= 0._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Sqrt argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = sqrt(x)
       end if
    case(FUN_SINH)
       if (isinv) then
          z = asinh(x)
       else
          z = sinh(x)
       end if
    case(FUN_COSH)
       if (isinv) then
          if (x < 1._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Acosh argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = acosh(x)
       else
          z = cosh(x)
       end if
    case(FUN_TANH)
       if (isinv) then
          if (abs(x) > 1._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Atanh argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = atanh(x)
       else
          z = tanh(x)
       end if
    case(FUN_LOG10)
       if (isinv) then
          z = 10._c_double ** x
       else
          if (x <= 0._c_double) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Log10 argument out of range"//cnull)
             call push_stack(x, show_result=.false.)
             return
          end if
          z = log10(x)
       end if
    case(FUN_INV)
       if (x == 0._c_double) then
          mid = gtk_statusbar_push(fstatus, 0, &
               & "1/X argument out of range"//cnull)
          call push_stack(x, show_result=.false.)
          return
       end if
       z = 1._c_double / x
    case(FUN_ABS)
       z = abs(x)
    case(FUN_INT)
       z = aint(x)
    case(FUN_FRAC)
       z = x - aint(x)
    case(FUN_FACTORIAL)
       if (x == aint(x) .and. x >= 0) then
          z = 1._c_double
          do i = 2, int(x)
             z = z*real(i, c_double)
          end do
       else
          mid = gtk_statusbar_push(fstatus, 0, &
               & "Factorial argument out of range"//cnull)
          call push_stack(x, show_result=.false.)
          return
       end if
    end select
    call push_stack(z)
    mid = gtk_statusbar_push(fstatus, 0, cnull)
    if (isinv) then
       call gtk_toggle_button_set_active(karc, FALSE)
       isinv = .FALSE.
       call set_labels
    end if
  end subroutine funpress

  subroutine hmspress(widget, gdata) bind(c)
    ! Display the entry box or top of stack in HMS format
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: nchars, mid
    logical :: status
    real(kind=c_double) :: val

    nchars=int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars > 0) then
       call read_entry(val, status,push=.false.)
       if (.not. status) return
    else
       call pop_stack(val, status, readonly=.TRUE.)
       if (.not. status) return
    end if
    call show_hms(val, fresult)
    mid = gtk_statusbar_push(fstatus, 0, cnull)
  end subroutine hmspress

  subroutine invtoggle(widget, gdata) bind(c)
    ! Toggle inverse functions
    type(c_ptr), value :: widget, gdata

    isinv = (gtk_toggle_button_get_active(widget) == TRUE)
    call set_labels
  end subroutine invtoggle

  subroutine set_trigunit(widget, gdata) bind(c)
    ! Select trig units
    type(c_ptr), value :: widget, gdata

    trigunit = hl_gtk_radio_group_get_select(rdgrp)
  end subroutine set_trigunit

  subroutine stacksel(widget, gdata) bind(c)
    ! Select an item on the stack
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: count
    integer(kind=c_int), dimension(:), allocatable :: sellist

    count = hl_gtk_listn_get_selections(fstack, sellist)
    if (count == 0) then
       stack_selected = -1
    else
       stack_selected = sellist(1)
       deallocate(sellist)
    end if
  end subroutine stacksel

  subroutine about_rpn(widget, gdata) bind(c)
    ! Display an about dialogue
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: adialog
    integer(kind=c_int) :: response
    character(kind=c_char), dimension(:), allocatable, target :: au1
    type(c_ptr), dimension(2) :: authors

    call convert_f_string((/ "James Tappin" /), au1)
    authors(1) = c_loc(au1)
    authors(2) = NULL

    adialog = gtk_about_dialog_new()
    call gtk_window_set_transient_for(adialog, win)
    call gtk_about_dialog_set_program_name(adialog, "RPN Calculator"//CNULL)
    call gtk_about_dialog_set_license(adialog, "GNU GPL 3"//CNULL)
    call gtk_about_dialog_set_comments(adialog, &
         & "This RPN calculator is a demonstration"//c_new_line// &
         & "of the capabilities of Gtk-fortran."//c_new_line//c_new_line// &
         & "It is entirely written in Fortran 95/2003."//c_new_line// &
         & "It should work with both Gtk+-2.24 and 3.0"//cnull)
    call gtk_about_dialog_set_authors(adialog, authors)

    response = gtk_dialog_run(adialog)
    call gtk_widget_destroy(adialog)
  end subroutine about_rpn

  subroutine mempress(widget, gdata) bind(c)
    ! A memory key.
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int), pointer :: memop
    integer(kind=c_int) :: midx
    real(kind=c_double) :: x, y
    integer(kind=c_int) :: nchars, mid
    logical :: status, need_x, entry_index

    call c_f_pointer(gdata, memop)

    need_x = .not. (memop == MEM_RCL .or. memop == MEM_CLR .or. &
         & memop == MEM_CLA)

    nchars = gtk_entry_get_text_length(fentry)

    if (memop /= MEM_CLA) then
       if (mem_selected >= 0) then ! A register is selected by the list
          midx = mem_selected
          if (need_x) then
             if (nchars > 0) then
                call read_entry(x, status, push=.false.)
             else
                call pop_stack(x, status, readonly=.true.)
             end if
             if (.not. status) return
          end if
          entry_index=.false.
       else
          if (nchars == 0) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "No content in entry and no register selected"//cnull)
             return
          end if
          call read_entry(midx, status)
          if (.not. status .or. midx < 0 .or. midx > maxreg) then
             mid = gtk_statusbar_push(fstatus, 0, &
                  & "Entry field is not a valid register"//cnull)
             return
          end if
          entry_index=.true.
          if (need_x) then
             call pop_stack(x, status, readonly=.true.)
             if (.not. status) return
          end if
       end if
    end if

    select case(memop)
    case(MEM_STO)
       call hl_gtk_listn_set_cell(fmemory, midx, 1, dvalue=x)
    case(MEM_RCL)
       call hl_gtk_listn_get_cell(fmemory, midx, 1, dvalue=y)
       call push_stack(y)
    case(MEM_PLUS)
       call hl_gtk_listn_get_cell(fmemory, midx, 1, dvalue=y)
       call hl_gtk_listn_set_cell(fmemory, midx, 1, dvalue=y+x)
    case(MEM_MINUS)
       call hl_gtk_listn_get_cell(fmemory, midx, 1, dvalue=y)
       call hl_gtk_listn_set_cell(fmemory, midx, 1, dvalue=y-x)
    case(MEM_CLR)
       call hl_gtk_listn_set_cell(fmemory, midx, 1, dvalue=0._c_double)
    case(MEM_CLA)
       do midx = 0, maxreg
          call hl_gtk_listn_set_cell(fmemory, midx, 1, dvalue=0._c_double)
       end do
    end select

    call hl_gtk_listn_set_selection(fmemory)
    mem_selected = -1
    if (entry_index) call gtk_entry_set_text(fentry, cnull)
    mid = gtk_statusbar_push(fstatus, 0, cnull)
  end subroutine mempress

  subroutine memsel(widget, gdata) bind(c)
    ! A selection in the registers list
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: count
    integer(kind=c_int), dimension(:), allocatable :: sellist

    count = hl_gtk_listn_get_selections(fmemory, sellist)
    if (count == 0) then
       mem_selected = -1
    else
       mem_selected = sellist(1)
       deallocate(sellist)
    end if
  end subroutine memsel

  subroutine save_values(widget, gdata) bind(c)
    ! Save the stack and registers
    ! NOTE: We use a formatted file and write the reals in Hex to getc
    ! full precision and endianness invariance.
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: response
    integer(kind=c_int) :: nrows, nchars
    type(c_ptr) :: cetext
    character(len=40) :: fetext
    real(kind=c_double) :: val
    character(len=256), dimension(:), allocatable :: file
    integer :: i
    character(len=8) :: dformat

    ! WARNING: This assumes that real kinds are the number of bytes
    ! which may not be quite portable. (If your compiler has it, use the
    ! C_SIZEOF function instead).
    write(dformat, "('(Z',I0')')") 2*c_double

    response = hl_gtk_file_chooser_show(file, confirm_overwrite=TRUE, &
         & parent=win, filter=(/"*.rpn"/), &
         & filter_name=(/"RPN save files"/), edit_filters=TRUE)

    if (response == FALSE) return

    open(47, file=file(1), action="write", form="formatted")
    deallocate(file)

    nchars=gtk_entry_get_text_length(fentry)
    if (nchars > 0) then
       cetext = gtk_entry_get_text(fentry)
       call convert_c_string(cetext, nchars, fetext)
       write(47, "(A/I0/A)") "Entry",nchars,fetext
    else
       write(47, "(A/I0)") "Entry",0
    endif

    nrows = hl_gtk_listn_get_n_rows(fstack)
    write(47, "(A/I0)") "Stack", nrows
    do i = 0, nrows-1
       call hl_gtk_listn_get_cell(fstack, i, 0, dvalue=val)
       write(47, dformat) val
    end do

    write(47, "(A/I0)") "Regs", maxreg
    do i = 0, maxreg
       call hl_gtk_listn_get_cell(fmemory, i, 1, dvalue=val)
       write(47, dformat) val
    end do

    write(47, "(A)") "Stats"
    do i = 0, 9
       call hl_gtk_listn_get_cell(fstats, i, 1, dvalue=val)
       write(47, dformat) val
    end do

    write(47, "(A/A)") "Form", result_format

    close(47)
  end subroutine save_values

  subroutine restore_values(widget, gdata) bind(c)
    ! restore the stack and registers
    ! NOTE: We use a formatted file and write the reals in Hex to getc
    ! full precision and endianness invariance.
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: response
    integer(kind=c_int) :: nrows, nchars, mid
    character(len=40) :: etext
    real(kind=c_double) :: val
    character(len=256), dimension(:), allocatable :: file
    integer :: i, ios
    character(len=5) :: tag
    character(len=8) :: dformat

    ! WARNING: This assumes that real kinds are the number of bytes
    ! which may not be quite portable. (If your compiler has it, use the
    ! C_SIZEOF function instead).
    write(dformat, "('(Z',I0')')") 2*c_double

    response = hl_gtk_file_chooser_show(file, create=FALSE, &
         & parent=win, filter=(/"*.rpn"/), &
         & filter_name=(/"RPN save files"/), edit_filters=TRUE)

    if (response == FALSE) return

    open(47, file=file(1), action="read", form="formatted")
    deallocate(file)

    call hl_gtk_listn_rem(fstack)   ! clear the stack

    do
       read(47, "(A)", iostat=ios) tag
       if (ios /= 0) exit

       select case(tag)
       case("Entry")
          read(47, *) nchars
          if (nchars > 0) then
             read(47, "(A)") etext
             call gtk_entry_set_text(fentry, trim(etext)//cnull)
          else
             call gtk_entry_set_text(fentry, cnull)
          end if
       case("Stack")
          read(47, *) nrows
          do i = 0, nrows-1
             read(47, dformat) val
             call hl_gtk_listn_ins(fstack)
             call hl_gtk_listn_set_cell(fstack, i, 0, dvalue=val)
             if (i == 0) call set_result(val)
          end do
       case("Stats")
          do i = 0, 9
             read(47, dformat) val
             call hl_gtk_listn_set_cell(fstats, i, 1, dvalue=val)
          end do
       case("Regs")
          read(47, *) nrows
          if (nrows > maxreg) mid = gtk_statusbar_push(fstatus, 0, &
               & "Too many registers"//cnull)
          do i = 0, nrows
             read(47, dformat) val
             if (i > maxreg) cycle
             call hl_gtk_listn_set_cell(fmemory, i, 1, dvalue=val)
          end do
       case("Form")
          read(47,"(A)") result_format
       case default
          mid = gtk_statusbar_push(fstatus, 0, &
               & "Unknown tag: "//tag//cnull)
       end select
    end do

    close(47)
  end subroutine restore_values

  subroutine show_help(widget, gdata) bind(c)
    ! Display help text
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: hscroll, hview, hquit, hbox

    help_window = hl_gtk_window_new("RPN Calculator"//cnull, &
         & deletable=FALSE, above=TRUE, parent=win)

    hbox = hl_gtk_box_new()
    call gtk_container_add(help_window, hbox)

    hview = hl_gtk_text_view_new(hscroll, editable=FALSE, &
         & ssize=(/600, 600/), initial_text = (/ &
         & 'RPN Calculator'//c_new_line// &
         & '=============='//c_new_line// &
         & ''//c_new_line// &
         & 'The GtkFortran rpncalc program is intended to be both a demonstration'//c_new_line// &
         & 'of some of the capabilities of GtkFortran and also a usable calculator'//c_new_line// &
         & 'application.'//c_new_line// &
         & ''//c_new_line// &
         & 'It uses Reverse Polish logic (similar to calculators from HP) for 2'//c_new_line// &
         & 'reasons:'//c_new_line// &
         & '1) It''s easier to implement.'//c_new_line// &
         & '2) I prefer RPN calculators when using a physical calculator.'//c_new_line// &
         & ''//c_new_line// &
         & 'The stack is of (at least in theory) unlimited size.'//c_new_line// &
         & ''//c_new_line// &
         & 'Usage'//c_new_line// &
         & '-----'//c_new_line// &
         & ''//c_new_line// &
         & 'Entering values:'//c_new_line// &
         & ''//c_new_line// &
         & 'Values can be entered either using the keypad, or by typing into the'//c_new_line// &
         & 'entry box. Values entered from the keypad make sanity checks for 2'//c_new_line// &
         & 'decimal points or a decimal entered after the exponent has been started'//c_new_line// &
         & 'and the change-sign key works in a reasonably intelligent way.'//c_new_line// &
         & ''//c_new_line// &
         & 'A value may be transferred from the entry to the stack by pressing the'//c_new_line// &
         & 'keyboard "Enter" key while focus is on the entry window, or by clicking'//c_new_line// &
         & 'the "Enter" key on the keypad. The "Dup" key copies the entry box to'//c_new_line// &
         & 'the stack without clearing the entry box. If the contents of the entry'//c_new_line// &
         & 'box are not a valid number (i.e. a Fortran "read" statement cannot'//c_new_line// &
         & 'convert it to a floating point value) a message is displayed in the'//c_new_line// &
         & 'status bar and you may edit the entry box to correct the problem.'//c_new_line// &
         & ''//c_new_line// &
         & 'Operators:'//c_new_line// &
         & ''//c_new_line// &
         & 'The operators (+, -, *, / and ** and the atan2 function) operate on the'//c_new_line// &
         & 'entry box and the top element on the stack if there is anything in the'//c_new_line// &
         & 'entry box. If the entry box is empty, then they operate on the top 2'//c_new_line// &
         & 'elements of the stack.'//c_new_line// &
         & ''//c_new_line// &
         & 'The result is placed on the top of the stack, and displayed in the'//c_new_line// &
         & 'result window.'//c_new_line// &
         & ''//c_new_line// &
         & 'Functions:'//c_new_line// &
         & ''//c_new_line// &
         & 'The functions operate on a single value, which is taken from the entry'//c_new_line// &
         & 'box if that has content or from the top of the stack otherwise. The'//c_new_line// &
         & 'result is placed on the top of the stack, and displayed in the result'//c_new_line// &
         & 'window.'//c_new_line// &
         & ''//c_new_line// &
         & 'If the "Inverse" checkbox is set, then functions are replaced by their'//c_new_line// &
         & 'inverses (e.g. "sin" becomes "asin"). The less-used functions in the'//c_new_line// &
         & 'pulldown are not affected by this.'//c_new_line// &
         & ''//c_new_line// &
         & 'The "Rad", "Deg" and "Grad" radio buttons are used to select Radians,'//c_new_line// &
         & 'Degrees or Grads for the trigonometric functions.'//c_new_line// &
         & ''//c_new_line// &
         & 'The "HMS" key is not a proper function, it doesn''t remove or add'//c_new_line// &
         & 'anything to the stack. It displays the contents of the entry box or the'//c_new_line// &
         & 'top of the stack as if it were a number of hours converted to'//c_new_line// &
         & 'HH:MM:SS.sss format.'//c_new_line// &
         & ''//c_new_line// &
         & 'Some less-used functions are in the "More>" pulldown. The "atan2"'//c_new_line// &
         & 'function computes atan(y/x) removing the quadrant ambiguities.'//c_new_line// &
         & ' '//c_new_line// &
         & 'Stack operations:'//c_new_line// &
         & ''//c_new_line// &
         & '"CE" clears the entry box, or if that is empty deletes the top entry on'//c_new_line// &
         & 'the stack.'//c_new_line// &
         & ''//c_new_line// &
         & '"CA" clears the entry box and all entries on the stack.'//c_new_line// &
         & ''//c_new_line// &
         & 'The up button moves the selected item in the stack up one place. If the'//c_new_line// &
         & 'top item (or nothing) is selected then it is exchanges with the entry'//c_new_line// &
         & 'box.'//c_new_line// &
         & ''//c_new_line// &
         & 'The down button moves the selected entry on the stack down one'//c_new_line// &
         & 'place.'//c_new_line// &
         & ''//c_new_line// &
         & 'The roll down button, moves the last element of the stack to the top'//c_new_line// &
         & 'and all others down one place.'//c_new_line// &
         & ''//c_new_line// &
         & 'Memory Registers:'//c_new_line// &
         & ''//c_new_line// &
         & 'The calculator also has 10[*] memory registers (numbered 0-9).'//c_new_line// &
         & ''//c_new_line// &
         & 'These can be accessed in one of two ways:'//c_new_line// &
         & '1) Select a register in the registers tab, and then click a memory'//c_new_line// &
         & 'operation. In this case the value used will be the entry box or the top'//c_new_line// &
         & 'of the stack if the entry is empty.'//c_new_line// &
         & '2) Enter a register number in the entry box and click the memory'//c_new_line// &
         & 'operation. The value used is the top of the stack.'//c_new_line// &
         & ''//c_new_line// &
         & 'The operations are:'//c_new_line// &
         & '"STO": Store the value in the selected register.'//c_new_line// &
         & '"RCL": Copy the selected register to the top of the stack'//c_new_line// &
         & '"M+": Add the value to the selected register'//c_new_line// &
         & '"M-": Subtract the value from the selected register'//c_new_line// &
         & '"MCL": Clear the selected register.'//c_new_line// &
         & ''//c_new_line// &
         & 'Statistics:'//c_new_line// &
         & ''//c_new_line// &
         & 'If the "Live stats" toggle is enabled, then a summary of the'//c_new_line// &
         & 'statistical properties of the contents of the stack is maintained in'//c_new_line// &
         & 'the "Statistics" tab of the display area. Clicking on a line of that'//c_new_line// &
         & 'display copies its value to the stack.'//c_new_line// &
         & ''//c_new_line// &
         & 'Save & Restore:'//c_new_line// &
         & ''//c_new_line// &
         & 'The stack, registers and entry box can be saved to and restored from a'//c_new_line// &
         & 'text file with the File->Save and File->Restore menu items.'//c_new_line// &
         & ''//c_new_line// &
         & 'The file format is a plain text file with the floating point values'//c_new_line// &
         & 'written in hexadecimal -- this allows the retention of full-precision'//c_new_line// &
         & 'but is endian-independent. Obviously any machines that do not use IEEE'//c_new_line// &
         & 'floating point values will not be able to read files from other'//c_new_line// &
         & 'machines. Also any machine with a c_double that is not 8-bytes will not'//c_new_line// &
         & 'work.'//c_new_line// &
         & ''//c_new_line// &
         & '[*] This can be changed by editing the "maxreg" value in widgets.f90'//cnull &
         & /) )

    call hl_gtk_box_pack(hbox, hscroll)

    hquit = hl_gtk_button_new("Dismiss"//cnull, clicked=c_funloc(help_del))

    call hl_gtk_box_pack(hbox, hquit)

    call gtk_widget_show_all(help_window)

    !    call gtk_main()

  end subroutine show_help

  subroutine help_del(widget, gdata) bind(c)
    ! Delete help window
    type(c_ptr), value :: widget, gdata

    call gtk_widget_destroy(help_window)
  end subroutine help_del


  subroutine about_gtkfortran (widget, gdata )  bind(c)
    ! About Gtk Fortran info.
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: dialog
    integer(c_int) :: response_id
    character(kind=c_char), dimension(:), allocatable, target :: au1, au2, &
         & au3, au4, au5
    type(c_ptr), dimension(6) :: authors

    call convert_f_string((/ "Jerry DeLisle" /), au1)
    call convert_f_string((/ "Vincent Magnin" /), au2)
    call convert_f_string((/ "James Tappin" /), au3)
    call convert_f_string((/ "Jens Hunger" /), au4)
    call convert_f_string((/ "Kyle Horne"/), au5)
    authors(1) = c_loc(au1)
    authors(2) = c_loc(au2)
    authors(3) = c_loc(au3)
    authors(4) = c_loc(au4)
    authors(5) = c_loc(au5)
    authors(6) = NULL

    dialog = gtk_about_dialog_new()
    call gtk_window_set_transient_for(dialog, win)
    call gtk_about_dialog_set_program_name(dialog, "Gtk-fortran"//CNULL)
    call gtk_about_dialog_set_license(dialog, "GNU GPL 3"//CNULL)
    call gtk_about_dialog_set_comments(dialog, &
         & "The gtk-fortran project aims to offer scientists programmi&
         &ng in Fortran a cross-platform library to build Graphical Us&
         &er Interfaces (GUI)."//c_new_line// & 
         &" Gtk-fortran is a partial GTK+ / Fortran binding 100% writt&
         &en in Fortran, thanks to the ISO_C_BINDING module for intero&
         &perability between C and Fortran, which is a part of the For&
         &tran 2003 standard."//c_new_line// & 
         & " GTK+ is a free software cross-platform graphical library &
         &available for Linux, Unix, Windows and MacOs X."//CNULL) 
    call gtk_about_dialog_set_website(dialog, &
         & "https://github.com/jerryd/gtk-fortran/wiki"//CNULL)

    call gtk_about_dialog_set_authors(dialog, authors)
    response_id =  gtk_dialog_run(dialog)
    call gtk_widget_destroy(dialog)
  end subroutine about_gtkfortran

  subroutine set_format_make(widget, gdata) bind(c)
    ! Dialog to set up the format for data display.
    type(c_ptr), value :: widget, gdata

    type(c_ptr) :: jb, jbb, junk

    fmt_window = hl_gtk_window_new("Set format"//cnull, above=TRUE, &
         & destroy=c_funloc(set_format_destroy), parent=win)

    jb = hl_gtk_box_new()
    call gtk_container_add(fmt_window, jb)
    jbb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(jb, jbb)
    junk = gtk_label_new("Format:"//cnull)
    call hl_gtk_box_pack(jbb, junk, expand=FALSE)
    fmt_entry = hl_gtk_entry_new(activate=c_funloc(set_format_cb), &
         & value=trim(result_format)//cnull, editable=TRUE, &
         & tooltip="Enter a Fortran format code for result display"//cnull)
    call hl_gtk_box_pack(jbb, fmt_entry)
    jbb = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(jb, jbb)
    junk = hl_gtk_button_new("Apply"//cnull, clicked=c_funloc(set_format_cb))
    call hl_gtk_box_pack(jbb, junk)
    junk = hl_gtk_button_new("Cancel"//cnull, &
         & clicked=c_funloc(set_format_destroy))
    call hl_gtk_box_pack(jbb, junk)
    call gtk_widget_show_all(fmt_window)
  end subroutine set_format_make

  subroutine set_format_cb(widget, gdata) bind(c)
    ! Set the display format
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: nchars
    type(c_ptr) :: ctext
    character(len=20) :: res_tmp
    integer :: idxo, idxc

    nchars = gtk_entry_get_text_length(fmt_entry)
    if (nchars == 0) then
       result_format = ''
    else
       ctext = gtk_entry_get_text(fmt_entry)
       call convert_c_string(ctext, nchars, res_tmp)
       if (res_tmp == "*") then
          result_format = ''
       else
          idxo = index(res_tmp, "(")
          idxc = index(res_tmp, ")", back=.true.)
          if (idxc /= len_trim(res_tmp)) res_tmp = trim(res_tmp)//")"
          if (idxo /= 1) res_tmp="("//res_tmp
          result_format=res_tmp
       end if
    end if

    call gtk_widget_destroy(fmt_window)
  end subroutine set_format_cb

  subroutine set_format_destroy(widget, gdata) bind(c)
    ! Don't set the display format
    type(c_ptr), value :: widget, gdata

    call gtk_widget_destroy(fmt_window)
  end subroutine set_format_destroy

  subroutine set_stats(widget, gdata) bind(c)
    ! Compute stats of the stack.
    type(c_ptr), value :: widget, gdata

    dynamic_stats = (gtk_toggle_button_get_active(widget) == TRUE)
    if (dynamic_stats) call stack_stats(0._c_double, initialize=.true.)
  end subroutine set_stats

  subroutine statsel(widget, gdata) bind(c)
    ! Copy stats to the stack.
    type(c_ptr), value :: widget, gdata

    integer(kind=c_int) :: count
    integer(kind=c_int), dimension(:), allocatable :: sellist
    real(kind=c_double) :: x

    count = hl_gtk_listn_get_selections(fstats, sellist)
    if (count > 0) then
       call hl_gtk_listn_get_cell(fstats, sellist(1), 1, dvalue=x)
       call push_stack(x)
       deallocate(sellist)
    end if
  end subroutine statsel

  subroutine show_list(col, cell, model, iter, data) bind(c)
    ! Display data in fortran list-directed default format, note that
    ! passing the column index via the data argument looks clumsy, but right now
    ! I can't see a better way. The only example I could find uses enums.
    type(c_ptr), value :: col, cell, model, iter, data

    character(len=40) :: rstring
    real(kind=c_double) :: val
        type(gvalue), target :: cvalue, svalue
    type(c_ptr) :: val_ptr
    integer(kind=c_int), pointer :: colno

    call c_f_pointer(data, colno)

    call gtk_tree_model_get_value(model, iter, colno, c_loc(cvalue))
    val = g_value_get_double(c_loc(cvalue))

    write(rstring, *) val

    val_ptr = c_loc(svalue)
    val_ptr = g_value_init(val_ptr, G_TYPE_STRING)

    call g_value_set_string(val_ptr, trim(rstring)//cnull)
    call g_object_set_property(cell, "text"//cnull, val_ptr)
  end subroutine show_list
end module handlers
