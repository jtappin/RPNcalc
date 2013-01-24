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

module utils
  ! This module contains "utility" functions for RPNcalc. These include
  ! tools to convert numbers and text and to move values on and off
  ! the stack and memories.

  use iso_c_binding
  use g, only: g_object_set_property, g_value_get_double, g_value_init, &
       & g_value_set_string

  use gtk, only: gtk_button_set_label, gtk_editable_insert_text, &
       & gtk_editable_set_position, gtk_entry_get_text, &
       & gtk_entry_get_text_length, gtk_entry_set_text, gtk_statusbar_push, &
       & gtk_tree_model_get_value, gtk_widget_grab_focus, TRUE

  use widgets
  use gtk_hl
  use iso_fortran_env

  implicit none

  interface read_entry
     module procedure read_entry_dbl
     module procedure read_entry_int
  end interface read_entry

contains
  subroutine show_hms(x, widget)
    ! Convert the value to H:M:S format & display it in the widget
    real(kind=c_double), intent(in) :: x
    type(c_ptr) :: widget

    integer :: ih, im, is
    real(kind=c_double) :: h, m, s, ms
    character(len=30) :: hms

    h = abs(x)
    ih = floor(h)
    m = (h-ih)*60.
    im = int(m)
    s = (m-im)*60.
    is = int(s)
    ms = s-is
    if (ms >= 0.9995) then
       ms = 0.
       is = is+1
       if (is == 60) then
          is = 0
          im = im+1
          if (im == 60) then
             im = 0
             ih = ih+1
          end if
       end if
    end if

    if (x < 0) ih = -ih
    if (dms_hms) then
       write(hms, "(I0,'° ',I2.2,''' ',I2.2,f4.3,'""')") ih, im, is, ms
    else
       write(hms, "(I0,':',I2.2,':',I2.2,f4.3)") ih, im, is, ms
    end if
    call gtk_entry_set_text(widget, trim(hms)//c_null_char)

  end subroutine show_hms

  subroutine append_char_entry(chr)
    ! Append a single character to the entry window
    character(kind=c_char), intent(in) :: chr

    integer(kind=c_int), target :: flen

    flen = int(gtk_entry_get_text_length(fentry), c_int)
    call gtk_editable_insert_text(fentry, chr//c_null_char, -1_c_int, c_loc(flen))

  end subroutine append_char_entry

  subroutine clear_entry_flags
    ! Clear the entry status flags.

    decimal_present = .FALSE.
    exponent_present = .FALSE.
  end subroutine clear_entry_flags

  subroutine read_entry_dbl(val, status, push)
    ! Read the value from the entry window
    real(kind=c_double), intent(out) :: val
    logical, intent(out) :: status
    logical, optional, intent(in) :: push

    integer(kind=c_int) :: nchars, mid
    type(c_ptr) :: ctext
    character(len=40) :: ftext
    character(len=80) :: iom
    integer :: ios

    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars == 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, &
            & "Entry field is empty"//c_null_char)
       status = .FALSE.
       return
    end if

    ctext = gtk_entry_get_text(fentry)
    call convert_c_string(ctext, int(nchars), ftext)

    read(ftext,*, iostat=ios, iomsg=iom) val
    if (ios /= 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, trim(iom)//c_null_char)
       status=.FALSE.
    else
       status=.TRUE.
       if (present(push)) then
          if (push) then
             call push_stack(val)
             call clear_entry_flags
          end if
       end if
    end if
  end subroutine read_entry_dbl

  subroutine read_entry_int(val, status)
    ! Read the value from the entry window
    integer(kind=c_int), intent(out) :: val
    logical, intent(out) :: status

    integer(kind=c_int) :: nchars, mid
    type(c_ptr) :: ctext
    character(len=40) :: ftext
    character(len=80) :: iom
    integer :: ios

    nchars = int(gtk_entry_get_text_length(fentry), c_int)
    if (nchars == 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, &
            & "Entry field is empty"//c_null_char)
       status = .FALSE.
       return
    end if

    ctext = gtk_entry_get_text(fentry)
    call convert_c_string(ctext, int(nchars), ftext)

    read(ftext,*, iostat=ios, iomsg=iom) val
    if (ios /= 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, trim(iom)//c_null_char)
       status=.FALSE.
    else
       status=.TRUE.
       call gtk_entry_set_text(fentry, c_null_char)
    end if
  end subroutine read_entry_int

  subroutine set_result(val)
    ! Set the result field
    real(kind=c_double), optional :: val

    character(len=40) :: text
    character(len=80) :: iom
    integer :: ios
    integer(kind=c_int) :: mid

    if (present(val)) then
       if (result_format == "") then
          write(text, *) val
       else
          write(text, result_format, iostat=ios, iomsg=iom) val
          if (ios /= 0) then
             mid = gtk_statusbar_push(fstatus, 0_c_int, trim(iom)//c_null_char)
             write(text, *) val
          end if
       end if
       call gtk_entry_set_text(fresult, trim(text)//c_null_char)
    else
       call gtk_entry_set_text(fresult, c_null_char)
    end if
  end subroutine set_result

  subroutine set_labels
    ! Set the labels of functions with inverses according to the isinv value

    if (isinv) then
       call gtk_button_set_label(ksin, "asin"//c_null_char)
       call gtk_button_set_label(kcos, "acos"//c_null_char)
       call gtk_button_set_label(ktan, "atan"//c_null_char)
       call hl_gtk_button_set_label(ksqrt, "x<sup>2</sup>"//c_null_char, &
            & is_markup=TRUE)
       call gtk_button_set_label(kloge, "exp"//c_null_char)
       call hl_gtk_button_set_label(klog10, "10<sup>x</sup>"//c_null_char, &
            & is_markup=TRUE)
       call gtk_button_set_label(ksinh, "asinh"//c_null_char)
       call gtk_button_set_label(kcosh, "acosh"//c_null_char)
       call gtk_button_set_label(ktanh, "atanh"//c_null_char)
       call hl_gtk_button_set_label_markup(kpower, &
            & "<sup>x</sup>√y"//c_null_char)
       call gtk_button_set_label(kroll, "R↑"//c_null_char)
    else
       call gtk_button_set_label(ksin, "sin"//c_null_char)
       call gtk_button_set_label(kcos, "cos"//c_null_char)
       call gtk_button_set_label(ktan, "tan"//c_null_char)
       call gtk_button_set_label(ksqrt, "√x"//c_null_char)
       call gtk_button_set_label(kloge, "ln"//c_null_char)
       call gtk_button_set_label(klog10, "log"//c_null_char)
       call gtk_button_set_label(ksinh, "sinh"//c_null_char)
       call gtk_button_set_label(kcosh, "cosh"//c_null_char)
       call gtk_button_set_label(ktanh, "tanh"//c_null_char)
       call hl_gtk_button_set_label_markup(kpower, &
            & "y<sup>x</sup>"//c_null_char)
       call gtk_button_set_label(kroll, "R↓"//c_null_char)
    end if
  end subroutine set_labels

  subroutine push_stack(val, show_result)

    ! PUSH_STACK
    ! Push a value onto the calculator stack.

    real(kind=c_double), intent(in) :: val
    logical, intent(in), optional :: show_result

    logical :: ishow

    if (present(show_result)) then
       ishow = show_result
    else
       ishow=.true.
    end if

    call hl_gtk_listn_ins(fstack, 0_c_int)
    call hl_gtk_listn_set_cell(fstack, 0_c_int, 0_c_int, dvalue=val)

    if (ishow) call set_result(val)
    if (dynamic_stats) call stack_stats(val)
  end subroutine push_stack

  subroutine pop_stack(val, status, readonly)
    ! POP_STACK
    ! Pop a value off the calculator stack

    real(kind=c_double), intent(out) :: val
    logical, intent(out) :: status
    logical, intent(in), optional :: readonly

    logical :: delete_top
    integer(kind=c_int) :: mid
    integer(kind=c_int) :: shwm

    if (present(readonly)) then
       delete_top = .not. readonly
    else
       delete_top = .true.
    end if

    shwm = hl_gtk_listn_get_n_rows(fstack)
    if (shwm == 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, &
            & "No values left on stack: cannot get a value"//c_null_char)
       status = .FALSE.
       return
    end if

    call hl_gtk_listn_get_cell(fstack, 0_c_int, 0_c_int, dvalue=val)
    if (delete_top) then
       call hl_gtk_listn_rem(fstack, 0_c_int)
       if (dynamic_stats) call stack_stats(val, remove=.true.)
    end if

    status=.TRUE.
  end subroutine pop_stack

  subroutine stack_stats(val, remove, clear, initialize)
    ! Compute stats of the stack.
    real(kind=c_double), intent(in) :: val
    logical, intent(in), optional :: remove, clear, initialize

    integer(kind=c_int) :: nrows, i, mid
    real(kind=c_double) :: x

    logical :: irem, iclr, iinit

    if (present(remove)) then
       irem = remove
    else
       irem=.FALSE.
    end if
    if (present(clear)) then
       iclr = clear
    else
       iclr = .false.
    end if
    if (present(initialize)) then
       iinit = initialize
    else
       iinit = .false.
    end if

    nrows = hl_gtk_listn_get_n_rows(fstack)
    if (iclr .or. nrows == 0) then
       if (nrows /= 0) mid = gtk_statusbar_push(fstatus, 0_c_int, &
            & "Clearing statistics while stack not empty"//c_null_char)

       s1 = 0._c_double
       s2 = 0._c_double
       s3 = 0._c_double
       s4 = 0._c_double

       avg=0._c_double
       sdev=0._c_double
       var=0._c_double
       skew=0._c_double
       kurt=0._c_double
    else
       if (iinit) then
          s1 = 0._c_double
          s2 = 0._c_double
          s3 = 0._c_double
          s4 = 0._c_double

          do i = 0, nrows-1
             call hl_gtk_listn_get_cell(fstack, i, 0_c_int, dvalue=x)
             s1 = s1+x
             s2 = s2+x**2
             s3 = s3+x**3
             s4 = s4+x**4
          end do
       else if (irem) then
          s1 = s1-val
          s2 = s2-val**2
          s3 = s3-val**3
          s4 = s4-val**4
       else
          s1 = s1+val
          s2 = s2+val**2
          s3 = s3+val**3
          s4 = s4+val**4
       end if

       if (nrows >= 1) then
          avg = s1/real(nrows, c_double)
          if (nrows >=2) then
             var = (s2 - 2._c_double*avg*s1 + nrows*avg**2)/(nrows-1)
             sdev=sqrt(var)
             if (nrows >= 3) then
                skew = (s3 - 3._c_double*avg*s2 + 3._c_double*avg**2*s1 - &
                     & nrows*avg**3) /(real(nrows,c_double)*sdev**3)
                if (nrows >= 4) then
                   kurt = (s4 - 4._c_double*avg*s3 + 6._c_double*avg**2*s2 - &
                        & 4._c_double*avg**3*s1 + nrows*avg**4)/ &
                        & (real(nrows,c_double)*sdev**4) - 3._c_double
                else
                   kurt=0._c_double
                end if
             else
                skew = 0._c_double
             end if
          else
             sdev = 0._c_double
             var = 0._c_double
          end if
       else
          avg = 0._c_double
       end if
    end if
    call hl_gtk_listn_set_cell(fstats, 0_c_int, 1_c_int, &
         & dvalue=real(nrows,c_double))
    call hl_gtk_listn_set_cell(fstats, 1_c_int, 1_c_int, dvalue=avg)
    call hl_gtk_listn_set_cell(fstats, 2_c_int, 1_c_int, dvalue=var)
    call hl_gtk_listn_set_cell(fstats, 3_c_int, 1_c_int, dvalue=sdev)
    call hl_gtk_listn_set_cell(fstats, 4_c_int, 1_c_int, dvalue=skew)
    call hl_gtk_listn_set_cell(fstats, 5_c_int, 1_c_int, dvalue=kurt)
    call hl_gtk_listn_set_cell(fstats, 6_c_int, 1_c_int, dvalue=s1)
    call hl_gtk_listn_set_cell(fstats, 7_c_int, 1_c_int, dvalue=s2)
    call hl_gtk_listn_set_cell(fstats, 8_c_int, 1_c_int, dvalue=s3)
    call hl_gtk_listn_set_cell(fstats, 9_c_int, 1_c_int, dvalue=s4)

  end subroutine stack_stats

  subroutine show_list(col, cell, model, iter, data) bind(c)
    ! Display data in fortran list-directed default format, note that
    ! passing the column index via the data argument looks clumsy, but right now
    ! I can't see a better way. The only example I could find uses enums and so
    ! can't be used for different columns in different lists.

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

    call g_value_set_string(val_ptr, trim(rstring)//c_null_char)
    call g_object_set_property(cell, "text"//c_null_char, val_ptr)

    call gtk_widget_grab_focus(fentry)
    call gtk_editable_set_position(fentry, -1_c_int)   ! Put cursor at end.

  end subroutine show_list

  subroutine save_all(file, status)
    ! Save the stack etc. to a file.
    ! NOTE: We use a formatted file and write the reals in Hex to get
    ! full precision and endianness invariance.
    character(len=*), intent(in) :: file
    integer, intent(out), optional :: status

    integer(kind=c_int) :: nrows, nchars, mid
    type(c_ptr) :: cetext
    character(len=40) :: fetext
    real(kind=c_double) :: val
    integer :: ios, unit
    integer(kind=c_int) :: i
    character(len=8) :: dformat
    character(len=80) :: iom

    ! WARNING: This assumes that real kinds are the number of bytes
    ! which may not be quite portable. (If your compiler has it, use the
    ! C_SIZEOF function instead).
    write(dformat, "('(Z',I0')')") 2*c_double

    open(newunit=unit, file=file, action="write", form="formatted", &
         & iostat=ios, iomsg=iom)
    if (present(status)) status=ios
    if (ios /= 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, trim(iom)//c_null_char)
       return
    end if

    nchars=gtk_entry_get_text_length(fentry)
    if (nchars > 0) then
       cetext = gtk_entry_get_text(fentry)
       call convert_c_string(cetext, int(nchars), fetext)
       write(unit, "(A/I0/A)") "Entry",nchars,fetext
    else
       write(unit, "(A/I0)") "Entry",0
    endif

    nrows = hl_gtk_listn_get_n_rows(fstack)
    write(unit, "(A/I0)") "Stack", nrows
    do i = 0, nrows-1
       call hl_gtk_listn_get_cell(fstack, i, 0_c_int, dvalue=val)
       write(unit, dformat) val
    end do

    write(unit, "(A/I0)") "Regs", maxreg
    do i = 0, maxreg
       call hl_gtk_listn_get_cell(fmemory, i, 1_c_int, dvalue=val)
       write(unit, dformat) val
    end do

    write(unit, "(A)") "Stats"
    do i = 0, 9
       call hl_gtk_listn_get_cell(fstats, i, 1_c_int, dvalue=val)
       write(unit, dformat) val
    end do

    write(unit, "(A/A/3I5)") "Fmt", result_format, fmt_type, &
         & fmt_decimal, fmt_expplaces

    close(unit)

  end subroutine save_all

  subroutine restore_all(file, status)
    ! Restore values to the calculator from a file
    ! NOTE: We use a formatted file and write the reals in Hex to getc
    ! full precision and endianness invariance.
    character(len=*), intent(in) :: file
    integer, intent(out), optional :: status

    integer(kind=c_int) :: nrows, nchars, mid
    character(len=40) :: etext
    real(kind=c_double) :: val
    integer :: ios, unit
    integer(kind=c_int) :: i
    character(len=5) :: tag
    character(len=8) :: dformat
    character(len=80) :: iom

    ! WARNING: This assumes that real kinds are the number of bytes
    ! which may not be quite portable. (If your compiler has it, use the
    ! C_SIZEOF function instead).
    write(dformat, "('(Z',I0')')") 2*c_double

    open(newunit=unit, file=file, action="read", status='old', &
         & form="formatted", iostat=ios, iomsg=iom)
    if (ios /= 0) then
       mid = gtk_statusbar_push(fstatus, 0_c_int, trim(iom)//c_null_char)
       if (present(status)) status=ios
       return
    end if

    call hl_gtk_listn_rem(fstack)   ! clear the stack

    do
       read(unit, "(A)", iostat=ios, iomsg=iom) tag
       if (ios /= 0) then
          if (ios == iostat_end) ios = 0  ! EOF is expected here
          exit
       end if

       select case(tag)
       case("Entry")
          read(unit, *, iostat=ios, iomsg=iom) nchars
          if (ios /= 0) exit
          if (nchars > 0) then
             read(unit, "(A)", iostat=ios, iomsg=iom) etext
             if (ios /= 0) exit
             call gtk_entry_set_text(fentry, trim(etext)//c_null_char)
          else
             call gtk_entry_set_text(fentry, c_null_char)
          end if

       case("Stack")
          read(unit, *, iostat=ios, iomsg=iom) nrows
          if (ios /= 0) exit
          do i = 0, nrows-1
             read(unit, dformat, iostat=ios, iomsg=iom) val
             if (ios /= 0) exit
             call hl_gtk_listn_ins(fstack)
             call hl_gtk_listn_set_cell(fstack, i, 0_c_int, dvalue=val)
             if (i == 0) call set_result(val)
          end do

       case("Stats")
          do i = 0, 9
             read(unit, dformat, iostat=ios, iomsg=iom) val
             if (ios /= 0) exit
             call hl_gtk_listn_set_cell(fstats, i, 1_c_int, dvalue=val)
          end do
       case("Regs")

          read(unit, *, iostat=ios, iomsg=iom) nrows
          if (ios /= 0) exit
          if (nrows > maxreg) mid = gtk_statusbar_push(fstatus, 0_c_int, &
               & "Too many registers"//c_null_char)
          do i = 0, nrows
             read(unit, dformat, iostat=ios, iomsg=iom) val
             if (ios /= 0) exit
             if (i > maxreg) cycle
             call hl_gtk_listn_set_cell(fmemory, i, 1_c_int, dvalue=val)
          end do

       case("Form")  ! Old-style format
          read(unit,"(A)", iostat=ios, iomsg=iom) result_format
          if (ios /= 0) exit

       case("Fmt")  ! New-style format
          read(unit,"(A)", iostat=ios, iomsg=iom) result_format
          if (ios /= 0) exit
          read(unit,*, iostat=ios, iomsg=iom) fmt_type, &
               & fmt_decimal, fmt_expplaces
          if (ios /= 0) exit

       case default
          mid = gtk_statusbar_push(fstatus, 0_c_int, &
               & "Unknown tag: "//tag//c_null_char)
       end select
    end do

    if (ios /= 0) mid = gtk_statusbar_push(fstatus, 0_c_int, &
         & trim(iom)//c_null_char)
    if (present(status)) status=ios

    close(unit)

  end subroutine restore_all

  function check_command(cmd)
    logical :: check_command
    character(len=*), intent(in) :: cmd

    ! Determine if a command exists. Does not find aliases or shell built-ins

    integer :: status

    call execute_command_line("which "//trim(cmd)//" > /dev/null 2> /dev/null", &
         & exitstat=status)
    check_command = status==0

  end function check_command

  subroutine find_pdf_reader(reader)
    character(len=*), intent(out) :: reader

    ! Identify an available PDF reader.

    integer :: vstatus, i
    character(len=80) :: env_reader

    ! First check if the user has specified a viewer, then scan the
    ! build in list.

    if (.not. pdf_is_init) then
       call get_environment_variable('RPNCALC_VIEWER', value=env_reader, &
            & status=vstatus)
       if (env_reader == 'text') then
             pdf_reader = ''
             pdf_is_init = .true.
       else if (env_reader /= '' .and. &
            & vstatus == 0 .and. check_command(pdf_reader)) then
          pdf_reader = env_reader
          pdf_is_init = .true.
       else
          do i = 1, size(pdf_readers)
             if (check_command(pdf_readers(i))) then
                pdf_reader = pdf_readers(i)
                pdf_is_init = .true.
                exit
             end if
          end do
       end if
    end if

    reader = pdf_reader

    ! Prevent repeated scans.

    if (.not. pdf_is_init) then
       write(error_unit, "(A)") "find_pdf_reader: No PDF viewer found"
       pdf_is_init = .true.
    end if

  end subroutine find_pdf_reader
    
  ! Inverse hyperbolics for those Fortrans that don't support them.

!!$  function asinh(x)
!!$    real(kind=c_double) :: asinh
!!$    real(kind=c_double), intent(in) :: x
!!$
!!$    asinh = log(x + sqrt(x**2 + 1._c_double))
!!$  end function asinh
!!$  function acosh(x)
!!$    real(kind=c_double) :: acosh
!!$    real(kind=c_double), intent(in) :: x
!!$
!!$    acosh = log(x + sqrt(x**2 - 1._c_double))
!!$  end function acosh
!!$  function atanh(x)
!!$    real(kind=c_double) :: atanh
!!$    real(kind=c_double), intent(in) :: x
!!$
!!$    atanh = log((1._c_double+x)/(1._c_double-x))/2._c_double
!!$  end function atanh

end module utils
