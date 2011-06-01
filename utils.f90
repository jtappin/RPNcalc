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
  use gtk, only: gtk_editable_insert_text, gtk_statusbar_push, &
       & gtk_button_set_label, gtk_widget_grab_focus, gtk_editable_set_position
  use g, only:g_object_set_property

  use widgets
  use gtk_hl

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
    write(hms, "(I0,':',I2.2,':',I2.2,f4.3)") ih, im, is, ms
    call gtk_entry_set_text(widget, trim(hms)//cnull)

  end subroutine show_hms

   subroutine append_char_entry(chr)
    ! Append a single character to the entry window
    character(kind=c_char), intent(in) :: chr

    integer(kind=c_int), target :: flen

    flen = int(gtk_entry_get_text_length(fentry), c_int)
    call gtk_editable_insert_text(fentry, chr//cnull, -1, c_loc(flen))

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
       mid = gtk_statusbar_push(fstatus, 0, "Entry field is empty"//cnull)
       status = .FALSE.
       return
    end if

    ctext = gtk_entry_get_text(fentry)
    call convert_c_string(ctext, nchars, ftext)

    read(ftext,*, iostat=ios, iomsg=iom) val
    if (ios /= 0) then
       mid = gtk_statusbar_push(fstatus, 0, trim(iom)//cnull)
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
       mid = gtk_statusbar_push(fstatus, 0, "Entry field is empty"//cnull)
       status = .FALSE.
       return
    end if

    ctext = gtk_entry_get_text(fentry)
    call convert_c_string(ctext, nchars, ftext)

    read(ftext,*, iostat=ios, iomsg=iom) val
    if (ios /= 0) then
       mid = gtk_statusbar_push(fstatus, 0, trim(iom)//cnull)
       status=.FALSE.
    else
       status=.TRUE.
       call gtk_entry_set_text(fentry, cnull)
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
             mid = gtk_statusbar_push(fstatus, 0, trim(iom)//cnull)
             write(text, *) val
          end if
       end if
       call gtk_entry_set_text(fresult, trim(text)//cnull)
    else
       call gtk_entry_set_text(fresult, cnull)
    end if
  end subroutine set_result

  subroutine set_labels
    ! Set the labels of functions with inverses according to the isinv value

    if (isinv) then
       call gtk_button_set_label(ksin, "asin"//cnull)
       call gtk_button_set_label(kcos, "acos"//cnull)
       call gtk_button_set_label(ktan, "atan"//cnull)
       call gtk_button_set_label(ksqrt, "x**2"//cnull)
       call gtk_button_set_label(kloge, "exp"//cnull)
       call gtk_button_set_label(klog10, "10**x"//cnull)
       call gtk_button_set_label(ksinh, "asinh"//cnull)
       call gtk_button_set_label(kcosh, "acosh"//cnull)
       call gtk_button_set_label(ktanh, "atanh"//cnull)
    else
       call gtk_button_set_label(ksin, "sin"//cnull)
       call gtk_button_set_label(kcos, "cos"//cnull)
       call gtk_button_set_label(ktan, "tan"//cnull)
       call gtk_button_set_label(ksqrt, "√x"//cnull)
       call gtk_button_set_label(kloge, "ln"//cnull)
       call gtk_button_set_label(klog10, "log"//cnull)
       call gtk_button_set_label(ksinh, "sinh"//cnull)
       call gtk_button_set_label(kcosh, "cosh"//cnull)
       call gtk_button_set_label(ktanh, "tanh"//cnull)
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

    call hl_gtk_listn_ins(fstack, 0)
    call hl_gtk_listn_set_cell(fstack, 0, 0, dvalue=val)
    
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
       mid = gtk_statusbar_push(fstatus, 0, &
            & "No values left on stack: cannot get a value"//cnull)
       status = .FALSE.
       return
    end if

    call hl_gtk_listn_get_cell(fstack, 0, 0, dvalue=val)
    if (delete_top) then
       call hl_gtk_listn_rem(fstack, 0)
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
       if (nrows /= 0) mid = gtk_statusbar_push(fstatus, 0, &
            & "Clearing statistics while stack not empty"//cnull)

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
             call hl_gtk_listn_get_cell(fstack, i, 0, dvalue=x)
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

       call hl_gtk_listn_set_cell(fstats, 0, 1, dvalue=real(nrows,c_double))
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
    call hl_gtk_listn_set_cell(fstats, 1, 1, dvalue=avg)
    call hl_gtk_listn_set_cell(fstats, 2, 1, dvalue=var)
    call hl_gtk_listn_set_cell(fstats, 3, 1, dvalue=sdev)
    call hl_gtk_listn_set_cell(fstats, 4, 1, dvalue=skew)
    call hl_gtk_listn_set_cell(fstats, 5, 1, dvalue=kurt)
    call hl_gtk_listn_set_cell(fstats, 6, 1, dvalue=s1)
    call hl_gtk_listn_set_cell(fstats, 7, 1, dvalue=s2)
    call hl_gtk_listn_set_cell(fstats, 8, 1, dvalue=s3)
    call hl_gtk_listn_set_cell(fstats, 9, 1, dvalue=s4)

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

    call g_value_set_string(val_ptr, trim(rstring)//cnull)
    call g_object_set_property(cell, "text"//cnull, val_ptr)
    if (focus_entry) then
       call gtk_widget_grab_focus(fentry)
       call gtk_editable_set_position(fentry, -1)   ! Put cursor at end.
    end if
  end subroutine show_list

end module utils
