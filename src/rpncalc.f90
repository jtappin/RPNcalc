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

program rpncalc
  ! RPNCALC
  ! A simple RPN notation calculator.
  ! It's not that pretty, but it does show some of what it's possible
  ! Gtk-Fortran. Converted from an earlier pilib code.

  ! Usage:
  !	rpncalc [-o|-open|-c|-closed] [{-r|--restore} <file>] \
  !	       [{-m|--registers} <n>] [-h|--help]
  !
  !	-o, --open: Start with the stack displays open (default)
  !	-c, --closed: Start with the stack displays closed
  !	-r, --restore: Restore the specified file.
  !	-m, --registers: Set the number of registers to the specified value.
  !	-h, --help: Print help text and exit.

  ! This source file contains the main program that creates the widgets.

  use iso_c_binding !, only: c_ptr, c_null_ptr, c_loc
  use iso_fortran_env, only: error_unit

  use gtk, only: gtk_container_add, gtk_expander_new, &
       & gtk_expander_set_expanded, gtk_label_new, gtk_main, &
       & gtk_statusbar_new, gtk_widget_show_all, gtk_init, TRUE, FALSE, &
       & GDK_SHIFT_MASK, GDK_CONTROL_MASK, GTK_PACK_DIRECTION_LTR
  use gtk_hl

  use handlers
  use widgets
  use physics

  implicit none

  integer(kind=c_int) :: i
  character, dimension(10), target :: pnum
  integer(kind=c_int), target :: pplus= OP_PLUS, pminus=OP_MINUS, &
       & ptimes=OP_TIMES, pdiv=OP_DIVIDE, ppow=OP_POWER, patan2=FUN_ATAN2, &
       & pmod = FUN_MOD, phcf = FUN_HCF, plcm = FUN_LCM

  integer(kind=c_int), target :: psin=FUN_SIN, pcos=FUN_COS, ptan=FUN_TAN,&
       & pln=FUN_LN, psqrt=FUN_SQRT, psinh=FUN_SINH, pcosh=FUN_COSH, &
       & ptanh=FUN_TANH, pl10=FUN_LOG10, pinv=FUN_INV, pabs=FUN_ABS, &
       & pint=FUN_INT, pfrac=FUN_FRAC, pfact=FUN_FACTORIAL, pnint=FUN_ROUND

  integer(kind=c_int), target :: pmsto=MEM_STO, pmrcl=MEM_RCL, &
       & pmplus=MEM_PLUS, pmminus=MEM_MINUS, pmclr=MEM_CLR, pmcla=MEM_CLA

  integer(kind=c_int), target :: stackcol=0, memcol=1, statcol=1

  integer(kind=c_int), target :: binary = 2, octal = 8, hexadecimal = 16

  ! Workspace variables
  character(len=10) :: ws ! workspace for number button labels
  integer(kind=c_int) :: ix, iy
  integer(kind=c_int) :: idx
  type(c_ptr) :: junk, jbase, jbase1

  ! Command line argument handling
  integer :: iarg, narg, status
  character(len=80) :: arg, msg
  integer(kind=c_int) :: isopen = TRUE
  character(len=200) :: restfile = '', smaxreg = ''

  ! Read the resource file (if any)

  call get_rc(quiet=.true.)

  ! Check for command line arguments

  narg = command_argument_count()
  iarg = 1
  do
     if (iarg > narg) exit
     call get_command_argument(iarg, arg, status=status)
     if (status > 0) exit
     if (status < 0) write(error_unit,*) "RPNcalc: Warning argument truncated"

     select case(arg)
     case("-h", "--help")
        print *, "Usage:"
        print *, " rpncalc [-o|-open|-c|-closed] [{-r|--restore} <file>] \ "
        print *, "       [{-m|--registers} <n>] [-h|--help] \ "
        print *, "       [-D|--degrees|-R|--radians|-G|--grads] "
        print *, " "
        print *, " -o, --open: Start with the stack displays open (default)"
        print *, " -c, --closed: Start with the stack displays closed"
        print *, " -r, --restore: Restore the specified file."
        print *, " -m, --registers: Set the number of registers to the specified value."
        print *, " -D, --degrees: Do trig in degrees (default)."
        print *, " -R, --radians: Do trig in radians."
        print *, " -G, --grads: Do trig in grads."
        print *, " -h, --help: Print help text and exit."
        stop

     case("-o", "--open")         ! Start with the stack display open (default)
        isopen = TRUE
     case("-c", "--closed")       ! Start with the stack display closed
        isopen = FALSE
     case("-r", "--restore")      ! Restore a save file
        if (iarg == narg) then
           write(error_unit, *) "RPNcalc: ",trim(arg)," option needs an argument"
        else
           call get_command_argument(iarg+1, restfile)
           if (index(restfile,'-') == 1) then
              write(error_unit, *) "RPNcalc: ",trim(arg)," option needs an argument"
           else
              iarg = iarg+1
           end if
        end if
     case("-m", "--registers")    ! Number of memory registers to allocate
        if (iarg == narg) then
           write(error_unit, *) "RPNcalc: ",trim(arg), &
                & " option needs an argument"
        else
           call get_command_argument(iarg+1, smaxreg)
           if (index(smaxreg,'-') == 1) then
              write(error_unit, *) "RPNcalc: ",trim(arg), &
                   & " option needs an argument"
           else
              read(smaxreg, *, iostat=status, iomsg=msg) maxreg
              if (status /= 0) then
                 write(error_unit, *) "RPNcalc: error reading register count: "
                 write(error_unit, *) "         ", msg
                 write(error_unit, *) "RPNcalc: ", trim(arg),&
                      & " invalid count: ", trim(smaxreg)
                 maxreg = 9    ! Reset to default
              else
                 maxreg = maxreg-1  ! The highest number is the count-1
              end if
              iarg = iarg+1
           end if
        end if
     case("-R","--radians")
        trigunit = 0
     case("-D","--degrees")
        trigunit = 1
     case("-G","--grads")
        trigunit = 2
     case("-f", "--focus")
        write(error_unit, *) "RPNcalc: Focus is now always maintained"
     case default                 ! Bad option
        write(error_unit, *) "RPNcalc: Unknown option:", trim(arg)
     end select
     iarg = iarg+1
  end do

  ! Initialise gtk
  call gtk_init()

  ! Create a window and put a vertical box into it
  win = hl_gtk_window_new("RPN Calculator"//c_null_char, &
       & destroy=c_funloc(my_destroy), &
       & resizable=FALSE, accel_group=accel, icon_name="calc"//c_null_char)
  base = hl_gtk_box_new()
  call gtk_container_add(win, base)

  ! Menu bar
  fmenu = hl_gtk_menu_new(orientation=GTK_PACK_DIRECTION_LTR)
  call hl_gtk_box_pack(base, fmenu)
  ffmenu = hl_gtk_menu_submenu_new(fmenu, "File"//c_null_char)
  ksave = hl_gtk_menu_item_new(ffmenu, "Save"//c_null_char, &
       & activate=c_funloc(save_values), accel_key="s"//c_null_char, &
       & accel_group=accel)
  krestore = hl_gtk_menu_item_new(ffmenu, "Restore"//c_null_char, &
       & activate=c_funloc(restore_values), accel_key="o"//c_null_char, &
       & accel_group=accel)
  kquit = hl_gtk_menu_item_new(ffmenu, "Quit"//c_null_char, &
       & activate=c_funloc(my_destroy), accel_key="q"//c_null_char, &
       & accel_group=accel)

  femenu = hl_gtk_menu_submenu_new(fmenu, "Edit"//c_null_char)
  kcut = hl_gtk_menu_item_new(femenu, "Cut"//c_null_char, &
       & activate=c_funloc(menu_cut), accel_key="x"//c_null_char, &
       & accel_group=accel, &
       & tooltip="Cut selection in entry box to the clipboard."//c_null_char)
  kcopy = hl_gtk_menu_item_new(femenu, "Copy"//c_null_char, &
       & activate=c_funloc(menu_copy), accel_key="c"//c_null_char, &
       & accel_group=accel, &
       & tooltip="Copy selection in entry or result to the clipboard."&
       &//c_null_char)
  kpaste = hl_gtk_menu_item_new(femenu, "Paste"//c_null_char, &
       & activate=c_funloc(menu_paste), accel_key="v"//c_null_char, &
       & accel_group=accel, &
       & tooltip="Paste from the clipboard to the entry box."//c_null_char)
  ktdel = hl_gtk_menu_item_new(femenu, "Delete"//c_null_char,&
       & activate=c_funloc(menu_delete), accel_key='x'//c_null_char, &
       & accel_group=accel, accel_mods=ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK), &
       & tooltip="Delete the selection from the entry box."//c_null_char)

  junk = hl_gtk_menu_item_new(femenu)

  kfedit = hl_gtk_menu_item_new(femenu, "Result Format"//c_null_char, &
       & activate=c_funloc(set_format_make), accel_key="f"//c_null_char, &
       & accel_group=accel)

  kpset = hl_gtk_menu_item_new(femenu, "Help viewer"//c_null_char, &
       & activate=c_funloc(set_pdf_reader))

  khrdeg = hl_gtk_check_menu_item_new(femenu, "Display degrees"//c_null_char, &
       & toggled = c_funloc(set_dms_hms), initial_state=f_c_logical(dms_hms), &
       & tooltip = &
       & "Select angular or time format for HMS display"//c_null_char)

  fhmenu = hl_gtk_menu_submenu_new(fmenu, "Help"//c_null_char)
  khelp = hl_gtk_menu_item_new(fhmenu, "Help"//c_null_char, &
       & activate=c_funloc(show_help), accel_key="h"//c_null_char, &
       & accel_group=accel)
  kabout = hl_gtk_menu_item_new(fhmenu, "About: RPN Calculator"//c_null_char, &
       & activate=c_funloc(about_rpn), accel_key="a"//c_null_char, &
       & accel_group=accel)
  kfabout = hl_gtk_menu_item_new(fhmenu, "About: Gtk-Fortran"//c_null_char, &
       & activate=c_funloc(about_gtkfortran), accel_key="a"//c_null_char, &
       & accel_group=accel, accel_mods=ior(GDK_CONTROL_MASK, GDK_SHIFT_MASK))

  ! Value entry window.
  jbase = hl_gtk_table_new()
  call hl_gtk_box_pack(base, jbase)
  junk=gtk_label_new("Enter:"//c_null_char)
  call hl_gtk_table_attach(jbase, junk, 0_c_int, 0_c_int, xopts=0_c_int)
  fentry = hl_gtk_entry_new(editable=TRUE, activate=c_funloc(enter_value), &
       & tooltip="Enter values here"//C_NULL_CHAR, &
       & insert_text=c_funloc(char_entered), &
       & delete_text=c_funloc(char_deleted), &
       & len=40_c_int)
  call hl_gtk_table_attach(jbase, fentry, 1_c_int, 0_c_int)

  ! result window. 
  junk=gtk_label_new("Result:"//c_null_char)
  call hl_gtk_table_attach(jbase, junk, 0_c_int, 1_c_int, xopts=0_c_int)
  fresult = hl_gtk_entry_new(editable=FALSE, &
       & tooltip="Results displayed here"//C_NULL_CHAR)
  call hl_gtk_table_attach(jbase, fresult, 1_c_int, 1_c_int)

  ! A status message bar
  fstatus = gtk_statusbar_new()
  call hl_gtk_box_pack(base, fstatus)

  ! Keypad

  keybox = hl_gtk_table_new(homogeneous=true)
  call hl_gtk_box_pack(base, keybox)

  ! numbers

  do i=1,10
     write(ws,"('    ',I1)") i-1
     write(pnum(i),"(I1)") i-1
     knum(i) = hl_gtk_button_new(ws//C_NULL_CHAR, clicked=c_funloc(numpress), &
          & data=c_loc(pnum(i)))
     if (i == 1) then
        ix=0
        iy=4
     else
        ix=mod(i-2,3)
        iy=3-(i-2)/3
     end if
     call hl_gtk_table_attach(keybox, knum(i), ix,iy)
  end do

  kpoint = hl_gtk_button_new("•"//C_NULL_CHAR, clicked=c_funloc(dppress))
  call hl_gtk_table_attach(keybox, kpoint, 1_c_int,4_c_int)

  kchs = hl_gtk_button_new("+/-"//C_NULL_CHAR, clicked=c_funloc(chspress), &
       & tooltip="Change Sign"//C_NULL_CHAR)
  call hl_gtk_table_attach(keybox, kchs, 2_c_int, 4_c_int)

  kee = hl_gtk_button_new("EE"//C_NULL_CHAR, clicked=c_funloc(eepress), &
       & tooltip= "Enter Exponent"//C_NULL_CHAR)
  call hl_gtk_table_attach(keybox, kee, 0_c_int, 5_c_int)

  kpi = hl_gtk_button_new("π"//c_null_char, clicked=c_funloc(pipress),&
       & tooltip="Enter π"//c_null_char)
  call hl_gtk_table_attach(keybox, kpi, 1_c_int, 5_c_int)

  ! Delete character from entry box
  kdel = hl_gtk_button_new("Del"//c_null_char, clicked=c_funloc(delpress), &
       & tooltip="Delete last char"//c_null_char)
  call hl_gtk_table_attach(keybox, kdel, 2_c_int, 5_c_int)

  ! Enter and duplicate entry

  kenter = hl_gtk_button_new("Enter"//c_null_char, &
       & clicked=c_funloc(enter_value), &
       & tooltip="Move entry to stack"//c_null_char)
  call hl_gtk_table_attach(keybox, kenter, 0_c_int, 6_c_int, xspan=3_c_int)
  kdup = hl_gtk_button_new("Duplicate"//c_null_char, &
       & clicked=c_funloc(duppress), &
       & tooltip="Copy entry to stack"//c_null_char)
  call hl_gtk_table_attach(keybox, kdup, 3_c_int, 6_c_int, xspan=2_c_int)

  ! Operations

  kplus = hl_gtk_button_new("+"//c_null_char, clicked=c_funloc(oppress), &
       & data=c_loc(pplus))
  call hl_gtk_table_attach(keybox, kplus, 3_c_int, 1_c_int)
  kminus = hl_gtk_button_new("-"//c_null_char, clicked=c_funloc(oppress), &
       & data=c_loc(pminus))
  call hl_gtk_table_attach(keybox, kminus, 3_c_int, 2_c_int)
  ktimes = hl_gtk_button_new("*"//c_null_char, clicked=c_funloc(oppress), &
       & data=c_loc(ptimes))
  call hl_gtk_table_attach(keybox, ktimes, 3_c_int, 3_c_int)
  kdivide = hl_gtk_button_new("/"//c_null_char, clicked=c_funloc(oppress), &
       & data=c_loc(pdiv))
  call hl_gtk_table_attach(keybox, kdivide, 3_c_int, 4_c_int)
  kpower = hl_gtk_button_new("y<sup>x</sup>"//c_null_char, &
       & clicked=c_funloc(oppress), &
       & data=c_loc(ppow), is_markup=TRUE)
  call hl_gtk_table_attach(keybox, kpower, 3_c_int, 5_c_int)

  ! Clear entry
  kce = hl_gtk_button_new("CE"//c_null_char, clicked=c_funloc(cepress), &
       & tooltip="Clear entry box or top of stack"//c_null_char)
  call hl_gtk_table_attach(keybox, kce, 4_c_int, 1_c_int)

  ! Clear all
  kca = hl_gtk_button_new("CA"//c_null_char, clicked=c_funloc(capress), &
       & tooltip="Clear everything"//c_null_char)
  call hl_gtk_table_attach(keybox, kca, 4_c_int, 2_c_int)

  ! Move up
  kup = hl_gtk_button_new("↑"//c_null_char, clicked=c_funloc(uppress), &
       & tooltip="Move selected entry up stack"//c_null_char)
  call hl_gtk_table_attach(keybox, kup, 4_c_int, 3_c_int)

  ! Move down
  kdown = hl_gtk_button_new("↓"//c_null_char, clicked=c_funloc(downpress), &
       & tooltip="Move selected entry down stack"//c_null_char)
  call hl_gtk_table_attach(keybox, kdown, 4_c_int, 4_c_int)

  ! Roll stack
  kroll = hl_gtk_button_new("R↓"//c_null_char, clicked=c_funloc(rollpress), &
       & tooltip="Roll stack down"//c_null_char)
  call hl_gtk_table_attach(keybox, kroll, 4_c_int, 5_c_int)

  ! Functions

  ksin = hl_gtk_button_new("sin"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(psin))
  call hl_gtk_table_attach(keybox, ksin, 6_c_int, 1_c_int)

  kcos = hl_gtk_button_new("cos"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(pcos))
  call hl_gtk_table_attach(keybox, kcos, 6_c_int, 2_c_int)

  ktan = hl_gtk_button_new("tan"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(ptan))
  call hl_gtk_table_attach(keybox, ktan, 6_c_int, 3_c_int)

  kloge = hl_gtk_button_new("ln"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(pln))
  call hl_gtk_table_attach(keybox, kloge, 6_c_int, 4_c_int)

  ksqrt = hl_gtk_button_new("√x"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(psqrt))
  call hl_gtk_table_attach(keybox, ksqrt, 6_c_int, 5_c_int)

  ksinh = hl_gtk_button_new("sinh"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(psinh))
  call hl_gtk_table_attach(keybox, ksinh, 7_c_int, 1_c_int)

  kcosh = hl_gtk_button_new("cosh"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(pcosh))
  call hl_gtk_table_attach(keybox, kcosh, 7_c_int, 2_c_int)

  ktanh = hl_gtk_button_new("tanh"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(ptanh))
  call hl_gtk_table_attach(keybox, ktanh, 7_c_int, 3_c_int)

  klog10 = hl_gtk_button_new("log"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(pl10))
  call hl_gtk_table_attach(keybox, klog10, 7_c_int, 4_c_int)

  kinv = hl_gtk_button_new("1/X"//c_null_char, clicked=c_funloc(funpress), &
       & data=c_loc(pinv))
  call hl_gtk_table_attach(keybox, kinv, 7_c_int, 5_c_int)

  ! Mode selectors

  karc = hl_gtk_check_button_new("Inverse"//c_null_char, &
       & toggled=c_funloc(invtoggle), &
       & tooltip="Select/Deselect inverse functions"//c_null_char)
  call hl_gtk_table_attach(keybox, karc, 6_c_int, 0_c_int, xspan=2_c_int)

  kstats = hl_gtk_check_button_new("Live stats"//c_null_char, &
       & toggled=c_funloc(set_stats), &
       & tooltip="Select/Deselect live stack statistics"//c_null_char)
  call hl_gtk_table_attach(keybox, kstats, 4_c_int, 0_c_int, xspan=2_c_int)

  ktrigs = hl_gtk_combo_box_new(changed=c_funloc(set_trigunit), &
       & initial_choices=['Radians', 'Degrees','Grads  '], &
       & active=trigunit, tooltip= &
       & 'Select the unit for trigonometric functions'//c_null_char)
  call hl_gtk_table_attach(keybox, ktrigs, 0_c_int, 0_c_int, xspan=2_c_int)

  ! A pulldown for more obscure functions

  menu = hl_gtk_menu_new()
  call hl_gtk_table_attach(keybox, menu, 7_c_int, 6_c_int)

  pull = hl_gtk_menu_submenu_new(menu, "More"//c_null_char, &
       & tooltip="Less-used functions"//c_null_char)
  kabs = hl_gtk_menu_item_new(pull, "abs"//c_null_char, &
       & activate=c_funloc(funpress), &
       & data=c_loc(pabs), tooltip="Absolute value"//c_null_char)
  kaint = hl_gtk_menu_item_new(pull, "int"//c_null_char, &
       & activate=c_funloc(funpress), data=c_loc(pint), &
       & tooltip="Integer part"//c_null_char)
  kanint = hl_gtk_menu_item_new(pull, "round"//c_null_char, &
       & activate=c_funloc(funpress), data=c_loc(pnint), &
       & tooltip="Round to nearest integer"//c_null_char)
  kfrac = hl_gtk_menu_item_new(pull, "frac"//c_null_char, &
       & activate=c_funloc(funpress), data=c_loc(pfrac), &
       & tooltip="Fractional part"//c_null_char)
  katan2 = hl_gtk_menu_item_new(pull, "atan2"//c_null_char, &
       & activate=c_funloc(oppress), data=c_loc(patan2), &
       & tooltip="Arctan y/x with disambiguation"//c_null_char)
  kfact = hl_gtk_menu_item_new(pull, "factorial"//c_null_char, &
       & activate=c_funloc(funpress), data=c_loc(pfact), &
       & tooltip="X factorial"//c_null_char)
  kmod = hl_gtk_menu_item_new(pull, "mod"//c_null_char, &
       & activate=c_funloc(oppress), data=c_loc(pmod), &
       & tooltip = "Remainder of y/x"//c_null_char)
  khcf = hl_gtk_menu_item_new(pull, "HCF"//c_null_char, &
       & activate=c_funloc(oppress), data=c_loc(phcf), &
       & tooltip="Highest common factor of x & y"//c_null_char)
  klcm = hl_gtk_menu_item_new(pull, "LCM"//c_null_char, &
       & activate=c_funloc(oppress), data=c_loc(plcm), &
       & tooltip="Lowest common multiple of x & y"//c_null_char)
  junk = hl_gtk_menu_item_new(pull)
  khms= hl_gtk_menu_item_new(pull, "HMS"//c_null_char, &
       & activate=c_funloc(hmspress), &
       & tooltip="Display entry or top of stack in H:M:S format"//c_null_char)
  khex = hl_gtk_menu_item_new(pull, "Hexadecimal"//c_null_char, &
       & activate=c_funloc(base_display), data=c_loc(hexadecimal), &
       & tooltip="Show entry or top of stack in hexadecimal"//c_null_char)
  koct = hl_gtk_menu_item_new(pull, "Octal"//c_null_char, &
       & activate=c_funloc(base_display), data=c_loc(octal), &
       & tooltip="Show entry or top of stack in octal"//c_null_char)
  kbin = hl_gtk_menu_item_new(pull, "Binary"//c_null_char, &
       & activate=c_funloc(base_display), data=c_loc(binary), &
       & tooltip="Show entry or top of stack in binary"//c_null_char)

  ! A Pulldown for fundamental physics constants

  phys = hl_gtk_menu_new()
  call hl_gtk_table_attach(keybox, phys, 6_c_int, 6_c_int)
  fconst = hl_gtk_menu_submenu_new(phys, "Phys"//c_null_char, &
       & tooltip="Fundamental physics constants (SI)"//c_null_char)

  k_c = hl_gtk_menu_item_new(fconst, "c"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_c),&
       & tooltip="Speed of light"//c_null_char)
  k_e = hl_gtk_menu_item_new(fconst, "e"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_e), &
       & tooltip="Electronic charge"//c_null_char)
  k_h = hl_gtk_menu_item_new(fconst, "h"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_h), &
       & tooltip="Planck's constant"//c_null_char)
  k_hb = hl_gtk_menu_item_new(fconst, "ħ"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_hbar), &
       & tooltip="Planck's constant / 2π"//c_null_char)
  k_k = hl_gtk_menu_item_new(fconst, "k"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_k), &
       & tooltip="Boltzmann's constant"//c_null_char)
  k_g = hl_gtk_menu_item_new(fconst, "G"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_g), &
       & tooltip="Gravitational constant"//c_null_char)
  k_e0 = hl_gtk_menu_item_new(fconst, "ε<sub>0</sub>"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_e0), &
       & tooltip="Pemittivity of free space"//c_null_char, is_markup=TRUE)
  k_m0 = hl_gtk_menu_item_new(fconst, "μ<sub>0</sub>"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_m0), &
       & tooltip="Permeability of free space"//c_null_char, is_markup=TRUE)
  k_sb = hl_gtk_menu_item_new(fconst, "σ"//c_null_char, &
       &activate=c_funloc(add_const), data=c_loc(phys_sb), &
       & tooltip="Stefan-Boltzmann constant"//c_null_char)
  k_sep = hl_gtk_menu_item_new(fconst)

  k_me = hl_gtk_menu_item_new(fconst, 'm<sub>e</sub>'//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_me), &
       & tooltip="Electron mass"//c_null_char, is_markup=TRUE)
  k_mp = hl_gtk_menu_item_new(fconst, 'm<sub>p</sub>'//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_mp), &
       & tooltip="Proton mass"//c_null_char, is_markup=TRUE)
  k_mn = hl_gtk_menu_item_new(fconst, 'm<sub>n</sub>'//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_mn), &
       & tooltip="Neutron mass"//c_null_char, is_markup=TRUE)
  k_sep = hl_gtk_menu_item_new(fconst)

  k_na = hl_gtk_menu_item_new(fconst, "N<sub>a</sub>"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_na), &
       & tooltip="Avogadro's constant"//c_null_char, is_markup=TRUE)
  k_r = hl_gtk_menu_item_new(fconst, "R"//c_null_char, &
       & activate=c_funloc(add_const), data=c_loc(phys_r), &
       & tooltip="Gas constant"//c_null_char, is_markup=TRUE)

  ! Memory registers
  kmsto = hl_gtk_button_new("STO"//c_null_char, clicked=c_funloc(mempress), &
       & tooltip="Store to register"//c_null_char, data=c_loc(pmsto))
  call hl_gtk_table_attach(keybox, kmsto, 5_c_int, 1_c_int)
  kmrcl = hl_gtk_button_new("RCL"//c_null_char, clicked=c_funloc(mempress), &
       & tooltip="Recall register"//c_null_char, data=c_loc(pmrcl))
  call hl_gtk_table_attach(keybox, kmrcl, 5_c_int, 2_c_int)
  kmplus = hl_gtk_button_new("M+"//c_null_char, clicked=c_funloc(mempress), &
       & tooltip="Add to register"//c_null_char, data=c_loc(pmplus))
  call hl_gtk_table_attach(keybox, kmplus, 5_c_int, 3_c_int)
  kmminus = hl_gtk_button_new("M-"//c_null_char, clicked=c_funloc(mempress), &
       & tooltip="Subtract from register"//c_null_char, data=c_loc(pmminus))
  call hl_gtk_table_attach(keybox, kmminus, 5_c_int, 4_c_int)
  kmclr = hl_gtk_button_new("MCL"//c_null_char, clicked=c_funloc(mempress), &
       & tooltip="Clear register"//c_null_char, data=c_loc(pmclr))
  call hl_gtk_table_attach(keybox, kmclr, 5_c_int, 5_c_int)
  kmcla = hl_gtk_button_new("MCA"//c_null_char, clicked=c_funloc(mempress), &
       & tooltip="Clear all registers"//c_null_char, data=c_loc(pmcla))
  call hl_gtk_table_attach(keybox, kmcla, 5_c_int, 6_c_int)

  ! AN expander to show/hide the displays
  fexpand = gtk_expander_new("Displays"//c_null_char)
  call hl_gtk_box_pack(base, fexpand)
  call gtk_expander_set_expanded(fexpand, isopen)

  ! Notebook for stack & registers.
  mstabs = hl_gtk_notebook_new()
  call gtk_container_add(fexpand, mstabs)

  ! Stack display

  fstack = hl_gtk_listn_new(sstack, changed=c_funloc(stacksel), &
       & height=350_c_int, titles=(/ "Stack"//c_null_char /), &
       & types= (/g_type_double/))
  call hl_gtk_listn_set_cell_data_func(fstack, stackcol, &
       & func=c_funloc(show_list), &
       & data=c_loc(stackcol))

  idx = hl_gtk_notebook_add_page(mstabs, sstack, label="Stack"//c_null_char)

  ! Registers.
  jbase = hl_gtk_box_new()
  idx = hl_gtk_notebook_add_page(mstabs, jbase, &
       & label="Registers"//c_null_char) 
  fmemory = hl_gtk_listn_new(smemory, changed=c_funloc(memsel), &
       & height=300_c_int, titles= &
       & (/ "Index"//c_null_char, "Value"//c_null_char /), &
       & types = (/ g_type_int, g_type_double /))
  call hl_gtk_listn_set_cell_data_func(fmemory, memcol, &
       & func=c_funloc(show_list), data=c_loc(memcol))
  call hl_gtk_box_pack(jbase, smemory)

  jbase1 = hl_gtk_box_new(horizontal = TRUE)
  call hl_gtk_box_pack(jbase,jbase1, expand=FALSE)

  junk=gtk_label_new(c_null_char)
  call hl_gtk_box_pack(jbase1,junk)
  junk=gtk_label_new("Number of registers:"//c_null_char)
  call hl_gtk_box_pack(jbase1,junk, expand=FALSE)

  mem_spin = hl_gtk_spin_button_new(0_c_int, huge(1_c_int), &
       & initial_value=maxreg+1_c_int, &
       & value_changed=c_funloc(add_remove_registers), &
       & tooltip="Change the number of registers"//c_null_char)
  call hl_gtk_box_pack(jbase1, mem_spin, expand=FALSE)

  ! Set up display of registers.

  do i = 0, maxreg
     call hl_gtk_listn_ins(fmemory)
     call hl_gtk_listn_set_cell(fmemory, i, 0_c_int, ivalue=i)
     call hl_gtk_listn_set_cell(fmemory, i, 1_c_int, dvalue=0._c_double)
  end do

  ! Statistics
  jbase = hl_gtk_box_new()
  idx = hl_gtk_notebook_add_page(mstabs, jbase, &
       & label="Statistics"//c_null_char)
  fstats = hl_gtk_listn_new(sstats, &
       & height=300_c_int, titles=(/ "Statistic"//c_null_char, &
       & "Value"//c_null_char//"    " /), &
       & types = (/ g_type_string, g_type_double /), &
       & multiple=TRUE)
  call hl_gtk_listn_set_cell_data_func(fstats, statcol, &
       & func=c_funloc(show_list), &
       & data=c_loc(statcol))
  call hl_gtk_box_pack(jbase, sstats)

  do i = 0, 9
     call hl_gtk_listn_ins(fstats)
     call hl_gtk_listn_set_cell(fstats, i, 1_c_int, dvalue=0._c_double)
  end do
  call hl_gtk_listn_set_cell(fstats, 0_c_int, 0_c_int, &
       & svalue="N vals"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 1_c_int, 0_c_int, &
       & svalue="Mean"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 2_c_int, 0_c_int, &
       & svalue="Variance"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 3_c_int, 0_c_int, &
       & svalue="Std Dev"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 4_c_int, 0_c_int, &
       & svalue="Skew"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 5_c_int, 0_c_int, &
       & svalue="Kurtosis"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 6_c_int, 0_c_int, &
       & svalue="∑ x"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 7_c_int, 0_c_int, &
       & svalue="∑ x**2"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 8_c_int, 0_c_int, &
       & svalue="∑ x**3"//c_null_char)
  call hl_gtk_listn_set_cell(fstats, 9_c_int, 0_c_int, &
       & svalue="∑ x**4"//c_null_char)

  junk = hl_gtk_button_new("Copy to stack"//c_null_char, &
       & clicked = c_funloc(statsel), &
       & tooltip = "Copy the selected statistic(s) to the stack"//c_null_char)
  call hl_gtk_box_pack(jbase, junk, expand=FALSE)

  ! Realize
  call gtk_widget_show_all(win)

  ! If a restore file was set, restore it now.
  if (restfile /= '') call restore_all(restfile, status)

  ! End of interface creation
  ! Event loop
  call gtk_main()
end program rpncalc
