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

  ! This source file contains the main program that creates the widgets.

  use iso_c_binding !, only: c_ptr, c_null_ptr, c_loc
  use gtk, only: gtk_button_new, gtk_check_button_new, gtk_container_add, gtk_ent&
       &ry_new, gtk_expander_new, gtk_label_new, gtk_main, gtk_menu_item_new, gtk_menu&
       &_new, gtk_radio_button_new, gtk_statusbar_new, gtk_table_attach, gtk_table_new&
       &, gtk_widget_show, gtk_widget_show_all, gtk_window_new, gtk_init,&
       & gtk_expander_set_expanded, GTK_PACK_DIRECTION_LTR
  use gtk_hl

  use handlers
  use widgets
  implicit none

  integer :: i
  character, dimension(10), target :: pnum
  integer(kind=c_int), target :: pplus= OP_PLUS, pminus=OP_MINUS, &
       & ptimes=OP_TIMES, pdiv=OP_DIVIDE, ppow=OP_POWER, patan2=FUN_ATAN2

  integer(kind=c_int), target :: psin=FUN_SIN, pcos=FUN_COS, ptan=FUN_TAN,&
       & pln=FUN_LN, psqrt=FUN_SQRT, psinh=FUN_SINH, pcosh=FUN_COSH, &
       & ptanh=FUN_TANH, pl10=FUN_LOG10, pinv=FUN_INV, pabs=FUN_ABS, &
       & pint=FUN_INT, pfrac=FUN_FRAC, pfact=FUN_FACTORIAL

  integer(kind=c_int), target :: pmsto=MEM_STO, pmrcl=MEM_RCL, &
       & pmplus=MEM_PLUS, pmminus=MEM_MINUS, pmclr=MEM_CLR, pmcla=MEM_CLA

  integer(kind=c_int), target :: stackcol=0, memcol=1, statcol=1

  ! Workspace variables
  character(len=10) :: ws ! workspace for number button labels
  integer :: ix, iy
  integer(kind=c_int) :: idx

  ! Creating the interface

  ! Initialise gtk
  call gtk_init()

  ! Create a window and put a vertical box into it
  win = hl_gtk_window_new("RPN Calculator", destroy=c_funloc(my_destroy), &
       & resizable=TRUE)
  base = hl_gtk_box_new()
  call gtk_container_add(win, base)

  ! Menu bar
  fmenu = hl_gtk_menu_new(orientation=GTK_PACK_DIRECTION_LTR)
  call hl_gtk_box_pack(base, fmenu)
  ffmenu = hl_gtk_menu_submenu_new(fmenu, "File"//cnull)
  ksave = hl_gtk_menu_item_new(ffmenu, "Save"//cnull, &
       & activate=c_funloc(save_values))
  krestore = hl_gtk_menu_item_new(ffmenu, "Restore"//cnull, &
       & activate=c_funloc(restore_values))
  kquit = hl_gtk_menu_item_new(ffmenu, "Quit"//cnull, &
       & activate=c_funloc(my_destroy))

  femenu = hl_gtk_menu_submenu_new(fmenu, "Edit"//cnull)
  kfedit = hl_gtk_menu_item_new(femenu, "Result Format"//cnull, &
       & activate=c_funloc(set_format_make))
  kefocus = hl_gtk_check_menu_item_new(femenu, "Hold entry focus"//cnull, &
       & toggled = c_funloc(set_entry_focus))

  fhmenu = hl_gtk_menu_submenu_new(fmenu, "Help"//cnull)
  khelp = hl_gtk_menu_item_new(fhmenu, "Help"//cnull, &
       & activate=c_funloc(show_help))
  kabout = hl_gtk_menu_item_new(fhmenu, "About: RPN Calculator"//cnull, &
       & activate=c_funloc(about_rpn))
  kfabout = hl_gtk_menu_item_new(fhmenu, "About: Gtk-Fortran"//cnull, &
       & activate=c_funloc(about_gtkfortran))

  ! Value entry window.
  jbase = hl_gtk_table_new(2, 2)
  call hl_gtk_box_pack(base, jbase)
  junk=gtk_label_new("Enter:"//cnull)
  call hl_gtk_table_attach(jbase, junk, 0, 0, xopts=0)
  fentry = hl_gtk_entry_new(editable=TRUE, activate=c_funloc(enter_value), &
       & tooltip="Enter values here"//CNULL, &
       & insert_text=c_funloc(char_entered), &
       & delete_text=c_funloc(char_deleted), &
       & len=40)
  call hl_gtk_table_attach(jbase, fentry, 1, 0)

  ! result window. 
  junk=gtk_label_new("Result:"//cnull)
  call hl_gtk_table_attach(jbase, junk, 0, 1, xopts=0)
  fresult = hl_gtk_entry_new(editable=FALSE, &
       & tooltip="Results displayed here"//CNULL)
  call hl_gtk_table_attach(jbase, fresult, 1, 1)

  ! A status message bar
  fstatus = gtk_statusbar_new()
  call hl_gtk_box_pack(base, fstatus)

  ! Keypad

  keybox = hl_gtk_table_new(7,8,homogeneous=true)
  call hl_gtk_box_pack(base, keybox)

  ! numbers

  do i=1,10
     write(ws,"('    ',I1)") i-1
     write(pnum(i),"(I1)") i-1
     knum(i) = hl_gtk_button_new(ws//CNULL, clicked=c_funloc(numpress), &
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

  kpoint = hl_gtk_button_new("."//CNULL, clicked=c_funloc(dppress))
  call hl_gtk_table_attach(keybox, kpoint, 1,4)

  kchs = hl_gtk_button_new("+/-"//CNULL, clicked=c_funloc(chspress), &
       & tooltip="Change Sign"//CNULL)
  call hl_gtk_table_attach(keybox, kchs, 2, 4)

  kee = hl_gtk_button_new("EE"//CNULL, clicked=c_funloc(eepress), &
       & tooltip= "Enter Exponent"//CNULL)
  call hl_gtk_table_attach(keybox, kee, 0, 5)

  kpi = hl_gtk_button_new("π"//cnull, clicked=c_funloc(pipress),&
       & tooltip="Enter π"//cnull)
  call hl_gtk_table_attach(keybox, kpi, 1, 5)

  ! Delete character from entry box
  kdel = hl_gtk_button_new("Del"//cnull, clicked=c_funloc(delpress), &
       & tooltip="Delete last char"//cnull)
  call hl_gtk_table_attach(keybox, kdel, 2, 5)

  ! Enter and duplicate entry

  kenter = hl_gtk_button_new("Enter"//cnull, clicked=c_funloc(enter_value), &
       & tooltip="Move entry to stack"//cnull)
  call hl_gtk_table_attach(keybox, kenter, 0, 6, xspan=3)
  kdup = hl_gtk_button_new("Duplicate"//cnull, clicked=c_funloc(duppress), &
       & tooltip="Copy entry to stack"//cnull)
  call hl_gtk_table_attach(keybox, kdup, 3, 6, xspan=2)

  ! Operations

  kplus = hl_gtk_button_new("+"//cnull, clicked=c_funloc(oppress), &
       & data=c_loc(pplus))
  call hl_gtk_table_attach(keybox, kplus, 3, 1)
  kminus = hl_gtk_button_new("-"//cnull, clicked=c_funloc(oppress), &
       & data=c_loc(pminus))
  call hl_gtk_table_attach(keybox, kminus, 3, 2)
  ktimes = hl_gtk_button_new("*"//cnull, clicked=c_funloc(oppress), &
       & data=c_loc(ptimes))
  call hl_gtk_table_attach(keybox, ktimes, 3, 3)
  kdivide = hl_gtk_button_new("/"//cnull, clicked=c_funloc(oppress), &
       & data=c_loc(pdiv))
  call hl_gtk_table_attach(keybox, kdivide, 3, 4)
  kpower = hl_gtk_button_new("**"//cnull, clicked=c_funloc(oppress), &
       & data=c_loc(ppow))
  call hl_gtk_table_attach(keybox, kpower, 3, 5)

  ! Clear entry
  kce = hl_gtk_button_new("CE"//cnull, clicked=c_funloc(cepress), &
       & tooltip="Clear entry box or top of stack"//cnull)
  call hl_gtk_table_attach(keybox, kce, 4, 1)

  ! Clear all
  kca = hl_gtk_button_new("CA"//cnull, clicked=c_funloc(capress), &
       & tooltip="Clear everything"//cnull)
  call hl_gtk_table_attach(keybox, kca, 4, 2)

  ! Move up
  kup = hl_gtk_button_new("↑"//cnull, clicked=c_funloc(uppress), &
       & tooltip="Move selected entry up stack"//cnull)
  call hl_gtk_table_attach(keybox, kup, 4, 3)

  ! Move down
  kdown = hl_gtk_button_new("↓"//cnull, clicked=c_funloc(downpress), &
       & tooltip="Move selected entry down stack"//cnull)
  call hl_gtk_table_attach(keybox, kdown, 4, 4)

  ! Roll stack
  kroll = hl_gtk_button_new("R↓"//cnull, clicked=c_funloc(rollpress), &
       & tooltip="Roll stack down"//cnull)
  call hl_gtk_table_attach(keybox, kroll, 4, 5)

  ! Functions

  ksin = hl_gtk_button_new("sin"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(psin))
  call hl_gtk_table_attach(keybox, ksin, 6, 1)

  kcos = hl_gtk_button_new("cos"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(pcos))
  call hl_gtk_table_attach(keybox, kcos, 6, 2)

  ktan = hl_gtk_button_new("tan"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(ptan))
  call hl_gtk_table_attach(keybox, ktan, 6, 3)

  kloge = hl_gtk_button_new("ln"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(pln))
  call hl_gtk_table_attach(keybox, kloge, 6, 4)

  ksqrt = hl_gtk_button_new("√x"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(psqrt))
  call hl_gtk_table_attach(keybox, ksqrt, 6, 5)

  khms= hl_gtk_button_new("HMS"//cnull, clicked=c_funloc(hmspress), &
       & tooltip="Display entry or top of stack in H:M:S format"//cnull)
  call hl_gtk_table_attach(keybox, khms, 6, 6)

  ksinh = hl_gtk_button_new("sinh"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(psinh))
  call hl_gtk_table_attach(keybox, ksinh, 7, 1)

  kcosh = hl_gtk_button_new("cosh"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(pcosh))
  call hl_gtk_table_attach(keybox, kcosh, 7, 2)

  ktanh = hl_gtk_button_new("tanh"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(ptanh))
  call hl_gtk_table_attach(keybox, ktanh, 7, 3)

  klog10 = hl_gtk_button_new("log"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(pl10))
  call hl_gtk_table_attach(keybox, klog10, 7, 4)

  kinv = hl_gtk_button_new("1/X"//cnull, clicked=c_funloc(funpress), &
       & data=c_loc(pinv))
  call hl_gtk_table_attach(keybox, kinv, 7, 5)

  ! Mode selectors

  karc = hl_gtk_check_button_new("Inverse"//cnull, &
       & toggled=c_funloc(invtoggle), &
       & tooltip="Select/Deselect inverse functions"//cnull)
  call hl_gtk_table_attach(keybox, karc, 6, 0, xspan=2)

  kstats = hl_gtk_check_button_new("Live stats"//cnull, &
       & toggled=c_funloc(set_stats), &
       & tooltip="Select/Deselect live stack statistics"//cnull)
  call hl_gtk_table_attach(keybox, kstats, 4, 0, xspan=2)

  rdgrp=NULL
  krad = hl_gtk_radio_button_new(rdgrp, "Rad"//cnull, &
       & toggled=c_funloc(set_trigunit), &
       & tooltip = "Select radians mode"//cnull)
  call hl_gtk_table_attach(keybox, krad, 0, 0)
  kdeg = hl_gtk_radio_button_new(rdgrp, "Deg"//cnull, &
       & toggled=c_funloc(set_trigunit), &
       & tooltip = "Select degrees mode"//cnull)
  call hl_gtk_table_attach(keybox, kdeg, 1, 0)
  kgrad = hl_gtk_radio_button_new(rdgrp, "Grad"//cnull, &
       & toggled=c_funloc(set_trigunit), &
       & tooltip = "Select grads mode"//cnull)
  call hl_gtk_table_attach(keybox, kgrad, 2, 0)

  call hl_gtk_radio_group_set_select(rdgrp, trigunit)

  ! A pulldown for more obscure functions

  menu = hl_gtk_menu_new()
  call hl_gtk_table_attach(keybox, menu, 7, 6)

  pull = hl_gtk_menu_submenu_new(menu, "More>"//cnull)
  kabs = hl_gtk_menu_item_new(pull, "abs"//cnull, activate=c_funloc(funpress), &
       & data=c_loc(pabs))
  kaint = hl_gtk_menu_item_new(pull, "int"//cnull, &
       & activate=c_funloc(funpress), data=c_loc(pint))
  kfrac = hl_gtk_menu_item_new(pull, "frac"//cnull, &
       &activate=c_funloc(funpress), data=c_loc(pfrac))
  katan2 = hl_gtk_menu_item_new(pull, "atan2"//cnull, &
       & activate=c_funloc(oppress), data=c_loc(patan2))
  kfact = hl_gtk_menu_item_new(pull, "factorial"//cnull, &
       & activate=c_funloc(funpress), data=c_loc(pfact))

  ! Memory registers
  kmsto = hl_gtk_button_new("STO"//cnull, clicked=c_funloc(mempress), &
       & tooltip="Store to register"//cnull, data=c_loc(pmsto))
  call hl_gtk_table_attach(keybox, kmsto, 5, 1)
  kmrcl = hl_gtk_button_new("RCL"//cnull, clicked=c_funloc(mempress), &
       & tooltip="Recall register"//cnull, data=c_loc(pmrcl))
  call hl_gtk_table_attach(keybox, kmrcl, 5, 2)
  kmplus = hl_gtk_button_new("M+"//cnull, clicked=c_funloc(mempress), &
       & tooltip="Add to register"//cnull, data=c_loc(pmplus))
  call hl_gtk_table_attach(keybox, kmplus, 5, 3)
  kmminus = hl_gtk_button_new("M-"//cnull, clicked=c_funloc(mempress), &
       & tooltip="Subtract from register"//cnull, data=c_loc(pmminus))
  call hl_gtk_table_attach(keybox, kmminus, 5, 4)
  kmclr = hl_gtk_button_new("MCL"//cnull, clicked=c_funloc(mempress), &
       & tooltip="Clear register"//cnull, data=c_loc(pmclr))
  call hl_gtk_table_attach(keybox, kmclr, 5, 5)
  kmcla = hl_gtk_button_new("MCA"//cnull, clicked=c_funloc(mempress), &
       & tooltip="Clear all registers"//cnull, data=c_loc(pmcla))
  call hl_gtk_table_attach(keybox, kmcla, 5, 6)

  ! Notebook for stack & registers.
  mstabs = hl_gtk_notebook_new()
  call hl_gtk_box_pack(base, mstabs)

  ! Stack display

  fstack = hl_gtk_listn_new(sstack, changed=c_funloc(stacksel), &
       & height=350, titles=(/ "Stack"//cnull /), types= (/g_type_double/))
  call hl_gtk_listn_set_cell_data_func(fstack, stackcol, func=c_funloc(show_list), &
       & data=c_loc(stackcol))

  idx = hl_gtk_notebook_add_page(mstabs, sstack, label="Stack"//cnull)

  ! Registers.
  fmemory = hl_gtk_listn_new(smemory, changed=c_funloc(memsel), &
       & height=350, titles= (/ "Index"//cnull, "Value"//cnull /), &
       & types = (/ g_type_int, g_type_double /))
  call hl_gtk_listn_set_cell_data_func(fmemory, memcol, func=c_funloc(show_list), &
       & data=c_loc(memcol))
  idx = hl_gtk_notebook_add_page(mstabs, smemory, label="Registers"//cnull)

  ! Set up display of registers.

  do i = 0, maxreg
     call hl_gtk_listn_ins(fmemory)
     call hl_gtk_listn_set_cell(fmemory, i, 0, ivalue=i)
     call hl_gtk_listn_set_cell(fmemory, i, 1, dvalue=0._c_double)
  end do

  ! Statistics
  fstats = hl_gtk_listn_new(sstats, changed=c_funloc(statsel),&
       & height=350, titles=(/ "Statistic"//cnull, "Value"//cnull//"    " /), &
       & types = (/ g_type_string, g_type_double /))
  call hl_gtk_listn_set_cell_data_func(fstats, statcol, func=c_funloc(show_list), &
       & data=c_loc(statcol))
  idx = hl_gtk_notebook_add_page(mstabs, sstats, label="Statistics"//cnull)

  do i = 0, 9
     call hl_gtk_listn_ins(fstats)
     call hl_gtk_listn_set_cell(fstats, i, 1, dvalue=0._c_double)
  end do
  call hl_gtk_listn_set_cell(fstats, 0, 0, svalue="N vals"//cnull)
  call hl_gtk_listn_set_cell(fstats, 1, 0, svalue="Mean"//cnull)
  call hl_gtk_listn_set_cell(fstats, 2, 0, svalue="Variance"//cnull)
  call hl_gtk_listn_set_cell(fstats, 3, 0, svalue="Std Dev"//cnull)
  call hl_gtk_listn_set_cell(fstats, 4, 0, svalue="Skew"//cnull)
  call hl_gtk_listn_set_cell(fstats, 5, 0, svalue="Kurtosis"//cnull)
  call hl_gtk_listn_set_cell(fstats, 6, 0, svalue="∑ x"//cnull)
  call hl_gtk_listn_set_cell(fstats, 7, 0, svalue="∑ x**2"//cnull)
  call hl_gtk_listn_set_cell(fstats, 8, 0, svalue="∑ x**3"//cnull)
  call hl_gtk_listn_set_cell(fstats, 9, 0, svalue="∑ x**4"//cnull)

  ! Realize
  call gtk_widget_show_all(win)

  ! End of interface creation
  ! Event loop
  call gtk_main()
end program rpncalc
