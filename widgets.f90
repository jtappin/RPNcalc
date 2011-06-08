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

module widgets
  ! Global place to store the widget ids and other global values needed by RPNcalc.

  use iso_c_binding
  use gtk, only: NULL

  implicit none

  ! Enumerators for operations and functions

  enum, bind(c)  ! Operator identifiers
     enumerator :: OP_NONE
     enumerator :: OP_PLUS
     enumerator :: OP_MINUS
     enumerator :: OP_TIMES
     enumerator :: OP_DIVIDE
     enumerator :: OP_POWER
     enumerator :: FUN_ATAN2      ! Behaves like an operator (2 args)
  end enum

  enum, bind(c)  ! Function identifiers
     enumerator :: FUN_SIN
     enumerator :: FUN_COS
     enumerator :: FUN_TAN
     enumerator :: FUN_LN
     enumerator :: FUN_SQRT
     enumerator :: FUN_SINH
     enumerator :: FUN_COSH
     enumerator :: FUN_TANH
     enumerator :: FUN_LOG10
     enumerator :: FUN_INV
     enumerator :: FUN_ABS
     enumerator :: FUN_INT
     enumerator :: FUN_FRAC
     enumerator :: FUN_FACTORIAL
  end enum
 
  enum, bind(c) ! Memory operations
     enumerator :: MEM_STO
     enumerator :: MEM_RCL
     enumerator :: MEM_PLUS
     enumerator :: MEM_MINUS
     enumerator :: MEM_CLR
     enumerator :: MEM_CLA
  end enum

  ! Number of memory registers
  integer(kind=c_int), parameter :: maxreg = 9

  ! number keys
  type(c_ptr), dimension(10) :: knum

  ! Other keys
  type(c_ptr) :: kpoint, kplus, kminus, ktimes, kdivide, kce, &
       & kca, kquit, kenter, kchs, kup, kdown, kee, ksin, kcos, ktan, &
       & ksqrt, kinv, kdup, karc, kloge, klog10, kpi, ksinh, kcosh, &
       & ktanh, kpower, kroll, kdel, khms, kstats

  ! The radians/degrees radio menu.
  type(c_ptr) :: krad, kdeg, kgrad, rdgrp

  ! The pulldown menu for "obscure" functions
  type(c_ptr) :: menu, pull, kabs, kaint, kfrac, katan2, kfact

  ! Menubar
  type(c_ptr) :: fmenu, ffmenu, fhmenu, kabout, ksave, krestore, khelp, &
       & kfabout, femenu, kfedit, kefocus

  ! Entry & result windows and stack display
  type(c_ptr) :: fentry, fstack, sstack, fresult, sbar, fstatus, &
       & fmemory, smemory, fstats, sstats, fexpand

  ! Windows & containers (and title label)
  type(c_ptr) :: win, base, keybox, mstabs, help_window, &
       & jbase, junk

  ! Memory keys
  type(c_ptr) :: kmsto, kmrcl, kmplus, kmminus, kmclr, kmcla

  ! Modes & flags
  logical ::  isinv=.false., focus_entry=.false.
  integer(kind=c_int) :: trigunit = 1

  ! Flags for "non-repeatable" elements in a number entry.
  logical :: decimal_present=.false., exponent_present=.false.

  ! Current selection on the stack
  integer(kind=c_int) :: stack_selected = -1, mem_selected = -1

  ! The display format
  type(c_ptr) :: fmt_window, fmt_choose, fmt_precision, fmt_expsize
  integer(kind=c_int) :: fmt_type=4, fmt_decimal=5, fmt_expplaces=2
  character(len=20) :: result_format=""

  ! Statistics
  real(kind=c_double) :: s1=0._c_double, s2=0._c_double, &
       & s3=0._c_double, s4=0._c_double
  real(kind=c_double) :: avg, var, sdev, skew, kurt, val
  logical :: dynamic_stats = .FALSE.

end module widgets
