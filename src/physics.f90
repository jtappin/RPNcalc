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

module physics
  ! Physics constants.
  ! Note that the rather odd layout of private parameters for the constants
  ! that are copied to protected target variables is because:
  ! a) parameters can't be targets. and
  ! b) variables can't be used in the initializer for the "derived" constants.

  use iso_c_binding
  
  implicit none

  real(kind=c_double), parameter :: pi = 3.1415926535897932384626433832795029_c_double

  ! basic constants

  real(kind=c_double), parameter, private :: par_c = 2.99792458e8_c_double
  real(kind=c_double), parameter, private :: par_e = 1.60217646e-19_c_double
  real(kind=c_double), parameter, private :: par_h = 6.626068e-34_c_double
  real(kind=c_double), parameter, private :: par_k = 1.3806503e-23_c_double
  real(kind=c_double), parameter, private :: par_g = 6.673e-11_c_double
  real(kind=c_double), parameter, private :: par_sb = 5.67051e-8_c_double
  real(kind=c_double), parameter, private :: par_m0 = 4.0e-7_c_double * pi
  real(kind=c_double), parameter, private :: par_na = 6.02214129e23_c_double
  real(kind=c_double), parameter, private :: par_me = 9.10938188e-31_c_double
  real(kind=c_double), parameter, private :: par_mp = 1.67262158e-27_c_double
  real(kind=c_double), parameter, private :: par_mn = 1.6749286e-27_c_double

  ! Copy to variables.

  real(kind=c_double), target, protected :: phys_c = par_c
  real(kind=c_double), target, protected :: phys_e = par_e
  real(kind=c_double), target, protected :: phys_h = par_h
  real(kind=c_double), target, protected :: phys_k = par_k
  real(kind=c_double), target, protected :: phys_g = par_g
  real(kind=c_double), target, protected :: phys_sb = par_sb
  real(kind=c_double), target, protected :: phys_m0 = par_m0
  real(kind=c_double), target, protected :: phys_na = par_na
  real(kind=c_double), target, protected :: phys_me = par_me
  real(kind=c_double), target, protected :: phys_mp = par_mp
  real(kind=c_double), target, protected :: phys_mn = par_mn

  ! Derived
  real(kind=c_double), target, protected :: phys_hbar = &
       & par_h / (2._c_double * pi)
  real(kind=c_double), target, protected :: phys_e0 = &
       & 1._c_double / (par_c**2 * par_m0)
  real(kind=c_double), target, protected :: phys_r = par_na * par_k

end module physics
