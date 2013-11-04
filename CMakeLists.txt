# Copyright (C) 2013 James Tappin
#
# This file is part of the RPNcalc scientific calculator.
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License along with
# this program; see the files COPYING3 and COPYING.RUNTIME respectively.
# If not, see <http://www.gnu.org/licenses/>.

cmake_minimum_required(VERSION 2.8.5)
project(RPNcalc Fortran)

set(NAME RPNcalc)

# Note that we override the lib directory here because most
# 64-bit systems don't actually work if we use the default.
include(GNUInstallDirs)
set(CMAKE_INSTALL_LIBDIR lib)

# Uncomment this to debug or use "cmake -D CMAKE_BUILD_TYPE=debug .."
# set(CMAKE_BUILD_TYPE debug)
# set(CMAKE_BUILD_TYPE release)

set(CMAKE_Fortran_FLAGS_DEBUG
"-g -pthread -Wall -Wtabs -fcheck-array-temporaries -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow -Wno-unused-dummy-argument")
set(CMAKE_Fortran_FLAGS_RELEASE
"-pthread -O3 -mtune=native -march=native")

# Packaging stuff I don't *YET* understand.
# set(CPACK_PACKAGE_VERSION "0.1.0")
# set(CPACK_PACKAGE_INSTALL_DIRECTORY "rpncalc")
# set(CPACK_GENERATOR "STGZ;TGZ")
# set(CPACK_SOURCE_GENERATOR "STGZ;TGZ")
# include(CPack)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

# uninstall target
configure_file(
	"${PROJECT_SOURCE_DIR}/cmake/cmake_uninstall.cmake.in"
	"${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
	IMMEDIATE @ONLY)
add_custom_target(uninstall COMMAND ${CMAKE_COMMAND} -P
${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)

enable_testing()

# Find Gtk-Fortran, if Gtk3 is preferred, then switch the order of
# the scans below, or to use only one version use -DGTK=2 or -DGTK=3.

if (GTK EQUAL 2)
  find_package(Gtk2Fortran)
  if (NOT GTK2FORTRAN_FOUND)
    message (FATAL_ERROR "Gtk2Fortran not found, aborting")
  endif()
elseif (GTK EQUAL 3) 
  find_package(Gtk3Fortran)
  if (NOT GTK3FORTRAN_FOUND)
    message (FATAL_ERROR "Gtk3Fortran not found, aborting")
  endif()
else ()
  find_package(Gtk2Fortran)
  if (NOT GTK2FORTRAN_FOUND)
    message(STATUS "Gtk2Fortran not found trying Gtk3Fortran")
    find_package(Gtk3Fortran)
    if (NOT GTK3FORTRAN_FOUND)
      message (FATAL_ERROR "Gtk3Fortran Not found either, aborting")
    endif()
  endif()
endif()

find_program(LATEX2MAN latex2man)
if (NOT LATEX2MAN)
  message(WARNING "latex2man not found cannot regenerate documentation")
endif()

find_program(PDFLATEX pdflatex)
if (NOT PDFLATEX)
  message(WARNING "pdflatex not found cannot regenerate PDF manual")
endif()

add_subdirectory(src)
add_subdirectory(docs)
