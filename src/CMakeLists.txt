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

set(sources 
  rpncalc.f90
  widgets.f90
  utils.f90
  physics.f90
  )

include_directories("/usr/include")
include_directories(${GTKFORTRAN_MODULE_DIR})
include_directories("/usr/local/include")

link_directories(${GTKFORTRAN_LIBRARY_DIRS})
link_directories(/usr/local/lib)

set(TEXTFILE ${CMAKE_INSTALL_FULL_DOCDIR}/${NAME}.txt)
set(PDFFILE ${CMAKE_INSTALL_FULL_DOCDIR}/${NAME}.pdf)
configure_file(handlers-tmpl.f90  handlers.f90)

add_executable(${NAME} ${sources} ${CMAKE_CURRENT_BINARY_DIR}/handlers.f90) 
target_link_libraries(RPNcalc ${GTKFORTRAN_LIBRARIES})

install(FILES
  "${CMAKE_CURRENT_SOURCE_DIR}/${NAME}.desktop"
  DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/applications)

install(PROGRAMS
  "${CMAKE_CURRENT_BINARY_DIR}/${NAME}"
  DESTINATION ${CMAKE_INSTALL_BINDIR})
