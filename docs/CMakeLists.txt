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

if (LATEX2MAN)
  message(STATUS "Generating documentation")
  
  add_custom_command(
    OUTPUT rpncalc.1
    COMMAND ${LATEX2MAN} ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.tex ${CMAKE_CURRENT_BINARY_DIR}/rpncalc.1
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.tex
    )
else()
  add_custom_command(
    OUTPUT rpncalc.1
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.1 ${CMAKE_CURRENT_BINARY_DIR}/rpncalc.1
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.1
    )
endif()

add_custom_command(
  OUTPUT rpncalc.txt
  COMMAND man ${CMAKE_CURRENT_BINARY_DIR}/rpncalc.1 > ${CMAKE_CURRENT_BINARY_DIR}/rpncalc.txt
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/rpncalc.1
  )

if (PDFLATEX)
  add_custom_command(
    OUTPUT rpncalc.pdf
    COMMAND ${PDFLATEX} -output-directory ${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.tex
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.tex
    )
else()
  add_custom_command(
    OUTPUT  rpncalc.pdf
    COMMAND cp ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.pdf ${CMAKE_CURRENT_BINARY_DIR}/rpncalc.pdf
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/rpncalc.pdf
    )
endif()


add_custom_target(helptexts ALL DEPENDS rpncalc.1 rpncalc.pdf rpncalc.txt)

install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/rpncalc.pdf"
  "${CMAKE_CURRENT_BINARY_DIR}/rpncalc.txt"
  DESTINATION ${CMAKE_INSTALL_DOCDIR}
)
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/rpncalc.1"
  DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
)

