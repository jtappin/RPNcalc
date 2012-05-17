# RPNcalc Makefile

# Where to install everything.
#   The binaries will go into $(PREFIX)/bin
#   The desktop file into $(PREFIX)/share/applications
#   The man page into $(PREFIX)/man/man1
#   The text & pdf documents into $(PREFIX)/share/doc/rpncalc

PREFIX=/usr/local

# Which GTK library to use (set this to 2 or 3)
GTKVER=3

# Fortran compiler. For gfortran it must be at least a v4.6 snapshot.
# If you have that and an earlier distribution version, then you can specify a 
# full path here.
FC=gfortran

# Choose compile flags according to whether you want a debug or optimized build
FFLAGS_T=-O3
#FFLAGS_T=-g -Wall -Wno-unused-dummy-argument 

# On some 64-bit systems when gfortran is from a binary snapshot, there is an 
# error searching for libquadmath. If this happens to you, then uncomment the 
# non-empty value and replace /opt with wherever you unpacked the tarball.
QUADLIB=
#QUADLIB=-Xlinker -R/opt/gcc-trunk/lib64

# Name extension (the executable becomes rpncalc$(NAME_SUFFIX))
# This would allow you to have both GTK2 & GTK3 versions to compare.
NAME_SUFFIX=

##############################################################
# You shouldn't normally need to change anything below here. #
##############################################################

SUBDIRS=docs src

all:
	for dir in $(SUBDIRS); do \
	$(MAKE) $(MFLAGS) -C $$dir "PREFIX=$(PREFIX)" \
	"FC=$(FC)" "QUADLIB=$(QUADLIB)" "GTKVER=$(GTKVER)" \
	"FFLAGS_T=$(FFLAGS_T)" "NAME_SUFFIX=$(NAME_SUFFIX)"; \
	done

install:
	for dir in $(SUBDIRS); do \
	$(MAKE) $(MFLAGS) -C $$dir "PREFIX=$(PREFIX)" \
	"FC=$(FC)" "QUADLIB=$(QUADLIB)" "GTKVER=$(GTKVER)" \
	"FFLAGS_T=$(FFLAGS_T)" "NAME_SUFFIX=$(NAME_SUFFIX)" install; \
	done

clean:
	for dir in $(SUBDIRS); do \
	$(MAKE) $(MFLAGS) -C $$dir "NAME_SUFFIX=$(NAME_SUFFIX)" clean; \
	done

uninstall:
	for dir in $(SUBDIRS); do \
	$(MAKE) $(MFLAGS) -C $$dir "PREFIX=$(PREFIX)" \
	"NAME_SUFFIX=$(NAME_SUFFIX)" uninstall; \
	done
