# Normally the adjustable parameters are passed from the top-level 
# Makefile.

# Which GTK library to use (set this to 2 or 3)
GTKVER=3

# Choose compile flags according to whether you want a debug or optimized build
FFLAGS_T=-O3
#FFLAGS_T=-g -Wall -Wno-unused-dummy-argument 

# On some 64-bit systems when gfortran is from a binary snapshot, there is an 
# error searching for libquadmath. If this happens to you, then uncomment the 
# non-empty value and replace /opt with wherever you unpacked the tarball.
QUADLIB=
#QUADLIB=-Xlinker -R/opt/gcc-trunk/lib64

# Installation prefix, i.e. the executable goes in $(PREFIX)/bin
PREFIX=/usr/local

# Fortran compiler. For gfortran it must be at least a v4.6 snapshot.
# If you have that and the distribution version, then you can specify a 
# full path here.
FC=gfortran

# Name extension (the executable becomes rpncalc$(NAME_SUFFIX))
# This would allow you to have both GTK2 & GTK3 versions to compare.
NAME_SUFFIX=

##############################################################
# You shouldn't normally need to change anything below here. #
##############################################################

FFLAGS=$(FFLAGS_T) `pkg-config --cflags gtk-$(GTKVER)-fortran`
LDFLAGS= $(QUADLIB) `pkg-config --libs gtk-$(GTKVER)-fortran`

BINDIR=$(PREFIX)/bin
SHRDIR=$(PREFIX)/share
APPDIR=$(SHRDIR)/applications

OBJS = rpncalc.o handlers.o widgets.o utils.o physics.o
PROG = RPNcalc$(NAME_SUFFIX)
DESKTOP=$(PROG).desktop

DOCDIR=$(SHRDIR)/doc/$(PROG)
TEXTFILE=$(DOCDIR)/$(PROG).txt
PDFFILE=$(DOCDIR)/$(PROG).pdf

all: $(PROG)

clean:
	-rm -f *.o *.mod $(PROG) *~ handlers.f90

$(PROG): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

install: $(PROG) $(BINDIR) $(APPDIR)
	install $(PROG) $(BINDIR)
	install $(DESKTOP) $(APPDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

$(APPDIR): 
	mkdir -p $(APPDIR)

uninstall:
	rm $(BINDIR)/$(PROG) $(APPDIR)/$(DESKTOP)

handlers.f90: handlers-tmpl.f90
	sed -e "s:@TEXTFILE@:$(TEXTFILE):" -e "s:@PDFFILE@:$(PDFFILE):" \
	handlers-tmpl.f90 > handlers.f90

# Dependencies
handlers.o:   widgets.o
handlers.o:   utils.o
rpncalc.o:   handlers.o
rpncalc.o:   widgets.o
rpncalc.o: physics.o
stack.o:   widgets.o
utils.o:   widgets.o

# Default compilation rule
%.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@
