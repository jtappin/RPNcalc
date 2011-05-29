# Choose compile flags according to whether you want a debug or optimized build
#FFLAGS=-g -Wall -Wno-unused-dummy-argument `pkg-config --cflags gtk-fortran`
FFLAGS=-O3 `pkg-config --cflags gtk-fortran`

# On some 64-bit systems when gfortran is from a binary snapshot, there is an 
# error searching for libquadmath. If this happens to you, then uncomment the 
# non-empty value and replace /opt with wherever you unpacked the tarball.
#QUADLIB=-Xlinker -R/opt/gcc-trunk/lib64
QUADLIB=

# Installation prefix, i.e. the executable goes on $(PREFIX)/bin
PREFIX=/usr/local

# Fortran compiler. For gfortran it must be at least a v4.6 snapshot.
# If you have that and the distribution version, then you can specify a 
# full path here.
FC=gfortran

##############################################################
# You shouldn't normally need to change anything below here. #
##############################################################

LDFLAGS= $(QUADLIB) `pkg-config --libs gtk-fortran`
BINDIR=$(PREFIX)/bin
SHRDIR=$(PREFIX)/share
APPDIR=$(SHRDIR)/applications

OBJS = rpncalc.o handlers.o widgets.o utils.o
PROG = rpncalc
DESKTOP=$(PROG).desktop

all: $(PROG)

clean:
	-rm -f *.o *.mod $(PROG) *~

$(PROG): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

install: $(PROG) $(BINDIR) $(APPDIR)
	install $(PROG) $(BINDIR)
	install $(DESKTOP) $(APPDIR)

$(BINDIR):
	mkdir $(BINDIR)

$(APPDIR): $(SHRDIR)
	mkdir $(APPDIR)

$(SHRDIR):
	mkdir $(SHRDIR)

uninstall:
	rm $(BINDIR)/$(PROG) $(SHRDIR)/$(DESKTOP)

# Dependencies
handlers.o:   widgets.o
handlers.o:   utils.o
rpncalc.o:   handlers.o
rpncalc.o:   widgets.o
stack.o:   widgets.o
utils.o:   widgets.o

# Default compilation rule
%.o: %.f90
	$(FC) -c $(FFLAGS) $< -o $@
