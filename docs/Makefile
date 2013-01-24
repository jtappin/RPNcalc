# Makefile for the RPNCalc documentation.
PREFIX=/usr/local

##############################################################
# You shouldn't normally need to change anything below here. #
##############################################################

NAME=RPNcalc$(NAME_SUFFIX)
SOURCE=$(NAME).tex
MAN=$(NAME).1
TEXT=$(NAME).txt
PDF=$(NAME).pdf

DOCDIR=$(PREFIX)/share/doc/$(NAME)
MANDIR=$(PREFIX)/man/man1

all: $(MAN) $(TEXT) $(PDF)

$(MAN): $(SOURCE)
	latex2man $(SOURCE) $(MAN)

$(TEXT): $(MAN)
	man ./$(MAN) > $(TEXT)

$(PDF): $(SOURCE)
	pdflatex $(SOURCE)

install: $(MAN) $(TEXT) $(PDF) $(DOCDIR) $(MANDIR)
	install $(MAN) $(MANDIR)
	install $(TEXT) $(PDF) $(DOCDIR)

$(DOCDIR):
	mkdir -p $(DOCDIR)

$(MANDIR):
	mkdir -p $(MANDIR)

clean:
	rm $(MAN) $(TEXT) $(PDF)

uninstall:
	rm $(MANDIR)/$(MAN) $(DOCDIR)/$(TEXT) $(DOCDIR)/$(PDF)
