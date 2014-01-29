# Automatically generated GNU Makefile. Qua Set  1 15:56:23 BRT 2010
# fgen v0.3 (C) 1997,98 Beroud Jean-Marc

# standard
SHELL     = sh
MAKE      = make
MAKEFILE  = Makefile
MAKEFLAGS = -r

# utils
FGEN    = fgen
F2HTML  = f2html
ECHO    = echo
RM      = rm
CP      = cp
MV      = mv
CD      = cd
MKDIR   = mkdir
TAR     = tar
GZIP    = gzip
INSTALL = install

# dirs
incdir = ./lib
srcdir = ./core

PRJDIRS = $(incdir)/w3lib $(incdir)/mpeu $(srcdir) 

# Not real file targets
.PHONY: $(MAKEFILE) all dep html clean install help

# targets
all: libw3.a libmpeu.a scamtec.x 

libw3.a:
	@$(CD) $(incdir)/w3lib && \
	    $(MAKE) -f $(MAKEFILE) $@

libmpeu.a:
	@$(CD) $(incdir)/mpeu && \
	    $(MAKE) -f $(MAKEFILE) $@

scamtec.x:
	@$(CD) $(srcdir) && \
	    $(MAKE) -f $(MAKEFILE) $@


# args for fgen & f2html
FGENARGS   = -s $(srcdir) -l $(incdir)/mpeu -i $(incdir)/mpeu -l $(incdir)/w3lib -i $(incdir)/w3lib
F2HTMLARGS = $(srcdir) $(incdir)/mpeu $(incdir)/w3lib

# rebuild dependencies
dep:
	@$(FGEN) -d $(FGENARGS)

# build html pages
HTMLDIR = html

html:
	@$(F2HTML) -d $(HTMLDIR) $(F2HTMLARGS) 

# cleanup
clean:
	@for dir in $(PRJDIRS); do \
	    ($(CD) $$dir && $(MAKE) -f $(MAKEFILE) $@); \
	 done

# installation
install:
	@for dir in $(PRJDIRS); do \
	    ($(CD) $$dir && $(MAKE) -f $(MAKEFILE) $@); \
	 done

# help page
help:
	@$(ECHO) "Defined targets:"
	@$(ECHO) "  all    : build targets (default)"
	@$(ECHO) "  dep    : build dependencies"
	@$(ECHO) "  html   : build html pages"
	@$(ECHO) "  clean  : cleanup"
	@$(ECHO) "  install: install executable"
	@$(ECHO) "Defined modes:"
	@$(ECHO) "  opt: enable flags for optimization (default)"
	@$(ECHO) "  dbg: enable flags for debugging"
	@$(ECHO) "  pro: enable flags for profiling"
	@$(ECHO) "Example:"
	@$(ECHO) "  type \`make mode=dbg+pro' to enable dbg and pro flags"

