# Automatically generated GNU Makefile. Dom Jul 27 11:51:57 BRT 2014
# fgen v0.3 (C) 1997,98 Beroud Jean-Marc

# simultaneous parallel jobs & load average limit
MAXJOBS = 1
MAXLOAD = 1

# standard
SHELL     = sh
MAKE      = make
MAKEFILE  = Makefile
MAKEFLAGS = -r -j$(MAXJOBS) -l$(MAXLOAD)

# utils
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
srcdir = ./

# preprocessor, compilers, linker & archiver
FPP = gcc
CC  = gcc
FC  = gfortran
LD  = gfortran
AR  = ar

# default mode (max. optimization)
mode = dbg

# header file directories
CCFLAGS = 
FFLAGS  = 

# default flags
CPP_FLAGS      =  -C -P 
CPP_F90FLAGS   =  -traditional-cpp 
FPPFLAGS = -C -E
CCFLAGS += -pipe
FFLAGS  += -cpp -fPIC -fconvert=big-endian 
LDFLAGS  = -shared -fPIC
ARFLAGS  = -r

# add flags for debugging if requested
ifeq (dbg,$(findstring dbg,$(mode)))
   CCFLAGS += -DEBUG -g -Wall -fbracktrace
#   FFLAGS  += -DDEBUG -g -O0 -fimplicit-none -Wall -Wline-truncation -Wcharacter-truncation -Wsurprising -Waliasing -Wimplicit-interface -Wunused-parameter -fwhole-file -fcheck=all -pedantic -fbacktrace
   FFLAGS += -DEBUG -O0 -g -ggdb -Wall -fbacktrace -fcheck=all -fcheck=bounds -Wuninitialized -ffpe-trap=zero,invalid,overflow,underflow

   LDFLAGS += -g
endif

# add flags for profiling if requested
ifeq (pro,$(findstring pro,$(mode)))
   CCFLAGS += -pg
   FFLAGS  += 
   LDFLAGS += 
endif

# add flags for optimization if requested
ifeq (opt,$(findstring opt,$(mode)))
   CCFLAGS += -O2 -m486 -malign-loops=2 -malign-jumps=2 -malign-functions=2 -fno-strength-reduce
   FFLAGS  += -O3 
   LDFLAGS += 
endif

# objectlist file
include Makefile-objs

# additionnal libraries and includes
INCDIRS =
LIBDIRS =
LIBS    =

# target names
LIB = libinterp.a

# Not real file targets
.PHONY: $(MAKEFILE) Makefile-deps Makefile-objs \
        all dep html clean install help

# targets
all: $(LIB) 


$(LIB): $(OBJS)
	@$(ECHO) ""
	@$(ECHO) "Creating archive $(@F)"
	@$(ECHO) ""
	$(AR) $(ARFLAGS) $@ $(notdir $(OBJS))

#$(LIB): $(OBJS) 
#	@$(ECHO) ""
#	@$(ECHO) "Linking executable $(@F)"
#	@$(ECHO) ""
#	$(LD) $(LDFLAGS) $(notdir $(OBJS)) $(LIBDIRS) $(LIBS) -o $@.so

# args for fgen & f2html
FGENARGS   = -s $(srcdir) 
F2HTMLARGS = $(srcdir) 

# cleanup
clean:
	$(RM) -f $(BIN) $(LIB) $(notdir $(OBJS)) *.[dlMT] *.lst *.mod work.pc* core *.a

# installation
LIBDIR = /usr/local/lib
BINDIR = /usr/local/bin

install:
	if [ -n "$(LIB)" ] && [ -r "$(LIB)" ]; then \
	   $(INSTALL) -m 644 $(LIB) $(LIBDIR); \
	fi
	if [ -n "$(BIN)" ] && [ -r "$(BIN)" ]; then \
	   $(INSTALL) -c $(BIN) $(BINDIR); \
	fi

# suffixes
.SUFFIXES: .h .F .f .F90 .f90 .c .o

# remove target on error
.DELETE_ON_ERROR:

# implicit rules
# Want full path? Change $(<F) to $< and add -o $(@F) or -o $@
%.o: %.F   ; $(FC) -c $(FFLAGS) $(INCDIRS) $(<F)
%.o: %.f   ; $(FC) -c $(FFLAGS) $(INCDIRS)  $(<F)
#%.o: %.F90 ; $(FC) -c $(FFLAGS) $(INCDIRS)  $(<F)
%.o: %.f90 ; $(FC) -c $(FFLAGS) $(INCDIRS)  $(<F)
%.o: %.c   ; $(CC) -c $(CCFLAGS) $(INCDIRS) $(<F)

%.o: %.F90
	$(CPP) $(CPP_FLAGS) $(CPP_F90FLAGS) $*.F90  > $*.f90
	$(FC) $(FFLAGS) -c $*.f90
	$(RM) $*.f90

# if the compiler do no support the F90 extension
#%.o: %.F90
#	$(MV) $(<F) $(*F).c
#	$(FPP) $(FPPFLAGS) $(*F).c > $(*F)-tmp.f90
#	$(FC) -c $(FFLAGS) $(INCDIRS) $(*F)-tmp.f90
#	$(MV) $(*F)-tmp.o $(*F).o
#	$(RM) -f $(*F).c $(*F)-tmp.f90 

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

# dependencies file
include Makefile-deps
