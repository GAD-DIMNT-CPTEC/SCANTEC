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
AR  = ar

# default mode (max. optimization)
mode = #dbg

# header file directories
CCFLAGS = 
FFLAGS  = 

# default flags
CPP_FLAGS      =  -C -P 
CPP_F90FLAGS   =  -traditional-cpp 
FPPFLAGS = -C -E
LDFLAGS  = -shared -fPIC
ARFLAGS  = -r

ifeq ($(SCAN_ARCH),Darwin_intel)
   FC  = ifort
   LD  = ifort

   FFLAGS += -fpp -assume byterecl -heap-arrays 

# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      FFLAGS  += -g -debug extended -O0 -traceback -fpe-all=0 -check all -pg -warn -DDEBUG
      LDFLAGS += -g -pg
   endif

# add flags for profiling if requested
   ifeq (pro,$(findstring pro,$(mode)))
      FFLAGS  += -O3 -fast
      LDFLAGS += 
   endif
endif

#
# Flags to CCE on Cray
#

ifeq ($(SCAN_ARCH),cray_cray)
   FC  = ftn
   LD  = ftn
   FFLAGS += -ffree -N255 -h nosecond_underscore -Ofp0 -ema
   LDFLAGS += -h byteswapio
# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      FFLAGS  += -eZ -G0 -DDEBUG -m2 -R bcdsp
      LDFLAGS += -g -pg
   endif

	# add flags for profiling if requested
   ifeq (pro,$(findstring pro,$(mode)))
      FFLAGS  += -O3
      LDFLAGS += 
   endif

endif
#
# Flags to GNU
#

ifeq ($(SCAN_ARCH),GNU_Linux)
   FC  = gfortran-9
   LD  = gfortran-9
   FFLAGS += -ffree-line-length-none

# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      FFLAGS  += -g -ggdb -O0 -DDEBUG -fbacktrace -Wuninitialized -fbounds-check -ffpe-trap=zero,invalid,overflow,underflow
      LDFLAGS += -g -pg
   endif

	# add flags for profiling if requested
   ifeq (pro,$(findstring pro,$(mode)))
      CCFLAGS += 
      FFLAGS  += -O3
      LDFLAGS += 
   endif

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

# args for fgen & f2html
FGENARGS   = -s $(srcdir) 
F2HTMLARGS = $(srcdir) 

# cleanup
clean:
	$(RM) -f $(BIN) $(LIB) $(notdir $(OBJS)) *.[dlMT] *.lst *.mod work.pc* core *.a *.i

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
