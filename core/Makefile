# Automatically generated GNU Makefile. Sex Set  9 11:26:26 BRT 2011
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
#MPEU_LIB = ../lib/mpeu
#MPEU_INC = ../lib/mpeu
BILIN_LIB = ../lib/BilinInterp
BILIN_INC = ../lib/BilinInterp
GRIB_LIB = ../lib/w3lib-2.0.6
GRIB_INC = ../lib/w3lib-2.0.6
incdir = ./
srcdir = ./

# default mode (max. optimization)
mode = #dbg #pro

# header file directories
CCFLAGS = -I$(incdir) -I$(BILIN_INC)
FFLAGS  += -I$(incdir) -I$(BILIN_INC) 

# default flags
  FPPFLAGS = -C -E 
  CCFLAGS += -DIFC
  LDFLAGS  = 
  ARFLAGS  = -r


#
# Flags to Intel fortran with gcc
#

ifeq ($(SCAN_ARCH),Darwin_intel)
   FPP = gcc
   CC  = gcc
   FC  = ifort
   LD  = ifort
   AR  = ar
   FFLAGS += -fpp -assume byterecl -heap-arrays 

# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      CCFLAGS += 
      FFLAGS  += -g -debug extended -O0 -traceback -fpe-all=0 -check all -pg -warn -DDEBUG
      LDFLAGS += -g -pg
   endif

# add flags for profiling if requested
   ifeq (pro,$(findstring pro,$(mode)))
      CCFLAGS += 
      FFLAGS  += -O3 -fast
      LDFLAGS += 
   endif
endif

#
# Flags to PGI on Cray
#

ifeq ($(SCAN_ARCH),cray_pgi)
   FPP = cc
   CC  = cc
   FC  = ftn
   LD  = ftn
   AR  = ar
   FFLAGS += -Mpreprocess -Mnorecursive -mp -tp amd64 -mcmodel=medium -Mlarge_arrays#-DDEBUG

# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      CCFLAGS += 
      FFLAGS  += -g -gopt -Mchkfpstk -Mchkptr -Mchkstk -Mcoff -Mdwarf1 -Mdwarf2 -Mdwarf3 -Melf -Mpgicoff -traceback -DDEBUG 
      LDFLAGS += -g -pg
   endif

	# add flags for profiling if requested
   ifeq (pro,$(findstring pro,$(mode)))
      CCFLAGS += 
      FFLAGS  += -O3
      LDFLAGS += 
   endif

endif
#
# Flags to GNU
#

ifeq ($(SCAN_ARCH),GNU_Linux)
   FPP = gcc
   CC  = gcc
   FC  = gfortran
   LD  = gfortran
   AR  = ar
#   FFLAGS += -cpp -frecursive -ffree-line-length-none -Dgfortran
   FFLAGS += -cpp -ffree-line-length-none -Dgfortran 

# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      CCFLAGS += 
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

ifeq ($(SCAN_ARCH),cray_cray)
   FPP = ftn
   CC  = ftn
   FC  = ftn
   LD  = ftn
   AR  = ar
   FFLAGS += -eZ -ffree -N255 -h nosecond_underscore -Ofp0 -ema

# add flags for debugging if requested
   ifeq (dbg,$(findstring dbg,$(mode)))
      CCFLAGS += 
      FFLAGS  += -eP -G0 -DDEBUG -m2 -R bcdsp
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

# additionnal libraries
LIBDIRS = -L$(BILIN_LIB) -L$(GRIB_LIB) 
LIBS    = -lw3  -linterp #-llapack 

PATH2=../bin

# target names
BIN = scantec.x
LIB = 

# Not real file targets
.PHONY: $(MAKEFILE) Makefile-deps Makefile-objs \
        all dep html clean install help

# targets
all: $(LIB) $(BIN)
#	mv $(BIN) $(PATH2)
#	rm *.mod *.o

$(LIB): $(OBJS)
	@$(ECHO) ""
	@$(ECHO) "Creating archive $(@F)"
	@$(ECHO) ""
	$(AR) $(ARFLAGS) $@ $(notdir $(OBJS))


$(BIN): $(OBJS) 
	@$(ECHO) ""
	@$(ECHO) "Linking executable $(@F)"
	@$(ECHO) ""
	$(LD) $(LDFLAGS) $(notdir $(OBJS)) $(LIBDIRS) $(LIBS) -o $@

# args for fgen & f2html
FGENARGS   = -s $(srcdir) -l $(incdir) -i $(incdir) 
F2HTMLARGS = $(srcdir) $(incdir) $(incdir) 

# rebuild dependencies
dep:
	@$(FGEN) -d $(FGENARGS)

# build html pages
HTMLDIR = html

html:
	@$(F2HTML) -d $(HTMLDIR) $(F2HTMLARGS) 

# cleanup
clean:
	$(RM) -f $(BIN) $(LIB) $(notdir $(OBJS)) *.[dlMT] *.lst *.mod work.pc* core *.i

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
.SUFFIXES:
.SUFFIXES: .h .F .f .F90 .f90 .c .o

# remove target on error
.DELETE_ON_ERROR:

# implicit rules
# Want full path? Change $(<F) to $< and add -o $(@F) or -o $@
%.o: %.F   ; $(FC) -c $(FFLAGS)  $(<F)
%.o: %.f   ; $(FC) -c $(FFLAGS)  $(<F)
%.o: %.F90 ; $(FC) -c $(FFLAGS)  $(<F)
%.o: %.f90 ; $(FC) -c $(FFLAGS)  $(<F)
%.o: %.c   ; $(CC) -c $(CCFLAGS) $(<F)

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
