LIB	= libw3.a

# Determine platform
UMACHINE := $(shell uname -m)

# Make sure one of the following options appears in your CFLAGS 
# variable to indicate which system you are on.
# -DLINUX, -DSGI, -DHP, -DCRAY90, -DIBM4, -DIBM8, -DLINUXF90 

#F77	= xlf
#FFLAGS	= -O3 -qnosave -qarch=auto
#CFLAGS	= -DIBM4 -q64
#ARFLAGS	= -X64
#CC	= cc
# OPTIONS FOR IBM-SP 

ifeq ($(SCAN_ARCH),AIX)
# Standard installation
F77	= xlf
FFLAGS	= -O -qstrict -qtune=auto -qarch=auto -qmaxmem=-1 -NS2000 -qsave -c -q64 -WF,-DAIX
# Non-standard installation
#F77	= /u/home/jim/local/bin/mpxlf_r
#FFLAGS	= -O -qstrict -qtune=auto -qarch=auto -qmaxmem=-1 -NS2000 -qsave -c -q64 -WF,-DAIX -F/site/xlf_10.1/etc/xlf.cfg
# Non-standard installation
#F77	= /u/wx22jg/local/bin/mpxlf_r
##F77	= /u/wx22jg/local/bin/xlf_r
#FFLAGS	= -O -qstrict -qtune=auto -qarch=auto -qmaxmem=-1 -NS2000 -qsave -c -q64 -WF,-DAIX -F/gpfs/h/usrx/local/xlf.10.1.0.6/etc/xlf.cfg.53
CFLAGS	= -DIBM4 -q64
ARFLAGS	= -X64 rv
CC	= cc
endif

ifeq ($(SCAN_ARCH),OSF1)
F77	= f90
FFLAGS	= -c -cpp -automatic -convert big_endian -assume byterecl -arch ev6 -tune ev6 -fpe3
CFLAGS	= -DOSF1
ARFLAGS = r
CC	= gcc
endif

ifeq ($(SCAN_ARCH),linux_lf95)
# OPTIONS FOR LINUX LAHEY
F77	= lf95
FFLAGS	= 
CFLAGS	= -O -DLINUX
CC	= cc
endif

ifeq ($(SCAN_ARCH),linux_absoft)
ifeq ($(UMACHINE), x86_64)
BIT_FLAG = -DLINUX64
else
BIT_FLAG =
endif
F77	= f90
FFLAGS	= -YEXT_NAMES=LCS -s -YEXT_SFX=_ -YCFRL=1 $(BIT_FLAG)
CFLAGS	= -O -DLINUX
CC	= gcc
ARFLAGS	= r
endif

ifeq ($(SCAN_ARCH),Darwin_intel)
F77     = ifort
FFLAGS  = $(BIT_FLAG)
CFLAGS  = -O -DLINUX
CC      = gcc
ARFLAGS = r
endif

ifeq ($(SCAN_ARCH),GNU_Linux)
F77     = gfortran
FFLAGS  = $(BIT_FLAG)
CFLAGS  = -O -DLINUX
CC      = gcc
ARFLAGS = r
endif

ifeq ($(SCAN_ARCH),linux_ifc)
ifeq ($(UMACHINE), ia64)
BIT_FLAG = -DLINUX64
else
BIT_FLAG =
endif
F77     = ifort
FFLAGS  = $(BIT_FLAG)
CFLAGS  = -O -DLINUX
CC      = icc
ARFLAGS = r
endif

ifeq ($(SCAN_ARCH),linux_pgi)
# OPTIONS FOR LINUX pgi
ifeq ($(UMACHINE), x86_64)
BIT_FLAG = -DLINUX64
else
BIT_FLAG =
endif
F77    = pgf90
FFLAGS    = -Mbyteswapio -Mpreprocess $(BIT_FLAG)
CFLAGS    = -O -DLINUX
CC    = gcc
ARFLAGS    = r
endif

ifeq ($(SCAN_ARCH),cray_pgi)
# OPTIONS FOR CRAY pgi
ifeq ($(UMACHINE), x86_64)
BIT_FLAG = -DLINUX64
else
BIT_FLAG =
endif
F77    = ftn
FFLAGS    = -Mbyteswapio -Mpreprocess $(BIT_FLAG)
CFLAGS    = -O -DLINUX
CC    = cc
ARFLAGS    = r
endif

# OPTIONS FOR VPP5000
#F77     = frt
#FFLAGS  = -X9 -Ad -Sw -Am -Of -Wv,-Of
#CFLAGS  = -O -K4 -DVPP5000
#CC      = vcc

.SUFFIXES: .o .f .F .c
#
# *****************************************************************
#
OBJ_MOD =  bacio_module.o

OBJS	= \
getgb.o     getgbmh.o   putgbex.o   w3fi73.o     baciof.o \
getgb1r.o   getgbmp.o   putgbn.o    w3fi74.o \
getgb1re.o  getgbp.o    r63w72.o    w3fi75.o \
getgb1s.o   getgi.o     sbyte.o     w3fi76.o \
getgbe.o    getgir.o    sbytes.o    w3fi82.o \
getgbeh.o   idsdef.o    skgb.o      w3fi83.o \
getgbem.o   iw3jdn.o    w3difdat.o  w3fs26.o \
getgbemh.o  ixgb.o      w3doxdat.o  w3log.o \
getgbemp.o  lengds.o    w3fi01.o    w3movdat.o \
getgbens.o  pdsens.o    w3fi58.o    w3reddat.o \
getgbep.o   pdseup.o    w3fi59.o    w3tagb.o \
errmsg.o    getgbex.o   putgb.o     w3fi63.o    \
gbytes.o                getgbexm.o  putgbe.o    w3fi68.o    \
gbytes_char.o           getgbh.o    putgben.o   w3fi71.o \
getbit.o                getgbm.o    putgbens.o  w3fi72.o \
errexit.o fparsei.o fparser.o instrument.o start.o summary.o w3utcdat.o \
w3fs21.o w3locdat.o

OBJS_CC= bacio.v1.3.o mova2i.o

SRC	= $(OBJ_MOD:.o=.f) $(OBJS:.o=.f) $(OBJS_CC:.o=.c)
#
# *****************************************************************
#

#$(LIB): $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
#	ar $(ARFLAGS) -ruv $(LIB) $(OBJ_MOD) $(OBJS) $(OBJS_CC)

$(LIB): $(OBJ_MOD) $(OBJS) $(OBJS_CC) 
	ar $(ARFLAGS) $(LIB) $(OBJ_MOD) $(OBJS) $(OBJS_CC)

clean:
	rm -f $(OBJ_MOD) $(OBJS) $(OBJS_CC) *.mod *.a

.F.o:
	$(F77) $(FFLAGS) -c $< 
.f.o:
	$(F77) $(FFLAGS) -c $< 
.c.o:
	$(CC) $(CFLAGS) $(DEFS) -c $< 
