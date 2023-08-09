# Macro file for use with GNU AutoTools for generating config files and Makefiles.
# This file was manually created based on information found via Google and the
# GNU documentation. Although it works for the source code it's included with,
# it may not be 100% correct so additional research/tweaking may be needed.
#
# bin_PROGRAMS is used to define the binary that's to be built from the
# source code. The binary name will also be used in variable names to
# pass values that will only apply to this binary.
bin_PROGRAMS = scantec.x
# Note: that the following names all start with the binary name defined in
# bin_PROGRAMS.
# <binary_name>_LDADD is used to pass extra parameters at link time, such as
# libraries that need to be linked in.
#postAlt_LDADD = -lmisc -lsigiobam
# <binary_name>_LDFLAGS is used to pass extra parameters at compilation time,
# such as the paths to libraries that are needed that are not in the default
# paths.
# If the user supplies a directory where libpcap is installed, the contents
# of the variable setup in configire.in will be substituted below so the
# library file will be found
#inctime_LDFLAGS = -L../libmisc
# <binary_name>_SOURCES takes a space delimited list of the source code files
# that need to be compiled/linked to build the binary.
scantec_x_SOURCES = MathExpress.f90 m_constants.f90 m_ioutil.F90 scan_bstatistic.f90 scan_coreMOD.f90 scan_dataMOD.f90 scandrv.f90 scan_MathPlugin.f90 scan_MetForm.f90 scan_ModelPlugin.f90 scan_outputMOD.f90 scantec_module.f90 scan_UtilsMOD.f90 varType.f90

scantec_x_LDFLAGS= -all-static

# Dependency
#
MathExpress.o: \
	varType.o

scan_ModelPlugin.o: \
	m_constants.o 

scan_UtilsMOD.o: \
	m_ioutil.o \
	scantec_module.o

scan_bstatistic.o: \
	m_ioutil.o \
	scan_dataMOD.o \
	scan_UtilsMOD.o \
	scantec_module.o

scan_coreMOD.o: \
	m_ioutil.o \
	scan_MathPlugin.o \
	scan_bstatistic.o \
	scan_dataMOD.o \
	scan_ModelPlugin.o \
	scan_outputMOD.o \
	scan_UtilsMOD.o \
	scantec_module.o

scan_dataMOD.o: \
	m_ioutil.o \
	m_constants.o \
	MathExpress.o \
	varType.o \
	scan_UtilsMOD.o \
	scantec_module.o

scan_outputMOD.o: \
	m_ioutil.o \
	scantec_module.o

scantec_module.o: \
   scan_ModelPlugin.o \
	m_ioutil.o \
	MathExpress.o \
	m_constants.o

scan_MathPlugin.o: \
	MathExpress.o \
	scan_MetForm.o \
	scantec_module.o

	
CLEANFILES = *.$(FC_MODEXT)

