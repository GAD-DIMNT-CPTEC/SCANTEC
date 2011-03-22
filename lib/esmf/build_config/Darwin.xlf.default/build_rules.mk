# $Id: build_rules.mk,v 1.24 2009/06/01 04:40:27 theurich Exp $
#
# Darwin.xlf.default
#

############################################################
# Default compiler setting.
#
ESMF_F90DEFAULT         = xlf90_r
ESMF_CXXDEFAULT         = xlC_r

############################################################
# Default MPI setting.
#
ifeq ($(ESMF_COMM),default)
export ESMF_COMM := mpi
endif

############################################################
# MPI dependent settings.
#
ifeq ($(ESMF_COMM),mpiuni)
# MPI stub library -----------------------------------------
ESMF_F90COMPILECPPFLAGS+= -WF,-DESMF_MPIUNI
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPIUNI
ESMF_CXXCOMPILEPATHS   += -I$(ESMF_DIR)/src/Infrastructure/stubs/mpiuni
ESMF_MPIRUNDEFAULT      = $(ESMF_DIR)/src/Infrastructure/stubs/mpiuni/mpirun
else
ifeq ($(ESMF_COMM),mpich)
# Mpich ----------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_MPICH
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpiCC
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),mpich2)
# Mpich2 ---------------------------------------------------
ESMF_F90DEFAULT         = mpif90
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),lam)
# LAM (assumed to be built with xlf90) ---------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif77
ESMF_CXXDEFAULT         = mpic++
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),openmpi)
# OpenMPI --------------------------------------------------
ESMF_CXXCOMPILECPPFLAGS+= -DESMF_NO_SIGUSR2
ESMF_F90DEFAULT         = mpif90
ESMF_F90LINKLIBS       += -lmpi_cxx
ESMF_CXXDEFAULT         = mpicxx
ESMF_MPIRUNDEFAULT      = mpirun $(ESMF_MPILAUNCHOPTIONS)
ESMF_MPIMPMDRUNDEFAULT  = mpiexec $(ESMF_MPILAUNCHOPTIONS)
else
ifeq ($(ESMF_COMM),user)
# User specified flags -------------------------------------
else
$(error Invalid ESMF_COMM setting: $(ESMF_COMM))
endif
endif
endif
endif
endif
endif

############################################################
# Print compiler version string
#
ESMF_F90COMPILER_VERSION    = which ${ESMF_F90COMPILER}
ESMF_CXXCOMPILER_VERSION    = ${ESMF_CXXCOMPILER} -qversion

############################################################
# xlf90 needs flag to indicate FPP options
#
ESMF_FPPPREFIX           = -WF,

############################################################
# OpenMP compiler and linker flags
#
ESMF_OPENMP_F90COMPILEOPTS += -qsmp=omp
ESMF_OPENMP_CXXCOMPILEOPTS += -qsmp=omp
ESMF_OPENMP_F90LINKOPTS    += -qsmp=omp
ESMF_OPENMP_CXXLINKOPTS    += -qsmp=omp

############################################################
# Special debug flags
#
ESMF_F90OPTFLAG_G       += -qcheck -qfullpath
ESMF_CXXOPTFLAG_G       += -qcheck -qfullpath -g

############################################################
# Blank out variables to prevent rpath encoding
#
ESMF_F90LINKRPATHS      =
ESMF_CXXLINKRPATHS      =

############################################################
# xlf90 does not know about Fortran suffices
#
ESMF_F90COMPILEFREECPP   = -qfree=f90 -qsuffix=cpp=F90
ESMF_F90COMPILEFREENOCPP = -qfree=f90 -qsuffix=f=f90
ESMF_F90COMPILEFIXCPP    = -qfixed=132 -qsuffix=cpp=F
ESMF_F90COMPILEFIXNOCPP  = -qfixed=132 -qsuffix=f=f

############################################################
# Determine where xlf's libraries are located
#
ESMF_CXXLINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.xl $(ESMF_F90COMPILER))

############################################################
# Determine where xlc's libraries are located
#
ESMF_F90LINKPATHS += -L$(shell $(ESMF_DIR)/scripts/libpath.xl $(ESMF_CXXCOMPILER))

############################################################
# Link against libesmf.a using the F90 linker front-end
#
ESMF_F90LINKLIBS += -libmc++ -lstdc++

############################################################
# Link against libesmf.a using the C++ linker front-end
#
ESMF_CXXLINKLIBS += -lxlf90_r -lxlfmath

############################################################
# Blank out shared library options
#
ESMF_SL_LIBS_TO_MAKE  =
