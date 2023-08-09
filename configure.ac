# AC_INIT takes two parameters, the package name and a version number.
AC_INIT([libax],[1.0],[joao.gerd@inpe.br])
AC_CONFIG_MACRO_DIR([m4])
# AC_CONFIG_SRCDIR takes one parameter, the path and filename
# for one of the source code files.
AC_CONFIG_SRCDIR([src/scandrv.f90])
AM_INIT_AUTOMAKE

LT_INIT
AC_LANG([Fortran])

# Add some optional features

AC_ENABLE_FEATURE([mpi],[enable mpi routines])
AC_ENABLE_FEATURE([dbg],[enable debugging])
AC_ENABLE_FEATURE([opt],[enable optimization])
AC_ENABLE_FEATURE([prf],[enable profiling])
AC_ENABLE_FEATURE([dpf],[enable double precision])
AC_ENABLE_FEATURE([cray],[Enable cray machine environment])
AC_ENABLE_FEATURE([egeon],[Enable egeon machine environment])


# Compiler specific thinks
AS_IF([test "x$enable_cray" = "xyes" ], [COMP='ftn'] )
AS_IF([test "x$enable_egeon" = "xyes" ], [COMP='mpif90'] )
AS_IF([test "x$enable_egeon" = "x" -a "x$enable_cray" = "x" ], [COMP='gfortran ifort'] )

AC_PROG_FC([$COMP])
if test -z $FC ;then
   AC_MSG_ERROR([No fortran compiler found !])
else
   if test "x$enable_cray" = "xyes" ; then
      AC_MSG_CHECKING([ftn compiler])
      AX_MPIF90_FC
      AC_MSG_RESULT([$AX_MPIFC])
      conf=config/flags.$AX_MPIFC
      if test -e $conf ;then 
         source $conf
      else
         AC_MSG_ERROR([Error: --enable-cray=$enable_cray, but does not exist config file for Fortran compiler $AX_MPIFC !,[-1]])
      fi
      enable_mpi=yes
   elif test "x$enable_egeon" = "xyes" ; then
      AC_MSG_CHECKING([mpif90 compiler])
      AX_MPIF90_FC
      AC_MSG_RESULT([$AX_MPIFC])
      conf=config/flags.$AX_MPIFC
      if test -e $conf ;then 
         source $conf
      else
         AC_MSG_ERROR([Error: --enable-egeon=$enable_cray, but does not exist config file for Fortran compiler $AX_MPIFC !,[-1]])
      fi
      enable_mpi=yes
   else   
      conf=config/flags.$FC
      if test -e $conf ;then 
         source $conf
      else
         AC_MSG_ERROR([Error : Found $FC compiler, but this compiler does not have support yet !],[-2])
      fi
   fi
fi


# Configure flags
AC_SUBST([FCFLAGS],[$DFLT_FCFLAGS])
AC_SUBST([LDFLAGS],[$LNK_FCFLAGS])

if test "x$enable_dbg" = "xyes";then
   AC_MSG_NOTICE([debug flags was enabled ...])
   AX_APPEND_FLAG([$DBG_FCFLAGS],[FCFLAGS])
fi

if test "x$enable_opt" = "xyes";then
   AC_MSG_NOTICE([optimazation flags was enabled ...])
   AX_APPEND_FLAG([$OPT_FCFLAGS],[FCFLAGS])
fi

if test "x$enable_prf" = "xyes";then
   AC_MSG_NOTICE([profiling flags was enabled ...])
   AX_APPEND_FLAG([$PRO_FCFLAGS],[FCFLAGS])
fi

if test "x$enable_dpf" = "xyes";then
   AC_MSG_NOTICE([double precision flags was enabled ...])
   AX_APPEND_FLAG([$DBL_FCFLAGS],[FCFLAGS])
fi
if test "x$enable_mpi" = "xyes";then
   AX_APPEND_FLAG([-DuseMPI],[FCFLAGS])
fi


# Find the compiler flag to include Fortran 90 module information from 
# another directory, and store that in the FC_MODINC variable.
AX_FC_MODULE_FLAG
if test -n "$FC_MODINC"; then
  AX_APPEND_FLAG([$FC_MODINC. $FC_MODINC../include],[FCFLAGS])
fi

# Find the compiler flag to write Fortran 90 module information to
# another directory, and store that in the FC_MODOUT variable.
AC_FC_MODULE_OUTPUT_FLAG
if test -n "$FC_MODOUT";then
   AX_APPEND_FLAG(,[$FC_MODOUT../include],[FCFLAGS])
fi

# Find the Fortran 90 module file name extension
AC_FC_MODULE_EXTENSION

#Determine the linker flags (e.g., ‘-L’ and ‘-l’) for the Fortran 
#intrinsic and runtime libraries that are required to successfully 
#link a Fortran program or shared library.
AC_FC_LIBRARY_LDFLAGS

#REQUIRE_LIBF90(name,lib,testCODE,description)

AX_LIB_NETCDF4([serial])

REQUIRE_LIBF90(ax,
               ax,
              [use fileAccess; end],
              [library file access (libax)])

REQUIRE_LIBF90(w3,
               w3,
              [call baopenw(50,'teste',iret); end],
              [grib 1 library (w3lib)])

REQUIRE_LIBF90(misc,
               misc,
              [use m_string; end],
              [miscellany library (libmisc)])
              
# 
if test "$with_netcdf4" = "yes"; then
        AC_MSG_NOTICE([Appending NetCDF Flags ])
        echo "NETCDF4_FFLAGS $NETCDF4_FFLAGS"
        echo "NETCDF4_LDFLAGS $NETCDF4_LDFLAGS"
        echo "NETCDF4_FLIBS $NETCDF4_FLIBS"
        
        AX_APPEND_FLAG([$NETCDF4_FFLAGS],[FCFLAGS])
        AX_APPEND_FLAG([$NETCDF4_LDFLAGS],[LDFLAGS])
        AX_APPEND_FLAG([$NETCDF4_FLIBS],[LIBS])
else
        AC_MSG_ERROR([Unable to find NetCDF4, we need serial NetCDF4.])
fi

# AC_OUTPUT takes a space delimited list of where the Makefiles are to be created.
# You need to pass all directories where there is source code (including the base
# directory containing all of the source code (which won't need a path pre-pended
# to the 'Makefile' keyword).
AC_CONFIG_FILES([Makefile src/Makefile])
AC_OUTPUT


