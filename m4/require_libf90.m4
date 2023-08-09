# ===========================================================================
#      
# ===========================================================================
#
# SYNOPSIS
#
#   REQUIRE_LIBF90(LIBNAME, LIB, TESTCODE, DESCRIPTION)
#
# DESCRIPTION
# 
#   This is a reusable macro for providing --with-libfoo functionality.
#
#   LIBNAME is the complete name of the library file without the extension.
#
#   LIB is the name of the library file without the 'lib' prefix and 
#   without the extension.
#
#   TESTCODE is a short code with one function included in the library 
#   that can be used for a test compilation.
#
#   DESCRIPTION is a human readable text to be displayed if the library 
#    
#   NOTE: Implementation based on REQUIRE_LIB.
#
# LICENSE
#
#   Copyright (c) 2008 Guido U. Draheim <guidod@gmx.de>
#   Copyright (c) 2011 Maarten Bosmans <mkbosmans@gmail.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved.  This file is offered as-is, without any
#   warranty.

AC_DEFUN([REQUIRE_LIBF90], [ 
   {
     AC_ARG_WITH(
                 [$1], 
                 AC_HELP_STRING([--with-$1=<path>],[Location where $4 is installed]),
                 [],
                 [with_$1=default]
     )


     AC_MSG_CHECKING([if $4 is detectable])

     AS_IF( 
          [test "x$with_$1" != xdefault],
          [
           AX_APPEND_FLAG([-L${with_$1}/lib],[LDFLAGS])
           AX_APPEND_FLAG([-I${with_$1}/include],[FCFLAGS])
          ]
     )

     AX_APPEND_FLAG([-l$2],[LIBS])

     AC_LINK_IFELSE([AC_LANG_SOURCE[$3]],[AC_MSG_RESULT([yes])],[AC_MSG_RESULT([no]);AC_MSG_ERROR([$4 was not found, try specifying --with-$1])])
     
   }
   ])

