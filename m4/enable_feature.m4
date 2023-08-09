# ===========================================================================
#      
# ===========================================================================
#
# SYNOPSIS
#
#   AC_ENABLE_FEATURE(FEATURE, DESCRIPTION)
#
# DESCRIPTION
# 
#   This is a reusable macro for providing --enable-featurefoo functionality.
#
#   FEATURE is the complete name of the feature to be enabled.
#
#   DESCRIPTION is a human readable text to be displayed if the feature
#
#   NOTE: Implementation based on REQUIRE_LIBF90.
#
# LICENSE
#
#   Copyright (c) 2022 Joao G. Z. de Mattos <joao.gerd@inpe.br>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved.  This file is offered as-is, without any
#   warranty.


AC_DEFUN([AC_ENABLE_FEATURE],
          [
           {
            AC_ARG_ENABLE([$1],
               AS_HELP_STRING([--enable-$1],[$2 [default=no]]))
           }
          ]
         )
