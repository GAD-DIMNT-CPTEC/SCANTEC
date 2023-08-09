# ===========================================================================
#      
# ===========================================================================
#
# SYNOPSIS
#
#   AC_MPIF90_FC()
#
# DESCRIPTION
# 
#   This is a reusable macro for find the compiler used by mpif90.
#
#
# LICENSE
#
#   Copyright (c) 2022 Joao G. Z. de Mattos <joao.gerd@inpe.br>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved.  This file is offered as-is, without any
#   warranty.






AC_DEFUN([AX_MPIF90_FC],
          [
           for ac_option in --version -v -V -qversion -show; do
              for ac_mpicomp in ftn mpif90; do
                 FC_INFO=$( $ac_mpicomp $ac_option 2>&1 )
                 for AX_MPIFC in gfortran pgf90 ifort cray; do
                    echo $FC_INFO | grep -i $AX_MPIFC > /dev/null 2>&1 
                    if test $? -eq 0 ;then
                       break
                    fi                   
                 done
              done
           done           
          ]
         )


