
#!/bin/bash
#-----------------------------------------------------------------------------#
#           Group on Data Assimilation Development - GDAD/CPTEC/INPE          #
#-----------------------------------------------------------------------------#
#BOP
#
# !SCRIPT:
#
# !DESCRIPTION: Script to verify what is the compiler used at cray machine.             
#               To verify that you are using the correct version of a
#               compiler, use:
#                    -V option on a cc, CC, or ftn command with CCE and Intel
#                    --version option on a cc, CC, or ftn command with GNU
#
# !REVISION HISTORY: 
# 13 Mai 2021 - J. G. de Mattos - Initial Version
#
# !REMARKS:
#
#
#EOP
#-----------------------------------------------------------------------------#
#BOC

verify(){
   txt=$(ftn -V 2>&1)
   for comp in gfortran pgf90 ifort cray; do
      echo ${txt} | grep -i $comp > /dev/null 2>&1
      if [ $? -eq 0 ];then
         case $comp in
            gfortran) echo "gnu";;
            pgf90) echo "pgi";;
            ifort) echo "intel";;
            cray) echo "cray";;
            *) echo "unknow: $comp"
         esac
         break
      fi
   done 
}

for opt in --version -V; do
   txt=$(ftn $opt 2>&1)
   if [ $? -eq 0 ];then verify; fi
done

#EOC
#-----------------------------------------------------------------------------#
