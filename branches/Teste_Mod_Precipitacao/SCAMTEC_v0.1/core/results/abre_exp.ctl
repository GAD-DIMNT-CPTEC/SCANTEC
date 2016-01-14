dset EXP_precip.bin
options sequential
undef -999.9
title Experimento
xdef 190 linear -82.625 0.25000000
ydef 246 linear -49.875 0.25000000
zdef 1 levels 1000
tdef 1 linear 00Z01mar2004 3hr
vars 1
prec         0   99 daily acc Rain  (mm)
endvars
