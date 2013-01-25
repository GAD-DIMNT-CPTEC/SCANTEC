'reinit'

'open climatologia50yr.apr.bin.ctl' 
'set z 1'

'set gxout fwrite'
'set fwrite -sq -be climato_apr_126.bin'
'd regrid2(topo,384,192,gg)'
'!rm -fr udf*'
'd regrid2(lsmk,384,192,gg)'
'!rm -fr udf*'
'd regrid2(roug,384,192,gg)'
'!rm -fr udf*'
'd regrid2(swos,384,192,gg)'
'!rm -fr udf*'
'd regrid2(swrz,384,192,gg)'
'!rm -fr udf*'
'd regrid2(swdz,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tmsp,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tszw,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tsmw,384,192,gg)'
'!rm -fr udf*'
'd regrid2(spmt,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tast,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tmrh,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tmst,384,192,gg)'
'!rm -fr udf*'
'd regrid2(prec,384,192,gg)'
'!rm -fr udf*'
'd regrid2(prcv,384,192,gg)'
'!rm -fr udf*'
'd regrid2(neve,384,192,gg)'
'!rm -fr udf*'
'd regrid2(rnof,384,192,gg)'
'!rm -fr udf*'
'd regrid2(pwat,384,192,gg)'
'!rm -fr udf*'
'd regrid2(pitp,384,192,gg)'
'!rm -fr udf*'
'd regrid2(cssf,384,192,gg)'
'!rm -fr udf*'
'd regrid2(clsf,384,192,gg)'
'!rm -fr udf*'
'd regrid2(usst,384,192,gg)'
'!rm -fr udf*'
'd regrid2(vsst,384,192,gg)'
'!rm -fr udf*'
'd regrid2(cbnv,384,192,gg)'
'!rm -fr udf*'
'd regrid2(olis,384,192,gg)'
'!rm -fr udf*'
'd regrid2(oles,384,192,gg)'
'!rm -fr udf*'
'd regrid2(role,384,192,gg)'
'!rm -fr udf*'
'd regrid2(iswf,384,192,gg)'
'!rm -fr udf*'
'd regrid2(ocis,384,192,gg)'
'!rm -fr udf*'
'd regrid2(oces,384,192,gg)'
'!rm -fr udf*'
'd regrid2(roce,384,192,gg)'
'!rm -fr udf*'
'd regrid2(vimf,384,192,gg)'
'!rm -fr udf*'
'd regrid2(dlwb,384,192,gg)'
'!rm -fr udf*'
'd regrid2(olwt,384,192,gg)'
'!rm -fr udf*'
'd regrid2(dswg,384,192,gg)'
'!rm -fr udf*'
'd regrid2(uswg,384,192,gg)'
'!rm -fr udf*'
'd regrid2(uswt,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tdst,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tgsc,384,192,gg)'
'!rm -fr udf*'
'd regrid2(cate,384,192,gg)'
'!rm -fr udf*'
'd regrid2(tcas,384,192,gg)'
'!rm -fr udf*'
'd regrid2(vpca,384,192,gg)'
'!rm -fr udf*'
'd regrid2(bslh,384,192,gg)'
'!rm -fr udf*'

i=1
j=18
while(i<=j)
'd regrid2(uvmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(vvmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(fcmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(pvmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(ghmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(atmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(rhmt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile
i=1
j=18
while(i<=j)
'd regrid2(ommt(z='i'),384,192,gg)'
'!rm -fr udf*'
i=i+1
endwhile


'disable fwrite climato_apr.bin'
'quit'
