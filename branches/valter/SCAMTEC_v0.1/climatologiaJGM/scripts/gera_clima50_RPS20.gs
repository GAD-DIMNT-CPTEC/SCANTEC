'reinit'
'open climatologia50yr.dec.bin.ctl' 
'set z 1'
'set gxout fwrite'
'set fwrite climato_ETA20_dec.bin'

# Interpolando para o ETA20:Topo 
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(topo,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:lsmk 
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(lsmk,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:roug
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(roug,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:swos
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(swos,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:swrz
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(swrz,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:swdz
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(swdz,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tmsp
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tmsp,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tszw
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tszw,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tsmw
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tsmw,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:spmt
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(spmt,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tast
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tast,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tmrh
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tmrh,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tmst
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tmst,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:prec
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(prec,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:prcv
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(prcv,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:neve
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(neve,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:rnof
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(rnof,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:pwat
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(pwat,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:pitp
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(pitp,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:cssf
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(cssf,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:clsf
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(clsf,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:usst
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(usst,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:vsst
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(vsst,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:cbnv
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(cbnv,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:olis
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(olis,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:oles
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(oles,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'


# Interpolando para o ETA20:role
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(role,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:iswf
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(iswf,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'


# Interpolando para o ETA20:ocis
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(ocis,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:oces
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(oces,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'


# Interpolando para o ETA20:roce
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(roce,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:vimf
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(vimf,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'


# Interpolando para o ETA20:dlwb
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(dlwb,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:olwt
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(olwt,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'


# Interpolando para o ETA20:dswg
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(dswg,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:uswg
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(uswg,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:uswt
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(uswt,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tdst
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tdst,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tgsc
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tgsc,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:cate
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(cate,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:tcas
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(tcas,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:vpca
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(vpca,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20:bslh
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(bslh,0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'

# Interpolando para o ETA20 em niveis:uvmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(uvmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:vvmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(vvmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:fcmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(fcmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:pvmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(pvmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:ghmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(ghmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:atmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(atmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:rhmt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(rhmt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Interpolando para o ETA20 em niveis:ommt
i=1
j=18
while(i<=j)
'set lon -85 -26'
'set lat -51 13'
'define tmp2=regrid2(ommt(z='i'),0.2,0.2,bl_p1,-83.0,-50.2)'
'set lon -83.0 -25.8'
'set lat -50.2 12.2'
'd tmp2'
'!rm -fr udf*'
i=i+1
endwhile

# Finalizar o arquivo
'disable fwrite climato_ETA20_dec.bin'
'quit'
