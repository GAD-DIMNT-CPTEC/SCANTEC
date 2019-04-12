"""
Objetivo: Este script realiza a plotagem das tabelas do SCANTEC (ACOR, RMSE e VIES)
          para o período indicado, em função dos dias da avaliação.

Uso: python3 scanplot_diario.py

Observações: Alterar o valor das variáveis "start_dt" e "end_dt"
"""

import ntpath
import glob
import numpy as np
import os
import csv
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from matplotlib.ticker import FormatStrFormatter
from fjso import select
from time_date import daterange
from datetime import timedelta, date

start_dt = date(2015, 5, 4)
end_dt = date(2015, 5, 31)
#
def plot_DIARIO_1_var(
                    start_dt,
                    end_dt,                    
                    title,
                    statistic,
                    previsao,
                    level,
                    positionSCANTEC,
                    regiao,
                    horario
                    ):

    var_1 = str(title) +'-'+ str(level) #"title+level
    #
    path_1 ='dadosscantec/aval_SMG/diario/00Z/SMG_V2.0.0.'+str(regiao)
    path_2 ='dadosscantec/aval_SMG/diario/00Z/SMG_V2.1.0.'+str(regiao)
    #
    print("inicio: "+ str(start_dt) +"  fim: "+ str(end_dt) )
    lista_1 = []
    datas_1 = []
    #
    lista = []
    datas = []
    #
    for dt in daterange(start_dt, end_dt):
        #
        dia = dt.strftime("%d")
        #
        mes = dt.strftime("%m")
        #
        ano = dt.strftime("%Y")
        #
        anomesdia = dt.strftime("%Y%m%d")
        #
        allFiles_1 = glob.glob(path_1 + '/' + str(statistic)
                                      + 'EXP01_'
                                      + str(ano)
                                      + str(mes) 
                                      + str(dia) 
                                      + str(horario)
                                      + str(ano)
                                      + str(mes) 
                                      + str(dia) 
                                      + str(horario)
                                      +'T.csv'
                                      )
        allFiles_2 = glob.glob(path_2 + '/'+str(statistic)
                                      +'EXP01_'
                                      + str(ano)
                                      + str(mes) 
                                      + str(dia) 
                                      + str(horario)
                                      + str(ano)
                                      + str(mes) 
                                      + str(dia) 
                                      + str(horario)
                                      +'T.csv'
                                      )
        #
        #
        allFiles_1.sort()
        allFiles_2.sort()
        #
        for file_ in allFiles_1:
            name_file=ntpath.basename(file_)
            #eixo x
            datas_1.append(name_file[26:28])
            df = pd.read_csv(file_)                  
            #eixo y    
            td = df.loc[positionSCANTEC,var_1]
            lista_1.append(td)
        #
        for file_ in allFiles_2:
            name_file=ntpath.basename(file_)
            #eixo x
            datas.append(name_file[26:28])
            print("exp2")
            print(name_file)
            df = pd.read_csv(file_)
            #
            #eixo y
            ts = df.loc[positionSCANTEC,var_1]
            #
            lista.append(ts)
    #
    #
    lista_format = [round(elem, 3) for elem in lista]
    lista_1_format = [round(elem, 3) for elem in lista_1]
    #
    print("datas:\n",datas)
    print("datas_1:\n",datas_1)
    #
    sns.set()
    #
    fig=plt.figure()
    #
    plt.plot(datas,lista_format, marker='s', label='v2.1.0')
    plt.plot(datas_1,lista_1_format, marker='8', label='v2.0.0')
    #
    plt.ylabel(str(statistic))
    plt.xlabel("Dia")
    #
    plt.axhline(y=0 , color='black') if str(statistic) == "VIES" else print("sem axhline")
    #
    #
    #    
    plt.title(str(statistic)
                +' Diário '
                +title+'-'
                + str(level) + 'hPa'
                +' '+str(regiao.upper())
                + ' FCT '
                + str(previsao) + 'h'
                + '\n'
                + str(start_dt)
                + str(horario) 
                +' - '
                + str(end_dt) 
                + str(horario)
                +' '
                )
    #
    #
    plt.tick_params(labelsize=6)
    #
    plt.xticks(rotation=45)#
    fig.align_labels()
    #
    print("lista_1_format:",lista_1_format)
    #
    plt.ylim(ymin = (min(lista_1_format)) - 0.01) if min(lista_1_format) < min(lista_format) else plt.ylim(ymin = min(lista_format) - 0.01)
    plt.ylim(ymax = (max(lista_1_format)) + 0.01) if max(lista_1_format) > max(lista_format) else plt.ylim(ymax = max(lista_format) + 0.01)
    #
    plt.legend()
    #
    plt.savefig('output/diario/' 
                + str(regiao.upper()) 
                + '/' 
                + str(statistic) 
                + '_DIARIO_' 
                + str(title) 
                + '-' 
                + str(level) 
                + '_' 
                + str(regiao.upper()) 
                + '_' 
                + str(previsao) 
                + 'h_12Z_indicefile' 
                + str(positionSCANTEC) 
                + '_' 
                + str(start_dt) 
                + str(horario) 
                + '_' 
                + str(end_dt) 
                + str(horario) 
                + '.png', 
                dpi=200)
    plt.close('all')
    return

a = select(6,3,12,5,6,5,2,4,2)

#Necessário para o relatório:

#UVEL 850
#VVEL 850
#UMES 925
#ZGEO 500
#PSNM 000
#TEMP 850



#print("TEMP-500")
#plot_DIARIO_1_var("ZGEO-850","ZGEO","VIES","24","850",4,"HN","00")
#print("TEMP-250")
##plot_DIARIO_1_var("TEMP-250","TEMP","RMSE","24","250",4,"GL","00")
#print("TEMP-850")
##plot_DIARIO_1_var("TEMP-850","TEMP","RMSE","24","850",4,"GL","00")


#print(a[8][0])

#start_dt = date(2015, 5, 3)
#end_dt = date(2015, 5, 28)
#
######EXPERIMENTO
#print("250 - 850")
#print("title+level<var_1> | title | statistic | previsao | level | positionSCANTEC | regiao | horario")
#for h in range(0,1):
#    #(0,4)
#    for z in range(0,1):
#        #(0,6)
#       for x in range (2,3):
#           #(0,4)
#           for b, c in zip(range(1,2), range(1,2)):
#                    #(0,3)
#                   for e, g in zip(range(2,3),range(2,3)):
#                           plt.figure(figsize=(5,5))
#                           print("positionSCANTEC: "+ str(a[5][x]))
#                           print("previsão: "+ str(a[1][x]))
#                           print("area:" + str(a[6][z]))
#                           print("horario:"+ a[8][h])
#                           print("title: "+ str(a[3][b]))
#                           print("estatistica: "+ str(a[2][e]))
#                           print("Level:"+ str(g))
#                           print(str(a[3][b]) +'-'+ str(a[4][g]))
#
#                           plot_DIARIO_1_var(start_dt,end_dt,
#                                    
#                                    str(a[3][c]), a[2][e],a[1][x],
#                                    a[4][g],a[5][x], a[6][z], a[8][h]
#                                    )
###
###
##
#
###EXPERIMENTO

#***
#Entre com a data
#start_dt = date(2015, 5, 4)
#end_dt = date(2015, 5, 31)
#end_dt = date(2015, 5, 23)

plot_DIARIO_1_var(start_dt,end_dt,"ZGEO","ACOR","24","850",4,"hn","00")

####***************
####Para areas GL, TR e HS -> 3 a 28 for z in range(0,3):
####Para areas AS -> 1 a 14 for z in range(4,5):
####Para area HN -> 1 a 23  for z in range(3,4):
###
###print("250 - 850")
###print("start_dt | end_dt | title | statistic | previsao | level | positionSCANTEC | regiao | horario")
###for h in range(0,4):
###    #(0,4)
###    for z in range(3,4):
###        #(0,6)
###       for x in range (0,3):
###           #(0,4)
###           for  c in range(0,3):
###                    #(0,3)
###                   for e, g in zip(range(0,3),range(0,3)):
###                           plt.figure(figsize=(5,5))
###                           print("positionSCANTEC: "+ str(x))
###                           print("statistic:" +str(e))
###                           print("area:" + str(a[6][z]))
###                           print("horario:"+ a[8][h])
###                           print(e)
###                           print("Level:"+ str(g))
###
###                           plot_DIARIO_1_var(start_dt,end_dt,                                    
###                                    str(a[3][c]), a[2][e],a[1][x],
###                                    a[4][g],a[5][x], a[6][z], a[8][h]
###                                    )
####print("500-925")
####print("start_dt | end_dt | title | statistic | previsao | level | positionSCANTEC | regiao | horario")
####for h in range(0,4):
####    for z in range(0,3):
####        for e in range(0,3):
####            for x in range (0,3):
####                for  c in range(4,6):
####                        for g in range(1,4):
####                                plt.figure(figsize=(5,5))
####                                print("positionSCANTEC: "+ str(x))
####                                print("statistic:" +str(e))
####                                print("Level:"+ str(g))
####                                plot_DIARIO_1_var(start_dt,end_dt,                                         
####                                         str(a[3][c]), a[2][e],a[1][x],
####                                         a[4][g],a[5][x], a[6][z], a[8][h]
####                                         )
######
####print("000 - 925")
####print("start_dt | end_dt | title | statistic | previsao | level | positionSCANTEC | regiao | horario")
####for h in range(0,4):
####    for z in range(0,3):
####       for x in range (0,3):
####           for  c, g in zip(range(6,8), range(3,5) ):
####                for e in range(0,3):
####                   # for g in range(3,4):
####                    plt.figure(figsize=(5,5))
####                    print("positionSCANTEC: "+ str(x))
####                    print("area:" + str(a[6][z]))
####                    print("horario:"+ a[8][h])
####                    print("title:"+str(a[3][b]))
####                    print(e)
####                    print("Level:"+ str(g))
####                    #
####                    plot_DIARIO_1_var(start_dt,end_dt,                                
####                                str(a[3][c]), a[2][e],a[1][x],
####                                str(a[4][g]),a[5][x], a[6][z], a[8][h]
####                                )
####
####
