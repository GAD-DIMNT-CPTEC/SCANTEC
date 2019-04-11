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

 
def plot_mensal_1_var(
                    diaInicial,
                    diaFinal,
                    mes,
                    ano,
                    title,
                    statistic,
                    level,
                    regiao,
                    horario
                    ):
    
    var_1 = str(title) +'-'+ str(level) #"title+level
    print("OK: "+str(var_1))
    

    path_1 ='dadosscantec/exp1/mensal/'+str(regiao)
    path_2 ='dadosscantec/exp2/mensal/'+str(regiao)
    #
    lista_1 = []
    datas_1 = []
    #
    lista = []
    datas = []
    #
    allFiles_1 = glob.glob(path_1   + '/'+str(statistic)
                                    +'EXP01_'
                                    + str(ano)
                                    + str(mes) 
                                    + str(diaInicial) 
                                    + str(horario)
                                    + str(ano)+ str(mes) 
                                    + str(diaFinal) 
                                    + str(horario)
                                    +'T.csv')

    allFiles_2 = glob.glob(path_2   + '/'+str(statistic)
                                    +'EXP01_'
                                    + str(ano)
                                    + str(mes) 
                                    + str(diaInicial) 
                                    + str(horario)
                                    + str(ano)
                                    + str(mes) 
                                    + str(diaFinal) 
                                    + str(horario)
                                    +'T.csv')
    allFiles_1.sort()
    allFiles_2.sort()
    #
    for file_ in allFiles_1:
        name_file=os.path.splitext(file_)[0][54:56]
        df = pd.read_csv(file_)
        td = df.loc[0:12,var_1]

    #
    for file_ in allFiles_2:
        name_file=os.path.splitext(file_)[0][54:56]
        datas.append(name_file)    
        df = pd.read_csv(file_)
        ts = df.loc[0:12,var_1]
        #

   
    sns.set()
    
    fig=plt.figure()

    plt.plot(td, marker='s', label='v2.1.0')
    plt.plot(ts, marker='8', label='v2.0.0')
    
    plt.ylabel(str(statistic))
    plt.xlabel("mensal")

    plt.axhline(y=0 , color='black') if str(statistic) == "VIES" else print("sem axhline")


    #    
    plt.title(str(statistic)
                +' Mensal '
                +title
                +'-'
                + str(level)
                +' '
                +str(regiao)
                +' FCT '
                +'\n'
                + str(ano)
                + str(mes)
                + str(diaInicial)
                + str(horario)
                +' - '
                + str(ano)
                + str(mes) 
                + str(diaFinal) 
                + str(horario)+' ')
    #
    plt.tick_params(labelsize=6)
    #
    plt.xticks(rotation=45)
    fig.align_labels()


    plt.legend()
  
    plt.savefig('tmp_exp0102/mensal/'
                +str(regiao)+'/'
                +str(statistic)
                +'_MENSAL_'
                + str(title) 
                +'-'
                + str(level) 
                +'_'
                + str(ano)
                + str(mes) 
                + str(diaInicial) 
                + str(horario)
                +'_'
                + str(ano)
                + str(mes) 
                + str(diaFinal) 
                + str(horario)+'.png')
    plt.close('all')
    return

#########################################################################################################
#EXECUÇÃO E PASSAGEM DE PARAMÊTROS

a =select(6,3,12,5,6,5,5,4,2)

diaInicial = "02"
diaFinal   = "31"
mes = "05"
ano = "2015"

#print("TEMP-500")
plot_mensal_1_var(diaInicial,diaFinal,mes,ano,"TEMP","VIES","500","HN","00")


#print("250 - 850")
#print("title+level<var_1> | title | statistic | previsao | level | positionSCANTEC | regiao | horario ")
#for h in range(0,1):
#    #(0,4)
#    for z in range(0,5):
#        #(0,6)
#       for x in range (0,3):
#           #(0,4)
#           for b, c in zip(range(0,4), range(0,4)):
#                    #(0,3)
#                   for e, g in zip(range(0,3),range(0,3)):
#                           plt.figure(figsize=(5,5))
#                           print("positionSCANTEC: "+ str(x))
#                           print("area:" + str(a[6][z]))
#                           print("horario:"+ a[8][h])
#                           print(b)
#                           print(e)
#                           print("Level:"+ str(g))
#                           print(str(a[3][b]) +'-'+ str(a[4][g]))
#
#                           plot_mensal_1_var(
#                                diaInicial,
#                                diaFinal,
#                                mes,
#                                ano,
#                                str(a[3][c]), 
#                                a[2][e],
#                                a[4][g], 
#                                a[6][z], 
#                                a[8][h]
#                                    )
#
#
#
#
#
#