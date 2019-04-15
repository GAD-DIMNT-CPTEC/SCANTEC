import glob
import ntpath
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from time_date import daterange
from datetime import timedelta, date
from matplotlib.ticker import StrMethodFormatter

# Alterar os valores das dates de início e fim da avaliação diária:
start_dt = date(2015,5,1)
end_dt = date(2015,5,31)

# Alterar os caminhos para as tabelas do SCANTEC:
base_path_1 = 'dadosscantec/aval_SMG/mensal/00Z/SMG_V2.0.0.'
base_path_2 = 'dadosscantec/aval_SMG/mensal/00Z/SMG_V2.1.0.'

def plot_mensal(
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
    

    path_1 = base_path_1 + str(regiao)
    print("path_1",path_1)
    path_2 = base_path_2 + str(regiao)
    #
    print("\n>> Inicio: " + str(start_dt) + "  Fim: " + str(end_dt))
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
                                    +'T.scam')

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
                                    +'T.scam')
    allFiles_1.sort()
    allFiles_2.sort()
    #
    print("allFiles_1:",allFiles_1)
    print("allFiles_2:",allFiles_2)
    
    if len(allFiles_1) != 0:
        file_1 = allFiles_1[0]
        name_file_1 = ntpath.basename(file_1)
        df_1 = pd.read_csv(file_1, sep="\s+")                  
        td_1 = df_1.loc[0:,var_1]
    else:
        print("O que colocar? Vai Existir?")


    if len(allFiles_2) != 0:
        file_2 = allFiles_2[0]
        name_file_2 = ntpath.basename(file_2)
        df_2 = pd.read_csv(file_2, sep="\s+")
        ts_2 = df_2.loc[0:,var_1]
        print(ts_2)
    else:
        print("O que colocar? Vai Existir?")


    #

        #

   
    sns.set()
    
    fig=plt.figure()
    #
    plt.plot(td_1, marker='8', label='v2.0.0')
    plt.plot(ts_2, marker='s', label='v2.1.0')
    #
    plt.ylabel(str(statistic))
    plt.xlabel("mensal")
    #
    plt.axhline(y=0 , color='black') if str(statistic) == "VIES" else print("sem axhline")
    #
    #
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
    #
    #
    plt.legend()
    #
    print("figura:",'output/mensal/' 
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
                        + 'h_' 
                        + str(start_dt) 
                        + str(horario) 
                        + '_' 
                        + str(end_dt) 
                        + str(horario) 
                        + '.png'
                        )
    plt.savefig('output/mensal/' 
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
                        + 'h_' 
                        + str(start_dt) 
                        + str(horario) 
                        + '_' 
                        + str(end_dt) 
                        + str(horario) 
                        + '.png', dpi=200)
    
  
    
    plt.close('all')
    return

#########################################################################################################
#EXECUÇÃO E PASSAGEM DE PARAMÊTROS

diaInicial = "02"
diaFinal   = "31"
mes = "05"
ano = "2015"

plot_mensal(diaInicial,diaFinal,mes,ano,"TEMP","VIES","500","hn","00")

