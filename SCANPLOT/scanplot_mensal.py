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

    print("td_1_fmt:\n",td_1)
    print("ts_2_fmt:\n",ts_2)
    print("name_file_1",name_file_1[26:28])


    td_1_min = np.nanmin(td_1)
    td_1_max = np.nanmax(td_1)
    ts_2_min = np.nanmin(ts_2)
    ts_2_max = np.nanmax(ts_2)

    if ts_2_min <= td_1_min:
        lista_min = ts_2_min
    else:
        lista_min = td_1_min

    if ts_2_max >= td_1_max:
        lista_max = ts_2_max
    else:
        lista_max = td_1_max

    

    #
    sns.set()
    #
    fig=plt.figure()
    #
    plt.plot(td_1, marker='8', label='v2.0.0')
    plt.plot(ts_2, marker='s', label='v2.1.0')
    #
    plt.ylabel(str(statistic))
    plt.xlabel("mensal")
    #
    #
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
    plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}')) # Sem casas decimais
    plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.3f}')) # Com 3 casas decimais
    
    plt.tick_params(labelsize=7, pad=-1.5)
    #
    plt.xticks(rotation=45)
    fig.align_labels()
    #
    print("lista_min",lista_min,"lista_max",lista_max)
    #
    if statistic == "ACOR":
        plt.ylim((lista_min - 0.1),1.0)
    elif statistic == "VIES":
        plt.axhline(y=0, color='black')
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

#Exps=["SMG_V2.0.0.","SMG_V2.1.0."]
Hsins=["00"]
Vars=["VVEL-850"]
Regs=["as"]
Stats=["ACOR"]
Hsins=["00"]

for var_name in Vars:

    var=var_name.split("-",1)[0]
    lev=var_name.split("-",1)[1]
    
    for reg in Regs:
        #
        for stat in Stats:
        #    
            for hsin in Hsins:
            #
                print(start_dt,end_dt,stat,var,lev,reg,hsin)
                plot_mensal(diaInicial,diaFinal,mes,ano,var,stat,lev,reg,hsin)



