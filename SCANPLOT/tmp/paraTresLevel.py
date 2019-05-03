import glob
import ntpath
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from time_date import daterange
from datetime import date
from matplotlib.ticker import StrMethodFormatter

# Alterar os valores das dates de início e fim da avaliação diária:
start_dt = date(2015,5,1)
end_dt = date(2015,5,31)

# Alterar os caminhos para as tabelas do SCANTEC:
base_path_1 = 'dadosscantec/aval_SMG/diario/00Z/SMG_V2.0.0.'
base_path_2 = 'dadosscantec/aval_SMG/diario/00Z/SMG_V2.1.0.'



def plot_DIARIO_1_var_3_level(start_dt,end_dt,var_1,var_2,var_3,title,statistic,previsao,positionSCANTEC,regiao,horario):
    path_1 = base_path_1 + str(regiao)
    print("path_1",path_1)
    path_2 = base_path_2 + str(regiao)
    
    print("\n>> Inicio: " + str(start_dt) + "  Fim: " + str(end_dt))
    
    print("var_1",var_1)
    print("var_2",var_2)
    print("var_3",var_3)

    lista_1 = []
    datas_1 = []
    
    lista_2 = []
    datas_2 = []
    
    for dt in daterange(start_dt, end_dt):

        dia = dt.strftime("%d")
        mes = dt.strftime("%m")
        ano = dt.strftime("%Y")

        print("\n >",title,previsao,regiao,horario)

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
                                      +'T.scam'
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
                                      +'T.scam'
                                      )

        allFiles_1.sort()
        allFiles_2.sort()

        print("allFiles_1:",allFiles_1)
        print("allFiles_2:",allFiles_2)

        if len(allFiles_1) != 0:
            file_1 = allFiles_1[0]
            name_file_1 = ntpath.basename(file_1)
            datas_1.append(name_file_1[26:28])
            df_1 = pd.read_csv(file_1, sep="\s+")                  
            td_1 = df_1.loc[positionSCANTEC,[var_1,var_2,var_3]]
            lista_1.append(td_1)
        else:
            datas_1.append(np.nan)
            lista_1.append(np.nan)
        
        if len(allFiles_2) != 0:
            file_2 = allFiles_2[0]
            name_file_2 = ntpath.basename(file_2)
            datas_2.append(name_file_2[26:28])
            df_2 = pd.read_csv(file_2, sep="\s+")
            ts_2 = df_2.loc[positionSCANTEC,[var_1,var_2,var_3]]
            lista_2.append(ts_2)
        else:
            datas_2.append(np.nan)
            lista_2.append(np.nan)

    lista_1_fmt = [round(elem,3) for elem in lista_1]
    lista_2_fmt = [round(elem,3) for elem in lista_2]


    print("lista_1_fmt:\n",lista_1_fmt)
    print("lista_2_fmt:\n",lista_2_fmt)

    print("len(lista_1_fmt):",len(lista_1_fmt))
    print("len(lista_2_fmt):",len(lista_2_fmt))
 
    print("datas_1:\n",datas_1)
    print("datas_2:\n",datas_2)
    
    sns.set()
        
    fig=plt.figure()
    #
    plt.tight_layout()
    #
    plt.plot(lista_1_fmt, marker='8', label='v2.0.0')
    plt.plot(lista_2_fmt, marker='s', label='v2.1.0')
    #
    plt.ylabel(str(statistic))
    plt.xlabel('Dia')
    #
    plt.title(str(statistic)
                +' Diário '
                + title + '-'
                + str(level) + 'hPa'
                + ' ' + str(regiao.upper())
                + ' FCT '
                + str(previsao) + 'h'
                + '\n'
                + str(start_dt)
                + str(horario) 
                + ' - '
                + str(end_dt) 
                + str(horario)
                + ' '
                )
    #
    plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}')) # Sem casas decimais
    plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.3f}')) # Com 3 casas decimais
    #
    plt.tick_params(labelsize=7, pad=-1.5)
    plt.xticks(x_tick_labels,rotation=45)
    #
    fig.align_labels()
    
    if statistic == "ACOR":
        plt.ylim((lista_min - 0.1),1.0)
    elif statistic == "VIES":
        plt.axhline(y=0, color='black')
    
    plt.legend()
    plt.show()
    
    #print("figura:",'output/diario/' 
    #                    + str(regiao.upper()) 
    #                    + '/' 
    #                    + str(statistic) 
    #                    + '_DIARIO_' 
    #                    + str(title) 
    #                    + '-' 
    #                    + str(level) 
    #                    + '_' 
    #                    + str(regiao.upper()) 
    #                    + '_' 
    #                    + str(previsao) 
    #                    + 'h_' 
    #                    + str(start_dt) 
    #                    + str(horario) 
    #                    + '_' 
    #                    + str(end_dt) 
    #                    + str(horario) 
    #                    + '.png'
    #                    )
    #plt.savefig('output/diario/' 
    #                    + str(regiao.upper()) 
    #                    + '/' 
    #                    + str(statistic) 
    #                    + '_DIARIO_' 
    #                    + str(title) 
    #                    + '-' 
    #                    + str(level) 
    #                    + '_' 
    #                    + str(regiao.upper()) 
    #                    + '_' 
    #                    + str(previsao) 
    #                    + 'h_' 
    #                    + str(start_dt) 
    #                    + str(horario) 
    #                    + '_' 
    #                    + str(end_dt) 
    #                    + str(horario) 
    #                    + '.png', dpi=200)
    plt.savefig("teste.png")
    plt.close('all')
    return

plot_DIARIO_1_var_3_level(start_dt,end_dt,"TEMP-250","TEMP-500","TEMP-850","TEMP","VIES","24",4,"hn","00")
















# Finalizado
#import glob
#import numpy as np
#import os
#import csv
#import matplotlib.pyplot as plt
#import pandas as pd
#import seaborn as sns
#from matplotlib.ticker import FormatStrFormatter
#
#from time_date import daterange
#from datetime import timedelta, date
#
#from fjso import select
#
#
#def plot_DIARIO_1_var(var_1,title,statistic,previsao,level,positionSCANTEC,regiao,horario):
#    path_1 ='dadosscantec/exp1/diario/'+str(regiao)
# 
#    start_dt = date(2015, 5, 1)
#    end_dt = date(2015, 5, 5)
#    print("inicio: "+ str(start_dt) +"  fim: "+ str(end_dt) )
#    lista_1 = []
#    for dt in daterange(start_dt, end_dt):
#        #
#        dia = dt.strftime("%d")
#        #
#        mes = dt.strftime("%m")
#        #
#        ano = dt.strftime("%Y")
#        #

#        allFiles_1 = glob.glob(path_1 + '/'+str(statistic)+'EXP01_'+ str(ano)+ str(mes) + str(dia) + str(horario)+ str(ano)+ str(mes) + str(dia) + str(horario)+'T.csv')
#        #
#        print(allFiles_1)
#        #    
#        #
#        #
#        allFiles_1.sort()
#        #
#        
#        #
#        for file_ in allFiles_1:
#            df = pd.read_csv(file_)   
#            td = df.loc[positionSCANTEC,var_1]
#            lista_1.append(td)
#            print(lista_1)
#        #
#        # 
#    lista_1_format = [round(elem, 3) for elem in lista_1]
#    print(" ")
#    print(lista_1_format)
#
#plot_DIARIO_1_var("ZGEO-850","ZGEO","VIES","24","850",4,"HN","00")