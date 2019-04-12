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

def plot_DIARIO_1_var_3_level(var_1,var_2,var_3,title,statistic,previsao,positionSCANTEC,regiao,horario):
    path_1 ='dadosscantec/exp1/diario/'+str(regiao)
    path_2 ='dadosscantec/exp2/diario/'+str(regiao)
    #
    start_dt = date(2015, 5, 2)
    end_dt = date(2015, 5, 15)
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
        allFiles_1 = glob.glob(path_1 + '/'+str(statistic)+'EXP01_'+ str(ano)+ str(mes) + str(dia) + str(horario)+ str(ano)+ str(mes) + str(dia) + str(horario)+'T.csv')
        allFiles_2 = glob.glob(path_2 + '/'+str(statistic)+'EXP01_'+ str(ano)+ str(mes) + str(dia) + str(horario)+ str(ano)+ str(mes) + str(dia) + str(horario)+'T.csv')
        #
        allFiles_1.sort()
        allFiles_2.sort()
        #
        #lista_1 = []
        #datas_1 = []
        for file_ in allFiles_1:
            name_file=os.path.splitext(file_)[0][54:56]
            datas_1.append(name_file)
            df = pd.read_csv(file_)   

            td = df.loc[positionSCANTEC,[var_1,var_2,var_3]]
            lista_1.append(td)
        #
        #
        for file_ in allFiles_2:
            name_file=os.path.splitext(file_)[0][54:56]
            datas.append(name_file)
            print(name_file)
            df = pd.read_csv(file_)
            #
            ts = df.loc[positionSCANTEC,[var_1,var_2,var_3]]
            #
            lista.append(ts)
        #
        #
    lista_1_format = [round(elem, 3) for elem in lista_1]
    print(lista_1_format)
    plt.plot(datas,lista_1_format, marker='s', label='v2.1.0')
    plt.legend([var_1, var_2,var_3], loc=0)
    plt.axhline(y=0 , color='black') if str(statistic) == "VIES" else print("sem axhline")
    
    plt.title('v2.1.0 \n'+ str(statistic)+' Diário '+title+'-'+ str(level)+' '+str(regiao)+ ' FCT '+
              str(previsao)+ '\n'+ str(start_dt)+ str(horario)+' - '+ str(end_dt) + str(horario)+' ')

    
    plt.ylabel(str(statistic))
    plt.xlabel("Dia")


    plt.tight_layout()
    plt.plot()
    print("OK")
    plt.savefig('TESTE.png')
    print("OK")
    #lista_format = [round(elem, 3) for elem in lista]
    #plt.plot(lista_format)
    #print(lista_format)
   
    #
    return

plot_DIARIO_1_var_3_level("TEMP-250","TEMP-500","TEMP-850","TEMP","VIES","24",4,"HN","00")
















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