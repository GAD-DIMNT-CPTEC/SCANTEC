#! /usr/bin/env python3
#-*- coding: utf-8 -*-

"""
Objetivo: Este script realiza a plotagem das tabelas do SCANTEC (ACOR, RMSE e VIES)
          para o período indicado, em função dos dias da avaliação.

Uso: python3 scanplot_diario.py

Observações: Alterar o valor das variáveis "start_dt", "end_dt", "base_path_1" e "base_path_2".

Versões dos pacotes:

Python 3.6.5.final.0
conda version : 4.5.11
numpy==1.14.3
pandas==0.23.0
seaborn==0.8.1

obs.:Os outros pacotes são nativos do python, logo suas releases acompanham
a versão do python

############################################################################

O pacote : from fjso import select
Foi criado uma função para manipular os dados dentro do scanplot.json
SCANPLOT
    |__fjso.py
    json
        |__ scanplot.json

O pacote: from time_date import daterange
Foi criado uma função para variação de Ano, Mês e Dia.                        
SCANPLOT
    |__time_date.py
"""

import glob
import ntpath
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from fjso import select
from time_date import daterange
from datetime import date
from matplotlib.ticker import StrMethodFormatter

# Alterar os valores das dates de início e fim da avaliação diária:
start_dt = date(2015,5,1)
end_dt = date(2015,5,31)

# Alterar os caminhos para as tabelas do SCANTEC:
base_path_1 = 'dadosscantec/aval_SMG/diario/00Z/SMG_V2.0.0.'
base_path_2 = 'dadosscantec/aval_SMG/diario/00Z/SMG_V2.1.0.'

# Função "plot_diario":
# Recebe as informações abaixo para ler as tabelas do SCANTEC e plotar os
# gŕficos de linha correspondentes aos parâmetros de entrada.
# Parâmetros de entrada:
# start_dt.:
# end_dt...:                  
# title....:
# statistic:
# previsao.:
# level....:
# linha....:
# regiao...:
# horario..:

def plot_diario(
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

    var_1 = str(title) + '-' + str(level) 

    path_1 = base_path_1 + str(regiao)
    path_2 = base_path_2 + str(regiao)
    
    print("\n>> Inicio: " + str(start_dt) + "  Fim: " + str(end_dt))

    lista_1 = []
    datas_1 = []
    
    lista_2 = []
    datas_2 = []
    
    for dt in daterange(start_dt, end_dt):

        dia = dt.strftime("%d")
        mes = dt.strftime("%m")
        ano = dt.strftime("%Y")

        print("\n >",title,level,previsao,regiao,horario)

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
            td_1 = df_1.loc[positionSCANTEC,var_1]
            lista_1.append(td_1)
        else:
            datas_1.append(np.nan)
            lista_1.append(np.nan)
        
        if len(allFiles_2) != 0:
            file_2 = allFiles_2[0]
            name_file_2 = ntpath.basename(file_2)
            datas_2.append(name_file_2[26:28])
            df_2 = pd.read_csv(file_2, sep="\s+")
            ts_2 = df_2.loc[positionSCANTEC,var_1]
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

    if len(datas_2) >= len(datas_1):
        datas = datas_2
    else:
        datas = datas_1
 
    lista_1_min = np.nanmin(lista_1_fmt)
    lista_1_max = np.nanmax(lista_1_fmt)
    lista_2_min = np.nanmin(lista_2_fmt)
    lista_2_max = np.nanmax(lista_2_fmt)

    if lista_2_min <= lista_1_min:
        lista_min = lista_2_min
    else:
        lista_min = lista_1_min

    if lista_2_max >= lista_1_max:
        lista_max = lista_2_max
    else:
        lista_max = lista_1_max

    if np.isnan(lista_max) and np.isnan(lista_min):
        print("All NaNs")
    else:
        # Figura
        x_tick_labels = np.arange(1,len(datas)+1,1)
        
        sns.set()
        
        fig=plt.figure()

        plt.tight_layout()

        plt.plot(x_tick_labels,lista_1_fmt, marker='8', label='v2.0.0')
        plt.plot(x_tick_labels,lista_2_fmt, marker='s', label='v2.1.0')
        
        plt.ylabel(str(statistic))
        plt.xlabel('Dia')
    
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
    
        plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}')) # Sem casas decimais
        plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.3f}')) # Com 3 casas decimais
    
        plt.tick_params(labelsize=7, pad=-1.5)
        plt.xticks(x_tick_labels,rotation=45)
    
        fig.align_labels()
    
        print("lista_min:",lista_min,"lista_max:",lista_max)
    
        # Não vai servir para todas as variáveis...
        if statistic == "ACOR":
            plt.ylim((lista_min - 0.1),1.0)
#        elif statistic == "RMSE":
#            if lista_min <= 0.:
#                plt.ylim((lista_min - 0.1),(lista_max + 0.1))
#            elif lista_min >= 0.:
#                plt.ylim((lista_min - 1.),(lista_max + 1.))
#            else:
#                plt.ylim(lista_min,lista_max)
        elif statistic == "VIES":
            plt.axhline(y=0, color='black')
#            if lista_min <= 0.:
#                plt.ylim((lista_min - 0.1),(lista_max + 0.1))
#            elif lista_min >= 0.:
#                plt.ylim((lista_min - 1.),(lista_max + 1.))
#            else:
#                plt.ylim(lista_min,lista_max)
    
        plt.legend()
    
        print("figura:",'output/diario/' 
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
                        + 'h_' 
                        + str(start_dt) 
                        + str(horario) 
                        + '_' 
                        + str(end_dt) 
                        + str(horario) 
                        + '.png'
                        )
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
                        + 'h_' 
                        + str(start_dt) 
                        + str(horario) 
                        + '_' 
                        + str(end_dt) 
                        + str(horario) 
                        + '.png', dpi=200)
    
        plt.close('all')

    return

# Main
a = select(6,3,12,5,6,5,2,4,2)

#Exps=["SMG_V2.0.0.","SMG_V2.1.0."]
#Vars=["TEMP-850", "PSNM-000", "UMES-925", "ZGEO-500", "UVEL-250", "VVEL-850"]
#Regs=["hn", "tr", "hs", "as"]
#Stats=["VIES","RMSE","ACOR"]
Hsins=["00"]
Vars=["VVEL-850"]
Regs=["as"]
Stats=["ACOR"]
Hsins=["00"]

for var_name in Vars:

    var=var_name.split("-",1)[0]
    lev=var_name.split("-",1)[1]

    for fct in np.arange(24,96,24):

        if fct==24:
            l_fct=4
        elif fct==48:
            l_fct=8
        elif fct==72:
            l_fct=12
        
        for reg in Regs:

            for stat in Stats:
 
                for hsin in Hsins:
                
                    print(start_dt,end_dt,stat,var,lev,fct,l_fct,reg,hsin)
                    plot_diario(start_dt,end_dt,var,stat,fct,lev,l_fct,reg,hsin)