#! /usr/bin/env python3
#-*- coding: utf-8 -*-

"""
Objetivo: Este script realiza a plotagem das tabelas do SCANTEC (ACOR, RMSE e VIES)
          para o período indicado, em função dos dias da avaliação.

Uso: python3 scanplot_diario.py

Observações: Alterar o valor das variáveis "start_dt", "end_dt", "base_path" e "files".

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

# NEW: Importa a biblioteca de regex
import re
import sys
import glob
import ntpath
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib
import matplotlib.pyplot as plt

from fjso import select
from time_date import daterange
from datetime import date
from matplotlib.ticker import StrMethodFormatter

print("##versões utilizadas##")
print("Python version: {}". format(sys.version))
print("NumPy version: {}". format(np.__version__))
print("pandas version: {}". format(pd.__version__))
print("matplotlib version: {}". format(matplotlib.__version__))
print("seaborn version: {}". format(sns.__version__))
print("    ")


# Alterar os valores das dates de início e fim da avaliação diária:
start_dt = date(2015,5,1)
end_dt = date(2015,5,31)

# NEW: Alterar o caminho para as tabelas do SCANTEC: (atentar para a barra [/] ao final do nome)
base_path = 'dadosscantec/aval_SMG/diario/00Z/'

# NEW: Adicionar os experimentos a serem processados: (atentar para o ponto [.] ao final do nome)
files = ['SMG_V2.0.0.',
         'SMG_V2.1.0.',
         'SMG_V2.1.2.',
         'SMG_V2.1.3.']

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

# Função criada exclusivamente para fazer o looping e escolha da previsão e linha.
# Ela retornará o valor da linha de acordo com o valor da previsão que passar por ela;
# no 'return' ele retornará exclusivamente o valor da linha selecionada.
# Na função plot_diario, chamaremos a função 'previsao_linha' atrelando ela a uma variável chamada 'linha'.
# Ficando assim visualmente:                         linha =  previsao_linha(previsao) 
# Que na verdade ele estará passando assim:          linha =  4 

def previsao_linha(previsao):
    if previsao==24:
        linha=4
    elif previsao==48:
        linha=8
    elif previsao==72:
        linha=12
    return (linha)



def plot_diario( start_dt,
                 end_dt,
                 var,
                 statistic,
                 previsao,
                 level,
                 regiao,
                 horario
                ):


    var_1 = str(var) + '-' + str(level) 

    #função que entra a 'previsao' passada pela função 'plot_diario' e saio o valor da linha na tabela do Scantec
    linha = previsao_linha(previsao)

    print("\n>> Inicio: " + str(start_dt) + "  Fim: " + str(end_dt))

    # NEW: armazena as listas e datas de cada experimento especificado na variável 'base_path'
    dic_listas = {}
    dic_datas = {}
    
    for dt in daterange(start_dt, end_dt):

        dia = dt.strftime("%d")
        mes = dt.strftime("%m")
        ano = dt.strftime("%Y")

        print("\n >",var,level,previsao,regiao,horario)

        # NEW: processa os experimentos especificados na variável 'base_path'
        for index, file in enumerate(files):

            allFiles = glob.glob(base_path + file + str(regiao) + '/'
                                                  + str(statistic)
                                                  + 'EXP01_'
                                                  + str(ano)
                                                  + str(mes)
                                                  + str(dia)
                                                  + str(horario)
                                                  + str(ano)
                                                  + str(mes)
                                                  + str(dia)
                                                  + str(horario)
                                                  + 'T.scam')
            allFiles.sort()

            print("allFiles_{}: {}\n".format(index, allFiles))

            lista_n = dic_listas.setdefault(index, list())
            datas_n = dic_datas.setdefault(index, list())

            if len(allFiles) != 0:
                file_n = allFiles[0]
                name_file_n = ntpath.basename(file_n)
                datas_n.append(name_file_n[26:28])
                df_n = pd.read_csv(file_n, sep="\s+")
                td_n = df_n.loc[linha, var_1]
                lista_n.append(td_n)
            else:
                datas_n.append(np.nan)
                lista_n.append(np.nan)

    # NEW: obtem os valores minimos e maximos das listas
    lista_min = np.nan
    lista_max = np.nan

    for index, lista_n in dic_listas.items():
        lista_n_fmt = [round(elem, 3) for elem in lista_n]
        dic_listas[index] = lista_n_fmt

        print("lista_{}_fmt: {}\n".format(index, lista_n_fmt))

        lista_n_min = np.nanmin(lista_n_fmt)
        lista_n_max = np.nanmax(lista_n_fmt)

        if np.isnan(lista_min) or lista_n_min < lista_min:
            lista_min = lista_n_min

        if np.isnan(lista_max) or lista_n_max > lista_max:
            lista_max = lista_n_max

    # NEW: obtem a lista com o maior numero de datas
    datas = ()
    for index, datas_n in dic_datas.items():
        print("datas_{}: {}\n".format(index, datas_n))

        if len(datas_n) > len(datas):
            datas = datas_n

    if np.isnan(lista_max) and np.isnan(lista_min):
        print("All NaNs")
    else:
        # Figura
        x_tick_labels = np.arange(1,len(datas)+1,1)
        
        sns.set()
        
        fig=plt.figure()

        plt.tight_layout()

        # NEW: plota a informacao de cada lista formatada
        for index, lista_n_fmt in dic_listas.items():
            marker = '${}$'.format(index)
            label = re.findall('SMG_(.+).', files[index])[0]
            plt.plot(x_tick_labels, lista_n_fmt, marker=marker, label=label)

        plt.ylabel(str(statistic))
        plt.xlabel('Dia')
    
        plt.title(str(statistic)
                    +' Diário '
                    + var + '-'
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
                        + str(var) 
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
                        + str(var) 
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
#a = select(6,3,12,5,6,5,2,4,2)
#
##Exps=["SMG_V2.0.0.","SMG_V2.1.0."]
##Vars=["TEMP-850", "PSNM-000", "UMES-925", "ZGEO-500", "UVEL-250", "VVEL-850"]
##Regs=["hn", "tr", "hs", "as"]
##Stats=["VIES","RMSE","ACOR"]
#Hsins=["00"]
#Vars=["VVEL-850"]
#Regs=["as"]
#Stats=["ACOR"]
#Hsins=["00"]
#
#for var_name in Vars:
#
#    var=var_name.split("-",1)[0]
#    lev=var_name.split("-",1)[1]
#
#    for fct in np.arange(24,96,24):
#        for reg in Regs:
#
#            for stat in Stats:
# 
#                for hsin in Hsins:
#                
#                    print(start_dt,end_dt,stat,var,lev,fct,reg,hsin)
#                    plot_diario(start_dt,end_dt,var,stat,fct,lev,reg,hsin)
#