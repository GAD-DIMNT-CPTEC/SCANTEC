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

from time_date import *
from datetime import date
from matplotlib.ticker import StrMethodFormatter

plt.switch_backend('agg')
# Foi acrescentado a linha acima pra retirar um erro que estava ocorrendo quando executava esse script
#na maquina virtual itapemirim. Erro:
# RuntimeError: Invalid DISPLAY variable

print("##versões utilizadas##")
print("Python version: {}". format(sys.version))
print("NumPy version: {}". format(np.__version__))
print("pandas version: {}". format(pd.__version__))
print("matplotlib version: {}". format(matplotlib.__version__))
print("seaborn version: {}". format(sns.__version__))
print("    ")

# NEW: Alterar o caminho para as tabelas do SCANTEC: (atentar para a barra [/] ao final do nome)
#base_path = 'aval_SMG/diario/00Z/'
#base_path = "/home/carlos/Documents/INPE2019/GDAD/SMG/teste_scamtec_v100/repo_carlos/wsantos.t4084/tmp/SCANPLOT/aval_SMG/diario/00Z/"

# NEW: Adicionar os experimentos a serem processados: (atentar para o ponto [.] ao final do nome)
#files = ['SMG_V2.0.0.',
#         'SMG_V2.1.0.',
#         'SMG_V2.1.2.',
#         'SMG_V2.1.3.']

files = ['SMG_V2.0.0.',
         'SMG_V2.1.0.']

# Função "plot_diario":
# Recebe as informações abaixo para ler as tabelas do SCANTEC e plotar os
# gŕficos de linha correspondentes aos parâmetros de entrada.
# Parâmetros de entrada:
# start_dt.:
# end_dt...:                  
# title....:
# statistic:
# fct.:
# lev....:
# linha....:
# regiao...:
# hsin..:

# Função criada exclusivamente para fazer o looping e escolha da previsão e linha.
# Ela retornará o valor da linha de acordo com o valor da previsão que passar por ela;
# no 'return' ele retornará exclusivamente o valor da linha selecionada.
# Na função plot_diario, chamaremos a função 'fct_linha' atrelando ela a uma variável chamada 'linha'.
# Ficando assim visualmente:                         linha =  fct_linha(fct) 
# Que na verdade ele estará passando assim:          linha =  4 




def previsao_linha(fct):
    if fct==24:
        linha=4
    elif fct==48:
        linha=8
    elif fct==72:
        linha=12
    return (linha)



def plot_diario(    diaInicial,
                    diaFinal,
                    mes,
                    ano,
                    Vars,
                    Stats,
                    Fcts,
                    Regs,
                    Hsins,
                    base_path_diario
                ):

    # Classe criada para padronizar o jeito que é inserido os dias, mes e ano
    #de todos os scripts
    #call class
    cclass = definirData(diaInicial,diaFinal,mes,ano)
    start_dt = cclass.getStart()
    end_dt = cclass.getEnd()


    for var_name in Vars:
        
        var=var_name.split("-",1)[0]
        lev=var_name.split("-",1)[1]

        for fct in Fcts:

            for reg in Regs:

                for stat in Stats:
               
                    for hsin in Hsins:

                        var_1 = str(var) + '-' + str(lev) 

                        #função que entra a 'fct' passada pela função 'plot_diario' e saio o valor da linha na tabela do Scantec
                        linha = previsao_linha(fct)

#                        print("\n>> Inicio: " + str(start_dt) + "  Fim: " + str(end_dt))
                        
                        # NEW: armazena as listas e datas de cada experimento especificado na variável 'base_path'
                        dic_listas = {}
                        dic_datas = {}

                        for dt in daterange(start_dt, end_dt):
                        
                            dia = dt.strftime("%d")
                            mes = dt.strftime("%m")
                            ano = dt.strftime("%Y")

#                            print("\n >",var,lev,fct,reg,hsin)
                            
                            # NEW: processa os experimentos especificados na variável 'base_path'
                            for index, file in enumerate(files):
                            
                                allFiles = glob.glob(base_path_diario + file + str(reg) + '/'
                                                                      + str(stat)
                                                                      + 'EXP01_'
                                                                      + str(ano)
                                                                      + str(mes)
                                                                      + str(dia)
                                                                      + str(hsin)
                                                                      + str(ano)
                                                                      + str(mes)
                                                                      + str(dia)
                                                                      + str(hsin)
                                                                      + 'T.scam')
                                allFiles.sort()

#                                print("allFiles_{}: {}\n".format(index, allFiles))
                                
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

#                            print("lista_{}_fmt: {}\n".format(index, lista_n_fmt))
                            
                            lista_n_min = np.nanmin(lista_n_fmt)
                            lista_n_max = np.nanmax(lista_n_fmt)

                            if np.isnan(lista_min) or lista_n_min < lista_min:
                                lista_min = lista_n_min

                            if np.isnan(lista_max) or lista_n_max > lista_max:
                                lista_max = lista_n_max

                        # NEW: obtem a lista com o maior numero de datas
                        datas = ()
                        for index, datas_n in dic_datas.items():
#                            print("datas_{}: {}\n".format(index, datas_n))
                            
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

                            plt.ylabel(str(stat))
                            plt.xlabel('Dia')

                            plt.title(str(stat)
                                        +' Diário '
                                        + var + '-'
                                        + str(lev) + 'hPa'
                                        + ' ' + str(reg.upper())
                                        + ' FCT '
                                        + str(fct) + 'h'
                                        + '\n'
                                        + str(start_dt)
                                        + str(hsin) 
                                        + ' - '
                                        + str(end_dt) 
                                        + str(hsin)
                                        + ' '
                                        )

                            plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}')) # Sem casas decimais
                            plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.3f}')) # Com 3 casas decimais

                            plt.tick_params(labelsize=7, pad=-1.5)
                            plt.xticks(x_tick_labels,rotation=45)

                            fig.align_labels()

#                            print("lista_min:",lista_min,"lista_max:",lista_max)

                            # Não vai servir para todas as variáveis...
                            if stat == "ACOR":
                                plt.ylim((lista_min - 0.1),1.0)
                    #        elif statistic == "RMSE":
                    #            if lista_min <= 0.:
                    #                plt.ylim((lista_min - 0.1),(lista_max + 0.1))
                    #            elif lista_min >= 0.:
                    #                plt.ylim((lista_min - 1.),(lista_max + 1.))
                    #            else:
                    #                plt.ylim(lista_min,lista_max)
                            elif stat == "VIES":
                                plt.axhline(y=0, color='black')
                    #            if lista_min <= 0.:
                    #                plt.ylim((lista_min - 0.1),(lista_max + 0.1))
                    #            elif lista_min >= 0.:
                    #                plt.ylim((lista_min - 1.),(lista_max + 1.))
                    #            else:
                    #                plt.ylim(lista_min,lista_max)

                            plt.legend()

                            print("figura:",'output/diario/' 
                                            + str(reg.upper()) 
                                            + '/' 
                                            + str(stat) 
                                            + '_DIARIO_' 
                                            + str(var) 
                                            + '-' 
                                            + str(lev) 
                                            + '_' 
                                            + str(reg.upper()) 
                                            + '_' 
                                            + str(fct) 
                                            + 'h_' 
                                            + str(start_dt) 
                                            + str(hsin) 
                                            + '_' 
                                            + str(end_dt) 
                                            + str(hsin) 
                                            + '.png'
                                            )
                            plt.savefig('output/diario/' 
                                            + str(reg.upper()) 
                                            + '/' 
                                            + str(stat) 
                                            + '_DIARIO_' 
                                            + str(var) 
                                            + '-' 
                                            + str(lev) 
                                            + '_' 
                                            + str(reg.upper()) 
                                            + '_' 
                                            + str(fct) 
                                            + 'h_' 
                                            + str(start_dt) 
                                            + str(hsin) 
                                            + '_' 
                                            + str(end_dt) 
                                            + str(hsin) 
                                            + '.png', dpi=200)

                            plt.close('all')
                            plt.show()

    return 

#diaInicial = 2
#diaFinal   = 31
#mes = 5
#ano = 2015
##
##
## Escolha das variáveis (em listas)
#Vars = ["VVEL-850"]
#Regs = ["gl"]
#Stats = ["ACOR"]
#Hsins = ["00"]
#Fcts = np.arange(24,96,24)
##
## Caminho para absoluto para as tabelas
#base_path = "./aval_SMG/diario/00Z/"
##
#plot_diario(diaInicial,diaFinal,mes,ano,Vars,Stats,Fcts,Regs,Hsins,base_path)
