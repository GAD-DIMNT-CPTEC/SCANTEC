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

 # Função "plot_mensal":
# Recebe as informações abaixo para ler as tabelas do SCANTEC e plotar os
# gŕficos de linha correspondentes aos parâmetros de entrada.
# Parâmetros de entrada:
# diaInicial.:
# diaFinal...:                  
# mes....:
# ano....:
# statistic:
# lev....:
# linha....:
# regiao...:
# hsin..:        

def plot_mensal(
                    diaInicial,
                    diaFinal,
                    mes,
                    ano,
                    Vars,
                    Stats,
                    Regs,
                    Hsins,
                    base_path_mensal
                    ):

        cclass = definirData(diaInicial,diaFinal,mes,ano)
        
        # Convertendo para o formato desejado
        diaInicial = cclass.getStart().strftime("%d")
        diaFinal = cclass.getEnd().strftime("%d")
        mes = cclass.getStart().strftime("%m")
     

        for var_name in Vars:
            #    
            var=var_name.split("-",1)[0]
            lev=var_name.split("-",1)[1]
            #
            #
            #
            for reg in Regs:
            #
                for stat in Stats:
            #      
                    for hsin in Hsins:
                        #
                        #
                        var_1 = str(var) +'-'+ str(lev) #"title+level
                        #
                        # NEW: armazena as listas e datas de cada experimento especificado na variável 'base_path'
                        dic_listas = {}
                        #
                        #
                        # NEW: processa os experimentos especificados na variável 'base_path'
                        for index, file in enumerate(files):
                        #
                            print("")
                            allFiles = glob.glob(base_path_mensal  + file  + str(reg) + '/'
                                                                    +str(stat)
                                                                    +'EXP01_'
                                                                    + str(ano)
                                                                    + str(mes) 
                                                                    + str(diaInicial) 
                                                                    + str(hsin)
                                                                    + str(ano)
                                                                    + str(mes) 
                                                                    + str(diaFinal) 
                                                                    + str(hsin)
                                                                    +'T.scam')
                            allFiles.sort()
                            #
                            lista_n = dic_listas.setdefault(index, list())
                            #
                            if len(allFiles) != 0:
                                file_n = allFiles[0]
                                name_file_n = ntpath.basename(file_n)
                                df_n = pd.read_csv(file_n, sep="\s+")                  
                                td_n = df_n.loc[0:,var_1]
                                lista_n.extend(td_n.tolist())
                            else:
                                print("")
                            # 
                            #
                     
                        
                        for index, lista_n in dic_listas.items():
                            lista_n_fmt = [round(elem, 3) for elem in lista_n]
                            print("")
                            dic_listas[index] = lista_n_fmt

                       
                        #
                        sns.set()
                        #
                        fig=plt.figure()
                        #
                        #NEW: plota a informacao de cada lista formatada
                        for index, lista_n_fmt in dic_listas.items():
                            marker = '${}$'.format(index)
                            label = re.findall('SMG_(.+).', files[index])[0]
                            plt.plot(lista_n_fmt, marker=marker, label=label)
                        #
                  

                        plt.ylabel(str(stat))
                        plt.xlabel("mensal")
                        #
                        plt.title(str(stat)
                                    +' Mensal '
                                    +str(var)
                                    +'-'
                                    + str(lev)
                                    +' '
                                    +str(reg)
                                    +' FCT '
                                    +'\n'
                                    + str(ano)
                                    + str(mes)
                                    + str(diaInicial)
                                    + str(hsin)
                                    +' - '
                                    + str(ano)
                                    + str(mes) 
                                    + str(diaFinal) 
                                    + str(hsin)+' ')
                        #
                        plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.0f}')) # Sem casas decimais
                        plt.gca().yaxis.set_major_formatter(StrMethodFormatter('{x:,.3f}')) # Com 3 casas decimais
                        #
                        plt.tick_params(labelsize=7, pad=-1.5)
                        #
                        plt.xticks(rotation=45)
                        fig.align_labels()
                        #
                        #print("lista_min",lista_min,"lista_max",lista_max)
                        #
                        if stat == "ACOR":
                            print("")
                            #plt.ylim((lista_min - 0.1),1.0)
                        elif stat == "VIES":
                            plt.axhline(y=0, color='black')
                        #
                        plt.legend()
                        #
                        print("figura:",'output/mensal/' 
                                            + str(reg.upper()) 
                                            + '/' 
                                            + str(stat) 
                                            + '_MENSAL_' 
                                            + str(var) 
                                            + '-' 
                                            + str(lev) 
                                            + '_' 
                                            + str(reg.upper()) 
                                            + '_' 
                                            + str(diaInicial)
                                            + str(mes) 
                                            + str(ano)  
                                            + str(hsin) 
                                            + '_' 
                                            + str(diaFinal)
                                            + str(mes) 
                                            + str(ano)
                                            + str(hsin) 
                                            + '.png'
                                            )
                        plt.savefig('output/mensal/' 
                                            + str(reg.upper()) 
                                            + '/' 
                                            + str(stat) 
                                            + '_MENSAL_' 
                                            + str(var) 
                                            + '-' 
                                            + str(lev) 
                                            + '_' 
                                            + str(reg.upper()) 
                                            + '_'
                                            + str(diaInicial)
                                            + str(mes) 
                                            + str(ano) 
                                            + str(hsin) 
                                            + '_' 
                                            + str(diaFinal)
                                            + str(mes) 
                                            + str(ano)
                                            + str(hsin) 
                                            + '.png', dpi=200)



                        #plt.close('all')
                        plt.show()
        return

#########################################################################################################
#EXECUÇÃO E PASSAGEM DE PARAMÊTROS

#diaInicial = 2
#diaFinal   = 31
#mes = 5
#ano = 2015
##
## Escolha das variáveis (em listas)
#Vars = ["VVEL-850"]
#Regs = ["hn"]
#Stats = ["ACOR"]
#Hsins = ["00"]
##
## Caminho para absoluto para as tabelas
#base_path = "./aval_SMG/mensal/00Z/"
##
##print(start_dt,end_dt,stat,var,lev,reg,hsin)
#plot_mensal(diaInicial,diaFinal,mes,ano,Vars,Stats,Regs,Hsins,base_path)

