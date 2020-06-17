#! /usr/bin/env python3
#-*- coding: utf-8 -*-

"""
scanplot
========

    Este módulo contém funções associadas à leitura das informações do namelist do SCANTEC
    e à plotagem das tabelas do SCANTEC (ACOR, RMSE e VIES).
    
Funções
-------
    read_nemalists : lê os namelists e arquivos de definições do SCANTEC.
    get_dataframe  : transforma as tabelas do SCANTEC em dataframes.
    plot_lines     : plota as tabelas do SCANTEC a partir dos dataframes.
    plot_scorecard : resume as informações das tabelas em um scorecard.    
"""

import re
import os.path
import ntpath
import numpy as np
import pandas as pd
from datetime import date, datetime
import matplotlib.pyplot as plt
import seaborn as sns

def read_namelists(basepath):

    """
    read_namelists
    ==============
    
    Esta função lê os namelists e arquivos de definições dos modelos do SCANTEC e
    retorna para o usuário dois dicionários, VarsLevs e Confs, com as informações lidas.
    
    Parâmetros de entrada
    ---------------------
        basepath : diretório raiz da instalação do SCANTEC.
        
    Resultados
    ----------
        VarsLevs : dicionário com as variáveis, níveis e nomes definidos no arquivo scantec.vars
        Confs    : dicionário com as definições contidas no arquivo scantec.conf
    
    Uso
    ---
        from scanplot import read_namelists
        data_vars, data_conf = read_namelists("~/SCANTEC")
    """
    
    # Lê o arquivo scantec.vars e transforma a lista de variáveis e níveis e um dicionário
    filename = str(basepath) + '/tables/scantec.vars'
     
    VarsLevs = {}
    
    # Com o método "with open", o arquivo é fechado automaticamente ao final
    with open(filename,'r') as scantec_vars:
      for idx, line in enumerate(scantec_vars.readlines(), start=-4):
        rline = line.lstrip()
        if not (rline.startswith('#') or rline.startswith('::') or rline.startswith('variables:')):
          varlevdesc = rline.strip().split(' ', 1)
          VarsLevs[idx] = (varlevdesc[0], varlevdesc[1].strip('\"'))
        
    # Lê do arquivo scantec.conf e transforma as informações principais em um dicionário
    filename = str(basepath) + '/bin/scantec.conf'
    
    # A função a seguir lê a linha com a informação requerida e cria uma lista com os elementos separados 
    # de acordo com o separador ':'
    Confs = {}
    
    def key_value(linew):
      nlist = re.split(': ',linew)
      key = nlist[0]
      value = nlist[1].split()[0]
      if key == 'Starting Time' or key == 'Ending Time':
          value = datetime.strptime(value, "%Y%m%d%H")
      Confs[key] = value
      return Confs 
    
    # A função a seguir lê a lista de experimentos e cria um dicionário
    Exps = {}
    
    def key_value_exps(lexps):
      for i in range(2, len(lexps)): # 2: desconsidera as linhas "Experiments:" e "#ModelId Name Diretory File_Name_with_mask"
        slexps = lexps[i].split()
        Exps[slexps[1]] = [slexps[0], slexps[2]]
        Confs['Experiments'] = Exps
      return Confs
    
    # Com o método "with open", o arquivo é fechado automaticamente ao final
    with open(filename,'r') as scantec_conf:
      for line in scantec_conf:
        if line.startswith('Starting Time'):
          key_value(line)
        elif line.startswith('Ending Time'):
          key_value(line)
        elif line.startswith('Analisys Time Step'):
          key_value(line)
        elif line.startswith('Forecast Time Step'):
          key_value(line)
        elif line.startswith('History Time'):
          key_value(line)
        elif line.startswith('scantec tables'):
          key_value(line)
        elif line.startswith('run domain number'):
          key_value(line)
        elif line.startswith('run domain lower left lat'):
          key_value(line)
        elif line.startswith('run domain lower left lon'):
          key_value(line)
        elif line.startswith('run domain upper right lat'):
          key_value(line)
        elif line.startswith('run domain upper right lon'):
          key_value(line)
        elif line.startswith('run domain resolution dx'):
          key_value(line)
        elif line.startswith('run domain resolution dy'):
          key_value(line)
        elif line.startswith('Reference Model Name'):
          key_value(line)
        elif line.startswith('Experiments'):
          exps = []
          while not(line.startswith('::')):
            exps.append(line)
            line = next(scantec_conf)
          key_value_exps(exps)
        elif line.startswith('Reference file'):
          key_value(line)
        elif line.startswith('Climatology Model Name'):
          key_value(line)
        elif line.startswith('Climatology file'):
          key_value(line)
        elif line.startswith('Output directory'):
          key_value(line)

    return VarsLevs, Confs

def get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir):

    """
    get_dataframe
    ==========
    
    Esta função transforma a(s) tabela(s) do SCANTEC em dataframe(s).
    
    Parâmetros de entrada
    ---------------------
        dataInicial : objeto datetime com a data inicial do experimento
        dataFinal   : objeto datetime com a data final do experimento
        Stats       : lista com os nomes das estatísticas a serem processadas
        Exps        : lista com os nomes dos experimentos
        outDir      : string com o diretório com as tabelas do SCANTEC
    
    Resultado
    ---------
        Dicionário com o(s) dataframe(s) com a(s) tabela(s) do SCANTEC.
    
    Uso
    ---
        from scanplot import get_dataframe
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Stats =  ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        outDir = data_conf["Output directory"]
        
        dTable = get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
    """
    
    ds_table = {}
    
    for stat in Stats:
               
        dataInicial_fmt = dataInicial.strftime("%Y%m%d%H")
        dataFinal_fmt = dataFinal.strftime("%Y%m%d%H")

        for exp in Exps:
        
            table = outDir + '/' + str(stat) + str(exp) + '_' + str(dataInicial_fmt) + str(dataFinal_fmt) + 'T.scan'
                 
            lista_n = []

            if os.path.exists(table):
                df_n = pd.read_csv(table, sep="\s+")

                ds_table[ntpath.basename(str(table))] = df_n    
                    
    return ds_table

def plot_lines(dTable,Vars,Stats,outDir,combine):

    """
    plot_lines
    ============
    
    Esta função plota um gráfico de linha a partir de um dicionário de tabelas do SCANTEC.
    
    Parâmetros de entrada
    ---------------------
        dTable  : objeto dicionário com uma ou mais tabelas do SCANTEC
        Vars    : lista com os nomes e níveis das variáveis
        Stats   : lista com os nomes das estatísticas a serem processadas
        outDir  : string com o diretório com as tabelas do SCANTEC
        combine : valor Booleano para combinar as curvas dos experimentos em um só gráfico
    
    Resultado
    ---------
        Figuras salvas no diretório definido na variável outDir (SCANTEC/dataout).
    
    Uso
    ---
        from scanplot import plot_lines
        
        plot_lines(dTable,Vars,Stats,outDir,combine=False)
    """
    
    if combine:

        for var in range(len(Vars)):
       
            for Stat in Stats:
                Tables = list(filter(lambda x:Stat in x, [*dTable.keys()]))
       
                dfTables = []
    
                for table in Tables:
                    df_exp = dTable[table].loc[:,[Vars[var][0].lower()]]
                    dfTables.append(df_exp)
            
       
                    #print(table,dfTables)
        
                fcts = dTable[table].loc[:,"%Previsao"].values
            
                ax = pd.concat(dfTables,axis=1).plot(title=Vars[var][1],
                                                    figsize=(8,5),
                                                    fontsize=12,
                                                    linewidth=3)
    
                ax.legend(list(table[4:9]))
            
                ax.set_xticks(dTable[table].index)
                ax.set_xticklabels(fcts)

                ax.legend(Tables)
                
                plt.ylabel(Stat)
                plt.xlabel('Tempo')

                plt.grid()
                
                plt.savefig(outDir + '/' + table + '-' + Vars[var][0] + '-combined.png', dpi=70) 
              
    else:
            
        for table in list(dTable.keys()):
            Stat = table[0:4]
            fcts = dTable[table].loc[:,"%Previsao"].values

            for var in range(len(Vars)):
                vname = Vars[var]
            
                ax = dTable[table].loc[:,[Vars[var][0].lower()]].plot(title=Vars[var][1], 
                                                                      figsize=(8,5),
                                                                      fontsize=12,
                                                                      linewidth=3)
 
                ax.set_xticks(dTable[table].index)
                ax.set_xticklabels(fcts)
        
                ax.legend(table)
            
                plt.ylabel(Stat)
                plt.xlabel('Tempo')
        
                plt.grid()
            
                plt.savefig(outDir + '/' + table + '-' + Vars[var][0] + '.png', dpi=70) 
                
    return

def plot_scorecard(dTable,Vars,Stats,Tstat,outDir):
    """
    plot_scorecard
    ==============
    
    Esta função calcula o "Ganho Percentual*" e a "Mudança Fracional*" a partir 
    das estatísticas do SCANTEC e plota os resultados na forma de um scorecard. 
    São necessários dois experimentos.
    
    *Banos et al., 2018: Impacto da Assimilação de Perfis de Refratividade do 
                         Satélite Metop-B nas Previsões de Tempo do CPTEC/INPE 
                         Durante os Meses de Janeiro e Agosto de 2014.
    
    Parâmetros de entrada
    ---------------------
        dTable  : objeto dicionário com uma ou mais tabelas do SCANTEC
        Vars    : lista com os nomes e níveis das variáveis
        Stats   : lista com os nomes das estatísticas a serem processadas
        Tstat   : tipo de score a ser calculado
        outDir  : string com o diretório com as tabelas do SCANTEC
    
    Resultado
    ---------
        Figuras salvas no diretório definido na variável outDir (SCANTEC/dataout).
    
    Uso
    ---
        from scanplot import plot_scorecard
        
        plot_scorecard(dTable,Vars,Stats,Tstat,outDir)
    """
    
    list_var = [ltuple[0].lower() for ltuple in Vars]

    for Stat in Stats:
        Tables = list(filter(lambda x:Stat in x, [*dTable.keys()]))
    
        p_table1 = pd.pivot_table(dTable[Tables[0]], index="%Previsao", values=list_var)
        p_table2 = pd.pivot_table(dTable[Tables[1]], index="%Previsao", values=list_var)
 
        if Tstat == "ganho":
            # Porcentagem de ganho
            if Stat == "ACOR":
                score_table = ((p_table2[1:].T - p_table1[1:].T) / (1.0 - p_table1[1:].T)) * 100
            elif Stat == "RMSE" or Stat == "VIES":
                score_table = ((p_table2[1:].T - p_table1[1:].T) / (0.0 - p_table1[1:].T)) * 100
        elif Tstat == "fc":
            # Mudança fracional
            score_table = (1.0 - (p_table2[1:].T / p_table1[1:].T))
 
        # Figura
        plt.figure(figsize = (8,6))
    
        sns.set(style="whitegrid", font_scale=1.25)
        sns.set_context(rc={"xtick.major.size":  1.5,  "ytick.major.size": 1.5,
                            "xtick.major.pad":   0.05,  "ytick.major.pad": 0.05,
                            "xtick.major.width": 0.5, "ytick.major.width": 0.5,
                            "xtick.minor.size":  1.5,  "ytick.minor.size": 1.5,
                            "xtick.minor.pad":   0.05,  "ytick.minor.pad": 0.05,
                            "xtick.minor.width": 0.5, "ytick.minor.width": 0.5})
 
        if Tstat == "ganho":
            ax = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                               vmin=-100, vmax=100, center=0, linewidths=0.25,
                               cbar_kws={"shrink": 1.0, 
                                         "ticks": np.arange(-100,110,10),
                                         "pad": 0.01,
                                         "orientation": "vertical"})
 
            cbar = ax.collections[0].colorbar
            cbar.set_ticks([-100, -50, 0, 50, 100])
            cbar.set_ticklabels(["pior", "-50%", "0", "50%", "melhor"])
            
            plt.title("Ganho " + str(Stat) + " (%)\n" + str(Tables[0][4:9]) + " X " + str(Tables[1][4:9]), fontsize=14)
            
            fig = ax.get_figure()
 
        elif Tstat == "fc":
            ax = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                               vmin=-1, vmax=1, center=0, linewidths=0.25,
                               cbar_kws={"shrink": 1.0, 
                                         "ticks": np.arange(-1,2,1),
                                         "pad": 0.01,
                                         "orientation": "vertical"})
 
            cbar = ax.collections[0].colorbar
            cbar.set_ticks([-1, -0.5, 0, 0.5, 1])
            cbar.set_ticklabels(["pior", "-0.5", "0", "0.5", "melhor"])
 
            plt.title("Mudança Fracional " + str(Stat) + "\n" + str(Tables[0][4:9]) + " X " + str(Tables[1][4:9]), fontsize=14)
    
            fig = ax.get_figure()

        plt.xlabel("Previsões")
    
        plt.figure()
        plt.tight_layout()

        fig.savefig(outDir + "/" + "scorecard_" + str(Tstat) + "_" + str(Stat) + "_" + str(Tables[0][4:9]) + "_" + str(Tables[1][4:9]) + "_" + str(Tables[0][10:20]) + "-" + str(Tables[0][20:30]) + ".png", bbox_inches="tight", dpi=70)
        
        plt.close()
        plt.show()
        
    return