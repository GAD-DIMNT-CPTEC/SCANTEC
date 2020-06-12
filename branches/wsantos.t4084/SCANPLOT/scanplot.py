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
    plot_lines_d   : plota as tabelas do SCANTEC a partir de um dicionário de dataframes.
    plot_lines     : plota as tabelas do SCANTEC a partir dos dataframes.
    
TODO
----
    Inserir função para produzir o Scorecard.

"""

import re
import os.path
import ntpath
import numpy as np
import pandas as pd
from datetime import date, datetime
import matplotlib.pyplot as plt

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

def get_dataframe(dataInicial,dataFinal,Stats,outDir):

    """
    get_dataframe
    ==========
    
    Esta função transforma a(s) tabela(s) do SCANTEC em dataframe(s).
    
    Parâmetros de entrada
    ---------------------
        dataInicial : objeto datetime com a data inicial do experimento
        dataFinal   : objeto datetime com a data final do experimento
        Stats       : lista com os nomes das estatísticas a serem processadas
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
        outDir = data_conf["Output directory"]
        
        dTable = get_dataframe(dataInicial,dataFinal,Stats,outDir)
    """
    
    ds_table = {}
    
    for stat in Stats:
               
        dataInicial_fmt = dataInicial.strftime("%Y%m%d%H")
        dataFinal_fmt = dataFinal.strftime("%Y%m%d%H")
                
        table = outDir + '/' + str(stat) + 'EXP01_' + str(dataInicial_fmt) + str(dataFinal_fmt) + 'T.scan'
                 
        lista_n = []

        if os.path.exists(table):
            df_n = pd.read_csv(table, sep="\s+")

            ds_table[ntpath.basename(str(table))] = df_n    
                    
    return ds_table

def plot_lines(dTable,Vars,Stats,outDir):

    """
    plot_lines
    ============
    
    Esta função plota um gráfico de linha a partir de um dicionário de tabelas do SCANTEC.
    
    Parâmetros de entrada
    ---------------------
        dTable : objeto dicionário com uma ou mais tabelas do SCANTEC
        Vars   : lista com os nomes e níveis das variáveis
        Stats  : lista com os nomes das estatísticas a serem processadas
        outDir : string com o diretório com as tabelas do SCANTEC
    
    Resultado
    ---------
        Figuras salvas no diretório definido na variável outDir (SCANTEC/dataout).
    
    Uso
    ---
        from scanplot import plot_lines
        
        plot_lines(dTable,Vars,Stats,outDir)
    """
    
    for table in list(dTable.keys()):
        Stat = table[0:4]
        fcts = fcts = dTable[table].loc[:,"%Previsao"].values
        for i in range(len(Vars)):
            vname = Vars[i]
            ax = dTable[table].loc[:,[Vars[i][0].lower()]].plot(title=Vars[i][1], 
                                                           figsize=(8,5),
                                                           fontsize=12,
                                                           linewidth=3)

            ax.set_xticks(dTable[table].index)
            ax.set_xticklabels(fcts)
            
            plt.ylabel(Stat)
            plt.xlabel('Tempo')
            
            plt.grid()
            
            plt.savefig(outDir + '/' + table + '-' + Vars[i][0] + '.png', dpi=70) 
            
    return