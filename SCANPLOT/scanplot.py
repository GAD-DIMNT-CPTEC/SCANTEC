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
from matplotlib import rcParams
import seaborn as sns
import skill_metrics as sm

import ipywidgets as widgets
from ipywidgets import interact, GridspecLayout, HBox, VBox, Layout
from IPython.display import display, clear_output, Image

plt.rcParams.update({'figure.max_open_warning': 0})

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
        
        fig, ax = plt.subplots()

        for var in range(len(Vars)):
       
            for Stat in Stats:
                Tables = list(filter(lambda x:Stat in x, [*dTable.keys()]))
       
                dfTables = []
    
                for table in Tables:
                    df_exp = dTable[table].loc[:,[Vars[var][0].lower()]]
                    dfTables.append(df_exp)
        
                fcts = dTable[table].loc[:,"%Previsao"].values
            
                ax = pd.concat(dfTables,axis=1).plot(title=Vars[var][1],
                                                    figsize=(8,5),
                                                    fontsize=12,
                                                    linewidth=3)
    
                ax.set_xticks(dTable[table].index)
                ax.set_xticklabels(fcts)

                enames=[]
                for table in Tables:
                    enames.append(table[4:9])
                    
                ax.legend(enames)
                
                plt.ylabel(Stat)
                plt.xlabel('Tempo')
                plt.xticks(rotation=90)

                plt.grid()
                
                plt.savefig(outDir + '/' + table + '-' + Vars[var][0] + '-combined.png', dpi=70) 
              
            #plt.cla()
                
        plt.close(fig)
                
    else:
        
        fig, ax = plt.subplots()
            
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
       
                ename = [table][0][4:9]
    
                ax.legend([ename])
            
                plt.ylabel(Stat)
                plt.xlabel('Tempo')
                plt.xticks(rotation=90)
        
                plt.grid()
            
                plt.savefig(outDir + '/' + table + '-' + Vars[var][0] + '.png', dpi=70) 
                
            #plt.cla()
                
        plt.close(fig)
        
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
        plt.figure(figsize = (15,10))
        
        sns.set(style="whitegrid", font_scale=0.90)
        sns.set_context(rc={"xtick.major.size":  1.5,  "ytick.major.size": 1.5,
                            "xtick.major.pad":   0.05,  "ytick.major.pad": 0.05,
                            "xtick.major.width": 0.5, "ytick.major.width": 0.5,
                            "xtick.minor.size":  1.5,  "ytick.minor.size": 1.5,
                            "xtick.minor.pad":   0.05,  "ytick.minor.pad": 0.05,
                            "xtick.minor.width": 0.5, "ytick.minor.width": 0.5})
 
        if Tstat == "ganho":
            ax = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                               vmin=-100, vmax=100, center=0, linewidths=0.25, square=False,
                               cbar_kws={"shrink": 1.0, 
                                         "ticks": np.arange(-100,110,10),
                                         "pad": 0.01,
                                         "orientation": "vertical"})
 
            cbar = ax.collections[0].colorbar
            cbar.set_ticks([-100, -50, 0, 50, 100])
            cbar.set_ticklabels(["pior", "-50%", "0", "50%", "melhor"])
            cbar.ax.tick_params(labelsize=12)    
                
            plt.title("Ganho " + str(Stat) + " (%)\n" + str(Tables[0][4:9]) + " X " + str(Tables[1][4:9]), fontsize=14)
            
            fig = ax.get_figure()
 
        elif Tstat == "fc":
            ax = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                               vmin=-1, vmax=1, center=0, linewidths=0.25, square=False,
                               cbar_kws={"shrink": 1.0, 
                                         "ticks": np.arange(-1,2,1),
                                         "pad": 0.01,
                                         "orientation": "vertical"})
 
            cbar = ax.collections[0].colorbar
            cbar.set_ticks([-1, -0.5, 0, 0.5, 1])
            cbar.set_ticklabels(["pior", "-0.5", "0", "0.5", "melhor"])
            cbar.ax.tick_params(labelsize=12)    
 
            plt.title("Mudança Fracional " + str(Stat) + "\n" + str(Tables[0][4:9]) + " X " + str(Tables[1][4:9]), fontsize=14)
    
            fig = ax.get_figure()

        plt.xlabel("Previsões")
        plt.yticks(fontsize=12)
        plt.xticks(rotation=90, fontsize=12)
    
        plt.figure()
        plt.tight_layout()

        fig.savefig(outDir + "/" + "scorecard_" + str(Tstat) + "_" + str(Stat) + "_" + str(Tables[0][4:9]) + "_" + str(Tables[1][4:9]) + "_" + str(Tables[0][10:20]) + "-" + str(Tables[0][20:30]) + ".png", bbox_inches="tight", dpi=70)
        
        plt.close()
        plt.show()
        
    return

def plot_dTaylor(dTable,data_conf,Vars,Stats,outDir):
    
    """
    plot_dTaylor
    ==============
    
    Esta função plota o diagrama de Taylor a partir das tabelas de estatísticas
    do SCANTEC, para um ou mais experimentos.
    
    Esta função utiliza o módulo SkillMetrics (https://pypi.org/project/SkillMetrics/). 
    
    Parâmetros de entrada
    ---------------------
        dTable    : objeto dicionário com uma ou mais tabelas do SCANTEC
        Vars      : lista com os nomes e níveis das variáveis
        data_conf : objeto dicionário com as configurações do SCANTEC
        Stats     : lista com os nomes das estatísticas a serem processadas
                    (são necessárias as tabelas ACOR, RMSE e VIES)
        outDir    : string com o diretório com as tabelas do SCANTEC
    
    Resultado
    ---------
        Figuras salvas no diretório definido na variável outDir (SCANTEC/dataout).
    
    Uso
    ---
        from scanplot import plot_dTaylor
        
        plot_dTaylor(dTable,data_conf,Vars,Stats,outDir)
    """
    
    # Ignore Seaborn and respect rcParams
    sns.reset_orig()
    
    # Set the figure properties (optional)
    rcParams["figure.figsize"] = [8.0, 6.5]
    rcParams['lines.linewidth'] = 1 # line width for plots
    rcParams.update({'font.size': 12}) # font size of axes text
    rcParams['axes.titlepad'] = 40 # title vertical distance from plot
    
    Exps = [*data_conf['Experiments'].keys()]
    
    fig = plt.figure()
       
    for exp in range(len(Exps)): 
        
        for var in range(len(Vars)):
        
            tAcor = list(filter(lambda x:'ACOR' in x, [*dTable.keys()]))[exp]
            tRmse = list(filter(lambda x:'RMSE' in x, [*dTable.keys()]))[exp]
            tVies = list(filter(lambda x:'VIES' in x, [*dTable.keys()]))[exp]
    
            bias  = dTable[tVies].loc[:,[Vars[var][0].lower()]].to_numpy()
            ccoef = dTable[tAcor].loc[:,[Vars[var][0].lower()]].to_numpy()
            crmsd = dTable[tRmse].loc[:,[Vars[var][0].lower()]].to_numpy()
            sdev  = (dTable[tRmse].loc[:,[Vars[var][0].lower()]]**(1/2)).to_numpy() # rever

            biasT = bias.T
            ccoefT = ccoef.T
            crmsdT = crmsd.T
            sdevT = sdev.T
    
            bias = np.squeeze(biasT)
            ccoef = np.squeeze(ccoefT)
            crmsd = np.squeeze(crmsdT)
            sdev = np.squeeze(sdevT)
    
            label = [*dTable[tVies].loc[:,"%Previsao"].values]
        
            #plt.figure()
            plt.tight_layout()
    
            sm.taylor_diagram(sdev, crmsd, ccoef, markerLabel = label, 
                              locationColorBar = 'EastOutside',
                              markerDisplayed = 'colorBar', titleColorBar = 'Bias',
                              markerLabelColor='black', markerSize=10,
                              markerLegend='off', cmapzdata=bias,
                              colRMS='g', styleRMS=':',  widthRMS=2.0, titleRMS='on',
                              colSTD='b', styleSTD='-.', widthSTD=1.0, titleSTD ='on',
                              colCOR='k', styleCOR='--', widthCOR=1.0, titleCOR='on')
        
            plt.title("Diagrama de Taylor " + str(Exps[exp]) + '\n' + str(Vars[var][1]), fontsize=14)

            plt.savefig(outDir + '/dtaylor-' + str(Exps[exp]) + '-' + Vars[var][0] + '.png', bbox_inches="tight", dpi=70) 

            plt.show()
            
    plt.close(fig)     
    
    return

global datai 

def show_buttons(data_vars,data_conf):

    dataInicial = data_conf["Starting Time"]
    dataFinal = data_conf["Ending Time"]
    Vars = list(map(data_vars.get,[*data_vars.keys()]))
    Stats = ["ACOR", "RMSE", "VIES"]
    Exps = list(data_conf["Experiments"].keys())
    outDir = data_conf["Output directory"]
    
    vexps = list(data_conf["Experiments"].keys())
    
    vlist = []
    for i in [*data_vars.keys()]:
        vlist.append(data_vars[i][0])
    
    dataInicial = widgets.DatePicker(
        description='Data inicial:',
        value=data_conf["Starting Time"],
        layout={'width': 'auto'},    
        disabled=False
    )

    dataFinal = widgets.DatePicker(
        description='Data final:',
        value=data_conf["Ending Time"],
        layout={'width': 'auto'},    
        disabled=False
    )    
    
    Vars = widgets.SelectMultiple(
        options=vlist,
        value=[vlist[0]],
        layout={'width': 'auto'},    
        description='Variável(is) e Nível(is):',
        disabled=False
    )

    Stats = widgets.SelectMultiple(
        options=['ACOR', 'RMSE', 'MAE', 'VIES'],
        value=['ACOR'],
        layout={'width': 'auto'}, 
        description='Estatística(s):',
        disabled=False
    )
    
    Exps = widgets.SelectMultiple(
        options=vexps,
        value=[vexps[0]],
        layout={'width': 'auto'}, 
        description='Experimento(s):',
        disabled=False
    )
    
    CaixaTexto = widgets.HTML(
        value="Selecione as opções a seguir e clique no botar Salvar para guardar a seleção ou no botão Limpar para limpar a seleção.",
        placeholder='Some HTML',
        description='',
    )
    
    Salvar = widgets.Button(
        description='Salvar',
        disabled=False,
        button_style='success', # 'success', 'info', 'warning', 'danger' or ''
        tooltip='Clique para salvar a seleção',
        icon='' # (FontAwesome names without the `fa-` prefix) https://fontawesome.com/icons
    )

    Resetar = widgets.Button(
        description='Resetar',
    #    layout=style.layout,
        disabled=False,
        button_style='warning', # 'success', 'info', 'warning', 'danger' or ''
        tooltip='Clique para resetar a seleção',
        icon='' # (FontAwesome names without the `fa-` prefix) https://fontawesome.com/icons
    )
    
    out = widgets.Output()

    box_layout1 = Layout(display='flex',
                    flex_flow='column',
                    align_items='stretch',
                    border='0px dotted #000000',
                    padding='10px',
                    margin='5px', 
                    width='99%')

    box_layout2 = Layout(display='flex',
                    flex_flow='column',
                    align_items='stretch',
                    border='0px dotted #000000',
                    padding='10px',
                    margin='5px',
                    width='50%')
    
    def on_reset_button_clicked(b):
        Resetar.description = 'Resetar'
        Resetar.button_style='warning'
        with out:
            clear_output()
    
    def on_save_button_clicked(change):
        Salvar.description = 'Salvar'
        Salvar.button_style='success'
        datai = dataInicial.value
        with out:
            clear_output()
            display(dataInicial.value,dataFinal.value,Vars.value,Stats.value,Exps.value)

#    dataInicial.observe(on_save_button_clicked, names='value')
            
    Salvar.on_click(on_save_button_clicked)
    Resetar.on_click(on_reset_button_clicked)
    
    top_box = VBox(children=[CaixaTexto],layout=box_layout1)
    right_box = VBox(children=[dataInicial,dataFinal,Stats,HBox(children=[Salvar,Resetar])],layout=box_layout2)
    left_box = VBox(children=[Exps,Vars],layout=box_layout2)
    tab1 = VBox(children=[top_box, HBox(children=[right_box, left_box]),widgets.VBox([out])])
#    tab2 = VBox(children=[widgets.VBox([out])])

    tab = widgets.Tab(children=[tab1])#, tab2])
    tab.set_title(0, 'Opções')

    return VBox(children=[tab])