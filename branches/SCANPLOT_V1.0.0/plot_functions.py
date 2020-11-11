#! /usr/bin/env python3

# SCANPLOT - Um sistema de plotagem simples para o SCANTEC
# Copyright (C) 2020 INPE
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import global_variables as gvars

import numpy as np
import pandas as pd

import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import rcParams

import seaborn as sns

import skill_metrics as sm

from scipy.stats import t
from scipy.stats import ttest_ind

def plot_lines(dTable,Vars,Stats,outDir,**kwargs):

    """
    plot_lines
    ==========
    
    Esta função plota gráficos de linha a partir de um dicionário de dataframes com as tabelas do SCANTEC.
    
    Parâmetros de entrada
    ---------------------
        dTable  : objeto dicionário com uma ou mais tabelas do SCANTEC
        Vars    : lista com os nomes e níveis das variáveis
        Stats   : lista com os nomes das estatísticas a serem processadas
        outDir  : string com o diretório com as tabelas do SCANTEC

    Parâmetros de entrada opcionais
    -------------------------------
        combine : valor Booleano para combinar as curvas dos experimentos em um só gráfico
                  combine=False (valor padrão), plota as curvas em gráficos separados
                  combine=True, plota as curvas das mesmas estatísticas no mesmo gráfico
    
    Resultado
    ---------
        Figuras salvas no diretório definido na variável outDir (SCANTEC/dataout).
    
    Uso
    ---
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Vars = list(map(data_vars.get,[*data_vars.keys()]))
        Stats = ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        outDir = data_conf["Output directory"]
        
        dTable = scanplot.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
        
        scanplot.plot_lines(dTable,Vars,Stats,outDir)
    """
  
    # Verifica se foram passados os argumentos opcionais e atribui os valores

    global tExt

    if 'combine' in kwargs:
        combine = kwargs['combine']
    else:
        combine = gvars.combine

    if 'tExt' in kwargs:
        tExt = kwargs['tExt']      
        # Atualiza o valor global de tExt
        gvars.tExt = tExt
    else:
        tExt = gvars.tExt

    # Ignore Seaborn and respect rcParams
    sns.reset_orig()
    
    if combine:
        
        fig, ax = plt.subplots()
        plt.rcParams.update({'figure.max_open_warning': 0})

        for var in range(len(Vars)):
       
            for Stat in Stats:
                Tables = list(filter(lambda x:Stat in x, [*dTable.keys()]))
       
                dfTables = []
    
                for table in Tables:
                    if tExt == 'scan':
                        df_exp = dTable[table].loc[:,[Vars[var][0].lower()]]
                    else:
                        df_exp = dTable[table].loc[:,[Vars[var][0]]]
                    dfTables.append(df_exp)
        
                fcts = dTable[table].loc[:,"%Previsao"].values
            
                ax = pd.concat(dfTables,axis=1).plot(title=Vars[var][1],
                                                    figsize=(8,5),
                                                    fontsize=12,
                                                    linewidth=1.5,
                                                    marker='o')
    
                ax.set_xticks(dTable[table].index)
                ax.set_xticklabels(fcts)

                enames=[]
                for table in Tables:
                    enames.append(table.split('_')[0][4:])
                    
                ax.legend(enames)
                
                plt.ylabel(Stat)
                plt.xlabel('Horas de Integração')
                plt.xticks(rotation=90)

                if Stat == 'ACOR':
                    plt.axhline(y=0.5, color='black', linestyle='-', linewidth=1)
                else:
                    plt.axhline(y=0.0, color='black', linestyle='-', linewidth=1)
  
                plt.grid(color='grey', linestyle='--', linewidth=0.5)
                
                plt.savefig(outDir + '/' + table + '-' + Vars[var][0] + '-combined.png', dpi=70) 
              
        plt.close(fig)
                
    else:
            
        for table in list(dTable.keys()):
            Stat = table[0:4]
            fcts = dTable[table].loc[:,"%Previsao"].values

            fig, ax = plt.subplots()
            plt.rcParams.update({'figure.max_open_warning': 0})

            for var in range(len(Vars)):
                vname = Vars[var]

                if tExt == 'scan':            
                    ax = dTable[table].loc[:,[Vars[var][0].lower()]].plot(title=Vars[var][1], 
                                                                          figsize=(8,5),
                                                                          fontsize=12,
                                                                          linewidth=1.5,
                                                                          marker='o')
                else:
                    ax = dTable[table].loc[:,[Vars[var][0]]].plot(title=Vars[var][1], 
                                                                          figsize=(8,5),
                                                                          fontsize=12,
                                                                          linewidth=1.5,
                                                                          marker='o')
 
                ax.set_xticks(dTable[table].index)
                ax.set_xticklabels(fcts)
       
                ename = table.split('_')[0][4:]

                ax.legend([ename])
            
                plt.ylabel(Stat)
                plt.xlabel('Horas de Integração')
                plt.xticks(rotation=90)
      
                if Stat == 'ACOR':
                    plt.axhline(y=0.5, color='black', linestyle='-', linewidth=1)
                else:
                    plt.axhline(y=0.0, color='black', linestyle='-', linewidth=1)
  
                plt.grid(color='grey', linestyle='--', linewidth=0.5)
            
                plt.savefig(outDir + '/' + table + '-' + Vars[var][0] + '.png', dpi=70) 
                
            plt.close(fig)
        
    return

def plot_lines_tStudent(Exps,ldrom_exp,ldrosup_exp,ldroinf_exp,varlev_exps):
        
    """
    plot_lines_tStudent
    ===================
    
    Esta função plota gráficos de linha acompanhados dos resultados do teste de significância t-Student.
    Os gráficos são plotados apenas com base nas tabelas de correlação de anomalia do SCANTEC.
    
    Parâmetros de entrada
    ---------------------
        Exps         : lista com os nomes dos experimentos
        ldrom_exp    : curva do teste referente ao experimento
        ldrosup_exp  : limite superior do teste
        ldroinf_exp  : limite inferior do teste
        varlev_exps  : dataframes com as variáveis dos experimentos
    
    Resultado
    ---------
        Resultado do teste de significância e valores críticos para serem utilizados pela função
        plot_lines_tStudent.
    
    Uso
    ---
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Vars = list(map(data_vars.get,[*data_vars.keys()]))
        Stats = ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        outDir = data_conf["Output directory"]
        
        dTable = scanplot.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
        
        varlev_exps = scanplot.concat_tables_and_loc(dTable,dataInicial,dataFinal,Exps,series=False)
        
        lst_varlev_dia_exps_rsp = scanplot.df_fill_nan(varlev_exps,varlev_dia_exps)
        
        ldrom_exp, ldrosup_exp, ldroinf_exp = scanplot.calc_tStudent(lst_varlev_dia_exps_rsp)
        
        scanplot.plot_lines_tStudent(Exps,ldrom_exp,ldrosup_exp,ldroinf_exp,varlev_exps)

    Observações
    -----------
        Experimental, esta função necessita ser validada.
    """        
        
    # Ignore Seaborn and respect rcParams
    sns.reset_orig()    
    
    colors = ['black', 'red', 'green', 'blue', 'orange', 'brown', 'cyan', 'magenta']
    
    fig, axs = plt.subplots(2, sharex=True, sharey=False, gridspec_kw={'hspace': 0}, figsize = (8,6))
    
    j = 0
    
    for i, varlev_exp in enumerate(varlev_exps):
        if i == 0:
            axs[0].plot(varlev_exp, color=colors[j], linestyle='--', label=str(Exps[j]), linewidth=1.5)
        else:
            axs[0].plot(varlev_exp, color=colors[j], label=str(Exps[j]), linewidth=1.5)
          
        axs[0].grid(color='grey', linestyle='--', linewidth=0.5)
        axs[0].legend()
        
        j += 1
        
    j = 1
        
    for drosup_exp, droinf_exp, drom_exp in zip(ldrosup_exp,ldroinf_exp,ldrom_exp):
        
        axs[1].bar(range(0, len(drosup_exp)), drosup_exp, color=(0, 0, 0, 0), edgecolor=colors[j], align='center', linewidth=1.5)
        axs[1].bar(range(0, len(droinf_exp)), droinf_exp, color=(0, 0, 0, 0), edgecolor=colors[j], align='center', linewidth=1.5)
        
        axs[1].plot(drom_exp, color=colors[j])

        j += 1
        
    axs[1].axhline(color='black', linewidth=0.5)
    axs[1].grid(color='grey', linestyle='--', linewidth=0.5)
        
    for ax in axs:
        ax.label_outer()        
        
    return

def plot_scorecard(dTable,Vars,Stats,Tstat,Exps,outDir):
    
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
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Vars = list(map(data_vars.get,[*data_vars.keys()]))
        Stats = ["ACOR", "RMSE", "VIES"]

        Exps = ["EXP1", "EXP2"]

        outDir = data_conf["Output directory"]
        
        dTable = scanplot.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
        
        scanplot.plot_scorecard(dTable,Vars,Stats,Tstat,Exps,outDir)

    Observações
    -----------
        Nos scorecards, as cores sempre indicam os ganhos do segundo experimento com relação ao primeiro.
        Portanto, os tons mais intensos de verde, indicam que o 'EXP2' apresentam maior ganho em relação 
        ao 'EXP1' ou que a mudança fracional é maior.
    """
    if tExt == 'scan': 
        list_var = [ltuple[0].lower() for ltuple in Vars]
    else:
        list_var = [ltuple[0] for ltuple in Vars]

    for Stat in Stats:
        Tables = list(filter(lambda x:Stat in x, [*dTable.keys()]))
    
        exp1 = [s for s in Tables if Exps[0] in s][0]
        exp2 = [s for s in Tables if Exps[1] in s][0]
    
        p_table1 = pd.pivot_table(dTable[exp1], index="%Previsao", values=list_var)
        p_table2 = pd.pivot_table(dTable[exp2], index="%Previsao", values=list_var)
 
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
                
            plt.title("Ganho " + str(Stat) + " (%)\n" + Exps[0] + " X " + Exps[1], fontsize=14)
            
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
 
            plt.title("Mudança Fracional " + str(Stat) + "\n" + Exps[0] + " X " + Exps[1], fontsize=14)
    
            fig = ax.get_figure()

        plt.xlabel("Horas de Integração")
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
    ============
    
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
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Vars = list(map(data_vars.get,[*data_vars.keys()]))
        Stats = ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        outDir = data_conf["Output directory"]
        
        dTable = scanplot.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
        
        scanplot.plot_dTaylor(dTable,data_conf,Vars,Stats,outDir)
        
    Observações
    -----------
        Experimental, esta função considera o devio-padrão como a raiz qadrada do RMSE.
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
