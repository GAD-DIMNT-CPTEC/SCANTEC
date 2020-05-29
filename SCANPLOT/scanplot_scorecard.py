#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""
Objetivo: Este script calcula o "Ganho Percentual" e a "Mudança Fracional*" a 
          partir das estatísticas do SCANTEC e plota os resultados na forma de 
          um scorecard.

Uso: python3 scanplot_scorecard.py

Observações: Alterar o valor das variáveis indicadas abaixo.

*Banos et al., 2018: Impacto da Assimilação de Perfis de Refratividade do 
                     Satélite Metop-B nas Previsões de Tempo do CPTEC/INPE 
                     Durante os Meses de Janeiro e Agosto de 2014

Versões dos pacotes:
"""

import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

from matplotlib import pylab
pylab.rcParams['figure.figsize'] = (10,10)
plt.switch_backend('agg')

def plot_scorecard( datai,
                    dataf,
                    list_var,
                    Stats,
                    Tstats,
                    tipo,
                    Regs,
                    hsin,
                    bexp1,
                    bexp2,
                    base_path
                ):

    for tstat in Tstats:

        for stat in Stats:
        
            for reg in Regs:
            
                nexp1 = str(bexp1) + "." + str(reg)
                nexp2 = str(bexp2) + "." + str(reg)

                exp1 = str(base_path) + "/" + str(tipo) + "/" + str(hsin) + "/" + str(nexp1)
                exp2 = str(base_path) + "/" + str(tipo) + "/" + str(hsin) + "/" + str(nexp2)

                table1 = pd.read_csv(exp1 + "/" + str(stat) + "EXP01_" + str(datai) + str(dataf) + "T.scam", sep="\s+")
                table2 = pd.read_csv(exp2 + "/" + str(stat) + "EXP01_" + str(datai) + str(dataf) + "T.scam", sep="\s+")


                p_table1 = pd.pivot_table(table1, index="%Previsao", 
                                          values=list_var)

                p_table2 = pd.pivot_table(table2, index="%Previsao",
                                          values=list_var)

                if tstat == "ganho":
                    # Porcentagem de ganho
                    if stat == "ACOR":
                        score_table = ((p_table2[1:].T - p_table1[1:].T) / (1.0 - p_table1[1:].T)) * 100
                    elif stat == "RMSE" or stat == "VIES":
                        score_table = ((p_table2[1:].T - p_table1[1:].T) / (0.0 - p_table1[1:].T)) * 100
                elif tstat == "fc":
                    # Mudança fracional
                    score_table = (1.0 - (p_table2[1:].T / p_table1[1:].T))

                # Figura
                
                fig = plt.subplots(figsize=(5,4))

                sns.set(style="whitegrid", font_scale=0.45)
                sns.set_context(rc={"xtick.major.size": 1.5,  "ytick.major.size": 1.5,
                                    "xtick.major.pad": 0.05,  "ytick.major.pad": 0.05,
                                    "xtick.major.width": 0.5, "ytick.major.width": 0.5,
                                    "xtick.minor.size": 1.5,  "ytick.minor.size": 1.5,
                                    "xtick.minor.pad": 0.05,  "ytick.minor.pad": 0.05,
                                    "xtick.minor.width": 0.5, "ytick.minor.width": 0.5})

                if tstat == "ganho":
                    plt.title("Ganho " + str(stat) + " " + str(reg.upper()) + " " + str(hsin) + " (%)\n" + str(bexp1) + " X " + str(bexp2), fontsize=10)
                    plot = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                                       vmin=-100, vmax=100, center=0, linewidths=0.25,
                                       cbar_kws={"shrink": 1.0, 
                                                 "ticks": np.arange(-100,110,10),
                                                 "pad": 0.01,
                                                 "orientation": "vertical"})

                    cbar = plot.collections[0].colorbar
                    cbar.set_ticks([-100, -50, 0, 50, 100])
                    cbar.set_ticklabels(["pior", "-50%", "0", "50%", "melhor"])

                elif tstat == "fc":
                    plt.title("Mudança Fracional " + str(stat) + " " + str(reg.upper()) + " " + str(hsin) + "\n" + str(bexp1) + " X " + str(bexp2), fontsize=10)
                    plot = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                                       vmin=-1, vmax=1, center=0, linewidths=0.25,
                                       cbar_kws={"shrink": 1.0, 
                                                 "ticks": np.arange(-1,2,1),
                                                 "pad": 0.01,
                                                 "orientation": "vertical"})

                    cbar = plot.collections[0].colorbar
                    cbar.set_ticks([-1, -0.5, 0, 0.5, 1])
                    cbar.set_ticklabels(["pior", "-0.5", "0", "0.5", "melhor"])

                plt.xlabel("Previsões")
                plt.figure()
                plt.tight_layout()
                plt.savefig("scorecard_" + str(tstat) + "_exps" + "_" + str(stat.lower()) + "_" + str(reg) + "_" + str(datai) + "-" + str(dataf) + "_" + str(hsin) + ".png", bbox_inches="tight", dpi=200)
                #plt.close()
                plt.show()
    return

#Stats = ["VIES", "RMSE", "ACOR"]
#Regs = ["as", "hn", "hs", "tr"]
##Stats = ["VIES"]
##Regs = ["hn"]
#
#Tstats = ["ganho", "fc"]
##Tstats = ["ganho"]
#
#bexp1 = "SMG_V2.0.0"
#bexp2 = "SMG_V2.1.0"
#
#datai = "2015050200"
#dataf = "2015053100"
#tipo = "mensal"
#hsin = "00Z"
#
##base_path = "/home/carlos/Documents/INPE2019/GDAD/SMG/teste_scamtec_v100/repo_carlos/wsantos.t4084/SCANPLOT/dadosscantec/aval_SMG/"
##base_path = "/SCANPLOT/aval_SMG/"
#base_path = "./aval_SMG/"
#
#
#list_var=["VTMP-925","VTMP-850", "VTMP-500","TEMP-850","TEMP-500", "TEMP-250","PSNM-000","UMES-925", "UMES-850","UMES-500",   
#"AGPL-925","ZGEO-850","ZGEO-500","ZGEO-250","UVEL-850","UVEL-500","UVEL-250","VVEL-850","VVEL-500","VVEL-250",   
#"PREC-000","PREV-000"]
#
#plot_scorecard(datai,dataf,list_var,Stats,Tstats,tipo,Regs,hsin,bexp1,bexp2,base_path)
