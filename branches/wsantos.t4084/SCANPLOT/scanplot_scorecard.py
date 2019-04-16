#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""
Objetivo: Este script calcula o "Fractional Change*" a partir das estatísticas 
          do SCANTEC e plota os resultados na forma de um scorecard.

Uso: python3 scanplot_scorecard.py

Observações: Alterar o valor das variáveis indicadas abaixo.

*Banos et al., 2018: Impacto da Assimilação de Perfis de Refratividade do Satélite Metop-B nas
Previsões de Tempo do CPTEC/INPE Durante os Meses de Janeiro e Agosto de 2014

Versões dos pacotes:
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

Stats = ["VIES", "RMSE", "ACOR"]
Regs = ["hn", "hs", "tr"]

bexp1 = "SMG_V2.0.0"
bexp2 = "SMG_V2.1.0"

datai = "2015050200"
dataf = "2015053100"
tipo = "mensal"
hsin = "00Z"

base_path = "/home/carlos/Documents/INPE2019/GDAD/SMG/teste_scamtec_v100/repo_carlos/wsantos.t4084/SCANPLOT/dadosscantec/aval_SMG/"

for stat in Stats:

    for reg in Regs:
    
        nexp1 = str(bexp1) + "." + str(reg)
        nexp2 = str(bexp2) + "." + str(reg)
        
        exp1 = str(base_path) + "/" + str(tipo) + "/" + str(hsin) + "/" + str(nexp1)
        exp2 = str(base_path) + "/" + str(tipo) + "/" + str(hsin) + "/" + str(nexp2)

        table1 = pd.read_csv(exp1 + "/" + str(stat) + "EXP01_" + str(datai) + str(dataf) + "T.scam", sep="\s+")
        table2 = pd.read_csv(exp2 + "/" + str(stat) + "EXP01_" + str(datai) + str(dataf) + "T.scam", sep="\s+")

        p_table1 = pd.pivot_table(table1, index="%Previsao", 
                                  values=["VTMP-925",  
                                          "VTMP-850", 
                                          "VTMP-500",
                                          "TEMP-850",
                                          "TEMP-500",   
                                          "TEMP-250",   
                                          "PSNM-000",   
                                          "UMES-925",   
                                          "UMES-850",   
                                          "UMES-500",   
                                          "AGPL-925",   
                                          "ZGEO-850",   
                                          "ZGEO-500",   
                                          "ZGEO-250",   
                                          "UVEL-850",   
                                          "UVEL-500",   
                                          "UVEL-250",   
                                          "VVEL-850",   
                                          "VVEL-500",   
                                          "VVEL-250",   
                                          "PREC-000",   
                                          "PREV-000"])
    
        p_table2 = pd.pivot_table(table2, index="%Previsao",
                                  values=["VTMP-925",  
                                          "VTMP-850", 
                                          "VTMP-500",
                                          "TEMP-850",
                                          "TEMP-500",   
                                          "TEMP-250",   
                                          "PSNM-000",   
                                          "UMES-925",   
                                          "UMES-850",   
                                          "UMES-500",   
                                          "AGPL-925",   
                                          "ZGEO-850",   
                                          "ZGEO-500",   
                                          "ZGEO-250",   
                                          "UVEL-850",   
                                          "UVEL-500",   
                                          "UVEL-250",   
                                          "VVEL-850",   
                                          "VVEL-500",   
                                          "VVEL-250",   
                                          "PREC-000",   
                                          "PREV-000"])
    
        # Cálculo do Fractional Change
        if stat == "ACOR":
            score_table = ((p_table1[1:-1].T - p_table2[1:-1].T) / (1. - p_table2[1:-1].T)) * 100
        elif stat == "RMSE" or stat == "VIES":
            score_table = ((p_table1[1:-1].T - p_table2[1:-1].T) / (0. - p_table2[1:-1].T)) * 100
            
        # Figura
        sns.set(style="darkgrid", font_scale=0.5)
            
        fig, ax = plt.subplots()    
            
        plt.plot(figsize=(40,40))
        plt.title("Fractional Change " + str(stat) + " " + str(reg.upper()) + " (%)\nSMG Experimentos V2.0.0 e V2.1.0", fontsize=10)
        plot = sns.heatmap(score_table, annot=True, fmt="1.0f", cmap="RdYlGn", 
                           cbar_kws={"label": stat}, vmin=-100, vmax=100, center=0,
                           linewidths=.25)
        plt.tight_layout()
        plt.savefig("scorecard_exps" + "_" + str(stat.lower()) + "_" + str(reg) + "_" + str(datai) + "-" + str(dataf) + ".png", bbox_inches="tight", dpi=200)