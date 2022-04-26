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

import numpy as np
import pandas as pd

import skill_metrics as sm

from scipy.stats import t
from scipy.stats import ttest_ind

def concat_tables_and_loc(dTable,dataInicial,dataFinal,Exps,Var,series):

    """
    concat_tables_and_loc
    =====================
    
    Esta função concatena um dicionário de tabelas do SCANTEC em um único dataframe e 
    retorna uma lista com as séries das variáveis e experimentos escolhidos.
    
    Parâmetros de entrada
    ---------------------
        dTable      : objeto dicionário com uma ou mais tabelas do SCANTEC;
        dataInicial : objeto datetime com a data inicial do experimento;
        dataFinal   : objeto datetime com a data final do experimento;
        Exps        : lista com os nomes das estatísticas a serem processadas;
        Var         : nome da variável na tabela de correlação de anomalia do SCANTEC;
        series      : valor Booleano para combinar as curvas dos experimentos em um só gráfico.
    
    Resultado
    ---------
        Lista com as séries das variáveis e experimentos escolhidos.
    
    Uso
    ---
        import scanplot
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Vars = list(map(data_vars.get,[*data_vars.keys()]))
        Var = Vars[0][0].lower()
        Stats = ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        outDir = data_conf["Output directory"]
        
        dTable = scanplot.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
        
        varlev_exps = scanplot.concat_tables_and_loc(dTable,dataInicial,dataFinal,Exps,Var,series=False)
    """
    
    datai_fmt = dataInicial.strftime("%Y%m%d%H")
    dataf_fmt = dataFinal.strftime("%Y%m%d%H")

    cTable = pd.concat(dTable, axis=0, join='outer', ignore_index=False, keys=None, sort=True)    
    
    varlev_exps = []
    
    if series:
    
        for exp in Exps:
        
            fname_exp_datai = 'ACOR' + str(exp) + '_' + datai_fmt + datai_fmt + 'T.scan'
            fname_exp_dataf = 'ACOR' + str(exp) + '_' + dataf_fmt + dataf_fmt + 'T.scan'
            
            varlev_dia_exp = cTable.sort_index(0).loc[fname_exp_datai:fname_exp_dataf, str(Var)]
            
            varlev_exps.append(varlev_dia_exp)
        
    else: 
    
        for exp in Exps:
        
            fname_exp = 'ACOR' + str(exp) + '_' + datai_fmt + dataf_fmt + 'T.scan'
            
            varlev_exp = cTable.loc[fname_exp, str(Var)]
        
            varlev_exps.append(varlev_exp)
        
    return varlev_exps

def df_fill_nan(varlev_exps,varlev_dia_exps):
    
    """
    df_fill_nan
    ===========
    
    Esta função completa os dataframes até um tamanho específico.
    
    Parâmetros de entrada
    ---------------------
        varlev_exps     : lista de dataframes com as variáveis avaliadas para um período;
        varlev_dia_exps : lista de dataframes com as variáveis avaliadas para todos os dias de um período.
    
    Resultado
    ---------
        Lista de dataframes completados com NaN até o tamanho do maior dataframe.
    
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
    """
    
    lst_shapes = []
    
    for varlev_exp, varlev_dia_exp in zip(varlev_exps, varlev_dia_exps):
        
        shape_exp = int(varlev_dia_exp.shape[0] / varlev_exp.shape[0]), int(varlev_exp.shape[0])
        lst_shapes.append(shape_exp)
    
    shape_exp_max = sorted(lst_shapes, key=lambda x: x[1], reverse=True)[0]
    shape_exp_min = sorted(lst_shapes, key=lambda x: x[1], reverse=False)[0]
    
    varlev_dia_exps_rsp = []
    
    for varlev_dia_exp, shape_exp in zip(varlev_dia_exps, lst_shapes):
    
        varlev_dia_exp_rsp = pd.DataFrame(varlev_dia_exp.values.reshape(shape_exp)) 
    
        if shape_exp < shape_exp_max:  
            df_nan = pd.DataFrame(np.nan, index=np.arange(shape_exp_max[0]), columns=np.arange(shape_exp[1],shape_exp_max[1]))
            varlev_dia_exp_rsp = varlev_dia_exp_rsp.join(df_nan)
    
        varlev_dia_exps_rsp.append(varlev_dia_exp_rsp)
    
    return varlev_dia_exps_rsp
   
def calc_tStudent(lst_varlev_dia_exps_rsp):
    
    """
    calc_tStudent
    ===========
    
    Esta função calcula o teste de significância t-Student com intervalo de confiânça de 95%.
    
    Parâmetros de entrada
    ---------------------
        lst_varlev_dia_exps_rsp : lista de dataframes com as variáveis avaliadas para um período.
    
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
    """
    
    lst_drom_exp = []
    lst_drosup_exp = []
    lst_droinf_exp = []
    
    for varlev_dia_exp_rsp in lst_varlev_dia_exps_rsp[1:]:
    
        dzm_exp = ((0.5 * np.log((1.0 + 0.5 * (lst_varlev_dia_exps_rsp[0] - varlev_dia_exp_rsp)) / 
                                 (1.0 - 0.5 * (lst_varlev_dia_exps_rsp[0] - varlev_dia_exp_rsp)))))
        
        med_exp = dzm_exp.mean()   
        var_exp = dzm_exp.var()
    
        stat, pval = ttest_ind(lst_varlev_dia_exps_rsp[0], varlev_dia_exp_rsp, equal_var=False)
    
        dof_exp = lst_varlev_dia_exps_rsp[0].shape[0] + varlev_dia_exp_rsp.shape[0] - 2.0    
    
        texp = t.ppf(pval, dof_exp)
    
        dzc_exp = texp * (np.sqrt(var_exp / dof_exp))
        
        drom_exp   = 2.0 * (np.exp(2.0 * med_exp) - 1.0) / (np.exp(2.0 * med_exp) + 1.0)
        lst_drom_exp.append(drom_exp)
        
        drosup_exp = 2.0 * (np.exp(2.0 * dzc_exp) - 1.0) / (np.exp(2.0 * dzc_exp) + 1.0)
        lst_drosup_exp.append(drosup_exp)
        
        droinf_exp = 2.0 * (np.exp(-2.0 * dzc_exp )- 1.0) / (np.exp(-2.0 * dzc_exp) + 1.0)
        lst_droinf_exp.append(droinf_exp)
    
    return lst_drom_exp, lst_drosup_exp, lst_droinf_exp
