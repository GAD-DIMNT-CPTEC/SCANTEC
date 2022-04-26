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

import re
import os
import ntpath

import numpy as np
import pandas as pd

from datetime import date, datetime, timedelta

import xarray as xr
import cartopy.crs as ccrs

def get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir,**kwargs):

    """
    get_dataframe
    =============
    
    Esta função transforma a(s) tabela(s) do SCANTEC em dataframe(s).
    
    Parâmetros de entrada
    ---------------------
        dataInicial : objeto datetime com a data inicial do experimento;
        dataFinal   : objeto datetime com a data final do experimento;
        Stats       : lista com os nomes das estatísticas a serem processadas;
        Exps        : lista com os nomes dos experimentos;
        outDir      : string com o diretório com as tabelas do SCANTEC.

    Parâmetros de entrada opcionais
    -------------------------------
        series : valor Booleano para ler uma série temporal das tabelas do SCANTEC:
                 * series=False (valor padrão), lê as tabelas do SCANTEC geradas para a avaliação de um período;
                 * series=True, lê as tabelas do SCANTEC geradas para a avaliação dos dias dentro de um período;
        tExt   : string com o extensão dos nomes das tabelas do SCANTEC:
                 * tExt='scan' (valor padrão), considera as tabelas do SCANTEC;
                 * tExt='scam', considera os nomes das tabelas das versões antigas do SCANTEC.
    
    Resultado
    ---------
        Dicionário com o(s) dataframe(s) com a(s) tabela(s) do SCANTEC.
    
    Uso
    ---
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        dataInicial = data_conf["Starting Time"]
        dataFinal = data_conf["Ending Time"]
        Stats =  ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        outDir = data_conf["Output directory"]
        
        dTable = scanplot.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir)
    """

    # Verifica se foram passados os argumentos opcionais e atribui os valores

    global tExt

    if 'series' in kwargs:
        series = kwargs['series']
    else:
        series = gvars.series

    if 'tExt' in kwargs:
        tExt = kwargs['tExt']
        # Atualiza o valor global de tExt
        gvars.tExt = tExt
    else:
        tExt = gvars.tExt

    # Dicionário com o(s) dataframe(s)
    ds_table = {}       
    
    if series:
    
        while (dataInicial <= dataFinal):
            
            dataInicial_fmt = dataInicial.strftime("%Y%m%d%H")
            dataFinal_fmt = dataFinal.strftime("%Y%m%d%H")
            
            for stat in Stats:
    
                for exp in Exps:
            
                    table_name = stat + exp + '_' + dataInicial_fmt + dataInicial_fmt + 'T.' + tExt
                    table = os.path.join(outDir, table_name) 

                    lista_n = []
    
                    if os.path.exists(table):
                        df_n = pd.read_csv(table, sep="\s+")
    
                        ds_table[ntpath.basename(str(table))] = df_n    
                        
            dataInicial = dataInicial + timedelta(hours=24) # pegar esta informação do namelist (timedelta)   

    else:
        
        for stat in Stats:
                   
            dataInicial_fmt = dataInicial.strftime("%Y%m%d%H")
            dataFinal_fmt = dataFinal.strftime("%Y%m%d%H")
    
            for exp in Exps:
            
                table_name = stat + exp + '_' + dataInicial_fmt + dataFinal_fmt + 'T.' + tExt 
                table = os.path.join(outDir, table_name) 

                lista_n = []
    
                if os.path.exists(table):
                    df_n = pd.read_csv(table, sep="\s+")
    
                    ds_table[ntpath.basename(str(table))] = df_n    
        
    return ds_table

def get_dataset(data_conf,data_vars,Stats,Exps):
       
    """
    get_dataset
    ===========
    
    Esta função transforma o(s) campo(s) com a distribuição espacial da(s) 
    estatística(s) do SCANTEC em dataset(s).
    
    Parâmetros de entrada
    ---------------------
        data_conf : dicionário com as configurações do SCANTEC;
        data_vars : dicionário com as variáveis avaliadas pelo SCANTEC;
        Stats     : lista com os nomes das estatísticas a serem processadas;
        Exps      : lista com os nomes dos experimentos.
    
    Resultado
    ---------
        Dicionário com o(s) dataset(s) com a(s) distribuição(ões) espacial(is)
        da(s) estatística(s) do SCANTEC.
    
    Uso
    ---
        import scanplot 
        
        data_vars, data_conf = scanplot.read_namelists("~/SCANTEC")
        
        Stats =  ["ACOR", "RMSE", "VIES"]
        Exps = list(data_conf["Experiments"].keys())
        
        dSet = scanplot.get_dataset(data_conf,data_vars,Stats,Exps)
    """
    
    # Datas
    dataInicial = data_conf["Starting Time"]
    dataFinal = data_conf["Ending Time"]
    t_step = str(data_conf["Forecast Time Step"]) + "H"
    dataInicial_fmt = dataInicial.strftime("%Y%m%d%H")
    dataFinal_fmt = dataFinal.strftime("%Y%m%d%H")

    ftime = np.int(data_conf['Forecast Total Time'])
    atime = np.int(data_conf['Analisys Time Step'])
    tdef = np.int((ftime / atime) + 1) # verificar, pois no arquivo CTL esta é a conta que é feita, mas no arquivo binário não!
    dataFinal2 = dataInicial + timedelta(hours=np.int(tdef)*np.int(data_conf["Forecast Time Step"]))
    #times = pd.date_range(dataInicial, dataFinal2, freq=t_step)  
    times = pd.date_range(dataInicial, dataFinal, freq=t_step)  
    tdef = len([*times])                     
    #tdef = 8
    print('Starting Time',dataInicial)
    print('Ending Time',dataFinal)
    print(times)
    print(tdef)
    print(np.arange(tdef))
    
    # Tamanho e limites do domínio                           
    lllat = np.float32(data_conf['run domain lower left lat'])
    lllon = np.float32(data_conf['run domain lower left lon'])
    urlat = np.float32(data_conf['run domain upper right lat'])
    urlon = np.float32(data_conf['run domain upper right lon'])
 
    gdx = np.float32(data_conf['run domain resolution dx'])
    gdy = np.float32(data_conf['run domain resolution dy'])
                               
    xdef = np.int(((urlon - lllon) / gdx) + 1)
    ydef = np.int(((urlat - lllat) / gdy) + 1)

    # Latitudes e longitudes                           
    lats = np.linspace(lllat, urlat, num=ydef)
    lons = np.linspace(lllon, urlon, num=xdef)                      
#    lats = np.arange(lllat, urlat, gdy)
#    lons = np.arange(lllon, urlon, gdx) # fica com tamanho menor (-1 ponto)

    outDir = data_conf['Output directory']
    
    # Variáveis                           
    fnames = []

    for i in [*data_vars.values()]:
        fnames.append(i[0])                           
 
    nvars = len(fnames)
    
    #print(nvars,fnames)
    
    # Dicionário com o(s) dataset(s)
    ds_field = {}
    
    for stat in Stats:
               
        dataInicial_fmt = dataInicial.strftime("%Y%m%d%H")
        dataFinal_fmt = dataFinal.strftime("%Y%m%d%H")

        for exp in Exps:
        
            file_name = str(stat) + str(exp) + '_' + str(dataInicial_fmt) + str(dataFinal_fmt) + 'F.scan'
            fname = os.path.join(outDir, file_name)
            
            lista_n = []

            if os.path.exists(fname):
                #print(fname)
                              
                dsl = []
                ds = xr.Dataset()                           
                                       
                with open(fname,'rb') as f:
                                       
                    for t in np.arange(tdef): 
                                       
                        for i in np.arange(nvars):
                                 
                            #s2d = str(xdef*ydef) + 'float32'
                            #pad = '8b'  
                                
                            #dt = [ ('pad1', pad),  ('field', s2d), ('pad2', pad) ]    
                                
                            #dt_obj = np.dtype(dt)#, align=True)    
                                
                            #data = np.fromfile(f, dtype=np.float32, count=xdef*ydef, offset=8)    
                                
                            data = np.fromfile(f, dtype=np.float32, count=xdef*ydef, offset=8)
                                       
                            field = np.reshape(data, (xdef, ydef), order='F')  
                                                       
                            print(t, stat, exp, i)
                            
                            ds[fnames[i]] = (('lon','lat'), field)
                            ds.coords['lat'] = ('lat', lats)
                            ds.coords['lon'] = ('lon', lons)
                            ds.coords['time'] = [times[t]]
                                       
                            dst = ds.transpose('time', 'lat', 'lon')
                                       
                        dsl.append(dst)
                
                dsc = xr.concat(dsl, dim='time')                
                
                ds_field[ntpath.basename(str(fname))] = xr.concat(dsl, dim='time')
                
    return ds_field
