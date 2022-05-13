#! /usr/bin/env python3

# Uso:
# $ conda activate SCANPLOT-teste2
# $ python test_cmd-get_dataset.py

# Importa o SCANPLOT (sc é um alias)
import scanplot as sc

cdir = '/lustre_xc50/carlos_bastarz/SCANPLOT/SCANPLOT_T11212'
#cdir = '/home/carlos/Downloads/SCANPLOT_T11212'

# Constrói os dicionários data_vars e data_conf
data_vars, data_conf = sc.read_namelists(cdir + '/test/SCANTEC.TESTS')

# Considera as configurações do SCANTEC para o período (JJA/2020)
dataInicial = data_conf['Starting Time']
dataFinal = data_conf['Ending Time']
Vars = list(map(data_vars.get,[11,12,13])) # ou [*map(data_vars.get,[12,14])]
Stats = ['ACOR', 'RMSE', 'VIES']
Exps = list(data_conf['Experiments'].keys()) # ou [*data_conf["Experiments"].keys()]
#outDir = data_conf['Output directory']

# Atualiza os caminhos com os resultados do SCANTEC e onde as figuras serão armazenadas
outDir = cdir + '/test/SCANTEC.TESTS/dataout'
figDir = outDir + '/figs'

# Constrói o dicionário dSet com as distribuições espaciais das estatísticas para o período JJA/2020
dSet = sc.get_dataset(d_data_conf, d_data_vars, Stats, 
                      Exps, outDir, series=False, save=True)

# Constrói o dicionário dSet_series com as distribuições espaciais das estatísticas para os intervalos do período JJA/2020
dSet_series = sc.get_dataset(d_data_conf, d_data_vars, Stats, 
                      Exps, outDir, series=True, save=True)

