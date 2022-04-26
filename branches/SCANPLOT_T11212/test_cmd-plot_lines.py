#! /usr/bin/env python3

# Uso:
# $ conda activate SCANPLOT-teste2
# $ python test_cmd-plot_lines.py

# Importa o SCANPLOT (sc é um alias)
import scanplot as sc

# Constrói os dicionários data_vars e data_conf
data_vars, data_conf = sc.read_namelists('/home/carlos/Downloads/SCANPLOT/test/SCANTEC.TESTS')

# Considera as configurações do SCANTEC para o período (JJA/2020)
dataInicial = data_conf['Starting Time']
dataFinal = data_conf['Ending Time']
Vars = list(map(data_vars.get,[11,12,13])) # ou [*map(data_vars.get,[12,14])]
Stats = ['ACOR', 'RMSE', 'VIES']
Exps = list(data_conf['Experiments'].keys()) # ou [*data_conf["Experiments"].keys()]
#outDir = data_conf['Output directory']

# Atualiza os caminhos com os resultados do SCANTEC e onde as figuras serão armazenadas
outDir = '/home/carlos/Downloads/SCANPLOT/test/SCANTEC.TESTS/dataout'
figDir = outDir + '/figs'

# Constrói o dicionário dTable com as tabelas das estatísticas para o período JJA/2020
dTable = sc.get_dataframe(dataInicial,dataFinal,Stats,Exps,outDir,series=False)

# Faz os gráficos de linha
sc.plot_lines(dTable, Vars, Stats,
              outDir, figDir=figDir, 
              showFig=False, saveFig=True,
              combine=False)

lineStyles = ['k--', 'r-', 'g-', 'b-']

sc.plot_lines(dTable, Vars, Stats,
              outDir, figDir=figDir, 
              lineStyles=lineStyles,
              showFig=False, saveFig=True,
              combine=True)
