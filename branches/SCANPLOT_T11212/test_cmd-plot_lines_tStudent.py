#! /usr/bin/env python3

# Uso:
# $ conda activate SCANPLOT-teste2
# $ python test_cmd-plot_lines_tStudent.py

# Importa o SCANPLOT (sc é um alias)
import scanplot as sc

# Outros módulos necessários para o escopo deste script
import pickle as pk

#cdir = '/lustre_xc50/carlos_bastarz/SCANPLOT/SCANPLOT_T11212'
cdir = '/home/carlos/Downloads/SCANPLOT_T11212'

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

## Constrói o dicionário dTable com as tabelas das estatísticas para o período JJA/2020
#dTable = sc.get_dataframe(dataInicial, dataFinal, Stats,
#                          Exps, outDir, series=False, save=False)
#
## Constrói o dicionário dTable_series com as tabelas das estatísticas para os intervalos do período JJA/2020
#dTable_series = sc.get_dataframe(dataInicial, dataFinal, Stats,
#                                 Exps, outDir, series=True, save=False)

# Se a função get_dataframe foi utilizada com a opção save=True,
# então pode-se utilizar as funções de plotagem a partir da leitura 
# do arquivo escrito em disco. Isso economiza tempo porque se várias
# funções plotagem forem executadas por scripts, então ler e criar
# o dicionário de dataframes apenas uma vez e salvar em disco. Após
# isso, todas as funções podem simplesmente ler o mesmo arquivo.

dTable = pk.load(open(cdir + '/test/SCANTEC.TESTS/dataout/scantec_ds_table.pkl', 'rb'))
dTable_series = pk.load(open(cdir + '/test/SCANTEC.TESTS/dataout/scantec_ds_table-series.pkl', 'rb'))

# Preparação das tabelas
Var = Vars[0][0].lower()

VarName = Vars[0][1]

varlev_exps = sc.concat_tables_and_loc(dTable, dataInicial, 
                                       dataFinal, Exps, Var, series=False)

varlev_dia_exps = sc.concat_tables_and_loc(dTable_series, dataInicial,
                                           dataFinal, Exps, Var, series=True)

lst_varlev_dia_exps_rsp = sc.df_fill_nan(varlev_exps, varlev_dia_exps)

# Cálculo dos limites do teste t-Student
ldrom_exp, ldrosup_exp, ldroinf_exp = sc.calc_tStudent(lst_varlev_dia_exps_rsp)

# Faz os gráficos de linha
lineStyles = ['b', 'g', 'r', 'k']

sc.plot_lines_tStudent(dataInicial, dataFinal, dTable_series,
                       Exps, Var, VarName, ldrom_exp, ldrosup_exp,
                       ldroinf_exp, varlev_exps, outDir, figDir=figDir,
                       showFig=True, saveFig=True)
