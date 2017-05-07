#! /usr/bin/env python

"""
Objetivo:
- Script para ler as tabelas de estatísticas do SCANTEC e plotar 
o diagrama de Taylor baseado no RMSE (raiz do erro quadrático médio)
e no ACOR (correlação de anomalia)

Dependências:
- taylorDiagram.py (https://gist.github.com/ycopin/3342888)
- matplotlib;
- numpy;
- sys;

Uso:
$ ./plota_taylor_diag.py 2013010100 2013013118 VTMP-500 CTRL EnKF EnSRF NCEP NCEP

No exemplo acima, estão sendo informados junto com o script as seguintes opções:
- 2013010100: data inicial
- 2013013118: data final
- VTMP-500..: nome da variável a ser avaliada
- EnSRF.....: nome da pasta que contém os arquivos com as estatísticas do EnSRF
- EnKF......: nome da pasta que contém os arquivos com as estatísticas do EnKF
- NCEP......: nome da pasta que contém os arquivos com as estatísticas do NCEP
- CTRL......: nome da pasta que contém os arquivos com as estatísticas do CTRL

Observações:
- As data inicial e final são aquelas que estão nos nomes dos arquivos com as
estatísticas. Por exemplo: RMSEEXP01_20130101002013013118T.scam;
- Pelo menos dois experimentos devem ser indicados para plotar o diagrama de Taylor;
- Nesta versão, ainda não se está considerando uma referência real (análise independente,
reanálise, climatologia independente ou observação). A referência é então um dos experimentos
indicados.

TODO:
- Implementar função para controle dos argumentos de entrada;
- Definir precisão das médias (rmse e acor);
- Consertar os raios dos experimentos (estão maiores/menores do que deveriam ser).

Histórico:
- XX/10/2016: versão inicial;
- 08/11/2016: modificada a aparencia do gráfico (posição e tamanho das fontes e legendas);
              modificada a forma como a lista de experimentos é armazenada (evita ordem aleatória);
              adicionado raio do experimento de referência (indica o valor de correlação de anomalia);
- 07/05/2017: início do versionamento (inclui tabelas exemplos do SCANTEC);

Carlos Frederico Bastarz (carlos.frederico@cptec.inpe.br, 26/10/2016)
"""

import sys
import numpy as np
from taylorDiagram_mod import TaylorDiagram
import matplotlib.pyplot as plt

global datai, dataf, exps, ref_name, varname, nexps

print()

exps = []
ref_exps = []

len_args = len(sys.argv)

for i in range(len_args-1,3,-1): # os quatro primeiros args. (0,1,2,3) sao o nome do script, as duas datas e o nome da variavel
  exps.append(sys.argv[i])

datai = sys.argv[1]
dataf = sys.argv[2]
varname = sys.argv[3]

print("Data Inicial:",datai)
print("Data Final:",dataf)

nexps = len(exps)
myorder = list(range(nexps))
exps = [exps[i] for i in myorder] # reordena a lista para evitar que a legenda seja plotada em ordem aleatória

print("Experimentos:",exps)
print("Variável:",varname)

# Verifica qual dos experimentos indicados é a referência:
# - Se um experimento for indicado mais de uma vez, então 
#   ele é a referência:
testListExps = {}

for exp in exps:
  try:
    testListExps[exp] += 1
  except:
    testListExps[exp] = 1

ref_name = list(testListExps.keys())[list(testListExps.values()).index(2)] # Está fixo com 2.
print("Experimento Referência:", ref_name)

ref_exps.append(ref_name)

# Escreve uma lista apenas com os nomes dos experimentos, sem a referencia:
exps_list = [x for x in exps if x not in ref_exps]

nexps_list = len(exps_list)

print("Experimentos Avaliados:",exps_list)

print()

def assembly_fnames(datai,dataf,exps):

  """
  Nesta função é montado o dicionário exps_fnames.
  Nele, as informações são armazenadas da seguinte forma:
  {
  'EnSRF': ('./EnSRF/VIESEXP01_20130101002013013118T.scam', 
            './EnSRF/RMSEEXP01_20130101002013013118T.scam', 
            './EnSRF/ACOREXP01_20130101002013013118T.scam'),
   'CTRL': ('./CTRL/VIESEXP01_20130101002013013118T.scam', 
            './CTRL/RMSEEXP01_20130101002013013118T.scam', 
            './CTRL/ACOREXP01_20130101002013013118T.scam'), 
   'NCEP': ('./NCEP/VIESEXP01_20130101002013013118T.scam', 
            './NCEP/RMSEEXP01_20130101002013013118T.scam', 
            './NCEP/ACOREXP01_20130101002013013118T.scam'),
   'EnKF': ('./EnKF/VIESEXP01_20130101002013013118T.scam', 
            './EnKF/RMSEEXP01_20130101002013013118T.scam', 
            './EnKF/ACOREXP01_20130101002013013118T.scam')
  }
  Ou seja, são criadas as keys EnSRF, CTRL, NCEP e EnKF (informadas
  pela linha de comando) e para cada uma delas, são formados e 
  atribuídos os nomes dos arquivos com as estatísticas de interesse.
  Esse dicionário será utilizado posteriormente para abrir um ou
  mais arquivos entre aqueles que o SCANTEC escreve.

  Observação: não é feito nenhum tipo de verificação dos arquivos.
  """

  global fname_vies, fname_rmse, fname_acor, exps_fnames

  exps_fnames = dict() 

  for j in range(len(exps)):

    exp = exps[j]

    fname_vies = "./" + exp + "/VIESEXP01_" + datai + dataf + "T.scam"

    fname_rmse = "./" + exp + "/RMSEEXP01_" + datai + dataf + "T.scam"
  
    fname_acor = "./" + exp + "/ACOREXP01_" + datai + dataf + "T.scam"

    fname_mean = "./" + exp + "/MEANEXP01_" + datai + dataf + "T.scam"
  
    exps_fnames[exps[j]] = fname_vies, fname_rmse, fname_acor, fname_mean

def read_file(fname):

  """
  Nesta função são lidos propriamente os arquivos de cada key do dicionário
  exps_fnames. Note que a primeira linha é ignorada e que apenas as colunas
  de 0 a 22 são lidas (o Python inicia a contagem em 0; a coluna 0 contém
  apenas os tempos de previsão). A opção unpack=True, faz com que os dados
  sejam lidos por colunas.

  Observação: não é feito nenhum tipo de verificação dos arquivos.
  """

  global data_table

#  print(fname)
  data_table = np.loadtxt(fname,skiprows=1, unpack=True)

def read_varc(data_table):

  global exp_data_table, vnames, vname

  vnames = {
            '%Previsao': ['0',''],
            'VTMP-925':  ['1','Temperatura Virtual em 925hPa'], 
            'VTMP-850':  ['2','Temperatura Virtual em 850hPa'],
            'VTMP-500':  ['3','Temperatura Virtual em 500hPa'],
            'TEMP-850':  ['4','Temperatura do Ar em 925hPa'],
            'TEMP-500':  ['5','Temperatura do Ar em 850hPa'],
            'TEMP-250':  ['6','Temperatura do Ar em 500hPa'],
            'PSNM-000':  ['7','Pressão em Superfície'],
            'UMES-925':  ['8','Umidade Específica em 925hPa'],
            'UMES-850':  ['9','Umidade Específica em 850hPa'],
            'UMES-500': ['10','Umidade Específica em 500hPa'],
            'AGPL-925': ['11','Água Precipitável em 925hPa'],
            'ZGEO-850': ['12','Altura Geopotencial em 850hPa'],
            'ZGEO-500': ['13','Altura Geopotencial em 500hPa'],
            'ZGEO-250': ['14','Altura Geopotencial em 250hPa'],
            'UVEL-850': ['15','Vento Zonal em 850hPa'],    
            'UVEL-500': ['16','Vento Zonal em 500hPa'],    
            'UVEL-250': ['17','Vento Zonal em 250hPa'],    
            'VVEL-850': ['18','Vento Meridional em 850hPa'],
            'VVEL-500': ['19','Vento Meridional em 500hPa'],
            'VVEL-250': ['20','Vento Meridional em 250hPa'],
            'PREC-000': ['21','Precipitação Convenctiva'],
            'PREV-000': ['22' 'Precipitação Estratiforme'],
           }

  vname = vnames[varname]
  exp_data_table = data_table[int(vname[0])]
#  print(varname,"(",vname,") =",data_table[int(vname[0])]) # imprime a coluna da variável da referência
#  print()

def samples(exps_fnames):

  """
  Nesta função são calculados os samples que serão utilizados no plot do
  diagrama de Taylor. O samples é também um dicionário e será organizado 
  da seguinte maneira (exemplo):
  {
  'TEMP-850': array([['10.0376309381', '0.882568506839', 'CTRL'],
                    ['10.0335339753', '0.880217920479', 'NCEP'],
                    ['10.0297731423', '0.90174622461',  'EnKF']])
  }
  Para cada variável lida de dentro dos arquivos de estatísticas do SCANTEC,
  é armazanada a seguinte tripleta: 
  desvio padrão (ou rms), coeficiente de correlação (ou correlação de anomalia) e nome do exp.
  """

  global refstd, refacor, model, samples_vars, samples, stdrefs, acorefs

  samples = dict()
  stdrefs = dict()
  acorefs = dict()

  ref = exps_fnames[ref_name] 

  ref_data_rmse = ref[1]           # arquivo rmse
  read_file(ref_data_rmse)         
  read_varc(data_table)            
  refstd = np.mean(exp_data_table) 
#  print("refstd =",refstd)

  stdrefs[varname] = refstd

#  print()

  ref_data_acor = ref[2]           # arquivo acor
  read_file(ref_data_acor)
  read_varc(data_table)
  refacor = np.mean(exp_data_table)
#  print("refacor =",refacor)

  acorefs[varname] = refacor

#  print()

  for i in range(len(exps_list)):
#    print(exps_list[i])

    m = exps_fnames[exps_list[i]] 

    m_data_rmse = m[1]
    read_file(m_data_rmse)
    read_varc(data_table)
    mstd = np.mean(exp_data_table)
#    print("mstd =",mstd)

#    print()

    m_data_acor = m[2]
    read_file(m_data_acor)
    read_varc(data_table)
    macor = np.mean(exp_data_table)
#    print("macor =",macor)

#    print()

    samples_vars = np.array([mstd, macor, exps_list[i]])

#    print(samples_vars)

#    print()

    samples.setdefault(varname,[]).append(samples_vars)

#    print(samples)

#    print()

#    print(stdrefs)

#    print()

def plota_taylor(samples,stdrefs,acorefs):
  
  """
  Nesta função é chamada a classe TaylorDiagram.
  """

  # Mais cores em: https://gist.github.com/endolith/2719900    
  colors = plt.matplotlib.cm.Set1(np.linspace(0,1,len(samples[varname])))

  fig = plt.figure(figsize=(5,5))
  
  for varlev in [varname]:
  
      dia = TaylorDiagram(stdrefs[varlev], acorefs[varlev], colors, fig=fig, label=ref_name)
  
      # Add samples to Taylor diagram
      for i,(stddev,corrcoef,name) in enumerate(samples[varname]):
      
          dia.add_sample(stddev, corrcoef,colors[i],
                         marker='$%d$' % (i+1), ms=7, ls='',
                         mfc=colors[i], mec=colors[i], 
                         label=name)
      
      # Add RMS contours, and label them
#      contours = dia.add_contours(levels=5, colors='0.5') # 5 levels
#      dia.ax.clabel(contours, inline=1, fontsize=10, fmt='%.1f')

      # Título
      if (datai == dataf):
        dia._ax.set_title(vnames[varlev][1] + '\n(' + datai + ')', y=1.025)
      else:
        dia._ax.set_title(vnames[varlev][1] + '\n(' + datai + ' - ' + dataf + ')', y=1.025)
  
  fig.legend(dia.samplePoints,
             [ p.get_label() for p in dia.samplePoints ],
             numpoints=1, prop={'size':6}, bbox_to_anchor=(0.95,0.9))

  if (datai == dataf):
    plt.savefig(ref_name + '_' + varname + '_' + datai + '.png')
  else:
    plt.savefig(ref_name + '_' + varname + '_' + datai + '-' + dataf + '.png')

def main():

  assembly_fnames(datai,dataf,exps)

  samples(exps_fnames)

  plota_taylor(samples,stdrefs,acorefs)

main()
