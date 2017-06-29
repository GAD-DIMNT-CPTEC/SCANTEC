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

#print("\n")
#exps = []
#datai = input("Entre com a data Inicial 'Exemplo 2013010100': ") #sys.argv[1]
#dataf = input("Entre com a data Final 'Exemplo 2013012118': ") #sys.argv[2]
#varname = input("Entre com os nome da Variável Exemplo UMES-850: ") #sys.argv[3]
#ref_name = input("Entre com o modelo de referência Exemplo NCEP: ")
#exps = input("Entre com os Experimentos Exemplo CTRL EnKF EnSRF: ")

#print("Data Inicial:", datai)
#print("Data Final:", dataf)
#print("Variável:", varname)
#print("Experimento Referência:", ref_name)
#print("Experimentos a ser Avaliados:", exps)

#rmse_experimetos = {}
#acor_experimentos = {}
#rmse_referecia = {}
#acor_refencia = {}

rmse_experimetos = {}
acor_experimentos = {}
rmse_referecia = {}
acor_refencia = {}

exps = []
ref_exps = []
instrucaoviaprompt = sys.argv #recebe o argumento via linha de comando
len_args = len(instrucaoviaprompt) #recebe o tamanho do argumento via parametro
#print("Tamanho da função len_args: ", len_args)
#print('Instrucaoviaprompt: ', instrucaoviaprompt)
for i in range(len_args-1,3,-1): # os quatro primeiros args. (0,1,2,3) sao o nome do script, as duas datas e o nome da variavel
  exps.append(sys.argv[i])

datai = sys.argv[1]
dataf = sys.argv[2]
varname = sys.argv[3]

#for i in range(len(exps)):
  #print("Valor do indice:",i,"\t\t Nome Do experimento:",exps[i])


nexps = len(exps)

myorder = list(range(nexps))
exps = [exps[i] for i in myorder] # reordena a lista para evitar que a legenda seja plotada em ordem aleatória


# Verifica qual dos experimentos indicados é a referência:
# - Se um experimento for indicado mais de uma vez, então 
#   ele é a referência:
testListExps = {}

for exp in exps:  #Modificar esse codigo tornar mais prático
  try:
    testListExps[exp] += 1
  except:
    testListExps[exp] = 1


ref_name = list(testListExps.keys())[list(testListExps.values()).index(2)] # Está fixo com 2.


ref_exps.append(ref_name)

# Escreve uma lista apenas com os nomes dos experimentos, sem a referencia:
exps_list = [x for x in exps if x not in ref_exps]

nexps_list = len(exps_list)

'''
print("Data Inicial:",datai)

print("Data Final:",dataf)

print("Valor do nexps:",nexps)

print("Experimentos: ",exps)

print("Variável:",varname)

print("Experimento Referência:", ref_name)

print("Experimentos Avaliados:",exps_list)
''' 
#print("\n")
#print("Valores do exps",exps)

def assembly_fnames(datai,dataf,exps):
  #print("entra na função assembly_fnames")

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
    #print("fname_vies",fname_vies)

    fname_rmse = "./" + exp + "/RMSEEXP01_" + datai + dataf + "T.scam"
    #print("fname_rmse: ",fname_rmse)
  
    fname_acor = "./" + exp + "/ACOREXP01_" + datai + dataf + "T.scam"
    #print("fname_acor: ",fname_acor)

    #fname_mean = "./" + exp + "/MEANEXP01_" + datai + dataf + "T.scam"
    #print("fname_mean: ",fname_mean)
  
    exps_fnames[exp] = fname_vies, fname_rmse, fname_acor#, fname_mean
  #print("VAlor do exps_fnames: ",exps_fnames)

def read_file(fname):
  #print("Entra na função read_file")
  """
  Nesta função são lidos propriamente os arquivos de cada key do dicionário
  exps_fnames. Note que a primeira linha é ignorada e que apenas as colunas
  de 0 a 22 são lidas (o Python inicia a contagem em 0; a coluna 0 contém
  apenas os tempos de previsão). A opção unpack=True, faz com que os dados
  sejam lidos por colunas.

  Observação: não é feito nenhum tipo de verificação dos arquivos.
  """

  global data_table

  #print(fname)
  data_table = np.loadtxt(fname,skiprows=1, unpack=True)
  #print("data_table: ",data_table)

def read_varc(data_table):
  #print("Entra na função read_varc")

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

  #print()
  #print("vname[0] = ", vname[0])
  #print()
  
  exp_data_table = data_table[int(vname[0])]

  #print()
  #print("TAbela da Referecnia: ", exp_data_table)
  #print(varname,"(",vname,") =",data_table[int(vname[0])]) # imprime a coluna da variável da referência
  #print()

def samples(exps_fnames):#SAMPLES SÃO AS AMOSTRAS OU SEJA OS EXPERIMENTOS

  #print("entra na função samples")

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
  
  global refstd, refacor, model, samples_vars, samples, stdrefs, acorefs, name_exp



  samples = dict()
  stdrefs = dict()
  acorefs = dict()

  ref = exps_fnames[ref_name] #lista com os valores da referencia

  #print("ref>>>>>>>>>>>>>>>>>>>>>>>>>>>>>: ",ref)
  #print("referencia:  ",ref[0])

  #A função np.mean Calcula a Média dos valor selecionados

  ref_data_rmse = ref[1]           # arquivo rmse
  #print("ref[1] = ",ref[1])

  read_file(ref_data_rmse)
  read_varc(data_table)

  #print()
  #print(data_table)
  #print()
  
  refstd = np.mean(exp_data_table)
  #rmse_referecia[ref_name] = exp_data_table #essa variavel recebe a coluna com os valores do RMSE da variavel de referencia pára fazer a comparação com os demais experimentos.
  
  rmse_referecia[0] = exp_data_table

  #print(exp_data_table) 
  #print("refstd =",refstd)

  stdrefs[varname] = refstd

  #print()

  ref_data_acor = ref[2] # arquivo acor
  #print("ref[2]: ", ref[2])

  read_file(ref_data_acor)
  read_varc(data_table)

  #print()
  #print(data_table)
  #print()

  refacor = np.mean(exp_data_table)
  #acor_refencia[ref_name] = exp_data_table #essa variavel recebe a coluna com os valores da Correlação de Anomalias da variavel de referencia pára fazer a comparação com os demais experimentos.
 
  acor_refencia[0] = exp_data_table
 
  #print(exp_data_table)
  #print("refacor =",refacor)

  acorefs[varname] = refacor

  #print()
  #print(len(exps_list))
  for i in range(len(exps_list)):    #print("Valor do exps_list: ",exps_list[i])

    #print("Valor de i = ",i)
    name_exp = exps_list[i]
    m = exps_fnames[exps_list[i]] 

    m_data_rmse = m[1]
    read_file(m_data_rmse)
    read_varc(data_table)

    mstd = np.mean(exp_data_table)
    #rmse_experimetos[name_exp] = exp_data_table
    rmse_experimetos[i] = exp_data_table

    #print("Valores do RMSE experimento",exps_list[i],"= ", rmse_experimetos[name_exp])

    #print("RMSE dos Experimentos = ",exp_data_table)
    #print("mstd =",mstd)

    #print()

    m_data_acor = m[2]
    read_file(m_data_acor)
    read_varc(data_table)

    macor = np.mean(exp_data_table)
    #acor_experimentos[name_exp] = exp_data_table
    acor_experimentos[i] = exp_data_table
    #print("Valores da Correlação de Anomalias do experimento", exps_list[i],"= ", acor_experimentos[name_exp])


    #print("Correlação de anomalias dos experimentos = ",exp_data_table)
    #print("macor =",macor)

    #print()

    samples_vars = np.array([mstd, macor, exps_list[i]])

    #print("Valor do samples: ",samples_vars)

    #print()

    samples.setdefault(varname,[]).append(samples_vars)

    #print("Valor do samples: ",samples)

    #print()

    #print("Valor do stdrefs: ",stdrefs)

    #print()

def plota_taylor(samples,stdrefs,acorefs):
  #print("Entra na função plota_taylor")
  

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
  #print("Entra na função main")

  assembly_fnames(datai,dataf,exps)
  #print('1')
  samples(exps_fnames)
  #print('2')
  plota_taylor(samples,stdrefs,acorefs)
  #print('3')

   
  print("\n\n\n\n\n\n")
  print("Nome do Modelo de referencia: ",ref_name)
  print()
  print("Valores da variavel de Referecnia sobre RMSE: \n", rmse_referecia)
  print("\n\n")

  print("Valores da variavel de referência sobre CORRELAÇÂO DE ANOMALIAS: \n",acor_refencia)
  print("\n\n")
  print("nomes das experimentos: ", exps_list)
  print()
  print("dicionário RMSE dos Experimentos: \n",rmse_experimetos)
  print("\n\n")
  print("dicionário da Correlação de Anomalias dos Experimentos: \n",acor_experimentos)
  print("\n\n")
  #print(rmse_experimetos.keys())
  #print(rmse_experimetos.values())
  res_rmse = []
  res_acor = []
  lista_r = []
  lista_a = []

  x = rmse_referecia.get(0)
  y = acor_refencia.get(0)

  for i in range(len(exps_list)):
  
    xx = rmse_experimetos.get(i)
    yy = acor_experimentos.get(i)

    for j in range(1,len(x),1):
      if(xx[j]>x[j]):
        r = xx[j]%x[j]
        #print("Valor de ",j,"= ",r)
        res_rmse.append(r)
      else:
        r = x[j]%xx[j]
        #print("Valor de ",j,"= ",r)
        res_rmse.append(r)

      
      if(yy[j]>y[j]):
        a = yy[j]%y[j]
        #print("Valor de ",j,"= ",a)
        res_acor.append(a)
      else:
        a = y[j]%yy[j]
        #print("Valor de ",j,"= ",a)
        res_acor.append(a)

      
  

  for k in range(0,len(x)-1 ,1):
    lr = [res_rmse[k],res_rmse[k+20],res_rmse[k+40]]
    la = [res_acor[k],res_acor[k+20],res_acor[k+40]]

    lista_r.append(lr)
    #print(lista_r[k])
    #print(min(lista_r[k]))

    lista_a.append(la)
    #print("res_rmse da",(k+1)*6,"horas: ",res_rmse[k],res_rmse[k+20],res_rmse[k+40])

  
  #print(valor_min)
  for m in range(0,len(lista_r),1):
    valor_min = min(lista_r[m])
    for n in range(0,len(exps_list),1):
        if valor_min == lista_r[m][n]:
          print("Experimento com o menor valor do RMSE: ",exps_list[n])

  #print(min(lista_r[0]))
  #print("\n\n")
  #print(lista_a)

  
  

main()
