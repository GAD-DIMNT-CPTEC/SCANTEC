# /bin/bash

#
#BOP
#----------------------------------------------------------------------------------#
# Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC) #
#----------------------------------------------------------------------------------#
#
# !DESCRIPTION:
# Script para executar o SCANTEC.
#
# !CALLING SEQUENCE:
# ./run_scantec.sh N
#                  
#  N=1 TestCase utilizando dados do modelo BRAMS 05km para janeiro de 2016
#  N=2 TestCase utilizando dados do modelo ETA 05km para abril de 2020
#  N=3 TestCase utilizando dados do BAM para agosto 2014
#  N=4 Configuração do usuário 
#
# !Histórico de revisões: 
#      20-05-2016 - Lucas Amarante - Versão inicial
#      24-05-2016 - Claudio Pavani - TestCase BRAMS, execução do plot.gpl
#      15-06-2016 - Carlos Bastarz - Ajustes, padronização e generalizações
#      16-06-2016 - Claudio Pavani - Acrescentando mais um testeCase
#      04-07-2016 - Lucas Amarante - Atualizando testcase e outors parametros
#      15-06-2020 - Carlos Bastarz - Adaptações para o novo namelist
#      18-06-2020 - Luiz Sapucci   - ajustes antes de publicação da versão beta
#      11-11-2020 - Luiz Sapucci   - ajustes do diretorio dados dados/das na release
#
#EOP  
#----------------------------------------------------------------------------------#
#BOC

# Verifica parâmetros de entrada
if [ -z "${1}" ] 
then
  echo "" 
  echo "Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC)"
  echo "" 
  echo "A opção TestCase não foi informada!"
  echo ""
  echo "Uso:"
  echo "./run_scantec.sh 1 - TestCase do BRAMS (Jan/2016)"
  echo "./run_scantec.sh 2 - TestCase do ETA   (Abr/2020)"  
  echo "./run_scantec.sh 3 - TestCase do BAM   (Ago/2014)"
  echo "./run_scantec.sh 4 - dados definidos pelo usuário"
  echo "" 
  exit 1
else
  export TESTCASE=${1} 
  if [ ${TESTCASE} -gt 4 ]
  then
    echo ""
    echo "Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC)"
    echo "" 
    echo "A opção TestCase desconhecida!"
    echo ""
    echo "Uso:"
    echo "./run_scantec.sh 1 - TestCase do BRAMS (Jan/2016)"
    echo "./run_scantec.sh 2 - TestCase do ETA   (Abr/2020)"  
    echo "./run_scantec.sh 3 - TestCase do BAM   (Ago/2014)"
    echo "./run_scantec.sh 4 - dados definidos pelo usuário"
    echo ""
    exit 1
  fi
fi

# Configuração inicial das variáveis 

# Hostname da máquina
maqui=$(hostname)

# Diretório atual do sistema
dir_act=$(pwd)

# Diretório com as tabelas do SCANTEC
scantec_tables=${dir_act}/tables

# Mudar aqui se o usuario desejar adaptar a uma outra heraquia de diretórios
# Diretorio dos dados da rodada do usuario
dir_data=${dir_act}
mkdir -p ${dir_data}/logfile
mkdir -p ${dir_data}/datain
mkdir -p ${dir_data}/dataout

# Data e arquivo de log
RUNTM=$(date "+%Y%m%d.%H.%M")
ARQlog=${dir_data}/logfile/scantec-${RUNTM}.log

############################
# Denifições dos testcases #
############################

case ${TESTCASE} in
[1]) 

# Configurações do TestCase BRAMS (NÃO ALTERAR!) 
MODELCASE=" <<<< TESTCASE BRAMS >>>>>"
dir_test=TestBRAMS

# Datas
datai=2016010100
dataf=2016010300
passo_analise=24
passo_previsao=24
total_previsao=36

# Regiões
lat_low=-49.875 
lon_low=-82.625 
lat_up=11.375 
lon_up=-35.375 
dx=0.25  
dy=0.25 

# Quantidade de experimentos
quant_exp=1

# Referencias
# Plugin modelo
pl_model_refer=BRAMS_5km_19levs

# Análises
arq_refer=/dados/das/pesq1/public/SCANTEC/TestCase/BRAMS/OPER.2016_exp5kmM/%y4%m2%d200/BRAMS5.exp5kmM_%y4%m2%d200-A-%y4-%m2-%d2-000000-g1.ctl

# Experimentos
# Plugin experimento
pl_model_exper=BRAMS_5km_19levs

# Previsões
arq_prev=/dados/das/pesq1/public/SCANTEC/TestCase/BRAMS/OPER.2016_exp5kmM/%y4%m2%d200/BRAMS5.exp5kmM_%iy4%im2%id200-A-%fy4-%fm2-%fd2-%fh20000-g1.ctl


# Climatologia
use_climatologia=1
arq_clim=/dados/das/pesq1/public/SCANTEC/climatologia/climatologia50yr.%mc.ctl

;;

[2])

#--------------------------------------------------------------------#
# Configuraçõees do TestCase do ETA (NÃO ALTERAR!)                   #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE ETA >>>>>>"
dir_test=TestETA

# Datas
datai=2020040400
dataf=2020040600
passo_analise=12
passo_previsao=12
total_previsao=36

# Regiões
lat_low=-51.000000 
lon_low=-84.099998
lat_up=15.0000
lon_up=-25.999998 
dx=0.05 
dy=0.05 

# Quantidade de experimentos
quant_exp=1

# Referências 
# Plugin modelo
pl_model_refer=ETA_ams_05km_22levs

# Análises
arq_refer=/dados/das/pesq1/public/SCANTEC/TestCase/ETA/Eta_ams_05km202004/%d2/%h2/eta_05km_%y4%m2%d2%h2+%y4%m2%d2%h2.ctl

# Experimento
# Plugin experimento
pl_model_exper=ETA_ams_05km_22levs

# Previsões
arq_prev=/dados/das/pesq1/public/SCANTEC/TestCase/ETA/Eta_ams_05km202004/%d2/%h2/eta_05km_%y4%m2%d2%h2+%fy4%fm2%fd2%fh2.ctl 

# Climatologia
use_climatologia=1
arq_clim=/dados/das/pesq1/public/SCANTEC/climatologia/climatologia50yr.%mc.ctl

;;

[3])

#--------------------------------------------------------------------#
# Configuraçõees do TestCase BAM (NÃO ALTERAR!)              #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE BAM  >>>>>>"
dir_test=TestBAM

# Datas
datai=2014080500
dataf=2014080600
passo_analise=24
passo_previsao=24
total_previsao=72

# Regiões
lat_low=-49.875 
lon_low=-82.625 
lat_up=11.375 
lon_up=-35.375 
dx=0.4  
dy=0.4 

# Quantidade de experimentos
quant_exp=1

# Referências 
# Plugin modelo
pl_model_refer=BAM_TQ0299L064_18levs

# Análises
arq_refer=/dados/das/pesq1/public/SCANTEC/TestCase/AGCM/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%y4%m2%d2%h2%y4%m2%d2%h2P.icn.TQ0299L064.ctl

# Experimento
# Plugin experimento
pl_model_exper=BAM_TQ0299L064_18levs

# Previsões
arq_prev=/dados/das/pesq1/public/SCANTEC/TestCase/AGCM/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0299L064.ctl

# Climatologia
use_climatologia=1
arq_clim=/dados/das/pesq1/public/SCANTEC/climatologia/climatologia50yr.%mc.ctl

;;

 
[4])

#--------------------------------------------------------------------#
# Configurações do usuário (ALTERAR O QUE FOR NECESSÁRIO)            #
#--------------------------------------------------------------------#


# Datas
datai=2016010100
dataf=2016010500
passo_analise=12
passo_previsao=12
total_previsao=120

# Regiões
lat_low=-49.875 
lon_low=-82.625 
lat_up=11.375 
lon_up=-35.375 
dx=0.4  
dy=0.4 

# Referências
# Plugin modelo
pl_model_refer=prefixo_tabela_modelo

# Análises
arq_refer=diretorio_dados/arquivo_anl.ctl

# Quantidade de experimentos
quant_exp=1

#Plugin experimento
pl_model_exper=prefixo_tabela_modelo/arquivo_fct.ctl

# Previões
arq_prev=diretorio_dados

# Climatologia
use_climatologia=0
arq_clim=diretorio_climatologia/climatologia50yr.%mc.ctl

;;

esac

# Diretorio de saida do resultados
saida_results=${dir_data}/dataout/${dir_test}


if [ ! -e ${saida_results} ]; then mkdir -p ${saida_results}; fi

echo ""
echo " <<< INICIANDO SCANTEC >>>        "
echo ""${MODELCASE} 
echo ""
echo " Configurações da avaliação:               " 
echo " ==========================                "  
echo " Data inicial:   ${datai}                  "
echo " Data final:     ${dataf}                  "
echo " Passo análise:  ${passo_analise}          "
echo " Passo previsão: ${passo_previsao}         "
echo " Total previsão: ${total_previsao}         "
echo ""
echo " Região:                                   "
echo " Lat low: ${lat_low}                       "
echo " Lon low: ${lon_low}                       "
echo " Lat up:  ${lat_up}                        "
echo " Lon up:  ${lon_up}                        "
echo ""
echo " Quantidade de experimentos: ${quant_exp}  "
echo ""
echo " Plugin da referência: ${pl_model_refer}   "
echo " Análise:                                  "
echo " ${arq_refer}                              "
echo ""
echo " Plugin do experimento: ${pl_model_exper}     "
echo " Previsões:                                "
echo " ${arq_prev}                               "  
echo ""
echo " Uso climatologia: ${use_climatologia}     "
echo ""
echo " Resultados:                               "
echo " ${saida_results}                          "
echo ""
echo " Arquivo de log:                           "
echo " Log do processo: ${ARQlog}                "
echo " ==========================                "  
echo ""

#--------------------------------------------------------------------#
data=$(date)
 
# Iniciando o processamento
echo " Início do processamento: ${data}" >> ${ARQlog}
echo " Início do processamento: ${data}"

# Entrando no diretorio do programa principal
cd ${dir_act}/bin

echo ""
echo " Criando o arquivo de configurações em bin/scantec.conf"
echo ""

INPUTDATA='$INPUTDATA'
cat <<EOT1 > scantec.conf
$INPUTDATA
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC) #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#===================================================================================
#                          Runtime options
#
Starting Time: ${datai}                #Format  :: YYYYMMDDHH
Ending Time: ${dataf}                  #Format  :: YYYYMMDDHH
Analisys Time Step: ${passo_analise}   #Format  :: HH
Forecast Time Step: ${passo_previsao}  #Format  :: HH
Forecast Total Time: ${total_previsao} #Format  :: HH
Time Step Type: forward                # backward or forward
History Time: 48                       #Format  :: HH
scantec tables: ${scantec_tables}

#===================================================================================
#                       DOMAIN SPECIFICATION
# Definition of Running Domain
# Specify the domain extremes in latitude and longitude
#
#              +----------------------------------+
#              |**********************************|
#              |**********************************|
#            L |*************** +--------------+ *|
#            A |*************** |              | *|
#            T |*************** |     Area     | *|
#            I | * +--------+ * |      02      | *|
#            T | * |        | * |              | *|
#            U | * |  area  | * |              | *|
#            D | * |   01   | * |              | *|
#            E | * |        | * +--------------+ *|
#              | * |        | ********************|
#              | * +--------+ ********************|
#              | *********************************|
#              +----------------------------------+
#                        L O N G I T U D E
#

run domain number: 1 # Number of areas to analise

# domain of each area
#                    AREAS   1          2         3      4           5        6              7        8
#                            1          Manaus    Global América Sul Brasil   Hemisferio Sul Trópicos Hemisfério Norte
run domain lower left lat:   ${lat_low} # -5.875  #-80   # -49.875   # -60.95 #  -35         # -80    # -20
run domain lower left lon:   ${lon_low} #-65.625  #  0   # -82.625   # -82.95 #  -80         #   0    #   0
run domain upper right lat:  ${lat_up}  #  5.375  # 80   #  11.375   #  20.95 #   10         # -20    #  80
run domain upper right lon:  ${lon_up}  #-60.375  #360   # -35.375   # -33.95 #  -30         # 360    # 360
run domain resolution dx:    ${dx}      #  0.5    #  0.4 #   0.4     #   0.4  #    0.4       #   0.4  #   0.4
run domain resolution dy:    ${dy}      #  0.5    #  0.4 #   0.4     #   0.4  #    0.4       #   0.4  #   0.4
# Obs.: para o modelo GFS, colocar lon de 0 a 360 (360-valorLON)

#======================================================================
#                              Files to Analyse
#===============
# Reference File
#
#         Name diretory File_Name_with_mask
#
Reference Model Name: ${pl_model_refer}
Reference file: ${arq_refer}

#=================
# Experiment Files
#
Experiments: ${quant_exp}
#ModelId Name Diretory File_Name_with_mask
${pl_model_exper} EXP01 ${arq_prev}
::

#=================
# Climatology File
#
Use Climatology: ${use_climatologia}  # 0-do not use, 1-use
# Diretory prefix mask sulfix
Climatology Model Name: AGCM_TQ0062L028_50YR_CLIMATOLOGY_18levs
Climatology file: ${arq_clim} 

#=======
# OUTPUT
#
Output directory: ${saida_results}
EOT1

echo " Arquivo de configuracao criado com sucesso."
echo ""

echo " Criando arquivo scantec.conf no diretório de execução do SCANTEC ..."  >> ${ARQlog}
echo  >> ${ARQlog}
cat scantec.conf >> ${ARQlog}                           
echo  >> ${ARQlog}
echo  >> ${ARQlog}

#--------------------------------------------------------------------#

# Executando o SCANTEC
cd ${dir_act}/bin 

if [ -e scantec.x ]
then
  echo " Executando o scantec.x ..."
  echo ""
  ./scantec.x >> ${ARQlog}
  if [ $(echo $?) -ne 0 ]
  then
    echo " Falha ao executar o SCANTEC. Abortando!"
    exit 2
  else
    data=$(date)
    echo " Final do processo: ${data}"
    
    echo " ============================" >> ${ARQlog}
    echo  >> ${ARQlog}
    echo " Final do processo: ${data}" >> ${ARQlog}
    echo >> ${ARQlog} 
    
    echo 
    echo " Log do processo: ${ARQlog}"
    echo "===================================================================================="
    echo 
    echo " Fim do processo!" 
    echo 
    
    echo " Fim do processo" >> ${ARQlog}
    echo >> ${ARQlog}
  fi
else
  echo ""
  echo " Programa scantec.x não existe! Verifique a compilação do programa."
  echo ""
  exit 3
fi
