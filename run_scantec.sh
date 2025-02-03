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
#      03-02-2025 - Victor Ranieri - Ajustes dos TestCases para usuarios externos, inclusão do modelo MONAN.
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
  echo "./run_scantec.sh 1 - TestCase do BRAMS         (Jul/2023)"
  echo "./run_scantec.sh 2 - TestCase do ETA           (Jul/2023)"  
  echo "./run_scantec.sh 3 - TestCase do WRF           (Jul/2023)"
  echo "./run_scantec.sh 4 - TestCase do BAM           (Jul/2023)"
  echo "./run_scantec.sh 5 - Compara WRF/ETA/BRAMS/BAM (Jul/2023)"
  echo "./run_scantec.sh 6 - MONAN                     (Jan/2025)"
  echo "./run_scantec.sh 7 - Dados definidos pelo usuário"
  echo "" 
  exit 1
else
  export TESTCASE=${1} 
  if [ ${TESTCASE} -gt 7 ]
  then
    echo ""
    echo "Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC)"
    echo "" 
    echo "A opção TestCase desconhecida!"
    echo ""
    echo "Uso:"
    echo "./run_scantec.sh 1 - TestCase do BRAMS         (Jul/2023)"
    echo "./run_scantec.sh 2 - TestCase do ETA           (Jul/2023)"
    echo "./run_scantec.sh 3 - TestCase do WRF           (Jul/2023)"
    echo "./run_scantec.sh 4 - TestCase do BAM           (Jul/2023)"
    echo "./run_scantec.sh 5 - Compara WRF/ETA/BRAMS/BAM (Jul/2023)"
    echo "./run_scantec.sh 6 - MONAN                     (Jan/2025)"
    echo "./run_scantec.sh 7 - Dados definidos pelo usuário"
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

# Diretórios para TestCase
mkdir -p ${dir_data}/datain/climatologia
mkdir -p ${dir_data}/datain/TestBRAMS
mkdir -p ${dir_data}/datain/TestETA
mkdir -p ${dir_data}/datain/TestWRF
mkdir -p ${dir_data}/datain/TestBAM
mkdir -p ${dir_data}/datain/TestMONAN

# Data e arquivo de log
RUNTM=$(date "+%Y%m%d.%H.%M")
ARQlog=${dir_data}/logfile/scantec-${RUNTM}.log

############################
# Denifições dos testcases #
############################

case ${TESTCASE} in

[1])

#------------------------------------------------#
# Configurações do TestCase BRAMS (NÃO ALTERAR!) #
#------------------------------------------------#

MODELCASE=" <<<< TESTCASE BRAMS >>>>>"

# Variavel de configuração scantec.conf
variavel=1

# diretório de saída
dir_test=TestBRAMS


# Verificação de existencia de arquivo

cd ${dir_data}/datain/TestBRAMS

tamanho_BRAMS=$(du -sh ${dir_data}/datain/TestBRAMS | awk '{print $1}')


tamanho_clima=$(du -sh ${dir_data}/datain/climatologia | awk '{print $1}')

if [ ${tamanho_BRAMS} = "5,0G" ]; then
	echo ''
	echo 'Download do modelo BRAMS já efetuado. Iniciando os trabalhos...'
	echo ''
else 
	echo ''
	echo 'Iniciando o download do TestCase do modelo BRAMS.'
	echo ''
#	wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/BRAMS/" >> TestBRAMS.log 2>&1
	echo ''
	echo 'Download Concluído'
	echo ''
fi

if [ ${tamanho_clima} = "159M" ]; then
	echo ''
        echo 'Download da climatologia já efetuado. Iniciando os trabalhos...'
	echo ''
else
        echo ''
        echo 'Iniciando o download da climatologia.'
        echo ''
	cd ../climatologia
	wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/datain/climatologia/" >> climatologia.log 2>&1 
	echo ''
	echo 'Download Concluido'
	echo ''
fi


# Datas
datai=2023070100
dataf=2023070700
passo_analise=24
passo_previsao=24
total_previsao=120

# Regiões
lat_low=-49.875 
lon_low=-82.625 
lat_up=11.375 
lon_up=-35.375 
dx=0.4  
dy=0.4 

# Quantidade de experimentos
quant_exp=1

# Referencias
# Plugin modelo
pl_model_refer=BRAMS_ams_08km_SCAN_5levs
#pl_model_refer=era5_SCAN_5levs

# Análises
arq_refer=${dir_data}/datain/TestBRAMS/%y4%m2%d200/BRAMS_ams_08km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl
#arq_refer=${dir_data}/datain/TestERA5/%y4%m2%d200/ERA5_SCAN_%y4%m2%d200_%y4%m2%d200.ctl

# Experimentos
# Plugin experimento
pl_model_exper=BRAMS_ams_08km_SCAN_5levs

# Previsões
arq_prev=${dir_data}/datain/TestBRAMS/%y4%m2%d200/BRAMS_ams_08km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl


# Climatologia
use_climatologia=1
arq_clim=${dir_data}/datain/climatologia/climatologia50yr.%mc.ctl

;;

[2])

#--------------------------------------------------------------------#
# Configuraçõees do TestCase do ETA (NÃO ALTERAR!)                   #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE ETA >>>>>>"

# Variável de configuração scantec.conf
variavel=2

# diretório de saída
dir_test=TestETA

# Verificação de existencia dos arquivos

cd ${dir_data}/datain/TestETA

tamanho_ETA=$(du -sh ${dir_data}/datain/TestETA | awk '{print $1}')

tamanho_clima=$(du -sh ${dir_data}/datain/climatologia | awk '{print $1}')


if [ ${tamanho_ETA} = "4,0G" ]; then

	echo ''
        echo 'Download do modelo ETA já efetuado. Iniciando os trabalhos...'
	echo ''
else
	echo ''
        echo 'Iniciando o download do TestCase do modelo ETA. '
        echo ''
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/ETA/" 
#>> TestETA.log 2>&1
fi

if [ ${tamanho_clima} = "159M" ]; then
	echo ''
        echo 'Download da climatologia já efetuado. Iniciando os trabalhos...'
	echo ''
else
        echo ''
        echo 'Iniciando o download da climatologia.'
        echo ''
        cd ../climatologia
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/datain/climatologia/" >> climatologia.log 2>&1
        echo ''
	echo 'Download concluido.'
	echo ''
fi

# Datas
datai=2023070100
dataf=2023070700
passo_analise=24
passo_previsao=24
total_previsao=120

# Regiões
lat_low=-51.000000 
lon_low=-84.099998
lat_up=15.0000
lon_up=-25.999998 
dx=0.4 
dy=0.4 

# Quantidade de experimentos
quant_exp=1

# Referências 
# Plugin modelo
pl_model_refer=Eta_ams_08km_SCAN_5levs

# Análises
arq_refer=${dir_data}/datain/${dir_test}/%y4%m2%d200/Eta_ams_08km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl


# Experimento
# Plugin experimento
pl_model_exper=Eta_ams_08km_SCAN_5levs

# Previsões
arq_prev=${dir_data}/datain/TestETA/%y4%m2%d200/Eta_ams_08km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl
 

# Climatologia
use_climatologia=1
arq_clim=${dir_data}/datain/climatologia/climatologia50yr.%mc.ctl

;;

[3])

#--------------------------------------------------------------------#
# Configuraçõees do TestCase WRF (NÃO ALTERAR!)                      #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE WRF  >>>>>>"

variavel=3

# Diretório de saída
dir_test=TestWRF

# Verificação de existencia dos arquivos
cd ${dir_data}/datain/TestWRF

tamanho_WRF=$(du -sh ${dir_data}/datain/TestWRF | awk '{print $1}')

tamanho_clima=$(du -sh ${dir_data}/datain/climatologia | awk '{print $1}')


if [ ${tamanho_WRF} = "5,4G" ]; then

        echo ''
        echo 'Download do modelo WRF já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download do TestCase do modelo WRF. '
        echo ''
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/WRF/" >> TestWRF.log 2>&1
fi

if [ ${tamanho_clima} = "159M" ]; then
        echo ''
        echo 'Download da climatologia já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download da climatologia.'
        echo ''
        cd ../climatologia
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/datain/climatologia/" >> climatologia.log 2>&1
        echo ''
        echo 'Download concluido.'
        echo ''
fi


# Datas
datai=2023070100
dataf=2023070700
passo_analise=24
passo_previsao=24
total_previsao=120

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
pl_model_refer=WRF_cpt_07KM_SCAN_5levs

# Análises
arq_refer=${dir_data}/datain/TestWRF/%y4%m2%d200/WRF_cpt_07KM_SCAN_%y4%m2%d200_%y4%m2%d200.ctl

# Experimento
# Plugin experimento
pl_model_exper=WRF_cpt_07KM_SCAN_5levs

# Previsões
arq_prev=${dir_data}/datain/TestWRF/%y4%m2%d200/WRF_cpt_07KM_SCAN_%y4%m2%d200_%y4%m2%d200.ctl

# Climatologia
use_climatologia=1
arq_clim=${dir_data}/datain/climatologia/climatologia50yr.%mc.ctl
 
;;

[4])



#--------------------------------------------------------------------#
# Configuraçõees do TestCase BAM (NÃO ALTERAR!)                      #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE BAM  >>>>>>"

variavel=4

# diretório de saída
dir_test=TestBAM


# Verificação de existencia de arquivos
cd ${dir_data}/datain/TestBAM

tamanho_BAM=$(du -sh ${dir_data}/datain/TestBAM | awk '{print $1}')

tamanho_clima=$(du -sh ${dir_data}/datain/climatologia | awk '{print $1}')


if [ ${tamanho_BAM} = "9,7G" ]; then

        echo ''
        echo 'Download já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download do TestCase do modelo BAM. '
        echo ''
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/BAM_T666L64/" >> TestBAM.log 2>&1
	rm -r old
fi

if [ ${tamanho_clima} = "159M" ]; then
        echo ''
        echo 'Download já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download da climatologia.'
        echo ''
        cd ../climatologia
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/datain/climatologia/" >> climatologia.log 2>&1
        echo ''
        echo 'Download concluido.'
        echo ''

fi

# Datas
datai=2023070100
dataf=2023070700
passo_analise=24
passo_previsao=24
total_previsao=120

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
pl_model_refer=BAM_global_20km_SCAN_5levs

# Análises
arq_refer=${dir_data}/datain/TestBAM/%y4%m2%d200/BAM_global_20km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl

# Experimento
# Plugin experimento
pl_model_exper=BAM_global_20km_SCAN_5levs

# Previsões
arq_prev=${dir_data}/datain/TestBAM/%y4%m2%d200/BAM_global_20km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl

# Climatologia
use_climatologia=1
arq_clim=${dir_data}/datain/climatologia/climatologia50yr.%mc.ctl

;;



[5])

#--------------------------------------------------------------------#
# Configuraçõees da comparação dos TestCases (NÃO ALTERAR!)          #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE COMPARAÇÃO  >>>>>>"

# Diretório de saída
dir_test=TestCompara

#Variável para configuração do scantec.conf
variavel=5

# Verifica a existencia dos arquivos para utilizar na comparação

tamanho_BAM=$(du -sh ${dir_data}/datain/TestBAM | awk '{print $1}')
tamanho_ETA=$(du -sh ${dir_data}/datain/TestETA | awk '{print $1}')
tamanho_WRF=$(du -sh ${dir_data}/datain/TestWRF | awk '{print $1}')
tamanho_BRAMS=$(du -sh ${dir_data}/datain/TestBRAMS | awk '{print $1}')


tamanho_clima=$(du -sh ${dir_data}/datain/climatologia | awk '{print $1}')


if [ ${tamanho_ETA} = "4,0G" ]; then

        echo ''
        echo 'Download do modelo ETA já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download do TestCase do modelo ETA. '
        echo ''
        cd ${dir_data}/datain/TestETA
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/ETA/" >> TestETA.log 2>&1
	echo ''
        echo 'Download concluido.'
        echo ''

fi

if [ ${tamanho_WRF} = "5,4G" ]; then

        echo ''
        echo 'Download do modelo WRF já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download do TestCase do modelo WRF. '
        echo ''
        cd ${dir_data}/datain/TestWRF
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/WRF/" >> TestWRF.log 2>&1
	echo ''
        echo 'Download concluido.'
        echo ''

fi

if [ ${tamanho_BRAMS} = "5,0G" ]; then
	echo ''
	echo 'Download do modelo BRAMS já efetuado. Iniciando os trabalhos...'
	echo ''
else 
	echo ''
	echo 'Iniciando o download do TestCase do modelo BRAMS.'
	echo ''
	cd ${dir_data}/datain/TestBRAMS
	wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/BRAMS/" >> TestBRAMS.log 2>&1
	echo ''
	echo 'Download Concluído'
	echo ''
fi

if [ ${tamanho_BAM} = "9,7G" ]; then

        echo ''
        echo 'Download já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download do TestCase do modelo BAM. '
        echo ''
	cd ${dir_data}/datain/TestBAM
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/BAM_T666L64" >> TestBAM.log 2>&1
	echo ''
        echo 'Download concluido.'
        echo ''	
fi

if [ ${tamanho_clima} = "159M" ]; then
        echo ''
        echo 'Download da climatologia já efetuado. Iniciando os trabalhos...'
        echo ''
else
        echo ''
        echo 'Iniciando o download da climatologia.'
        echo ''
        cd ${dir_data}/datain/climatologia
        wget -r -np -nH -nc --cut-dirs=6 -R "index.html*" "http://ftp1.cptec.inpe.br/pesquisa/das/scantec/datain/climatologia/" >> climatologia.log 2>&1
        echo ''
        echo 'Download concluido.'
        echo ''
fi


# Datas
datai=2023070100
dataf=2023070700
passo_analise=24
passo_previsao=24
total_previsao=120

# Regiões
lat_low=-49.875
lon_low=-82.625
lat_up=11.375
lon_up=-35.375
dx=0.4
dy=0.4

# Quantidade de experimentos
quant_exp=4

# Referências
# Plugin modelo
pl_model_refer=WRF_cpt_07KM_SCAN_5levs
#pl_model_refer=era5_SCAN_5levs

# Análises
arq_refer=${dir_data}/datain/TestWRF/%y4%m2%d200/WRF_cpt_07KM_SCAN_%y4%m2%d200_%y4%m2%d200.ctl
#arq_refer=${dir_data}/datain/TestERA5/%y4%m2%d200/ERA5_SCAN_%y4%m2%d200_%y4%m2%d200.ctl

# Experimento
# Plugin experimento
pl_model_exper=WRF_cpt_07KM_SCAN_5levs
pl_model_exper1=BRAMS_ams_08km_SCAN_5levs
pl_model_exper2=Eta_ams_08km_SCAN_5levs
pl_model_exper3=BAM_global_20km_SCAN_5levs

# Previsões
arq_prev=${dir_data}/datain/TestWRF/%y4%m2%d200/WRF_cpt_07KM_SCAN_%y4%m2%d200_%y4%m2%d200.ctl
arq_prev1=${dir_data}/datain/TestBRAMS/%y4%m2%d200/BRAMS_ams_08km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl
arq_prev2=${dir_data}/datain/TestETA/%y4%m2%d200/Eta_ams_08km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl
arq_prev3=${dir_data}/datain/TestBAM/%y4%m2%d200/BAM_global_20km_SCAN_%y4%m2%d200_%y4%m2%d200.ctl


# Climatologia
use_climatologia=1
arq_clim=${dir_data}/datain/climatologia/climatologia50yr.%mc.ctl

;;

[6])

#--------------------------------------------------------------------#
# Configuraçõees TestCase MONAN (NÃO ALTERAR!)          #
#--------------------------------------------------------------------#
MODELCASE=" <<<<< TESTCASE MONAN  >>>>>>"

# Diretório de saída
dir_test=TestMONAN

#Variável para configuração do scantec.conf
variavel=5





# Datas
datai=2024021400
dataf=2024021900
passo_analise=24
passo_previsao=24
total_previsao=96

# Regiões
lat_low=-90.000000
lon_low=-180.0000
lat_up=90.0000
lon_up=179.0000
dx=0.4
dy=0.4

# Quantidade de experimentos
quant_exp=1

# Referências 
# Plugin modelo
pl_model_refer=MONAN_global_SCAN

# Análises
arq_refer=${dir_data}/datain/${dir_test}/MONAN_v1.1.0/%y4%m2%d200/global/MONAN_global_v1_%y4%m2%d200-%y4%m2%d200-reg.ctl
                                                                         #MONAN_global_v1_2024021400-2024021400-reg.ctl

# Experimento
# Plugin experimento
pl_model_exper=MONAN_global_SCAN

# Previsões
arq_prev=${dir_data}/datain/${dir_test}/MONAN_v1.1.0/%y4%m2%d200/global/MONAN_global_v1_%y4%m2%d200-%y4%m2%d200-reg.ctl


# Climatologia
use_climatologia=1
arq_clim=${dir_data}/datain/climatologia/climatologia50yr.%mc.ctl
i



;;

[7])
#--------------------------------------------------------------------#
# Configurações do usuário (ALTERAR O QUE FOR NECESSÁRIO)            #
#--------------------------------------------------------------------#

echo 'Em desenvolvimento'
exit

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
echo " <<< INICIANDO SCANTEC >>>                 "
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
echo " Plugin do experimento: ${pl_model_exper}  "
echo " ${pl_model_exper1}                       "
echo " ${pl_model_exper2}                       "
echo " ${pl_model_exper3}                       "
echo ""
echo " Previsões:                                "
echo " ${arq_prev}                               " 
echo " ${arq_prev1}                              "
echo " ${arq_prev2}                              "
echo " ${arq_prev3}                              "   
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

# Condição para a comparação dos TestCase

if [ ${variavel} -eq 6 ]; then
	linha_out="${pl_model_exper} WRF ${arq_prev}"
	linha_out1="${pl_model_exper1} BRAMS ${arq_prev1}"	
	linha_out2="${pl_model_exper2} ETA ${arq_prev2}"
	linha_out3="${pl_model_exper3} BAM ${arq_prev3}"
	linha_out4='::'	
else
	linha_out="${pl_model_exper} EXP01 ${arq_prev}"
	linha_out1='::'
fi

	
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
${linha_out}
${linha_out1}
${linha_out2}
${linha_out3}
${linha_out4}



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
  ./scantec.x 
  # >> ${ARQlog}
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
