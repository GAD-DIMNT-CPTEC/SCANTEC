#! /bin/bash

#
#BOP
#----------------------------------------------------------------------------------#
# Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC) #
#----------------------------------------------------------------------------------#
#
# !DESCRIPTION:
# Script que transforma dados .nc do MONAN em dados .nc recortados e depois em formato grib
# Altere os caminhos que precisar. Essa configuração é a mesma que foi usada para criar o 
# testcase do MONAN disponivel em 
# https://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/MONAN/
#
# !CALLING SEQUENCE:
# ./run_scantec.sh 
#                  
# !Histórico de revisões: 
#      05/02/2025 - Victor Ranieri - versão inicial basweado no projeto PIBIC
#      07-02-2025 - Luiz Sapucci   - ajustes nos diretorio dados no scantec
#
# !Pre requesitos:
#      inctime no /home/bin
#      versao do grads com lat4d disponivel
#      Espaco em disco suficiente para seu periodo.
#
#EOP  
#----------------------------------------------------------------------------------#
#BOC

# Caso não tenha o inctime instalado obtenha-o em https://svn.cptec.inpe.br/inctime
inctime=${HOME}/bin/inctime

# Esse script precisa ser rodado no diretório scripts_uteis na arvore do scantec.
# Dados processados no datain
cd ../datain

## diretório onde estão os dados brutos 
##(copiado de /mnt/beegfs/luiz.sapucci/MONAN/scripts_CD-CT/dataout/yyyymmddhh/Post/)
datanc="/share/das/dist/victor.ranieri/MONANexp/MONAN_v1.1.0/"
dataout="MONAN/"

# Definindo as datas de início e fim do periodo
dataINI=2024021400
dataFIM=2024022300
ndia=5                  # Numero de dias de integração de cada rodada
nhor=24                 # Numero de horas entre cada saida avaliada
nhan=24                 # Numero de horas entre as analises avaliadas
area=global             # Area onde o modelo está sendo avaliado

# Criando o diretorio de saida para essa area
mkdir -p ${dataout}/${area}
cd ${dataout}/${area}

#Função para formato especifico do tempo para o lats4d
data_fmt() {

  data=${1}      
  yyyy=${data:0:4}
  mm=${data:4:2}
  dd=${data:6:2}
  hh=${data:8:2}
  if [ ${mm} -eq 01 ]; then nmm=JAN; fi
  if [ ${mm} -eq 02 ]; then nmm=FEB; fi
  if [ ${mm} -eq 03 ]; then nmm=MAR; fi
  if [ ${mm} -eq 04 ]; then nmm=APR; fi
  if [ ${mm} -eq 05 ]; then nmm=MAY; fi
  if [ ${mm} -eq 06 ]; then nmm=JUN; fi
  if [ ${mm} -eq 07 ]; then nmm=JUL; fi
  if [ ${mm} -eq 08 ]; then nmm=AUG; fi
  if [ ${mm} -eq 09 ]; then nmm=SEP; fi
  if [ ${mm} -eq 10 ]; then nmm=OCT; fi
  if [ ${mm} -eq 11 ]; then nmm=NOV; fi
  if [ ${mm} -eq 12 ]; then nmm=DEC; fi

  datafmt=${hh}Z${dd}${nmm}${yyyy}

}        

dataANA=$dataINI

while [ ${dataANA} -le ${dataFIM} ]; do
  echo  
  echo "%%%%%%%%Processando a data do periodo:"${dataANA}
  echo 

  mkdir -p ${dataANA}
  cd ${dataANA}

  data=${dataANA}
  dataf=$(${inctime} ${dataANA} +${ndia}d %y4%m2%d2%h2)  

  # Chama a função para formatar a data
  data_fmt ${dataf}
  dataffmt=${datafmt}
  data_fmt ${dataANA}

  ## Nome do arquivo em .nc
  file_name="MONAN_DIAG_G_POS_GFS_${dataANA}.00.00.x1024002L55.nc"


  ## lats4d.sh e usado aqui para recortar o dado bruto em dados menores 

  echo "Fazendo o recorte para o arquivo ${file_name} com recorte " $area

  fout=MONAN_v1_${area}_${dataANA}
        
  lats4d.sh -v -i ${datanc}/${dataANA}/${file_name} -o ${fout} -ftype sdf -format netcdf -time $datafmt  $dataffmt -vars uzonal umeridional zgeo relhum temperature mslp -levs 250 500 850 925 1000 

  # América do sul com as variaveis rainc rainnc mslp u10 v10, lat/lon -60 15 -lon 270 330
  # Para o recorte da américa do sul descomente a linha abaixo mude  $area para "AS"    

  #lats4d.sh -v -i ${datain}${file_name} -o ${fout} -ftype sdf -format netcdf -time 00Z{data}FEB2024 00Z${final_data}FEB2024 -lat -60 15 -lon 270 330 -vars rainc rainnc mslp u10 v10

  # configuração do regrid requerido

cat << EOF > grade.txt  
gridtype = lonlat
xsize    = 360
ysize    = 180
xfirst   = -180.0
xinc     = 1.0
yfirst   = -90
yinc     = 1.0
EOF

  # Loop para iterar sobre as datas
  while [ ${data} -le ${dataf} ]; do
        echo
	echo ">>>>>>>>>>>>>>>>>> Processando a previsão : "${data}
        ls 
        # Chama a função para formatar a data
        data_fmt ${data}

        fout=MONAN_global_v1_${dataANA}-${data}

        # Executa o comando nohup para processar os dados
       nohup lats4d.sh -v -i MONAN_v1_global_${dataANA}.nc -time ${datafmt} ${datafmt} -o ${fout} -ftype sdf -format netcdf 

#> ${data}.log &
	
	
 	# Incrementa a data
        data=$(${inctime} ${data} +${nhor}h %y4%m2%d2%h2)  
	
  done

  for file in MONAN_global_v1*.nc; do
    echo "$file"

    filein=${file}
    
    filein_reg=$(basename "${filein}" .nc)
    if [[ ! "$filein_reg" =~ -reg$ ]]; then
      filein_reg="${filein_reg}-reg"
    fi
    
    cdo remapbil,grade.txt "${filein}" "${filein_reg}.nc"
    lats4d.sh -v -i "${filein_reg}.nc" -o "${filein_reg}" -ftype sdf -format grads_grib
	        
   sed -i 's/temperature/temp/'  ${filein_reg}.ctl
   sed -i 's/umeridional/vwnd/'  ${filein_reg}.ctl
   sed -i 's/uzonal/uwnd/'  ${filein_reg}.ctl
   sed -i 's/relhum/Urel/'  ${filein_reg}.ctl

  done 

  dataANA=$(${inctime} ${dataANA} +${nhan}h %y4%m2%d2%h2)  

  cd ..
 
done
