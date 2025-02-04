#! /bin/bash


## Parte 1 Script que transforma dados .nc em dados .nc menor
## Altere os caminhos que precisar

inctime=${HOME}/bin/inctime

dia_inicio=14
dia_final=23
dias_previsao=5


for data in $(seq $dia_inicio $dia_final); do
    final_data=$((data + dias_previsao))

## diretório onde estão os dados brutos
    datain="/share/das/dist/victor.ranieri/MONANexp/MONAN_v1.1.0/202402${data}00/"
    dataout="/share/das/dist/victor.ranieri/MONANexp/MONAN_v1.1.0/global/202402${data}00/"
    dir_script="/share/das/dist/victor.ranieri/MONANexp/MONAN_v1.1.0/"
#
## Nome do arquivo em .nc
    file_name="MONAN_DIAG_G_POS_GFS_202402${data}00.00.00.x1024002L55.nc"

## Criação do diretório e log
    nome_dir=$(echo "$file_name" | awk -F'[_.]' '{print $6}')


## lats4d.sh ajuda a recortar o dado bruto em dados menores 

    echo "Fazendo o recorte para o arquivo ${file_name} com recorte global (?)"

    fout=${dataout}MONAN_v1_global_202402${data}00
        
        # América do sul com as variaveis rainc rainnc mslp u10 v10, lat/lon -60 15 -lon 270 330
        # 
 
    #lats4d.sh -v -i ${datain}${file_name} -o ${fout} -ftype sdf -format netcdf -time 00Z{data}FEB2024 00Z${final_data}FEB2024 -lat -60 15 -lon 270 330 -vars rainc rainnc mslp u10 v10

         #Global 

    lats4d.sh -v -i ${datain}${file_name} -o ${fout} -ftype sdf -format netcdf -time 00Z{data}FEB2024 00Z${final_data}FEB2024 -vars uzonal umeridional zgeo relhum temperature mslp -levs 250 500 850 925 1000 

## Parte 2 

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



# Definindo as datas de início e fim do periodo
dataINI=2024021400
dataFIM=2024022300
ndia=5                  # Numero de dias de integração de cada rodada
nhor=24                 # Numero de horas entre cada saida avaliada
nhan=24                 # Numero de horas entre as analises avaliadas
area=global             # Area onde o modelo está sendo avaliado

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

  data=${dataANA}
  dataf=$(${inctime} ${dataANA} +${ndia}d %y4%m2%d2%h2)  

  cd ${dir_script}${area}${dataANA}

  # Loop para iterar sobre as datas
  while [ ${data} -le ${dataf} ]; do
        echo
	echo ">>>>>>>>>>>>>>>>>> Processando a previsão : "${data}
        ls 
        # Chama a função para formatar a data
        data_fmt ${data}

        fout=MONAN_global_v1_${datai}-${data}

        # Executa o comando nohup para processar os dados
       nohup lats4d.sh -v -i MONAN_v1_global_${data}.nc -time ${datafmt} ${datafmt} -o ${fout} -ftype sdf -format netcdf 
#> ${data}.log &
	
	
 	# Incrementa a data
        data=$(${inctime} ${data} +${nhor}h %y4%m2%d2%h2)  
	
  done
  cp ${dir_script}grade.txt .
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

  cd ${dir_script}
 
done
