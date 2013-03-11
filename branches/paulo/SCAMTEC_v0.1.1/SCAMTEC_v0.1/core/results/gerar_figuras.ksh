#!/bin/ksh
#
#Script para gerar graficos de Histograma no grads 
#Paulo Henrique Diniz Dias
#
#


ROOT_DIR=`pwd`
echo '------------- Parametros ----------------'
echo "::: Digite o valor Maximo do Histograma :::"
read histo_max

echo "::: Digite o valor Minimo do Histograma :::"
read histo_min

echo "::: Digite a Quantidade de Experimento :::"
read n_exp

exp=1

while [ ${exp} -le ${n_exp} ]
do
	echo "::: Digite o CTL do Experimento " ${exp} ":::"
	read ctl
	
	parametro="'run histograma.gs ${ROOT_DIR}/ ${ctl} ${histo_max} ${histo_min} ${exp}'"
	grads -blc ${parametro}
	exp=$((exp+1))
done



exit

parametro="'run histograma.gs ${ROOT_DIR}/ ${ctl} ${histo_max} ${histo_min} ${n_exp}'"



echo "::: Digite o CTL :::"
read ctl

echo "::: Digite o valor Maximo do Histograma :::"
read histo_max

echo "::: Digite o valor Minimo do Histograma :::"
read histo_min

echo "::: Digite o numero do Experimento :::"
read n_exp
 
parametro="'run histograma.gs ${ROOT_DIR}/ ${ctl} ${histo_max} ${histo_min} ${n_exp}'"
grads -blc ${parametro}

echo '\033[41;1;37m ::: Graficos gerados com sucesso :::\033[0m'

