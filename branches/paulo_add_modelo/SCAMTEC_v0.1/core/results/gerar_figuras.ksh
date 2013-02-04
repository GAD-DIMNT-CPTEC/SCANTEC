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

echo "::: Digite o numero de experimentos :::"
read n_exp
 
parametro="'run histograma.gs ${ROOT_DIR}/ histo.bin.ctl ${histo_max} ${histo_min} ${n_exp}'"
 
grads -blc ${parametro}

echo '\033[41;1;37m ::: Graficos gerados com sucesso :::\033[0m'

