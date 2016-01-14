#!/bin/ksh
#
#Script para gerar graficos de Histograma no grads 
#Paulo Henrique Diniz Dias
#
#

grads -blc "run histograma.gs /home2/paulo.henrique/SCAMTEC_IWV_PSAS/paulo_add_modelo/SCAMTEC_v0.1/core/results/ histo.bin.ctl 4500 350"
#mv Histograma_EXP01_*.eps /home2/paulo.henrique/SCAMTEC_IWV_PSAS/paulo_add_modelo/SCAMTEC_v0.1/core/results/imagens_EXP01
#mv Histograma_EXP02_*.eps /home2/paulo.henrique/SCAMTEC_IWV_PSAS/paulo_add_modelo/SCAMTEC_v0.1/core/results/imagens_EXP02

echo '\033[41;1;37m ::: Graficos gerados com sucesso :::\033[0m'
