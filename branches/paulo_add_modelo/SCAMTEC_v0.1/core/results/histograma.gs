*****************************************************************************************
*                                                                                       *
*     Rotina operacional para gerar arquivos gifs para a                                *
* apresentacao dos resultados de histograma da precipitacao                             *
*                                                                                       *
*Usage:> grads -blc "run histograma.gs /path/ namefile.ctl max_range_p1 max_range_p2 "  *
*                                                                                       *
*                       Versao 1.0                                                      *
*                                                                                       *
*      Luiz Fernado Sapucci / Paulo Henrique Diniz Dias                                 *
*   Divisao de modelagem e desenvolvimento do DMD-CPTEC-INPE                            *    
*                                                                                       *
*****************************************************************************************
*                                                                                       *
*****************************************************************************************
*  Procedimento iniciais: carrega as referencias de tempo                               *   
*****************************************************************************************
*
function getarg(arg)
'reinit'
pathin=subwrd(arg,1)
arquivo=subwrd(arg,2)
max_range_p1=subwrd(arg,3)
max_range_p2=subwrd(arg,4)

*arquivo=sublin(ret,2)
ano=substr(arquivo,16,4)
mes=substr(arquivo,20,2)
say 'Ano da avaliacao: 'ano
say 'Mes da avaliacao: 'mes
say 'Path do  arquivo: ' pathin
say 'Nome do  arquivo: ' arquivo
*
**************************************************************
*              Configuraçoes iniciais do grads              *
**************************************************************
*
 'open 'pathin''arquivo''
 'set grads off'
 'set display color white'
* Dividindo a tela em 3x3
 
 rc = gsfallow("on")
 rc = panels(3' '3)

say 


*
**************************************************************
*           Loop para gerar os gifs do Histograma            *
**************************************************************
*

* Loop dos experimentos
exp=1
while(exp<=4)


* Loop para variar o tempo 
esta=1
 while(esta<=105)
 
*loop para divir em dois
part=1
 while(part<=2) 
 
 
 'c'
  'set strsiz 0.10'
 'set parea 1.5 10.5  1.0  6.3' 
 'set t '%esta
 
* Pegando o volor de t do terminal
 time=subwrd(result,4)

 

  
  if(exp=1)
  nome_saida='Histograma_EXP01_'
  titulo='draw string 3.0 8 EXPERIMENTO 01 - Convencional'
  endif
  if(exp=2)
  nome_saida='Histograma_EXP02_'
  titulo='draw string 3.0 8 EXPERIMENTO 02 - Convencional + SSMI + AIRS'
  endif
  if(exp=3)
  nome_saida='Histograma_EXP03_'
  titulo='draw string 3.0 8 EXPERIMENTO 03 - Convencional + AIRS'
  endif
  if(exp=4) 
  nome_saida='Histograma_EXP04_'
  titulo='draw string 3.0 8 EXPERIMENTO 04 - Convencional + SSMI'  
  endif
  
  
 
* Escrevendo o nome do arquivo de saida
 saida1='estat.prn'
 if(esta=105)
  saida2=nome_saida'MEDIA_part'%part'.eps' 
 else
  saida2=nome_saida%time'_part'%part'.eps'
  
 endif
 
*
 'enable print 'pathin''saida1
 
 
 
* Configuracao do Grads e plots  
**************************************************************
**************** Titulo da figura ****************************
**************************************************************

 if(esta=105)
 'set string 2 l 3 0'
 titulo' (MEDIA do MES) Parte '%part

 'set string 2 l 3 90'
 'draw string 0.1 3.2 Frequencia de Pontos' 
 
 'set string 2 l 3 0'
 'draw string 5 0.1 Classe de Precipitacao'
 else
 'set string 2 l 3 0'
 titulo' (DIA '%time') Parte '%part

 'set string 2 l 3 90'
 'draw string 0.1 3.2 Frequencia de Pontos' 
 
 'set string 2 l 3 0'
 'draw string 4.5 0.1 Classe de Precipitacao'
 endif
**************************************************************


hora=0 
plot=1
 while(hora<=48)
  
* Plotagem do histograma 
 _vpg.plot
 'set lon '%hora 
 
 
 
 
 
 if(part=1)
 
 'set lat 1' 
 'd histo'%exp 
 classe1=subwrd(result,4)
 'set string 1 l 3 0' 
 'set strsiz 0.25'
 'draw string 3.5 5.5 Precipitacao igual a 0 = '%classe1 
 
 'set lat 2' 
 'd histo'%exp 
 classe2=subwrd(result,4)
 'set string 1 l 3 0' 
 'set strsiz 0.25'
 'draw string 3.5 5.0 Precipitacao >0 e <=2 = '%classe2 
 
 'set vrange 0 '%max_range_p1
 'set lat 3 17' 
 'set xaxis 4 32 2'
 else
*Figura Parte 2 
 'set lat 37' 
 'd histo'%exp 
 classe37=subwrd(result,4)
 'set string 1 l 3 0' 
 'set strsiz 0.25'
 'draw string 3.5 5.5 Precipitacao >70 = '%classe37 
  
 'set vrange 0 '%max_range_p2
 'set lat 18 36' 
 'set xaxis 34 70 2'
 endif 
  
 'set gxout bar'
 'set ccolor '%plot
 'set bargap 20' 
*Tamanho da fonte do eixo X e Y 
 'set xlopts 1 6 0.2'
 'set ylopts 1 6 0.2'
*
 'set grads off'
 'd histo'%exp
 if(hora=0)
 'draw title OBS' 
 else 
 'draw title '%hora'h'
 endif
  
   hora=hora+6
   plot=plot+1
 
*
* Fim do laco de hora
 endwhile
 
 

* Procedimentos finais para gerar o eps

 'print'
 'disable print'
 '!/usr/local/bin/gxeps -i 'pathin''saida1' -o 'pathin''saida2
 
 
 
 
  
 'set parea off'
 'set vpage off'
part=part+1
* Fim do laco de partes 
endwhile
 
 
 esta=esta+1
* Fim do laco de tempo 
endwhile

 exp=exp+1
* Fim do laco de experimento
endwhile

*
'quit'
