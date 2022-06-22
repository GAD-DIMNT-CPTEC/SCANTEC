Neste diretório estão alocados dois scripts e um arquivo template do SCANTEC. Estes scripts podem ser utilizados para submeter o SCANTEC para a avaliação de múltiplos experimentos, tirando vantagem da submissão por array. Os scripts estão preparados para realizar as submissões para a avaliação no modo "série" em que dentro de uma período, são avaliados todos os passos de tempo disposníveis para a avaliação (eg., todos os dias às 00Z no período entre 2020060100 a 2020083100), e no modo "período" em que a avaliação é feita para um período (eg., 2020060100 a 2020083100). No primeiro modo, são escritas as tabelas do SCANTEC para cada passo de tempo dentro do período; no segundo modo, as tabelas do SCANTEC refletem as estatísticas calculadas para o período. Para o cálculo do teste de significância t-Student a partir do SCANPLOT, na forma como o script de plotagem está preparado, é necessário submeter o SCANTEC para obter as estatísticas em ambos os modos.

Nos scripts `run_scantec-series.sh` e `run_scantec-periodo.sh`, o usuário deverá fazer alterações nas variáveis:

* `bpath`: caminho que aponto para o local do script, que deverá conter também o arquivo template `scantec.conf.template`, além do executável `scantec.x`;
* `Exps`: lista com os nomes dos experimentos a serem avaliados;
* `Regs`: lista com os nomes das regiões a serem avaliadas;
* `inctime`: caminho para o executável do inctime;
* `data_i`: data inicial da avaliação;
* `data_f`: data final da avaliação.

Dentro do loop principal, realizado entre as datas `data_i` e `data_f`, o usuário deverá criar os blocos correspondentes aos seus experimentos. Os experimentos listados servem de exemplo. As variáveis destes blocos que devem ser ajustadas são:

* `MODELTABLE`: nome da tabela descritora do modelo a ser avaliado (vide diretório tables da instalação do SCANTEC). Note que a extensão `.table` não é necessária;
* `FILENAME`: caminho completo para o nome dos arquivos descritores `ctl` das previsões do modelo a ser avaliado. Note que o caminho e o nome do arquivo contém máscaras para as datas, i.e., `%iy4%im2%id2%ih2` (para a data de análise) e `y4%fm2%fd2%fh2` (para a data de previsão);
* `ICNFILENAME`: idem `FILENAME`, mas para os arquivos de análise;
* `FCTOTAL`: tempo total de previsões disponíveis do experimento, em horas.

Ainda no script, mais adiante há um bloco onde são definidos os limites das regiões a serem avaliadas. O usuário deverá alterar estes valores de acordo com a sua necessidade.

No arquivo template `scantec.conf.template`, o usuário deverá alterar os seguintes parâmetros, de acordo com a sua necessidade:

* `Analisys Time Step`: passo de tempo entre as análises dos seus experimentos;
* `Forecast Time Step`: passo de tempo entre as previsões dos seus experimentos;
* `scantec tables`: diretório onde podem ser encontradas as tabelas do SCANTEC, de acordo com a sua instalação;
* `run domain resolution dx`: resolução da grade em x;
* `run domain resolution dy`: resolução da grade em y;
* `Climatology Model Name`: nome da tabela descritora da climatologia a ser utilizada;
* `Climatology file`: diretório e nome completo do arquivo descritor da climatologia a ser utilizada. Note a máscara utilizada para a descrição do mês (`%mc`).

Finalmente, o usuário deverá escolher a referência utilizada na avaliação dos experimentos. No exemplo, cada previsão é avaliada contra a sua própria análise. Para alterar isso, basta trocar o valor do parâmetro "Reference file" nos scripts run_scantec-series.sh ou run_scantec-periodo.sh.

Depois de feitas as modificações, para utilizar os scripts, basta executar:

```
$ nohup ./run_scantec-series.sh > log &
````

ou

```
$ nohup ./run_scantec-periodo.sh > log &
````

carlos.bastarz@inpe.br (13 de Julho de 2021)
