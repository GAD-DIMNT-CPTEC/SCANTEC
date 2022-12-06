#  Uso

Para utilizar o SCANTEC é preciso editar um namelist (arquivo de configurações) localizado em `SCANTEC.2.1.0/core/scantec.conf` e modificar as informações para adaptar para os dados do usuário salvando a versão modificada no diretorio `SCANTEC.2.1.0/bin`. Para edição do arquivo de configuração `scantec.conf`, há um conjunto de palavras-chave que antecedem a informação requerida pelo sistema (para mais informações sobre o namelist do SCANTEC, veja a página [Namelist](namelist.md)). O usuário deve procurar por essas palavras-chave e atribuir os valores desejados. 

1. Para utilizar o sistema entre no diretório `bin` do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC.2.1.0/bin/
        ```

2. Edite o arquivo `SCANTEC.2.1.0/core/scantec.conf` e modifique apropriadamente as informações solicitadas e salve a versão modificada no diretório `SCANTEC.2.1.0/bin` (no exemplo, está sendo utilizado o editor `vi`, mas o usuário pode utilizar o editor que melhor lhe convier):

    === "Comando"
        ```bash linenums="1"
        cp ../core/scantec.conf scantec.conf
        vi scantec.conf
        ```

3. Execute o SCANTEC com o comando (na aba **Resultado** a seguir, é apresentado apenas um exemplo do que o SCANTEC fará quando configurado para a avaliação de um período específico):

    === "Comando"
        ```bash linenums="1"
        ./scantec.x
        ```
    === "Resultado"
        ```
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         !                         Climatology Not Found                       !
         !         The mean reference field will be used as climatology        !
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
         !                          Running scantec                            !
         !     Please wait while the system is performing the statistics       !
         !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
           Analisys   Forecast        fct
         2014080500 2014080500        00h
         2014080500 2014080600        24h
         2014080500 2014080700        48h
         2014080500 2014080800        72h
         2014080512 2014080512        00h
         2014080512 2014080612        24h
         2014080512 2014080712        48h
         2014080512 2014080812        72h
         2014080600 2014080600        00h
         2014080600 2014080700        24h
         2014080600 2014080800        48h
         2014080600 2014080900        72h
        ```

4. De forma mais simplificada, pode-se utilizar o script `run_scantec.sh` que modifica apropriadamente o arquivo `scantec.conf`, chama o `scantec.x` e armazenas as informações em um arquivo de log. Esse mesmo script tem uma série de Testcases para permitir a validação da versão instalada pelo usuário:

    === "Comando"
        ```bash linenums="1"
        ./run_scantec.sh
        ```
    === "Resultado"
        ```
        Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC)
        
        A opção TestCase não foi informada!
        
        Uso:
        ./run_scantec.sh 1 - TestCase do BRAMS (Jan/2016)
        ./run_scantec.sh 2 - TestCase do ETA   (Abr/2020)
        ./run_scantec.sh 3 - TestCase do BAM   (Ago/2014)
        ./run_scantec.sh 4 - dados definidos pelo usuário
        ```

### Execução do SCANTEC com os dados do Testcase

Os dados de entrada do Testcase para as 3 opções de modelos disponíveis (i.e., BRAMS, ETA e BAM) estão no disco NetApp em `/dados/das/pesq1/public/SCANTEC`. Dentro do ambiente de computação do CPTEC, esses dados podem ser utilizados para testes e são acessíveis a partir da máquina Tupã e das máquinas virtuais como a Itapemirim, Ilopolis, Colorado entre outras que enxergam o disco NetApp.

1. Para utilizar o sistema (tanto no Tupã como nas máquinas virtuais) entre no diretório raiz da instalação do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC.2.1.0/
        ```

2. Execute o script de execução do SCANTEC com um parâmetro na linha de comando:

    === "Comando"
        ```bash linenums="1"
        ./run_scantec.sh [parâmetro]
        ```

    * Sendo esse parâmetro uma das seguintes opções:

        - Testcase do BRAMS (Jan/2016);
        - Testcase do ETA (Abr/2020);
        - Testcase do BAM (Ago/2014);
        - dados definidos pelo usuário.

Para cada uma dessas opções o script irá criar um novo namelist `SCANTEC.2.1.0/bin/scantec.conf`, onde serão adicionadas as informações necessárias para cada um dos experimentos, i.e., o período dos dados, o intervalo entre as análises, o intervalo entre as previsões e o período de integração dos modelos. O formato dos arquivos disponíveis para os testes é determinado em arquivos alocados no diretório `tables`. Para cada novo modelo ou versão com diferente resolução ou domínio, novos arquivos `*.table` devem ser disponibilizados no diretório `SCANTEC.2.1.0/tables`. Para mais informações sobre como adicionar outros modelos, veja a seção intitulada [**Adicionando outras versões ou modelos no SCANTEC**](#adicionando-outras-versoes-ou-modelos-no-scantec).

As informações de saída dos testcases do SCANTEC são escritas no diretório `SCANTEC.2.1.0/dataout/TestMODEL` onde `MODEL` pode ser `BRAMS`, `ETA` ou `BAM`, dependendo da opção escolhida acima. Por exemplo:

```
ls -ltr SCANTEC.2.1.0/dataout/TestMODEL
total 17292
-rw-rw-r-- 1 user group    1154 Jun 28 17:20 RMSEEXP01_20140805002014080600F.ctl
-rw-rw-r-- 1 user group    1154 Jun 28 17:20 VIESEXP01_20140805002014080600F.ctl
-rw-rw-r-- 1 user group    1154 Jun 28 17:20 MEANEXP01_20140805002014080600F.ctl
-rw-rw-r-- 1 user group    1055 Jun 28 17:22 RMSEEXP01_20140805002014080600T.scan
-rw-rw-r-- 1 user group    1055 Jun 28 17:22 VIESEXP01_20140805002014080600T.scan
-rw-rw-r-- 1 user group    1055 Jun 28 17:22 ACOREXP01_20140805002014080600T.scan
-rw-rw-r-- 1 user group 5864960 Jun 28 17:22 RMSEEXP01_20140805002014080600F.scan
-rw-rw-r-- 1 user group 5864960 Jun 28 17:22 VIESEXP01_20140805002014080600F.scan
-rw-rw-r-- 1 user group 5864960 Jun 28 17:22 MEANEXP01_20140805002014080600F.scan
```

No exemplo acima, são identificados os seguintes tipos de arquivos, todos resultantes de uma execução do SCANTEC:

* `RMSEEXP01_20140805002014080600F.ctl`: arquivo ascii descritor para o arquivo `RMSEEXP01_20140805002014080600F.scan`. Pode ser aberto no software GrADS;
* `RMSEEXP01_20140805002014080600F.scan`: arquivo binário com a distribuição espacial da estatística RMSE (Root Mean Square Error, ou Raiz do Erro Quadrático Médio);
* `VIESEXP01_20140805002014080600T.scan`: arquivo ascci com uma tabela com o resultado do RMSE calculado para cada uma das variáveis escolhidas, por tempo de previsão para o período escolhido.

### Executando o SCANTEC com dados do usuário

Para executar o script `run_scantec.sh` com as informações inseridas pelo usuário, escolha a opção `4`. Mas antes, é preciso editar o script e modificar apropriadamente algumas informações. Veja a seguir:

1. Para utilizar o sistema (tanto no Tupã como nas máquinas virtuais) entre no diretório raiz da instalação do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC.2.1.0/
        ```

2. Edite o script `run_scantec.sh`:

    === "Comando"
        ```bash linenums="1"
        vi run_scantec.sh
        ```

    Modifique apropriadamente os grupos de variáveis (Datas, Regiões, Referências, Análises, Quantidade de experimentos, Plugin experimento, Previsões e Climatologia), conforme a seguir:

    ```
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
    
    # Plugin experimento
    pl_model_exper=prefixo_tabela_modelo/arquivo_fct.ctl
    
    # Previões
    arq_prev=diretorio_dados
    
    # Climatologia
    use_climatologia=0
    arq_clim=diretorio_climatologia/climatologia50yr.%mc.ctl

    ```

3. Em seguida, salve as modificações no script `run_scantec.sh` execute-o com a opção `4`:

    === "Comando"
        ```bash linenums="1"
        ./run_scantec.sh 4
        ```
    === "Resultado"
        ```
        <<< INICIANDO SCANTEC >>>        
        
        
         Configurações da avaliação:               
         ==========================                
         Data inicial:   2016010100                  
         Data final:     2016010500                  
         Passo análise:  12          
         Passo previsão: 12         
         Total previsão: 120         
        
         Região:                                   
         Lat low: -49.875                       
         Lon low: -82.625                       
         Lat up:  11.375                        
         Lon up:  -35.375                        
        
         Quantidade de experimentos: 1  
        
         Num. plugin referência: prefixo_tabela_modelo 
         Análise:                                  
         diretorio_dados/arquivo_anl.ctl                              
        
         Num. plugin experimento: prefixo_tabela_modelo/arquivo_fct.ctl
         Previsões:                                
         diretorio_dados                               
        
         Uso climatologia: 0     
        
         Resultados:                               
         /scripts/ensemble/SCANTEC.2.1.0/dataout/                          
        
         Arquivo de log:                           
         Log do processo: /scripts/ensemble/SCANTEC.2.1.0/logfile/scantec-20220628.17.30.log                
         ==========================                
        
         Início do processamento: Tue Jun 28 17:30:01 GMT 2022
        
         Criando o arquivo de configurações em bin/scantec.conf
        
         Arquivo de configuracao criado com sucesso.
        
         Executando o scantec.x ...
        
         . . . 

         Final do processo: Tue Jun 28 17:30:01 GMT 2022
        
         Log do processo: /scripts/ensemble/SCANTEC.2.1.0/logfile/scantec-20220628.17.30.log
        ====================================================================================
        
         Fim do processo!
        ```

4. Para verificar os resultados de saída do SCANTEC, liste os arquivos do diretório `SCANTEC.2.1.0/dataout`:

    === "Comando"
        ```bash linenums="1"
        ls SCANTEC.2.1.0/dataout
        ```

!!! note "Nota"

    Para visualizar os resultados gerados pelo SCANTEC, pode-se utilizar softwares como o GrADS (para visualização da distribuição espacial dos campos) e o GNUPlot (para a plotagem das tabelas). A partir da versão SCANTEC V2.1.0, recomenda-se a utilização do SCANPLOT (veja mais detalhes na página [Visualização de resultados usando o SCANPLOT](scanplot.md) desse manual).

## Adicionando outras versões ou modelos no SCANTEC

Para adicionar uma nova versão do modelo na lista das opções em que o sistema está preparado para processar (i.e., BRAMS, ETA, BAM), siga as instruções descritas nessa seção. Inicialmente, verifique se a versão desejada já não está implementada no sistema. Caso não esteja, para incluir um novo modelo, ou versão, crie um novo arquivo com a extensão `.model` dentro do diretório `SCANTEC.2.1.0/tables`, com as informações pertinentes à versão do modelo a ser utilizado.

!!! note "Nota"

    * Observe que modelos com resoluções, recortes ou domínio diferentes, ou mesmo com modificações no número de níveis pós-processados, requerem ajustes para que o sistema seja capaz de ler os arquivos binários;
    * Cabe salientar que apenas arquivos binários (`*.bin`) e GRIB1 (`*.grb`) são lidos pela atual versão do sistema. Arquivos no formato GRIB2 e NetCDF não são suportados pela versão SCANTEC V2.1.0. Caso o modelo que deseja adicionar não esteja nesses formatos, estes precisam ser convertidos para binário ou GRIB1 com o auxílio do script [`lats4d`](http://opengrads.org/doc/scripts/lats4d/) ou similares. 

A lista abaixo, elenca os modelos já implementados na versão SCANTEC V2.1.0 junto com os seus respectivos arquivos `tables`, os quais podem servir como exemplo para criar outros:

* Modelo BAM truncamento 299 64 níveis com pós de 18 níveis: `BAM_TQ0299L064_18levs.model`;
* Modelo BAM truncamento 299 64 níveis com pós de 28 níveis: `BAM_TQ0299L064_28levs.model`;
* Modelo BRAMS 5km de resolução horizontal e pós de 19 níveis: `BRAMS_5km_19levs.model`;
* Modelo ETA 5km de resolução horizontal e pós de 22 níveis: `ETA_ams_05km_22levs.model`;

Para adicionar um novo modelo basta editar um dos arquivos acima que mais se assemelha com o modelo desejado e fazer os devidos ajustes. Salve o arquivo modificado com um nome apropriado (incluindo a extensão `.model`) dentro do diretório `tables`, e no arquivo de configurações `scantec.conf` (ou se tiver usando o script `run_scantec.sh`), assegure-se de que o novo modelo ou versão seja lido a partir do arquivo `table` criado. Para isso na linha do experimento em que esse modelo se refere a primeira palavra deve ser o nome do arquivo `table`.

No script `run_scantec.sh`:

```
pl_model_refer=BAM_TQ0299L064_18levs
```

ou, no arquivo namelist do SCANTEC:

```
BAM_TQ0299L064_18levs EXP01
```

!!! warning "Atenção"

    * Se esse arquivo é também utilizado como referência na avaliação o novo `table` deve também ser colocado após a palavra `Reference Model Name:` no arquivo de configurações do SCANTEC:

    ```
    Reference Model Name: BAM_TQ0299L064_18levs
    ```

O novo arquivo `table` deve conter as seguintes informações:

* Tipo de arquivo depois da palavra `ftype:`;
* Como os valores devem ser considerados indefinidos depois da palavra `undef:`;
* Dimensões da grade na longitude depois da palavra `xdim:`;
* Dimensões da grade na latitude depois da palavra `ydim:`;
* Número de níveis verticais do pós e a lista deles depois da palavra `zdim:`;
* Por fim uma tabela de variáveis depois da palavra `vars:`.

Veja exemplo do arquivo `ETA_ams_05km_22levs.model`:

```
ftype: grib
undef: 1e+20
xdim: 1162 linear -84.099998 0.050000
ydim: 1320 linear -51.000000 0.050000
zdim:
22 levels 1020 1000 950 925 900 850 800 750 700 650 600 
           550  500 450 400 350 300 250 200 150 100 50
```

Outro arquivo importante para a customização do SCANTEC, é o arquivo `scantec.vars`. Este arquivo contém a tabela de variáveis a serem utilizadas na avaliação. A tabela de variáveis é preenchida da seguinte forma (colunas separadas por espaço):

* A primeira coluna é o nome da variável do SCANTEC, veja lista em `tables/scantec.vars`;
* A segunda coluna pode ser o nome da variável correspondente no modelo da forma como listado no arquivo descritor (`.ctl`) do modelo.

Veja exemplo do arquivo `scantec.vars`:

```
vars:
vtmp:925 vtmp2(temp:925,umes:925)
vtmp:850 vtmp2(temp:850,umes:850)
vtmp:500 vtmp2(temp:500,umes:500)
temp:850 temp:850
temp:500 temp:500
temp:250 temp:250
psnm:000 pslm:1020
umes:925 umes:925
umes:850 umes:850
umes:500 umes:500
agpl:925 agpl:1020
zgeo:850 zgeo:850
zgeo:500 zgeo:500
zgeo:250 zgeo:500
uvel:850 uvel:850
uvel:500 uvel:500
uvel:250 uvel:250
vvel:850 vvel:850
vvel:500 vvel:500
vvel:250 vvel:250
```

## Funções Matemáticas Implícitas

A edição do arquivo `scantec.vars` permite a utilização de funções matemáticas para a definição de novas variáveis. Caso o modelo não tenha a variável que o SCANTEC requer (ou que se deseja avaliar), uma função matemática pode ser chamada ao colocar na segunda coluna algumas das funções disponiveis no SCANTEC, utilizando para isto, nas variáveis pós-processadas do modelo. Estas funções são escritas entre parênteses depois do nome da função e são separadas por vírgulas.

Veja o exemplo a seguir onde a variável `vtmp` (temperatura virtual) é calculada a partir das variáveis `temp` (temperatura absoluta) e `umes` (umidade específica):

```
vars:
vtmp:925 vtmp2(temp:925,umes:925)
vtmp:850 vtmp2(temp:850,umes:850)
vtmp:500 vtmp2(temp:500,umes:500)
```

As seguintes funções matemáticas estão disponíveis na versão SCANTEC V2.1.0:

* Funções Trigonométricas:
    - `sin(x)`: seno de x;
    - `cos(x)`: cosseno de x;
    - `tan(x)`: tangente de x;
    - `asin(x)`: arco seno de x;
    - `acos(x)`: arco cosseno de x;
    - `atan(x)`: arco tangente de x.


* Funções Aritméticas:

    - `sqrt(x)`: raiz quadrada de x;
    - `exp(x)`: exponencial de x;
    - `log(x)`: logarítmo natural de x;
    - `log10(x)`: logarítmo comum (na base 10) de x.


* Funções Numéricas:

    - `abs(x)`: retorna o valor absoluto de x;
    - `min(x,y)`: retorna o menor valor entre x e y;
    - `max(x,y)`: retorna o maior valor entre x e y;
    - `mod(x,y)`: rRetorna o resto da divisão de x por y.


* Funções para conversões físicas:

    - `svap(temp)`: calcula a pressão de vapor saturado [Pa] a partir da temperatura do ar (C);
    - `vapp(es, rh)`: calcula a pressão de vapor [Pa] a partir da pressão de vapor saturado [Pa] e da umidade relativa [%];
    - `hmxr1(q)`: calcula a razão de mistura [kg/kg] a partir da umidade específica [kg/kg];
    - `hmxr2(p, ee)`: calcula a razão de mistura [kg/kg] a partir da pressão atmosférica [Pa] e da pressão de vapor [Pa];
    - `umes1(w)`: calcula a umidade específica [kg/kg] a partir da razão de mistura [kg/kg];
    - `umes2(p, td)`: calcula a razão de mistura [kg/kg] a partir da pressão atmosférica [Pa] e da temperatura do ponto de orvalho [C];
    - `umes3(p, t, rh)`: calcula a razão de mistura [kg/kg] a partir da pressão atmosférica [Pa], da temperatura do ar (C) e da umidade relativa do ar [%];
    - `tpor(t, rh)`: calcula a temperatura do ponto de orvalho [C] a partir da temperatura do ar [C] e da umidade relativa do ar [%];
    - `umrl(p, w, es)`: calcula a umidade relativa [%] a partir da razão de mistura [kg/kg], da pressão de vapor de saturação [Pa] e da pressão atmosférica [Pa];
    - `vtmp1(p, t, rh)`: calcula a temperatura virtual [C] a partir da temperatura do ar [C] e da umidade relativa [%] e da pressão atmosférica [Pa];
    - `vtmp2(t, q)`: calcula a temperatura virtual [C ou K] a partir da temperatura do ar [C ou K] e da Umidade especícia [Kg/Kg];

!!! note "Nota"

    A implementação das funções matemáticas no SCANTEC, está nas rotinas [MathExpress.f90](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/blob/master/core/MathExpress.f90) e [scan_MathPlugin.f90](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/blob/master/core/scan_MathPlugin.f90).
