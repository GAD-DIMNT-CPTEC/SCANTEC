#  Uso

Para utilizar o SCANTEC é preciso editar um arquivo de configurações localizado em `SCANTEC.2.0.0/core/scantec.conf` e modificar as informações para adaptar para os dados do usuário salvando a versão modificada no diretorio `SCANTEC.2.0.0/bin`. Entre nesse diretório `bin`, e acione o executavel `scantec.x`, gerado na compilação. Para edição do arquivo de configuração `scantec.conf`, há um conjunto de palavras chaves que antecedem a informação requerida pelo sistema. O usuário procura essas palavras chaves e armazena nas devidas variáveis a informação constante após essas palavras chaves. Veja a lista de informações requeridas e as respectivas palavras chaves no final desta página, com uma breve descrição de cada uma dessas informações.

1. Para utilizar o sistema entre no diretório bin do SCANTEC:

    === "Comando"
        ```
        cd SCANTEC.2.0.0/bin/
        ```

2. Edite o arquivo `SCANTEC.2.0.0/core/scantec.conf` e apropriadamente modifique as informações solicitadas sem modificar as palavras chaves e salve a versão modificada no diretório `SCANTEC.2.0.0/bin`:

    === "Comando"
        ```
        cp ../core/scantec.conf scantec.conf
        vi scantec.conf
        ```

3. Execute o SCANTEC com o comando (na aba **Resultado**, é apresentado apenas um exemplo do que o SCANTEC fará quando configurado para a avaliação de um período particular):

    === "Comando"
        ```
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

4. De forma mais simplificada e mais versátil, pode-se utilizar o script `run_scantec.sh` que modifica o arquivo `scantec.conf` apropriadamente, chama o `scantec.x` e armazenas as informações em um arquivo de log. Esse mesmo script tem uma série de Testcases para permitir a validação da versão instalada pelo usuário:

    === "Comando"
        ```
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

Como os dados de entrada do testcase, de todas as 3 opções de modelos disponíveis estão no disco NetApp em `/dados/das/public/SCANTEC/TestCase/`, esses testes funcionam tanto no Tupã como nas máquinas virtuais, Itapemirim e outras que enxergam o NetApp.

1. Para utilizar o sistema (tanto no Tupã como nas máquinas virtuais) entre no diretório raiz da instalação do SCANTEC:

    === "Comando"
        ```
        cd SCANTEC.2.0.0/
        ```

2. Execute o script de execução do SCANTEC com um parâmetro na linha de comando:

    === "Comando"
        ```
        ./run_scantec.sh [parâmetro]
        ```

    * Sendo esse parâmetro as seguintes opções:

        - Testcase do BRAMS (Jan/2016);
        - Testcase do ETA (Abr/2020);
        - Testcase do BAM (Ago/2014);
        - dados definidos pelo usuário.

Para cada uma dessas opções o script irá criar um novo arquivo `SCANTEC.2.0.0/bin/scantec.conf` colocando nele as informações necessárias para cada um dos experimentos, já configurado o período dos dados, o passo de análise, passo de previsão e o período de integração dos modelos. O formato dos arquivos disponíveis para os testes é configurado em arquivos colocados no diretório `tables` e para novos modelos, ou versões com diferentes resoluções ou domínios, novos arquivos `*.table` devem ser disponibilizados no diretório `SCANTEC.2.0.0/tables`. Para mais informações sobre adicionar outros modelos veja a próxima seção intitulada **Adicionando outras versões ou modelos no SCANTEC**.

A informações de saída dos testcases do SCANTEC são colocadas no diretório `SCANTEC.2.0.0/dataout/TestMODEL` onde `MODEL` pode ser `BRAMS`, `ETA` ou `BAM`, dependendo da opção escolhida acima.

### Executando o SCANTEC com dados do usuário

Para executar o script `run_scantec.sh` com as informações editadas pelo usuário, escolha a opção `4`. Mas antes, é preciso editar o script e modificar apropriadamente algumas informações.

1. Para utilizar o sistema (tanto no Tupã como nas máquinas virtuais) entre no diretório raiz da instalação do SCANTEC:

    === "Comando"
        ```
        $ cd SCANTEC.2.0.0/
        ```

2. Edite o script `run_scantec.sh`:

    === "Comando"
        ```
        $ vi run_scantec.sh
        ```

    Modifique apropriadamente as variáveis entre as (aproximadamente entre as linhas 235 e 273) da seguinte lista:
    
    * Datas:
    
    ```
    datai=2014080500
    dataf=2014080600
    passo_analise=12
    passo_previsao=12
    total_previsao=72
    ```
    
    * Regiões:
    
    ```
    lat_low=-49.875
    lon_low=-82.625
    lat_up=11.375
    lon_up=-35.375
    dx=0.4
    dy=0.4
    ```
    
    * Quantidade de experimentos:
    
    ```
    quant_exp=1
    ```
    
    * Referências Plugin modelo:
    
    ```
    pl_model_refer=BAM_TQ0299L064_18levs
    ```
    
    * Endereço das Análises usadas como referência:
    
    ```
    arq_refer=/dados/das/public/SCANTEC/TestCase/AGCM/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%y4%m2%d2%h2%y4%m2%d2%h2P.icn.TQ0299L064.ctl
    ```
    
    * Plugin experimento:
    
    ```
    pl_model_exper=BAM_TQ0299L064_18levs
    ```
    
    * Previsões:
    
    ```
    arq_prev=/dados/das/public/SCANTEC/TestCase/AGCM/TQ0299L064/%y4%m2%d2%h2/GPOSNMC%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2P.fct.TQ0299L064.ctl
    ```
    
    * Climatologia:
    
    ```
    use_climatologia=0
    arq_clim=/dados/das/public/SCANTEC/climatologia/climatologia50yr.%mc.ctl
    ```

3. Depois de salvar as modificações no scritp `run_scantec.sh` execute-o com a opção `4`:

    === "Comando"
        ```
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
         /scripts/ensemble/SCANTEC.2.0.0/dataout/                          
        
         Arquivo de log:                           
         Log do processo: /scripts/ensemble/SCANTEC.2.0.0/logfile/scantec-20220628.17.30.log                
         ==========================                
        
         Início do processamento: Tue Jun 28 17:30:01 GMT 2022
        
         Criando o arquivo de configurações em bin/scantec.conf
        
         Arquivo de configuracao criado com sucesso.
        
         Executando o scantec.x ...
        
         . . . 

         Final do processo: Tue Jun 28 17:30:01 GMT 2022
        
         Log do processo: /scripts/ensemble/SCANTEC.2.0.0/logfile/scantec-20220628.17.30.log
        ====================================================================================
        
         Fim do processo!
        ```

4. Ver os resultados de saída do SCANTEC no diretório:

    === "Comando"
        ```
        ls SCANTEC.2.0.0/dataout
        ```

Para visualizar os resultados gerados pelo SCANTEC é recomendado que se utilize o SCANPLOT, ver mais detalhes na seção **Visualização de resultados usando o SCANPLOT** desse manual.

## Adicionando outras versões ou modelos no SCANTEC

Para adicionar uma nova versão do modelo na lista das opções em que o sistema está preparado para processar basta seguir as instruções descrita nessa seção. Inicialmente verifique se a versão desejada já não está implementada no sistema. Caso não, para incluir um novo modelo, ou versão, basta criar um novo arquivo `table` (com extensão `.model`) no diretório `SCANTEC.2.0.0/tables` com algumas particularidades.

!!! note "Nota"

    * Observe que modelos com resolução diferentes, ou recortes de modelos, ou mudanças no domínio, ou mesmo com modificações no número de níveis realizados no pós processamento, requerem ajustes para que o sistema seja capaz de ler os arquivos binários.
    * Cabe salientar que apenas arquivos binários e GRIB1 são lidos pela atual versão do sistema. Caso o modelo que deseja adicionar não esteja nesses formatos, precisam ser convertidos ou terá que aguardar uma nova versão do sistema. 

A lista dos modelos já implementados na versão SCANTEC V2.0.0 é a que segue com seus respectivos arquivos `tables` que podem servir como exemplo para criar outros:

* Modelo BAM Truncamento 299 64 níveis com pós de 18 níveis: `BAM_TQ0299L064_18levs.model`;
* Modelo BAM Truncamento 299 64 níveis com pós de 28 níveis: `BAM_TQ0299L064_28levs.model`;
* Modelo BRAMS 5km de resolução horizontal e pós de 19 níveis: `BRAMS_5km_19levs.model`;
* Modelo ETA 5km de resolução horizontal e pós de 22 níveis: `ETA_ams_05km_22levs.model`;

Para adicionar um novo modelo basta editar um dos arquivos acima que mais se assemelha com o modelo desejado fazer os devidos ajustes e salvar com um nome apropriado (extensão `.model`) dentro do diretório `tables`, e no arquivo de configuração `scantec.conf` ou se tiver usando o script `run_scantec.sh`, assegure que o novo modelo ou versão seja lido com o arquivo `table` criado. Para isso na linha do experimento em que esse modelo se refere a primeira palavra deve ser o nome do arquivo `table`.

!!! warning "Atenção"

    * Se esse arquivo é também utilizado como referência na avaliação o novo `table` deve também ser colocado após a palavra `Reference Model Name:` no arquivo de configurações do SCANTEC:

    ```
    Reference Model Name: ETA_ams_05km_22levs
    ```

O importante é que o novo arquivo `table` tenha as seguintes informações:

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

A tabela de variáveis é preenchida da seguinte forma (colunas separadas por espaço):

* A primeira coluna é o nome da variável do SCANTEC, veja lista em `tables/scantec.vars`;
* A segunda coluna pode ser o nome da variável correspondente no modelo da forma como listado no arquivo descritor (`.ctl`) do modelo.

Veja exemplo do arquivo `ETA_ams_05km_22levs.model`:

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

Caso o modelo não tenha a variável que o SCANTEC requer uma função pode ser chamada ao colocar na segunda coluna algumas das funções disponiveis no SCANTEC, a qual deverá produzir a variável desejada, baseando-se nas variáveis pré-existentes do modelo, as quais são colocadas entre parenteses depois do nome da função separadas por vírgulas.

Veja exemplo do arquivo `ETA_ams_05km_22levs.model`:

```
vars:
vtmp:925 vtmp2(temp:925,umes:925)
vtmp:850 vtmp2(temp:850,umes:850)
vtmp:500 vtmp2(temp:500,umes:500)
```

Nestes exemplos, a temperatura virtual o modelo Eta não está disponível e é chamada uma função para seu cálculo, utilizando a temperatura e umidade nos respectivos níveis desejados.

!!! note "Nota"

    A lista das funções e os argumentos requiridas por cada uma delas está disponivel no documento chamado `funcoes.readme` no diretóro `docs`, o qual detalha como utilizar as funções disponíveis.
