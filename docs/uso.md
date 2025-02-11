#  Uso

O SCANTEC possui alguns arquivos de configuração (namelists) onde são definidos os parâmetros utilizados nas avaliações numéricas. Estes arquivos são os seguintes:

* Arquivo namelist `scantec.conf`: arquivo com os parâmetros da avaliação (e.g., datas, nomes dos arquivos etc);
* Arquivo namelist `scantec.vars`: arquivo com os nomes das variáveis e suas equivalências utilizadas na avaliação;
* Arquivos tables `*.model`: arquivos de definição das grades dos modelos utilizados na avaliação.

Todos estes arquivos devem ser revisados antes de se iniciar uma avaliação. Para uma descrição mais detalhada sobre os parâmetros e elementos definidos nestes arquivos, veja a página [Namelist](namelist.md).

!!! note "Nota"

    Para os usuários internos que usam as maquinas virtuais não é necessário baixar os arquivos de testcase, pois eles estão disponiveis na maquina no /share, para os usuários externos ao INPE esses dados são baixados por FTP, na primeira ocasião de teste com o script que roda o scantec. Os dados necessários ( arquivos de análises, previsões e climatologia) são baixados e colocados no datain/Test{Model} e estão já configurados no 'scantec.conf' para esses locais. Para mais informações sobre a utilização do arquivo `scantec.conf`, veja a página [Namelist](namelist.md).

Para utilizar o SCANTEC é preciso editar o arquivo namelist `SCANTEC/core/scantec.conf` e modificar as informações para refletir os dados do usuário. O arquivo deve ser modificado e salvo dentro do diretorio `SCANTEC/bin`, junto com o arquivo executável `scantec.x`. Para edição do arquivo namelist `scantec.conf`, há um conjunto de palavras-chave que antecedem a informação requerida pelo sistema. O usuário deve procurar por essas palavras-chave e atribuir os valores desejados.

Nas instruções apresentadas a seguir, são mostrados os procedimentos utilizados para preparar o SCANTEC para um tipo de avaliação, utilizando dados de exemplo.

!!! info "Informação"

    Os arquivos de exemplo utilizados na avaliação com o SCANTEC, estão disponíveis no disco NetApp do CPTEC, acessível a partir das máquinas virtuais do centro e Egeon. Os usuários externos ao INPE que desejam utilizar o SCANTEC em suas avaliações, deverão preparar os seus próprios arquivos de análises, previsões e climatologias.

1. Para utilizar o sistema entre no diretório `bin` do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC/bin/
        ```

2. Crie uma cópia do arquivo `SCANTEC/core/scantec.conf` para o diretório `SCANTEC/bin` e modifique apropriadamente as informações solicitadas (no exemplo, está sendo utilizado o editor `vi`, mas o usuário pode utilizar o editor que melhor lhe convier):

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

    !!! warning "Atenção"

        No **Resultado** do exemplo acima, observe que foi emitida a mensagem `Climatology Not Found The mean reference field will be used as climatology`, o que indica que um arquivo de climatologia não foi inidicado para uso na avaliação e que uma média dos campos de referência serão utilizados para este propósito. A climatologia é utilizada para o cálculo do Coeficiente de Correlação de Anomalia.

    !!! note "Nota"

        No exemplo, observe que o SCANTEC foi executado para realizar avaliações objetivas a partir de arquivos de previsões numéricas para até 72 horas, entre os dias 2014080500 e 2014080600, a cada 12 horas. Verifique novamente o arquivo namelist `scantec.conf` e compare os valores dos parâmetros com as informações impressas na tela.

4. De outra forma, pode-se também utilizar o script `run_scantec.sh`. Este script modifica o arquivo namelist `scantec.conf` com valores pré-definidos, executa o arquivo binário `scantec.x` e armazenas as informações em um arquivo de log. Esse script possui uma série de testcases para permitir a validação da versão instalada pelo usuário:

    === "Comando"
        ```bash linenums="1"
        ./run_scantec.sh
        ```
    === "Resultado"
        ```
        Sistema Comunitário de Avaliação de modelos Numéricos de Tempo e Clima (SCANTEC)
        
        A opção TestCase não foi informada!
        
        Uso:
        ./run_scantec.sh 1 - TestCase do BRAMS         (Jul/2023)
        ./run_scantec.sh 2 - TestCase do ETA           (Jul/2023)
        ./run_scantec.sh 3 - TestCase do WRF           (Jul/2023)
        ./run_scantec.sh 4 - TestCase do BAM           (Jul/2023)
        ./run_scantec.sh 5 - Compara WRF/ETA/BRAMS/BAM (Jul/2023)
        ./run_scantec.sh 6 - TestCase do MONAN         (Jan/2025)
        ./run_scantec.sh 7 - Dados definidos pelo usuário
        ```

![image](https://github.com/user-attachments/assets/eb87d3b4-4328-41e1-8e0d-57225ff8cd44)

!!! note "Nota"

	A correta utilização do script `run_scantec.sh` dependerá do acesso aos dados requeridos, sejam eles provenientes dos testcases do SCANTEC ou informados pelo usuário.
	
	O conjunto de dados de testcase do SCANTEC não são distribuídos com a release devido ao seu tamanho, no entanto esse script está apto a baixar os dados do repositorio do CPTEC https://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/ onde estão disponiveis um conjunto de dados para testar os modelos listados acima.

Na seção a seguir, e detalhada a utilização do SCANTEC a partir dos dados de testcase.

### Execução do SCANTEC com os dados do testcase

Os dados de entrada do testcase para as 6 opções de modelos disponíveis (i.e., BRAMS, ETA, WRF, BAM e MONAN) estão no disco NetApp do CPTEC, acessíveis em locais diferentes a depender da máquina utilizada:

* Máquinas virtuais (e.g., Itapemirim, Ilopolis, Colorado): `/share/das/dist/scantec/TestCase_SCANTEC/`;
* Máquina Egeon: `/pesq/share/das/dist/scantec/TestCase_SCANTEC/`;
* Máquina externas ao inpe: `https://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/`.

!!! warning "Atenção"

    Na máquina XC50, estes dados não estão disponíveis, mas podem ser copiadas a partir das máquinas Itapemirim ou Egeon.

1. Para utilizar o sistema (tanto local, quanto nas máquinas virtuais) entre no diretório raiz da instalação do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC/
        ```
!!! warning "Atenção"

    Nas máquinas virtuais, é preciso antes de rodar o run_scantec.sh apagar os diretorios do datain e criar links para os locais onde esses dadps estão pois, caso contrário os dados (que já estão na itapemerim) serão baixados via ftp ocupando espaço desnecessário. Nesse caso é preciso fazer a proxima ação, caso esteja em maquina sem acesso direto a esses dados pule essa etapa:

2. Se estiver rodando nas maquina virtuais (itapemirim,ilopolis, etc) faça:

    === "Comando"
        ```bash linenums="12"
        rmdir datain/TestBRAMS
        ln -s /share/das/dist/scantec/TestCase_SCANTEC/BRAMS datain/TestBRAMS
        rmdir datain/TestETA
        ln -s /share/das/dist/scantec/TestCase_SCANTEC/ETA datain/TestETA
        rmdir datain/TestWRF
        ln -s /share/das/dist/scantec/TestCase_SCANTEC/WRF datain/TestWRF  
        rmdir datain/TestBAM
        ln -s /share/das/dist/scantec/TestCase_SCANTEC/BAM_T666L64 datain/TestBAM
        rmdir datain/TestMONAN
        ln -s /share/das/dist/scantec/TestCase_SCANTEC/MONAN datain/TestMONAN
        rmdir datain/climatologia
        ln -s /share/das/dist/scantec/TestCase_SCANTEC/Climatologia datain/climatologia
        ```
 
3. Execute o script de execução do SCANTEC com um parâmetro na linha de comando:

    === "Comando"
        ```bash linenums="1"
        ./run_scantec.sh [parâmetro]
        ```

    * Sendo esse parâmetro uma das seguintes opções:

        - 1 - TestCase do BRAMS         (Jul/2023)
        - 2 - TestCase do ETA           (Jul/2023)
        - 3 - TestCase do WRF           (Jul/2023)
        - 4 - TestCase do BAM           (Jul/2023)
        - 5 - Compara WRF/ETA/BRAMS/BAM (Jul/2023)
        - 6 - TestCase do MONAN         (Jan/2025)
        - 7 - Dados definidos pelo usuário

Para cada uma dessas opções, o script criará um novo arquivo namelist `SCANTEC/bin/scantec.conf`, onde serão ajustados os parâmetros e informações necessárias para cada um dos experimentos, i.e., o período dos dados, o intervalo entre as análises, o intervalo entre as previsões e o período de integração dos modelos. O formato dos arquivos disponíveis para os testes é determinado em arquivos alocados no diretório `tables`. Para cada novo modelo ou versão de modelo com diferente resolução ou domínio, novos arquivos `*.table` devem ser preparados e alocados no diretório `SCANTEC/tables`. Para mais informações sobre como adicionar outros modelos, veja a seção intitulada [Adicionando outras versões ou modelos no SCANTEC](#adicionando-outras-versoes-ou-modelos-no-scantec).

Observe que a opção 5 faz uma intercomparação entre os modelos WRF, ETA BRAMS e BAM. Observe nos arquivos de saida é possivel gerar gráficos que comparam o desempenho dos diferentes modelos avaliados. Obviamente que um pre requisito para essa intercomparação é que todos os períodos dos dados disponíveis para os diferentes modelos devem ser o mesmo. Observe esse detalhe ao elaborar avaliações de intercomparações de modelos. 

As informações de saída dos testcases do SCANTEC são escritas no diretório `SCANTEC/dataout/TestMODEL` onde `MODEL` pode ser `BRAMS`, ` WRF`, ` MONAN`,  `ETA` ou `BAM`, dependendo da opção escolhida acima. Para a opção 5 o diretorio está seetado para se chamar TestCompara. Por exemplo:

```
ls -ltr SCANTEC/dataout/TestMODEL
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

* `RMSEEXP01_20140805002014080600F.ctl`: arquivo ascii (texto) descritor para o arquivo `RMSEEXP01_20140805002014080600F.scan`. Pode ser aberto no software GrADS;
* `RMSEEXP01_20140805002014080600F.scan`: arquivo binário com a distribuição espacial da estatística RMSE (Root Mean Square Error, ou Raiz do Erro Quadrático Médio);
* `VIESEXP01_20140805002014080600T.scan`: arquivo ascci (texto) com uma tabela com o resultado do RMSE, calculado para cada uma das variáveis escolhidas e ordenadas por tempo de previsão para o período escolhido.

!!! note "Nota"

    Dependendo do testcase escolhido, o tempo de execução do SCANTEC pode ser diferente devido ao número de pontos de grade a serem considerados na avaliação. Independente do número de modelos utilizados em uma avaliação com o SCANTEC, sempre será realizada a interpolação bilinear (espacial, horizontal) das grades para uma grade comum. Veja na página [Namelist](namelist.md) os parâmetros `run domain resolution dx` (ou `dx`) e `run domain resolution dy` (ou `dy`), os quais indicam a resolução (em graus) para a qual as grades serão interpoladas. Na vertical, as variáveis não são interpoladas, visto que elas são definidas por níveis de pressão como descrito no arquivo `SCANTEC/tables/scantec.vars`.

### Utilizando o SCANTEC em dados NetCDF

A atual versão do SCANTEC está apta para ler apenas dados nos formatos GRIB ou binário (`.grb` ou `.bin`.), logo se a saida do pós-processamento do modelo é escrita no formato NetCDF (`.nc`) é necessário um pré-processamento nesses dados para depois serem avaliados no SCANTEC. Esse é o caso dos dados do WRF, MPAS e MONAN. Para isso no diretório `scripts_uteis`, há um script chamado `nc2grb.sh`, que converte arquivos NetCDF para um conjunto de arquivos compatíveis no formato GRIB (`.grb`, `.ctl`, `.gmp`). Esse script utiliza o script [`lats4d.sh`](http://opengrads.org/doc/scripts/lats4d/), que permite recortar os dados espacialmente, temporalmente, por níveis de pressão e por variáveis, facilitando o processamento dos arquivos brutos. Os dados dos modelos [MONAN](https://monanadmin.github.io/monan_cc_docs/) (Model for Ocean-laNd-Atmosphere predictioN) e [WRF](https://www.mmm.ucar.edu/models/wrf) (Weather Research and Forecasting model) disponiveis no testacase foram pré-processados usando esse script e o mesmo deve ser feito para outros arquivos que o usuário deseja avaliar no SCANTEC, que estejam no formato NetCDF.

A atual versão do script `lats4d.sh` no diretório `SCANTEC/scripts_uteis`, está configurado para ler a versão  pós-processada do MONAN v1.1.0 (dados brutos disponiveis no endereço [https://ftp1.cptec.inpe.br/pesquisa/das/victor.ranieri/MONANexp/MONAN_v1.1.0/](https://ftp1.cptec.inpe.br/pesquisa/das/victor.ranieri/MONANexp/MONAN_v1.1.0/)) e foi utilizado para gerar os arquivos GRIB disponíveis no testcase do MONAN ([https://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/MONAN](https://ftp1.cptec.inpe.br/pesquisa/das/scantec/TestCase_SCANTEC/MONAN)). 

!!! note "Nota"

    A presente versão dos scripts está ajustada para o modelo MONAN. Ajustes podem ser necessários para adaptar os scripts para as necessidades do usuário! 

Observe que foi feita nessa conversão uma seleção de variáveis e níveis do modelo visando deixar o arquivo mais leve para o download durante o testcase. Na máquina Itapemirim, o script pode ser executado e testado da forma como está. Fora da rede interna do CPTEC, é preciso baixar os dados brutos (~1,2 TB) e modificar o endereço de onde ele deve ser lido localmente. Os dados são gerados no diretório `datain/MONAN`. O domínio dos dados é o global, mas há um recorte sobre a América do Sul. Para utilizar este recorte, basta descomentar as linhas apropriadamente. Para executar o scritp faça:

1. Entre no diretório `scripts_uteis` onde o SCANTEC foi instalado:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC/scripts_uteis
        ```

2. Execute o script `nc2grb.sh`:

    === "Comando"
        ```bash linenums="1"
        nc2grb.sh
        ```
!!! note "Nota"

	Para utilizar os dados do usuário, é preciso modificar os endereços das variáveis a seguir:

    * `datain`
    * `dataout`
    * `dir_script`

### Executando o SCANTEC com dados do usuário

Para executar o script `run_scantec.sh` com as informações inseridas pelo usuário, escolha a opção `7`. Mas antes, é preciso editar o script e modificar apropriadamente algumas informações. Veja a seguir:

1. Para utilizar o sistema (tanto no Tupã como nas máquinas virtuais) entre no diretório raiz da instalação do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC/
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

3. Em seguida, salve as modificações no script `run_scantec.sh` e execute-o com a opção `7`:

    === "Comando"
        ```bash linenums="1"
        ./run_scantec.sh 7
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
         /scripts/ensemble/SCANTEC/dataout/                          
        
         Arquivo de log:                           
         Log do processo: /scripts/ensemble/SCANTEC/logfile/scantec-20220628.17.30.log                
         ==========================                
        
         Início do processamento: Tue Jun 28 17:30:01 GMT 2022
        
         Criando o arquivo de configurações em bin/scantec.conf
        
         Arquivo de configuracao criado com sucesso.
        
         Executando o scantec.x ...
        
         . . . 

         Final do processo: Tue Jun 28 17:30:01 GMT 2022
        
         Log do processo: /scripts/ensemble/SCANTEC/logfile/scantec-20220628.17.30.log
        ====================================================================================
        
         Fim do processo!
        ```

4. Para verificar os resultados de saída do SCANTEC, liste os arquivos do diretório `SCANTEC/dataout`:

    === "Comando"
        ```bash linenums="1"
        ls SCANTEC/dataout
        ```

!!! info "Informação"

    Para visualizar os resultados gerados pelo SCANTEC, pode-se utilizar softwares como o GrADS (para visualização da distribuição espacial dos campos) e o GNUPlot (para a plotagem das tabelas). A partir da versão SCANTEC V2.0.0, recomenda-se a utilização do SCANPLOT (veja mais detalhes na página [Visualização de resultados usando o SCANPLOT](scanplot.md) ou no site do projeto em [https://gam-dimnt-cptec.github.io/SCANPLOT/](https://gam-dimnt-cptec.github.io/SCANPLOT/)).

## Adicionando outras versões ou modelos no SCANTEC

Para adicionar uma nova versão de modelo à lista das opções em que o sistema está preparado para processar (i.e., BRAMS, ETA, BAM), siga as instruções descritas nessa seção. Inicialmente, verifique se a versão desejada já não está implementada no sistema. Caso não esteja, para incluir uma nova versão, crie um novo arquivo com a extensão `.model` dentro do diretório `SCANTEC/tables` com as informações pertinentes à versão do modelo a ser utilizado. Utilize um arquivo existente para saber como formatar o novo arquivo.

!!! note "Notas"

    * Observe que modelos com resoluções, recortes ou domíniois diferentes, ou mesmo com modificações no número de níveis pós-processados, requerem ajustes para que o sistema seja capaz de ler os arquivos binários;
    * Ressalta-se que apenas arquivos binários (`*.bin`) e GRIB1 (`*.grb`) são lidos pela atual versão do sistema. Arquivos no formato GRIB2 e NetCDF não são suportados pela versão SCANTEC V2.1.0. Caso o modelo que deseja adicionar não esteja nesses formatos, estes podem ser convertidos para os formatos binário ou GRIB1 com o auxílio do script [`lats4d`](http://opengrads.org/doc/scripts/lats4d/) ou similares. 

A lista abaixo, elenca os modelos já implementados na versão SCANTEC V2.1.0 junto com os seus respectivos arquivos `table`, os quais podem servir como exemplo para criar outros:

* Modelo AGCM[^1] TQ0062L028 (climatologia de 50 anos), pós-processado em 18 níveis de pressão: `AGCM_TQ0062L028_50YR_CLIMATOLOGY_18levs.model`;
* Modelo AGCM TQ0126L028, pós-processado em 9 níveis de pressão: `AGCM_TQ0126L028_9levs.model`;
* Modelo BAM TQ0299L064, pós-processado em 18 níveis de pressão: `BAM_TQ0299L064_18levs.model`;
* Modelo BAM TQ0299L064, pós-processado em 28 níveis de pressão: `BAM_TQ0299L064_28levs.model`;
* Modelo BAM TQ0666L064, pós-processado em 33 níveis de pressão: `BAM_TQ0666L064_33levs.model`;
* Modelo BRAMS com 5km resolução horizontal, pós-processado em de 19 níveis de pressão: `BRAMS_5km_19levs.model`;
* Modelo ETA com 5km de resolução horizontal, pós-processado em 22 níveis de pressão: `ETA_ams_05km_22levs.model`;
* Modelo CFSR T382L64, pós-processado em 33 níveis de pressão: `CFSR_T382L064_CLIMATOLOGY_37levs.model`;
* Modelo GFS[^2] 0,25 graus, pós-processado em 22 níveus de pressão: `GFS_0p25_5levs.model`.

[^1]: O modelo AGCM (Atmospheric General Circulation Modelo) representa uma geração anterior do modelo BAM (Brazilian Atmospheric Model).
[^2]: O modelo GFS foi testado a partir da conversão dos arquivos GRIB2 para GRIB1, utilizando o script [`lats4d`](http://opengrads.org/doc/scripts/lats4d/). A utilização do SCANTEC com os arquivos convertidos desse modelo, devem ser feita com atenção para os nomes das variáveis e os níveis verticais, uma vez que o modelo utiliza coordenada vertical híbrida.

Para adicionar um novo modelo basta editar um dos arquivos acima, escolhendo-se aquele que mais se assemelha com o modelo desejado e fazer os ajustes necessários. Salve o arquivo modificado com um nome apropriado (incluindo a extensão `.model`) dentro do diretório `SCANTEC/tables`, e no arquivo namelist `scantec.conf` (ou no script `run_scantec.sh`, caso esteja sendo utilizado), assegure-se de que a nova versão do novo modelo seja lida a partir do arquivo `table` criado. Para isso, na linha em que o experimento é referenciado, a primeira palavra deve ser o nome do arquivo `table`. Veja a seguir:

No script `run_scantec.sh`:

```
pl_model_refer=BAM_TQ0299L064_18levs
```

ou, no arquivo namelist `scantec.conf`:

```
BAM_TQ0299L064_18levs EXP01 /caminho/para/o/arquivo.ctl
```

!!! warning "Atenção"

    * Se esse arquivo é também utilizado como referência na avaliação, o novo arquivo `table` deve também ser colocado após a palavra `Reference Model Name:`, dentro do arquivo namelist do SCANTEC:

    ```
    Reference Model Name: BAM_TQ0299L064_18levs
    ```

O novo arquivo `table` deve conter as seguintes informações:

* Tipo de arquivo, depois da palavra `ftype:`;
* Valor considerado indefinido, depois da palavra `undef:`;
* Dimensões da grade na longitude, depois da palavra `xdim:`;
* Dimensões da grade na latitude, depois da palavra `ydim:`;
* Número de níveis verticais do pós-processamento e a sua lista, depois da palavra `zdim:`;
* Tabela de variáveis, depois da palavra `vars:`.

Veja o exemplo do arquivo `ETA_ams_05km_22levs.model`:

```
ftype: grib
undef: 1e+20
xdim: 1162 linear -84.099998 0.050000
ydim: 1320 linear -51.000000 0.050000
zdim:
22 levels 1020 1000 950 925 900 850 800 750 700 650 600 
           550  500 450 400 350 300 250 200 150 100 50
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
zgeo:250 zgeo:250 
uvel:850 uvel:850 
uvel:500 uvel:500 
uvel:250 uvel:250 
vvel:850 vvel:850 
vvel:500 vvel:500 
vvel:250 vvel:250 
::
```

Outro arquivo igualmente importante para a configuração do SCANTEC, é o arquivo namelist `scantec.vars`. Este arquivo contém uma tabela de variáveis a serem utilizadas na avaliação. A tabela de variáveis é preenchida da seguinte forma (colunas separadas por espaço):

* A primeira coluna é o nome da variável do SCANTEC;
* A segunda coluna pode ser o nome da variável correspondente no modelo da forma como listado no arquivo descritor (`.ctl`) do modelo.

Veja o exemplo do arquivo `scantec.vars`:

```
variables:
VTMP:925 "Virtual Temperature @ 925 hPa [K]"
VTMP:850 "Virtual Temperature @ 850 hPa [K]"
VTMP:500 "Virtual Temperature @ 500 hPa [K]"
TEMP:850 "Absolute Temperature @ 850 hPa [K]"
TEMP:500 "Absolute Temperature @ 500 hPa [K]"
TEMP:250 "Absolute Temperature @ 250 hPa [K]"
PSNM:000 "Pressure reduced to MSL [hPa]"
UMES:925 "Specific Humidity @ 925 hPa [g/Kg]"
UMES:850 "Specific Humidity @ 850 hPa [g/Kg]"
UMES:500 "Specific Humidity @ 500 hPa [g/Kg]"
AGPL:925 "Inst. Precipitable Water @ 925 hPa [Kg/m2]"
ZGEO:850 "Geopotential height @ 850 hPa [gpm]"
ZGEO:500 "Geopotential height @ 500 hPa [gpm]"
ZGEO:250 "Geopotential height @ 250 hPa [gpm]"
UVEL:850 "Zonal Wind @ 850 hPa [m/s]"
UVEL:500 "Zonal Wind @ 500 hPa [m/s]"
UVEL:250 "Zonal Wind @ 250 hPa [m/s]"
VVEL:850 "Meridional Wind @ 850 hPa [m/s]"
VVEL:500 "Meridional Wind @ 500 hPa [m/s]"
VVEL:250 "Meridional Wind @  250 hPa [m/s]"
::
```

!!! note "Nota"

    Observe que ambos os arquivos `table` e `scantec.conf`, possuem uma seção específica para a definição das variáveis. No arquivo `scantec.conf`, são definidas as variáveis e os nomes das variáveis nos níveis que se deseja avaliar com o SCANTEC. Nos arquivos `table`, as variáveis que se deseja avaliar, devem ser definidas com base nas variáveis do modelo. Isso ocorre pois nem sempre as variáveis que se dejesa avaliar, não são pós-processadas pelos modelos. Para contornar isso, o SCANTEC possibilita o cálculo de variáveis (com base nas variáveis prós-processadas do modelo) durante o seu tempo de execução. Veja a seção [Funções Matemáticas Implícitas](#funcoes-matematicas-implicitas) para mais informações.

## Funções Matemáticas Implícitas

A edição do arquivo `table` permite a utilização de funções matemáticas para a definição de novas variáveis. Caso o modelo não tenha a variável que o SCANTEC requer (ou que se deseja avaliar), funções matemáticas podem ser chamadas na segunda coluna da seção `vars:`. Algumas das funções disponiveis no SCANTEC podem ser aplicadas para isto, utilizando as variáveis pós-processadas do modelo. Estas funções são escritas entre parênteses depois do nome da função e são separadas por vírgulas.

Veja o exemplo a seguir onde a variável `vtmp` (temperatura virtual) é calculada a partir das variáveis `temp` (temperatura absoluta) e `umes` (umidade específica):

```
vars:
vtmp:925 vtmp2(temp:925,umes:925)
vtmp:850 vtmp2(temp:850,umes:850)
vtmp:500 vtmp2(temp:500,umes:500)
```

As seguintes funções matemáticas estão disponíveis na versão SCANTEC:

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

!!! tip "Dica"

    A implementação das funções matemáticas no SCANTEC, está nas rotinas [MathExpress.f90](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/blob/master/core/MathExpress.f90) e [scan_MathPlugin.f90](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/blob/master/core/scan_MathPlugin.f90).
