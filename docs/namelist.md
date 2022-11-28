# Namelist 

O namelist (arquivo de configurações) do SCANTEC é constituído de 5 grupos de informações:

1. Período em avaliação;
2. Recorte comum dos domínios dos modelos em avaliação;
3. Endereços de tabelas e dos arquivos de saída dos resultados;
4. Endereços dos arquivos dos modelos em avaliação e plugins usados;
5. Informações sobre a climatologia usada no cálculo do Coeficiente de Correlação de Anomalia (CCA);

Para cada um desses grupos são apresentadas as palavras-chave de cada informação junto com um exemplo e um comentário sobre essa informação (linha de cima):

* Data inicial do período (primeira análise usada):

    ```
    Starting Time: 2020040400
    ```

* Data final do período (última análise usada):

    ```
    Ending Time: 2020040812
    ```

* Passo de tempo em horas entre as análises no período:

    ```
    Analisys Time Step: 12
    ```
!!! warning "Atenção"

    Independente da forma como o SCANTEC é utilizado, é fundamental que a variável `Analisys Time Step` seja adequadamente ajustada para que os arquivos `.ctl` (utilizados para a visualização da distribuição espacial das estatísticas) sejam corretamente definidos. Se o SCANTEC for utilizado para a avaliação de modelos para apenas uma data (e.g., quando as variáveis `Starting Time` e `Ending Time` possuírem os mesmos valores), ajuste a variável `Analisys Time Step` com um valor que reflita o intervalo de tempo entre as previsões. Este valor é utilizado para o cálculo da variável `tdef` a ser alocada dentro do arquivo `.ctl`. Veja a [issue #3 no GitHub do SCANTEC](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/issues/3) para mais informações.


* Passo de tempo em horas entre as previsões avaliadas (tem que ser o mesmo passo entre as análises usadas como referência):

    ```
    Forecast Time Step: 6
    ```

* Tempo de integração do modelo em avaliação:

    ```
    Forecast Total Time: 72
    ```

* Número de recortes, para o caso de se usar uma avaliação com diversos domínios:

    ```
    run domain number: 1
    ```

* Latitude inferior do recorte:

    ```
    run domain lower left lat: -51.000000
    ```

* Longitude inferior do recorte:

    ```
    run domain lower left lon: -84.099998
    ```

* Latitude superior do recorte:

    ```
    run domain upper right lat: 15.0000
    ```

* Longitude superior do recorte:

    ```
    run domain upper right lon: -25.999998
    ```

* Resolução em graus na longitude:

    ```
    run domain resolution dx: 0.05
    ```

* Resolução em graus na latitude:

    ```
    run domain resolution dy: 0.05
    ```

* Endereço onde são encontrados os plugins dos modelos (arquivos com a extensão `.model`):

    ```
    scantec tables: /scratchin/grupos/das/home/luiz.sapucci/SCANTEC.2.0.0/tables
    ```

* Diretório de saída dos resultados do scantec:

    ```
    Output directory: /scratchin/grupos/das/home/luiz.sapucci/SCANTEC.2.0.0/dataout/ETA
    ```

* Nome do arquivo de configuração dos dados de referência:

    ```
    Reference Model Name: ETA_ams_05km_22levs
    ```

* Endereço dos arquivos de análise usados como referência na avaliação das previsões, bem como as máscaras (i.e., `%y4%m2%d2%h2` indicando a data no formato YYYYMMDD) com a formatação das datas nos nomes dos arquivos:

    ```
    Reference file: /dados/das/public/SCANTEC/TestCase/ETA/Eta_ams_05km202004/%d2/%h2/eta_05km_%y4%m2%d2%h2+%y4%m2%d2%h2.ctl
    ```

* Quantidade de versões dos modelos ou de diferentes modelos em avaliação:

    ```
    Experiments: 1
    ```

* Três informações em cada linha indicando o (1) nome do arquivo de configuração, (2) o label do experimento e (3) o endereço das previsões em avaliação, bem como a formatação das datas nos nomes dos arquivos:

    ```
    ETA_ams_05km_22levs EXP01 /dados/das/public/SCANTEC/TestCase/ETA/Eta_ams_05km202004/%d2/%h2/eta_05km_%y4%m2%d2%h2+%fy4%fm2%fd2%fh2.ctl
    ```

* Uma opção de usar ou não a climatologia no cálculo da CCA, sendo 1 para usar e 0 para não usar:

    ```
    Use Climatology: 0
    ```

* Nome do arquivo de configuração dos dados de referência:

    ```
    Climatology Model Name: AGCM_CLIMATOLOGY.model
    ```

* Endereço dos arquivos de climatologia usado no cálculo do CCA, bem como a formatação das datas nos nomes dos arquivos:

    ```
    Climatology file: /dados/das/public/SCANTEC/climatologia/climatologia50yr.%mc.ctl
    ```
