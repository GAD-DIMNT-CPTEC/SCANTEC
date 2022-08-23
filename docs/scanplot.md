#  Visualização de resultados usando o SCANPLOT

O [SCANPLOT](https://gam-dimnt-cptec.github.io/SCANPLOT/) é um módulo escrito em linguagem Python preparado para ler e plotar as tabelas com as estatísticas do SCANTEC. O seu uso pode ser feito por meio da linha de comando ou através do Jupyter notebook. O SCANPLOT transforma as tabelas do SCANTEC em dataframes do Pandas e pode ser facilmente extendido a partir da introdução de funções para a plotagem destes dataframes na forma como o usuário precisar. Para mais informações detalhada sobre a versão SCANPLOT V1.0.0 utiliza nesta release do SCANTEC, acesse o arquivo `SCANTEC.2.0.0/scanplot/README.md`.

##  Configuração do ambiente para utilização do SCANPLOT

Para usar o SCANPLOT é preciso uma configuração inicial do ambiente de execução, o que pode ser feito da seguinte forma:

1. Faça login na máquina Itapemirim:

    === "Comando"
        ```bash linenums="1"
        ssh usuario@itapemirim.cptec.inpe.br -XC
        ```

2. Crie a seguinte estrutura de diretórios em `/scripts/das/$USER`:

    === "Comando"
        ```bash linenums="1"
        cd /scripts/das/$USER
        mkdir conda
        cd conda
        mkdir envs pkgs
        ```

    !!! warning "Observações"
    
        * Depois disso verifique se no diretório `/scripts/das/$USER/conda` existem os diretórios `envs` e `pkgs`;
        * Caso não exista o diretório `/scripts/das/$USER` é preciso entrar em contato com o suporte para que seu usuário seja adicionado no grupo `das` e esse diretório seja criado;

3. Crie o arquivo `$HOME/.condarc` contendo as seguintes informações (troque o `#USER#` pelo seu username no sistema, o mesmo da variável `$USER`):

    ```
    envs_dirs:
    - /scripts/das/conda/envs
    - /scripts/das/#USER#/conda/envs
    pkgs_dirs:
    - /scripts/das/#USER#/conda/pkgs
    channels:
    - conda-forge
    - defaults
    ```

2. Para isso crie um arquivo com o nome `$HOME/.condarc` com seu editor preferido e copie o conteúdo acima (trocando o `#USER#` pelo seu username no sistema) e salve em seu home. Caso não consiga execute os comando abaixo que copiarão o arquivo do usuário `luiz.sapucci`, fezando as modificações apropriadamente:

    === "Comando"
        ```bash linenums="1"
        cd $HOME
        cp ~luiz.sapucci/.condarc .condarc
        sed -i "s/luiz.sapucci/${USER}/" .condarc
        ```

3. Em seguida, execute os comandos:

    === "Comando"
        ```bash linenums="1"
        source activate DASSCANPLOT
        python -m ipykernel install --user --name DASSCANPLOT --display-name DASSCANPLOT
        ```

## Passo a passo para a utilização do SCANPLOT

Depois de configurado o sistema na máquina Itapemirim, utilize o Jupyter notebook, disponível a partir da máquina Ilopolis:

1. Acesse a plataforma do Jupyter em seu navegador de internet pelo endereço: [http://ilopolis.cptec.inpe.br/hub/login](http://ilopolis.cptec.inpe.br/hub/login);
2. Entre com suas credenciais (as mesmas utilizadas para acessar a máquina Itepemirim);
3. O sistema abrirá toda a árvore de diretórios das máquinas virtuais. Nela é preciso abrir o diretório onde foi instalado o SCANTEC.2.0.0 e nele abra o diretório `docs/Tutorial` onde estará disponivel um tutorial detalhado de como usar o SCANPLOT. Abra o arquivo denominado `Tutorial_SCANPLOT.ipynb`; 
4. Depois de acompanhar todo o tutorial, abra a aba `Kernel`, e depois a sub-aba `Change kernel` e nela selecione a opção `DASSCANPLOT`, que carregará o kernel `DASSCANPLOT` nesse notebook, o que é indicado na caixinha no canto superior direito da tela;
5. Abra o arquivo `SCANTEC.2.0.0/scanplot/SCANPLOT.ipynb` e utilize o SCANPLOT executando cada uma das linhas de comandos do passo a passo nessa página;
6. Caso não tenho familiaridade com o Jupyter, observe que para executar os comandos basta clicar sucessivamente no botão `Run` da interface, o cursor indicará o comando sendo executado e os resultados são apresentados nas linhas posteriores ao comando nessa mesma página;
7. Observe que no terceiro comando, é necessário ajustar os dados para o seu usuário e o local onde o SCANTEC foi executado;
8. Acompanhe os resultados e salve as figuras que desejar em seu disco como convencionalmente é feito com seu navegador.

Também está disponível um tutorial do SCANTEC que dá a possibilidade de instalar e utilizar todo o sistema via notebook do Jupyter, o que abre algumas facilidades interessantes. Para isso abra o arquivo `SCANTEC.2.0.0/docs/Tutorial/Tutorial_SCANTEC.ipynb` e siga as instruções na página.

Esse mesmo tutorial pode se obtido a partir do endereço [Tutorial.tar](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/download/V2.0.0/Tutorial.tar).
