#  Visualização de resultados usando o SCANPLOT

O [SCANPLOT](https://gam-dimnt-cptec.github.io/SCANPLOT/) é um módulo escrito em linguagem Python preparado para ler e plotar as tabelas e os arquivos binários com as estatísticas do SCANTEC. O seu uso pode ser feito por meio da linha de comando ou através do Jupyter notebook. O SCANPLOT transforma os resultados do SCANTEC em estruturas de dados como dataframes e datasets, podendo ser facilmente extendido a partir da introdução de funções para a plotagem dos resultados na forma como o usuário precisar. Para informações detalhadas sobre a versão SCANPLOT V1.1.0a utiliza nesta release do SCANTEC, acesse a página do projeto em [https://gam-dimnt-cptec.github.io/SCANPLOT/](https://gam-dimnt-cptec.github.io/SCANPLOT/).

##  Configuração do ambiente para utilização do SCANPLOT

!!! warning "Atenção"

    As instruções a seguir sugerem a utilização do SCANPLOT a partir da máquina Ilopolis do CPTEC. Para uso em outras máquinas, as instruções de uso podem ser diferentes. Para os usuários que desejam utilizar o SCANPLOT em suas máquinas pessoais, recomenda-se seguir as instruções de instalação e uso que se encontram na página do projeto em [https://gam-dimnt-cptec.github.io/SCANPLOT/](https://gam-dimnt-cptec.github.io/SCANPLOT/).

Para usar o SCANPLOT na máquina Ilopolis, é necessário configurar o ambiente Python `DASSCANPLOT` para a sua conta a partir da máquina Itapemirim. A configuração inicial do ambiente de execução é feito da seguinte forma e é necessária apenas na primeira vez. ==Não se esqueça de substituir a palavra `<usuario>` pelo seu nome de usuário na máquina.==

1. Faça login na máquina Itapemirim:

    === "Comando"
        ```bash linenums="1"
        ssh <usuario>@itapemirim.cptec.inpe.br -XC
        ```

2. Crie a seguinte estrutura de diretórios em `/scripts/das/$USER`:

    === "Comando"
        ```bash linenums="1"
        cd /scripts/das/$USER
        mkdir conda
        cd conda
        mkdir envs pkgs
        ```

    !!! tip "Dicas"
    
        * Depois disso verifique se no diretório `/scripts/das/$USER/conda` existem os diretórios `envs` e `pkgs`;
        * Caso não exista o diretório `/scripts/das/$USER` é preciso entrar em contato com o suporte para que seu usuário seja adicionado no grupo `das` e para que esse diretório seja criado;

3. Crie o arquivo `$HOME/.condarc` contendo as seguintes informações:

    ```
    envs_dirs:
    - /scripts/das/conda/envs
    - /scripts/das/<usuario>/conda/envs
    pkgs_dirs:
    - /scripts/das/<usuario>/conda/pkgs
    channels:
    - conda-forge
    - defaults
    ```

4. Em seguida, faça logout e faça login novamente na máquina Itapemirim;
5. Na Itapemirim, execute os comandos para registrar o ambiente `DASSCANPLOT` no Jupyter notebook:

    === "Comando"
        ```bash linenums="1"
        conda activate DASSCANPLOT
        python -m ipykernel install --user --name DASSCANPLOT --display-name DASSCANPLOT
        ```

## Utilização do SCANPLOT na máquina Ilopolis

Depois de configurado o ambiente na máquina Itapemirim, utilize o Jupyter notebook disponível a partir da máquina Ilopolis:

1. Acesse a plataforma do Jupyter em seu navegador de internet pelo endereço: [http://ilopolis.cptec.inpe.br/hub/login](http://ilopolis.cptec.inpe.br/hub/login);
2. Insira as suas credenciais (as mesmas utilizadas para acessar a máquina Itepemirim);
3. O sistema abrirá a árvore de diretórios do seu diretório `/home/<usuario>`. Localize o diretório onde foi instalado o SCANTEC-2.1.0. Navegue até o diretório `scanplot` onde estará disponivel o notebook `SCANPLOT.ipynb` e abra-o; 
4. Com o arquivo aberto dentro do Jupyter, localize a aba `Kernel` e em seguida aponte para o item `Change kernel` e selecione a opção `DASSCANPLOT`. O kernel `DASSCANPLOT`, necessário para a execução desse notebook, ficará indicado na caixinha no canto superior direito da tela;
5. Utilize o notebook executando cada uma das céluas;

    !!! note "Nota"

        Caso não tenha familiaridade com o Jupyter, observe que para executar os comandos basta clicar sucessivamente no botão `Run` da interface, o cursor indicará o comando sendo executado e os resultados são apresentados nas linhas posteriores ao comando nessa mesma página. Observe que em algumas células, é necessário ajustar algumas opções e os dados para o seu usuário e o local onde o SCANTEC foi executado.

6. Acompanhe os resultados e salve as figuras que desejar em seu disco como convencionalmente é feito com seu navegador.

Assista no video a seguir, um exemplo de utilização do SCANPLOT a partir da máquina Itapemirim do CPTEC.

![type:video](https://youtube.com/embed/HOao_F0-Pi8)

Na [release V2.0.0 do SCANTEC](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/tag/V2.0.0), também está disponível um tutorial que pode ser utilizado (com algumas diferenças em relação à release mais atual) para a instalação e utilização de todo o sistema via notebook do Jupyter. Para isso abra o arquivo `Tutorial/Tutorial_SCANTEC.ipynb` e siga as instruções na página. Esse mesmo tutorial pode se obtido a partir do endereço [Tutorial.tar](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/download/V2.0.0/Tutorial.tar).
