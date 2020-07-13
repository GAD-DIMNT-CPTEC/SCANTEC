# SCANPLOT - Um sistema simples de plotagem para o SCANTEC (V1.0.0)

Source: https://svn.cptec.inpe.br/scamtec/tags/SCANPLOT_V1.0.0/ via comando export do svn

24 de Junho de 2020

Este pacote é composto do módulo principal scanplot.py, do notebook Jupyter SCANPLOT.ipynb e do arquivo SCANPLOT.yml. O módulo scanplot.py contém funções que são utilizadas para transformar as tabelas do SCANTEC em dataframes do Pandas. Outras funções do scanplot.py foram preparadas para facilitar o processo de plotagem e servem como template e/ou exemplos para que os usuários possam desenvolver as suas próprias funções, as quais podem também ser incluídas no SCANPLOT em versões futuras.

## Notas desta versão

Nesta versão V1.0.0, estão incluídas as seguintes funções:

1. read_nemalists: lê os namelists e arquivos de definições do SCANTEC;
2. get_dataframe : transforma as tabelas do SCANTEC em dataframes;
3. plot_lines    : plota gráficos de linha com os dataframes das tabelas do SCANTEC;
4. plot_scorecard: resume as informações dos dataframes com as tabelas do SCANTEC em scorecards;    
5. plot_dTaylor  : plota diagramas de Taylor a partir de dois experimentos utilizando os dataframes com as tabelas do SCANTEC.

Desenvolvido pelos colaboradores do CPTEC, para os colaboradores do CPTEC.

## Uso na máquina Ilopolis

O SCANPLOT está preparado para ser utilizado na máquina Ilopolis, por meio do Jupyter-Hub. O Jupyter-Hub é um servidor do Jupyter e permite que os usuários do SCANPLOT possam utilizar o módulo scanplot.py para explorar as tabelas do SCANTEC e plotar os gráficos com as estatísticas calculadas. O acesso ao Jupyter-Hub da máquina Ilopolis, pode ser feito a partir do seguinte endereço: http://ilopolis.cptec.inpe.br. Para logar, o usuário deverá inserir seu login e senha do NIS.

No ambiente do Jupyter-Hub na máquina Ilopolis, o usuário terá acesso à sua estrutura de arquivos. O usuário poderá obter uma cópia do SCANPLOT com o seguinte comando (através da linha de comando*):

$ svn export https://svn.cptec.inpe.br/scamtec/tags/SCANPLOT_V1.0.0

Recomenda-se que esta cópia seja feita dentro da área de scripts do usuário em /scripts/grupos/usuario.

*A interface do Jupyter-Hub permite que o usuário abra uma seção do terminal, onde o comando acima poderá ser digitado. Para abrir uma seção do terminal a partir da interface do Jupyter-Hub, clique em 'New > Other > Terminal'. Se o usuário preferir, poderá efetuar login na máquina Ilopolis via SSH.

Com a cópia do SCANPLOT no seu diretório de trabalho, a partir da interface do Jupyter-Hub, o usuário deverá procurar pelo arquivo SCANPLOT.ipynb e seguir as instruções que lá se encontram. É importante notar que um ambiente específico para o uso do SCANPLOT deverá ser configurado. No arquivo SCANPLOT.yml estão listados os pacotes e versões necessários para a configuração do ambiente. Contate o administrador do seu grupo e solicite a criação deste ambiente.

## Uso em máquina local

É possível utilizar o pacote SCANPLOT em outras máquinas além da Ilopolis. Para isso configurar um ambiente de uso e desenvolvimentos do SCANPLOT, recomenda-se a instalação do pacote Anaconda Python e dos pacotes que estão listados no arquivo SCANPLOT.yml.

Com o Anaconda Pyhton instalado na máquina, o usuário deverá revisar o valor do parâmetro 'prefix: /scripts/das/conda/envs/SCANPLOT' e ajustar de acordo com a sua necessidade e preferência.

Para criar o ambiente para o uso do SCANPLOT, utilize o comando a seguir:

$ conda env create -f SCANPLOT.yml

Após a criação do ambiente, o usuário deverá ativá-lo:

$ conda activate SCANPLOT

O SCANPLOT poderá ser utilizado tanto pela linha de comando quando pela interface do Jupyter (recomendado). Para abrir uma seção do Jupyter, basta digitar (dentro do ambiente SCANPLOT):

$ jupyter-notebook

Pelo navegador, na interface do Jupyter, o usuário deverá selecionar o kernel adequado. Para isso, deverá clicar em 'Kernel > Change kernel > SCANPLOT'. O kernel SCANPLOT (ou outro nome que faça referência ao SCANPLOT), é o nome do ambiente de desenvolvimntos que foi criado na etapa anterior.

## Notas finais

O desenvolvimento do SCANPLOT está aberto para a contribuição de todos os usuários. O código principal está disponível no repositório do SCANTEC em https://projetos.cptec.inpe.br/projects/scamtec e um fóruns para discussões e troca de idéias sobre as funções e cenários de uso do SCANTEC e do SCANPLOT estão disponíveis em https://projetos.cptec.inpe.br/projects/scamtec/boards.

Grupo de Desenvolvimento em Assimilação de Dados
