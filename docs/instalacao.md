# Instalação

Para a instalação, o sistema tem implementado um script criado para essa função, o qual prepara o ambiente para a instalação, entra nos diretórios, em uma sequência adequada e compila cada uma das bibliotecas e módulos que compõem o sistema e move o executável para a pasta bin. Ele está preparado para funcionar em diversos ambientes, máquinas virtuais do CPTEC, ou mesmo máquinas locais na mesa do usuário.

!!! note "Nota"

    Embora a versão idealizada deva funcionar também na máquina Tupã do CPTEC, a presente versão **SCANTEC V.2.0.0** requer recursos mais atuais do compilador Fortran, o que não é disponível ainda na Tupã e por isso não está apta. Veja instruções específicas para outras máquinas nas instruções abaixo.

##  Instalação em máquinas virtuais e locais

A máquina itapemirim foi utilizada para validar a versão, a qual foi aprovada e podem ser utilizadas sem nenhum problema reportado. Em uma outra máquina qualquer que se deseja ter o SCANTEC funcionando, é necessário ter instalado:

* Compilador Intel Fortran (`ifort`) ou o GNU Fortran (`gfortran`, versão 9 ou superior);
* Pacote Subversion (`svn` e `git`);
* Biblioteca [LAPACK - Linear Algebra PACKage](https://www.netlib.org/lapack/).

Para obter uma cópia do SCANTEC:


* Baixe a versão release de distribuição do SCANTEC (arquivo compactado extensão `.tar.gz` ou `.zip`) a partir do GitHub em [https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/tag/V2.0.0](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/tag/V2.0.0):

    === "Comando"
        ```bash linenums="1"
        wget https://github.com/GAM-DIMNT-CPTEC/SCANTEC/archive/refs/tags/V2.0.0.tar.gz
        ```

    !!! note "Nota"

        Na página de releases do SCANTEC, há outros arquivos que serão utilizados ao longo deste manual:

        * `README_SCANTEC.2.0.0.pdf`: uma cópia deste manual;
        * `Tutorial.tar`: arquivos do Jupyter notebook para uso do SCANTEC e SCANPLOT;

Para a instalação, siga os passos a seguir.

1. Faça login na máquina Itapemirim:

    === "Comando"
        ```bash linenums="1"
        ssh usuario@itapemirim.cptec.inpe.br -XC
        ```

2. Entre em um diretório de preferência do usuário para realizar a instalação do SCANTEC (`/home/usuario` por exemplo) e descompacte o pacote nele copiado:

    === "Comando"
        ```bash linenums="1"
        cd $HOME
        tar -xvf SCANTEC.2.0.0.tar
        ```

3. Entre no diretório do `SCANTEC.2.0.0`:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC.2.0.0
        ```

4. Para compilar o SCANTEC execute o script install seguindo as instruções abaixo:

    === "Comando"
        ```bash linenums="1"
        ./install
        ```
    === "Resultado"
        ```
        ------------------choose one of options-----------------
        
         1 - Clean Environment
        
         2 - GNU - Load GNU Gfortran Environment to LINUX and compile
        
         3 - CCE XE - Load Cray Environment to XT/XE and compile
        
         4 - INTEL - Load INTEL Environment to LINUX and compile
        
         E - Exit
        
        Choose one of options (1-E): 
        ```

!!! warning "Observações"

    * Escolha a opção `2` para utilizar o compilador Gfortran, que deverá ser a versão 9 ou melhor;
    * Acompanhe a compilação com as informações no terminal;
    * Verifique o sucesso do processo identificando o arquivo `SCANTEC.2.0.0/bin/scantec.x`;
    * Caso algum problema seja detectado, ou precise compilar novamente, use a opção `1` para limpar a compilação anterior.
