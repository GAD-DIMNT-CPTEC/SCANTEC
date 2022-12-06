# Instalação

Para a instalação do SCANTEC, o sistema tem implementado um script que prepara o ambiente para a instalação, a qual envolve a compilação de bibliotecas e módulos que compõem o sistema. Ao final do processo, é gerado o executável `scantec.x`, que é então alocado no diretório `bin`. O SCANTEC está preparado para funcionar em diversos ambientes computacionais, como as máquinas virtuais do CPTEC ou mesmo máquinas locais na mesa do usuário.

!!! note "Nota"

    Idealmente, o SCANTEC deve funcionar também na máquina Tupã do CPTEC, porém, a presente versão **SCANTEC V.2.1.0** requer recursos mais atuais do compilador Fortran, os quais não estão disponíveis na máquina. Apesar disso, ainda há a possibilidade de se utilizar o SCANTEC a partir de um container. Instruções específicas para a utilização do SCANTEC em outras máquinas são fornecidas ao longo desta seção.

##  Instalação em máquinas virtuais e locais

A máquina Itapemirim (baseada no Ubuntu 18.04, kernel 4.4.0-200-generic) foi utilizada para validar o SCANTEC V2.1.0. Em uma outra máquina qualquer que se deseja ter o SCANTEC para uso, é necessário ter instalado os seguintes requerimentos:

* Compilador Intel Fortran (`ifort`) ou o GNU Fortran (`gfortran`, versão 9 ou superior);
* Pacote Subversion e Git (`svn` e `git`);
* Biblioteca [LAPACK - Linear Algebra PACKage](https://www.netlib.org/lapack/).

Com os requisitos instalados na máquina, para obter uma cópia do SCANTEC basta seguir as instruções abaixo.

* Baixe a release de distribuição do SCANTEC V2.1.0 (arquivo compactado extensão `.tar.gz` ou `.zip`) a partir do GitHub em [https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/tag/V2.1.0](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/tag/V2.1.0):

    === "Comando"
        ```bash linenums="1"
        wget https://github.com/GAM-DIMNT-CPTEC/SCANTEC/archive/refs/tags/V2.1.0.tar.gz
        ```

    !!! note "Nota"

        Na página de release do SCANTEC V2.0.0, há outros arquivos que fazem parte da release e que podem ser úteis no entendimento e utilização da versão:

        * `README_SCANTEC.2.0.0.pdf`: uma cópia deste manual;
        * `Tutorial.tar`: arquivos do Jupyter notebook para uso do SCANTEC e SCANPLOT (um sistema de visualização).

Para a instalação do SCANTEC, siga os passos a seguir.

1. Faça login na máquina Itapemirim:

    === "Comando"
        ```bash linenums="1"
        ssh usuario@itapemirim.cptec.inpe.br -XC
        ```

2. Entre em um diretório da preferência do usuário para realizar a instalação do SCANTEC (e.g., `/home/usuario`) e descompacte o pacote nele copiado:

    === "Comando"
        ```bash linenums="1"
        cd $HOME
        tar -xvf SCANTEC.2.1.0.tar
        ```

3. Entre no diretório criado `SCANTEC.2.1.0`:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC.2.1.0
        ```

4. Execute o script `install`, seguindo as instruções abaixo:

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

    * Escolha a opção `2` para utilizar o compilador `gfortran`, que deverá ser a versão 9 ou superior;
    * Acompanhe a compilação com as informações no terminal;
    * Verifique o sucesso do processo identificando o arquivo executável `SCANTEC.2.1.0/bin/scantec.x`;
    * Caso algum problema seja detectado ou precise compilar novamente, utilize a opção `1` para limpar a compilação anterior e reinicie o processo.
