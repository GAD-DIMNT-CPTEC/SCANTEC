# Instalação

Para a instalação do SCANTEC, o sistema possui um script que prepara o ambiente para a instalação, o qual envolve a compilação de bibliotecas e módulos que integram o sistema. Ao final do processo, é gerado o executável `scantec.x`, que é então alocado no diretório `bin`. O SCANTEC está preparado para funcionar em diversos ambientes computacionais, como as máquinas virtuais do CPTEC (e.g., Itapemirim), e supercomputadores XC50 e Egeon. O SCANTEC também pode ser utilizado em máquinas locais na mesa do usuário, sejam elas workstations ou mesmo notebooks.

!!! note "Nota"

    Idealmente, o SCANTEC deve funcionar também na máquina Tupã do CPTEC, porém, a presente versão **SCANTEC V.2.1.0** requer recursos mais atuais do compilador Fortran, os quais não estão disponíveis na máquina. Apesar disso, ainda há a possibilidade de se utilizar o SCANTEC a partir de um container. Instruções específicas para a utilização do SCANTEC em outras máquinas são fornecidas ao longo desta seção.

##  Instalação em máquinas virtuais e locais

A máquina Itapemirim (baseada no Ubuntu 18.04, kernel 4.4.0-200-generic) foi utilizada para validar o SCANTEC V2.1.0. Em uma outra máquina em que se deseja ter o SCANTEC para uso, é necessário ter instalado os seguintes requerimentos:

* Compilador Intel Fortran (`ifort`) ou o GNU Fortran (`gfortran`, versão 9 ou superior);
* Pacote Subversion e Git (`svn` e `git`);
* Biblioteca [LAPACK - Linear Algebra PACKage](https://www.netlib.org/lapack/).

!!! info "Informação"

    Para a [release do SCANTEC V2.0.0](https://github.com/GAM-DIMNT-CPTEC/SCANTEC/releases/tag/V2.0.0), há outros arquivos que fazem parte da distribuição e que podem ser úteis no entendimento e utilização do SCANTEC:

    * `README_SCANTEC.2.0.0.pdf`: uma cópia do manual de uso para a V2.0.0;
    * `Tutorial.tar`: arquivos do Jupyter notebook para uso do SCANTEC e SCANPLOT (um sistema de visualização dos resultados do SCANTEC).

    Embora estes arquivos pertençam à release V2.0.0, eles podem também ser considerados pelos usuários caso as informações apresentadas neste manual não sejam suficientes.

Para a instalação do SCANTEC, realize os procedimentos a seguir. ==Não se esqueça de substituir as palavras `<grupo>` e `<usuario>` pelos nomes do seu grupo e usuário, respectivamente.==

1. Faça login na máquina Itapemirim:

    === "Comando"
        ```bash linenums="1"
        ssh <usuario>@itapemirim.cptec.inpe.br -XC
        ```

2. Entre no diretório `/scripts/<grupo>/usuario` (recomendado) ou em um diretório da preferência do usuário para realizar a instalação do SCANTEC. Faça o download da release V2.1.0 e descompacte o arquivo baixado:

    === "Comando"
        ```bash linenums="1"
        cd /scripts/<grupo>/<usuario>
        wget https://github.com/GAM-DIMNT-CPTEC/SCANTEC/archive/refs/tags/V2.1.0.tar.gz
        tar -zxvf V2.1.0.tar.gz
        ```

    !!! note "Nota"

        Embora o nome do pacote baixado seja `V2.1.0.tar.gz`, o comando `tar -zxvf V2.1.0.tar.gz` criará o diretório `SCANTEC-2.1.0` no local escolhido pelo usuário.

3. Entre no diretório criado `SCANTEC-2.1.0`:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC-2.1.0
        ```

4. Carregue o ambiente `SCANTEC` para a compilação do SCANTEC:

    * Na máquina Itapemirim, carregue o ambiente (disponível na máquina) com o comando:    

        === "Comando"
            ```bash linenums="1"
            source source /scripts/das/conda/envs/SCANTEC/bin/activate
            ```

    * Em outras máquinas esta etapa não é necessária, mas certifique-se de que há pelo menos o compilador GNU Fortran versão 9 ou superior instalado.

5. Execute o script `install`:

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

    !!! note "Notas"
    
        * Escolha a opção `2` para utilizar o compilador `gfortran`, que deverá ser a versão 9 ou superior;
        * Acompanhe a compilação com as informações no terminal;
        * Verifique o sucesso do processo identificando o arquivo executável `SCANTEC-2.1.0/bin/scantec.x`;
        * Caso algum problema seja detectado ou seja necessário compilar novamente, utilize a opção `1` para limpar a compilação anterior e reinicie o processo.
