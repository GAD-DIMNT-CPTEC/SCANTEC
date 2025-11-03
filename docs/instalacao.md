# Instalação

Para a instalação do SCANTEC, o sistema possui um script que prepara o ambiente para a instalação, o qual envolve a compilação de bibliotecas e módulos que integram o sistema. Ao final do processo, é gerado o executável `scantec.x`, que é então alocado no diretório `bin`. O SCANTEC está preparado para funcionar em diversos ambientes computacionais, como as máquinas virtuais do CPTEC (e.g., Itapemirim), e supercomputadores XC50. O SCANTEC também pode ser utilizado em máquinas locais na mesa do usuário, sejam elas workstations ou mesmo notebooks.


## Instalação na máquina Egeon do CPTEC

A partir da versão **SCANTEC V2.1.0**, a utilização do SCANTEC tem como requisito a versão 12.2.0 do Gfortran, a qual não está disponível na máquina. Para contornar este problema, os usuários podem criar um ambiente do conda e instalar esta versão do compilador Gfortran. Veja a seguir como construir o ambiente para instalar e executar o SCANTEC na mquina Egeon. 

### Construção do ambiente conda para a instalação do SCANTEC na Egeon

Na Egeon, siga os passos a seguir para construir um ambiente (na sua conta de usuário) para instalar e utilizar o SCANTEC na máquina Egeon:

1. Carregar o módulo do anaconda:

    === "Comando"
        ```bash linenums="1"
        module load anaconda3-2022.05-gcc-11.2.0-q74p53i
        ```

2. Criar um ambiente conda para a instalação do Gfortran 12.2.0:

    === "Comando"
        ```bash linenums="1"
        conda create -n SCANTEC python=3.11.0
        ```
3. Ativação do ambiente criado:

    === "Comando"
        ```bash linenums="1"
        conda activate SCANTEC
        ```
4. Instalação do Gfortran 12.2.0:

    === "Comando"
        ```bash linenums="1"
        conda install conda-forge::gfortran=12.2.0
        ```

5. Instalação do SCANTEC:

    Com o ambiente do conda preparado e ativado, a instalação do SCANTEC na Egeon deve ser feita dentro desse ambiente, de forma a garantir que o compilador e as bibliotecas necessárias sejam corretamente utilizadas. Para isso, proceda da seguinte forma:

    * Obtenha a release mais recente do SCANTEC (atualmente a versão V2.2.0):

        === "Comando"
            ```bash linenums="1"
            wget -c https://github.com/GAD-DIMNT-CPTEC/SCANTEC/archive/refs/tags/V2.2.0.tar.gz
            ```
        
    * Desempacote do código e entre no diretório criado `SCANTEC`:
    
        === "Comando"
            ```bash linenums="1"
            tar -zxvf V2.2.0.tar.gz
            cd SCANTEC-2.2.0
            ```
    
    * Execute o script `install` e escolha a opção `2 - GNU - Load GNU Gfortran Environment to LINUX and compile`:
    
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

!!! note "Nota"

    Na máquina Egeon, quando o SCANTEC for utilizado, será necessário exportar a variável de ambiente a seguir antes de utilizar o executável `scantec.x`:

    === "Comando"
        ```bash linenums="1"
        export LD_LIBRARY_PATH=$CONDA_PREFIX/lib:$LD_LIBRARY_PATH
        ```

##  Instalação em máquinas virtuais e locais

A máquina Itapemirim (baseada no Ubuntu 18.04, kernel 4.4.0-200-generic) foi utilizada para validar o SCANTEC V2.2.0. Em uma outra máquina em que se deseja ter o SCANTEC para uso, é necessário ter instalado os seguintes requerimentos:

* Compilador Intel Fortran (`ifort`) ou o GNU Fortran (`gfortran`, versão 9 ou superior);
* Pacote Subversion e Git (`svn` e `git`);
* Biblioteca [LAPACK - Linear Algebra PACKage](https://www.netlib.org/lapack/).

!!! info "Informação"

    Essa versão contém algumas atualizações como um script para conversão de dados NETCDF para .grb além de atualização dos dados de teste.

Para a instalação do SCANTEC, realize os procedimentos a seguir. ==Não se esqueça de substituir as palavras `<grupo>` e `<usuario>` pelos nomes do seu grupo e usuário, respectivamente.==

1. Faça login na máquina Itapemirim:

    === "Comando"
        ```bash linenums="1"
        ssh <usuario>@itapemirim.cptec.inpe.br -XC
        ```

2. Entre no diretório `/scripts/<grupo>/usuario` (recomendado) ou em um diretório da preferência do usuário para realizar a instalação do SCANTEC:

    === "Comando"
        ```bash linenums="1"
        cd /scripts/<grupo>/<usuario>
        wget -c https://github.com/GAD-DIMNT-CPTEC/SCANTEC/archive/refs/tags/V2.2.0.tar.gz
        ```

2. Descompacte o código baixado e entre no diretório criado:

    === "Comando"
        ```bash linenums="1"
        tar -zxvf V2.2.0.tar.gz
        cd SCANTEC
        ```
        
3. Carregue o ambiente `SCANTEC` para a compilação do SCANTEC:

    Para usuários do grupo DAS, a máquina Itapemirim, carregue o ambiente (disponível em `/scripts/das/conda/envs/SCANTEC`) com o comando:    

    === "Comando"
        ```bash linenums="1"
        source /scripts/das/conda/envs/SCANTEC/bin/activate
        ```

    !!! note "Nota"
            
        Para usuários fora do grupo DAS, é preciso copiar um pacote do ambiente conda do SCANTEC, desempacotá-lo para ativar esse ambiente e ajustar os endereços com o comando `conda-pack`. Para isso faça os commandos abaixo:
    
        * Copie o pacote SCANTEC.tar para sua instalação local do conda:
    
            === "Comando"
                ```bash linenums="1"
                cp /scripts/das/conda/envs/SCANTEC.tar ~/conda/envs
                ```
        	    
        * Desempacotar o ambiente:
  
            === "Comando"
                ```bash linenums="1"
                cd ~/conda/envs ; tar -xvf SCANTEC.tar
                ```
        
        * Ativar o ambiente:
  
            === "Comando"
                ```bash linenums="1"
                source ~/conda/envs/SCANTEC/bin/activate
                ```
        
        * Executar o conda-unpack para corrigir/atualizar os paths:
  
            === "Comando"
                ```bash linenums="1"
                conda-unpack
                ```
       
        Em outras máquinas esta etapa não é necessária, mas certifique-se de que há pelo menos o compilador GNU Fortran versão 9 ou superior instalado.

4. Execute o script `install`:

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

## Instalação do SCANTEC em maquina laptops no windows via WSL

A instalação do SCANTEC em máquinas com o sistema operacional Windows pode ser feita através do WSL.
Um guia de instalação mais detalhado pode ser encontrado através do link: [https://learn.microsoft.com/pt-br/windows/wsl/install](https://learn.microsoft.com/pt-br/windows/wsl/install).

O WSL (Windows Subsystem for Linux) é uma funcionalidade do Windows que permite executar um ambiente Linux diretamente no Windows, sem a necessidade de uma máquina virtual. Ele possibilita a execução de comandos e ferramentas do Linux nativamente, proporcionando integração entre os sistemas operacionais e facilitando o desenvolvimento de software que precisa utilizar recursos do Linux enquanto ainda opera no ambiente Windows.

### Requisitos para instalação

Para instalar o WSL você deve estar executando o Windows 10 ou versões superiores.

### Comandos para a instalação

No Prompt de Comando do Windows digite a seguinte linha de código:

=== "Comando"

   ```linenums="1"
   wsl --install
   ```
   ![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/72c1201d-aa1b-4751-b788-aa43ffc8fb28)


A versão defaut que será instalada será UBUNTU. Reinicie a máquina para que as novas atualizações sejam feitas. Com a máquina reiniciada, busque pela aplicação linux instalada e faça o devido processo de "cadastro" de login e senha.

![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/11fa35a7-be67-4d4f-95cc-fd5a2c7193a5)

Pronto! Um terminal linux está instalado em sua máquina.

### Instalação do SCANTEC

1. Instale os programas e as bibliotecas necessárias:

    === "Comando"
    
        ```linenums="1"
        sudo apt update
        sudo apt upgrade
        sudo apt install wget make gfortran
        ```
        ![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/63117044-8b0c-4454-b993-da725b3e91c0)
    
    Verifique a instalação do Gfortran com o comando:
    
    === "Comando"
    
        ```linenums="1"
        gfortran --version
        ```
        ![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/9b162cfc-fb5c-43ce-9509-7240b88a7944)

2. Baixe a release V2.2.0 do SCANTEC:

    === "Comando"

        ```linenums="1"
        wget -c https://github.com/GAD-DIMNT-CPTEC/SCANTEC/archive/refs/tags/V2.2.0.tar.gz
        ```

3. Desempacote do código baixado:

    === "Comando"
        ```bash linenums="1"
        tar -zxvf V2.2.0.tar.gz
        cd SCANTEC-2.2.0
        ```

4. Compilação do SCANTEC:

    Com as bibliotecas e compilador instalados, entre no diretório onde o SCANTEC foi instalado.
    
    Digite o comando no terminal linux
    
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
    
    Escolha a opção 2 para compilação com o Gfortran.
