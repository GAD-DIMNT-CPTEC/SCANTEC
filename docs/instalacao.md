# Instalação

Para a instalação do SCANTEC, o sistema possui um script que prepara o ambiente para a instalação, o qual envolve a compilação de bibliotecas e módulos que integram o sistema. Ao final do processo, é gerado o executável `scantec.x`, que é então alocado no diretório `bin`. O SCANTEC está preparado para funcionar em diversos ambientes computacionais, como as máquinas virtuais do CPTEC (e.g., Itapemirim), e supercomputadores XC50. O SCANTEC também pode ser utilizado em máquinas locais na mesa do usuário, sejam elas workstations ou mesmo notebooks.

!!! note "Nota"

    Idealmente, o SCANTEC deve funcionar também na máquina EGEON do CPTEC, porém, a presente versão **SCANTEC V.2.1.0** requer recursos mais atuais do compilador Fortran, os quais não estão disponíveis na máquina. Apesar disso, ainda há a possibilidade de se utilizar o SCANTEC a partir de um container. Instruções específicas para a utilização do SCANTEC em outras máquinas são fornecidas ao longo desta seção.

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

2. Entre no diretório `/scripts/<grupo>/usuario` (recomendado) ou em um diretório da preferência do usuário para realizar a instalação do SCANTEC. Faça o download utilizando o github https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER:

    === "Comando"
        ```bash linenums="1"
        cd /scripts/<grupo>/<usuario>
        git clone https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER.git
        cd SCANTEC-MASTER
        ```
        
4. Entre no diretório criado `SCANTEC-MASTER`:

    === "Comando"
        ```bash linenums="1"
        cd SCANTEC-MASTER
        ```

5. Carregue o ambiente `SCANTEC` para a compilação do SCANTEC:

    * 5.1 Para usuarios do grupo DAS, aa máquina Itapemirim, carregue o ambiente (disponível em /scripts/das/) com o comando:    

        === "Comando"
            ```bash linenums="1"
            source /scripts/das/conda/envs/SCANTEC/bin/activate
            ```
    * 5.2 Para usuarios fora do grupo DAS, é preciso copiar um pacote do ambiente conda do SCANTEC, desempacota-lo ativar esse ambiente e ajustar os endereços com o comando conda-pack. Para isso faça os commandos abaixo:

      -5.2.1 Copie o pacote SCANTEC.tar para sua  instalação local do conda:

        === "Comando"
            ```bash linenums="1"
            cp /scripts/das/conda/envs/SCANTEC.tar ~/conda/envs
            ```
      -5.2.2  Desempacotar o ambiente:

        === "Comando"
            ```bash linenums="1"
            cd ~/conda/envs ; tar -xvf SCANTEC.tar
            ```
      -5.2.3  Ativar o ambiente:

        === "Comando"
            ```bash linenums="1"
            source ~/conda/envs/SCANTEC/bin/activate
            ```
     -5.2.4   Executar o conda-unpack para corrigir/atualizar os paths:

        === "Comando"
            ```bash linenums="1"
            conda-unpack
            ```
    * Em outras máquinas esta etapa não é necessária, mas certifique-se de que há pelo menos o compilador GNU Fortran versão 9 ou superior instalado.

6. Execute o script `install`:

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

## Instalação do SCANTEC em maquina leptops no windows via WSL

A instalação do SCANTEC em máquinas com o sistema operacional Windows pode ser feita através do WSL.
Um guia de instalação mais detalhado pode ser encontrado através do link: https://learn.microsoft.com/pt-br/windows/wsl/install

### WSL

O WSL (Windows Subsystem for Linux) é uma funcionalidade do Windows que permite executar um ambiente Linux diretamente no Windows, sem a necessidade de uma máquina virtual. Ele possibilita a execução de comandos e ferramentas do Linux nativamente, proporcionando integração entre os sistemas operacionais e facilitando o desenvolvimento de software que precisa utilizar recursos do Linux enquanto ainda opera no ambiente Windows.

### Requisitos para instalação

Para instalar o WSL você deve estar executando o Windows 10 ou versões superiores.

### Comandos para a instalação

No Prompt de Comando do Windows digite a seguinte linha de código:
```
wsl --install
```
![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/72c1201d-aa1b-4751-b788-aa43ffc8fb28)


A versão defaut que será instalada será UBUNTU. Reinicie a máquina para que as novas atualizações sejam feitas. Com a máquina reiniciada, busque pela aplicação linux instalada e faça o devido processo de "cadastro" de login e senha.

![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/11fa35a7-be67-4d4f-95cc-fd5a2c7193a5)
Pronto! Um terminal linux está instalado em sua máquina.

### Instalação do SCANTEC

Entre na página https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER e procure pela barra azul Code. Vá na aba HTTPS, copie o endereço https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER.git e digite o código no terminal linux

```
git clone https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER.git
```

![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/6b5618af-e7ab-42a4-9c53-d4aa435f6c51)

Instale as bibliotecas necessárias. (você irá precisar da senha que você cadastrou). No terminal linux digite as seguintes linhas de código:
```
sudo apt install make
```
![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/63117044-8b0c-4454-b993-da725b3e91c0)
```
sudo apt update

sudo apt install gfortran
```
Verifique a instalação do Gfortran com o comando:
```
gfortran --version
```
![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/9b162cfc-fb5c-43ce-9509-7240b88a7944)

### Compilação do SCANTEC

Com as bibliotecas e compilador instalados, entre no diretório onde o SCANTEC foi instalado.

Digite o comando no terminal linux
```
./install
```
Escolha a opção 1 para limpar o ambiente. Faça esse procedimento novamente porém agora digite 2 (opção para compilação do Gfortran)
![image](https://github.com/GAD-DIMNT-CPTEC/SCANTEC-MASTER/assets/71741679/1aa5f665-93d6-4acb-9d2d-76bb81551a43)


