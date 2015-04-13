clear all
clear all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Script para gerar figuras dos resultados (ACOR, RMS e VIES) do SCAMTEC (Arquivos .scam) para todas as variaveis de acordo com os parametros de
% entrada do usuario.
%
% O SCAMTEC (versao mais estavel) que gera os resultados .scam esta no repositorio:
% https://svn.cptec.inpe.br/scamtec/branches/lucamarante/SCAMTEC_v0.1@139
%
% *** Esse script deve-se ter ao cuidado de operar com a diminuicao ou acrescimo de variaveis de acordo com a versao do SCAMTEC que gerou os
% resultados .scam.
%
% O mesmo esta preparado somente para o conjunto de variavies descrito no cabeçalho abaixo
% Arquivo .scam da versao do SCAMTEC @139: 
% Previsao VTMP-925 VTMP-850 VTMP-500 TEMP-850 TEMP-500 TEMP-250 PSNM-000 UMES-925 UMES-850 UMES-500 AGPL-925 ZGEO-850 ZGEO-500 ZGEO-250 UVEL-850 
% UVEL-500 UVEL-250 VVEL-850 VVEL-500 VVEL-250
%
% Os parametros de entrada requeridos sao:
% Numero de experimentos 
% Peridodo do experimento
% Recorte do experimento
% Horas de integracao
% Nome dos experimentos (utilizado nas legendas)
% Diretorio atual dos resultados do SCAMTEC (arquivos .scam - ACOR,VIES e RMS)
% Diretorio load_data onde esta o script atual
% Ajustar manualmente a legenda nos plots ***
%
% Script: Plota_results_scam.m 
% Versao: 1.0
%
% Manutencao:Lucas Amarante
% Data:16/03/2015
%
% “Porque Deus amou ao mundo de tal maneira que deu o seu Filho unigênito, para que todo o que Nele crê não pereça, mas tenha a Vida Eterna.” (João 3.16)
% “dizendo: O tempo está cumprido, e o reino de Deus está próximo; arrependei-vos e crede no evangelho.” (Marcos 1.15)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%=======================================================================
%Parametros de entrada do usuario
%=======================================================================

%Numero de experimentos a serem comparados
Nexp=4;

%Peridodo dos experimentos
PERexp='20120605002012062518';

%Recorte do experimento
RECexp='AmericaDoSul';

%Horas de integracao
HORint=120;

%Nome dos experimentos (utilizado nas legendas)
%Ajustar manualmente a legenda nos plots das figuras ***
NOMexp01='Assimilacao GPS';
NOMexp02='Assimilacao IWV';
NOMexp03='Assimilacao GPS+IWV';
NOMexp04='CNT';


%Diretorio atual dos resultados do SCAMTEC (arquivos .scam - ACOR,VIES e RMS)
DIRatualexps='/home/amarante/Downloads/Teste_Results_SCAM/';

%diretorio load_data onde esta o script atual
DIRloaddata='/home/amarante/TRAB2015/Desk349/Plota_Results_Matlab/';


%=======================================================================
% Fim parametros de entrada do usuario
%=======================================================================

disp(' ');
disp('<< Parametros de Entrada >>  ');
disp([ 'Numero de experimentos:   ' num2str(Nexp) ]);
disp([ 'Periodo dos experimentos: ' PERexp ]); 
disp([ 'Recorte dos experimentos: ' RECexp ]); 
disp([ 'Horas de integracao:      ' num2str(HORint) ]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Ajustando os diretorios para os arquivos de entrada .scam

%extensao dos arquivos .scam para a copia
dotSCAM='*.scam';

%concatenando diretorio e extensao
CPscam=strcat(DIRatualexps,dotSCAM);

%criacao das pastas no diretorio atual de execucao
%verificando se existe ja load_data e figs
okLD=exist('load_data');
okFIG=exist('figs');

if((okLD == 7) && (okFIG == 7));
    disp('Diretorios load_data/ e figs/  ja existem!');
else
    disp('Criando diretorios load_data/ e figs/');
    mkdir('load_data');
    mkdir('figs');
end

recld=([ 'load_data/' RECexp ]);
recfig=([ 'figs/' RECexp ]);

%criacao dos diretorios especificos do recorte
mkdir(recld);
mkdir(recfig);

%variavel para nome load_data
ldt=([ 'load_data/' RECexp ]);

%variavel para saida figs
outfig=([ recfig '/' ]);

%destino dos arquivos .scam na estrutura load_data (concatenando variaveis)
DESTldt=strcat(DIRloaddata,ldt);

%Copiando os arquivos *.scam
copyfile(CPscam,DESTldt);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%loop para quantidade de experimentos
for n=1:Nexp

    %verificacao para ajuste do nome correto do arquivo .scam
     if(n < 10); 
         antN='0'; 
     else
         antN='';
     end
     
   disp([ 'Carregando o arquivo: ' num2str(n) ]);  
    
%carregando os arquivos .scam do load_data/RECORTE
%adicionando em matrizes tridimensionais
  ACOR(:,:,n)=load([ 'load_data/' RECexp '/' 'ACOREXP' antN num2str(n) '_' PERexp '.scam' ]);

  RMSE(:,:,n)=load([ 'load_data/' RECexp '/' 'RMSEEXP' antN num2str(n) '_' PERexp '.scam' ]);

  VIES(:,:,n)=load([ 'load_data/' RECexp '/' 'VIESEXP' antN num2str(n) '_' PERexp '.scam' ]);
  
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%mudanca de cores para cada experimento
cores=colormap(colorcube);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%loop para passar por todas as variaveis do SCAMTEC e para quantidade de experimentos (Nexp)
    for cont=2:21      
              
        switch cont
            case 2;  var='VTMP-925';
            case 3;  var='VTMP-850';
            case 4;  var='VTMP-500';
            case 5;  var='TEMP-850';
            case 6;  var='TEMP-500';
            case 7;  var='TEMP-250';
            case 8;  var='PSNM-000';
            case 9;  var='UMES-925';
            case 10; var='UMES-850';
            case 11; var='UMES-500';
            case 12; var='AGPL-925';
            case 13; var='ZGEO-850';
            case 14; var='ZGEO-500';
            case 15; var='ZGEO-250';
            case 16; var='UVEL-850';
            case 17; var='UVEL-500';
            case 18; var='UVEL-250';
            case 19; var='VVEL-850';
            case 20; var='VVEL-500';
            case 21; var='VVEL-250';           
        end
        
        disp([ 'Realizando case ' num2str(cont)  ' para variavel: ' var ]);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         
%plotando no grafico
       figure(1)  
      
       for n=1:Nexp               
          disp([ 'Plotando figura para ACOR ' num2str(n) ]);
          plot(ACOR(:,1,n),ACOR(:,cont,n),'o-','color',[cores(n+5,1) cores(n+5,2) cores(n+5,3)],'LineWidth',2,'MarkerFaceColor',[cores(n+5,1) cores(n+5,2) cores(n+5,3)],'MarkerSize',4);hold on
       end
       
       set(gca,'fontsize',26);

       title([ 'ACOR - ' var ' - ' RECexp ]);
       xlabel('Horas de Integracao');
       ylabel('ACOR');

       set(gca,'XLim',[0 HORint]);
       set(gca,'XTick',[0:12:HORint]);
       
       legend(NOMexp01,NOMexp02,NOMexp03,NOMexp04,'Location','SouthOutside','Orientation','horizontal');
       grid

       %salvando a figura
       figacor=([ outfig 'ACOR_' var '_' RECexp '.png' ]);
       print('-dpng', figacor);
       close(1)
     
       disp('------------');
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       figure(2)  
      
       for n=1:Nexp               
          disp([ 'Plotando figura para RMS ' num2str(n) ]);
          plot(RMSE(:,1,n),RMSE(:,cont,n),'o-','color',[cores(n+5,1) cores(n+5,2) cores(n+5,3)],'LineWidth',2,'MarkerFaceColor',[cores(n+5,1) cores(n+5,2) cores(n+5,3)],'MarkerSize',4);hold on
       end


       set(gca,'fontsize',26);

       title([ 'RMS - ' var ' - ' RECexp ]);
       xlabel('Horas de Integracao');
       ylabel('RMS');

       set(gca,'XLim',[0 HORint]);
       set(gca,'XTick',[0:12:HORint]);
       
       legend(NOMexp01,NOMexp02,NOMexp03,NOMexp04,'Location','SouthOutside','Orientation','horizontal');
       grid

       %salvando a figura
       figrms=([ outfig 'RMSE_' var '_' RECexp '.png' ]);
       print('-dpng', figrms);
       close(2)
     
       disp('------------');

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       figure(3)  
      
       for n=1:Nexp               
          disp([ 'Plotando figura para VIES ' num2str(n) ]);
          plot(VIES(:,1,n),VIES(:,cont,n),'o-','color',[cores(n+5,1) cores(n+5,2) cores(n+5,3)],'LineWidth',2,'MarkerFaceColor',[cores(n+5,1) cores(n+5,2) cores(n+5,3)],'MarkerSize',4);hold on
       end


       set(gca,'fontsize',26);

       title([ 'VIES - ' var ' - ' RECexp ]);
       xlabel('Horas de Integracao');
       ylabel('VIES');

       set(gca,'XLim',[0 HORint]);
       set(gca,'XTick',[0:12:HORint]);
       
       legend(NOMexp01,NOMexp02,NOMexp03,NOMexp04,'Location','SouthOutside','Orientation','horizontal');
       grid

       %salvando a figura
       figvies=([ outfig 'VIES_' var '_' RECexp '.png' ]);
       print('-dpng', figvies);
       close(3)
     
       disp('------------');

    end
    disp(' ');
    disp('Fim do processamento!!'); 