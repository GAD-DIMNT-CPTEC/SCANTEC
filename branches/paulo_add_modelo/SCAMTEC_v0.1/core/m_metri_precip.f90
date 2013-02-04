MODULE m_metri_precip
USE SCAM_dataMOD
USE SCAM_bstatistic
USE m_ioutil
USE SCAM_Utils 
USE time_module
USE m_string
USE scamtec_module

IMPLICIT NONE


CONTAINS
  
  SUBROUTINE precipitation()

  INTEGER              :: t, e, f,i,j                         !Variaveis de loop
  INTEGER              :: time, ftime, nymd, nhms, fymd, fhms !Variaveis do tempo 
  INTEGER              :: time_ant, nymd_ant, nhms_ant        !Variaveis do tempo anterior 
  INTEGER              :: quant_arq_ant                       !Quantidade de arquivos anterior  
  CHARACTER(len=1024)  :: Reference                           !Reference File Name
  CHARACTER(len=1024)  :: Experiment                          !Experiment File Name
  CHARACTER(len=1024)  :: Precipi                             !Precipitation File Name
  Character(len=3)     :: tempo_char                          !variavel para converter inteiro para char paulo dias
  INTEGER, ALLOCATABLE :: histo(:), obs_histo(:)              !variavel do histograma
  INTEGER, ALLOCATABLE :: total_histo(:,:,:,:)                !variavel do histograma
  INTEGER, ALLOCATABLE :: tempo(:)                            !Intervalo de tempo ex(00,06,12...)
  REAL,ALLOCATABLE     :: obs_precip(:,:), ant_obs_precip(:,:)!Variavel de precipitation
  INTEGER :: tam_hist
  
  
  
  !--------------------------------------------------------------------------------------------- paulo dias
    !
    !Variaveis para EOF
    !
    
    ! PARAMETERS
    !
    !  Fanom  [input] = field anomalies
    !  Ndim   [input] = dimension of rows of fanom as declared in the
    !                   calling program; ndim must be >= npts
    !  Fvar  [output] = fraction of variance accounted for by EOFs
    !  Feof  [output] = neof corresponding eigenvectors
    !  Ieof   [input] = first dimension of feof as declared in the
    !                   calling program
    !  Jeof   [input] = dimension of fvar and second dimension of feof
    !  Cut    [input] = determines how many EOFs should be computed;
    !                   cut < 1.0: cut gives the relative variance
    !                   explained by the computed EOFs;
    !                   cut > 1.0: cut is the number of EOFs to be
    !                   computed
    !  Neof  [output] = number of eigenvalues computed according to the
    !                   value of  cut
    !  Trace [output] = trace of covariance matrix
    !  Npts   [input] = number of (grid) points in input field
    !  Nflds  [input] = number of fields in data set
    
    
    ! Variaveis que entra na rotina EOF
    Real, Dimension(scamtec%nexp,int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),scamtec%ntime_steps)                        :: Fanom ! Fanom(e,x*Y,t)
    Real, Dimension(scamtec%nexp,int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),scamtec%ntime_steps,scamtec%ntime_forecast) :: precip_tempo   !precip_tempo(e,x*y,t,f)
    Real, Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),scamtec%ntime_steps)                                     :: precip_obs_eof !precip_obs_eof(x*y,t)
    Real      :: Cut
    Integer   :: Ndim !nx_pontos * ny+pontos
    Integer   :: Ieof
    Integer   :: Jeof	
    Integer   :: Npts
    Integer   :: Nflds
    
    ! Variaveis que sai da rotina EOF
    Real (Kind=4), Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),4)   :: Feof
    Real (Kind=4), Dimension(4)        :: Fvar    
    Real (Kind=4)              	          :: Trace
    Integer   :: Neof    
    
    
    
    !teste-----    
    Real,Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3))) :: vet_precp
    Integer              :: cont
    !-----------
    
    print*, ''
    print*, 'teste', scamtec%gridDesc(2), scamtec%gridDesc(3)
    print*, ''
    
   ! nx = int(scamtec%gridDesc(2))
   ! ny = int(scamtec%gridDesc(3))
   
   
   
    Ndim=int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3))
    Ieof=Ndim
    Npts=Ndim
    !Fanom=0
    Jeof=4
    Fvar=0.0
    Cut=4.0
    Neof=0
    Trace=0.0
    Feof=0.0
    Nflds=scamtec%ntime_steps
    
    
   ! CALL eof(20   ,Ndim,Fvar,Feof,Ieof,Jeof,Cut,Neof,Trace,Npts,Nflds)
   !      eof(Fanom,Ndim,Fvar,Feof,Ieof,Jeof,Cut,Neof,Trace,Npts,Nflds)     
    
    !--------------------------------------------------------------------------------------------- paulo dias
  
  
  
  
  
  
  tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2    !Calculando o tamanho do histograma
  quant_arq_ant=hist%acumulo_exp/hist%acumulo_obs     !Calculando quantidade de arquivos anterior para abrir
  
  ALLOCATE(histo(tam_hist))	   
  ALLOCATE(obs_histo(tam_hist))	   
  ALLOCATE(tempo(scamtec%ntime_forecast))
  ALLOCATE(total_histo(scamtec%ntime_steps,scamtec%ntime_forecast,scamtec%nexp,tam_hist))
  ALLOCATE(obs_precip(scamtec%nxpt,scamtec%nypt))
  ALLOCATE(ant_obs_precip(scamtec%nxpt,scamtec%nypt))  
  
  !Zerando variaveis
  histo(:)=0
  obs_histo(:)=0
  tempo(:)=0
  total_histo(:,:,:,:)=0
  
  
  
  !Iniciando os loops de tempos
  time=starting_time
  
  
 
 
    
  
  DO t=1,scamtec%ntime_steps !quantidade de diasnymd
      time_ant=jul2cal(cal2jul(time)-(scamtec%incr/2)*7)
      nymd = time/100
      nhms = MOD(time,100) * 10000
      
      
      !zrendo variaveis de obs_precip
      ant_obs_precip(:,:)=0
      obs_precip(:,:)=0
      
    	
      !
      ! 1.1 Create file name and Open Reference data file 
      !
  
      do j=1, quant_arq_ant
      	
      	ant_obs_precip(:,:)=0
      	
	      !tempo anterior
	      nymd_ant = (time_ant/100)
	      nhms_ant = MOD(time_ant,100) * 10000
	      
	      
	      Precipi = TRIM(precip%file)
	      CALL str_template(Precipi, nymd_ant,nhms_ant)
	      CALL ldata('P', 1, precip%Id, Precipi)
	      
	      ant_obs_precip(:,:)=scamdata(1)%prefield(:,:,16)
	      
	      obs_precip(:,:)=obs_precip(:,:)+ant_obs_precip(:,:)
	      
	      time_ant=jul2cal(cal2jul(time_ant)+(scamtec%incr/2))    
   
      enddo
      
   ! Preenchendo matris de obs_preciptacao para eof
      cont=0
      do j=1, int(scamtec%gridDesc(3))
                do i=1, int(scamtec%gridDesc(2))
                    cont=cont+1
                    precip_obs_eof(cont,t)=obs_precip(i,j)
                enddo
      enddo
  !----------------------------------------------------------    
            
      obs_histo(:)=0
      histo(:)=0
      
      CALL histograma(obs_precip(:,:),hist%rang,hist%valor_min,hist%valor_limit,histo)
          
      obs_histo(:)=histo(:)
      DO i=1, tam_hist
                 
                 print*,t,obs_histo(i)		 	
       ENDDO
      
      
      !
      ! 1.4 Loop over time forecast
      !
       
      ftime = time 

      DO f = 1, scamtec%ntime_forecast !quantidade de intervalo de tempo
  
          fymd = ftime/100
          fhms = MOD(ftime,100) * 10000
  
  
         DO e = 1, scamtec%nexp !numeros de experimento 
          	
             !
             ! 1.3.1 Create Experiment File Names
             !
             Experiment = TRIM(Exper(e)%file)
             CALL str_template(Experiment, fymd, fhms, nymd, nhms)
             
             CALL ldata('E',e,Exper(e)%Id, Experiment)
             
             print*, ''
             print*, 'Preenchendo matris de anomalia preciptacao', scamtec%ntime_forecast, scamtec%ntime_steps
             print*, ''
             
             !Preenchendo Matriz de preciptacao para rotina EOF    
                                      
             cont=0
             do j=1, int(scamtec%gridDesc(3))
                do i=1, int(scamtec%gridDesc(2))
                cont=cont+1     
                
                precip_tempo(e,cont,t,f)=scamdata(e)%expfield(i,j,hist%tipo_precip)                
                
                enddo
             enddo     
                                    
             
            ! Histograma
             histo(:)=0
             tempo(f)=(f-1)*time_step 	     
                          
             CALL histograma(scamdata(e)%expfield(:,:,hist%tipo_precip),hist%rang,hist%valor_min,hist%valor_limit,histo)    
                                         
             DO i=1, tam_hist
                          	
             	if (f .eq. 1) then
             	    scamdata(e)%time_histo(i,f)= obs_histo(i)
             	    total_histo(t,f,e,i)=obs_histo(i)          
             	       		
             	else
             	    scamdata(e)%time_histo(i,f)= histo(i)
             	    total_histo(t,f,e,i)=histo(i)                 		
             	endif
             	print*,t,tempo(f),e,total_histo(t,f,e,i)
             	           	 	
            ENDDO
    
          ENDDO ! fim do loop do experimento
  
          ftime = jul2cal(cal2jul(ftime)-scamtec%incr)
  
          if (t.eq.scamtec%ntime_steps)then
              DO e=1,scamtec%nexp
                  !call escreve_histograma(e,tempo,histo,obs_histo)
          
              ENDDO
          endif
  
         
  
  	ENDDO ! fim do loop do intevalo de tempo 
  	
  	time=jul2cal(cal2jul(time)+scamtec%incr)
  	time_ant=jul2cal(cal2jul(time_ant)+(scamtec%incr))
  ENDDO !fim do loop dos dias 
  
    
  
  !Chamando rotina EOF para cada experimento
  DO e = 1, scamtec%nexp !numeros de experimento 
  	
  	tempo(f)=(f-1)*time_step
  	
        do f=1, scamtec%ntime_forecast
        Fanom(:,:,:)=0
        cont=0
                       
  	    	do t=1, scamtec%ntime_steps
             	cont=cont+1    	
             	
             	!precip_tempo(e,cont,t,f)=scamdata(e)%expfield(i,j,hist%tipo_precip)              	
             	Fanom(e,:,cont)=  precip_tempo(e,:,t,f)          
            enddo           
            
            print*, 'tempo f', tempo(f)
            !convertendo inteiro para char
            write(tempo_char,'(I3.3)')tempo(f)
              
            CALL eof(Fanom(e,:,:),Ndim,Fvar,Feof,Ieof,Jeof,Cut,Neof,Trace,Npts,Nflds,e,tempo_char)       	
        enddo	
  ENDDO ! fim do loop do experimento
  
  !Chmando rotina EOF para OBS
  ! e=0 siqnifica que é 0bservação
  e=0
  CALL eof(precip_obs_eof(:,:),Ndim,Fvar,Feof,Ieof,Jeof,Cut,Neof,Trace,Npts,Nflds,e,'OBS')         
  
  
 !-----------------------------------------------------------------------------------------------------------
  
  
  call escreve_histograma_binario(total_histo,obs_histo)
  
 
 
  
  END SUBROUTINE precipitation
  
  
  SUBROUTINE histograma(prec,rang,valor_min,valor_limit, histo)
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: histo(:)                                   !variavel do histograma
  REAL                 :: divisao,rang, valor_min, valor_limit       !variavel divisao
  REAL,DIMENSION(:,:)  :: prec                                       !Variavel de precipitation
  INTEGER              :: indice, tam_hist 
  INTEGER              :: k, i, j                                    !Variavel de loop
  
  tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
 
!Percorrendo toda a matris
	
	
	
	print*, '::::::::::::::::::::::::::'	
  
  do i=1, scamtec%nxpt
	do j=1, scamtec%nypt	
		indice=0	

		! Prenha na posicao 1 quantos foram o valor minimo
		
		if(valor_min .EQ. 0)then
		valor_min=0.01		
		endif
						
		if (prec(i,j) .LE. valor_min) then		
			histo(1)=histo(1)+1
				
			! Prenha na posicao ultima posicao quantos foram acima do valor maximo	
		else if (prec(i,j) .GT. valor_limit) then
			histo(tam_hist)=histo(tam_hist)+1
		
		else 
	
			!Resto da divisão, funcao mod(a,b)
			divisao=mod(prec(i,j),2.0)
	
			
			if (divisao .EQ. 0) then
				indice=prec(i,j)/rang
				indice=indice+1
		
				!Percorrendo o vetor
				do k=2, tam_hist
					if(k .EQ. indice)then	
					histo(k)=histo(k)+1
					endif
				enddo					

		
			else
				indice=prec(i,j)/rang
				indice=indice+2

				!Percorrendo o vetor
				do k=2, tam_hist
			
					if(k .EQ. indice)then
					histo(k)=histo(k)+1
					endif
				enddo			
							
		
			endif
	
			
		endif
		

	enddo!fim do j
  enddo !fim do i
  valor_min=0.0	

  
  END SUBROUTINE histograma
  
  ! Rotina para escrever o Histograma em txt
  SUBROUTINE escreve_histograma(e,tempo,histo,obs_histo)
  IMPLICIT NONE
  INTEGER              :: tam_hist              !tamanho do histograma
  INTEGER              :: f,e,i,ier,v           !variaveis de loop
  REAL                 :: valor_rang,valor_min  !Minimo e maximo
  CHARACTER(len=1024)  :: formato               !formato de escrita
  INTEGER, ALLOCATABLE :: histo(:),obs_histo(:) !variavel do histograma
  INTEGER, ALLOCATABLE :: tempo(:)              !Intervalo de tempo ex(00,06,12...)
 
  
  
  valor_min=hist%valor_min 
  valor_rang=0
 
  if(valor_min .EQ. 0.01)then
  valor_min=0
  endif
  
  tam_hist=((hist%valor_limit-valor_min)/hist%rang)+2
  
  call opntext(e+3,trim(output_dir)//'/'//'histo'//Trim(Exper(e)%name)//'.txt','unknown',ier)
   
  write(formato,*)'(2X,A43,4x,',scamtec%ntime_forecast,'I10.3)' 
  write(e+3,formato)'%POSICAO      LIMITE              HISTO_OBS   ',(tempo(v),v=1,scamtec%ntime_forecast)   
          
  do i=1,tam_hist
  	if(i .eq. 1)then
  	write(formato,*)'(3x,I4,8x,A4,F6.2,8x,I10,6X,',scamtec%ntime_forecast,'I10)' 
  	write(e+3,formato)i,'x = ',valor_min,obs_histo(i),(scamdata(e)%time_histo(i,f),f=1,scamtec%ntime_forecast)
  	else if(i .eq. tam_hist)then
  		write(formato,*)'(3x,I4,5x,F8.2,A4,9x,I10,6X,',scamtec%ntime_forecast,'I10)'                    		
  		
  		write(e+3,formato)i,hist%valor_limit,'> x',obs_histo(i),(scamdata(e)%time_histo(i,f),f=1,scamtec%ntime_forecast)
  		
  		
  	    else
		write(formato,*)'(3x,I4,3x,F6.2,A7,3x,F6.2,1x,I10,6X,',scamtec%ntime_forecast,'I10)'                          		
		valor_rang=valor_min+hist%rang
		write(e+3,formato)i,valor_min,'> x <=',valor_rang,obs_histo(i),(scamdata(e)%time_histo(i,f),f=1,scamtec%ntime_forecast)
		valor_min=valor_min+hist%rang
	   endif	       
  enddo 
   
  call clstext(e+3,ier)   
  
 
 
  
  END SUBROUTINE escreve_histograma
  
  
  
  !Rotina para escrever o histograma em binario
  SUBROUTINE escreve_histograma_binario(total_histo,obs_histo)
  IMPLICIT NONE
  INTEGER              :: tam_hist              !tamanho do histograma
  INTEGER              :: t,f,e,i,ier,v,k         !variaveis de loop
  REAL                 :: valor_rang,valor_min  !Minimo e maximo
  CHARACTER(len=1024)  :: formato               !formato de escrita
  INTEGER, ALLOCATABLE :: total_histo(:,:,:,:)  !variavel do histograma
  INTEGER, ALLOCATABLE :: media_histo(:,:,:)        !variavel do histograma
  INTEGER, ALLOCATABLE :: soma_histo(:,:,:)        !variavel do histograma
  INTEGER, ALLOCATABLE :: obs_histo(:)          !variavel do histograma
  INTEGER, ALLOCATABLE ::tempo(:)               !Intervalo de tempo ex(00,06,12...)
  integer :: j
 
  
   
  valor_min=hist%valor_min 
  valor_rang=0
 
  if(valor_min .EQ. 0.01)then
  valor_min=0
  endif
  
  tam_hist=((hist%valor_limit-valor_min)/hist%rang)+2
  
  ALLOCATE(tempo(scamtec%ntime_forecast))
  ALLOCATE(media_histo(scamtec%nexp,scamtec%ntime_forecast,tam_hist))
  ALLOCATE(soma_histo(scamtec%nexp,scamtec%ntime_forecast,tam_hist))
  media_histo(:,:,:)=0
  soma_histo(:,:,:)=0
  k=0
  
  OPEN(4,file=trim(output_dir)//'/'//'histo'//'.bin',form='unformatted',status='unknown',iostat=ier)            
  
!Somando todos as classes 
    
  DO e=1, scamtec%nexp
  	DO i=1,tam_hist !quantidade de classe
  		DO f = 1, scamtec%ntime_forecast !quantidade de intervalo de tempo
  			DO t=1,scamtec%ntime_steps !quantidade de dias  
  			
  			soma_histo(e,f,i)=soma_histo(e,f,i)+total_histo(t,f,e,i)
  		
  			ENDDO !fim quantidade de dias   	
  		ENDDO !fim quantidade de intervalo de tempo    	
  	ENDDO  !fimquantidade de classe
  ENDDO ! Fim loop expereimento
  
  

!Tirando a media  
  DO e=1, scamtec%nexp
  	DO f = 1, scamtec%ntime_forecast !quantidade de intervalo de tempo
  		DO i=1,tam_hist !quantidade de classe
  			media_histo(e,f,i)=soma_histo(e,f,i)/(scamtec%ntime_steps)
  		
  		ENDDO !fimquantidade de classe
  	ENDDO !fim quantidade de intervalo de tempo    	
  ENDDO ! Fim loop expereimento

! Escrevendo binario histo.bin   
  DO t=1,scamtec%ntime_steps+1 !quantidade de dias  	
  	
  	DO e = 1, scamtec%nexp !numeros de experimento
  	
   		! Escrevendo Media do Histograma
 		if (t .eq. scamtec%ntime_steps+1)then
			write(4)real(media_histo(e,:,:),4)
	    ! Escrevendo Histograma 
		else
			write(4)real(total_histo(t,:,e,:),4)
		endif
		
  				
  	ENDDO ! Fim loop expereimento
  ENDDO ! Fim loop de dias  
!--------------------------------------------------------------------------------------------------------------------------------   
 
  
  close(4)
  
  DEALLOCATE(total_histo)
  DEALLOCATE(media_histo)
  DEALLOCATE(soma_histo)
  DEALLOCATE(obs_histo)
  DEALLOCATE(tempo)
  
  END SUBROUTINE escreve_histograma_binario
  

END MODULE m_metri_precip
