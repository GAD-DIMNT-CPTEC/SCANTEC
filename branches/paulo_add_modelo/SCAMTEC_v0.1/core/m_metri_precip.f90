MODULE m_metri_precip
USE SCAM_dataMOD
USE SCAM_bstatistic
USE m_ioutil
USE SCAM_Utils 
USE time_module
USE m_string

IMPLICIT NONE




CONTAINS
  
  SUBROUTINE precipitation()
  INTEGER              :: t, e, f,i                           !Variaveis de loop
  INTEGER              :: time, ftime, nymd, nhms, fymd, fhms !Variaveis do tempo 
  CHARACTER(len=1024)  :: Reference                           !Reference File Name
  CHARACTER(len=1024)  :: Experiment                          !Experiment File Name
  CHARACTER(len=1024)  :: Precipi                             !Precipitation File Name
  INTEGER, ALLOCATABLE :: histo(:), obs_histo(:)              !variavel do histograma
  INTEGER, ALLOCATABLE :: tempo(:)                            !Intervalo de tempo ex(00,06,12...)
  
  tam_hist=(hist%valor_limit/hist%rang)+2  	!Calculando o tamanho do histograma
  ALLOCATE(histo(tam_hist))	   
  ALLOCATE(obs_histo(tam_hist))	   
  ALLOCATE(tempo(scamtec%ntime_forecast))
  
  !Zerando variaveis
  histo(:)=0
  obs_histo(:)=0
  tempo(:)=0
  
  time=starting_time
  DO t=1,scamtec%ntime_steps !quantidade de dias
      nymd = time/100
      nhms = MOD(time,100) * 10000
  
      !
      ! 1.1 Create file name and Open Reference data file 
      !
  
      !Reference=TRIM(Refer%file)
      !CALL str_template(Reference, nymd,nhms)
      !CALL ldata('R', 1, Refer%Id, Reference)
      !print*,Refer%Id, Reference
      
      
      Precipi = TRIM(precip%file)
      CALL str_template(Precipi, nymd,nhms)
      CALL ldata('P', 1, precip%Id, Precipi)
      print*,precip%Id, Precipi
      
      
      obs_histo(:)=0
      histo(:)=0
      CALL histograma(scamdata(1)%reffield(:,:,16),hist%rang,hist%valor_min,hist%valor_limit,histo)
      obs_histo(:)=histo(:)
      
      
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
     
             histo(:)=0
             tempo(f)=(f-1)*time_step
             CALL histograma(scamdata(e)%expfield(:,:,16),hist%rang,hist%valor_min,hist%valor_limit,histo)    
                  			
             DO i=1, tam_hist
                 scamdata(e)%time_histo(i,f)= histo(i)
                 print*,t,f,e,histo(i)		 	
            ENDDO
    
          ENDDO ! fim do loop do experimento
  
          ftime = jul2cal(cal2jul(ftime)-scamtec%incr)
  
          if (t.eq.scamtec%ntime_steps)then
              DO e=1,scamtec%nexp
                  call escreve_histograma(e,tempo,histo,obs_histo)
          
              ENDDO
          endif
  
          time=jul2cal(cal2jul(time)+scamtec%incr)
  
  	ENDDO ! fim do loop do intevalo de tempo 
  ENDDO !fim do loop dos dias 
   
 
 
  
  END SUBROUTINE precipitation
  
  
  SUBROUTINE histograma(prec,rang,valor_min,valor_limit, hist)
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: hist(:)                                   !variavel do histograma
  REAL                 :: divisao,rang                              !variavel divisao
  REAL,DIMENSION(:,:)  :: prec                                      !Variavel de precipitation
  INTEGER              :: indice, valor_limit, valor_min, tam_hist 
  INTEGER              :: k, i, j                                   !Variavel de loop
  
  tam_hist=(valor_limit/rang)+2 	
 


!Percorrendo toda a matris
	
  
do i=1, scamtec%nxpt
	do j=1, scamtec%nypt	
		indice=0	

		! Prenha na posicao 1 quantos foram o valor minimo
		if (prec(i,j) .EQ. valor_min) then
			hist(1)=hist(1)+1
				
			! Prenha na posicao ultima posicao quantos foram acima do valor maximo	
		else if (prec(i,j) .GT. valor_limit) then
			hist(tam_hist)=hist(tam_hist)+1
		
		else 
	
			!Resto da divisão, funcao mod(a,b)
			divisao=mod(prec(i,j),2.0)
	
			if (divisao .EQ. 0) then
				indice=prec(i,j)/rang
				indice=indice+1
		
				!Percorrendo o vetor
				do k=2, tam_hist
					if(k .EQ. indice)then	
					hist(k)=hist(k)+1
					endif
				enddo					

		
			else
				indice=prec(i,j)/rang
				indice=indice+2

				!Percorrendo o vetor
				do k=2, tam_hist
			
					if(k .EQ. indice)then
					hist(k)=hist(k)+1
					endif
				enddo			
							
		
			endif
	
			
		endif
		

	enddo!fim do j
enddo !fim do i

 
  
  END SUBROUTINE histograma
  
  SUBROUTINE escreve_histograma(e,tempo,histo,obs_histo)
  IMPLICIT NONE
  INTEGER              :: tam_hist              !tamanho do histograma
  INTEGER              :: f,e,i,ier,v           !variaveis de loop
  REAL                 :: valor_rang,valor_min  !Minimo e maximo
  CHARACTER(len=1024)  :: formato               !formato de escrita
  INTEGER, ALLOCATABLE :: histo(:),obs_histo(:) !variavel do histograma
  INTEGER, ALLOCATABLE ::tempo(:)               !Intervalo de tempo ex(00,06,12...)
  
  tam_hist=(hist%valor_limit/hist%rang)+2 
  
  call opntext(e+3,trim(output_dir)//'/'//'histo'//Trim(Exper(e)%name)//'.txt','unknown',ier)      
  write(formato,*)'(A43,4x,',scamtec%ntime_forecast,'I10.3)' 
  write(e+3,formato)'%POSICAO      LIMITE              HISTO_OBS   ',(tempo(v),v=1,scamtec%ntime_forecast) 
       	         
  
  valor_rang=0
  valor_min=hist%valor_min
  do i=1,tam_hist
  	if(i .eq. 1)then
  	write(formato,*)'(3x,I2,8x,A4,F4.2,10x,I10,6X,',scamtec%ntime_forecast,'I10)' 
  	write(e+3,formato)i,'x = ',valor_min,obs_histo(i),(scamdata(e)%time_histo(i,f),f=1,scamtec%ntime_forecast)
  	else if(i .eq. tam_hist)then
  		write(formato,*)'(3x,I2,5x,I4,A4,13x,I10,6X,',scamtec%ntime_forecast,'I10)'                    		
  		
  		write(e+3,formato)i,hist%valor_limit,'> x',obs_histo(i),(scamdata(e)%time_histo(i,f),f=1,scamtec%ntime_forecast)
  		
  	    else
		write(formato,*)'(3x,I2,3x,F6.2,A7,3x,F6.2,1x,I10,6X,',scamtec%ntime_forecast,'I10)'                          		
		valor_rang=valor_rang+hist%rang
		write(e+3,formato)i,valor_min,'> x <=',valor_rang,obs_histo(i),(scamdata(e)%time_histo(i,f),f=1,scamtec%ntime_forecast)
		valor_min=valor_min+hist%rang
	   endif	       
  enddo 
  if(Precipitation_Flag.eq.1)call clstext(e+3,ier) 
  
  
  
  END SUBROUTINE escreve_histograma
  

END MODULE m_metri_precip
