!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_iwv.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE m_iwv
	
	USE m_string          		! String Manipulation
  	USE m_die                       ! Error Messages
  	USE m_stdio                     ! Module to defines std. I/O parameters
  	USE scamtec_module
  	USE m_ioutil
	
	
	IMPLICIT NONE
	Private

	INTEGER :: H,G,Z,I,F,Nest,ni0,ni6,ni12,ni18,ce
	INTEGER :: PR0,PR6,PR12,PR18,TEMP0,TEMP6,TEMP12,TEMP18,HUM0,HUM6,HUM12,HUM18
	INTEGER(8),DIMENSION(:), ALLOCATABLE :: segjul
	REAL, DIMENSION(:), ALLOCATABLE :: iwv0,iwv6,iwv12,iwv18
	REAL, DIMENSION(:), ALLOCATABLE :: MedIWV0,MedIWV6,MedIWV12,MedIWV18,MedPR0,MedPR6,MedPR12,MedPR18,MedTEMP0,MedTEMP6,MedTEMP12,MedTEMP18, MedHUM0,MedHUM6,MedHUM12,MedHUM18
	REAL, DIMENSION(90000, 6) :: obs
	INTEGER(8), DIMENSION(90000, 3) :: times
	CHARACTER(LEN=4) :: arq
	CHARACTER(LEN=1024) :: juntarq
	CHARACTER(LEN=4), DIMENSION(9999) :: id
	CHARACTER(LEN=12), DIMENSION(9999) :: latGPS,lonGPS,Long,Lat
	CHARACTER(LEN=4), DIMENSION(9999) :: codigo
	CHARACTER(LEN=8), DIMENSION(9999) :: altGPS
	CHARACTER(LEN=150) :: TEXTO
	CHARACTER(LEN=12), DIMENSION(:), ALLOCATABLE :: latIWV,lonIWV
  	CHARACTER(LEN=180) :: cabec
        CHARACTER(LEN=5) :: interval, prim_esta
        CHARACTER(LEN=4), DIMENSION(5) :: esta
    	
    	public  :: iwv_init
    	
CONTAINS
	
	SUBROUTINE iwv_init(observation)
    	character(len=*), intent(IN) :: observation
    !  ABRINDO ARQUIVO de lat e lon  (a abertura do arquivo de lat e lon pode ser rotina/funcao)
      		OPEN(1,FILE='gps.txt',STATUS='OLD')
       
64    		READ(1,'(A)',END=66)TEXTO
     			 IF(TEXTO(1:1).EQ.'#')GOTO 64
      		BACKSPACE (1)
 
	
	
    ! CARREGANDO AS INFORMACOES DAS ESTACOES GPS
       	DO I=1,150

 	  READ(1,65,END=66)codigo(I),latGPS(I),lonGPS(I),altGPS(I)	
65        Format(7x,A4,5x,A12,5x,A12,5x,A8)
        
         ! print*,codigo(I),' ',latGPS(I),' ',lonGPS(I),' ',altGPS(I)
        
 	END DO

66 Nest=I-1 

! Alocando memoria, o 3 é por causa da quant. de estacoes que esta sendo usado
 ALLOCATE(segjul(3))
 ALLOCATE(latIWV(3))
 ALLOCATE(lonIWV(3))

 ALLOCATE(iwv0(3))
 ALLOCATE(iwv6(3))
 ALLOCATE(iwv12(3))
 ALLOCATE(iwv18(3))
 ALLOCATE(MedIWV0(3))
 ALLOCATE(MedIWV6(3))
 ALLOCATE(MedIWV12(3))
 ALLOCATE(MedIWV18(3))
 ALLOCATE(MedPR0(3))
 ALLOCATE(MedPR6(3))
 ALLOCATE(MedPR12(3))
 ALLOCATE(MedPR18(3))
 ALLOCATE(MedTEMP0(3))
 ALLOCATE(MedTEMP6(3))
 ALLOCATE(MedTEMP12(3))
 ALLOCATE(MedTEMP18(3))
 ALLOCATE(MedHUM0(3))
 ALLOCATE(MedHUM6(3))
 ALLOCATE(MedHUM12(3))
 ALLOCATE(MedHUM18(3))


     juntarq= observation

	    print*,'***************************'
    	    print*,'Abrindo o arquivo ',observation 
            print*,'***************************'	
    
!  ABRINDO ARQUIVO 
     
     OPEN(2,FILE=trim(observation),STATUS='Unknown')

!Aqui começa a leitura do cabeçalho
   ce=0        
   DO WHILE(cabec(1:6) .ne. '*SITE')  
 
       READ(2,'(A150)')cabec



       if(cabec(2:19) .eq. 'SAMPLING INTERVAL') interval= cabec(50:57)
          
       if(cabec(1:6) .eq. '*SITE') then
         READ(2,'(A150)')cabec     
            DO WHILE(cabec(1:22) .ne. '-TROP/STA_COORDINATES')                        
                  ce=ce+1
                  esta(ce)= cabec(2:6)
                     if(ce==1) prim_esta = esta(ce) 
                  print*,'Estacao: ',esta(ce)
                  READ(2,'(A)')cabec 

            END DO
       end if

       if(cabec(1:15).eq.'+TROP/SOLUTION') READ(2,*)cabec
    
   END DO


    print*,'Intervalo da observacao: ',interval
    print*,'cabec final: ',cabec
    print*,'primeira estacao: ',prim_esta

!Aqui termina a leitura do cabeçalho.

     
 		




! Loop da leitura para saber a quantidade de linhas corretas 
    
    DO H=1,5000
!                           Esta    ano       dia jul    seg jul     ztd      sig      iwv     press    temp     humid
          READ(2,32, END=15) id(H),times(H,1),times(H,2),times(H,3),obs(H,1),obs(H,2),obs(H,3),obs(H,4),obs(H,5),obs(H,6)
32        FORMAT (1x,A4,1x,I2,1x,I3,1x,I5,1x,F6.1,2x,F4.2,2x,F7.4,2x,F6.2,2x,F5.2,2x,F5.2)         
          
15 Continue       
         
         !print*,id(H),times(H,1),times(H,2),times(H,3)       
         !print*,obs(H,1),obs(H,2),obs(H,3),obs(H,4),obs(H,5),obs(H,6)

          

    END DO
  


   
! contadores de observacao equivale as 0,6,12,18 h	
     	ni0=0
     	ni6=0
     	ni12=0
     	ni18=0

	
     
     	Z=1
	
! loop da quantidade de estaçoes
    DO F=1,ce
    
    
    	PR0=0
	PR6=0
	PR12=0
	PR18=0

	TEMP0=0
	TEMP6=0
	TEMP12=0
	TEMP18=0

	HUM0=0
	HUM6=0
	HUM12=0
	HUM18=0

     	MedIWV0=0
     	MedIWV6=0
     	MedIWV12=0
     	MedIWV18=0

	MedPR0=0
	MedPR6=0
	MedPR12=0
	MedPR18=0

	MedTEMP0=0
	MedTEMP6=0
	MedTEMP12=0
	MedTEMP18=0

	MedHUM0=0
	MedHUM6=0
	MedHUM12=0
	MedHUM18=0
	
		DO G=1,150
 
			if (prim_esta .EQ. codigo(G)) then
				!print*,codigo(G),' ',latGPS(G),' ',lonGPS(G)
				Long(F)= lonGPS(G)
				Lat(F)=  latGPS(G) 
				 
			end if
		
		end do


!loop de comparação da mesma estação
 	DO WHILE (prim_esta .EQ. id(Z))              
           
! verificando o intervalo de tempo e para tirar a media das 0 hora  (30 min apos 0h)
       	if((times(Z,3) .ge. 0 ) .and. (times(Z,3) .le. 1800)) then     	       	
              	ni0=ni0+1
             	MedIWV0=MedIWV0+obs(Z,3)
	     	
            
		PR0=PR0+1
              	MedPR0=MedPR0+obs(Z,4)
	     	
	
		TEMP0=TEMP0+1
              	MedTEMP0=MedTEMP0+obs(Z,5)
	      	
		HUM0=HUM0+1
              	MedHUM0=MedHUM0+obs(Z,6)
	      	
        end if
        
           
! verificando o intervalo de tempo e para tirar a media das 6 hora  (15 min antes e 15min apos 6h)
       	if((times(Z,3) .ge. 20700 ) .and. (times(Z,3) .le. 22500 )) then
       	      	ni6=ni6+1
              	MedIWV6=MedIWV6+obs(Z,3)
	      	

		PR6=PR6+1
              	MedPR6=MedPR6+obs(Z,4)
	     	
	
		TEMP6=TEMP6+1
              	MedTEMP6=MedTEMP6+obs(Z,5)
	      	

		HUM6=HUM6+1
              	MedHUM6=MedHUM6+obs(Z,6)
	      	
        end if
        
        
! verificando o intervalo de tempo e para tirar a media das 12 hora  (15 min antes e 15min apos 12h)
       	if((times(Z,3) .ge. 42300) .and. (times(Z,3) .le. 44100)) then      	       	
              	ni12=ni12+1
              	MedIWV12=MedIWV12+obs(Z,3)
	      	 

		PR12=PR12+1
              	MedPR12=MedPR12+obs(Z,4)
	     	
	
		TEMP12=TEMP12+1
              	MedTEMP12=MedTEMP12+obs(Z,5)
	      	

		HUM12=HUM12+1
              	MedHUM12=MedHUM12+obs(Z,6)
	      	             
        end if
        
           
! verificando o intervalo de tempo e para tirar a media das 18 hora  (15 min antes e 15min apos 18h)
       	if((times(Z,3) .ge. 63900) .and. (times(Z,3) .le. 65700)) then
       	     	ni18=ni18+1
             	MedIWV18=MedIWV18+obs(Z,3)
	     	

		PR18=PR18+1
              	MedPR18=MedPR18+obs(Z,4)
	     	
	
		TEMP18=TEMP18+1
              	MedTEMP18=MedTEMP18+obs(Z,5)
	      	

		HUM18=HUM18+1
             	MedHUM18=MedHUM18+obs(Z,6)
	      	
        end if

           Z= Z+1

     	END DO
	
		PRINT*, 'Tempo   ESTA    LAT          LON             UMI           PRESS                  TEMP            IWV      '	
		PRINT*, '-- 0 -- ',prim_esta,'   ',Lat(F),'  ',Long(F),'  ',MedHUM0(F)/HUM0,(MedPR0(F)/PR0),'  ',(MedTEMP0(F)/TEMP0),'  ',(MedIWV0(F)/ni0)
		PRINT*, '-- 6 -- ',prim_esta,'   ',Lat(F),'  ',Long(F),'  ',MedHUM6(F)/HUM6,MedPR6(F)/PR6,'  ',MedTEMP6(F)/TEMP6,'  ',MedIWV6(F)/ni6
		PRINT*, '-- 12 - ',prim_esta,'   ',Lat(F),'  ',Long(F),'  ',MedHUM12(F)/HUM12,MedPR12(F)/PR12,'  ',MedTEMP12(F)/TEMP12,'  ',MedIWV12(F)/ni12
		PRINT*, '-- 18 - ',prim_esta,'   ',Lat(F),'  ',Long(F),'  ',MedHUM18(F)/HUM18,MedPR18(F)/PR18,'  ',MedTEMP18(F)/TEMP18,'  ',MedIWV18(F)/ni18
		PRiNt*, '-------------------------------------------------------------------------------'

	
	prim_esta = id(Z)


	END DO

	
   
		END SUBROUTINE iwv_init


	
END MODULE m_iwv
