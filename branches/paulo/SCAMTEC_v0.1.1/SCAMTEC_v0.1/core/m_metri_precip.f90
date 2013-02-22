!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_metri_precip.f90
!
! !DESCRIPTON:
! Modulo para calcular Histograma de frequencia e EOFs
!             
!                 
!\\
!\\
! !INTERFACE:
!


MODULE m_metri_precip

  !
  ! !USES:
  !
  
  
  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE m_string                      ! string manipulations
  USE SCAM_bstatistic
  
  
    
 USE SCAM_dataMOD
 USE SCAM_Utils 
 USE time_module


 IMPLICIT NONE
 PRIVATE
  !
  ! !PARAMETERS:
  !
  
  type bstat
     integer, allocatable :: histo(:,:,:)     
  end type bstat
  type(bstat), allocatable :: dado(:)
  
  
  type b_hist
     INTEGER, ALLOCATABLE :: soma_histo(:,:)        !variavel do histograma
     INTEGER, ALLOCATABLE :: media_histo(:,:)       !variavel do histograma      
  end type b_hist
  type(b_hist), allocatable :: histogram(:)
  
  
  integer, allocatable :: Idx(:)
  integer              :: nidx
  integer              :: FUnitOut = 4

  character(len=*),parameter :: myname='m_metri_precip'
  
  !
  ! !PUBLIC DATA MEMBERS:
  !
  !real, pointer     :: reffield(:,:) ! (x*y,v)
  real, pointer     :: expfield(:,:) ! (x*y,v)
  !real, pointer     :: clmfield(:,:)
  real, pointer     :: prefield(:,:) ! (x*y,v)
  
  
  !
  ! !PUBLIC MEMBER FUNCTIONS:
  !

  public :: HistoStat
  
  !
  ! !REVISION HISTORY:
  !  20 FEV 2013 - J. G. de Mattos - Initial Version
  !
  ! !SEE ALSO:
  !   
  !
  !EOP
  !-----------------------------------------------------------------------------!

Contains

 Subroutine InitBstat( Run )
 Implicit None
    !
    ! !INPUT PARAMETERS:
    !

    integer, intent(in) :: run ! experiment number

    !
    !
    ! !REVISION HISTORY: 
    !  31 January 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    integer            :: i, npts,ier, tam_hist
    character(len=512) :: fname, fmt
    integer            :: nymd, nhms
    integer            :: fymd, fhms

        
    !
    ! Identificando Indice dos pontos validos
    !

    nidx = count (scamdata(run)%UdfIdx)
    npts = scamtec%nxpt*scamtec%nypt

    Allocate(Idx(nidx))

    Idx(1:nidx) = PACK ( (/(i,i=1,npts)/), mask = scamdata(run)%UdfIdx)

    !
    ! transferindo dados para o calculo dos indices
    !

    !reffield => scamdata(1)%reffield
    !clmfield => scamdata(1)%clmfield
    expfield => scamdata(run)%expfield
    prefield => scamdata(1)%prefield

    
    if(scamtec%loop_count.eq.1)then

       !
       ! Abrindo arquivo de saida e escrevendo o Cabecalho
       !

       OPEN(unit=FUnitOut, &
            file=trim(scamtec%output_dir)//'histo'//Trim(Exper(run)%name)//'.bin', &
            form='unformatted', &
            status='replace',iostat=ier)            


       !
       ! Allocando Memoria para o calculo dos Indices
       !
       tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
    
       Allocate(dado(scamtec%nexp))
       Allocate(histogram(scamtec%nexp))       
       
       Do i=1,scamtec%nexp

          Allocate(dado(i)%histo(scamtec%ntime_steps,scamtec%ntime_forecast,tam_hist))
          dado(i)%histo = 0.0                  
    
          ALLOCATE(histogram(i)%soma_histo(scamtec%ntime_forecast,tam_hist))
          histogram(i)%soma_histo = 0
    
          ALLOCATE(histogram(i)%media_histo(scamtec%ntime_forecast,tam_hist))
          histogram(i)%media_histo = 0
          
       Enddo


    endif

  End Subroutine InitBstat
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  InitBstat
  !
  ! !DESCRIPTION: This routine ....
  !
  !\\
  !\\
  ! !INTERFACE:

  Subroutine FinalizeBstat( run )
    Implicit None
    !
    ! !INPUT PARAMETERS:
    !

    integer, intent(in) :: run ! experiment number

    !
    !
    ! !REVISION HISTORY: 
    !  31 January 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !


    !
    ! Desassociando ponteiros
    !

   ! if (associated(reffield)) nullify(reffield)
    if (associated(expfield)) nullify(expfield)
    !if (associated(clmfield)) nullify(clmfield)
    if (associated(prefield)) nullify(prefield)

    !
    ! Desalocando variaveis
    !

    DeAllocate(Idx)
    DeAllocate(dado)
    DeAllocate(histogram)

    !
    ! Fechando arquivo 
    !

    close(FUnitOut)

  End Subroutine FinalizeBstat

  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  HistoStat
  !
  ! !DESCRIPTION: This routine ....
  !
  !\\
  !\\
  ! !INTERFACE:
  !

 
  
  SUBROUTINE HistoStat(run)
  
  integer, intent(in) :: run ! experiment number
  integer             :: npts
  
  
  
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
  REAL,ALLOCATABLE     :: obs_precip(:), ant_obs_precip(:)    !Variavel de precipitation
  INTEGER :: tam_hist
  
  character(len=*),parameter :: myname_=myname//'::HistoStat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

  CALL InitBstat( run )  
  
  tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2    !Calculando o tamanho do histograma
  quant_arq_ant=hist%acumulo_exp/hist%acumulo_obs     !Calculando quantidade de arquivos anterior para abrir
  
  ALLOCATE(histo(tam_hist))    
  ALLOCATE(obs_histo(tam_hist))    
  ALLOCATE(tempo(scamtec%ntime_forecast))
  ALLOCATE(total_histo(scamtec%ntime_steps,scamtec%ntime_forecast,scamtec%nexp,tam_hist))
  ALLOCATE(obs_precip(scamtec%nxpt*scamtec%nypt))
  ALLOCATE(ant_obs_precip(scamtec%nxpt*scamtec%nypt))  
  
  
  
  !Zerando variaveis
  histo(:)=0
  obs_histo(:)=0
  tempo(:)=0
  total_histo(:,:,:,:)=0
  
  t=scamtec%time_step
   print *, 'teste:',t
  j = scamtec%ftime_idx 
   
  
  !scamtec%time_step.eq.scamtec%ntime_steps
  !DO i=1, tam_hist
   !print *, t,j,run,dado(run)%histo(j,i)
  !enddo
   
   
  if(j .EQ. 1)then
        CALL histograma(prefield(Idx,16),hist%rang,hist%valor_min,hist%valor_limit,histo)
        obs_histo(:)=histo(:)
        DO i=1, tam_hist
            print*,obs_histo(i)
            dado(run)%histo(t,j,i)=obs_histo(i)
        ENDDO
  else      
        histo(:)=0
        CALL histograma(expfield(Idx,hist%tipo_precip),hist%rang,hist%valor_min,hist%valor_limit,histo)
        DO i=1, tam_hist
            !print*, histo(i)
            dado(run)%histo(t,j,i)=histo(i)                       
        
        print*,t,j,run,dado(run)%histo(t,j,i)
                               
        ENDDO
  endif
  
  if ( (scamtec%time_step.eq.scamtec%ntime_steps) .and. (scamtec%ftime_idx .eq.scamtec%ntime_forecast) )then
       CALL WriteBstat( run )
       CALL FinalizeBStat( run )
      
  endif
      
         
  
  
  
  
 END SUBROUTINE HistoStat
  
  SUBROUTINE histograma(prec,rang,valor_min,valor_limit, histo)
  IMPLICIT NONE
  
  INTEGER, ALLOCATABLE, intent(out):: histo(:)                     !variavel do histograma
  REAL, intent(in)                 :: rang, valor_min, valor_limit !variavel divisao
  REAL                             :: divisao, valor_minimo
  REAL,DIMENSION(:), intent(in)    :: prec                         !Variavel de precipitation
  INTEGER                          :: indice, tam_hist 
  INTEGER                          :: k, i, j                      !Variavel de loop
  
  valor_minimo=valor_min
  tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
  ALLOCATE(histo(tam_hist))
!Percorrendo toda a matris
    
    
    
    print*, '::::::::::::::::::::::::::'
  
  do i=1, scamtec%nxpt*scamtec%nypt
        indice=0    

        ! Prenha na posicao 1 quantos foram o valor minimo
        
        if(valor_minimo .EQ. 0)then
            valor_minimo=0.01 
        endif
                        
        if (prec(i) .LE. valor_minimo) then   
            histo(1)=histo(1)+1
              
            ! Prenha na posicao ultima posicao quantos foram acima do valor maximo  
        else if (prec(i) .GT. valor_limit) then
            histo(tam_hist)=histo(tam_hist)+1
                       
        else 
    
            !Resto da divisão, funcao mod(a,b)
            divisao=mod(prec(i),2.0)
           
            
            if (divisao .EQ. 0) then
                indice=prec(i)/rang
                indice=indice+1
        
                !Percorrendo o vetor
                do k=2, tam_hist
                    if(k .EQ. indice)then   
                    histo(k)=histo(k)+1
                    endif
                enddo                   

        
            else
                indice=prec(i)/rang
                indice=indice+2

                !Percorrendo o vetor
                do k=2, tam_hist
            
                    if(k .EQ. indice)then
                    histo(k)=histo(k)+1
                    endif
                enddo               
            endif          
        endif    
  enddo !fim do i
  
  
  END SUBROUTINE histograma
  
  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  WriteBstat
  !
  ! !DESCRIPTION: This routine ....
  !
  !\\
  !\\
  ! !INTERFACE:
  !

  Subroutine WriteBstat( run )
    Implicit None

    !
    ! !INPUT PARAMETERS:
    !

    integer, intent(in)  :: run ! experiment number
      
    
    !
    !
    ! !REVISION HISTORY: 
    !  20 February 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    integer              :: iret,i,j,ier, tam_hist
    integer              :: t,f
    
    
    INTEGER, ALLOCATABLE :: media_histo(:,:)        !variavel do histograma
    INTEGER, ALLOCATABLE :: soma_histo(:,:)        !variavel do histograma  
    
    tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
    
    
    ! Verificando se o arquivo esta aberto 
    inquire(unit=FUnitOut, opened=iret)
    if(.not.iret) then       
       OPEN(unit = FUnitOut, &
           file=trim(scamtec%output_dir)//'histo'//Trim(Exper(run)%name)//'.bin', &
           form='unformatted', &
           status='old',iostat=ier)    
    endif    
    
  
    ! Somando todas as classes
    do i=1,tam_hist
        do f=1, scamtec%ntime_forecast
            do t=1, scamtec%ntime_steps
                 histogram(run)%soma_histo(f,i)= histogram(run)%soma_histo(f,i)+dado(run)%histo(t,f,i)
                
            enddo  !fim quantidade de dias
         enddo !fim das previsoes
     enddo ! fim das classes
     
    
     print *, ''
            print *, '::: MEDIA :::'
     !Tirando a media  
     do f = 1, scamtec%ntime_forecast 
        do i=1,tam_hist !quantidade de classe
             histogram(run)%media_histo(f,i)= histogram(run)%soma_histo(f,i)/(scamtec%ntime_steps)
                       
        enddo !fimquantidade de classe
    enddo !fim das previsoes
   
    ! Escrevendo binario histo.bin   
    do t=1, scamtec%ntime_steps+1 ! +1 é para guardar a media
        if (t .eq. scamtec%ntime_steps+1)then
            
            ! Escrevendo Media do Histograma
            write(FUnitOut)real( histogram(run)%media_histo(:,:),4)
        else
            
            ! Escrevendo Histograma    
            write(FUnitOut)real(dado(run)%histo(t,:,:),4)
        endif        
    enddo
      
    !Fechando arquivo binario 
    Close(FUnitOut)




  End Subroutine WriteBstat
  
  
  
  

END MODULE m_metri_precip