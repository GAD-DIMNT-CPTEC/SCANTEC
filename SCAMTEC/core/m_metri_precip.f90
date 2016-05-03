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
     integer, allocatable :: prec(:,:,:)    
  end type bstat
  type(bstat), allocatable :: dado(:)
  
  
  type b_hist
     INTEGER, ALLOCATABLE :: soma_histo(:,:)        !variavel do histograma
     INTEGER, ALLOCATABLE :: media_histo(:,:)       !variavel do histograma      
  end type b_hist
  type(b_hist), allocatable :: histogram(:)
  
  
  integer, allocatable :: Idx(:)
  integer              :: nidx
  integer              :: FUnitOut=4

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
    

        
    !
    ! Identificando Indice dos pontos validos
    !
  
    nidx = count (scamdata(run)%UdfIdx)
    npts = scamtec%nxpt*scamtec%nypt

    Allocate(Idx(nidx))
    
    !Idx(1:nidx) = PACK ( (/(i,i=1,npts)/), mask = scamdata(run)%UdfIdx) !old

    Idx(1:nidx) = PACK ( (/(i,i=1,npts)/), mask = scamdata(run)%UdfIdx(1:nidx,21))
        
    !
    ! transferindo dados para o calculo dos indices
    !

    !reffield => scamdata(1)%reffield
    !clmfield => scamdata(1)%clmfield
    expfield => scamdata(run)%expfield
    prefield => scamdata(1)%prefield

    
    if((scamtec%loop_count.eq.1) .and. (run .eq. 1))then
     
       !
       ! Allocando Memoria para o calculo dos Indices
       !
       tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
    
       Allocate(dado(scamtec%nexp))
       Allocate(histogram(scamtec%nexp)) 
          
       
       Do i=1,scamtec%nexp

          Allocate(dado(i)%histo(scamtec%ntime_steps,scamtec%ntime_forecast,tam_hist))
                   
          Allocate(dado(i)%prec(scamtec%npts,scamtec%ntime_steps,scamtec%ntime_forecast))
             
          ALLOCATE(histogram(i)%soma_histo(scamtec%ntime_forecast,tam_hist))
              
          ALLOCATE(histogram(i)%media_histo(scamtec%ntime_forecast,tam_hist))
          
          dado(i)%histo = 0.0   
          dado(i)%prec = 0.0   
          histogram(i)%soma_histo = 0
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
  INTEGER, ALLOCATABLE :: histo(:), obs_histo(:)              !variavel do histograma
  REAL,ALLOCATABLE     :: obs_precip(:), ant_obs_precip(:)    !Variavel de precipitation
  INTEGER :: tam_hist
  INTEGER, ALLOCATABLE :: tempo(:)                            !Intervalo de tempo ex(00,06,12...)
  
  character(len=*),parameter :: myname_=myname//'::HistoStat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif
    
  CALL InitBstat( run )  
    
  tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2    !Calculando o tamanho do histograma
    
  ALLOCATE(histo(tam_hist))    
  ALLOCATE(obs_histo(tam_hist))    
  ALLOCATE(obs_precip(scamtec%nxpt*scamtec%nypt))
  ALLOCATE(ant_obs_precip(scamtec%nxpt*scamtec%nypt))  
  ALLOCATE(tempo(scamtec%ntime_forecast))

  
  !Zerando variaveis
  histo(:)=0
  obs_histo(:)=0
   
  t=scamtec%time_step
  
  j = scamtec%ftime_idx 
    
  if(j .EQ. 1)then  
 
        print*,'Min/Max PREFIELD: ',minval(prefield(:,21)),maxval(prefield(:,21))

        !Preenchendo dados de precipitacao OBS
        dado(run)%prec(:,t,j)=prefield(Idx,21)       
                        
                        
        !gravar matriz observacao
        !OPEN(45,file=trim(scamtec%output_dir)//'/'//'soma_obs_precip1'//'.bin',form='unformatted',status='unknown',convert='big_endian')  
        !write(45)prefield(:,21)                
                                                 
        histo(:)=0      
              
        !Chamando rotina de Histograma     
        CALL histograma(prefield(:,21),hist%rang,hist%valor_min,hist%valor_limit,histo)
        
        print *, '::: HISTOGRAMA OBS :::'
        DO i=1, tam_hist
            dado(run)%histo(t,j,i)=histo(i)
            print*,t,j,run,i, dado(run)%histo(t,j,i)
        ENDDO
        
  else  
        
        print*,'Min/Max EXPFIELD: ',minval(expfield(:,hist%tipo_precip)),maxval(expfield(:,hist%tipo_precip))
        
        !Preenchendo dados de precipitacao EXP
        dado(run)%prec(:,t,j)=expfield(:,hist%tipo_precip)
        
        !open(46,file=trim(scamtec%output_dir)//'/'//'EXP_precip'//'.bin',form='unformatted',status='unknown',access = 'sequential')         
        !write(46)expfield(:,hist%tipo_precip)  
                     
                        
        histo(:)=0
        !Chamando rotina de Histograma        
        CALL histograma(expfield(:,hist%tipo_precip),hist%rang,hist%valor_min,hist%valor_limit,histo)
        
        print *, '::: HISTOGRAMA EXP :::', run        
        tempo(j)=(j-1)*scamtec%ftime_step                
                       
        DO i=1, tam_hist
            !print*, histo(i)
            dado(run)%histo(t,j,i)=histo(i)                       
            print*,t,tempo(j),run,dado(run)%histo(t,j,i)
                               
        ENDDO
  endif
  

  if ( (scamtec%time_step.eq.scamtec%ntime_steps) .and. (scamtec%ftime_idx .eq.scamtec%ntime_forecast)) then
  
          CALL WriteBstat( run )
       if (EOFs_Flag .eq. 1)CALL DadosEOFs ( run )
  endif
     
  if ( (scamtec%time_step.eq.scamtec%ntime_steps) .and. (scamtec%ftime_idx .eq.scamtec%ntime_forecast) .and. (run .eq. scamtec%nexp))then
         CALL FinalizeBStat( run )      
  endif  
  
  DEALLOCATE (histo)
  DEALLOCATE (obs_histo)
  DEALLOCATE (obs_precip)
  DEALLOCATE (ant_obs_precip)
  DEALLOCATE (tempo)
  
 END SUBROUTINE HistoStat
  
  !Rotina para Calcular Histograma
  SUBROUTINE histograma(prec,rang,valor_min,valor_limit, histo)
  IMPLICIT NONE
  
  INTEGER, ALLOCATABLE, intent(out):: histo(:)                     !variavel do histograma
  REAL, intent(in)                 :: rang, valor_min, valor_limit !variavel divisao
  REAL                             :: divisao, valor_minimo
  REAL,DIMENSION(:), intent(in)    :: prec                         !Variavel de precipitation
  INTEGER                          :: indice, tam_hist 
  INTEGER                          :: k, i, j                      !Variavel de loop
  character(len=*),parameter :: myname_=myname//'::histograma'
  
 #ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif   
  
  valor_minimo=valor_min
  tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
  ALLOCATE(histo(tam_hist))
!Percorrendo toda a matriz
 

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
    
    
    !INTEGER, ALLOCATABLE :: media_histo(:,:)        !variavel do histograma
    !INTEGER, ALLOCATABLE :: soma_histo(:,:)        !variavel do histograma  
    
    tam_hist=((hist%valor_limit-hist%valor_min)/hist%rang)+2
    
    
    ! Verificando se o arquivo esta aberto 
    inquire(unit=FUnitOut, opened=iret)
   
       !
       ! Abrindo arquivo binario de saida
       !
         
       OPEN(unit = FUnitOut, &
           file=trim(scamtec%output_dir)//'histo'//Trim(Exper(run)%name)//'.bin', &
           form='unformatted', &
           status='replace',iostat=ier)  
              
  
    ! Somando todas as classes
    print *, ''
    print *, 'SOMA'
    do i=1,tam_hist
        do f=1, scamtec%ntime_forecast
            do t=1, scamtec%ntime_steps
                                
                 histogram(run)%soma_histo(f,i)= histogram(run)%soma_histo(f,i)+dado(run)%histo(t,f,i)
                 PRINT *, run, t, f, i, histogram(run)%soma_histo(f,i)
                
            enddo  !fim quantidade de dias
         enddo !fim das previsoes
     enddo ! fim das classes
     
            
     !Tirando a media
     print *, ''
     print *, 'MEDIA'  
     do f = 1, scamtec%ntime_forecast 
        do i=1,tam_hist !quantidade de classe
            
             histogram(run)%media_histo(f,i)= histogram(run)%soma_histo(f,i)/(scamtec%ntime_steps)
             print *, run, f, histogram(run)%media_histo(f,i)
                       
        enddo !fimquantidade de classe
    enddo !fim das previsoes
    
   
    ! Escrevendo binario histo.bin   
    do t=1, scamtec%ntime_steps+1 ! +1 é para guardar a media
        if (t .eq. scamtec%ntime_steps+1)then
            
            ! Escrevendo Media do Histograma
            do f=1, scamtec%ntime_forecast
                do i=1, tam_hist
                    print*,f, i, histogram(run)%media_histo(f,i)
                enddo
            enddo
            write(FUnitOut)real( histogram(run)%media_histo(:,:),4)
        else
            
            ! Escrevendo Histograma    
            write(FUnitOut)real(dado(run)%histo(t,:,:),4)
        endif        
    enddo
      
    !Fechando arquivo binario 
    Close(FUnitOut)
    
  End Subroutine WriteBstat
  
  
  
  SUBROUTINE DadosEOFs(run)
  
    !
    ! !INPUT PARAMETERS:
    !

    integer, intent(in)  :: run ! experiment number
    
    
    !
    ! Variaveis
    !
    integer :: i,t,f
    INTEGER, ALLOCATABLE :: tempo(:) 
    Character(len=3)     :: tempo_char      !variavel para converter inteiro para char paulo dias
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
    Real, Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),scamtec%ntime_steps)           :: Fanom ! Fanom(e,x*Y,t)
    Real, Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),scamtec%ntime_steps,scamtec%ntime_forecast) :: precip_tempo   !precip_tempo(e,x*y,t,f)
    Real, Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),scamtec%ntime_steps)                        :: precip_obs_eof !precip_obs_eof(x*y,t)
    Real      :: Cut
    Integer   :: Ndim !nx_pontos * ny+pontos
    Integer   :: Ieof
    Integer   :: Jeof   
    Integer   :: Npts
    Integer   :: Nflds
    
    ! Variaveis que sai da rotina EOF
    Real (Kind=4), Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3)),4)   :: Feof
    Real (Kind=4), Dimension(4)        :: Fvar    
    Real (Kind=4)                      :: Trace
    Integer   :: Neof    
    
    
    
    !teste-----    
    Real,Dimension(int(scamtec%gridDesc(2))*int(scamtec%gridDesc(3))) :: vet_precp
    Integer              :: cont
    !-----------
    
    print*, ''
    print*, 'teste', scamtec%gridDesc(2), scamtec%gridDesc(3), scamtec%npts
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
    
    
    precip_obs_eof(:,:)=0
    precip_tempo(:,:,:)=0
    
        !dado(run)%histo(t,f,i)
    ! Total de preciptação do arquivo de Observação
    
    ALLOCATE(tempo(scamtec%ntime_forecast))
        
    do f=1, scamtec%ntime_forecast
        Fanom(:,:)=0
        tempo(f)=(f-1)*scamtec%ftime_step 
        cont=0
        do t=1, scamtec%ntime_steps
           Fanom(:,t)=dado(run)%prec(:,t,f)            
             
         enddo ! fim do dias     
         !convertendo inteiro para char
         write(tempo_char,'(I3.3)')tempo(f)
         CALL eof(Fanom(:,:),Ndim,Fvar,Feof,Ieof,Jeof,Cut,Neof,Trace,Npts,Nflds,run,tempo_char)  
      enddo
                
    
  END SUBROUTINE DadosEOFs
  
  
  
  

END MODULE m_metri_precip
