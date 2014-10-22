!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_bstatistic.f90
!
! !DESCRIPTON:
!             
!                 
!\\
!\\
! !INTERFACE:
!


Module SCAM_bstatistic

  !
  ! !USES:
  !

  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE m_string                      ! string manipulations

  IMPLICIT NONE
  PRIVATE
  !
  ! !PARAMETERS:
  !

  character(len=*),parameter :: myname='SCAM_bstatistic' 

  !
  ! !PUBLIC DATA MEMBERS:
  !
  real, pointer     :: reffield(:,:) ! (x*y,v)
  real, pointer     :: expfield(:,:) ! (x*y,v)
  real, pointer     :: clmfield(:,:)

  real, allocatable :: diffield(:,:) ! diference field

  type bstat
     real, allocatable :: rmse(:,:)
     real, allocatable :: vies(:,:)
     real, allocatable :: acor(:,:)
     real, allocatable :: rmse_field(:,:,:)
     real, allocatable :: vies_field(:,:,:)
     real, allocatable :: exp_mean_field(:,:,:)
  !   real, allocatable :: desp(:,:) ! desvio padrao paulo dias
  end type bstat

  type(bstat), allocatable :: dado(:)

  integer, allocatable :: Idx(:,:)
  integer              :: nidx


  character(len=512) :: FNameOut = 'EXP%e_%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.scam'
  integer            :: FUnitOut = 10

  !
  ! !PUBLIC MEMBER FUNCTIONS:
  !

  public :: CalcBstat
  public :: Corr

  !
  ! !REVISION HISTORY:
  !  09 OCT 2011 - J. G. de Mattos - Initial Version
  !
  ! !SEE ALSO:
  !   
  !
  !EOP
  !-----------------------------------------------------------------------------!

Contains
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

    integer            :: i, j, npts
    character(len=512) :: fname, fmt
    integer            :: nymd, nhms
    integer            :: fymd, fhms

    character(len=*),parameter :: myname_=myname//'::InitBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif


    !
    ! Identificando Indice dos pontos validos
    !

    npts = scamtec%nxpt*scamtec%nypt

    Allocate(Idx(npts,scamtec%nvar))
    Idx = -1

    DO i=1,scamtec%nvar
       nidx = count (scamdata(run)%UdfIdx(1:npts,i))
       Idx(1:nidx,i) = PACK ( (/(j,j=1,npts)/), mask = scamdata(run)%UdfIdx(1:npts,i))
    ENDDO


    !
    ! transferindo dados para o calculo dos indices
    !

    reffield => scamdata(1)%reffield
    clmfield => scamdata(1)%clmfield
    expfield => scamdata(run)%expfield


    if(scamtec%loop_count.eq.1)then

       !
       ! Abrindo arquivo de saida e escrevendo o Cabecalho
       !

       nymd = scamtec%starting_time/100
       nhms = MOD(scamtec%starting_time,100) * 10000
       fymd = scamtec%ending_time/100
       fhms = MOD(scamtec%ending_time,100) * 10000

       fname = FNameOut
       CALL str_template(FName, nymd, nhms, fymd, fhms, int2str(run,'(I2.2)'))

       open(unit   = FUnitOut+0,      &
            File   = trim(scamtec%output_dir)//'/RMSE'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+1,      &
            File   = trim(scamtec%output_dir)//'/VIES'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+2,      &
            File   = trim(scamtec%output_dir)//'/ACOR'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+3,      &
            File   = trim(scamtec%output_dir)//'/RMSEF'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )
            
       open(unit   = FUnitOut+4,      &
            File   = trim(scamtec%output_dir)//'/VIESF'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )

       open(unit   = FUnitOut+5,      &
            File   = trim(scamtec%output_dir)//'/MEANF'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )

       write(fmt,'(A4,I3.3,A5)')'(A9,',scamtec%nvar,'A9)'
       write(FUnitOut+0,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar)
       write(FUnitOut+1,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar)
       write(FUnitOut+2,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar)
     !  write(FUnitOut+3,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar) ! paulo dias

       !
       ! Allocando Memoria para o calculo dos Indices
       !

       if(.NOT.Allocated(dado))Allocate(dado(scamtec%nexp))
       !Do i=1,scamtec%nexp

          Allocate(dado(run)%rmse(scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(run)%vies(scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(run)%acor(scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(run)%rmse_field(npts,scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(run)%vies_field(npts,scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(run)%exp_mean_field(npts,scamtec%nvar,scamtec%ntime_forecast))

          !Allocate(dado(i)%desp(scamtec%nvar,scamtec%ntime_forecast))! paulo dias

          dado(run)%rmse = 0.0
          dado(run)%vies = 0.0
          dado(run)%acor = 0.0

          dado(run)%rmse_field = 0.0
          dado(run)%vies_field = 0.0
          dado(run)%exp_mean_field = 0.0
          DO i = 1, scamtec%nvar
             dado(run)%rmse_field(Idx(1:nidx,i),i,:) = scamtec%udef
             dado(run)%vies_field(Idx(1:nidx,i),i,:) = scamtec%udef
             dado(run)%exp_mean_field(Idx(1:nidx,i),i,:) = scamtec%udef
          ENDDO

         ! dado(i)%desp = 0.0 ! paulo dias

       !Enddo

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

    if (associated(reffield)) nullify(reffield)
    if (associated(expfield)) nullify(expfield)
    if (associated(clmfield)) nullify(clmfield)

    !
    ! Desalocando variaveis
    !

    DeAllocate(Idx)

    !
    ! Fechando arquivo 
    !

    close(FUnitOut)

  End Subroutine FinalizeBstat

  !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE:  CalcBstat
  !
  ! !DESCRIPTION: This routine ....
  !
  !\\
  !\\
  ! !INTERFACE:
  !

  Subroutine CalcBstat( run )
    Implicit None

    !
    ! !INPUT PARAMETERS:
    !

    integer, intent(in) :: run ! experiment number

    !
    !
    ! !REVISION HISTORY: 
    !  19 October 2012 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    integer             :: i, j, v
    integer             :: npts
    real                :: tmp
    real, allocatable   :: anomfield(:,:)
        
    integer :: ier



    character(len=*),parameter :: myname_=myname//'::CalcBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    CALL InitBstat( run )

    npts = scamtec%nxpt*scamtec%nypt

    Allocate(diffield(npts,scamtec%nvar),stat=ier)
    diffield = scamtec%udef

    DO i = 1, scamtec%nvar
       nidx = count(Idx(:,i).gt.0)

       diffield(Idx(1:nidx,i),i) = expfield(Idx(1:nidx,i),i) - reffield(Idx(1:nidx,i),i)
       


    !    Allocate(rmse(scamtec%nvar))
    !    Allocate(vies(scamtec%nvar))
    !    Allocate(acor(scamtec%nvar))

       Allocate(anomfield(npts,2))
       j = scamtec%ftime_idx
       


      
       dado(run)%rmse_field(Idx(1:nidx,i),i,j) = dado(run)%rmse_field(Idx(1:nidx,i),i,j) + & 
                                                (diffield(Idx(1:nidx,i),i)*diffield(Idx(1:nidx,i),i))

       dado(run)%vies_field(Idx(1:nidx,i),i,j) = dado(run)%vies_field(Idx(1:nidx,i),i,j) + & 
                                                (diffield(Idx(1:nidx,i),i))

       dado(run)%exp_mean_field(Idx(1:nidx,i),i,j) = dado(run)%exp_mean_field(Idx(1:nidx,i),i,j) + & 
                                                (expfield(Idx(1:nidx,i),i))

       dado(run)%rmse(i,j) = dado(run)%rmse(i,j) + sum (diffield(Idx(1:nidx,i),i)*diffield(Idx(1:nidx,i),i)) / nidx
       dado(run)%vies(i,j) = dado(run)%vies(i,j) + sum (diffield(Idx(1:nidx,i),i)) / nidx


       if (scamtec%cflag.eq.1)then

          anomfield(Idx(1:nidx,i),1) = expfield(Idx(1:nidx,i),i)-clmfield(Idx(1:nidx,i),i)
          anomfield(Idx(1:nidx,i),2) = reffield(Idx(1:nidx,i),i)-clmfield(Idx(1:nidx,i),i)

          CALL corr(anomfield(Idx(1:nidx,i),1),&
               anomfield(IdX(1:nidx,i),2),&
               tmp              &
              )

       else
          CALL corr(expfield(Idx(1:nidx,i),i),&
               reffield(Idx(1:nidx,i),i),&
               tmp             &
              )
       endif

       dado(run)%acor(i,j) = dado(run)%acor(i,j) + tmp
!
!       EndDo

       DeAllocate(anomfield)

    ENDDO
    
    if ( scamtec%time_step.eq.scamtec%ntime_steps )then
       CALL WriteBstat( run )
    endif
    
    DeAllocate(diffield)
    !    DeAllocate(rmse)
    !    DeAllocate(vies)
    !    DeAllocate(acor)

    CALL FinalizeBStat( run )


  End Subroutine CalcBstat

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

    integer, intent(in) :: run ! experiment number

    !
    !
    ! !REVISION HISTORY: 
    !  01 February 2013 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    integer            :: iret,i,j
    character(len=512) :: fname, fmt
    integer            :: nymd, nhms
    integer            :: fymd, fhms
    integer            :: nidx
    integer            :: npts
    real               :: k

    inquire(unit=FUnitOut, opened=iret)
    if(.not.iret) then

       nymd = scamtec%starting_time/100
       nhms = MOD(scamtec%starting_time,100) * 10000
       fymd = scamtec%ending_time/100
       fhms = MOD(scamtec%ending_time,100) * 10000

       fname = FNameOut

       CALL str_template(FName, nymd, nhms, fymd, fhms, int2str(run,'(I2.2)'))


       Open( unit   = FUnitOut+0,      &
            file   = trim(scamtec%output_dir)//'/RMSE'//trim(Fname),   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+1,      &
            file   = trim(scamtec%output_dir)//'/VIES'//trim(Fname),   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+2,      &
            file   = trim(scamtec%output_dir)//'/ACOR'//trim(Fname),   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+3,      &
            file   = trim(scamtec%output_dir)//'/RMSEF'//trim(Fname),   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )
       Open( unit   = FUnitOut+4,      &
            file   = trim(scamtec%output_dir)//'/VIESF'//trim(Fname),   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )
       Open( unit   = FUnitOut+5,      &
            file   = trim(scamtec%output_dir)//'/MEANF'//trim(Fname),   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )           
    endif

    write(fmt,'(A9,I3.3,A5)')'(6x,I3.3,',scamtec%nvar,'F9.3)'

    j = scamtec%ftime_idx

    dado(run)%rmse(:,j) = sqrt(dado(run)%rmse(:,j) / scamtec%ftime_count(j))
    dado(run)%vies(:,j) = dado(run)%vies(:,j) / scamtec%ftime_count(j)
    dado(run)%acor(:,j) = dado(run)%acor(:,j) / scamtec%ftime_count(j)
    

    
    !dado(run)%desp(:,i) = sqrt(dado(run)%desp(:,i) / (scamtec%ftime_count(i)-1)) ! paulo dias

    write(FunitOut+0,fmt)(j-1)*scamtec%ftime_step,(dado(run)%rmse(i,j),i=1,scamtec%nvar)
    write(FunitOut+1,fmt)(j-1)*scamtec%ftime_step,(dado(run)%vies(i,j),i=1,scamtec%nvar)
    write(FunitOut+2,fmt)(j-1)*scamtec%ftime_step,(dado(run)%acor(i,j),i=1,scamtec%nvar)

    npts = scamtec%nxpt*scamtec%nypt

    DO i=1,scamtec%nvar
        nidx = count (scamdata(run)%UdfIdx(1:npts,i))

        dado(run)%rmse_Field(Idx(1:nidx,i),i,j) = sqrt(dado(run)%rmse_Field(Idx(1:nidx,i),i,j)/ scamtec%ftime_count(j))
        dado(run)%vies_Field(Idx(1:nidx,i),i,j) = dado(run)%vies_Field(Idx(1:nidx,i),i,j)/ scamtec%ftime_count(j)
        dado(run)%exp_mean_Field(Idx(1:nidx,i),i,j) = dado(run)%exp_mean_Field(Idx(1:nidx,i),i,j)/ scamtec%ftime_count(j)
       
        write(FunitOut+3)dado(run)%rmse_Field(:,i,j)
        write(FunitOut+4)dado(run)%vies_Field(:,i,j)
        write(FunitOut+5)dado(run)%exp_mean_Field(:,i,j)
!print*,minval(dado(run)%rmse_Field(:,i,j)),maxval(dado(run)%rmse_Field(:,i,j))
    ENDDO



    Close(FUnitOut+0)
    Close(FUnitOut+1)
    Close(FUnitOut+2)
    Close(FUnitOut+3)
    Close(FUnitOut+4)
    Close(FUnitOut+5)

  End Subroutine WriteBstat
  !EOC
  !
  !-----------------------------------------------------------------------------!

  !=======================================
  Subroutine corr(A,B,Rho)
    !
    ! Calcula a correlacao entre os
    ! campos de anomalia X e Y.
    ! Retorna o valor na variavel Rho
    !
    ! A [Input]    = Left anomaly Field       => A(xp*yp)
    ! B [Input]    = Right anomaly Field      => B(xp*yp)
    ! Rho [OutPut] = Correlation
    !=======================================

    Implicit None

    Real, Intent(In), Dimension(:) :: A
    Real, Intent(In), Dimension(:) :: B
    Real, Intent(Out) :: Rho
    !
    Real :: Sab, Saa, Sbb
    Real :: Ma, Mb
    Integer      ::  NPts
    !

    NPts  = size(A,1)

    Ma  = sum(A)/float(NPts)
    Mb  = sum(B)/float(NPts)

    Sab = sum( (A-Ma)*(B-Mb) )
    Saa = sum( (A-Ma)*(A-Ma) )
    Sbb = sum( (B-Mb)*(B-Mb) )
    Rho =  Sab/sqrt(Saa*Sbb)

    return

  End Subroutine corr


End Module SCAM_bstatistic
