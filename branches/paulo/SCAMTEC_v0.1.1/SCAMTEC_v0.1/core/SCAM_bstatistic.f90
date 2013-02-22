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
  end type bstat

  type(bstat), allocatable :: dado(:)

  integer, allocatable :: Idx(:)
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

    integer            :: i, npts
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
            File   = trim(scamtec%output_dir)//'RMSE'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+1,      &
            File   = trim(scamtec%output_dir)//'VIES'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+2,      &
            File   = trim(scamtec%output_dir)//'ACOR'//Trim(FName),   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )

       write(fmt,'(A4,I3.3,A5)')'(A9,',scamtec%nvar,'A9)'
       write(FUnitOut+0,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar)
       write(FUnitOut+1,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar)
       write(FUnitOut+2,fmt)'%Previsao',(scamtec%VarName(i),i=1,scamtec%nvar)


       !
       ! Allocando Memoria para o calculo dos Indices
       !

       Allocate(dado(scamtec%nexp))
       Do i=1,scamtec%nexp

          Allocate(dado(i)%rmse(scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(i)%vies(scamtec%nvar,scamtec%ntime_forecast))
          Allocate(dado(i)%acor(scamtec%nvar,scamtec%ntime_forecast))

          dado(i)%rmse = 0.0
          dado(i)%vies = 0.0
          dado(i)%acor = 0.0

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

    integer             :: i, j
    real                :: tmp
    real, allocatable   :: anomfield(:,:)



    character(len=*),parameter :: myname_=myname//'::CalcBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    CALL InitBstat( run )

    Allocate(diffield(scamtec%nxpt*scamtec%nypt,scamtec%nvar))
    diffield = scamtec%udef
            
    diffield = expfield(Idx,:) - reffield(Idx,:)

    !    Allocate(rmse(scamtec%nvar))
    !    Allocate(vies(scamtec%nvar))
    !    Allocate(acor(scamtec%nvar))

    Allocate(anomfield(size(Idx),2))

    j = scamtec%ftime_idx
           
    Do i = 1, scamtec%nvar

       dado(run)%rmse(i,j) = dado(run)%rmse(i,j) + sum (diffield(Idx,i)*diffield(Idx,i)) / size(Idx)
       dado(run)%vies(i,j) = dado(run)%vies(i,j) + sum (diffield(Idx,i)) / size(Idx)

       if (scamtec%cflag.eq.1)then

          anomfield(:,1) = expfield(Idx,i)-clmfield(Idx,i)
          anomfield(:,2) = reffield(Idx,i)-clmfield(Idx,i)

          CALL corr(anomfield(Idx,1),&
               anomfield(IdX,2),&
               tmp              &
               )

       else
          CALL corr(expfield(Idx,i),&
               reffield(Idx,i),&
               tmp             &
               )
       endif

       dado(run)%acor(i,j) = dado(run)%acor(i,j) + tmp

    EndDo

    if ( scamtec%time_step.eq.scamtec%ntime_steps )then
       CALL WriteBstat( run )
    endif


    DeAllocate(anomfield)
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
            file   = trim(scamtec%output_dir)//'RMSE'//trim(Fname),   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+1,      &
            file   = trim(scamtec%output_dir)//'VIES'//trim(Fname),   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+2,      &
            file   = trim(scamtec%output_dir)//'ACOR'//trim(Fname),   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
    endif

    write(fmt,'(A9,I3.3,A5)')'(6x,I3.3,',scamtec%nvar,'F9.3)'

    i = scamtec%ftime_idx

    dado(run)%rmse(:,i) = sqrt(dado(run)%rmse(:,i) / scamtec%ftime_count(i))
    dado(run)%vies(:,i) = dado(run)%vies(:,i) / scamtec%ftime_count(i)
    dado(run)%acor(:,i) = dado(run)%acor(:,i) / scamtec%ftime_count(i)

    write(FunitOut+0,fmt)(i-1)*scamtec%time_step,(dado(run)%rmse(j,i),j=1,scamtec%nvar)
    write(FunitOut+1,fmt)(i-1)*scamtec%time_step,(dado(run)%vies(j,i),j=1,scamtec%nvar)
    write(FunitOut+2,fmt)(i-1)*scamtec%time_step,(dado(run)%acor(j,i),j=1,scamtec%nvar)




    Close(FUnitOut)




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
