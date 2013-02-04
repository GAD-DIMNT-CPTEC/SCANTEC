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
  real, allocatable :: rmse(:)
  real, allocatable :: vies(:)
  real, allocatable :: acor(:)
  real, allocatable :: tmp(:)

  integer, allocatable :: Idx(:)
  integer              :: nidx


  character(len=512) :: FNameOut = 'BstatExp%e_%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.scam'
  character(len=512) :: FNameCtl = 'BstatExp%e_%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.ctl'
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
    character(len=512) :: fname
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
  
    !
    ! Abrindo arquivo de saida
    !

    if(scamtec%loop_count.eq.1)then

       nymd = scamtec%starting_time/100
       nhms = MOD(scamtec%starting_time,100) * 10000
       fymd = scamtec%ending_time/100
       fhms = MOD(scamtec%ending_time,100) * 10000
       
       fname = FNameOut
       CALL str_template(FName, nymd, nhms, fymd, fhms, int2str(run,'(I2.2)'))

       open(unit   = FUnitOut,      &
            File   = Trim(FName),   &
            access = 'sequential',  &
            Form   = 'Unformatted', &
            Status = 'unknown'      &
            )

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

  Subroutine FinalizeBstat( )
    Implicit None
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

    integer             :: i
    real, allocatable   :: anomfield(:,:)
    


    character(len=*),parameter :: myname_=myname//'::CalcBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    CALL InitBstat( run )

    Allocate(diffield(scamtec%nxpt*scamtec%nypt,scamtec%nvar))
    diffield = scamtec%udef

    diffield = expfield(Idx,:) - reffield(Idx,:)

    Allocate(rmse(scamtec%nvar))
    Allocate(vies(scamtec%nvar))
    Allocate(acor(scamtec%nvar))

    Allocate(anomfield(size(Idx),2))

    Do i = 1, scamtec%nvar

       rmse(i) = sum (diffield(Idx,i)*diffield(Idx,i)) / size(Idx)
       vies(i) = sum (diffield(Idx,i)) / size(Idx)

       if (scamtec%cflag.eq.1)then

          anomfield(:,1) = expfield(Idx,i)-clmfield(Idx,i)
          anomfield(:,2) = reffield(Idx,i)-clmfield(Idx,i)

          CALL corr(anomfield(Idx,1),&
                    anomfield(IdX,2),&
                    acor(i)                         &
                    )

       else
          CALL corr(expfield(Idx,i),&
                    reffield(Idx,i),&
                    acor(i)         &
                    )
       endif

    EndDo


    CALL WriteBstat( run )

    DeAllocate(anomfield)
    DeAllocate(diffield)
    DeAllocate(rmse)
    DeAllocate(vies)
    DeAllocate(acor)

    CALL FinalizeBStat( )


  End Subroutine CalcBstat


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
    integer            :: iret
    character(len=512) :: fname
    integer            :: nymd, nhms
    integer            :: fymd, fhms

    inquire(unit=FUnitOut, opened=iret)
    if(.not.iret) then

       nymd = scamtec%starting_time/100
       nhms = MOD(scamtec%starting_time,100) * 10000
       fymd = scamtec%ending_time/100
       fhms = MOD(scamtec%ending_time,100) * 10000

       fname = FNameOut

       CALL str_template(FName, nymd, nhms, fymd, fhms, int2str(run,'(I2.2)'))


       Open( unit   = FUnitOut,      &
             file   = trim(Fname),   &
             form   = 'Unformatted', &
             access = 'append'       &
            )

       write(FUnitOut) rmse(:)
       write(FUnitOut) vies(:)
       write(FUnitOut) acor(:)


    endif

    !
    ! escrevendo ctl dos resultados caso seja o ultimo tempo
    !

    if (scamtec%atime_step.eq.scamtec%ntime_steps)then

       fname = FNameCtl
       CALL str_template(FName, nymd, nhms, fymd, fhms, int2str(run,'(I2.2)'))

       open(Unit = FUnitOut+1,  &
            File = trim(fname), &
            Form = 'Formatted'  &
            )


       fname = FNameOut
       CALL str_template(FName, nymd, nhms, fymd, fhms, int2str(run,'(I2.2)'))
       write(FUnitOut+1,           '(2(A,1x))')'dset',Trim(FName)
       write(FUnitOut+1,         '(A,1x,F9.3)')'undef',scamtec%udef
       write(FUnitOut+1,      '(A,1x,I3,1x,A)')'xdef',scamtec%nvar,'linear 1 1'
       write(FUnitOut+1,                 '(A)')'ydef   1 linear 1 1'
       write(FUnitOut+1,'(A,1x,I3,1x,A,1x,I3)')'zdef',scamtec%ntime_forecast,'linear 0',scamtec%time_step

       fname = '%h2Z%d2%mc%y4'
       CALL str_template(FName, nymd, nhms)
       write(FUnitOut+1,'(A,1x,I3,1x,A,1x,A,1x,I3,A)')'tdef',scamtec%ntime_steps,'linear',trim(fname),scamtec%time_step,'hr'
       write(FUnitOut+1,                 '(A)')'vars 3'
       write(FUnitOut+1,      '(A,1x,I3,1x,A)')'rmse',scamtec%ntime_forecast,'99 Root Mean Square Error'
       write(FUnitOut+1,      '(A,1x,I3,1x,A)')'vies',scamtec%ntime_forecast,'99 Vies'
       write(FUnitOut+1,      '(A,1x,I3,1x,A)')'acor',scamtec%ntime_forecast,'99 Anomaly Correlation'
       write(FUnitOut+1,                 '(A)')'endvars'

       write(FUnitOut+1,      '(A)')'*                                       *'
       write(FUnitOut+1,      '(A)')'*                                       *'
       write(FUnitOut+1,      '(A)')'*                   LEIAME              *'
       write(FUnitOut+1,      '(A)')'*                                       *'
       write(FUnitOut+1,      '(A)')'* Este ctl foi criado para plotar os'
       write(FUnitOut+1,      '(A)')'* resultados da estatistica basica (rmse,vies, acor)'
       write(FUnitOut+1,      '(A)')'* Na dimensao Xdef estao as variaveis'
       write(FUnitOut+1,      '(A)')'* avaliadadas na seguinte ordem:'
       write(FUnitOut+1,      '(A)')'* 1-VTMP:925'
       write(FUnitOut+1,      '(A)')'* 2-VTMP:850'
       write(FUnitOut+1,      '(A)')'* 3-VTMP:500'
       write(FUnitOut+1,      '(A)')'* 4-PSNM:000'
       write(FUnitOut+1,      '(A)')'* 5-UMES:925'
       write(FUnitOut+1,      '(A)')'* 6-AGPL:925'
       write(FUnitOut+1,      '(A)')'* 7-ZGEO:850'
       write(FUnitOut+1,      '(A)')'* 8-ZGEO:500'
       write(FUnitOut+1,      '(A)')'* 9-ZGEO:250'
       write(FUnitOut+1,      '(A)')'* 10-UVEL:850'
       write(FUnitOut+1,      '(A)')'* 11-UVEL:500'
       write(FUnitOut+1,      '(A)')'* 12-UVEL:250'
       write(FUnitOut+1,      '(A)')'* 13-VVEL:850'
       write(FUnitOut+1,      '(A)')'* 14-VVEL:500'
       write(FUnitOut+1,      '(A)')'* 15-VVEL:250'
       write(FUnitOut+1,      '(A)')'* A dimensao zdef correponde aos horarios'
       write(FUnitOut+1,      '(A)')'* de previsao com relacao a data tdef'

       close(Unit=FUnitOut+1)
    endif


  End Subroutine WriteBstat
  !EOC
  !
  !-----------------------------------------------------------------------------!


End Module SCAM_bstatistic
