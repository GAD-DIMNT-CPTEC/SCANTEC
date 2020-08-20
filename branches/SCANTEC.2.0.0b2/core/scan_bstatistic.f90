!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: scan_bstatistic.f90
!
! !DESCRIPTON:
!             
!                 
!\\
!\\
! !INTERFACE:
!


Module scan_bstatistic

  !
  ! !USES:
  !

  USE scantec_module                ! scantec types
  USE scan_dataMOD, only : scandata ! scantec data matrix
  USE m_ioutil                      ! Module to defines std. I/O parameters
  USE m_string                      ! string manipulations
  use omp_lib
  USE scan_Utils, only: Exper 

  IMPLICIT NONE
  PRIVATE
  !
  ! !PARAMETERS:
  !

  character(len=*),parameter :: myname='scan_bstatistic' 

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
  
  type idxUndef
     integer, allocatable :: pt(:)
     type(idxUndef), pointer :: next => null()
  end type
  type(idxUndef), pointer :: idxFirstVar => null()
  type(idxUndef), pointer :: idx => null()



  character(len=512) :: FNameOut = '%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2'
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

    character(len=512)   :: fname, fmt
    integer              :: i, j
    integer              :: nvar, npts
    integer              :: istat
    integer              :: nidx
    integer              :: nymd, nhms
    integer              :: fymd, fhms
    logical, pointer     :: RefBitMap(:,:) => null()
    logical, pointer     :: expBitMap(:,:) => null()
    logical, allocatable :: bitMap(:,:)

    character(len=*),parameter :: myname_=myname//'::InitBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif
    !
    ! transferindo dados para o calculo dos indices
    !

    call scantec%getField('refer',reffield,istat)
    call scantec%getBitMap('refer',refBitMap,istat)

    call scantec%getField('clima',clmfield,istat)

    call scantec%getField(Exper(run)%Name, expfield, istat)
    call scantec%getBitMap(Exper(run)%Name, expBitMap, istat)
    !
    ! Identificando Indice dos pontos validos
    !

    npts = scantec%nxpt*scantec%nypt
    nvar = scantec%nvar

    ! Merge reference and experiment
    allocate(bitMap(npts,nvar))
    bitMap = refBitMap .and. expBitMap

    allocate(idxFirstVar)
    idx => idxFirstVar

    DO i=1,nvar
       nidx = count (bitMap(1:npts,i))
       allocate(idx%pt(nidx))
       Idx%pt(1:nidx) = PACK ( (/(j,j=1,npts)/), mask = BitMap(1:npts,i))
       if(i.lt.nvar)then
          allocate(idx%next)
          idx => idx%next
       endif
    ENDDO

    deallocate(bitMap)
    
    if(scantec%loop_count.eq.1)then

       !
       ! Abrindo arquivo de saida e escrevendo o Cabecalho
       !

       nymd = scantec%starting_time/100
       nhms = MOD(scantec%starting_time,100) * 10000
       fymd = scantec%ending_time/100
       fhms = MOD(scantec%ending_time,100) * 10000

       fname = trim(Exper(Run)%name)//'_'//trim(FNameOut)
       CALL str_template(FName, nymd, nhms, fymd, fhms, label=num2str(run,'(I2.2)'))

       open(unit   = FUnitOut+0,      &
            File   = trim(scantec%output_dir)//'/RMSE'//Trim(FName)//'T.scan',   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+1,      &
            File   = trim(scantec%output_dir)//'/VIES'//Trim(FName)//'T.scan',   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+2,      &
            File   = trim(scantec%output_dir)//'/ACOR'//Trim(FName)//'T.scan',   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+3,      &
            File   = trim(scantec%output_dir)//'/RMSE'//Trim(FName)//'F.scan',   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )
            
       open(unit   = FUnitOut+4,      &
            File   = trim(scantec%output_dir)//'/VIES'//Trim(FName)//'F.scan',   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )

       open(unit   = FUnitOut+5,      &
            File   = trim(scantec%output_dir)//'/MEAN'//Trim(FName)//'F.scan',   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )

       write(fmt,'(A7,I3.3,A8)')'(1x,A9,',scantec%nvar,'(1x,A9))'
       write(FUnitOut+0,fmt)'%Previsao',(scantec%VarName(i),i=1,scantec%nvar)
       write(FUnitOut+1,fmt)'%Previsao',(scantec%VarName(i),i=1,scantec%nvar)
       write(FUnitOut+2,fmt)'%Previsao',(scantec%VarName(i),i=1,scantec%nvar)
     !  write(FUnitOut+3,fmt)'%Previsao',(scantec%VarName(i),i=1,scantec%nvar) ! paulo dias

       !
       ! Allocando Memoria para o calculo dos Indices
       !

       if(.NOT.Allocated(dado))Allocate(dado(scantec%nexp))
       !Do i=1,scantec%nexp

          Allocate(dado(run)%rmse(scantec%nvar,scantec%ntime_forecast))
          Allocate(dado(run)%vies(scantec%nvar,scantec%ntime_forecast))
          Allocate(dado(run)%acor(scantec%nvar,scantec%ntime_forecast))
          Allocate(dado(run)%rmse_field(npts,scantec%nvar,scantec%ntime_forecast))
          Allocate(dado(run)%vies_field(npts,scantec%nvar,scantec%ntime_forecast))
          Allocate(dado(run)%exp_mean_field(npts,scantec%nvar,scantec%ntime_forecast))

          !Allocate(dado(i)%desp(scantec%nvar,scantec%ntime_forecast))! paulo dias

          dado(run)%rmse = 0.0
          dado(run)%vies = 0.0
          dado(run)%acor = 0.0

          dado(run)%rmse_field = 0.0
          dado(run)%vies_field = 0.0
          dado(run)%exp_mean_field = 0.0

         ! dado(i)%desp = 0.0 ! paulo dias

       !Enddo

       ! write ctl
       call writeCtl(FName)

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

    idx => IdxFirstVar%next
    do while(associated(idx))
       deallocate(idxFirstVar)
       idxFirstVar => idx
       idx => idxFirstVar%next
    enddo

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

    integer             :: i, j, k, p, v
    integer             :: nidx, npts
    real                :: tmp, TmpRMSE, TmpVIES, TmpDiff1, TmpDiff2
    real                :: expMD, refMD
    real, allocatable   :: anomfield(:,:)
        
    integer :: ier



    character(len=*),parameter :: myname_=myname//'::CalcBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    CALL InitBstat( run )

    npts = scantec%nxpt*scantec%nypt

    idx => idxFirstVar
    DO i = 1, scantec%nvar
       nidx = count(Idx%pt.gt.0)
       if (nidx .eq. 0)then
          idx => idx%next
          cycle
       endif
       refMD = sum(refField(Idx%pt,i))/nidx
       expMD = sum(expField(Idx%pt,i))/nidx
       
       !
       ! PRECISO INCLUIR AQUI ALGO PARA CONTAR OS ARQUIVOS QUE SÃO LIDOS
       ! PARA CADA VARIAVEL. AI UTILIZAR ISSO PRA FAZER A MÉDIA
       !
       j    = scantec%ftime_idx

       TmpVIES = 0.0
       TmpRMSE = 0.0

       DO k=1,nidx

          p = Idx%pt(k)
          
          TmpDiff1 = expfield(p,i) - reffield(p,i)
          TmpDiff2 = TmpDiff1 * TmpDiff1

          TmpVIES  = TmpVIES + TmpDiff1
          TmpRMSE  = TmpRMSE + TmpDiff2
!
          dado(run)%vies_field(p,i,j)     = dado(run)%vies_field(p,i,j) + TmpDiff1
          dado(run)%rmse_field(p,i,j)     = dado(run)%rmse_field(p,i,j) + TmpDiff2
          dado(run)%exp_mean_field(p,i,j) = dado(run)%exp_mean_field(p,i,j) + expfield(p,i)

       ENDDO

       dado(run)%vies(i,j) = dado(run)%vies(i,j) + TmpVIES / nidx
       dado(run)%rmse(i,j) = dado(run)%rmse(i,j) + TmpRMSE / nidx

       Allocate(anomfield(npts,2))

       if (scantec%cflag.eq.1)then

          anomfield(Idx%pt,1) = expfield(Idx%pt,i)-clmfield(Idx%pt,i)
          anomfield(Idx%pt,2) = reffield(Idx%pt,i)-clmfield(Idx%pt,i)

       else

          anomfield(Idx%pt,1) = expField(Idx%pt,i) - expMD
          anomfield(Idx%pt,2) = refField(Idx%pt,i) - refMD
          
       endif

       CALL corr(anomfield(Idx%pt,1),&
                 anomfield(IdX%pt,2),&
                 tmp              &
                )

       dado(run)%acor(i,j) = dado(run)%acor(i,j) + tmp
!
!       EndDo

       DeAllocate(anomfield)
       idx => idx%next
    ENDDO
    
    if ( scantec%time_step.eq.scantec%ntime_steps )then
       CALL WriteBstat( run )
    endif
    
    !    DeAllocate(diffield)
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

    integer              :: iret,i,j
    character(len=512)   :: fname, fmt
    integer              :: nymd, nhms
    integer              :: fymd, fhms
    integer              :: nidx
    integer              :: npts
    integer              :: nvar
    integer              :: istat
    real                 :: k
    logical              :: isOpen
    logical, pointer     :: refBitMap(:,:) => null()
    logical, pointer     :: expBitMap(:,:) => null()
    logical, allocatable :: bitMap(:,:)


    nymd = scantec%starting_time/100
    nhms = MOD(scantec%starting_time,100) * 10000
    fymd = scantec%ending_time/100
    fhms = MOD(scantec%ending_time,100) * 10000

!       fname = FNameOut
    fname = trim(Exper(Run)%name)//'_'//trim(FNameOut)

    CALL str_template(FName, nymd, nhms, fymd, fhms, label=num2str(run,'(I2.2)'))

    inquire(unit=FUnitOut, opened=isOpen)
    if(.not.isOpen) then

       Open( unit   = FUnitOut+0,      &
            file   = trim(scantec%output_dir)//'/RMSE'//trim(Fname)//'T.scan',   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+1,      &
            file   = trim(scantec%output_dir)//'/VIES'//trim(Fname)//'T.scan',   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+2,      &
            file   = trim(scantec%output_dir)//'/ACOR'//trim(Fname)//'T.scan',   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+3,      &
            file   = trim(scantec%output_dir)//'/RMSE'//trim(Fname)//'F.scan',   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )
       Open( unit   = FUnitOut+4,      &
            file   = trim(scantec%output_dir)//'/VIES'//trim(Fname)//'F.scan',   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )
       Open( unit   = FUnitOut+5,      &
            file   = trim(scantec%output_dir)//'/MEAN'//trim(Fname)//'F.scan',   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )           
    endif

    write(fmt,'(A9,I3.3,A6)')'(7x,I3.3,',scantec%nvar,'F10.3)'

    j = scantec%ftime_idx

    dado(run)%rmse(:,j) = sqrt(dado(run)%rmse(:,j) / scantec%ftime_count(j))
    dado(run)%vies(:,j) = dado(run)%vies(:,j) / scantec%ftime_count(j)
    dado(run)%acor(:,j) = dado(run)%acor(:,j) / scantec%ftime_count(j)
        
    !dado(run)%desp(:,i) = sqrt(dado(run)%desp(:,i) / (scantec%ftime_count(i)-1)) ! paulo dias

    write(FunitOut+0,fmt)(j-1)*scantec%ftime_step,(dado(run)%rmse(i,j),i=1,scantec%nvar)
    write(FunitOut+1,fmt)(j-1)*scantec%ftime_step,(dado(run)%vies(i,j),i=1,scantec%nvar)
    write(FunitOut+2,fmt)(j-1)*scantec%ftime_step,(dado(run)%acor(i,j),i=1,scantec%nvar)

    npts = scantec%nxpt*scantec%nypt
    nvar = scantec%nvar

    call scantec%getBitMap('refer',refBitMap,istat)
    call scantec%getBitMap(Exper(run)%Name, expBitMap, istat)

    allocate(bitMap(npts,nvar))
    bitMap = refBitMap .and. expBitMap
    
    idx => idxFirstVar
    DO i=1,nvar
        nidx = count (bitMap(1:npts,i))

        dado(run)%rmse_Field(Idx%pt,i,j) = sqrt(dado(run)%rmse_Field(Idx%pt,i,j)/ scantec%ftime_count(j))
        dado(run)%vies_Field(Idx%pt,i,j) = dado(run)%vies_Field(Idx%pt,i,j)/ scantec%ftime_count(j)
        dado(run)%exp_mean_Field(Idx%pt,i,j) = dado(run)%exp_mean_Field(Idx%pt,i,j)/ scantec%ftime_count(j)
       
        where(.not.bitmap(:,i)) dado(run)%rmse_Field(:,i,j) = scantec%udef
        where(.not.bitmap(:,i)) dado(run)%vies_Field(:,i,j) = scantec%udef
        where(.not.bitmap(:,i)) dado(run)%exp_mean_Field(:,i,j) = scantec%udef

        write(FunitOut+3)dado(run)%rmse_Field(:,i,j)
        write(FunitOut+4)dado(run)%vies_Field(:,i,j)
        write(FunitOut+5)dado(run)%exp_mean_Field(:,i,j)
        idx => idx%next
    !print*,minval(dado(run)%rmse_Field(:,i,j)),maxval(dado(run)%rmse_Field(:,i,j))
    ENDDO



    Close(FUnitOut+0)
    Close(FUnitOut+1)
    Close(FUnitOut+2)
    Close(FUnitOut+3)
    Close(FUnitOut+4)
    Close(FUnitOut+5)
    
  End Subroutine WriteBstat

  subroutine writeCtl( FName )
     character(len=*), intent(in) :: fname
     integer :: i, j 

      open(150, file=trim(scantec%output_dir)//'/RMSE'//trim(Fname)//'F.ctl',&
                status='unknown')
      
      write(150,'(A,A)')'dset ^','RMSE'//trim(Fname)//'F.scan'
      write(150,'(A)')
      write(150,'(A)')'options sequential'
      write(150,'(A)')      
      write(150,'(A)')'undef -999.9'      
      write(150,'(A)')      
      write(150,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scantec%nxpt,'linear', scantec%gridDesc(5), scantec%gridDesc(10)
      write(150,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scantec%nypt,'linear', scantec%gridDesc(4), scantec%gridDesc(9)      
      write(150,'(A)') 
      write(150,'(A)')'zdef    1 linear 0 1'                  
      write(150,'(A)') 
      write(150,'(A,I3,A,I2,A)')'tdef  ',(scantec%forecast_time/scantec%atime_step)+1,' linear 00Z05AUG2014 ',scantec%atime_step,'HR'
      write(150,'(A)')
      write(150,'(A,1x,I3)')'vars', scantec%nvar
      do i=1,scantec%nvar
         j = index(scantec%varName(i),':')
         write(150,'(A,2(1x,A))')scantec%varName(i)(1:j-1)//scantec%varName(i)(j+1:len_trim(scantec%varName(i))),&
                                '0 99',trim(scantec%varDesc(i))
      enddo
      write(150,'(A)')'endvars'   
      
      close(150)
    
      !VIES   
      open(151, file=trim(scantec%output_dir)//'/VIES'//trim(Fname)//'F.ctl',&
                status='unknown')

      
      write(151,'(A,A)')'dset ^','VIES'//trim(Fname)//'F.scan'
      write(151,'(A)')
      write(151,'(A)')'options sequential'
      write(151,'(A)')      
      write(151,'(A)')'undef -999.9'      
      write(151,'(A)')      
      write(151,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scantec%nxpt,'linear', scantec%gridDesc(5), scantec%gridDesc(10)
      write(151,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scantec%nypt,'linear', scantec%gridDesc(4), scantec%gridDesc(9)      
      write(151,'(A)') 
      write(151,'(A)')'zdef    1 linear 0 1'                  
      write(151,'(A)') 
      write(151,'(A,I3,A,I2,A)')'tdef  ',(scantec%forecast_time/scantec%atime_step)+1,' linear 00Z05AUG2014 ',scantec%atime_step,'HR'
      write(151,'(A)')
      write(151,'(A,1x,I3)')'vars', scantec%nvar
      do i=1,scantec%nvar
         j = index(scantec%varName(i),':')
         write(151,'(A,2(1x,A))')scantec%varName(i)(1:j-1)//scantec%varName(i)(j+1:len_trim(scantec%varName(i))),&
                                '0 99',trim(scantec%varDesc(i))
      enddo
      write(151,'(A)')'endvars'   
      
      close(151)
      
      !MEAN   
      open(152, file=trim(scantec%output_dir)//'/MEAN'//trim(Fname)//'F.ctl',&
                status='unknown')
      
      write(152,'(A,A)')'dset ^','MEAN'//trim(Fname)//'F.scan'
      write(152,'(A)')
      write(152,'(A)')'options sequential'
      write(152,'(A)')      
      write(152,'(A)')'undef -999.9'      
      write(152,'(A)')      
      write(152,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scantec%nxpt,'linear', scantec%gridDesc(5), scantec%gridDesc(10)
      write(152,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scantec%nypt,'linear', scantec%gridDesc(4), scantec%gridDesc(9)      
      write(152,'(A)') 
      write(152,'(A)')'zdef    1 linear 0 1'                  
      write(152,'(A)') 
      write(152,'(A,I3,A,I2,A)')'tdef  ',(scantec%forecast_time/scantec%atime_step)+1,' linear 00Z05AUG2014 ',scantec%atime_step,'HR'
      write(152,'(A)')
      write(152,'(A,1x,I3)')'vars', scantec%nvar
      do i=1,scantec%nvar
         j = index(scantec%varName(i),':')
         write(152,'(A,2(1x,A))')scantec%varName(i)(1:j-1)//scantec%varName(i)(j+1:len_trim(scantec%varName(i))),&
                                '0 99',trim(scantec%varDesc(i))
      enddo
      write(152,'(A)')'endvars'     
      
      close(152)
    
  End Subroutine writeCtl
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
    Real    :: Sab, Saa, Sbb
    Real    :: Ma, Mb
    Integer :: NPts
    Integer :: I
    !

    NPts  = size(A,1)

!    Ma  = sum(A)/float(NPts)
!    Mb  = sum(B)/float(NPts)

!    Sab = sum( (A-Ma)*(B-Mb) )
!    Saa = sum( (A-Ma)*(A-Ma) )
!    Sbb = sum( (B-Mb)*(B-Mb) )

!Estatistica de correlacao de anomalia alterada pelo Joao
    Sab = sum( A*B )
    Saa = sum( A*A )
    Sbb = sum( B*B )
    
    Rho =  Sab/sqrt(Saa*Sbb)


    return

  End Subroutine corr


End Module scan_bstatistic
