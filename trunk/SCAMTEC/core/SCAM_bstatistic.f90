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
  use omp_lib
  USE SCAM_Utils, only: Exper 

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

       fname = trim(Exper(Run)%name)//'_'//trim(FNameOut)
       CALL str_template(FName, nymd, nhms, fymd, fhms, label=num2str(run,'(I2.2)'))

       open(unit   = FUnitOut+0,      &
            File   = trim(scamtec%output_dir)//'/RMSE'//Trim(FName)//'T.scam',   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+1,      &
            File   = trim(scamtec%output_dir)//'/VIES'//Trim(FName)//'T.scam',   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+2,      &
            File   = trim(scamtec%output_dir)//'/ACOR'//Trim(FName)//'T.scam',   &
            access = 'sequential',  &
            Form   = 'formatted', &
            Status = 'replace'      &
            )
       open(unit   = FUnitOut+3,      &
            File   = trim(scamtec%output_dir)//'/RMSE'//Trim(FName)//'F.scam',   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )
            
       open(unit   = FUnitOut+4,      &
            File   = trim(scamtec%output_dir)//'/VIES'//Trim(FName)//'F.scam',   &
            access = 'sequential',  &
            Form   = 'unformatted', &
            Status = 'replace'      &
            )

       open(unit   = FUnitOut+5,      &
            File   = trim(scamtec%output_dir)//'/MEAN'//Trim(FName)//'F.scam',   &
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
             dado(run)%rmse_field(Idx(1:nidx,i),i,:) = 0.0 !scamtec%udef
             dado(run)%vies_field(Idx(1:nidx,i),i,:) = 0.0 !scamtec%udef
             dado(run)%exp_mean_field(Idx(1:nidx,i),i,:) = 0.0 !scamtec%udef
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

    integer             :: i, j, k, p, v
    integer             :: npts
    real                :: tmp, TmpRMSE, TmpVIES, TmpDiff1, TmpDiff2
    real, allocatable   :: anomfield(:,:)
        
    integer :: ier



    character(len=*),parameter :: myname_=myname//'::CalcBstat'

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    CALL InitBstat( run )

    npts = scamtec%nxpt*scamtec%nypt


    DO i = 1, scamtec%nvar
       nidx = count(Idx(:,i).gt.0)
       j    = scamtec%ftime_idx

       TmpVIES = 0.0
       TmpRMSE = 0.0

       DO k=1,nidx

          p = Idx(k,i)
          
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

    integer            :: i,j
    character(len=512) :: fname, fmt
    integer            :: nymd, nhms
    integer            :: fymd, fhms
    integer            :: nidx
    integer            :: npts
    real               :: k
    logical            :: Opened

    inquire(unit=FUnitOut, opened=Opened)
    if(.not.Opened) then

       nymd = scamtec%starting_time/100
       nhms = MOD(scamtec%starting_time,100) * 10000
       fymd = scamtec%ending_time/100
       fhms = MOD(scamtec%ending_time,100) * 10000

!       fname = FNameOut
       fname = trim(Exper(Run)%name)//'_'//trim(FNameOut)

       CALL str_template(FName, nymd, nhms, fymd, fhms, label=num2str(run,'(I2.2)'))


       Open( unit   = FUnitOut+0,      &
            file   = trim(scamtec%output_dir)//'/RMSE'//trim(Fname)//'T.scam',   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+1,      &
            file   = trim(scamtec%output_dir)//'/VIES'//trim(Fname)//'T.scam',   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+2,      &
            file   = trim(scamtec%output_dir)//'/ACOR'//trim(Fname)//'T.scam',   &
            form   = 'formatted', &
            access = 'sequential',  &
            position = 'append'   &
            )
       Open( unit   = FUnitOut+3,      &
            file   = trim(scamtec%output_dir)//'/RMSE'//trim(Fname)//'F.scam',   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )
       Open( unit   = FUnitOut+4,      &
            file   = trim(scamtec%output_dir)//'/VIES'//trim(Fname)//'F.scam',   &
            form   = 'unformatted', &
            access = 'sequential',  &
            position = 'append'   &
           )
       Open( unit   = FUnitOut+5,      &
            file   = trim(scamtec%output_dir)//'/MEAN'//trim(Fname)//'F.scam',   &
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
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !escrevento ctl para campos
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !Obs* podera ser ajustado o nome usando Fname(ocorre erros quando as datas sao iguais)
    !por isso o nome sem a variavel, com datas diferentes nao ocorre erros com o nome.
    
    !RMS    
      open(150, file=trim(scamtec%output_dir)//'/Campo_RMSE.ctl', status='unknown')
      
      write(150,'(A,A)')'dset ^','RMSE'//trim(Fname)//'F.scam'
      write(150,'(A)')
      write(150,'(A)')'options sequential'
      write(150,'(A)')      
      write(150,'(A)')'undef -999.9'      
      write(150,'(A)')      
      write(150,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      write(150,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      
      write(150,'(A)') 
      write(150,'(A)')'zdef    1 linear 0 1'                  
      write(150,'(A)') 
      write(150,'(A,I3,A,I2,A)')'tdef  ',(scamtec%forecast_time/scamtec%atime_step)+1,' linear 00Z05AUG2014 ',scamtec%atime_step,'HR'
      write(150,'(A)')
      write(150,'(A)')'vars 22'
      write(150,'(A)')'VT925 00 99 Virtual Temperature @ 925 hPa [K]'
      write(150,'(A)')'VT850 00 99 Virtual Temperature @ 850 hPa [K]'
      write(150,'(A)')'VT500 00 99 Virtual Temperature @ 500 hPa [K]'                                           
      write(150,'(A)')'TM850 00 99 Absolute Temperature @ 850 hPa [K]'
      write(150,'(A)')'TM500 00 99 Absolute Temperature @ 500 hPa [K]'
      write(150,'(A)')'TM250 00 99 Absolute Temperature @ 250 hPa [K]'
      write(150,'(A)')'PSNM0 00 99 Pressure reduced to snm [hPa]'                                           
      write(150,'(A)')'SH925 00 99 Specific Humidity @ 925 hPa [g/Kg]'
      write(150,'(A)')'SH850 00 99 Specific Humidity @ 850 hPa [g/Kg]'
      write(150,'(A)')'SH500 00 99 Specific Humidity @ 500 hPa [g/Kg]'                                         
      write(150,'(A)')'AG925 00 99 Inst. Precipitable Water @ 925 hPa [Kg/m2]'
      write(150,'(A)')'ZG850 00 99 Geopotential height @ 850 hPa [gpm]'
      write(150,'(A)')'ZG500 00 99 Geopotential height @ 500 hPa [gpm]'
      write(150,'(A)')'ZG250 00 99 Geopotential height @ 250 hPa [gpm]'
      write(150,'(A)')'UV850 00 99 Zonal Wind @ 850 hPa [m/s]'
      write(150,'(A)')'UV500 00 99 Zonal Wind @ 500 hPa [m/s]'
      write(150,'(A)')'UV250 00 99 Zonal Wind @ 250 hPa [m/s]'
      write(150,'(A)')'VV850 00 99 Meridional Wind @ 850 hPa [m/s]'
      write(150,'(A)')'VV500 00 99 Meridional Wind @ 500 hPa [m/s]'
      write(150,'(A)')'VV250 00 99 Meridional Wind @  250 hPa [m/s]'
      write(150,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
      write(150,'(A)')'PV001 00 99 CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]'
      write(150,'(A)')'endvars'   
      
      close(150)
    
      !VIES   
      open(151, file=trim(scamtec%output_dir)//'/Campo_VIES.ctl', status='unknown')
      
      write(151,'(A,A)')'dset ^','VIES'//trim(Fname)//'F.scam'
      write(151,'(A)')
      write(151,'(A)')'options sequential'
      write(151,'(A)')      
      write(151,'(A)')'undef -999.9'      
      write(151,'(A)')      
      write(151,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      write(151,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      
      write(151,'(A)') 
      write(151,'(A)')'zdef    1 linear 0 1'                  
      write(151,'(A)') 
      write(151,'(A,I3,A,I2,A)')'tdef  ',(scamtec%forecast_time/scamtec%atime_step)+1,' linear 00Z05AUG2014 ',scamtec%atime_step,'HR'
      write(151,'(A)')
      write(151,'(A)')'vars 22'
      write(151,'(A)')'VT925 00 99 Virtual Temperature @ 925 hPa [K]'
      write(151,'(A)')'VT850 00 99 Virtual Temperature @ 850 hPa [K]'
      write(151,'(A)')'VT500 00 99 Virtual Temperature @ 500 hPa [K]'                                           
      write(151,'(A)')'TM850 00 99 Absolute Temperature @ 850 hPa [K]'
      write(151,'(A)')'TM500 00 99 Absolute Temperature @ 500 hPa [K]'
      write(151,'(A)')'TM250 00 99 Absolute Temperature @ 250 hPa [K]'
      write(151,'(A)')'PSNM0 00 99 Pressure reduced to snm [hPa]'                                           
      write(151,'(A)')'SH925 00 99 Specific Humidity @ 925 hPa [g/Kg]'
      write(151,'(A)')'SH850 00 99 Specific Humidity @ 850 hPa [g/Kg]'
      write(151,'(A)')'SH500 00 99 Specific Humidity @ 500 hPa [g/Kg]'                                         
      write(151,'(A)')'AG925 00 99 Inst. Precipitable Water @ 925 hPa [Kg/m2]'
      write(151,'(A)')'ZG850 00 99 Geopotential height @ 850 hPa [gpm]'
      write(151,'(A)')'ZG500 00 99 Geopotential height @ 500 hPa [gpm]'
      write(151,'(A)')'ZG250 00 99 Geopotential height @ 250 hPa [gpm]'
      write(151,'(A)')'UV850 00 99 Zonal Wind @ 850 hPa [m/s]'
      write(151,'(A)')'UV500 00 99 Zonal Wind @ 500 hPa [m/s]'
      write(151,'(A)')'UV250 00 99 Zonal Wind @ 250 hPa [m/s]'
      write(151,'(A)')'VV850 00 99 Meridional Wind @ 850 hPa [m/s]'
      write(151,'(A)')'VV500 00 99 Meridional Wind @ 500 hPa [m/s]'
      write(151,'(A)')'VV250 00 99 Meridional Wind @  250 hPa [m/s]'
      write(151,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
      write(151,'(A)')'PV001 00 99 CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]'
      write(151,'(A)')'endvars'   
      
      close(151)
      
      !MEAN   
      open(152, file=trim(scamtec%output_dir)//'/Campo_MEAN.ctl', status='unknown')
      
      write(152,'(A,A)')'dset ^','MEAN'//trim(Fname)//'F.scam'
      write(152,'(A)')
      write(152,'(A)')'options sequential'
      write(152,'(A)')      
      write(152,'(A)')'undef -999.9'      
      write(152,'(A)')      
      write(152,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(10)
      write(152,'(A,1x,I4.3,1x,A,F9.3,F9.3)')'ydef',scamtec%nypt,'linear', scamtec%gridDesc(4), scamtec%gridDesc(9)      
      write(152,'(A)') 
      write(152,'(A)')'zdef    1 linear 0 1'                  
      write(152,'(A)') 
      write(152,'(A,I3,A,I2,A)')'tdef  ',(scamtec%forecast_time/scamtec%atime_step)+1,' linear 00Z05AUG2014 ',scamtec%atime_step,'HR'
      write(152,'(A)')
      write(152,'(A)')'vars 22'
      write(152,'(A)')'VT925 00 99 Virtual Temperature @ 925 hPa [K]'
      write(152,'(A)')'VT850 00 99 Virtual Temperature @ 850 hPa [K]'
      write(152,'(A)')'VT500 00 99 Virtual Temperature @ 500 hPa [K]'                                           
      write(152,'(A)')'TM850 00 99 Absolute Temperature @ 850 hPa [K]'
      write(152,'(A)')'TM500 00 99 Absolute Temperature @ 500 hPa [K]'
      write(152,'(A)')'TM250 00 99 Absolute Temperature @ 250 hPa [K]'
      write(152,'(A)')'PSNM0 00 99 Pressure reduced to snm [hPa]'                                           
      write(152,'(A)')'SH925 00 99 Specific Humidity @ 925 hPa [g/Kg]'
      write(152,'(A)')'SH850 00 99 Specific Humidity @ 850 hPa [g/Kg]'
      write(152,'(A)')'SH500 00 99 Specific Humidity @ 500 hPa [g/Kg]'                                         
      write(152,'(A)')'AG925 00 99 Inst. Precipitable Water @ 925 hPa [Kg/m2]'
      write(152,'(A)')'ZG850 00 99 Geopotential height @ 850 hPa [gpm]'
      write(152,'(A)')'ZG500 00 99 Geopotential height @ 500 hPa [gpm]'
      write(152,'(A)')'ZG250 00 99 Geopotential height @ 250 hPa [gpm]'
      write(152,'(A)')'UV850 00 99 Zonal Wind @ 850 hPa [m/s]'
      write(152,'(A)')'UV500 00 99 Zonal Wind @ 500 hPa [m/s]'
      write(152,'(A)')'UV250 00 99 Zonal Wind @ 250 hPa [m/s]'
      write(152,'(A)')'VV850 00 99 Meridional Wind @ 850 hPa [m/s]'
      write(152,'(A)')'VV500 00 99 Meridional Wind @ 500 hPa [m/s]'
      write(152,'(A)')'VV250 00 99 Meridional Wind @  250 hPa [m/s]'
      write(152,'(A)')'PC000 00 99 TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]'
      write(152,'(A)')'PV001 00 99 CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]'
      write(152,'(A)')'endvars'   
      
      close(152)
    
    
    

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
    Real    :: Sab, Saa, Sbb
    Real    :: Ma, Mb
    Integer :: NPts
    Integer :: I
    !

    NPts  = size(A,1)

    Ma  = sum(A)/float(NPts)
    Mb  = sum(B)/float(NPts)

!Estatistica de correlacao de anomalia alterada pelo Joao
     !Ma  = 0.0
     !Mb  = 0.0
    
    Sab = sum( (A-Ma)*(B-Mb) )
    Saa = sum( (A-Ma)*(A-Ma) )
    Sbb = sum( (B-Mb)*(B-Mb) )
    Rho =  Sab/sqrt(Saa*Sbb)


    Rho =  Sab/sqrt(Saa*Sbb)


    return

  End Subroutine corr


End Module SCAM_bstatistic
