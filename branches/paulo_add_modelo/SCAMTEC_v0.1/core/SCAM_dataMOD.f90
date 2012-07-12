MODULE SCAM_dataMOD
  USE scamtec_module
  USE SCAM_Utils, only: dom, nvmx, Clima_Flag, Precipitation_Flag,hist !Paulo Dias

  !BOP
  !
  ! !MODULE: SCAM_dataMod
  ! 
  ! !DESCRIPTION: 
  !
  !  \subsubsection{Overview}
  !
  !
  !  \begin{description}
  !
  !
  !
  ! !REVISION HISTORY: 
  !  09 Oct 2011    Joao Gerd  
  !                 Initial Specification
  ! 
  ! !USES:

  !
  ! Variables to be Evaluated
  !

  integer :: tam_hist !Paulo dias
  integer, public, Parameter :: NumVarAval = 17
  character(len=8), public, parameter ::   VarName(1:NumVarAval) = (/ &
  'VTMP:925',& ! Virtual Temperature @ 925 hPa [K]                     1
  'VTMP:850',& ! Virtual Temperature @ 850 hPa [K]                     2
  'VTMP:500',& ! Virtual Temperature @ 500 hPa [K]                     3
  'PSNM:000',& ! Pressure reduced to MSL [hPa]                         4
  'UMES:925',& ! Specific Humidity @ 925 hPa [Kg/Kg]                   5
  'AGPL:925',& ! Inst. Precipitable Water @ 925 hPa [Kg/m2]            6
  'ZGEO:850',& ! Geopotential height @ 850 hPa [gpm]                   7
  'ZGEO:500',& ! Geopotential height @ 500 hPa [gpm]                   8
  'ZGEO:250',& ! Geopotential height @ 250 hPa [gpm]                   9
  'UVEL:850',& ! Zonal Wind @ 850 hPa [m/s]                           10
  'UVEL:500',& ! Zonal Wind @ 500 hPa [m/s]                           11
  'UVEL:250',& ! Zonal Wind @ 250 hPa [m/s]                           12
  'VVEL:850',& ! Meridional Wind @ 850 hPa [m/s]                      13
  'VVEL:500',& ! Meridional Wind @ 500 hPa [m/s]                      14
  'VVEL:250',& ! Meridional Wind @ 250 hPa [m/s]                      15
  'PREC:000',& ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]           16
  'PREV:000' & ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]      17
  /)


  !-------------------------------------------------------------------!
  ! !PUBLIC TYPES
  !-------------------------------------------------------------------!

  public :: scamdata

  !EOP

  type model_dec_type
     real, allocatable    :: tmpfield(:,:,:) ! data from model read
     real, allocatable    :: expfield(:,:,:) ! experiment model field
     real, allocatable    :: reffield(:,:,:) ! reference model field
     real, allocatable    :: clmfield(:,:,:) ! climatology field
     real, allocatable    :: prefield(:,:,:) ! preciptation field
     real, allocatable    :: diffield(:,:,:) ! diference field
     real, allocatable    :: rmsfield(:,:,:)
     real, allocatable    :: time_rmse(:,:)
     real, allocatable    :: time_vies(:,:)
     real, allocatable    :: time_acor(:,:)
     integer, allocatable :: time_histo(:,:) ! histogroma paulo dias
     
  end type model_dec_type


  TYPE obs_dec_type
     real, allocatable :: tmpfield(:) ! data from model read
     real, allocatable :: expfield(:) ! experiment model field
     real, allocatable :: reffield(:) ! reference model field
     real, allocatable :: clmfield(:) ! climatology field
     real, allocatable :: prefield(:) ! preciptation field paulo dias
     real, allocatable :: diffield(:) ! diference field
  END TYPE obs_dec_type

  type(model_dec_type), allocatable :: scamdata(:)


CONTAINS

  SUBROUTINE data_config()
    IMPLICIT NONE
    integer :: I

    scamtec%gridDesc = 0

    scamtec%gridDesc( 1) = 0
    scamtec%gridDesc( 2) = dom(1)%nx
    scamtec%gridDesc( 3) = dom(1)%ny
    scamtec%gridDesc( 4) = dom(1)%ur_lat
    scamtec%gridDesc( 5) = dom(1)%ll_lon
    scamtec%gridDesc( 6) = 128
    scamtec%gridDesc( 7) = dom(1)%ll_lat
    scamtec%gridDesc( 8) = dom(1)%ur_lon
    scamtec%gridDesc( 9) = dom(1)%dx
    scamtec%gridDesc(10) = dom(1)%dy
    scamtec%gridDesc(20) = 0

    scamtec%nxpt = dom(1)%nx
    scamtec%nypt = dom(1)%ny
    scamtec%npts = dom(1)%nx*dom(1)%ny

    scamtec%udef = -999.9

    scamtec%nvar = NumVarAval

#ifdef DEBUG  
    write(6, FMT=123)'::: xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(9)
    write(6, FMT=123)'::: ydef',scamtec%nypt,'linear', scamtec%gridDesc(7), scamtec%gridDesc(10)
    print*, '::: npts',scamtec%npts
123 FORMAT(A,1x,I4.3,1x,A,F9.3,F9.3)    
#endif


  END SUBROUTINE data_config



  SUBROUTINE allocate_data_mem()
    IMPLICIT NONE
    integer :: I
    
    
     if(Precipitation_Flag.eq.1)then  
      tam_hist=(hist%valor_limit/hist%rang)+2  	
      endif

    allocate(scamdata(scamtec%nexp))

    allocate(scamdata(1)%reffield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
    allocate(scamdata(1)%tmpfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
    IF(clima_Flag.EQ.1)allocate(scamdata(1)%clmfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
    

    DO I=1,scamtec%nexp
       allocate(scamdata(I)%expfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
       allocate(scamdata(I)%diffield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
       allocate(scamdata(I)%rmsfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
       IF(Precipitation_Flag.EQ.1)allocate(scamdata(I)%prefield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))!paulo dias
       allocate(scamdata(I)%time_rmse(scamtec%ntime_forecast,scamtec%nvar))
       allocate(scamdata(I)%time_vies(scamtec%ntime_forecast,scamtec%nvar))
       allocate(scamdata(I)%time_acor(scamtec%ntime_forecast,scamtec%nvar))
       IF(Precipitation_Flag.EQ.1)allocate(scamdata(I)%time_histo(tam_hist,scamtec%ntime_forecast))!paulo dias
    ENDDO 

  END SUBROUTINE allocate_data_mem

  SUBROUTINE data_init()
    IMPLICIT NONE
    integer :: I

    DO I=1,scamtec%nexp

      scamdata(I)%diffield  = 0.0
      scamdata(I)%rmsfield  = 0.0
      scamdata(I)%time_rmse = 0.0
      scamdata(I)%time_vies = 0.0
      scamdata(I)%time_acor = 0.0

    ENDDO

  END SUBROUTINE data_init

  SUBROUTINE release_data_mem()   
    IMPLICIT NONE
    integer :: I

    IF (Allocated(scamdata(1)%reffield))DeAllocate(scamdata(1)%reffield)
    IF (Allocated(scamdata(1)%clmfield))DeAllocate(scamdata(1)%clmfield)
    IF (Allocated(scamdata(1)%tmpfield))DeAllocate(scamdata(1)%tmpfield)

    DO I=1,scamtec%nexp
       IF (Allocated(scamdata(I)%prefield))DeAllocate(scamdata(I)%prefield)
       IF (Allocated(scamdata(I)%expfield))DeAllocate(scamdata(I)%expfield)
       IF (Allocated(scamdata(I)%diffield))DeAllocate(scamdata(I)%diffield)
       IF (Allocated(scamdata(I)%rmsfield))DeAllocate(scamdata(I)%rmsfield)
    ENDDO

  END SUBROUTINE release_data_mem
	

  SUBROUTINE ldata( type, e, Id, name )
		
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: type
    INTEGER,          INTENT(IN) :: Id !numero do modelo
    INTEGER,          INTENT(IN) :: e  !numero do experimento
    CHARACTER(LEN=*), INTENT(IN) :: name
	

    call load_data(Id, name//char(0))

#ifdef DEBUG  
     write(6,'(A,1x,A,1x,2F15.3)')                              &
                                 trim(type),': [MIN/MAX]::',   &
                                 minval(scamdata(1)%tmpfield), &
                                 maxval(scamdata(1)%tmpfield)
#endif
	
    SELECT CASE(trim(type))
    CASE('R')
       scamdata(e)%reffield = scamdata(1)%tmpfield
    CASE('E')		
       scamdata(e)%expfield = scamdata(1)%tmpfield
    CASE('C')		
       scamdata(e)%clmfield = scamdata(1)%tmpfield
    CASE('P')		
       scamdata(e)%prefield = scamdata(1)%tmpfield !paulo dias
    END SELECT


  END SUBROUTINE ldata


END MODULE SCAM_dataMOD
