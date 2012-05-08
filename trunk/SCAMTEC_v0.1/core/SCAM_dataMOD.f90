MODULE SCAM_dataMOD
  USE scamtec_module
  USE SCAM_Utils, only: dom, nvmx, Clima_Flag

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

  integer, public, Parameter :: NumVarAval = 15
  character(len=8), public, parameter ::   VarName(1:NumVarAval) = (/ &
  'VTMP:925',& ! Virtual Temperature @ 925 hPa [K]
  'VTMP:850',& ! Virtual Temperature @ 850 hPa [K]
  'VTMP:500',& ! Virtual Temperature @ 500 hPa [K]
  'PSNM:000',& ! Pressure reduced to MSL [hPa]
  'UMES:925',& ! Specific Humidity @ 925 hPa [Kg/Kg]
  'AGPL:925',& ! Inst. Precipitable Water @ 925 hPa [Kg/m2]
  'ZGEO:850',& ! Geopotential height @ 850 hPa [gpm]
  'ZGEO:500',& ! Geopotential height @ 500 hPa [gpm]
  'ZGEO:250',& ! Geopotential height @ 250 hPa [gpm]
  'UVEL:850',& ! Zonal Wind @ 850 hPa [m/s]
  'UVEL:500',& ! Zonal Wind @ 500 hPa [m/s]
  'UVEL:250',& ! Zonal Wind @ 250 hPa [m/s]
  'VVEL:850',& ! Meridional Wind @ 850 hPa [m/s]
  'VVEL:500',& ! Meridional Wind @ 500 hPa [m/s]
  'VVEL:250' & ! Meridional Wind @  250 hPa [m/s]
  /)


  !-------------------------------------------------------------------!
  ! !PUBLIC TYPES
  !-------------------------------------------------------------------!

  public :: scamdata

  !EOP

  type model_dec_type
     real, allocatable :: tmpfield(:,:,:) ! data from model read
     real, allocatable :: expfield(:,:,:) ! experiment model field
     real, allocatable :: reffield(:,:,:) ! reference model field
     real, allocatable :: clmfield(:,:,:) ! climatology field
     real, allocatable :: diffield(:,:,:) ! diference field
     real, allocatable :: rmsfield(:,:,:)
     real, allocatable :: time_rmse(:,:)
     real, allocatable :: time_vies(:,:)
     real, allocatable :: time_acor(:,:)
  end type model_dec_type


  TYPE obs_dec_type
     real, allocatable :: tmpfield(:) ! data from model read
     real, allocatable :: expfield(:) ! experiment model field
     real, allocatable :: reffield(:) ! reference model field
     real, allocatable :: clmfield(:) ! climatology field
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
    write(6, FMT=123)'xdef',scamtec%nxpt,'linear', scamtec%gridDesc(5), scamtec%gridDesc(9)
    write(6, FMT=123)'ydef',scamtec%nypt,'linear', scamtec%gridDesc(7), scamtec%gridDesc(10)
123 FORMAT(A,1x,I4.3,1x,A,F9.3,F9.3)    
#endif


  END SUBROUTINE data_config



  SUBROUTINE allocate_data_mem()
    IMPLICIT NONE
    integer :: I

    allocate(scamdata(scamtec%nexp))

    allocate(scamdata(1)%reffield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
    allocate(scamdata(1)%tmpfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
    IF(clima_Flag.EQ.1)allocate(scamdata(1)%clmfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))

    DO I=1,scamtec%nexp
       allocate(scamdata(I)%expfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
       allocate(scamdata(I)%diffield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
       allocate(scamdata(I)%rmsfield(scamtec%nxpt,scamtec%nypt,scamtec%nvar))
       allocate(scamdata(I)%time_rmse(scamtec%ntime_forecast,scamtec%nvar))
       allocate(scamdata(I)%time_vies(scamtec%ntime_forecast,scamtec%nvar))
       allocate(scamdata(I)%time_acor(scamtec%ntime_forecast,scamtec%nvar))
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

  END SUBROUTINE

  SUBROUTINE release_data_mem()
    IMPLICIT NONE
    integer :: I

    IF (Allocated(scamdata(1)%reffield))DeAllocate(scamdata(1)%reffield)
    IF (Allocated(scamdata(1)%clmfield))DeAllocate(scamdata(1)%clmfield)
    IF (Allocated(scamdata(1)%tmpfield))DeAllocate(scamdata(1)%tmpfield)

    DO I=1,scamtec%nexp
       IF (Allocated(scamdata(I)%expfield))DeAllocate(scamdata(I)%expfield)
       IF (Allocated(scamdata(I)%diffield))DeAllocate(scamdata(I)%diffield)
       IF (Allocated(scamdata(I)%rmsfield))DeAllocate(scamdata(I)%rmsfield)
    ENDDO

  END SUBROUTINE release_data_mem


  SUBROUTINE ldata( type, e, Id, name )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: type
    INTEGER,          INTENT(IN) :: Id
    INTEGER,          INTENT(IN) :: e
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
    END SELECT


  END SUBROUTINE ldata


END MODULE SCAM_dataMOD
