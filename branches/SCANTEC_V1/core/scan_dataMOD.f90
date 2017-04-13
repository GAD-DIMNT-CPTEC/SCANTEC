!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: scan_dataMOD.f90
!
! !DESCRIPTON:
!             
!                 
!\\
!\\
! !INTERFACE:
!

MODULE scan_dataMOD
!
! !USES:
!

  USE scantec_module    ! scantec types
  USE m_string          ! String Manipulation
  USE scan_Utils, only: dom, nvmx, Refer, Clima, Exper, Precip
  
  IMPLICIT NONE
  PRIVATE

!
! !PARAMETERS:
!

  integer, public, Parameter :: NumVarAval = 20
  character(len=8), public, parameter ::   VarName(1:NumVarAval) = (/ &
                                           'VTMP-925',& ! Virtual Temperature @ 925 hPa [K]
                                           'VTMP-850',& ! Virtual Temperature @ 850 hPa [K]
                                           'VTMP-500',& ! Virtual Temperature @ 500 hPa [K]                                           
                                           'TEMP-850',& ! Absolute Temperature @ 850 hPa [K]
                                           'TEMP-500',& ! Absolute Temperature @ 500 hPa [K]
                                           'TEMP-250',& ! Absolute Temperature @ 250 hPa [K]
                                           'PSNM-000',& ! Pressure reduced to MSL [hPa]                                           
                                           'UMES-925',& ! Specific Humidity @ 925 hPa [g/Kg]
                                           'UMES-850',& ! Specific Humidity @ 850 hPa [g/Kg]
                                           'UMES-500',& ! Specific Humidity @ 500 hPa [g/Kg]
                                           'AGPL-925',& ! Inst. Precipitable Water @ 925 hPa [Kg/m2]
                                           'ZGEO-850',& ! Geopotential height @ 850 hPa [gpm]
                                           'ZGEO-500',& ! Geopotential height @ 500 hPa [gpm]
                                           'ZGEO-250',& ! Geopotential height @ 250 hPa [gpm]
                                           'UVEL-850',& ! Zonal Wind @ 850 hPa [m/s]
                                           'UVEL-500',& ! Zonal Wind @ 500 hPa [m/s]
                                           'UVEL-250',& ! Zonal Wind @ 250 hPa [m/s]
                                           'VVEL-850',& ! Meridional Wind @ 850 hPa [m/s]
                                           'VVEL-500',& ! Meridional Wind @ 500 hPa [m/s]
                                           'VVEL-250' & ! Meridional Wind @  250 hPa [m/s]
                                          /)

!
! !PUBLIC TYPES
!

  type model_dec_type
     real, allocatable :: tmpfield(:,:) ! data from model read
     real, allocatable :: expfield(:,:) ! experiment model field
     real, allocatable :: reffield(:,:) ! reference model field
     real, allocatable :: clmfield(:,:) ! climatology field
     

     logical, allocatable :: UdfIdx (:,:)
  end type model_dec_type

  TYPE obs_dec_type
     real, allocatable :: tmpfield(:) ! data from model read
     real, allocatable :: expfield(:) ! experiment model field
     real, allocatable :: reffield(:) ! reference model field
     real, allocatable :: clmfield(:) ! climatology field
     real, allocatable :: diffield(:) ! diference field
  END TYPE obs_dec_type

  public :: model_dec_type
  public :: obs_dec_type

!
! !PUBLIC DATA MEMBERS:
!

  type(model_dec_type), public, Target, allocatable :: scandata(:)

!
! !PUBLIC MEMBER FUNCTIONS:
!

  public :: data_config
  public :: allocate_data_mem
  public :: data_init
  public :: ldata
  public :: scan_ModelData

!
! !REVISION HISTORY:
!  09 OCT 2011 - J. G. de Mattos - Initial Version
!     SEP 2012 - J. G. de Mattos - Include routine to read files
!  11 Oct 2012 - J. G. de Mattos - Remove bug at data_config routine.
!                                  Lat was being read inverted
!                                  dx and dy was inverted
!
! !SEE ALSO:
!   
!
!EOP
!-----------------------------------------------------------------------------!
!

CONTAINS

  SUBROUTINE data_config()
    IMPLICIT NONE
    integer :: I

    scantec%gridDesc = 0

    scantec%gridDesc( 1) = 0
    scantec%gridDesc( 2) = dom(1)%nx        ! Number of x points
    scantec%gridDesc( 3) = dom(1)%ny        ! number of y points
    scantec%gridDesc( 4) = dom(1)%ll_lat    ! First latitude point (South point)
    scantec%gridDesc( 5) = dom(1)%ll_lon    ! First longitude point (West point)
    scantec%gridDesc( 6) = 128
    scantec%gridDesc( 7) = dom(1)%ur_lat    ! Last latitude point (North Point)
    scantec%gridDesc( 8) = dom(1)%ur_lon    ! Last longitude Point (East point)
    scantec%gridDesc( 9) = dom(1)%dy        ! Delta y point
    scantec%gridDesc(10) = dom(1)%dx        ! Delta x point
    scantec%gridDesc(20) = 0

    scantec%nxpt = dom(1)%nx
    scantec%nypt = dom(1)%ny
    scantec%npts = dom(1)%nx*dom(1)%ny

    scantec%udef = -999.9

    scantec%nvar = NumVarAval
    
    Allocate(scantec%VarName(NumVarAval))
    scantec%VarName = VarName

#ifdef DEBUG  
    write(6, FMT=123)'xdef',scantec%nxpt,'linear', scantec%gridDesc(5), scantec%gridDesc(10)
    write(6, FMT=123)'ydef',scantec%nypt,'linear', scantec%gridDesc(4), scantec%gridDesc(9)
123 FORMAT(A,1x,I4.3,1x,A,F9.3,F9.3)    
#endif


  END SUBROUTINE data_config



  SUBROUTINE allocate_data_mem()
    IMPLICIT NONE
    integer :: I

    allocate(scandata(scantec%nexp))

    allocate(scandata(1)%reffield(scantec%nxpt*scantec%nypt,scantec%nvar))
    allocate(scandata(1)%tmpfield(scantec%nxpt*scantec%nypt,scantec%nvar))
    IF(scantec%cflag.EQ.1)allocate(scandata(1)%clmfield(scantec%nxpt*scantec%nypt,scantec%nvar))
    
    
    DO I=1,scantec%nexp
       allocate(scandata(I)%expfield(scantec%nxpt*scantec%nypt,scantec%nvar))
       allocate(scandata(I)%UdfIdx(scantec%nxpt*scantec%nypt,scantec%nvar))
    ENDDO


  END SUBROUTINE allocate_data_mem

  SUBROUTINE data_init()
    IMPLICIT NONE
    integer :: I

    DO I=1,scantec%nexp

      scandata(I)%UdfIdx    = .true.

    ENDDO

  END SUBROUTINE

  SUBROUTINE release_data_mem()
    IMPLICIT NONE
    integer :: I

    IF (Allocated(scandata(1)%reffield))DeAllocate(scandata(1)%reffield)
    IF (Allocated(scandata(1)%clmfield))DeAllocate(scandata(1)%clmfield)
    IF (Allocated(scandata(1)%tmpfield))DeAllocate(scandata(1)%tmpfield)
     
    DO I=1,scantec%nexp
       IF (Allocated(scandata(I)%expfield))DeAllocate(scandata(I)%expfield)
       IF (Allocated(scandata(I)%UdfIdx))Deallocate(scandata(I)%UdfIdx)
    ENDDO

  END SUBROUTINE release_data_mem


  SUBROUTINE ldata( type, e, Id, name )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: type
    INTEGER,          INTENT(IN) :: Id
    INTEGER,          INTENT(IN) :: e
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER   :: stat
    INTEGER   :: I


    call load_data(Id, name//char(0))

#ifdef DEBUG  
    write(6,'(A,1x,A,1x,2F15.3)')                              &
                                 trim(type),': [MIN/MAX]::',   &
                                 minval(scandata(1)%tmpfield,mask=scandata(1)%tmpfield .ne. scantec%udef), &
                                 maxval(scandata(1)%tmpfield,mask=scandata(1)%tmpfield .ne. scantec%udef)
#endif

    !
    ! Definindo pontos onde nao calcular indices estatisticos
    !

    DO I=1,scantec%nvar
       where (scandata(1)%tmpfield(:,I) .eq. scantec%udef) scandata(e)%UdfIdx(:,I) = .false.
    ENDDO

    

    !
    ! Selecionando qual eh o campo que esta sendo lido
    !

    SELECT CASE(trim(type))
    CASE('R')
       scandata(e)%reffield = scandata(1)%tmpfield
    CASE('E')
       scandata(e)%expfield = scandata(1)%tmpfield
    CASE('C')
       scandata(e)%clmfield = scandata(1)%tmpfield
    END SELECT


  END SUBROUTINE ldata

  SUBROUTINE scan_ModelData( NExp )
     IMPLICIT NONE
     integer, intent(in) :: NExp
     integer             :: aymd, ahms
     integer             :: fymd, fhms
     character(len=1024) :: Reference    ! Reference File Name
     character(len=1024) :: Experiment   ! Experiment File Name
     character(len=1024) :: Climatology  ! Climatology File Name
     

     aymd = scantec%atime/100
     ahms = MOD(scantec%atime,100) * 10000
     fymd = scantec%ftime/100
     fhms = MOD(scantec%ftime,100) * 10000

     !
     ! 1. Create file name and Open 
     !

     !
     ! 1.1 Reference data file 
     !
	
     Reference=TRIM(Refer%file)
     CALL str_template(Reference, fymd,fhms)
     CALL ldata('R', 1, Refer%Id, Reference)

     !
     ! 1.2 Climatology data file
     !

     if (scantec%atime_flag)then
      
        IF(scantec%cflag.EQ.1)THEN
           Climatology=TRIM(Clima%file)
           CALL str_template(Climatology, fymd,fhms)
           CALL ldata('C', 1, Clima%Id, Climatology)
        END IF

        !
        ! 1.3 definindo flag = .false. para inibir a reabertura do arquivo de
        ! referencia e de climatologia para o mesmo tempo
        !

        scantec%atime_flag = .false.

     endif

     !
     ! 1.3 Experiment Data File
     !
     
     !Joao adicionou para verificar quando nao tem o link das 0h
     Experiment = TRIM(Exper(NExp)%file)
     
     if (scantec%atime.eq.scantec%ftime)then
        CALL replace(Experiment, 'fct','icn')
     end if
     
     CALL str_template(Experiment, aymd, ahms, fymd, fhms)
     CALL ldata('E',NExp,Exper(NExp)%Id, Experiment)

  END SUBROUTINE


END MODULE scan_dataMOD
