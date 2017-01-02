!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_dataMOD.f90
!
! !DESCRIPTON:
!             
!                 
!\\
!\\
! !INTERFACE:
!

MODULE SCAM_DataMod
!
! !USES:
!

  USE SCAM_GlobalTypes  ! SCAMTEC types
  USE SCAM_StringMod    ! String Manipulation
  USE SCAM_UtilsMod, only: dom, nvmx, Refer, Clima, Exper
  USE SCAM_UtilsMod, only: Precipitation_Flag,hist,Precip !Paulo Dias
  
  IMPLICIT NONE
  PRIVATE

!
! !PARAMETERS:
!

  integer, public, Parameter :: NumVarAval = 22
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
                                           'VVEL-250',& ! Meridional Wind @  250 hPa [m/s]
                                           'PREC-000',& ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]
                                           'PREV-000' & ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]
                                          /)

!
! !PUBLIC TYPES
!

  type model_dec_type
     real, allocatable :: tmpfield(:,:) ! data from model read
     real, allocatable :: expfield(:,:) ! experiment model field
     real, allocatable :: reffield(:,:) ! reference model field
     real, allocatable :: clmfield(:,:) ! climatology field
     real, allocatable :: prefield(:,:) ! preciptation field (paulo dias)

     logical, allocatable :: UdfIdx (:,:) ! Undefined points Mask

  end type model_dec_type

  TYPE obs_dec_type
     real, allocatable :: tmpfield(:) ! data from model read
     real, allocatable :: expfield(:) ! experiment model field
     real, allocatable :: reffield(:) ! reference model field
     real, allocatable :: clmfield(:) ! climatology field
     real, allocatable :: prefield(:) ! preciptation field (paulo dias)
     real, allocatable :: diffield(:) ! diference field
  END TYPE obs_dec_type

  public :: model_dec_type
  public :: obs_dec_type

!
! !PUBLIC DATA MEMBERS:
!

  type(model_dec_type), public, Target, allocatable :: ScamData(:)

!
! !PUBLIC MEMBER FUNCTIONS:
!

  public :: SCAM_GetModelData  ! Read Model fields
  public :: SCAM_GridConf      ! Configure matrices to read model
  public :: SCAM_DataInit      ! Initialize matrices to read model


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

  SUBROUTINE SCAM_GridConf()
    IMPLICIT NONE

    SCAM%gridDesc = 0

    SCAM%gridDesc( 1) = 0
    SCAM%gridDesc( 2) = dom(1)%nx        ! Number of x points
    SCAM%gridDesc( 3) = dom(1)%ny        ! number of y points
    SCAM%gridDesc( 4) = dom(1)%ll_lat    ! First latitude point (South point)
    SCAM%gridDesc( 5) = dom(1)%ll_lon    ! First longitude point (West point)
    SCAM%gridDesc( 6) = 128
    SCAM%gridDesc( 7) = dom(1)%ur_lat    ! Last latitude point (North Point)
    SCAM%gridDesc( 8) = dom(1)%ur_lon    ! Last longitude Point (East point)
    SCAM%gridDesc( 9) = dom(1)%dy        ! Delta y point
    SCAM%gridDesc(10) = dom(1)%dx        ! Delta x point
    SCAM%gridDesc(20) = 0

    SCAM%nxpt = dom(1)%nx
    SCAM%nypt = dom(1)%ny
    SCAM%npts = dom(1)%nx*dom(1)%ny

!    SCAM%udef = -999.9

    SCAM%nvar = NumVarAval
    
    Allocate(SCAM%VarName(NumVarAval))
    SCAM%VarName = VarName

#ifdef DEBUG  
    write(6, FMT=123)'xdef',SCAM%nxpt,'linear', SCAM%gridDesc(5), SCAM%gridDesc(10)
    write(6, FMT=123)'ydef',SCAM%nypt,'linear', SCAM%gridDesc(4), SCAM%gridDesc(9)
123 FORMAT(A,1x,I4.3,1x,A,F9.3,F9.3)    
#endif


  END SUBROUTINE SCAM_GridConf



  SUBROUTINE SCAM_AllocDataMem()
    IMPLICIT NONE
    integer :: I

    allocate(ScamData(SCAM%nexp))

    allocate(ScamData(1)%reffield(SCAM%nxpt*SCAM%nypt,SCAM%nvar))
    allocate(ScamData(1)%tmpfield(SCAM%nxpt*SCAM%nypt,SCAM%nvar))

    IF(SCAM%cflag.EQ.1)allocate(ScamData(1)%clmfield(SCAM%nxpt*SCAM%nypt,SCAM%nvar))
    IF(Precipitation_Flag.EQ.1)allocate(ScamData(1)%prefield(SCAM%nxpt*SCAM%nypt,SCAM%nvar))!paulo dias
    
    DO I=1,SCAM%nexp
       allocate(ScamData(I)%expfield(SCAM%nxpt*SCAM%nypt,SCAM%nvar))
       allocate(ScamData(I)%UdfIdx(SCAM%nxpt*SCAM%nypt,SCAM%nvar))
    ENDDO


  END SUBROUTINE SCAM_AllocDataMem

  SUBROUTINE SCAM_DataInit( )
    IMPLICIT NONE
    integer :: I

    DO I=1,SCAM%nexp

      ScamData(I)%UdfIdx    = .true.

    ENDDO

  END SUBROUTINE SCAM_DataInit

  SUBROUTINE SCAM_ReleaseDataMem( )
    IMPLICIT NONE
    integer :: I

    IF (Allocated(ScamData(1)%reffield))DeAllocate(ScamData(1)%reffield)
    IF (Allocated(ScamData(1)%clmfield))DeAllocate(ScamData(1)%clmfield)
    IF (Allocated(ScamData(1)%tmpfield))DeAllocate(ScamData(1)%tmpfield)
    IF (Allocated(ScamData(1)%prefield))DeAllocate(ScamData(1)%prefield)!paulo dias
     
    DO I=1,SCAM%nexp

       IF (Allocated(ScamData(I)%expfield))DeAllocate(ScamData(I)%expfield)
       IF (Allocated(ScamData(I)%UdfIdx))Deallocate(ScamData(I)%UdfIdx)

    ENDDO

  END SUBROUTINE SCAM_ReleaseDataMem



  SUBROUTINE SCAM_ReadModel( NExp )
     IMPLICIT NONE
     integer, intent(in) :: NExp
     integer             :: aymd, ahms
     integer             :: fymd, fhms
     character(len=1024) :: Reference    ! Reference File Name
     character(len=1024) :: Experiment   ! Experiment File Name
     character(len=1024) :: Climatology  ! Climatology File Name
     character(len=1024) :: Precipitation  ! Precipitation File Name (Paulo dias)
     

     aymd = SCAM%atime/100
     ahms = MOD(SCAM%atime,100) * 10000
     fymd = SCAM%ftime/100
     fhms = MOD(SCAM%ftime,100) * 10000

     !
     ! 1. Create file name and Open 
     !

!     if (SCAM%atime_flag)then

        !
        ! 1.1 Reference data file 
        !
	
        Reference=TRIM(Refer%file)
        CALL str_template(Reference, fymd,fhms)
        CALL ldata('R', 1, Refer%Id, Reference)

        !
        ! 1.2 Climatology data file
        !

     if (SCAM%atime_flag)then
      
        IF(SCAM%cflag.EQ.1)THEN
           Climatology=TRIM(Clima%file)
           CALL str_template(Climatology, fymd,fhms)
           CALL ldata('C', 1, Clima%Id, Climatology)
        END IF

        !
        ! 1.3 definindo flag = .false. para inibir a reabertura do arquivo de
        ! referencia e de climatologia para o mesmo tempo
        !

        SCAM%atime_flag = .false.

     endif

     !
     ! 1.3 Experiment Data File
     !
     
     !Joao adicionou para verificar quando nao tem o link das 0h
     Experiment = TRIM(Exper(NExp)%file)
     
!     if (Exper(NExp)%id.eq.1.and.(SCAM%atime.eq.SCAM%ftime))then
     if (SCAM%atime.eq.SCAM%ftime)then
        CALL replace(Experiment, 'fct','icn')
     end if
     
     CALL str_template(Experiment, aymd, ahms, fymd, fhms)
     CALL ldata('E',NExp,Exper(NExp)%Id, Experiment)

     !
     ! 1.4 Precipitation data file 
     !
     IF(Precipitation_Flag .EQ. 1)THEN
     
        Precipitation=TRIM(Precip%file)
        CALL str_template(Precipitation, fymd,fhms)
        CALL ldata('P', 1, Precip%Id, Precipitation)
     END IF


  END SUBROUTINE SCAM_ReadModel


  SUBROUTINE SCAM_GetModelData( type, e, Id, name )
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: type
    CHARACTER(LEN=*), INTENT(IN) :: Id
    INTEGER,          INTENT(IN) :: e
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER   :: stat
    INTEGER   :: I


    call SCAM_ModelRead(Id//char(0), name//char(0))

#ifdef DEBUG  
    write(6,'(A,1x,A,1x,2F15.3)')                              &
                                 trim(type),': [MIN/MAX]::',   &
                                 minval(ScamData(1)%tmpfield,mask=ScamData(1)%tmpfield .ne. SCAM%udef), &
                                 maxval(ScamData(1)%tmpfield,mask=ScamData(1)%tmpfield .ne. SCAM%udef)
#endif

    !
    ! Definindo pontos onde nao calcular indices estatisticos
    !

    DO I=1,SCAM%nvar
       where (ScamData(1)%tmpfield(:,I) .eq. SCAM%udef) ScamData(e)%UdfIdx(:,I) = .false.
    ENDDO

    

    !
    ! Selecionando qual eh o campo que esta sendo lido
    !

    SELECT CASE(trim(type))
    CASE('R')
       ScamData(e)%reffield = ScamData(1)%tmpfield
    CASE('E')
       ScamData(e)%expfield = ScamData(1)%tmpfield
    CASE('C')
       ScamData(e)%clmfield = ScamData(1)%tmpfield
    CASE('P')       
       ScamData(e)%prefield = ScamData(1)%tmpfield !paulo dias
    END SELECT


  END SUBROUTINE SCAM_GetModelData



END MODULE SCAM_DataMod
