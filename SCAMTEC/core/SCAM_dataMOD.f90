MODULE SCAM_dataMOD
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

  !-------------------------------------------------------------------!
  ! !PUBLIC TYPES
  !-------------------------------------------------------------------!
  
  TYPE grd
     REAL, DIMENSION(:), ALLOCATABLE :: lat  ! Latitude (South to North | -90 to 90)
     REAL, DIMENSION(:), ALLOCATABLE :: lon  ! Longitude ( 0 to 360)
     REAL, DIMENSION(:), ALLOCATABLE :: UVEL ! Zonal wind component (m/s)
     REAL, DIMENSION(:), ALLOCATABLE :: VVEL ! Meridional wind component (m/s)
     REAL, DIMENSION(:), ALLOCATABLE :: ZGEO ! Geopotential Height (m)
     REAL, DIMENSION(:), ALLOCATABLE :: TAIR ! Air Temperature (k)
     REAL, DIMENSION(:), ALLOCATABLE :: QAIR ! Specific Air Humidity (kg/kg)
  END TYPE

  TYPE obs
     REAL :: lat  ! Latitude (South to North | -90 to 90)
     REAL :: lon  ! Longitude ( 0 to 360)
     REAL :: uvel ! Zonal wind component (m/s)
     REAL :: vvel ! Meridional wind component (m/s)
     REAL :: psnm ! Geopotential Height (m)
     REAL :: tair ! Air Temperature (k)
     REAL :: qair ! Specific Air Humidity (kg/kg)
  END TYPE


  !-------------------------------------------------------------------!
  !
  !-------------------------------------------------------------------!

  TYPE(grd) :: Mbufr
  TYPE(obs) :: Obufr

  CONTAINS

  SUBROUTINE ldata( type, Id, name )
     IMPLICIT NONE
     CHARACTER(LEN=*), INTENT(IN) :: type
     INTEGER,          INTENT(IN) :: Id
     CHARACTER(LEN=*), INTENT(IN) :: name
     print*,'----------------------------------------'

     call load_data(Id, name//char(0))
     SELECT CASE(trim(type))
        CASE('R')
          print*,'Reference'
        CASE('E')
          print*,'Experiment'
        CASE('C')
          print*,'Climatology'
     END SELECT


  END SUBROUTINE


END MODULE SCAM_dataMOD
