MODULE SCAM_ModelPlugin

  USE SCAM_ModTemplate
  USE SCAM_TQ062L028
  USE SCAM_Clima50yr

  IMPLICIT NONE
  !BOP
  !
  !  !MODULE: SCAM_Modelplugin
  ! 
  !  !DESCRIPTION: 
  !   The code in this file provides values of indices used to 
  !   to register functions in the plugin modules
  !  
  !  The index definitions are simply a convention
  !  The user may change these options, and the scamtec.conf 
  !  should be changed appropriately to ensure that the correct function
  !  is called at run time
  !
  !  This module contains, also, the definition of the functions used 
  !  for open the numerical models states to use for evaluatin an other 
  !  relevant computations using the models, corresponding to each of 
  !  the Model used in scamtec.
  !
  !  !REVISION HISTORY: 
  !  25 Oct 2011    J. G. de Mattos  Initial Specification
  !
  !EOP

  PRIVATE
  !-------------------------------------------------------------------
  ! Available Models
  !-------------------------------------------------------------------

!  integer, public, parameter :: templateId  = 0  ! Template
!  integer, public, parameter :: AGCMId      = 1  ! AGCM/CPTEC
!  integer, public, parameter :: BRAMS       = 2  ! BRAMS/CPTEC
!  integer, public, parameter :: clima50yrId = 3  ! 50yr Climatology / CPTEC

  character(len=50), public, parameter :: templateId  = 'none'              ! Template
  character(len=50), public, parameter :: TQ062L28Id  = 'TQ062L028'         ! AGCM/CPTEC 
  character(len=50), public, parameter :: clima50yrId = '50yr Climatology'  ! 50yr Climatology / CPTEC
!  character(len=50), public, parameter :: BAMId       = 'BAM'               ! Operational AGCM/CPTEC
!  character(len=50), public, parameter :: BRAMSId     = 'BRAMS'             ! Operational BRAMS/CPTEC
  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: SCAM_ModelPluginRegister

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='SCAM_ModelPlugin'

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: SCAM_ModelPluginRegister
  !  \label{SCAM_ModelPluginRegister}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new Model. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each of the Model that is included in SCAMTEC.
  !
  ! !INTERFACE:
    
  SUBROUTINE SCAM_ModelPluginRegister

    !  !REVISION HISTORY: 
    !  25 Oct 2011    J. G. de Mattos  Initial Prototype
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::SCAM_ModelPluginRegister'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !------------------------------------------------------------------
    ! Registering models
    !------------------------------------------------------------------
    ! Template

    call RegisterModelInit(trim(templateId)//char(0),ModTemplate_init)
    call RegisterModelRead(trim(templateId)//char(0),ModTemplate_read)

    ! AGCM/CPTEC - TQ062L28

    call RegisterModelInit(trim(TQ062L028Id)//char(0),TQ062L028_init)
    call RegisterModelRead(trim(TQ062L028Id)//char(0),TQ062L028_read)

    ! clima50yr/CPTEC

    call RegisterModelInit(trim(Clima50yrId)//char(0),clima50yr_init)
    call RegisterModelRead(trim(Clima50yrId)//char(0),clima50yr_read)

    ! Operational AGCM/CPTEC (BAM)

!    call RegisterModelInit(trim(BAMId)//char(0),BAM_init)
!    call RegisterModelRead(trim(BAMId)//char(0),BAM_read)

    ! Operational BRAMS/CPTEC

!    call RegisterModelInit(trim(BRAMSId)//char(0),eta_init)
!    call RegisterModelRead(trim(BRAMSId)//char(0),eta_read)


  END SUBROUTINE scam_ModelPluginRegister
  !EOC
  !-------------------------------------------------------------------
END MODULE SCAM_ModelPlugin
