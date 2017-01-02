MODULE SCAM_StatPlugin

  USE StatTemplateModule

  IMPLICIT NONE
  !BOP
  !
  !  !MODULE: SCAM_StatPlugin
  ! 
  !  !DESCRIPTION: 
  !   The code in this file provides values of indices used to 
  !   to register functions in the plugin statistics types
  !  
  !  The index definitions are simply a convention
  !  The user may change these options, and the scamtec.conf 
  !  should be changed appropriately to ensure that the correct function
  !  is called at run time
  !
  !  This module contains, also, the definition of the functions used 
  !  for apply statistical methods to evaluate the numerical models,
  !  each module registred correspond to a different statistical method 
  !  used by scamtec.
  !
  !  !REVISION HISTORY: 
  !  08 Apr 2016    J. G. de Mattos  Initial Prototype
  !
  !EOP

  PRIVATE
  !-------------------------------------------------------------------
  ! Available Statistics Methods
  !-------------------------------------------------------------------

  integer, public, parameter :: templateId  = 0  ! Template

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: SCAM_StatPluginRegister

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='SCAM_Statplugin'

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: SCAM_StatPluginRegister
  !  \label{SCAM_StatPluginRegister}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new statistical 
  ! method. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each statistical method that is included in SCAMTEC.
  !
  ! !INTERFACE:
    
  SUBROUTINE SCAM_StatPluginRegister

    !  !REVISION HISTORY: 
    !  08 Apr 2016    J. G. de Mattos  Initial Prototype
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::SCAM_StatPluginRegister'

    !------------------------------------------------------------------
    ! External Functions to initilize, run and Finalize statistical methods
    !------------------------------------------------------------------
    ! Template
!    external template_Init
!    external template_Run
!    external template_Write
!    external template_Finalize

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !------------------------------------------------------------------
    ! Registering Statistical Methods
    !------------------------------------------------------------------
    ! Template
    call RegisterStatInit(templateId,template_Init)
    call RegisterStatRun(templateId,template_Run)
    call RegisterStatWrite(templateId,template_Write)
    call RegisterStatFinalize(templateId,template_Finalize)

  END SUBROUTINE SCAM_StatPluginRegister
  !EOC
  !-------------------------------------------------------------------
END MODULE SCAM_StatPlugin
