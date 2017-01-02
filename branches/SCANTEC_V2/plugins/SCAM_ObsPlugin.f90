MODULE SCAM_ObsPlugin

  USE ObsTemplateModule

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
  !  08 Apr 2016    J. G. de Mattos  Initial Prototype
  !
  !EOP

  PRIVATE
  !-------------------------------------------------------------------
  ! Available Models
  !-------------------------------------------------------------------

  integer, public, parameter :: templateId  = 0  ! Template

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: SCAM_ObsPluginRegister

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='SCAM_ObsPlugin'

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: SCAM_ObsPlugin
  !  \label{SCAM_Obslugin}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new obs type. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each of the obs type that is included in SCAMTEC.
  !
  ! !INTERFACE:
    
  SUBROUTINE SCAM_ObsPluginRegister

    !  !REVISION HISTORY: 
    !  08 Apr 2016    J. G. de Mattos  Initial prototype
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::SCAM_ObsPluginRegister'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !------------------------------------------------------------------
    ! Registering Observation Types
    !------------------------------------------------------------------
    ! Template
    call RegisterObsInit(templateId,template_Obsinit)
    call RegisterObsRead(templateId,template_ObsRead)

  END SUBROUTINE SCAM_ObsPluginRegister
  !EOC
  !-------------------------------------------------------------------
END MODULE SCAM_ObsPlugin
