MODULE SCAM_Modelplugin
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

  integer, public, parameter :: templateId = 0  ! Template
  integer, public, parameter :: AGCMId     = 1  ! AGCM/CPTEC
  integer, public, parameter :: EtaId      = 2  ! Eta/CPTEC

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: SCAM_Models_Plugin  

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: scam_models_plugin
  !  \label{scam_models_plugin}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new Model. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each of the Model that is included in SCAMTEC.
  !
  ! !INTERFACE:
    
  SUBROUTINE SCAM_Models_Plugin

    !  !REVISION HISTORY: 
    !  25 Oct 2011    J. G. de Mattos  Initial Specification
    !
    !------------------------------------------------------------------
    !BOC

    !------------------------------------------------------------------
    ! External Functions to read models
    !------------------------------------------------------------------
    ! Template
!    external template_init
    external template_read
    ! AGCM/CPTEC
!    external agcm_init
    external agcm_read
    ! Eta/CPTEC
!    external eta_init
    external eta_read

    !------------------------------------------------------------------
    ! Registering models
    !------------------------------------------------------------------
    ! Template
!    call registermodelinit(templateId,template_init)
    call registermodelread(templateId,template_read)
    ! AGCM/CPTEC
!    call registermodelinit(AGCMId,agcm_init)
    call registermodelread(AGCMId,agcm_read)
    ! Eta/CPTEC
!    call registermodelinit(EtaId,eta_init)
    call registermodelread(EtaId,eta_read)

  END SUBROUTINE scam_models_plugin
  !EOC
  !-------------------------------------------------------------------
END MODULE SCAM_Modelplugin
