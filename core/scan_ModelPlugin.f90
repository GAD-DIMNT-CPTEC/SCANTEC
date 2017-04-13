MODULE scan_Modelplugin

  USE m_agcm
  USE m_clima50yr
  USE m_brams
  
  IMPLICIT NONE
  !BOP
  !
  !  !MODULE: scan_Modelplugin
  ! 
  !  !DESCRIPTION: 
  !   The code in this file provides values of indices used to 
  !   to register functions in the plugin modules
  !  
  !  The index definitions are simply a convention
  !  The user may change these options, and the scantec.conf 
  !  should be changed appropriately to ensure that the correct function
  !  is called at run time
  !
  !  This module contains, also, the definition of the functions used 
  !  for open the numerical models states to use for evaluatin an other 
  !  relevant computations using the models, corresponding to each of 
  !  the Model used in scantec.
  !
  !  !REVISION HISTORY: 
  !  25 Oct 2011    J. G. de Mattos  Initial Specification
  !
  !EOP

  PRIVATE
  !-------------------------------------------------------------------
  ! Available Models
  !-------------------------------------------------------------------

  integer, public, parameter :: templateId           = 0  ! Template
  integer, public, parameter :: AGCMId               = 1  ! AGCM/CPTEC
  integer, public, parameter :: bramsId              = 2  ! BRAMS 5KM / CPTEC
  integer, public, parameter :: clima50yrId          = 3  ! 50yr Climatology / CPTEC

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: scan_Models_Plugin  

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='scan_Modelplugin'

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: scan_models_plugin
  !  \label{scan_models_plugin}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new Model. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each of the Model that is included in scantec.
  !
  ! !INTERFACE:
    
  SUBROUTINE scan_Models_Plugin

    !  !REVISION HISTORY: 
    !  25 Oct 2011    J. G. de Mattos  Initial Specification
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::scan_Models_Plugin'

    !------------------------------------------------------------------
    ! External Functions to read models
    !------------------------------------------------------------------
    ! Template
!    external template_init
    external template_read     
    ! AGCM/CPTEC
!    external agcm_init
!    external agcm_read
    ! Eta/CPTEC
!    external eta_init
    external eta_read 

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !------------------------------------------------------------------
    ! Registering models
    !------------------------------------------------------------------
    ! Template
!    call registermodelinit(templateId,template_init)
    call registermodelread(templateId,template_read)
    ! AGCM/CPTEC
    call registermodelinit(AGCMId,agcm_init)
    call registermodelread(AGCMId,agcm_read)
    ! clima50yr/CPTEC
    call registermodelinit(Clima50yrId,clima50yr_init)
    call registermodelread(Clima50yrId,clima50yr_read)

    ! BRAMS 5KM / CPTEC
    call registermodelinit(bramsId,brams_init)
    call registermodelread(bramsId,brams_read)
    
  END SUBROUTINE scan_models_plugin
  !EOC
  !-------------------------------------------------------------------
END MODULE scan_Modelplugin
