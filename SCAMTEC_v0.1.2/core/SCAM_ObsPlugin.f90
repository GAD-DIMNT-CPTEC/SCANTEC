MODULE SCAM_Obsplugin
  USE m_iwv
  
  
  IMPLICIT NONE
  !BOP
  !
  !  !MODULE: SCAM_obsplugin
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
  !  23 Oct 2013    F. A. Capizzani
  !
  !EOP

  PRIVATE
  !-------------------------------------------------------------------
  ! Available Models
  !-------------------------------------------------------------------

  integer, public, parameter :: templateId       = 0  ! Template
  integer, public, parameter :: IWVId            = 1  ! IWV/CPTEC
  

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: SCAM_Obs_Plugin  

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='SCAM_Obsplugin'

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: scam_obs_plugin
  !  \label{scam_obs_plugin}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new Observation. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each of the Model that is included in SCAMTEC.
  !
  ! !INTERFACE:
    
  SUBROUTINE SCAM_Obs_Plugin

    !  !REVISION HISTORY: 
    !25 Oct 2011    J. G. de Mattos    Initial Specification
    !23 Oct 2013    F. A. Capizzani  
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::SCAM_obs_Plugin'

    !------------------------------------------------------------------
    ! External Functions to read models
    !------------------------------------------------------------------
    ! Template
!    external template_init
    external template_read
    ! IWV/CPTEC
!    external IWV_init
!    external IWV_read
#ifdef DEBUG
    WRITE(6,'(     2A)')'Entrando na rotina ', myname_, ' obs'
#endif 

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
	PRINT*,'Iniciando SCAM OBS PLUGIN'
#endif
	
    !------------------------------------------------------------------
    ! Registering observations
    !------------------------------------------------------------------
    ! Template
!    call registermodelinit(templateId,template_init)
    !call registerobsread(templateId,template_read)
    ! IWV/CPTEC
    call registerobsinit(IWVId,iwv_init)
    !call registerobsread(IWVId,iwv_read)
    
  END SUBROUTINE scam_obs_plugin
  !EOC
  !-------------------------------------------------------------------
END MODULE SCAM_Obsplugin
