MODULE SCAM_Modelplugin
  USE m_agcm
  USE m_clima50yr
  USE m_T126_Seq !paulo dias
  USE obs_Precipitation !Paulo Dias
  USE m_agcm_T213 !Paulo Dias
  USE m_Ensemble_T126
  USE m_grb_ensemble_t126
  USE m_TQ062L028
  USE m_EraInterim
!  USE m_WRF9km
!  USE m_WRF3km
  
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

  integer, public, parameter :: templateId           = 0  ! Template
  integer, public, parameter :: AGCMId               = 1  ! AGCM/CPTEC
  integer, public, parameter :: EtaId                = 2  ! Eta/CPTEC
  integer, public, parameter :: clima50yrId          = 3  ! 50yr Climatology / CPTEC
  integer, public, parameter :: T126_SeqId           = 4  ! IWV-PSAS T126 !Paulo Dias
  integer, public, parameter :: PrecipitationId     = 5  ! Precipitation !Paulo Dias
  integer, public, parameter :: AGCM_T213Id          = 6  ! AGCM_T213/CPTEC !Paulo Dias
  integer, public, parameter :: Ensemble_T126Id      = 7  ! T126 Ensemble 
  integer, public, parameter :: Grb_ensemble_t126Id  = 8  ! Grb_ensemble_t126
  integer, public, parameter :: TQ062L028Id          = 9
  integer, public, parameter :: EraInterimId         = 10
!  integer, public, parameter :: WRF9kmId             = 11 ! WRF9km
!  integer, public, parameter :: WRF3kmId             = 12 ! WRF3km

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: SCAM_Models_Plugin  

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='SCAM_Modelplugin'

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
    character(len=*),parameter :: myname_=myname//'::SCAM_Models_Plugin'

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
    ! Eta/CPTEC
!    call registermodelinit(EtaId,eta_init)
    call registermodelread(EtaId,eta_read)
    ! clima50yr/CPTEC
    call registermodelinit(Clima50yrId,clima50yr_init)
    call registermodelread(Clima50yrId,clima50yr_read)
    ! IWV-PSAS_T126
    call registermodelinit(T126_SeqId,T126_Seq_init) !Paulo Dias 
    call registermodelread(T126_SeqId,T126_Seq_read) !Paulo Dias
    ! Precipitation
    call registermodelinit(PrecipitationId,Precipitation_init) !Paulo Dias 
    call registermodelread(PrecipitationId,Precipitation_read)
    ! AGCM/CPTEC
    call registermodelinit(AGCM_T213Id,agcmT213_init)
    call registermodelread(AGCM_T213Id,agcmT213_read)
    ! T126 Ensemble
    call registermodelinit(Ensemble_T126Id,Ensemble_T126_init) !
    call registermodelread(Ensemble_T126Id,Ensemble_T126_read) !    
    ! T126 Ensemble gribs
    call registermodelinit(Grb_ensemble_t126Id,Grb_ensemble_t126_init) !
    call registermodelread(Grb_ensemble_t126Id,Grb_ensemble_t126_read) !
    ! TQ062L028 gribs
    call registermodelinit(TQ062L028Id,TQ062L028_init) !
    call registermodelread(TQ062L028Id,TQ062L028_read) !
     ! EraInterim gribs
    call registermodelinit(EraInterimId,EraInterim_init) !
    call registermodelread(EraInterimId,EraInterim_read) !
     ! WRF9km gribs
!    call registermodelinit(WRF9kmId,WRF9km_init) !
!    call registermodelread(WRF9kmId,WRF9km_read) !
     ! WRF3km gribs
!    call registermodelinit(WRF3kmId,WRF3km_init) !
!    call registermodelread(WRF3kmId,WRF3km_read) !
 
    
  END SUBROUTINE scam_models_plugin
  !EOC
  !-------------------------------------------------------------------
END MODULE SCAM_Modelplugin
