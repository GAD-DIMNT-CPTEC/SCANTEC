SUBROUTINE SCAM_metricas (v,f,e)
USE SCAM_dataMOD
USE SCAM_bstatistic
USE SCAM_Utils
USE SCAM_coreMOD

  USE scamtec_module

IMPLICIT NONE


integer v,f,e !indices dos loops que vem do SCAM_coreMod
real :: tmp
!- Calculando VIES
!----------------------------------------------------------------------------------------------------------------- 
scamdata(e)%diffield(:,:,v) = scamdata(e)%expfield(:,:,v) - scamdata(1)%reffield(:,:,v)
             scamdata(e)%time_vies(f,v)  = scamdata(e)%time_vies(f,v) + &
                                        (sum(scamdata(e)%diffield(:,:,v))/scamtec%npts)/scamtec%ntime_steps
!-----------------------------------------------------------------------------------------------------------------

!- Calculando RMSE
!------------------------------------------------------------------------------------------------------------------
scamdata(e)%rmsfield(:,:,v) = scamdata(e)%diffield(:,:,v)*scamdata(e)%diffield(:,:,v)
             scamdata(e)%time_rmse(f,v)  = scamdata(e)%time_rmse(f,v) + &
                                        (sum(scamdata(e)%rmsfield(:,:,v))/scamtec%npts)/scamtec%ntime_steps
!-------------------------------------------------------------------------------------------------------------------

!- Calculando ACOR
!-------------------------------------------------------------------------------------------------------------------
if(Clima_Flag.eq.1)then
	CALL corr(scamdata(e)%expfield(:,:,v)-scamdata(1)%clmfield(:,:,v),&
                          scamdata(1)%reffield(:,:,v)-scamdata(1)%clmfield(:,:,v),tmp)
                scamdata(e)%time_acor(f,v) = scamdata(e)%time_acor(f,v) + &
                                          tmp/float(scamtec%ntime_steps)
endif

!-------------------------------------------------------------------------------------------------------------------




END SUBROUTINE SCAM_metricas
