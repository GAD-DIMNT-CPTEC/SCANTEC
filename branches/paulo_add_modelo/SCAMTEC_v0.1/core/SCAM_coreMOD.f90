!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                  SCAMTeC - GDAD/CPTEC/INPE - 2010                   !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! Copyright 2010 Free Software Foundation, Inc.                       !
!                                                                     !
! This program is free software; you can redistribute it and/or modify!
! it under the terms of the GNU General Public License as published by!
! the Free Software Foundation; either version 2 of the License, or   !
! (at your option) any later version.                                 !
!                                                                     !
! This program is distributed in the hope that it will be useful,     !
! but WITHOUT ANY WARRANTY; without even the implied warranty of      !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       !
! GNU General Public License for more details.                        !
!                                                                     !
! You should have received a copy of the GNU General Public License   !
! along with GNU Emacs; see the file COPYING.  If not, write to the   !
! Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    !
! Boston, MA 02110-1301, USA.                                         !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!


MODULE SCAM_coreMOD
  !BOP
  !
  ! !MODULE: SCAM_coreMod
  ! 
  ! !DESCRIPTION: 
  !  The code in this file contains the basic datastructures and 
  !  control routines for the operation of SCAMTeC
  !
  !  \subsubsection{Overview}
  !
  !
  !  \begin{description}
  !
  !
  !
  ! !REVISION HISTORY: 
  !  30 Sep 2010    Joao Gerd  
  !                 Initial Specification
  ! 
  ! !USES:

  USE scamtec_module
  USE SCAM_Utils       ! Utilities for SCAMTeC running
  USE SCAM_dataMOD     ! Grid Data Structure
  USE time_module      ! Time operations
  USE m_string         ! String Manipulation
  USE SCAM_ModelPlugin ! A model register 
  USE SCAM_OutputMOD , only: write_2d   !
  USE SCAM_bstatistic  !
  USE m_ioutil

  IMPLICIT NONE
  PRIVATE

  !---------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !---------------------------------------------------------------------

  public :: SCAM_Config_init
  public :: SCAM_Init
  public :: SCAM_RUN
  public :: SCAM_Finalize


  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='SCAM_coreMod'

CONTAINS
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SCAM_Config_init - Configure some apects of SCAMTeC
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE SCAM_Config_init()

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP

    !_____________________________________________________________________

    character(len=*),parameter :: myname_=myname//'::SCAM_Config_init'

    !
    !  0. Hello
    !


#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !
    !  1. Read information about statistical analisys
    !

    CALL readcard()

    !
    !  2. Grid Reallocation to 0-360
    !

    WHERE(dom(:)%ll_lon.LT.0.)dom(:)%ll_lon=dom(:)%ll_lon+360.
    WHERE(dom(:)%ur_lon.LT.0.)dom(:)%ur_lon=dom(:)%ur_lon+360.    

    !
    !  3. Time configuration
    !

    scamtec%hist_incr      = real(hist_time/24.0d0)
    scamtec%incr           = real(time_step/24.0d0)
    scamtec%ntime_steps    = ( ( cal2jul(ending_time) - cal2jul(starting_time) + scamtec%incr ) / scamtec%incr )
    scamtec%ntime_forecast = ( Forecast_time / time_step ) + 1

#ifdef DEBUG    
   write(6,'(A,F9.3)')'history increment    :',scamtec%hist_incr
   write(6,'(A,F9.3)')'increment time       :',scamtec%incr
   write(6,'(A,I9.3)')'N time steps         :',scamtec%ntime_steps
   write(6,'(A,I9.3)')'N forecat time steps :',scamtec%ntime_forecast
#endif

  END SUBROUTINE SCAM_Config_init

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SCAM_init - Initialize all global variables of SCAMTeC
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE SCAM_Init()

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP

    !_____________________________________________________________________

    character(len=*),parameter :: myname_=myname//'::SCAM_Init'
    integer :: i

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !
    !  1. Registering models to be used
    !
	print*, '::: Chama o modelo a ser usado :::' !Paulo Dias
    call scam_models_plugin()

    !
    !  2. Registering observations to be used
    !

!    call scam_obs_plugin()

    !
    !  3. Registering statistical Methods
    !

!    call scam_stat_plugins()

    !
    !  4.
    !
	print*, '::: Preenchendo as variasveis do modelo ex: Area 1 :::'!Paulo Dias 
    call data_config()

    !
    !  5. Allocating memory
    !
	print*, '::: Alocando memoria :::'!Paulo Dias 
    call allocate_data_mem()

    !
    ! 6. Data Init
    !

    call data_init()

    !
    !  7. Models Init
    !

    DO I=1,size(scamtec%Init_ModelID)
       call config_model(scamtec%Init_ModelID(I))
    END DO

  END SUBROUTINE SCAM_Init

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SCAM_RUN - Run SCAMTeC
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE SCAM_RUN()

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP
    !------------------------------------------------------------------

    character(len=*),parameter :: myname_=myname//'::SCAM_RUN'
    integer             :: t, i, e, f, v
    integer             :: time
    integer             :: ftime
    integer             :: nymd, nhms
    integer             :: fymd, fhms
    character(len=1024) :: Reference      ! Reference File Name
    character(len=1024) :: Experiment     ! Experiment File Name
    character(len=1024) :: Climatology    ! Climatology File Name
    character(len=1024) :: OutFName
    integer             :: ier,CONT
    character(len=1024) :: formato
    real                :: tmp

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !
    !  1. Time Running
    !
    time=starting_time

    i=1

	print*, '::: Calculando VIES / RMSE / ACOR :::'!Paulo Dias		

    DO t=1,scamtec%ntime_steps !quantidade de tempo

       nymd = time/100
       nhms = MOD(time,100) * 10000

       !
       ! 1.1 Create file name and Open Reference data file 
       !
			
       Reference=TRIM(Refer%file)
       CALL str_template(Reference, nymd,nhms)
       CALL ldata('R', 1, Refer%Id, Reference)
		

       !
       ! 1.2 Create file name and Open Climatology data file
       !
       
       IF(clima_Flag.EQ.1)THEN		
          Climatology=TRIM(Clima%file)
          CALL str_template(Climatology, nymd,nhms)
          CALL ldata('C', 1, Clima%Id, Climatology)
		 
       END IF

       !
       ! 1.3 Loop over time forecast
       !
		
       ftime = time 

       DO f = 1, scamtec%ntime_forecast !quantidade de intervalo de tempo
		
          fymd = ftime/100
          fhms = MOD(ftime,100) * 10000

          DO e = 1, scamtec%nexp !numeros de experimento 
		
             !
             ! 1.3.1 Create Experiment File Names
             !

             Experiment = TRIM(Exper(e)%file)
             CALL str_template(Experiment, fymd, fhms, nymd, nhms)

             !
             ! 1.3.2 Open Experiment Data Files
             !
            
             CALL ldata('E',e,Exper(e)%Id, Experiment)
			
             DO v=1,scamtec%nvar !quantidades de variaveis 
		
             !
             ! 1.3.3 Basic Statistic Analisys
             !
            	
             !
             
             !		

		call SCAM_metricas (v,f,e) !Subrotina para calcular as Metricas 

!----------------------------------------------------------------------------------------------------------------- Descomentar
           !  scamdata(e)%diffield(:,:,v) = scamdata(e)%expfield(:,:,v) - scamdata(1)%reffield(:,:,v)  !Descomentar       
           !  scamdata(e)%time_vies(f,v)  = scamdata(e)%time_vies(f,v) + &  !Descomentar
           !                            (sum(scamdata(e)%diffield(:,:,v))/scamtec%npts)/scamtec%ntime_steps  !Descomentar
!-----------------------------------------------------------------------------------------------------------------Descomentar
		

!             OutFName = "B"//Exper(e)%name//"%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.gs4r"
!             CALL str_template(OutFName, fymd, fhms, nymd, nhms)
!             CALL write_2d(scamdata(e)%diffield,OutFName)

			

             !
             ! RMSE
             !
!-----------------------------------------------------------------------------------------------------------------Descomentar
            ! scamdata(e)%rmsfield(:,:,v) = scamdata(e)%diffield(:,:,v)*scamdata(e)%diffield(:,:,v) !Descomentar
            ! scamdata(e)%time_rmse(f,v)  = scamdata(e)%time_rmse(f,v) + & !Descomentar				
            !                            (sum(scamdata(e)%rmsfield(:,:,v))/scamtec%npts)/scamtec%ntime_steps !Descomentar
!-----------------------------------------------------------------------------------------------------------------Descomentar
			
	
		

!             OutFName = "R"//Exper(e)%name//"%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.gs4r"
!             CALL str_template(OutFName, fymd, fhms, nymd, nhms)
!             CALL write_2d(scamdata(e)%diffield,OutFName)
 
             !
             ! Anomaly Correlation
             !
!-----------------------------------------------------------------------------------------------------------------Descomentar
            ! if(Clima_Flag.eq.1)then !Descomentar

!             OutFName = "A"//Exper(e)%name//"%iy4%im2%id2%ih2%fy4%fm2%fd2%fh2.gs4r" 
!             CALL str_template(OutFName, fymd, fhms, nymd, nhms)
!             CALL write_2d(scamdata(e)%anofield,OutFName)


              !  CALL corr(scamdata(e)%expfield(:,:,v)-scamdata(1)%clmfield(:,:,v),& !Descomentar
              !            scamdata(1)%reffield(:,:,v)-scamdata(1)%clmfield(:,:,v),& !Descomentar
              !            tmp) !Descomentar
              !  scamdata(e)%time_acor(f,v) = scamdata(e)%time_acor(f,v) + & !Descomentar
                                          !tmp/float(scamtec%ntime_steps) !Descomentar
		
             !endif !Descomentar
!-----------------------------------------------------------------------------------------------------------------Descomentar
             !
             ! 1.3.4 Other statistics metrics
             !

             ENDDO

          ENDDO
	
          ftime = jul2cal(cal2jul(ftime)-scamtec%incr)

       ENDDO
	
       !
       !  1.3. History output
       !

       if ( time .EQ. jul2cal(cal2jul(starting_time)+scamtec%hist_incr*i) )then
          print*,'Writing History file : ',time,starting_time

          !         CALL HISTORY(time)

          i=i+1
       endif

       !
       ! 1.4 Write output
       !
       
       

       if (t.eq.scamtec%ntime_steps)then
          print*, '::: Arquivos txt salvo no diretorio de saida:::'!Paulo Dias
          DO e=1,scamtec%nexp
		
             scamdata(e)%time_rmse = sqrt(scamdata(e)%time_rmse)


             call opntext(e+0,trim(output_dir)//'/'//'vies'//trim(Exper(e)%name)//'.txt','unknown',ier)
             call opntext(e+1,trim(output_dir)//'/'//'rmse'//trim(Exper(e)%name)//'.txt','unknown',ier)
             if(clima_flag.eq.1)call opntext(e+2,trim(output_dir)//'/'//'acor'//Trim(Exper(e)%name)//'.txt','unknown',ier)
	                  
             write(formato,'(A4,I3.3,A5)')'(A9,',scamtec%nvar,'A9)'
             write(e+0,formato)'%Previsao',(VarName(v),v=1,scamtec%nvar)
             write(e+1,formato)'%Previsao',(VarName(v),v=1,scamtec%nvar)
             write(e+2,formato)'%Previsao',(VarName(v),v=1,scamtec%nvar)


             write(formato,'(A9,I3.3,A5)')'(6x,I3.3,',scamtec%nvar,'F9.3)'
             DO f=1,scamtec%ntime_forecast
               write(e+0,formato)(f-1)*time_step,(scamdata(e)%time_vies(f,v),v=1,scamtec%nvar)
               write(e+1,formato)(f-1)*time_step,(scamdata(e)%time_rmse(f,v),v=1,scamtec%nvar)
               if(clima_flag.eq.1)write(e+2,formato)(f-1)*time_step,(scamdata(e)%time_acor(f,v),v=1,scamtec%nvar)
	    ENDDO

             call clstext(e+0,ier)
             call clstext(e+1,ier)
             if(clima_flag.eq.1)call clstext(e+2,ier)
	     
          ENDDO

       endif


       time=jul2cal(cal2jul(time)+scamtec%incr)
    ENDDO


  END SUBROUTINE SCAM_RUN

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: SCAM_Finalize - Finalize all processes of SCAMTeC
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE SCAM_Finalize

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP

    character(len=*),parameter :: myname_=myname//'::SCAM_Finalize'

    !
    !  0. Hello
    !


#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


  END SUBROUTINE SCAM_Finalize

END MODULE SCAM_coreMOD
