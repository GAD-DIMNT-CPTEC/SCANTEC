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

  USE SCAM_Utils       ! Utilities for SCAMTeC running
  USE SCAM_dataMOD     ! Grid Data Structure
  USE time_module      ! Time operations
  USE m_string         ! String Manipulation
  USE SCAM_ModelPlugin ! A model register 

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

    PRINT*,'Hello from ', myname_

    !
    !  1. Read information about statistical analisys
    !

    CALL readcard()

    !
    !  2. Grid Reallocation to 0-360
    !

    WHERE(dom(:)%ll_lon.LT.0.)dom(:)%ll_lon=dom(:)%ll_lon+180.
    WHERE(dom(:)%ur_lon.LT.0.)dom(:)%ur_lon=dom(:)%ur_lon+180.    

    !
    !  3. Time configuration
    !
    hist_incr      = real(hist_time/24.0d0)
    incr           = real(time_step/24.0d0)
    ntime_steps    = ( ( cal2jul(ending_time) - cal2jul(starting_time) + incr ) / incr )
    ntime_forecast = ( Forecast_time / time_step ) + 1
    !
    !  4.
    !
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
    !
    !  0. Hello
    !

    PRINT*,'Hello from ', myname_

    !
    !  1. Registering models to be used
    !

    call scam_models_plugin()

    !
    !  2. Registering observations to be used
    !

!    call scam_obs_plugin()

    !
    !  3. Registering statistical Methods
    !

!    call scam_stat_plugins()

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
    integer             :: t, i, e, f
    integer             :: time
    integer             :: ftime
    integer             :: nymd, nhms
    integer             :: fymd, fhms
    character(len=1024) :: Reference    ! Reference File Name
    character(len=1024) :: Experiment   ! Experiment File Name
    character(len=1024) :: Climatology  ! Climatology File Name

    !
    !  0. Hello
    !

    PRINT*,'Hello from ', myname_

    !
    !  1. Time Running
    !
    time=starting_time

    i=1
    DO t=1,ntime_steps

       nymd = time/100
       nhms = MOD(time,100) * 10000

       !
       ! 1.1 Create file name and Open Reference data file 
       !

       Reference=TRIM(Refer%file)
       CALL str_template(Reference, nymd,nhms)
       CALL ldata('R', Refer%Id, Reference)

       !
       ! 1.2 Create file name and Open Climatology data file
       !

       IF(clima_Flag.EQ.1)THEN
          Climatology=TRIM(Clima%file)
          CALL str_template(Climatology, nymd,nhms)
          CALL ldata('C', Clima%Id, Climatology)
       END IF

             
       !
       ! 1.3 Loop over time forecast
       !

       ftime = time 

       DO f = 1, ntime_forecast

          fymd = ftime/100
          fhms = MOD(ftime,100) * 10000

          DO e = 1, nexper

             !
             ! 1.3.1 Create Experiment File Names
             !

             Experiment = TRIM(Exper(e)%file)
             CALL str_template(Experiment, fymd, fhms, nymd, nhms)

             !
             ! 1.3.2 Open Experiment Data Files
             !
             
             CALL ldata('E',Exper(e)%Id, Experiment)


          ENDDO

          ftime = jul2cal(cal2jul(ftime)-incr)

       ENDDO

       !      CALL ANALISE(time)

       !
       !  1.3. History output
       !

       if ( time .EQ. jul2cal(cal2jul(starting_time)+hist_incr*i) )then
          print*,'Writing History file : ',time,starting_time

          !         CALL HISTORY(time)

          i=i+1
       endif


       time=jul2cal(cal2jul(time)+incr)
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

    PRINT*,'Hello from ', myname_


  END SUBROUTINE SCAM_Finalize

END MODULE SCAM_coreMOD
