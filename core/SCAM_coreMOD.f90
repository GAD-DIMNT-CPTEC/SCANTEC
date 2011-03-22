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

   USE SCAM_Utils    ! Utilities for SCAMTeC running
   USE SCAM_gridMOD  ! Grid Data Structure
   USE time_module   ! Time operations

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
!	07ct10 - Joao Gerd
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
      hist_incr   = real(hist_time/24.0d0)
      incr        = real(time_step/24.0d0)
      ntime_steps = ( ( cal2jul(ending_time) - cal2jul(starting_time) + incr ) / incr )
!
!  4.
!
   END SUBROUTINE

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
!	07ct10 - Joao Gerd
!           Initial prototaype Code
!EOP

!_____________________________________________________________________

      character(len=*),parameter :: myname_=myname//'::SCAM_Init'
!
!  0. Hello
!

      PRINT*,'Hello from ', myname_

   END SUBROUTINE

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
!	07ct10 - Joao Gerd
!           Initial prototaype Code
!EOP
!_____________________________________________________________________

         character(len=*),parameter :: myname_=myname//'::SCAM_RUN'
         integer  :: t, time,i

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

      !
      !  1.1. Open True data
      !

!      CALL TRUE(time)

      !
      !  1.2. Analise each forecast 
      !

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

   END SUBROUTINE

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
!	07ct10 - Joao Gerd
!           Initial prototaype Code
!EOP

         character(len=*),parameter :: myname_=myname//'::SCAM_Finalize'

!
!  0. Hello
!

      PRINT*,'Hello from ', myname_


   END SUBROUTINE

END MODULE
