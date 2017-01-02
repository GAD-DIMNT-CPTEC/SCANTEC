!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                  SCAMTEC - CPTEC /INPE - 2010-2016                  !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~! 
!  Copyright 2016 Creative Commons BY-NC-SA 4.0 License               !
!                                                                     !
!                       Release License                               !
!                                                                     !
!  Released into the public domain.                                   !
!  This work is free: you can redistribute it and/or modify it under  !
!  the terms of Creative Commons BY-NC-SA 4.0 International License.  !
!                                                                     !
!  To view a copy of this license, visit:                             !
!                                                                     !
!  https:!creativecommons.org/licenses/by-nc-sa/4.0/legalcode         !
!                                                                     !
!  or send a letter to Creative Commons, 171 Second Street, Suite 300,!
!  San Francisco, California, 94105, USA.                             !
!                                                                     !
! This program is distributed in the hope that it will be useful,     !
! but WITHOUT ANY WARRANTY; without even the implied warranty of      !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       !
! CC BY-NC-SA 4.0 International License for more details.             !
!                                                                     !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

PROGRAM SCAMTEC

!BOP
!
! ! TITLE: SCAMTeC Documentation \\ Version 1.0
!
! !AUTHORS: Jo\~a Gerd Z. de Mattos
!
! !AFFILIATION: Group on Data Assimilation Development, CPTEC/INPE, Cachoeira Paulista - SP
! 
! !DATE: April 15, 2016
!
! !INTRODUCTION: Package Overview
!
!    SCAMTeC is a Fortran 90 collection of routines/functions for 
!    .
!    .
!    .
!
!BOP

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! !ROUTINE: main.f90
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!               INPE/CPTEC Data Assimilation Group                   !
!---------------------------------------------------------------------
!
! !REVISION HISTORY:
!  Initial Code :: Joao Gerd - 11Jun2010
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !USES:

   USE SCAM_coreMOD

! !INTERFACE:
!

   IMPLICIT NONE

!
!  1. Configure SCAMTEC to RUN
!

   CALL SCAM_Config_init( )

!
!  2. Initialize SCAMTEC main Variables and Plugins
!

   CALL SCAM_Init( )

!
!  3. RUN SCAMTeC
!

   CALL SCAM_Run( )

!
!  4. Finalize SCAMTEC, clean memory and etc ...
!

   CALL SCAM_Finalize( )

END PROGRAM
