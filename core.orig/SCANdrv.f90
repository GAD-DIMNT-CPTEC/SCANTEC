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

PROGRAM SCANTeC

!BOP
!
! ! TITLE: SCAMTeC Documentation \\ Version 0.01
!
! !AUTHORS: Jo\~a Gerd Z. de Mattos
!
! !AFFILIATION: Group on Data Assimilation Development, CPTEC/INPE, Cachoeira Paulista - SP
! 
! !DATE: June 11, 2010
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
!  1. Configure SCAMTeC to RUN
!

   CALL SCAM_Config_init()                ! Subroutine defined in SCAM_coreMOD.f90

!
!  2. Initialize SCAMTeC main Variables and Plugins
!

   CALL SCAM_Init()                       ! Subroutine defined in SCAM_coreMOD.f90

!
!  3. RUN SCAMTeC
!

   CALL SCAM_RUN()                        ! Subroutine defined in SCAM_coreMOD.f90
   
!
!  4. Finalize SCAMTeC, clean memory and etc ...
!

   CALL SCAM_FINALIZE()                   ! Subroutine defined in SCAM_coreMOD.f90

END PROGRAM
