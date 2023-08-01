!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                  scantec - GDAD/CPTEC/INPE - 2010                   !
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
!BOP
!
MODULE scan_Utils
   USE scantec_module
   ! Modules from INPAK90 lib
   USE m_inpak90, only: i90_LoadF, &
                        i90_getVal,&
                        i90_gint,  &
                        i90_gfloat,&
                        i90_gtoken,&
                        i90_gline, &
                        i90_label, &
                        i90_perr,  &
                        i90_die,   &
                        i90_lcase, &
                        i90_fullRelease
   use m_constants, only: i4, r4, r8
   
   !
   IMPLICIT NONE

   PRIVATE
!
! ROUTINES
!

   PUBLIC :: readcard
   PUBLIC :: banner

!
! ISTES DERIVED TYPES DEVEM PASSAR PARA O modulo >>> scantec_module.f90 <<<
!


!
! DERIVED TYPES
!


!   TYPE, PUBLIC  :: RUNS
!      CHARACTER(len=300)               :: Id
!      CHARACTER(len=300)               :: name
!      CHARACTER(len=300)               :: file
!   END TYPE
!
! Global Variables
!
   INTEGER, PUBLIC   :: starting_time
   INTEGER, PUBLIC   :: ending_time
   INTEGER, PUBLIC   :: time_step
   INTEGER, PUBLIC   :: Forecast_time
   INTEGER, PUBLIC   :: ntime_forecast
   INTEGER, PUBLIC   :: ntime_steps
   REAL(r8), PUBLIC  :: incr
   INTEGER, PUBLIC   :: hist_time
   REAL(r8), PUBLIC  :: hist_incr

!   TYPE(domain), PUBLIC, DIMENSION(:), ALLOCATABLE    :: dom


!   TYPE(RUNS), PUBLIC                            :: Refer
!   TYPE(RUNS), PUBLIC                            :: Clima
!   TYPE(RUNS), PUBLIC                            :: Precip             !paulo dias
   INTEGER   , PUBLIC                            :: Precipitation_Flag !paulo dias
!   TYPE(RUNS), PUBLIC, DIMENSION(:), ALLOCATABLE :: Exper
!   TYPE(RUNS), 

   !
   !Variaveis EOFs
   !
   INTEGER   , PUBLIC                            :: EOFs_Flag  !paulo dias
   INTEGER   , PUBLIC                            :: quant_EOFs !paulo dias

!---------------------------------------------------------------------paulo dias
! Variaveis de Preciptation Histograma  paulo dias
!
   TYPE, PUBLIC     :: param_hist                  
     REAL, PUBLIC     :: valor_limit, valor_min   !valor maximo e valor minimo 
     REAL, PUBLIC     :: rang                     !valor do range
     INTEGER, PUBLIC  :: tipo_precip              !tipo de precipitacao
     INTEGER, PUBLIC  :: acumulo_obs              !acumulo de precipitacao da observacao
     INTEGER, PUBLIC  :: acumulo_exp              !acumulo de precipitacao do experimento
   END TYPE
   
   TYPE(param_hist), public   :: hist   
!-------------------------------------------------------------------- paulo dias

!
! INTERFACES
!


   CONTAINS
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !ROUTINE: readcard
!  \label{readcard}
!
! !REVISION HISTORY:
!  ## ### ####   J. G. de Mattos - Initial Code
!  22 Oct 2012 - R. Mello        - Correcao na aquisição dos indices 
!                                  dos modelos.
!  06 Jan 2016 - J. G. de Mattos - Colocando linked list para obter 
!                                  informacoes dos experimentos.
!                                  Nao e mais necessario informar o
!                                  numeros de experimentos que deverao 
!                                  ser avaliados, isto evita erros de 
!                                  esquecimento.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
! !INTERFACE:
!
   SUBROUTINE readcard(istat)
      IMPLICIT NONE
      INTEGER, OPTIONAL,INTENT(OUT) :: istat
      INTEGER                       :: I, J, narg, ndom
      CHARACTER(len=300)            :: config, formato
      LOGICAL                       :: exists
      INTEGER, ALLOCATABLE          :: tmp(:)
#ifndef gfortran
      INTEGER, EXTERNAL             :: iargc
#endif

      CHARACTER(len=*),PARAMETER    :: myname_="readcard"
      integer :: epcount
      integer :: iret, ierr
      character(len=256) :: ModelName
      character(len=256) :: FileName
      character(len=256) :: ExpName
      character(len=256) :: timeStepType


!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
! !DESCRIPTION:
!
!  The code in this file initializes the scantec configuration management 
!  utility.
!  The generic model-independent runtime specifications of a scantec
!  are read by this routine. The routine also initializes the scantec
!  log buffers, and calls some of the registries that set up the 
!  component plugin definitions.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!
!EOP
#ifdef DEBUG
         WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
         WRITE(*,'(1x,a10,a8,a8,44x,a1)')'! INSIDE :', TRIM(myname_),' Routine','!'
         WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
#endif

         if(present(istat)) istat=0



   END SUBROUTINE

   SUBROUTINE BANNER()
      IMPLICIT NONE
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'!                  scantec - GDAD/CPTEC/INPE - 2010                   !'
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'! Copyright 2010 Free Software Foundation, Inc.                       !'
      WRITE(*,'(a72)')'!                                                                     !'
      WRITE(*,'(a72)')'! This program is free software; you can redistribute it and/or modify!'
      WRITE(*,'(a72)')'! it under the terms of the GNU General Public License as published by!'
      WRITE(*,'(a72)')'! the Free Software Foundation; either version 2 of the License, or   !'
      WRITE(*,'(a72)')'! (at your option) any later version.                                 !'
      WRITE(*,'(a72)')'!                                                                     !'
      WRITE(*,'(a72)')'! This program is distributed in the hope that it will be useful,     !'
      WRITE(*,'(a72)')'! but WITHOUT ANY WARRANTY; without even the implied warranty of      !'
      WRITE(*,'(a72)')'! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       !'
      WRITE(*,'(a72)')'! GNU General Public License for more details.                        !'
      WRITE(*,'(a72)')'!                                                                     !'
      WRITE(*,'(a72)')'! You should have received a copy of the GNU General Public License   !'
      WRITE(*,'(a72)')'! along with GNU Emacs; see the file COPYING.  If not, write to the   !'
      WRITE(*,'(a72)')'! Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,    !'
      WRITE(*,'(a72)')'! Boston, MA 02110-1301, USA.                                         !'
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
#ifdef DEBUG
      WRITE(*,'(a72)')'                                                                       '
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'!   ATTENTION :: RUNNING IN DEBUG MODE, THE PROGRAM CAN BECOME SLOW   !'
      WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
      WRITE(*,'(a72)')'                                                                       '
#endif

   END SUBROUTINE

   character(20) function int2str(num) 
      integer, intent(in):: num
      character(20) :: str
      ! convert integer to string using formatted write
      write(str, '(i20)') num
      int2str = adjustl(str)
   end function int2str

   subroutine unique(input,output,nelem)
     
     integer, intent(in)  :: input(:)   ! The input
     integer, intent(out) :: output(:)  ! The output
     integer, intent(out) :: nelem      ! The number of unique elements
     integer :: i, j
 
  nelem = 1
  output(1) = input(1)
  outer: do i=2,size(input)
     do j=1,nelem
        if (output(j) == input(i)) then
           ! Found a match so start looking again
           cycle outer
        end if
     end do
     ! No match found so add it to the output
     nelem = nelem + 1
     output(nelem) = input(i)
  end do outer

  end subroutine


END MODULE
