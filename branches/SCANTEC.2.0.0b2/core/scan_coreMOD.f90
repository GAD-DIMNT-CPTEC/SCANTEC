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


MODULE scan_coreMOD
  !BOP
  !
  ! !MODULE: scan_coreMod
  ! 
  ! !DESCRIPTION: 
  !  The code in this file contains the basic datastructures and 
  !  control routines for the operation of scantec
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

  USE scantec_module
  USE scan_Utils                          ! Utilities for scantec running
  USE scan_dataMOD                        ! Grid Data Structure
  USE time_module, only: jul2cal, cal2jul ! Time operations
  USE m_string                            ! String Manipulation
  USE scan_ModelPlugin                    ! A model register 
  USE scan_OutputMOD , only: write_2d     !
  USE scan_bstatistic                     !
  USE m_ioutil
  USE m_constants, only: r8
  USE scan_MathPlugin
!  use omp_lib

  IMPLICIT NONE
  PRIVATE

  !---------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !---------------------------------------------------------------------

  public :: scan_Config_init
  public :: scan_Init
  public :: scan_RUN
  public :: scan_Finalize
  public :: scan_EndRun


  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='scan_coreMod'

CONTAINS
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: scan_Config_init - Configure some apects of scantec
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE scan_Config_init()

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP

    !_____________________________________________________________________

    character(len=*),parameter :: myname_=myname//'::scan_Config_init'

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


    scantec%time_step      = 1
    scantec%ftime_idx      = 1
    scantec%loop_count     = 1
    scantec%atime_flag     = .true.


    scantec%hist_incr      = real(scantec%hist_time/24.0d0)
    scantec%aincr          = real(scantec%atime_step/24.0d0)
    scantec%fincr          = real(scantec%ftime_step/24.0d0)


    scantec%ntime_steps    = ( ( cal2jul(scantec%ending_time) - &
                              cal2jul(scantec%starting_time) +  &
                              scantec%aincr ) / scantec%aincr )

    scantec%ntime_forecast = ( scantec%Forecast_time / scantec%ftime_step ) + 1

    !scantec%atime          = scantec%starting_time
    !scantec%ftime          = jul2cal(cal2jul(scantec%starting_time)+scantec%fincr)

    scantec%atime          = scantec%starting_time
    scantec%ftime          = scantec%starting_time

    Allocate(scantec%ftime_count(scantec%ntime_forecast))
    scantec%ftime_count    = 0
    scantec%ftime_count(1) = 1

    ! 4. Initialize Mathematical Expressions

    call scan_mathPlugin_Init()

#ifdef DEBUG    
   write(6,'(A,F9.3)')'history increment    :',scantec%hist_incr
   write(6,'(A,F9.3)')'Analisys increment   :',scantec%aincr
   write(6,'(A,F9.3)')'Forecast increment   :',scantec%aincr           
   write(6,'(A,I9.3)')'N time steps         :',scantec%ntime_steps
   write(6,'(A,I9.3)')'N forecast time steps :',scantec%ntime_forecast
#endif

  END SUBROUTINE scan_Config_init

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: scan_init - Initialize all global variables of scantec
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE scan_Init()

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP

    !_____________________________________________________________________

    character(len=*),parameter :: myname_=myname//'::scan_Init'
    integer :: i

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !
    !  1. condigure scantec grid and variable names
    !

    call data_config ( )

    !
    !  2. Registering models to be used and configure it
    !

    call scan_models_plugin()

    !
    !  3. Registering observations to be used
    !

!    call scan_obs_plugin()

    !
    !  4. Registering statistical Methods
    !

!    call scan_stat_plugins()

    !
    !  5. Allocating memory
    !

    call allocate_data_mem()

    !
    ! 6. Data Init
    !

    call data_init()


    !
    ! 8. Print informations
    !

!    write ( *, '(a,i8)' ) '  The number of processors available = ', omp_get_num_procs ( )
!    write ( *, '(a,i8)' ) '  The number of threads available    = ', omp_get_max_threads ( )

  END SUBROUTINE scan_Init
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: scan_RUN - Run scantec
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE scan_RUN()

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP
    !------------------------------------------------------------------

    character(len=*),parameter :: myname_=myname//'::scan_RUN'
    integer             :: NExp

    !
    !  0. Hello
    !
    
       WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'
       WRITE(*,'(a72)')'!                          Running scantec                            !'
       WRITE(*,'(a72)')'!     Please wait while the system is performing the statistics       !'
       WRITE(*,'(a72)')'!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    !
    !  1. Loop over time and experiments
    !

     write(*,'(3A11)')'Analisys','Forecast','fct'
     DO WHILE (.NOT.is_last_step())
        if(scantec%timeStepType .eq. 'forward')then
           write(*,'(2(1x,I10.10),7x,I3.2"h")')scantec%atime, scantec%ftime,&
                 int(abs(cal2jul(scantec%atime)-cal2jul(scantec%ftime))*24)
        else
           write(*,'(2(1x,I10.10),7x,I3.2"h")')scantec%ftime, scantec%atime,&
                 int(abs(cal2jul(scantec%atime)-cal2jul(scantec%ftime))*24)
        endif
        
        DO NExp=1,scantec%nexp
           CALL scan_ModelData ( NExp )  ! Load Files: Analisys, Forecast and Climatology
           CALL CalcBstat ( NExp )       ! Calculate Basic Statistics: Bias, RMSE, Anomaly Correlation
        ENDDO

        call scan_NextStep( )
     ENDDO


  END SUBROUTINE scan_RUN


  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: scan_Finalize - Finalize all processes of scantec
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  SUBROUTINE scan_Finalize

    implicit none

    ! !REVISION HISTORY:
    !       07ct10 - Joao Gerd
    !           Initial prototaype Code
    !EOP

    character(len=*),parameter :: myname_=myname//'::scan_Finalize'

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

  END SUBROUTINE scan_Finalize
 
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !               INPE/CPTEC Data Assimilation Group                   !
  !---------------------------------------------------------------------
  !BOP
  !
  ! !IROUTINE: scan_Finalize - verify if julian is the last day 
  !                            of run of scantec analisys
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  recursive SUBROUTINE scan_EndRun( )
!     implicit none
!     integer, intent(in) :: jd
!     integer, intent(in) :: jd_incr
!     integer, intent(in) :: jd_end


    !REVISION HISTORY:
    !  Initial Code :: Joao Gerd - 17Aug2012
    !
    !EOP

!    character(len=*),parameter :: myname_=myname//'::scan_EndRun'

    !
    !  0. Hello
    !

!#ifdef DEBUG
!    WRITE(6,'(     2A)')'Hello from ', myname_
!#endif
    
!   scantec%atime=jul2cal(cal2jul(scantec%atime)+scantec%incr)
!   print*,scantec%atime,scantec%ending_time
!   if (scantec%atime.ge.scantec%ending_time) call exit()

  END SUBROUTINE scan_EndRun


  SUBROUTINE scan_NextStep()
     implicit none
     REAL(r8) :: aincr
     REAL(r8) :: fincr
     integer :: I, Nx, Ny
     integer :: ii, jj

     INTEGER :: atimebufr

     scantec%loop_count = scantec%loop_count + 1

     I  = scantec%loop_count
     Nx = scantec%ntime_steps
     Ny = scantec%ntime_forecast

     ii = ceiling((I)/float(Ny))  
     jj = ( I + Ny ) - Ny * ii
    
               
     aincr = (ii-1) * scantec%aincr
     fincr = (jj-1) * scantec%fincr
     
     atimebufr               = scantec%atime
     scantec%atime           = jul2cal(cal2jul(scantec%starting_time)+aincr)

     if(scantec%TimeStepType .eq. 'forward')then
        scantec%ftime           = jul2cal(cal2jul(scantec%atime)+fincr)
     else
        scantec%ftime           = jul2cal(cal2jul(scantec%atime)-fincr)
     endif

     scantec%ftime_idx       = jj
     scantec%ftime_count(jj) = scantec%ftime_count(jj) + 1
     scantec%atime_flag      = (atimebufr.ne.scantec%atime)

     if (scantec%atime_flag)scantec%time_step = scantec%time_step + 1


  END SUBROUTINE

!BOP
! !ROUTINE: is_last_step
! \label{is_last_step}
!
! !INTERFACE:
function is_last_step( )
   implicit none
! !ARGUMENTS: 
   logical :: is_last_step
! 
! !DESCRIPTION:
!
! Function returns true on last timestep.
!
!  \begin{description}
!   \item [is\_last\_step]
!     result of the function 
!  \end{description}
!EOP

   is_last_step = .false.
   if(scantec%atime .gt. scantec%ending_time) then
      is_last_step = .true.
   endif
   
end function is_last_step
END MODULE scan_coreMOD
