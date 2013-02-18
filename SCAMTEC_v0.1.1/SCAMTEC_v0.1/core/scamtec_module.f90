MODULE scamtec_module

!
! KINDS
!

   integer, public, parameter :: I4B = SELECTED_INT_KIND(9)
   integer, public, parameter :: I2B = SELECTED_INT_KIND(4)
   integer, public, parameter :: I1B = SELECTED_INT_KIND(2)
   integer, public, parameter :: SP  = KIND(1.0)
   integer, public, parameter :: DP  = KIND(1.0D0)

!
! scamtec data type
!

  type scamdec

!
! General Variables
!

     integer(I4B) :: starting_time      ! Initial time
     integer(I4B) :: ending_time        ! End time
     integer(I4B) :: time_step          ! Atual time step in hours
     integer(I4B) :: loop_count         !
!
! Analisys Variables
!
     integer(I4B) :: atime              ! Analisys time
     integer(I4B) :: atime_step         ! Analisys time step
     integer(I4B) :: ntime_steps        ! total # of Analisys time step
     real(DP)     :: aincr              ! Analisys time increment
     logical      :: atime_flag         ! is it a new date reference (not forecast)?

!
! Forecast Variables
!
     integer(I4B) :: ftime              ! forecast time
     integer(I4B) :: ftime_step         ! Forecast time step 
     integer(I4B) :: ntime_forecast     ! total # of forecast time step
     real(DP)     :: fincr              ! Forecast time increment
     integer(I4B) :: Forecast_time      ! Forecast total time
     integer(I4B) :: ftime_idx          ! forecast time idx count
     integer(I4B), Allocatable :: ftime_count(:)  
!
! History Variables
!
     integer(I4B) :: hist_time
     real(DP)     :: hist_incr          ! history time increment
!
! Grid Variables
!
     real(SP)     :: gridDesc(50)
     integer(I4B) :: nxpt
     integer(I4B) :: nypt
     integer(I4B) :: npts
     integer(I4B) :: nexp
     real(SP)     :: udef

!
!
!
     integer(I4B)                  :: nvar
     character(len=8), allocatable :: VarName(:)
!
!
!
     integer(I4B), allocatable :: Init_ModelID(:)
!
!    
!
     integer(I4B) :: cflag
     
  end type scamdec

  type(scamdec) :: scamtec


END MODULE scamtec_module
