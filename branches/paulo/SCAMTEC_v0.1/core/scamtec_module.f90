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
     integer(I4B) :: starting_time
     integer(I4B) :: ending_time
     integer(I4B) :: time_step
     integer(I4B) :: Forecast_time
     integer(I4B) :: ntime_forecast
     integer(I4B) :: ntime_steps
     integer(I4B) :: hist_time
     real(DP)     :: incr
     real(DP)     :: hist_incr

     real(SP)     :: gridDesc(50)
     integer(I4B) :: nxpt
     integer(I4B) :: nypt
     integer(I4B) :: npts
     integer(I4B) :: nexp
     real(SP)     :: udef

     integer(I4B) :: nvar

     integer(I4B), allocatable :: Init_ModelID(:)


  end type scamdec

  type(scamdec) :: scamtec


END MODULE scamtec_module
