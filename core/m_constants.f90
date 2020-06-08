module m_constants
   implicit none
   private

   !

   !precisao das variaveis

   integer, public, parameter :: I4 = SELECTED_INT_KIND( 9)  ! Kind for 32-bits Integer Numbers
   integer, public, parameter :: I8 = SELECTED_INT_KIND(14)  ! Kind for 64-bits Integer Numbers
   integer, public, parameter :: R4 = SELECTED_REAL_KIND( 6) ! Kind for 32-bits Real Numbers
   integer, public, parameter :: R8 = SELECTED_REAL_KIND(15) ! Kind for 64-bits Real Numbers

   !Logical Units 

!   integer,  public, parameter :: stderr = 0 ! Error Unit
!   integer,  public, parameter :: stdinp = 5 ! Input Unit
!   integer,  public, parameter :: stdout = 6 ! Output Unit

   !Undefine value

   real(kind=r4), public, parameter :: Undef = -1e12

   ! other parameters

   integer, public, parameter :: LongStr   = 1024
   integer, public, parameter :: NormalStr = 512
   integer, public, parameter :: shortStr  = 64
   integer, public, parameter :: smallStr  = 32
   integer, public, parameter :: tinyStr   = 16

end module
