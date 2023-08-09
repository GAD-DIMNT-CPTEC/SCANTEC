module scanTimeMod
   use m_constants
   implicit none

   type time
      integer(kind = i4) :: ymd
      integer(kind = i4) :: hms
      integer(kind = i4) :: stp 
   end type

   type scanTime
      type(time) :: anl
      type(time) :: fct
   end type

   contains
   function _init_(startTime, endTime)

end module
