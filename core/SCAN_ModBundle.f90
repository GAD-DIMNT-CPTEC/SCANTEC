!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAN_DataBundleMod -- Implements Meteorological Data for SCANTEC
!
! !DESCRIPTON:  
!              
!             
!                 
!\\
!\\
! !INTERFACE:
!

Module SCAN_DataBundleMod

!
! !USES:
!

   Implicit None
!
! !
!

!
! !PUBLIC MEMBER FUNCTIONS:
!

   Public :: SCAN_DataBundle_Create  ! Create a data bundle
   Public :: SCAN_DataBundle_Destroy ! Destroy a data bundle
   Public :: SCAN_DataBundle_Init    ! Initialize bundle resources 
   Public :: SCAN_DataBundle_Put     ! Put data on bundle
   Public :: SCAN_DataBundle_Get     ! Get data from bundle

! !REVISION HISTORY:
!
!   28 Apr 2016 - J. G. de Mattos -  Initial code.
!
!EOP
!-------------------------------------------------------------------------

! !PRIVATE ROUTINES:
!BOC

End Module
