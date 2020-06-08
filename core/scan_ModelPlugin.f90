MODULE scan_Modelplugin
  use scantec_module
  use bilinInterp
  use scan_readModel
  use m_inpak90
!  USE m_agcm
!  USE m_clima50yr
!  USE m_brams
  
  IMPLICIT NONE
  !BOP
  !
  !  !MODULE: scan_Modelplugin
  ! 
  !  !DESCRIPTION: 
  !   The code in this file provides values of indices used to 
  !   to register functions in the plugin modules
  !  
  !  The index definitions are simply a convention
  !  The user may change these options, and the scantec.conf 
  !  should be changed appropriately to ensure that the correct function
  !  is called at run time
  !
  !  This module contains, also, the definition of the functions used 
  !  for open the numerical models states to use for evaluatin an other 
  !  relevant computations using the models, corresponding to each of 
  !  the Model used in scantec.
  !
  !  !REVISION HISTORY: 
  !  25 Oct 2011    J. G. de Mattos  Initial Specification
  !
  !EOP

  PRIVATE
  !-------------------------------------------------------------------
  ! Available Models
  !-------------------------------------------------------------------

!  integer, public, parameter :: templateId           = 0  ! Template
!  integer, public, parameter :: AGCMId               = 1  ! AGCM/CPTEC
!  integer, public, parameter :: bramsId              = 2  ! BRAMS 5KM / CPTEC
!  integer, public, parameter :: clima50yrId          = 3  ! 50yr Climatology / CPTEC

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: scan_Models_Plugin  

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='scan_Modelplugin'

Contains
!  !-------------------------------------------------------------------
!  !BOP
!  !
!  ! !ROUTINE: scan_models_plugin
!  !  \label{scan_models_plugin}
!  !
!  ! !DESCRIPTION:
!  !
!  ! This is a custom-defined plugin point for introducing a new Model. 
!  ! The interface mandates that the following routines be implemented
!  ! and registered for each of the Model that is included in scantec.
!  !
!  ! !INTERFACE:
!    
!  SUBROUTINE scan_Models_Plugin_
!
!    !  !REVISION HISTORY: 
!    !  25 Oct 2011    J. G. de Mattos  Initial Specification
!    !
!    !------------------------------------------------------------------
!    !BOC
!    character(len=*),parameter :: myname_=myname//'::scan_Models_Plugin'
!
!    !------------------------------------------------------------------
!    ! External Functions to read models
!    !------------------------------------------------------------------
!    ! Template
!!    external template_init
!    external template_read     
!    ! AGCM/CPTEC
!!    external agcm_init
!!    external agcm_read
!    ! Eta/CPTEC
!!    external eta_init
!    external eta_read 
!
!#ifdef DEBUG
!    WRITE(6,'(     2A)')'Hello from ', myname_
!#endif
!
!    !------------------------------------------------------------------
!    ! Registering models
!    !------------------------------------------------------------------
!    ! Template
!!    call registermodelinit(templateId,template_init)
!    call registermodelread(templateId,template_read)
!    ! AGCM/CPTEC
!    call registermodelinit(AGCMId,agcm_init)
!    call registermodelread(AGCMId,agcm_read)
!    ! clima50yr/CPTEC
!    call registermodelinit(Clima50yrId,clima50yr_init)
!    call registermodelread(Clima50yrId,clima50yr_read)
!
!    ! BRAMS 5KM / CPTEC
!    call registermodelinit(bramsId,brams_init)
!    call registermodelread(bramsId,brams_read)
!    
!  END SUBROUTINE scan_models_plugin_
  !EOC
  !-------------------------------------------------------------------
  subroutine scan_models_plugin
    !  !REVISION HISTORY: 
    !  18 May 2020    J. G. de Mattos  Initial Specification
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::scan_Models_Plugin'
    real, pointer :: rlat(:) => null()
    real, pointer :: rlon(:) => null()
    real :: GDesc(200)
    integer, pointer :: xdim => null()
    integer, pointer :: ydim => null()
    integer :: i, j, k
    character(len=ShortStr), pointer :: mapping => null()

    scantec%currModel => scantec%FirstModel
    do while(associated(scantec%currModel))
    
       call readModelConf(scantec%currModel)

       ! compute weights to be used for interpolation
       rlat => getDimVec(scantec%currModel, 'ydim:')
       rlon => getDimVec(scantec%currModel, 'xdim:')

       xdim => getDimInfo(scantec%currModel, 'xdim:')
       ydim => getDimInfo(scantec%currModel, 'ydim:')
       mapping => getMapping(scantec%currModel, 'xdim:')

       if (i90_lcase(mapping) .eq. 'linear')then
          GDesc = 0
          GDesc( 1) = 0
          GDesc( 2) = xdim
          GDesc( 3) = ydim
          GDesc( 4) = rlat(1)
          GDesc( 5) = rlon(1)
          GDesc( 6) = 128
          GDesc( 7) = rlat(ydim)
          GDesc( 8) = rlon(xdim)
          GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)
          GDesc(10) = (rlat(ydim)-rlat(1))/(ydim-1)
       else
          ! For now support only Gaussian grid
          GDesc = 0
          GDesc( 1) =       4!     6    Data representation type (see Code table 6)
                                                !
                                                !-------------------------------------------------------------------------------
                                                !       Table 6
                                                ! 0 Latitude/longitude grid - equidistant cylindrical or Plate Carrée projection
                                                ! 1 Mercator projection
                                                ! 2 Gnomonic projection
                                                ! 3 Lambert conformal, secant or tangent, conic or bi-polar, projection
                                                ! 4 Gaussian latitude/longitude grid
                                                ! 5 Polar stereographic projection
                                                ! ...
                                                !
                                                !-------------------------------------------------------------------------------
                                                ! 7-32  Grid definition (according to data representation type - octet 6 above)
                                                !-------------------------------------------------------------------------------
                                                !
          GDesc( 2) = xdim                      !  7- 8    Ni - number of points along a parallel
          GDesc( 3) = ydim                      !  9-10    Nj - number of points along a meridian
          GDesc( 4) = rlat(1)                   ! 11-13    La1 - latitude of first grid point
          GDesc( 5) = rlon(1)                   ! 14-16    Lo1 - longitude of first grid point
      
                                                ! --------------------------------------------------------------
          GDesc( 6) =     128!    17    Resolution and component flags (see Code table 7)
                                                ! --------------------------------------------------------------
                                                !
                                                ! Bit No.   Value  Meaning
              	                                 !    1        0    Direction increments not given
                                                !             1    Direction increments given
                                                !
              	                                 !    2        0    Earth assumed spherical with radius 6367.47 km
                                                !             1    Earth assumed oblate spheroidal with size as 
                                                !                  determined by IAU in 1965 (6378.160 km, 6356.775 km, f=1/297.0)
                                                !
              	                                 !   3-4            Reserved
                                                !
                                                !    5        0    Resolved u- and v-components of vector quantities relative to easterly and northerly directions
                                                !             1    Resolved u- and v-components of vector quantities relative to the defined grid in the direction
                                                !                  of increasing x and y (or i and j) coordinates respectively
                                                !
                                                !   6-8       0    Reserved - set to zero
                                                !    
      
          GDesc( 7) = rlat(ydim)                ! 18-20    La2 - latitude of last grid point
          GDesc( 8) = rlon(xdim)                ! 21-23    Lo2 - longitude of last grid point
          GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)! 24-25    Di - i direction increment
          GDesc(10) = ydim/2.0                  ! 26-27    N - number of parallels between a pole and the equator
      
                                                ! --------------------------------------------------------------
          GDesc(11) =       0!    28	   Scanning mode (flags - see Code table 8)
                                                ! --------------------------------------------------------------
                                                !    WMO Code table 8 - Scanning mode
                                                !
                                                ! Bit No.   Value  Meaning
                                                !    1	     0    Points scan in +i direction
              	                                 !             1    Points scan in -i direction
                                                !    2        0    Points scan in -j direction
              	                                 !             1    Points scan in +j direction
                                                !    3        0    Adjacent points in i direction are consecutive
              	                                 !             1    Adjacent points in j direction are consecutive
                                                !  4-8        0	 Reserved
                                                !
                                                ! NOTES:
                                                !
                                                !   i direction: west to east along a parallel, or left to right along an X-axis.
                                                !   j direction: south to north along a meridian, or bottom to top along a Y-axis.
                                                !   -----------------------------------------------
                                                !
                                                ! Values to GDS(11)
                                                !  Value  Result
                                                !     0    0 0 0
                                                !    32    0 0 1
                                                !    64    0 1 0
                                                !    96    0 1 1
                                                !   128    1 0 0
                                                !   160    1 0 1
                                                !   192    1 1 0
                                                !   224    1 1 1
                                                !
      
          GDesc(12) =       0! 29-32    Set to zero (reserved)
                                                !
                                                !-------------------------------------------------------------------------------
                                                !
                                                ! 33-42 Extensions of grid definition for rotation or stretching of the coordinate system
                                                !        or Lambert conformal projection or Mercator projection
                                                !-------------------------------------------------------------------------------
                                                !
          GDesc(13) =       0! 33-35    Latitude of the southern pole in millidegrees (integer)
                                                !          Latitude of pole of stretching in millidegrees (integer)
          GDesc(14) =       0! 36-38    Longitude of the southern pole in millidegrees (integer)
                                                !          Longitude of pole of stretching in millidegrees (integer)
          GDesc(15) =       0! 39-42    Angle of rotation (represented in the same way as the reference value)
                                                !          Stretching factor (representation as for the reference value)
                                                !
                                                !-------------------------------------------------------------------------------
                                                !
                                                ! 33-44 Extensions of grid definition for space view perspective projection
                                                ! 33-52 Extensions of grid definition for stretched and rotated coordinate system
                                                !
                                                !-------------------------------------------------------------------------------
                                                !
          GDesc(16) =       0! 43-45    Latitude of pole of stretching in millidegrees (integer)
          GDesc(17) =       0! 46-48    Longitude of pole of stretching in millidegrees (integer)
          GDesc(18) =       0! 49-52    Stretching factor (representation as for the reference value)
      
                                                !-------------------------------------------------------------------------------
          GDesc(19) =       0! PV	List of vertical coordinate parameters 
                                                ! (length = NV x 4 octets); if present, then PL = 4NV + PV
          GDesc(20) =       0! PL	List of numbers of points in each row 
                                                ! (length = NROWS x 2 octets, where NROWS is the total number of rows defined within the grid description)
                                                !
                                                ! NOTES:
                                                !
                                                ! 1- Vertical coordinate parameters are used in association with hybrid vertical coordinate systems.
      
                                                ! 2- Hybrid systems, in the context, employ a means of representing vertical coordinates in terms
                                                !    of a mathematical combination of pressure and sigma coordinates. When used in conjunction with
                                                !    a surface pressure field and an appropriate mathematical expression, the vertical coordinate 
                                                !    parameters may be used to interpret the hybrid vertical coordinate.
                                                !
                                                ! 3- Each vertical coordinate parameter is represented in 4 octets, using the scheme for representing
                                                !    floating point numbers described in the Regulations.
                                                !
                                                !-------------------------------------------------------------------------------

       endif


       allocate(scantec%currModel%w11(xdim*ydim))
       allocate(scantec%currModel%w12(xdim*ydim))
       allocate(scantec%currModel%w21(xdim*ydim))
       allocate(scantec%currModel%w22(xdim*ydim))
       allocate(scantec%currModel%n11(xdim*ydim))
       allocate(scantec%currModel%n12(xdim*ydim))
       allocate(scantec%currModel%n21(xdim*ydim))
       allocate(scantec%currModel%n22(xdim*ydim))

       call bilinear_interp_init(GDesc, scantec%GridDesc, &
                                 scantec%currModel%w11, &
                                 scantec%currModel%w12, &
                                 scantec%currModel%w21, &
                                 scantec%currModel%w22, &
                                 scantec%currModel%n11, &
                                 scantec%currModel%n12, &
                                 scantec%currModel%n21, &
                                 scantec%currModel%n22  &
                                 )
!       write(*,'(A,8F9.4,8i6)')trim(scantec%currModel%Name_),&
!          minval(scantec%currModel%w11),maxval(scantec%currModel%w11),&
!          minval(scantec%currModel%w12),maxval(scantec%currModel%w12),&
!          minval(scantec%currModel%w21),maxval(scantec%currModel%w21),&
!          minval(scantec%currModel%w22),maxval(scantec%currModel%w22),&
!          minval(scantec%currModel%n11),maxval(scantec%currModel%n11),&
!          minval(scantec%currModel%n12),maxval(scantec%currModel%n12),&
!          minval(scantec%currModel%n21),maxval(scantec%currModel%n21),&
!          minval(scantec%currModel%n22),maxval(scantec%currModel%n22)
       deallocate(rlon)
       deallocate(rlat)
       scantec%currModel => scantec%currModel%next
    enddo    

  end subroutine scan_models_plugin


END MODULE scan_Modelplugin
