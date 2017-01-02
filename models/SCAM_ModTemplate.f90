!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: SCAM_ModTemplate
!
! !DESCRIPTON:  This is a dummy placehold module implementation of a 
!               Atmospheric model, to be used as a template for new 
!               implementations. 
!                 
!\\
!\\
! !INTERFACE:
!

MODULE SCAM_ModTemplate

!
! !USES:
!
!  USE scamtec_module                ! SCAMTEC types
!  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
!  USE interp_mod                    ! Interpolation module
!  USE read_grib                     ! Generic Module to read GRIB 1 files
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters


  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC TYPES:  
!
  type ModTemplate_type_dec 

     integer                :: npts
     real, allocatable      :: gridDesc(:)
     real, allocatable      :: rlat1(:)
     real, allocatable      :: rlon1(:)
     integer, allocatable   :: n111(:)
     integer, allocatable   :: n121(:)
     integer, allocatable   :: n211(:)
     integer, allocatable   :: n221(:)
     real, allocatable      :: w111(:),w121(:)
     real, allocatable      :: w211(:),w221(:)

  end type ModTemplate_type_dec

  type(ModTemplate_type_dec) :: ModTemplate_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: ModTemplate_read ! Function to read files from ModTemplate model
  public :: ModTemplate_init ! Function to initilize weights to interpolate fields
!
!
! !REVISION HISTORY:
!  03 May 2012 - J. G. de Mattos - Initial Version
!
!
! !SEE ALSO:
!   
!
!EOP
!-----------------------------------------------------------------------------!
!
  character(len=*),parameter :: myname='SCAM_ModTemplate' 

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  ModTemplate_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE ModTemplate_init()

!
!
! !REVISION HISTORY: 
!  03 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
  
    IMPLICIT NONE

    character(len=*),parameter :: myname_=myname//'::ModTemplate_init'

    !
    !  0. Hello
    !

    WRITE(stdout,'(  1x,2A)')'Hello from ', myname_
    
    !
    
    WRITE(stdout,'(  1x,1A)')'├── Allocate Grid Structure'

!    Allocate(ModTemplate_struc%gridDesc(50))

    call ModTemplate_domain()

!    nx = int(scamtec%gridDesc(2))
!    ny = int(scamtec%gridDesc(3))

!    Allocate(ModTemplate_struc%rlat1(nx*ny))
!    Allocate(ModTemplate_struc%rlon1(nx*ny))              
!    Allocate(ModTemplate_struc%n111(nx*ny))
!    Allocate(ModTemplate_struc%n121(nx*ny))
!    Allocate(ModTemplate_struc%n211(nx*ny))
!    Allocate(ModTemplate_struc%n221(nx*ny))
!    Allocate(ModTemplate_struc%w111(nx*ny))
!    Allocate(ModTemplate_struc%w121(nx*ny))
!    Allocate(ModTemplate_struc%w211(nx*ny))
!    Allocate(ModTemplate_struc%w221(nx*ny))


    WRITE(stdout,'(  1x,1A)')'└── Initialize Routines to Interpolate Model'

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

!    call bilinear_interp_input(ModTemplate_struc%gridDesc, scamtec%gridDesc,        &
!                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
!                                     ModTemplate_struc%rlat1,ModTemplate_struc%rlon1,      &
!                                     ModTemplate_struc%n111,ModTemplate_struc%n121,        &
!                                     ModTemplate_struc%n211,ModTemplate_struc%n221,        &
!                                     ModTemplate_struc%w111,ModTemplate_struc%w121,        &
!                                     ModTemplate_struc%w211,ModTemplate_struc%w221)


  END SUBROUTINE ModTemplate_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  ModTemplate_domain
!
! !DESCRIPTION: This routine initilize domain parameters of ModTemplate model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE ModTemplate_domain()

!
!
! !REVISION HISTORY: 
!  03 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
  
    IMPLICIT NONE
    character(len=*),parameter :: myname_=myname//'::ModTemplate_domain'

    !
    !  0. Hello
    !

    WRITE(stdout,'(  1x,2A)')'Hello from ', myname_

    WRITE(stdout,'(  1x,1A)')'├── Configure Model Domain!'
    

!    ModTemplate_struc%gridDesc = -1 


  END SUBROUTINE ModTemplate_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  ModTemplate_read
!
! !DESCRIPTION: For a given file name, read fields from a ModTemplate model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE ModTemplate_read(fname)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  
    character(len=*), intent(IN) :: fname ! File name of the ModTemplate model

!
!
! !REVISION HISTORY: 
!  03 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    character(len=*),parameter :: myname_=myname//'::ModTemplate_read'

print*,fname

    !
    !  0. Hello
    !

    WRITE(stdout,'(  1x,2A)')'Hello from ', myname_
    WRITE(stdout,'(  1x,2A)')'├── Open File ::', trim(fname)

    WRITE(stdout,'(  1x,1A)')'├── Read Fields ...'

    WRITE(stdout,'(  1x,1A)')'├── Interpolates a givem field to the SCAMTEC domain ...'
 
    WRITE(stdout,'(  1x,1A)')'└── Transfer fields to SCAMTEC ...'

!    scamdata(1)%tmpfield(:,:) = READED FIELD



  END SUBROUTINE ModTemplate_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_ModTemplate
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_ModTemplate( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)

!
! !INPUT PARAMETERS:
!

    integer, intent(in)   :: kpds(:)     ! grid deconding array information
    integer, intent(in)   :: npts        ! number of points in the input grid
    real, intent(out)     :: f(:)        ! input field to be interpolated
    logical*1, intent(in) :: lb(:)       ! input bitmap
    real, intent(in)      :: gridDesc(:) ! array description of the SCAMTEC grid
    integer, intent(in)   :: nxpt        ! number of columns (in the east-west dimension) in the SCAMTEC grid
    integer, intent(in)   :: nypt        ! number of rows (in the north-south dimension) in the SCAMTEC grid
!
! !OUTPUT PARAMETERS:
!
!    real, intent(out)     :: varfield(:,:) ! output interpolated field
    real, intent(out)     :: field1d(:)
 ! output interpolated field

    integer, intent(out)  :: iret          ! error code

!
! !REVISION HISTORY: 
!  03 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

!    real, dimension(nxpt*nypt) :: field1d
    logical*1, dimension(nxpt,nypt) :: lo

    integer :: ip, ipopt(20),ibi,km
    integer :: ibo
    integer :: i,j,k
    character(len=*),parameter :: myname_=myname//'::interp_ModTemplate'

    !
    ! DEBUG print
    !

    WRITE(stdout,'(     2A)')'Hello from ', myname_

    iret  = 0
!    ip    = 0
!    ipopt = 0
!    km    = 1
!    ibi   = 1
!    lo    = .true.

!    call bilinear_interp(gridDesc,ibi,lb,f,ibo,lo,field1d,   &
!                         ModTemplate_struc%npts,nxpt*nypt,          &
!                         ModTemplate_struc%rlat1, ModTemplate_struc%rlon1, &
!                         ModTemplate_struc%w111, ModTemplate_struc%w121,   &
!                         ModTemplate_struc%w211, ModTemplate_struc%w221,   &
!                         ModTemplate_struc%n111, ModTemplate_struc%n121,   &
!                         ModTemplate_struc%n211, ModTemplate_struc%n221,scamtec%udef,iret)

    if (iret.ne.0)then
       call perr(myname_,'bilinear_interp ( ... ) ',iret)
       return
    endif


  END SUBROUTINE interp_ModTemplate
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE SCAM_ModTemplate
