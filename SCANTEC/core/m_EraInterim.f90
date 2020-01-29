!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_EraInterim.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE m_EraInterim

!
! !USES:
!
  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE interp_mod                    ! Interpolation module
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE SCAM_MetForm                  ! Module to conversion of meteorological variables
  USE read_grib


  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC TYPES:  
!
  type EraInterim_type_dec 

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

  end type EraInterim_type_dec

  type(EraInterim_type_dec) :: EraInterim_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: EraInterim_read ! Function to read files from EraInterim model
  public :: EraInterim_init ! Function to initilize weights to interpolate fields
!
!
! !REVISION HISTORY:
!  03 May 2012 - J. G. de Mattos - Initial Version
!  06 May 2012 - J. G. de Mattos - Include new fields read
!  17 Oct 2012 - J. G. de Mattos - change UMES to g/kg
!  20 Feb 2013 - J. G. de Mattos - include SCAM_MetForm.f90 
!                                - and use it to make conversions
!
!
!
! !SEE ALSO:
!   
!
!EOP
!-----------------------------------------------------------------------------!
!
  character(len=*),parameter :: myname='m_EraInterim' 

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  EraInterim_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE EraInterim_init()

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
    integer :: nx, ny
    character(len=*),parameter :: myname_=myname//'::EraInterim_init'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    Allocate(EraInterim_struc%gridDesc(50))

    call EraInterim_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(EraInterim_struc%rlat1(nx*ny))
    Allocate(EraInterim_struc%rlon1(nx*ny))              
    Allocate(EraInterim_struc%n111(nx*ny))
    Allocate(EraInterim_struc%n121(nx*ny))
    Allocate(EraInterim_struc%n211(nx*ny))
    Allocate(EraInterim_struc%n221(nx*ny))
    Allocate(EraInterim_struc%w111(nx*ny))
    Allocate(EraInterim_struc%w121(nx*ny))
    Allocate(EraInterim_struc%w211(nx*ny))
    Allocate(EraInterim_struc%w221(nx*ny))

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(EraInterim_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     EraInterim_struc%rlat1,EraInterim_struc%rlon1,      &
                                     EraInterim_struc%n111,EraInterim_struc%n121,        &
                                     EraInterim_struc%n211,EraInterim_struc%n221,        &
                                     EraInterim_struc%w111,EraInterim_struc%w121,        &
                                     EraInterim_struc%w211,EraInterim_struc%w221)


  END SUBROUTINE EraInterim_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  EraInterim_domain
!
! !DESCRIPTION: This routine initilize domain parameters of EraInterim model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE EraInterim_domain()

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
    character(len=*),parameter :: myname_=myname//'::EraInterim_domain'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    EraInterim_struc%gridDesc     = 0 

!    EraInterim_struc%gridDesc( 1) = 0         !Input grid type (4=Gaussian)
!    EraInterim_struc%gridDesc( 2) = 480       !Number of points on a lat circle
!    EraInterim_struc%gridDesc( 3) = 241       !Number of points on a meridian
!    EraInterim_struc%gridDesc( 4) = 90.00000  !Latitude of origin
!    EraInterim_struc%gridDesc( 5) = 0.0       !Longitude of origin
!    EraInterim_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
!                                        !(recall that 10000000 = 128), Table 7
!    EraInterim_struc%gridDesc( 7) = -90.00000 !Latitude of extreme point
!    EraInterim_struc%gridDesc( 8) = 359.250   !Longitude of extreme point
!    EraInterim_struc%gridDesc( 9) = 0.75       !N/S direction increment
!    EraInterim_struc%gridDesc(10) = 0.75       !(Gaussian) # lat circles pole-equator
!    EraInterim_struc%gridDesc(20) = 255  

    EraInterim_struc%gridDesc( 1) =      4 
    EraInterim_struc%gridDesc( 2) =    192 
    EraInterim_struc%gridDesc( 3) =     96 
    EraInterim_struc%gridDesc( 4) = 88.572 
    EraInterim_struc%gridDesc( 5) =    0.0 
    EraInterim_struc%gridDesc( 6) =    128 
                                           
    EraInterim_struc%gridDesc( 7) = -88.572
    EraInterim_struc%gridDesc( 8) = 358.125
    EraInterim_struc%gridDesc( 9) =   1.875
    EraInterim_struc%gridDesc(10) =      48
    EraInterim_struc%gridDesc(11) =       0

    EraInterim_struc%npts = EraInterim_struc%gridDesc(2)*EraInterim_struc%gridDesc(3)

  END SUBROUTINE EraInterim_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  EraInterim_read
!
! !DESCRIPTION: For a given file name, read fields from a EraInterim model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE EraInterim_read(fname)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  
    character(len=*), intent(IN) :: fname ! File name of the EraInterim model

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
    character(len=*),parameter :: myname_=myname//'::EraInterim_read'

    integer :: iret, jret, gbret, stat
    logical :: file_exists
    integer :: jpds(200),jgds(200),gridDesc(200),kpds(200)
    integer :: lugb
    real    :: lubi
    real    :: kf,k
    integer :: i,j,iv,y
    integer :: npts
    integer :: nx
    integer :: ny
    character(len=5), dimension(28) :: pds5
    integer, dimension(28) :: pds7
    logical*1, dimension(:,:), allocatable :: lb, lb2
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:),   allocatable :: varfield
    character(10) :: dataini, datafinal 
    type(grib) :: grb
    real, allocatable :: fld(:,:)


    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
    WRITE(stdout,'( A,1X,A)')'Open File ::', trim(fname)
#endif

    !
    !
    !

    pds5( 1:10) = (/'    Q','    Q','    Q','    T','    T','    T','    T','  MSL','  PWC','    Z'/)
    pds7( 1:10) = (/   925 ,   850 ,   500 ,   925 ,   850 ,   500 ,   250 ,   000 ,   000 ,   850 /)

    pds5(11:20) = (/'    Z','    Z','    U','    U','    U','    V','    V','    V','   TP','   CP'/)
    pds7(11:20) = (/   500 ,   250 ,   850 ,   500 ,   250 ,   850 ,   500 ,   250 ,   000 ,   000 /)

    pds5(21:28) = (/'  SKT','   2D','   2T','SWVL1','SWVL2','SWVL3','  10U','  10V'/)
    pds7(21:28) = (/   000 ,   000 ,   000 ,   007 ,  1820 ,  7268 ,   000 ,   000 /)

    y=size(pds7)


   inquire (file=trim(fname), exist=file_exists)
   if (file_exists) then

      grb%file = trim(fname)
      call OpenGrib(grb,iret)
      if (iret .ne. 0) then
         stat = iret
         return
      endif

      allocate(f(EraInterim_struc%npts,size(pds7)))
      allocate(lb(EraInterim_struc%npts,size(pds7)))
      lb = .true.

      do iv=1,y
         call ReadGrib(grb,trim(adjustl(pds5(iv))),pds7(iv),f(:,iv))
         where(f(:,iv).eq.grb%undef) lb(:,iv) = .false.
      enddo

      call CloseGrib(grb)

   else
      stat = -1

      call perr(myname_,'File Not Found: '//trim(fname),stat)
      return

   endif

 
    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !

    allocate(f2(EraInterim_struc%npts,scamtec%nvar))
    allocate(lb2(EraInterim_struc%npts,scamtec%nvar))


    do i=1,EraInterim_struc%npts
       f2(i, 1) = tv(f(i,4),f(i,1)); lb2(i,1) = lb(i,1) ! Vtmp @ 925 hPa [K]
       f2(i, 2) = tv(f(i,5),f(i,2)); lb2(i,2) = lb(i,2) ! Vtmp @ 850 hPa [K]
       f2(i, 3) = tv(f(i,6),f(i,3)); lb2(i,3) = lb(i,3) ! Vtmp @ 500 hPa [K]  
    enddo
    
    f2(:, 4) = f(:, 5); lb2(:,4) = lb(:,5)              ! Absolute Temperature @ 850 hPa [K]
    f2(:, 5) = f(:, 6); lb2(:,5) = lb(:,6)              ! Absolute Temperature @ 500 hPa [K]
    f2(:, 6) = f(:, 7); lb2(:,6) = lb(:,7)              ! Absolute Temperature @ 250 hPa [K]
    

    f2(:, 7) = f(:, 8)/100.0; lb2(:, 7) = lb(:,8)             ! PSNM [hPa]
    f2(:, 8) = f(:, 1); lb2(:, 8) = lb(:,1)     ! Umes @ 925 hPa [g/Kg]
    f2(:, 9) = f(:, 2); lb2(:, 9) = lb(:,2)     ! Umes @ 850 hPa [g/Kg]
    f2(:,10) = f(:, 3); lb2(:,10) = lb(:,3)     ! Umes @ 500 hPa [g/Kg]
    
    f2(:,11) = f(:, 9); lb2(:,11) = lb(:, 9)             ! Agpl @ 925 hPa [Kg/m2]
    f2(:,12) = f(:,10)/9.8; lb2(:,12) = lb(:,10)             ! Zgeo @ 850 hPa [gpm]
    f2(:,13) = f(:,11)/9.8; lb2(:,13) = lb(:,11)             ! Zgeo @ 500 hPa [gpm]
    f2(:,14) = f(:,12)/9.8; lb2(:,14) = lb(:,12)             ! Zgeo @ 250 hPa [gpm]
    f2(:,15) = f(:,13); lb2(:,15) = lb(:,13)             ! Uvel @ 850 hPa [m/s]
    f2(:,16) = f(:,14); lb2(:,16) = lb(:,14)             ! Uvel @ 500 hPa [m/s]
    f2(:,17) = f(:,15); lb2(:,17) = lb(:,15)             ! Uvel @ 250 hPa [m/s]
    f2(:,18) = f(:,16); lb2(:,18) = lb(:,16)             ! Vvel @ 850 hPa [m/s]
    f2(:,19) = f(:,17); lb2(:,19) = lb(:,17)             ! Vvel @ 500 hPa [m/s]
    f2(:,20) = f(:,18); lb2(:,20) = lb(:,18)             ! Vvel @ 250 hPa [m/s]
           
!    if (y .eq. size(pds5)-2)then
!    f2(:,21) = 0.0; lb2(:,21) = .false.             ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]     
!    f2(:,22) = 0.0; lb2(:,22) = .false.             ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]   
!    else
    f2(:,21) = f(:,19); lb2(:,21) = lb(:,19)             ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]     
    f2(:,22) = f(:,20); lb2(:,22) = lb(:,20)             ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]   
!    endif

    f2(:,23) = f(:,21); lb2(:,23) = lb(:,21)  ! SURFACE ABSOLUTE TEMPERATURE [K]               

    do i=1,EraInterim_struc%npts
       f2(:,24) = q(101325.0,f(i,22)); lb2(:,24) = lb(:,22)  ! SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (G/KG)
    enddo

    f2(:,25) = f(:,23); lb2(:,25) = lb(:,23)  ! TEMPERATURE AT 2-M FROM SURFACE         (K)    
    f2(:,26) = f(:,24); lb2(:,26) = lb(:,24)  ! SOIL WETNESS OF SURFACE                 (0-1)  
    f2(:,27) = f(:,25); lb2(:,27) = lb(:,25)  ! SOIL WETNESS OF ROOT ZONE               (0-1)  
    f2(:,28) = f(:,26); lb2(:,28) = lb(:,26)  ! SOIL WETNESS OF DRAINAGE ZONE           (0-1)  
    f2(:,29) = f(:,27); lb2(:,29) = lb(:,27)  ! 10 METRE U-WIND COMPONENT               (M/S)  
    f2(:,30) = f(:,28); lb2(:,30) = lb(:,28)  ! 10 METRE V-WIND COMPONENT               (M/S)  


    DeAllocate(lb)
    DeAllocate(f)

    !
    ! padronizando pontos com undef
    !

    where(.not.lb2) f2 = scamtec%udef

    !
    ! Interpolando para a grade do SCAMTEC
    !

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx*ny))

    DO iv=1,scamtec%nvar

       call interp_EraInterim( kpds, EraInterim_struc%npts,f2(:,iv),lb2(:,iv), scamtec%gridDesc,&
                         scamtec%nxpt,scamtec%nypt, varfield, iret)    
#ifdef DEBUG
       write(*,*)trim(scamtec%VarName(iv)),&
                 minval(f2(:,iv),mask=f2(:,iv).ne.scamtec%udef),maxval(f2(:,iv),mask=f2(:,iv).ne.scamtec%udef),&
                 minval(varfield,mask=varfield.ne.scamtec%udef),maxval(varfield,mask=varfield.ne.scamtec%udef)
#endif

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !

       scamdata(1)%tmpfield(:,iv) = varfield(:)


    Enddo


    DeAllocate(varfield)

  END SUBROUTINE EraInterim_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_EraInterim
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_EraInterim( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)

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
    character(len=*),parameter :: myname_=myname//'::interp_EraInterim'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    ip    = 0
    ipopt = 0
    km    = 1
    ibi   = 1
    lo    = .true.

    call bilinear_interp(gridDesc,ibi,lb,f,ibo,lo,field1d,   &
                         EraInterim_struc%npts,nxpt*nypt,          &
                         EraInterim_struc%rlat1, EraInterim_struc%rlon1, &
                         EraInterim_struc%w111, EraInterim_struc%w121,   &
                         EraInterim_struc%w211, EraInterim_struc%w221,   &
                         EraInterim_struc%n111, EraInterim_struc%n121,   &
                         EraInterim_struc%n211, EraInterim_struc%n221,scamtec%udef,iret)

    if (iret.ne.0)then
       call perr(myname_,'bilinear_interp ( ... ) ',iret)
       return
    endif

!    k = 0
!    do j = 1, nypt
!       do i = 1, nxpt
!          varfield(i,j) = field1d(i+k)
!       enddo
!       k = k + nxpt
!    enddo

  END SUBROUTINE interp_EraInterim
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE m_EraInterim
