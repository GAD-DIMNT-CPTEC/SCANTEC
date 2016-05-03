!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_TQ062L028.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE m_TQ062L028

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
  type TQ062L028_type_dec 

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

  end type TQ062L028_type_dec

  type(TQ062L028_type_dec) :: TQ062L028_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: TQ062L028_read ! Function to read files from TQ062L028 model
  public :: TQ062L028_init ! Function to initilize weights to interpolate fields
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
  character(len=*),parameter :: myname='m_TQ062L028' 

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  TQ062L028_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE TQ062L028_init()

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
    character(len=*),parameter :: myname_=myname//'::TQ062L028_init'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    Allocate(TQ062L028_struc%gridDesc(50))

    call TQ062L028_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(TQ062L028_struc%rlat1(nx*ny))
    Allocate(TQ062L028_struc%rlon1(nx*ny))              
    Allocate(TQ062L028_struc%n111(nx*ny))
    Allocate(TQ062L028_struc%n121(nx*ny))
    Allocate(TQ062L028_struc%n211(nx*ny))
    Allocate(TQ062L028_struc%n221(nx*ny))
    Allocate(TQ062L028_struc%w111(nx*ny))
    Allocate(TQ062L028_struc%w121(nx*ny))
    Allocate(TQ062L028_struc%w211(nx*ny))
    Allocate(TQ062L028_struc%w221(nx*ny))

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(TQ062L028_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     TQ062L028_struc%rlat1,TQ062L028_struc%rlon1,      &
                                     TQ062L028_struc%n111,TQ062L028_struc%n121,        &
                                     TQ062L028_struc%n211,TQ062L028_struc%n221,        &
                                     TQ062L028_struc%w111,TQ062L028_struc%w121,        &
                                     TQ062L028_struc%w211,TQ062L028_struc%w221)


  END SUBROUTINE TQ062L028_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  TQ062L028_domain
!
! !DESCRIPTION: This routine initilize domain parameters of TQ062L028 model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE TQ062L028_domain()

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
    character(len=*),parameter :: myname_=myname//'::TQ062L028_domain'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    !                    http://rda.ucar.edu/docs/formats/grib/gribdoc/
    !
    !                    Grib Section 2 - Grid description section
    !

    TQ062L028_struc%gridDesc     = 0 

                                          !Octet No.	Contents
    TQ062L028_struc%gridDesc( 1) =       4!     6    Data representation type (see Code table 6)
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
    TQ062L028_struc%gridDesc( 2) =     192!  7- 8    Ni - number of points along a parallel
    TQ062L028_struc%gridDesc( 3) =      96!  9-10    Nj - number of points along a meridian
    TQ062L028_struc%gridDesc( 4) =  88.572! 11-13    La1 - latitude of first grid point
    TQ062L028_struc%gridDesc( 5) =     0.0! 14-16    Lo1 - longitude of first grid point

                                          ! --------------------------------------------------------------
    TQ062L028_struc%gridDesc( 6) =     128!    17    Resolution and component flags (see Code table 7)
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

    TQ062L028_struc%gridDesc( 7) = -88.572! 18-20    La2 - latitude of last grid point
    TQ062L028_struc%gridDesc( 8) = 358.125! 21-23    Lo2 - longitude of last grid point
    TQ062L028_struc%gridDesc( 9) =   1.875! 24-25    Di - i direction increment
    TQ062L028_struc%gridDesc(10) =      48! 26-27    N - number of parallels between a pole and the equator

                                          ! --------------------------------------------------------------
    TQ062L028_struc%gridDesc(11) =       0!    28	   Scanning mode (flags - see Code table 8)
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

    TQ062L028_struc%gridDesc(12) =       0! 29-32    Set to zero (reserved)
                                          !
                                          !-------------------------------------------------------------------------------
                                          !
                                          ! 33-42 Extensions of grid definition for rotation or stretching of the coordinate system
                                          !        or Lambert conformal projection or Mercator projection
                                          !-------------------------------------------------------------------------------
                                          !
    TQ062L028_struc%gridDesc(13) =       0! 33-35    Latitude of the southern pole in millidegrees (integer)
                                          !          Latitude of pole of stretching in millidegrees (integer)
    TQ062L028_struc%gridDesc(14) =       0! 36-38    Longitude of the southern pole in millidegrees (integer)
                                          !          Longitude of pole of stretching in millidegrees (integer)
    TQ062L028_struc%gridDesc(15) =       0! 39-42    Angle of rotation (represented in the same way as the reference value)
                                          !          Stretching factor (representation as for the reference value)
                                          !
                                          !-------------------------------------------------------------------------------
                                          !
                                          ! 33-44 Extensions of grid definition for space view perspective projection
                                          ! 33-52 Extensions of grid definition for stretched and rotated coordinate system
                                          !
                                          !-------------------------------------------------------------------------------
                                          !
    TQ062L028_struc%gridDesc(16) =       0! 43-45    Latitude of pole of stretching in millidegrees (integer)
    TQ062L028_struc%gridDesc(17) =       0! 46-48    Longitude of pole of stretching in millidegrees (integer)
    TQ062L028_struc%gridDesc(18) =       0! 49-52    Stretching factor (representation as for the reference value)

                                          !-------------------------------------------------------------------------------
    TQ062L028_struc%gridDesc(19) =       0! PV	List of vertical coordinate parameters 
                                          ! (length = NV x 4 octets); if present, then PL = 4NV + PV
    TQ062L028_struc%gridDesc(20) =       0! PL	List of numbers of points in each row 
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

    TQ062L028_struc%npts = TQ062L028_struc%gridDesc(2)*TQ062L028_struc%gridDesc(3)

  END SUBROUTINE TQ062L028_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  TQ062L028_read
!
! !DESCRIPTION: For a given file name, read fields from a TQ062L028 model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE TQ062L028_read(fname)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  
    character(len=*), intent(IN) :: fname ! File name of the TQ062L028 model

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
    character(len=*),parameter :: myname_=myname//'::TQ062L028_read'

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
    character(len=4), dimension(28) :: pds5
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


    pds5( 1:10) = (/'UMES','UMES','UMES','TEMP','TEMP','TEMP','TEMP','PSNM','AGPL','ZGEO'/)
    pds7( 1:10) = (/  925 ,  850 ,  500 ,  925 ,  850 ,  500 ,  250 ,  000 ,  000 ,  850 /)

    pds5(11:20) = (/'ZGEO','ZGEO','UVEL','UVEL','UVEL','VVEL','VVEL','VVEL','PREC','PRCV'/)
    pds7(11:20) = (/  500 ,  250 ,  850 ,  500 ,  250 ,  850 ,  500 ,  250 ,  000 ,  000 /)

    pds5(21:28) = (/'TEMS','Q02M','TP2M','USSL','UZRS','UZDS','U10M','V10M'/)
    pds7(21:28) = (/  000 ,  002 ,  002 ,  000 ,  000 ,  000 ,  010 ,  010 /)

  
    
    do i=1, len_trim(fname)	
		
		    if(trim(fname(i:i+6)) .eq. 'GPOSERA')then
			    dataini=trim(fname(i+7:i+16))
			    datafinal=trim(fname(i+17:i+26))
		    endif			
		
    enddo 
	   	
!   if(dataini .eq. datafinal)then
!	    y=size(pds7)-2
!   else
	    y=size(pds7)
!   endif


   inquire (file=trim(fname), exist=file_exists)
   if (file_exists) then

      grb%file = trim(fname)
      call OpenGrib(grb,iret)
      if (iret .ne. 0) then
         stat = iret
         return
      endif

      allocate(f(TQ062L028_struc%npts,size(pds7)))
      allocate(lb(TQ062L028_struc%npts,size(pds7)))
      lb = .true.

      do iv=1,y
         call ReadGrib(grb,pds5(iv),pds7(iv),f(:,iv))
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

    allocate(f2(TQ062L028_struc%npts,scamtec%nvar))
    allocate(lb2(TQ062L028_struc%npts,scamtec%nvar))


    do i=1,TQ062L028_struc%npts
       f2(i, 1) = tv(f(i,4),f(i,1)); lb2(i,1) = lb(i,1) ! Vtmp @ 925 hPa [K]
       f2(i, 2) = tv(f(i,5),f(i,2)); lb2(i,2) = lb(i,2) ! Vtmp @ 850 hPa [K]
       f2(i, 3) = tv(f(i,6),f(i,3)); lb2(i,3) = lb(i,3) ! Vtmp @ 500 hPa [K]  
    enddo
    
    f2(:, 4) = f(:, 5); lb2(:,4) = lb(:,5)              ! Absolute Temperature @ 850 hPa [K]
    f2(:, 5) = f(:, 6); lb2(:,5) = lb(:,6)              ! Absolute Temperature @ 500 hPa [K]
    f2(:, 6) = f(:, 7); lb2(:,6) = lb(:,7)              ! Absolute Temperature @ 250 hPa [K]
    

    f2(:, 7) = f(:, 8); lb2(:, 7) = lb(:,8)             ! PSNM [hPa]
    f2(:, 8) = f(:, 1)*1000.0 ; lb2(:, 8) = lb(:,1)     ! Umes @ 925 hPa [g/Kg]
    f2(:, 9) = f(:, 2)*1000.0 ; lb2(:, 9) = lb(:,2)     ! Umes @ 850 hPa [g/Kg]
    f2(:,10) = f(:, 3)*1000.0 ; lb2(:,10) = lb(:,3)     ! Umes @ 500 hPa [g/Kg]
    
    f2(:,11) = f(:, 9); lb2(:,11) = lb(:, 9)             ! Agpl @ 925 hPa [Kg/m2]
    f2(:,12) = f(:,10); lb2(:,12) = lb(:,10)             ! Zgeo @ 850 hPa [gpm]
    f2(:,13) = f(:,11); lb2(:,13) = lb(:,11)             ! Zgeo @ 500 hPa [gpm]
    f2(:,14) = f(:,12); lb2(:,14) = lb(:,12)             ! Zgeo @ 250 hPa [gpm]
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
    f2(:,24) = f(:,22)*1000.0; lb2(:,24) = lb(:,22)  ! SPECIFIC HUMIDITY AT 2-M FROM SURFACE   (G/KG)
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

       call interp_TQ062L028( kpds, TQ062L028_struc%npts,f2(:,iv),lb2(:,iv), scamtec%gridDesc,&
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

  END SUBROUTINE TQ062L028_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_TQ062L028
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_TQ062L028( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)

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
    character(len=*),parameter :: myname_=myname//'::interp_TQ062L028'

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
                         TQ062L028_struc%npts,nxpt*nypt,          &
                         TQ062L028_struc%rlat1, TQ062L028_struc%rlon1, &
                         TQ062L028_struc%w111, TQ062L028_struc%w121,   &
                         TQ062L028_struc%w211, TQ062L028_struc%w221,   &
                         TQ062L028_struc%n111, TQ062L028_struc%n121,   &
                         TQ062L028_struc%n211, TQ062L028_struc%n221,scamtec%udef,iret)

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

  END SUBROUTINE interp_TQ062L028
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE m_TQ062L028
