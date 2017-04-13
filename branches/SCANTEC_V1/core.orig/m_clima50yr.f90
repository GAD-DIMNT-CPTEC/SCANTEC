!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_clima50yr.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!


MODULE m_clima50yr

!
! !USES:
!
  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE interp_mod                    ! Interpolation module
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE SCAM_MetForm                  ! Module to convert meteorological variables

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC TYPES:  
!  
  type clima50yr_type_dec 

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

  end type clima50yr_type_dec

  type(clima50yr_type_dec) :: clima50yr_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!

  public :: clima50yr_read ! Function to read files from clima50yr model
  public :: clima50yr_init ! Function to initilize weights to interpolate fields
!
!
! !REVISION HISTORY:
!  06 May 2012 - J. G. de Mattos - Initial Version
!  18 Oct 2012 - J. G. de Mattos - change UMES to g/kg
!  20 Feb 2013 - J. G. de Mattos - include SCAM_MetForm.f90 
!                                - and use it to make conversions
! !SEE ALSO:
!   
!
!EOP
!-----------------------------------------------------------------------------!
!

  character(len=*),parameter :: myname='m_clima50yr'

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  clima50yr_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE clima50yr_init()
!
!
! !REVISION HISTORY: 
!  06 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!
    IMPLICIT NONE
    integer :: nx, ny

    character(len=*),parameter :: myname_=myname//'::clima50yr_init'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    Allocate(clima50yr_struc%gridDesc(50))

    call clima50yr_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(clima50yr_struc%rlat1(nx*ny))
    Allocate(clima50yr_struc%rlon1(nx*ny))              
    Allocate(clima50yr_struc%n111(nx*ny))
    Allocate(clima50yr_struc%n121(nx*ny))
    Allocate(clima50yr_struc%n211(nx*ny))
    Allocate(clima50yr_struc%n221(nx*ny))
    Allocate(clima50yr_struc%w111(nx*ny))
    Allocate(clima50yr_struc%w121(nx*ny))
    Allocate(clima50yr_struc%w211(nx*ny))
    Allocate(clima50yr_struc%w221(nx*ny))

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(clima50yr_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     clima50yr_struc%rlat1,clima50yr_struc%rlon1,      &
                                     clima50yr_struc%n111,clima50yr_struc%n121,        &
                                     clima50yr_struc%n211,clima50yr_struc%n221,        &
                                     clima50yr_struc%w111,clima50yr_struc%w121,        &
                                     clima50yr_struc%w211,clima50yr_struc%w221)


  END SUBROUTINE clima50yr_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  clima50yr_domain
!
! !DESCRIPTION: This routine initilize domain parameters of clima50yr model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE clima50yr_domain()
!
!
! !REVISION HISTORY: 
!  06 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

    IMPLICIT NONE
    character(len=*),parameter :: myname_=myname//'::clima50yr_domain'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif


    clima50yr_struc%gridDesc     = 0 

    clima50yr_struc%gridDesc( 1) = 4         !Input grid type (4=Gaussian)
    clima50yr_struc%gridDesc( 2) = 192       !Number of points on a lat circle
    clima50yr_struc%gridDesc( 3) = 96        !Number of points on a meridian
    clima50yr_struc%gridDesc( 4) = 88.5722   !Latitude of origin
    clima50yr_struc%gridDesc( 5) = 0.0       !Longitude of origin
    clima50yr_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                             !(recall that 10000000 = 128), Table 7
    clima50yr_struc%gridDesc( 7) = -88.5722  !Latitude of extreme point
    clima50yr_struc%gridDesc( 8) = -1.875    !Longitude of extreme point
    clima50yr_struc%gridDesc( 9) = 1.875     !N/S direction increment
    clima50yr_struc%gridDesc(10) =  48       !(Gaussian) # lat circles pole-equator
    clima50yr_struc%gridDesc(20) = 0.0  

    clima50yr_struc%npts = clima50yr_struc%gridDesc(2)*clima50yr_struc%gridDesc(3)

  END SUBROUTINE clima50yr_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  clima50yr_read
!
! !DESCRIPTION: For a given file name, read fields from a clima50yr model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE clima50yr_read(fname)
!
! !USES:
!
  USE m_ioutil, only : opnieee, clsieee

    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  
    character(len=*), intent(IN) :: fname ! File name of the clima50yr model
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
    character(len=*),parameter :: myname_=myname//'::clima50yr_read'

    integer :: ferror
    integer :: iret,lugb
    logical :: file_exists
    integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
    real :: lubi,kf
    integer :: i, j, k
    integer :: iv, rc
    integer :: npts
    integer :: nx
    integer :: ny
    integer, dimension(18) :: pds5, pds7
    logical*1, dimension(:,:), allocatable :: lb, lb2
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:), allocatable :: varfield

    !
    ! Parametros para ler os dados do arquivo de Climatologia
    !

    integer :: NV1lev = 43 ! Numero de Variaveis com 1 Nivel
    integer :: NLev   = 18 ! Numero de Niveis
    real    :: undef  = 1e+20

    !
    ! Variaveis temporarias utilizadas para conversao de umidades e variaveis
    !
    
!    real, allocatable, dimension(:) :: ee, es, rv, qq

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

    lugb = 10
    lubi = 0
    j    = 0
    jpds = -1

    !          1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18
    !          T   T   T   T  rh  rh  rh   P   A   Z   Z   Z   U   U   U   V   V   V 
    !        925 850 500 250 925 850 500 000 000 850 500 250 850 500 250 850 500 250 
    pds5 = (/ 49, 49, 49, 49, 50, 50, 50, 10, 18, 48, 48, 48, 44, 44, 44, 45, 45, 45/) !parameter
    pds7 = (/002,003,006,009,002,003,006,000,000,003,006,009,003,006,009,003,006,009/) !htlev2
     
    allocate(lb2(clima50yr_struc%npts,scamtec%nvar))
    allocate(lb(clima50yr_struc%npts,size(pds5)))
    allocate(f(clima50yr_struc%npts,size(pds5)))
    lb = .true.

    inquire (file=trim(fname), exist=file_exists)
    if (file_exists) then 

       jpds(5) = pds5(1)
       jpds(7) = pds7(1)
       
        open (unit=lugb, file=trim(fname),form='unformatted',access='direct',recl=192*96*4,status='unknown',iostat=iret)

       if(iret.eq.0) then

          do iv=1,size(pds5)
             rc = pds5(iv) + pds7(iv)+                        &
                  max0((pds5(iv)-(NV1Lev+1)),0)*NLev -        &
                  max0((pds5(iv)-NV1Lev),0) +                 &
                  max0(1-mod(pds5(iv),NV1Lev+1),0)

             read(lugb,rec=rc)f(:,iv)
             where(f(:,iv).eq.undef) lb(:,iv) = .false.

          enddo

       else
          gbret = 99
          call perr(myname_,'baopenr("'//   &
                    trim(fname)//'")',iret      )
          return          
       endif

       call clsieee(lugb,jret)
       if(jret.ne.0) then
         call perr(myname_,'deallocate()',jret)
         return
       endif

    else

       ferror = 0
       deallocate(f)
       deallocate(lb)
       
       call perr(myname_,'File Not Found: '//trim(fname),ferror)
       return

    endif


    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !

    allocate(f2(clima50yr_struc%npts,scamtec%nvar))

    do i=1,clima50yr_struc%npts
       f2(i,1) = tv(f(i,1)-273.16,f(i,5),92500.0) + 273.16 ; lb2(i, 1) = lb(i,1)! Vtmp @ 925 hPa [K]
       f2(i,2) = tv(f(i,2)-273.16,f(i,6),85000.0) + 273.16 ; lb2(i, 2) = lb(i,2)! Vtmp @ 850 hPa [K]
       f2(i,3) = tv(f(i,3)-273.16,f(i,7),50000.0) + 273.16 ; lb2(i, 3) = lb(i,3)! Vtmp @ 500 hPa [K]
       f2(i,8)  = q(92500.0,f(i,1)-273.16,f(i,5)) * 1000.00; lb2(i, 8) = lb(i,5) ! Umes @ 925 hPa [g/Kg]
       f2(i,9)  = q(92500.0,f(i,2)-273.16,f(i,6)) * 1000.00; lb2(i, 9) = lb(i,6) ! Umes @ 850 hPa [g/Kg]
       f2(i,10) = q(92500.0,f(i,3)-273.16,f(i,7)) * 1000.00; lb2(i,10) = lb(i,7) ! Umes @ 500 hPa [g/Kg]
    enddo
    
    f2(:, 4) = f(:, 2); lb2(:, 4) = lb(:, 2)                          ! Absolute Temperature @ 850 hPa [K]             4 paulo dias
    f2(:, 5) = f(:, 3); lb2(:, 5) = lb(:, 3)                          ! Absolute Temperature @ 500 hPa [K]             5 paulo dias
    f2(:, 6) = f(:, 4); lb2(:, 6) = lb(:, 4)                          ! Absolute Temperature @ 250 hPa [K]             6 paulo dias
    	
    f2(:, 7) = f(:, 8); lb2(:, 7) = lb(:, 8)                          ! PSNM [hPa]
    f2(:,11) = f(:, 9); lb2(:,11) = lb(:, 9)                          ! Agpl @ 925 hPa [Kg/m2]
    f2(:,12) = f(:,10); lb2(:,12) = lb(:,10)                          ! Zgeo @ 850 hPa [gpm]
    f2(:,13) = f(:,11); lb2(:,13) = lb(:,11)                          ! Zgeo @ 500 hPa [gpm]
    f2(:,14) = f(:,12); lb2(:,14) = lb(:,12)                          ! Zgeo @ 250 hPa [gpm]
    f2(:,15) = f(:,13); lb2(:,15) = lb(:,13)                          ! Uvel @ 850 hPa [m/s]
    f2(:,16) = f(:,14); lb2(:,16) = lb(:,14)                          ! Uvel @ 500 hPa [m/s]
    f2(:,17) = f(:,15); lb2(:,17) = lb(:,15)                          ! Uvel @ 250 hPa [m/s]
    f2(:,18) = f(:,16); lb2(:,18) = lb(:,16)                          ! Vvel @ 850 hPa [m/s]
    f2(:,19) = f(:,17); lb2(:,19) = lb(:,17)                          ! Vvel @ 500 hPa [m/s]
    f2(:,20) = f(:,18); lb2(:,20) = lb(:,18)                          ! Vvel @ 250 hPa [m/s]
    DeAllocate(f)

    !
    ! invertendo y
    !

    allocate(f(clima50yr_struc%npts,scamtec%nvar))
    

    nx = int(clima50yr_struc%gridDesc(2))
    ny = int(clima50yr_struc%gridDesc(3))

    k = 0
    do j = 1, ny
       do i = 1, nx
          f(i+(nx*ny)-nx-k,:) = f2(i+k,:)
       enddo
       k = k + nx
    enddo

    deallocate(f2)

    !
    ! Interpolando para a grade do SCAMTEC
    !

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx*ny))
    DO iv = 1, scamtec%nvar

       call interp_clima50yr( kpds, clima50yr_struc%npts,f(:,iv),lb2(:,iv), scamtec%gridDesc,&
                              scamtec%nxpt,scamtec%nypt, varfield)    

    !
    ! padronizando pontos com undef
    !

       where( varfield .eq. undef ) varfield = scamtec%udef

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       scamdata(1)%tmpfield(:,iv) = varfield

    enddo


  END SUBROUTINE clima50yr_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_clima50yr
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_clima50yr( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d)  

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
    real, intent(out)     :: field1d(:) ! output interpolated field

!
! !REVISION HISTORY: 
!  06 May 2012 - J. G. de Mattos - Initial Version
!
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
!

    logical*1, dimension(nxpt,nypt) :: lo

    integer :: ip, ipopt(20),ibi,km,iret
    integer :: ibo
    integer :: i,j,k
    character(len=*),parameter :: myname_=myname//'::interp_clima50yr'

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
                         clima50yr_struc%npts,nxpt*nypt,          &
                         clima50yr_struc%rlat1, clima50yr_struc%rlon1, &
                         clima50yr_struc%w111, clima50yr_struc%w121,   &
                         clima50yr_struc%w211, clima50yr_struc%w221,   &
                         clima50yr_struc%n111, clima50yr_struc%n121,   &
                         clima50yr_struc%n211, clima50yr_struc%n221,scamtec%udef,iret)
    if (iret.ne.0)then
       call perr(myname_,'bilinear_interp ( ... ) ',iret)
       return
    endif

!   k = 0
!   do j = 1, nypt
!      do i = 1, nxpt
!         varfield(i,j) = field1d(i+k)
!      enddo
!      k = k + nxpt
!   enddo

  END SUBROUTINE interp_clima50yr

END MODULE m_clima50yr
