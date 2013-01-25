MODULE m_clima50yr

  USE scamtec_module
  USE SCAM_dataMOD, only : scamdata
  USE interp_mod
  USE m_ioutil

  IMPLICIT NONE
  PRIVATE
  type clima50yr_type_dec 

     integer            :: npts
     real, pointer      :: gridDesc(:)
     real, pointer      :: rlat1(:)
     real, pointer      :: rlon1(:)
     integer, pointer   :: n111(:)
     integer, pointer   :: n121(:)
     integer, pointer   :: n211(:)
     integer, pointer   :: n221(:)
     real, pointer      :: w111(:),w121(:)
     real, pointer      :: w211(:),w221(:)

  end type clima50yr_type_dec

  type(clima50yr_type_dec) :: clima50yr_struc

  public :: clima50yr_read
  public :: clima50yr_init

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='m_clima50yr'

CONTAINS

  SUBROUTINE clima50yr_init()
    IMPLICIT NONE
    integer :: nx, ny

    character(len=*),parameter :: myname_=myname//'::clima50yr_init'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
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



    call bilinear_interp_input(clima50yr_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     clima50yr_struc%rlat1,clima50yr_struc%rlon1,      &
                                     clima50yr_struc%n111,clima50yr_struc%n121,        &
                                     clima50yr_struc%n211,clima50yr_struc%n221,        &
                                     clima50yr_struc%w111,clima50yr_struc%w121,        &
                                     clima50yr_struc%w211,clima50yr_struc%w221)


  END SUBROUTINE clima50yr_init

  SUBROUTINE clima50yr_domain()
    IMPLICIT NONE

    character(len=*),parameter :: myname_=myname//'::clima50yr_domain'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
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

  SUBROUTINE clima50yr_read(fname)
    IMPLICIT NONE
    character(len=*), intent(IN) :: fname
    integer :: ferror

    integer :: iret,lugb
    logical :: file_exists
    integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
    real :: lubi,kf,k
    integer :: i,j
    integer :: iv, rc
    integer :: npts
    integer :: nx
    integer :: ny
    integer, dimension(17) :: pds5, pds7
    logical*1, dimension(:), allocatable :: lb
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:,:), allocatable :: varfield
    character(len=*),parameter :: myname_=myname//'::clima50yr_read'

    !
    ! Parametros para ler os dados do arquivo de Climatologia
    !

    integer :: NV1lev = 43 ! Numero de Variaveis com 1 Nivel
    integer :: NLev   = 18 ! Numero de Niveis

    !
    ! Variaveis temporarias utilizadas para conversao de umidades e variaveis
    !
    
    real, allocatable, dimension(:) :: ee, es, rv, qq

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
    WRITE(6,'( A,1X,A)')'Open File ::', trim(fname)
#endif

    !
    !
    !

    lugb = 10
    lubi = 0
    j    = 0
    jpds = -1 
    !          1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
    !          T   T   T  rh  rh  rh   P   A   Z   Z   Z   U   U   U   V   V   V
    !        925 850 500 925 850 500 000 000 850 500 250 850 500 250 850 500 250
    pds5 = (/ 49, 49, 49, 50, 50, 50, 10, 18, 48, 48, 48, 44, 44, 44, 45, 45, 45/) !parameter
    pds7 = (/002,003,006,002,003,006,000,000,003,006,009,003,006,009,003,006,009/) !htlev2
     
    allocate(lb(clima50yr_struc%npts))
    allocate(f(clima50yr_struc%npts,size(pds5)))

    inquire (file=trim(fname), exist=file_exists)
    if (file_exists) then 

       jpds(5) = pds5(1)
       jpds(7) = pds7(1)
       
!       call opnieee(lugb, trim(fname), 'unknown', iret, clima50yr_struc%npts*4)
        open (unit=lugb, file=trim(fname),form='unformatted',access='direct',recl=435*386*4,status='unknown',iostat=iret)

       if(iret.eq.0) then

          do iv=1,size(pds5)
             rc = pds5(iv) + pds7(iv)+                        &
                  max0((pds5(iv)-(NV1Lev+1)),0)*NLev -        &
                  max0((pds5(iv)-NV1Lev),0) +                 &
                  max0(1-mod(pds5(iv),NV1Lev+1),0)

             read(lugb,rec=rc)f(:,iv)
             lb = .true.
          enddo

       else
          gbret = 99
       endif

       call clsieee(lugb,jret)

    else

       ferror = 0
       deallocate(f)
       deallocate(lb)

    endif


    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !
    allocate(f2(clima50yr_struc%npts,scamtec%nvar))
    allocate(es(clima50yr_struc%npts))
    allocate(ee(clima50yr_struc%npts))
    allocate(rv(clima50yr_struc%npts))


    es       = 6.1078*exp((17.2693882*(f(:,1)-273.16))/(f(:,1)-35.86))
    ee       = f(:,4)*es
    rv       = (0.622*ee)/(925.0-ee)

    f2(:, 5) = rv/(1+rv)                           ! Umes @ 925 hPa [Kg/Kg]

    f2(:, 1) = f(:,1)*(1 + 0.61*rv)                ! Vtmp @ 925 hPa [K]

    es       = 6.1078*exp((17.2693882*(f(:,2)-273.16))/(f(:,2)-35.86))
    ee       = f(:,5)*es
    rv       = (0.622*ee)/(925.0-ee)
    f2(:, 2) = f(:,2)*(1 + 0.61*rv)                ! Vtmp @ 850 hPa [K]

    es       = 6.1078*exp((17.2693882*(f(:,3)-273.16))/(f(:,3)-35.86))
    ee       = f(:,6)*es
    rv       = (0.622*ee)/(925.0-ee)
    f2(:, 3) = f(:,3)*(1 + 0.61*rv)                ! Vtmp @ 500 hPa [K]

    DeAllocate(es)
    DeAllocate(ee)
    DeAllocate(rv)

    f2(:, 4) = f(:, 7)                            ! PSNM [hPa]
    f2(:, 6) = f(:, 8)                            ! Agpl @ 925 hPa [Kg/m2]
    f2(:, 7) = f(:, 9)                            ! Zgeo @ 850 hPa [gpm]
    f2(:, 8) = f(:,10)                            ! Zgeo @ 500 hPa [gpm]
    f2(:, 9) = f(:,11)                            ! Zgeo @ 250 hPa [gpm]
    f2(:,10) = f(:,12)                            ! Uvel @ 850 hPa [m/s]
    f2(:,11) = f(:,13)                            ! Uvel @ 500 hPa [m/s]
    f2(:,12) = f(:,14)                            ! Uvel @ 250 hPa [m/s]
    f2(:,13) = f(:,15)                            ! Vvel @ 850 hPa [m/s]
    f2(:,14) = f(:,16)                            ! Vvel @ 500 hPa [m/s]
    f2(:,15) = f(:,17)                            ! Vvel @ 250 hPa [m/s]

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
    
    allocate(varfield(nx,ny))
    DO iv = 1, scamtec%nvar

       call interp_clima50yr( kpds, clima50yr_struc%npts,f(:,iv),lb, scamtec%gridDesc,&
                              scamtec%nxpt,scamtec%nypt, varfield)    

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       scamdata(1)%tmpfield(:,:,iv) = varfield(:,:)

    enddo


  END SUBROUTINE clima50yr_read

  SUBROUTINE interp_clima50yr( kpds, npts,f,lb,gridDesc, nxpt, nypt, varfield)  

    ! !ARGUMENTS:   
    integer, intent(in)   :: kpds(:)
    integer, intent(in)   :: npts
    real, intent(out)     :: f(:)
    logical*1, intent(in) :: lb(:)
    real, intent(in)      :: gridDesc(:)
    integer, intent(in)   :: nxpt
    integer, intent(in)   :: nypt
    real, intent(out)     :: varfield(:,:)

    !
    !
    !

    real, dimension(nxpt*nypt) :: field1d
    logical*1, dimension(nxpt,nypt) :: lo

    integer :: ip, ipopt(20),ibi,km,iret
    integer :: ibo
    integer :: i,j,k

    character(len=*),parameter :: myname_=myname//'::interp_clima50yr'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
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

    k = 0
    do j = 1, nypt
       do i = 1, nxpt
          varfield(i,j) = field1d(i+k)
       enddo
       k = k + nxpt
    enddo

  END SUBROUTINE interp_clima50yr

END MODULE m_clima50yr
