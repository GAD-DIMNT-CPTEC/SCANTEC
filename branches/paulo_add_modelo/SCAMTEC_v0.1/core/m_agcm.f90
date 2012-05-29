MODULE m_agcm
  USE scamtec_module
  USE SCAM_dataMOD, only : scamdata
  USE interp_mod

  IMPLICIT NONE
  PRIVATE
  type agcm_type_dec 

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

  end type agcm_type_dec

  type(agcm_type_dec) :: agcm_struc

  public :: agcm_read
  public :: agcm_init

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='m_agcm'

CONTAINS

  SUBROUTINE agcm_init()
    IMPLICIT NONE
    integer :: nx, ny
    character(len=*),parameter :: myname_=myname//'::agcm_init'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    Allocate(agcm_struc%gridDesc(50))

    call agcm_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(agcm_struc%rlat1(nx*ny))
    Allocate(agcm_struc%rlon1(nx*ny))              
    Allocate(agcm_struc%n111(nx*ny))
    Allocate(agcm_struc%n121(nx*ny))
    Allocate(agcm_struc%n211(nx*ny))
    Allocate(agcm_struc%n221(nx*ny))
    Allocate(agcm_struc%w111(nx*ny))
    Allocate(agcm_struc%w121(nx*ny))
    Allocate(agcm_struc%w211(nx*ny))
    Allocate(agcm_struc%w221(nx*ny))



    call bilinear_interp_input(agcm_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     agcm_struc%rlat1,agcm_struc%rlon1,      &
                                     agcm_struc%n111,agcm_struc%n121,        &
                                     agcm_struc%n211,agcm_struc%n221,        &
                                     agcm_struc%w111,agcm_struc%w121,        &
                                     agcm_struc%w211,agcm_struc%w221)


  END SUBROUTINE agcm_init

  SUBROUTINE agcm_domain()
    IMPLICIT NONE
    character(len=*),parameter :: myname_=myname//'::agcm_domain'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    agcm_struc%gridDesc     = 0 

    agcm_struc%gridDesc( 1) = 4         !Input grid type (4=Gaussian)
    agcm_struc%gridDesc( 2) = 900       !Number of points on a lat circle
    agcm_struc%gridDesc( 3) = 450       !Number of points on a meridian
    agcm_struc%gridDesc( 4) = 89.69415  !Latitude of origin
    agcm_struc%gridDesc( 5) = 0.0       !Longitude of origin
    agcm_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                        !(recall that 10000000 = 128), Table 7
    agcm_struc%gridDesc( 7) = -89.69415 !Latitude of extreme point
    agcm_struc%gridDesc( 8) = -0.400    !Longitude of extreme point
    agcm_struc%gridDesc( 9) = 0.400     !N/S direction increment
    agcm_struc%gridDesc(10) = 225       !(Gaussian) # lat circles pole-equator
    agcm_struc%gridDesc(20) = 0.0  

    agcm_struc%npts = agcm_struc%gridDesc(2)*agcm_struc%gridDesc(3)
		
  END SUBROUTINE agcm_domain

  SUBROUTINE agcm_read(fname)
    IMPLICIT NONE
    character(len=*), intent(IN) :: fname

    integer :: ferror

    character(len=*),parameter :: myname_=myname//'::agcm_read'

    integer :: iret,lugb
    logical :: file_exists
    integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
    real :: lubi,kf,k
    integer :: i,j,iv
    integer :: npts
    integer :: nx
    integer :: ny
    integer, dimension(17) :: pds5, pds7
    logical*1, dimension(:,:), allocatable :: lb
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:,:), allocatable :: varfield


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

    lugb = 1
    lubi = 0
    j    = 0
    jpds = -1 
    !          Q   Q   Q   T   T   T   P   A   Z   Z   Z   U   U   U   V   V   V
    pds5 = (/ 51, 51, 51, 11, 11, 11,  2, 54,  7,  7,  7, 33, 33, 33, 34, 34, 34/) !parameter
    pds7 = (/925,850,500,925,850,500,000,000,850,500,250,850,500,250,850,500,250/) !htlev2

    allocate(lb(agcm_struc%npts,size(pds5)))
    allocate(f(agcm_struc%npts,size(pds5)))

    inquire (file=trim(fname), exist=file_exists)
    do iv = 1, size(pds5)

       if (file_exists) then 

          jpds(5) = pds5(iv)
          jpds(7) = pds7(iv)

          lugb    = lugb + iv

          call baopenr(lugb,fname,iret)

          if(iret.eq.0) then
             call getgb(lugb,lubi,agcm_struc%npts,j,jpds,jgds,kf,k,kpds, &
                  gridDesc,lb(:,iv),f(:,iv),gbret)

          else
             gbret = 99
          endif

          call baclose(lugb,jret)

       else

          ferror = 0
          deallocate(f)
          deallocate(lb)

       endif

    enddo

    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !

    allocate(f2(agcm_struc%npts,scamtec%nvar))

    f2(:, 1) = f(:, 4)*(1 + 0.61*(f(:,1)/(1-f(:,1)))) ! Vtmp @ 925 hPa [K]
    f2(:, 2) = f(:, 5)*(1 + 0.61*(f(:,2)/(1-f(:,2)))) ! Vtmp @ 850 hPa [K]
    f2(:, 3) = f(:, 6)*(1 + 0.61*(f(:,3)/(1-f(:,3)))) ! Vtmp @ 500 hPa [K]
    f2(:, 4) = f(:, 7)                                ! PSNM [hPa]
    f2(:, 5) = f(:, 1)                                ! Umes @ 925 hPa [Kg/Kg]
    f2(:, 6) = f(:, 8)                                ! Agpl @ 925 hPa [Kg/m2]
    f2(:, 7) = f(:, 9)                                ! Zgeo @ 850 hPa [gpm]
    f2(:, 8) = f(:,10)                                ! Zgeo @ 500 hPa [gpm]
    f2(:, 9) = f(:,11)                                ! Zgeo @ 250 hPa [gpm]
    f2(:,10) = f(:,12)                                ! Uvel @ 850 hPa [m/s]
    f2(:,11) = f(:,13)                                ! Uvel @ 500 hPa [m/s]
    f2(:,12) = f(:,14)                                ! Uvel @ 250 hPa [m/s]
    f2(:,13) = f(:,15)                                ! Vvel @ 850 hPa [m/s]
    f2(:,14) = f(:,16)                                ! Vvel @ 500 hPa [m/s]
    f2(:,15) = f(:,17)                                ! Vvel @ 250 hPa [m/s]

    DeAllocate(f)

    !
    ! Interpolando para a grade do SCAMTEC
    !

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx,ny))

    DO iv=1,scamtec%nvar

       call interp_agcm( kpds, agcm_struc%npts,f2(:,iv),lb(:,iv), scamtec%gridDesc,&
                         scamtec%nxpt,scamtec%nypt, varfield)    

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !

       scamdata(1)%tmpfield(:,:,iv) = varfield(:,:)

    Enddo

    DeAllocate(varfield)

  END SUBROUTINE agcm_read

  SUBROUTINE interp_agcm( kpds, npts,f,lb,gridDesc, nxpt, nypt, varfield)  

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
    character(len=*),parameter :: myname_=myname//'::interp_agcm'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    ip    = 0
    ipopt = 0
    km    = 1
    ibi   = 1
    lo    = .true.

    call bilinear_interp(gridDesc,ibi,lb,f,ibo,lo,field1d,   &
                         agcm_struc%npts,nxpt*nypt,          &
                         agcm_struc%rlat1, agcm_struc%rlon1, &
                         agcm_struc%w111, agcm_struc%w121,   &
                         agcm_struc%w211, agcm_struc%w221,   &
                         agcm_struc%n111, agcm_struc%n121,   &
                         agcm_struc%n211, agcm_struc%n221,scamtec%udef,iret)

    k = 0
    do j = 1, nypt
       do i = 1, nxpt
          varfield(i,j) = field1d(i+k)
       enddo
       k = k + nxpt
    enddo

  END SUBROUTINE interp_agcm

END MODULE m_agcm
