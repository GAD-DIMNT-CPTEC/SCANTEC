MODULE m_T126_Seq

  USE scamtec_module
  USE SCAM_dataMOD, only : scamdata
  USE interp_mod
  USE m_ioutil

  IMPLICIT NONE
  PRIVATE
  type T126_Seq_type_dec 

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

  end type T126_Seq_type_dec

  type(T126_Seq_type_dec) :: T126_Seq_struc

  public :: T126_Seq_read
  public :: T126_Seq_init

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='m_T126_Seq'

CONTAINS

  SUBROUTINE T126_Seq_init()
    IMPLICIT NONE
    integer :: nx, ny

    character(len=*),parameter :: myname_=myname//'::T126_Seq_init'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    Allocate(T126_Seq_struc%gridDesc(50))

    call T126_Seq_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(T126_Seq_struc%rlat1(nx*ny))
    Allocate(T126_Seq_struc%rlon1(nx*ny))              
    Allocate(T126_Seq_struc%n111(nx*ny))
    Allocate(T126_Seq_struc%n121(nx*ny))
    Allocate(T126_Seq_struc%n211(nx*ny))
    Allocate(T126_Seq_struc%n221(nx*ny))
    Allocate(T126_Seq_struc%w111(nx*ny))
    Allocate(T126_Seq_struc%w121(nx*ny))
    Allocate(T126_Seq_struc%w211(nx*ny))
    Allocate(T126_Seq_struc%w221(nx*ny))



    call bilinear_interp_input(T126_Seq_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     T126_Seq_struc%rlat1,T126_Seq_struc%rlon1,      &
                                     T126_Seq_struc%n111,T126_Seq_struc%n121,        &
                                     T126_Seq_struc%n211,T126_Seq_struc%n221,        &
                                     T126_Seq_struc%w111,T126_Seq_struc%w121,        &
                                     T126_Seq_struc%w211,T126_Seq_struc%w221)


  END SUBROUTINE T126_Seq_init

  SUBROUTINE T126_Seq_domain()
    IMPLICIT NONE

    character(len=*),parameter :: myname_=myname//'::T126_Seq_domain'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


    T126_Seq_struc%gridDesc     = 0 

    T126_Seq_struc%gridDesc( 1) = 4         !Input grid type (4=Gaussian)
    T126_Seq_struc%gridDesc( 2) = 384       !Number of points on a lat circle
    T126_Seq_struc%gridDesc( 3) = 192       !Number of points on a meridian
    T126_Seq_struc%gridDesc( 4) = 89.2842   !Latitude of origi n
    T126_Seq_struc%gridDesc( 5) = 0.0       !Longitude of origin
    T126_Seq_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                                 !(recall that 10000000 = 128), Table 7
    T126_Seq_struc%gridDesc( 7) = -89.2842  !Latitude of extreme point
    T126_Seq_struc%gridDesc( 8) = -0.9375   !Longitude of extreme point
    T126_Seq_struc%gridDesc( 9) = 0.9375    !N/S direction increment
    T126_Seq_struc%gridDesc(10) =  96       !(Gaussian) # lat circles pole-equator
    T126_Seq_struc%gridDesc(20) = 0.0  

    T126_Seq_struc%npts = T126_Seq_struc%gridDesc(2)*T126_Seq_struc%gridDesc(3)

  END SUBROUTINE T126_Seq_domain

  SUBROUTINE T126_Seq_read(fname)
    IMPLICIT NONE
    character(len=*), intent(IN) :: fname
    integer :: ferror

    integer :: iret,lugb
    logical :: file_exists
    integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
    real :: lubi,kf,k
    integer :: i,j,z,y
    integer :: iv, rc
    integer :: npts
    integer :: nx
    integer :: ny
    integer, dimension(17) :: pds5, pds7
    logical*1, dimension(:), allocatable :: lb
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:,:), allocatable :: varfield

    REAL,DIMENSION(384*192,208) :: binario !variavel de leitura  

    character(len=*),parameter :: myname_=myname//'::T126_Seq_read'

    !
    ! Parametros para ler os dados do arquivo de T126_Seq
    !

    integer :: NV1lev = 10! Numero de Variaveis com 1 Nivel
    integer :: NLev   = 18 ! Numero de Niveis
    
    
    
    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
    WRITE(6,'( A,1X,A)')'Open File ::', trim(fname)
#endif
     
    allocate(lb(T126_Seq_struc%npts))
	lb = .true.
    allocate(f(T126_Seq_struc%npts,17))
	
    lugb = 1

    inquire (file=trim(fname), exist=file_exists)
    if (file_exists) then 
	OPEN (UNIT=lugb,FILE=trim(fname),FORM='unformatted', CONVERT='BIG_ENDIAN',STATUS ='Unknown')  ! abrindo binario T126_Seq
		
	do z=1, 208
		
		read(UNIT=lugb)binario(:,z)
                !write(15)binario(:,z)
		
	enddo 
		
       call clsieee(lugb,jret)

    else

       ferror = 0
       !deallocate(f)
       !deallocate(lb)

    endif


    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !
    allocate(f2(T126_Seq_struc%npts,scamtec%nvar))



!----------------------------------------------------------------------------

   f(:,1) = binario(:,153)     ! T 925            ABSOLUTE TEMPERATURE  
   f(:,2) = binario(:,154)     ! T 850            ABSOLUTE TEMPERATURE  
   f(:,3) = binario(:,157)     ! T 500            ABSOLUTE TEMPERATURE  
   f(:,4) = binario(:,150)     ! PSNM [hPa]

		

   f(:,5) = binario(:,190)     ! Q 925 		  SPECIFIC HUMIDITY
   f(:,6) = binario(:,191)     ! Q 850 		  SPECIFIC HUMIDITY
   f(:,7) = binario(:,194)     ! Q 500 		  SPECIFIC HUMIDITY

   f(:,8)  = binario(:,207)    ! Agpl @ 925 hPa [Kg/m2]
   f(:,9)  = binario(:,134)    ! Zgeo @ 850 hPa [gpm]
   f(:,10) = binario(:,137)    ! Zgeo @ 500 hPa [gpm]
   f(:,11) = binario(:,140)    ! Zgeo @ 250 hPa [gpm]
   f(:,12) = binario(:,7)      ! Uvel @ 850 hPa [m/s]
   f(:,13) = binario(:,10)     ! Uvel @ 500 hPa [m/s]
   f(:,14) = binario(:,13)     ! Uvel @ 250 hPa [m/s]
   f(:,15) = binario(:,26)     ! Vvel @ 850 hPa [m/s]
   f(:,16) = binario(:,29)     ! Vvel @ 500 hPa [m/s]
   f(:,17) = binario(:,32)     ! Vvel @ 250 hPa [m/s]
  
  




   
   
   
   !f2(:, 1) = f(:, 1)*((461.5250/287.0597-1)*f(:,5)+1) ! Vtmp @ 925 hPa [K] (modo de calcular temperatura virtual do operador rognss)
   !f2(:, 2) = f(:, 2)*((461.5250/287.0597-1)*f(:,6)+1) ! Vtmp @ 850 hPa [K] (modo de calcular temperatura virtual do operador rognss)
   !f2(:, 3) = f(:, 3)*((461.5250/287.0597-1)*f(:,7)+1) ! Vtmp @ 500 hPa [K] (modo de calcular temperatura virtual do operador rognss)

   f2(:,1) = f(:,1)*(1 + 0.61*(f(:,5)/(1-f(:,5)))) ! Vtmp @ 925 hPa [K]
   f2(:,2) = f(:,2)*(1 + 0.61*(f(:,6)/(1-f(:,6)))) ! Vtmp @ 850 hPa [K]
   f2(:,3) = f(:,3)*(1 + 0.61*(f(:,7)/(1-f(:,7)))) ! Vtmp @ 500 hPa [K]
   
   f2(:, 4) = f(:,4)                                 ! PSNM [hPa]
   f2(:, 5) = f(:,5)*1000                            ! Umes @ 925 hPa [Kg/Kg] (esta multiplicado para rmse ficar mais visivel)
   f2(:, 6) = f(:,8)                                 ! Agpl @ 925 hPa [Kg/m2]
   f2(:, 7) = f(:,9)                                 ! Zgeo @ 850 hPa [gpm]
   f2(:, 8) = f(:,10)                                ! Zgeo @ 500 hPa [gpm]
   f2(:, 9) = f(:,11)                                ! Zgeo @ 250 hPa [gpm]
   f2(:,10) = f(:,12)                                ! Uvel @ 850 hPa [m/s]
   f2(:,11) = f(:,13)                                ! Uvel @ 500 hPa [m/s]
   f2(:,12) = f(:,14)                                ! Uvel @ 250 hPa [m/s]
   f2(:,13) = f(:,15)                                ! Vvel @ 850 hPa [m/s]
   f2(:,14) = f(:,16)                                ! Vvel @ 500 hPa [m/s]
   f2(:,15) = f(:,17)                                ! Vvel @ 250 hPa [m/s]

print*, f2(:,1)	
	
!------------------------------------------------------------------------------

    DeAllocate(f)

   
    !
    ! Interpolando para a grade do SCAMTEC
    !

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx,ny))
    DO iv = 1, scamtec%nvar !15 vairaveis
	
       call interp_T126_Seq( kpds, T126_Seq_struc%npts,f2(:,iv),lb, scamtec%gridDesc,&
                              scamtec%nxpt,scamtec%nypt, varfield)    
		
    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       scamdata(1)%tmpfield(:,:,iv) = varfield(:,:)
       write(75)varfield

    enddo
    
    DeAllocate(varfield)


  END SUBROUTINE T126_Seq_read

  SUBROUTINE interp_T126_Seq( kpds, npts,f,lb,gridDesc, nxpt, nypt, varfield)  

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

    character(len=*),parameter :: myname_=myname//'::interp_T126_Seq'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif
		
    ip    = 0
    ipopt = 0
    km    = 1
    ibi   = 1
    lo    = .true.

    call bilinear_interp(gridDesc,ibi,lb,f,ibo,lo,field1d,   &
                         T126_Seq_struc%npts,nxpt*nypt,          &
                         T126_Seq_struc%rlat1, T126_Seq_struc%rlon1, &
                         T126_Seq_struc%w111, T126_Seq_struc%w121,   &
                         T126_Seq_struc%w211, T126_Seq_struc%w221,   &
                         T126_Seq_struc%n111, T126_Seq_struc%n121,   &
                         T126_Seq_struc%n211, T126_Seq_struc%n221,scamtec%udef,iret)

    k = 0
    do j = 1, nypt
       do i = 1, nxpt
          varfield(i,j) = field1d(i+k)
       enddo
       k = k + nxpt
    enddo

  END SUBROUTINE interp_T126_Seq

END MODULE m_T126_Seq
