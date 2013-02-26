MODULE obs_Precipitation

  USE scamtec_module
  USE SCAM_dataMOD, only : scamdata
  USE interp_mod
  USE m_ioutil
  USE time_module, only: jul2cal, cal2jul ! Time operations
  USE SCAM_Utils, only: Precip, hist
  USE m_string          ! String Manipulation
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  
  IMPLICIT NONE
  PRIVATE
  type Precipitation_type_dec 

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

  end type Precipitation_type_dec

  type(Precipitation_type_dec) :: Precipitation_struc

  public :: Precipitation_read
  public :: Precipitation_init

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='obs_Precipitation'

CONTAINS

  SUBROUTINE Precipitation_init()
    IMPLICIT NONE
    integer :: nx, ny

    character(len=*),parameter :: myname_=myname//'::Precipitation_init'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    Allocate(Precipitation_struc%gridDesc(50))
    
    call Precipitation_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(Precipitation_struc%rlat1(nx*ny))
    Allocate(Precipitation_struc%rlon1(nx*ny))              
    Allocate(Precipitation_struc%n111(nx*ny))
    Allocate(Precipitation_struc%n121(nx*ny))
    Allocate(Precipitation_struc%n211(nx*ny))
    Allocate(Precipitation_struc%n221(nx*ny))
    Allocate(Precipitation_struc%w111(nx*ny))
    Allocate(Precipitation_struc%w121(nx*ny))
    Allocate(Precipitation_struc%w211(nx*ny))
    Allocate(Precipitation_struc%w221(nx*ny))



    call bilinear_interp_input(Precipitation_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     Precipitation_struc%rlat1,Precipitation_struc%rlon1,      &
                                     Precipitation_struc%n111,Precipitation_struc%n121,        &
                                     Precipitation_struc%n211,Precipitation_struc%n221,        &
                                     Precipitation_struc%w111,Precipitation_struc%w121,        &
                                     Precipitation_struc%w211,Precipitation_struc%w221)


  END SUBROUTINE Precipitation_init

  SUBROUTINE Precipitation_domain()
    IMPLICIT NONE

    character(len=*),parameter :: myname_=myname//'::Precipitation_domain'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


    Precipitation_struc%gridDesc     = 0 

    Precipitation_struc%gridDesc( 1) = 0         !Input grid type (4=Gaussian)
    Precipitation_struc%gridDesc( 2) = 190       !Number of points on a lat circle
    Precipitation_struc%gridDesc( 3) = 246       !Number of points on a meridian
    Precipitation_struc%gridDesc( 4) = -49.875   !Latitude of origin
    Precipitation_struc%gridDesc( 5) = -82.625   !Longitude of origin
    Precipitation_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                                 !(recall that 10000000 = 128), Table 7
    Precipitation_struc%gridDesc( 7) = 11.375    !Latitude of extreme point
    Precipitation_struc%gridDesc( 8) = -35.375   !Longitude of extreme point
    Precipitation_struc%gridDesc( 9) = 0.250     !N/S direction increment
    Precipitation_struc%gridDesc(10) = 0.250     !(Gaussian) # lat circles pole-equator
    Precipitation_struc%gridDesc(20) = 0.0  

    Precipitation_struc%npts = Precipitation_struc%gridDesc(2)*Precipitation_struc%gridDesc(3)

  END SUBROUTINE Precipitation_domain

  SUBROUTINE Precipitation_read(fname)
    IMPLICIT NONE
    character(len=*), intent(IN) :: fname
    integer :: ferror

    integer :: iret,lugb
    logical :: file_exists
    integer :: gbret,jret,jpds(200),jgds(200),gridDesc(200),kpds(200)
    real :: lubi,kf,k
    integer :: i,j,z,y,w
    integer :: iv, rc
    integer :: npts
    integer :: nx
    integer :: ny
    integer, dimension(19) :: pds5, pds7
    logical*1, dimension(:,:), allocatable :: lb
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:), allocatable :: varfield

    REAL,DIMENSION(190*246) :: binario !variavel de leitura  
    REAL,DIMENSION(190*246) :: binario_total !variavel de leitura  

    character(len=*),parameter :: myname_=myname//'::Precipitation_read'

    !
    ! Parametros para ler os dados do arquivo de Precipitation
    !

    integer :: NV1lev = 1! Numero de Variaveis com 1 Nivel
    integer :: NLev   = 1 ! Numero de Niveis
    real    :: undef = -9999.0
    
    
   !------------------------ 
  
    character(len=512) :: fname2, fmt
    integer            :: nymd, nhms
    integer            :: fymd, fhms
    INTEGER            :: quant_arq_ant   !Quantidade de arquivos anterior  
    character(len=1024) :: Precipitation  ! Precipitation File Name (Paulo dias)
    integer(I4B) :: atime 
    real :: incr  
    
    
    
    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_    
#endif
    
    !print *, Precipitation_struc%npts
    !stop
    
    allocate(lb(Precipitation_struc%npts,scamtec%nvar))
    lb = .true.
    allocate(f(Precipitation_struc%npts,19))
    
    lugb = 1
    binario_total=0
    inquire (file=trim(fname), exist=file_exists)
    
    !Calculo para o incremento do tempo
    incr=((100*hist%acumulo_obs)/24.0)/100.0   
    
    atime=scamtec%atime
 
    if (file_exists) then        
    
        quant_arq_ant=hist%acumulo_exp/hist%acumulo_obs     !Calculando quantidade de arquivos anterior para abrir
        
        ! loop para somar o acumulo de precipitacao
        do i=1, quant_arq_ant
            scamtec%ftime  = atime   
            fymd = scamtec%ftime/100
            fhms = MOD(scamtec%ftime,100) * 10000
            Precipitation=TRIM(Precip%file)                
            CALL str_template(Precipitation, fymd,fhms)
            
#ifdef DEBUG
   WRITE(6,'( A,1X,A)')'Open File ::', trim(Precipitation)
#endif         
            OPEN (UNIT=lugb,FILE=trim(Precipitation),FORM='unformatted',convert='big_endian',access='direct',recl=190*246*4,ACTION = 'read',STATUS ='Unknown',iostat=iret)
                
            read(lugb, rec=1)binario(:)
            binario_total(:)=binario_total(:)+binario(:)
            !write(15)binario(:,:)                    
            close(lugb)
            
            atime=jul2cal(cal2jul(atime)-incr)
            
          enddo  

    else

       ferror = 0
       !deallocate(f)
       !deallocate(lb)

    endif     
    
    
    ! abrindo binario Precipitation
    
    

    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !
    allocate(f2(Precipitation_struc%npts,scamtec%nvar))



!----------------------------------------------------------------------------

   f(:,1) = 0     ! T 925            ABSOLUTE TEMPERATURE  
   f(:,2) = 0     ! T 850            ABSOLUTE TEMPERATURE  
   f(:,3) = 0     ! T 500            ABSOLUTE TEMPERATURE  
   f(:,4) = 0     ! PSNM [hPa]

        

   f(:,5) = 0     ! Q 925         SPECIFIC HUMIDITY
   f(:,6) = 0     ! Q 850         SPECIFIC HUMIDITY
   f(:,7) = 0     ! Q 500         SPECIFIC HUMIDITY

   f(:,8)  = 0    ! Agpl @ 925 hPa [Kg/m2]
   f(:,9)  = 0    ! Zgeo @ 850 hPa [gpm]
   f(:,10) = 0    ! Zgeo @ 500 hPa [gpm]
   f(:,11) = 0    ! Zgeo @ 250 hPa [gpm]
   f(:,12) = 0    ! Uvel @ 850 hPa [m/s]
   f(:,13) = 0    ! Uvel @ 500 hPa [m/s]
   f(:,14) = 0    ! Uvel @ 250 hPa [m/s]
   f(:,15) = 0    ! Vvel @ 850 hPa [m/s]
   f(:,16) = 0    ! Vvel @ 500 hPa [m/s]
   f(:,17) = 0    ! Vvel @ 250 hPa [m/s]
   f(:,18) = 0    ! PREC @ 000 hPa [kg/m2/day]
   f(:,19) = binario_total(:)!teste    ! PREV @ 000 hPa [kg/m2/day] 
   
   

   f2(:,1) = f(:,1) ! Vtmp @ 925 hPa [K]
   f2(:,2) = f(:,2) ! Vtmp @ 850 hPa [K]
   f2(:,3) = f(:,3) ! Vtmp @ 500 hPa [K]
   
   f2(:, 4) = f(:,4)                                 ! PSNM [hPa]
   f2(:, 5) = f(:,5)                                 ! Umes @ 925 hPa [Kg/Kg] (esta multiplicado para rmse ficar mais visivel)
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
   f2(:,16) = f(:,18)                                ! PREC @ 000 hPa [kg/m2/day]
   f2(:,17) = f(:,19)                                ! PREV @ 000 hPa [kg/m2/day]

    do iv=1, scamtec%nvar
       where(f2(:,iv).eq.undef) lb(:,iv) = .false.
    enddo
!------------------------------------------------------------------------------

    DeAllocate(f)
!*************************************************************
! Pergunata
! Tem que inverter Y?

    !
    ! invertendo y
    !

    !allocate(f(clima50yr_struc%npts,scamtec%nvar))
    

   ! nx = int(clima50yr_struc%gridDesc(2))
   ! ny = int(clima50yr_struc%gridDesc(3))

    !k = 0
    !do j = 1, ny
     !  do i = 1, nx
      !    f(i+(nx*ny)-nx-k,:) = f2(i+k,:)
       !enddo
       !k = k + nx
   ! enddo

!*************************************************************
   
    !
    ! Interpolando para a grade do SCAMTEC
    !


  
    scamdata(1)%tmpfield(:,16) = binario(:)


    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx*ny))
    DO iv = 1, scamtec%nvar !15 vairaveis
    
      !call interp_Precipitation( kpds, Precipitation_struc%npts,f2(:,iv),lb, scamtec%gridDesc,scamtec%nxpt,scamtec%nypt, varfield)    
        
    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       !scamdata(1)%tmpfield(:,:,iv) = varfield(:,:)
       

    enddo
    
    DeAllocate(varfield)


  END SUBROUTINE Precipitation_read

  SUBROUTINE interp_Precipitation( kpds, npts,f,lb,gridDesc, nxpt, nypt, varfield)  

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

    character(len=*),parameter :: myname_=myname//'::interp_Precipitation'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif
        
    ip    = 0
    ipopt = 0
    km    = 1
    ibi   = 1
    lo    = .true.

    call bilinear_interp(gridDesc,ibi,lb,f,ibo,lo,field1d,   &
                        Precipitation_struc%npts,nxpt*nypt,          &
                        Precipitation_struc%rlat1, Precipitation_struc%rlon1, &
                        Precipitation_struc%w111, Precipitation_struc%w121,   &
                        Precipitation_struc%w211, Precipitation_struc%w221,   &
                        Precipitation_struc%n111, Precipitation_struc%n121,   &
                        Precipitation_struc%n211, Precipitation_struc%n221,scamtec%udef,iret)

    k = 0
    do j = 1, nypt
       do i = 1, nxpt
          varfield(i,j) = field1d(i+k)
       enddo
       k = k + nxpt
    enddo

  END SUBROUTINE interp_Precipitation

END MODULE obs_Precipitation
    