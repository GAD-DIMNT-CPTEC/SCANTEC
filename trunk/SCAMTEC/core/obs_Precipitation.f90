!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: obs_Precipitation.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the precipitation to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!


MODULE obs_Precipitation

!
! !USES:
!

  USE scamtec_module
  USE SCAM_dataMOD, only : scamdata
  USE interp_mod
  USE m_ioutil
  USE time_module, only: jul2cal, cal2jul ! Time operations
  USE SCAM_Utils, only: Precip, hist
  USE m_string          ! String Manipulation
  USE m_die             ! Error Messages
  USE m_stdio           ! Module to defines std. I/O parameters
  
  IMPLICIT NONE
  PRIVATE
  
!
! !PUBLIC TYPES:  
! 
  
  type Precipitation_type_dec 

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

  end type Precipitation_type_dec

  type(Precipitation_type_dec) :: Precipitation_struc
  
  
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: Precipitation_read
  public :: Precipitation_init


!---------------------------------------------------------------------

  character(len=*),parameter :: myname='obs_Precipitation'

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Precipitation_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!


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
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Precipitation_domain
!
! !DESCRIPTION: This routine initilize domain parameters of precipitation
!               
!\\
!\\
! !INTERFACE:
!


  SUBROUTINE Precipitation_domain()
  
  
    IMPLICIT NONE

    character(len=*),parameter :: myname_=myname//'::Precipitation_domain'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif


    Precipitation_struc%gridDesc     = 0 

    Precipitation_struc%gridDesc( 1) = 0         !Input grid type (4=Gaussian)
    Precipitation_struc%gridDesc( 2) = 1440      !Number of points on a lat circle
    Precipitation_struc%gridDesc( 3) = 480       !Number of points on a meridian
    Precipitation_struc%gridDesc( 4) = -59.87500 !Latitude of origin
    Precipitation_struc%gridDesc( 5) = 0.125000  !Longitude of origin
    Precipitation_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                                 !(recall that 10000000 = 128), Table 7
    Precipitation_struc%gridDesc( 7) = 59.87500  !Latitude of extreme point
    Precipitation_struc%gridDesc( 8) = -0.125000 !Longitude of extreme point
    Precipitation_struc%gridDesc( 9) = 0.250     !N/S direction increment
    Precipitation_struc%gridDesc(10) = 0.250     !(Gaussian) # lat circles pole-equator
    Precipitation_struc%gridDesc(20) = 0.0  

    Precipitation_struc%npts = Precipitation_struc%gridDesc(2)*Precipitation_struc%gridDesc(3)
    

  END SUBROUTINE Precipitation_domain

!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Precipitation_read
!
! !DESCRIPTION: For a given file name, read fields from a precipitation.
!                    
!               
!\\
!\\
! !INTERFACE:
!

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
    logical*1, dimension(:,:), allocatable :: lb, lb2
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:), allocatable :: varfield

    REAL,DIMENSION(1440*480) :: binario       !variavel de leitura  
    REAL,DIMENSION(1440*480) :: binario_total !variavel de leitura  
    

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
    integer(I4B) :: atime , ftime
    real :: incr  
    
    
    
    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_    
#endif
        
    allocate(lb(Precipitation_struc%npts,scamtec%nvar))

    allocate(lb2(Precipitation_struc%npts,scamtec%nvar))
    
    !lb = .true.
    
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
            ftime  = atime   
            fymd = ftime/100
            fhms = MOD(ftime,100) * 10000
            Precipitation=TRIM(Precip%file)                
            CALL str_template(Precipitation, fymd,fhms)
            
#ifdef DEBUG
   WRITE(6,'( A,1X,A)')'Open File ::', trim(Precipitation)
#endif         
            !dados de 2012
            !OPEN (UNIT=lugb,FILE=trim(Precipitation),FORM='unformatted',convert='big_endian',access='direct',recl=190*246*4,ACTION = 'read',STATUS ='Unknown',iostat=iret) 
                
            !dados de 2014
            OPEN (UNIT=lugb,FILE=trim(Precipitation),FORM='unformatted',convert='big_endian',access='direct',recl=1440*480*4,ACTION = 'read',STATUS ='Unknown',iostat=iret)
                
            read(lugb, rec=1)binario(:)
                                   
            binario_total(:)=binario_total(:)+binario(:)
            
            print*,'Min/ Max precipitation: ',minval(binario_total(:)),maxval(binario_total(:))
                              
            close(lugb)
            
            atime=jul2cal(cal2jul(atime)-incr)
            
          enddo  

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
    allocate(f2(Precipitation_struc%npts,scamtec%nvar))



!----------------------------------------------------------------------------

   f(:,1) = 0 ;                 lb(:,1) = .true.   ! T 850 ABSOLUTE TEMPERATURE  
   f(:,2) = 0 ;                 lb(:,2) = .true.   ! T 500 ABSOLUTE TEMPERATURE  
   f(:,3) = 0 ;                lb(:,3) = .true.    ! T 250 ABSOLUTE TEMPERATURE  
   f(:,4) = 0 ;                 lb(:,4) = .true.   ! PSNM [hPa]
        
   f(:,5) = 0 ;                 lb(:,5) = .true.   ! Q 925 SPECIFIC HUMIDITY
   f(:,6) = 0 ;                 lb(:,6) = .true.   ! Q 850 SPECIFIC HUMIDITY
   f(:,7) = 0 ;                 lb(:,7) = .true.   ! Q 500 SPECIFIC HUMIDITY

   f(:,8)  = 0 ;                lb(:,8) = .true.   ! Agpl @ 925 hPa [Kg/m2]
   f(:,9)  = 0 ;                lb(:,9) = .true.   ! Zgeo @ 850 hPa [gpm]
   f(:,10) = 0 ;                lb(:,10) = .true.  ! Zgeo @ 500 hPa [gpm]
   f(:,11) = 0 ;                lb(:,11) = .true.  ! Zgeo @ 250 hPa [gpm]
   f(:,12) = 0 ;                lb(:,12) = .true.  ! Uvel @ 850 hPa [m/s]
   f(:,13) = 0 ;                lb(:,13) = .true.  ! Uvel @ 500 hPa [m/s]
   f(:,14) = 0 ;                lb(:,14) = .true.  ! Uvel @ 250 hPa [m/s]
   f(:,15) = 0 ;                lb(:,15) = .true.  ! Vvel @ 850 hPa [m/s]
   f(:,16) = 0 ; 	       lb(:,16) = .true.   ! Vvel @ 500 hPa [m/s]
   f(:,17) = 0 ;                lb(:,17) = .true.  ! Vvel @ 250 hPa [m/s]
   
   f(:,18) = binario_total(:) ; lb(:,18) = .true.  ! PREC @ 000 hPa [kg/m2/day]
   f(:,19) = 0  ;               lb(:,19) = .true.  ! PREV @ 000 hPa [kg/m2/day] 
   

   !print*,'f1 >> ',minval(f(:,18)),maxval(f(:,18))

!----------------------------------------------------------------------------      

   f2(:,1) = f(:,1);   lb2(:,1) = lb(:,1)   ! Vtmp @ 925 hPa [K] (era pra ser calculo da temp. virtual mas pra deixar nesse mesmo pois esta zerado
   f2(:,2) = f(:,2);   lb2(:,2) = lb(:,2)   ! Vtmp @ 850 hPa [K]
   f2(:,3) = f(:,3);   lb2(:,3) = lb(:,3)   ! Vtmp @ 500 hPa [K]
   
   f2(:,4) = f(:,1);  lb2(:,4) = lb(:,4)    ! Absolute Temperature @ 850 hPa [K]
   f2(:,5) = f(:,2);  lb2(:,5) = lb(:,5)    ! Absolute Temperature @ 500 hPa [K]                               
   f2(:,6) = f(:,3);  lb2(:,6) = lb(:,6)    ! Absolute Temperature @ 250 hPa [K]                               

      
   f2(:,7) =  f(:,4); lb2(:,7) = lb(:,7)    ! PSNM [hPa]
   f2(:,8) =  f(:,5); lb2(:,8) = lb(:,8)    ! Umes @ 925 hPa [Kg/Kg] (esta multiplicado para rmse ficar mais visivel)
   f2(:,9) =  f(:,6); lb2(:,9) = lb(:,9)    ! Umes @ 850 hPa [Kg/Kg] (esta multiplicado para rmse ficar mais visivel)
   f2(:,10) = f(:,7); lb2(:,10) = lb(:,10)  ! Umes @ 500 hPa [Kg/Kg] (esta multiplicado para rmse ficar mais visivel)
   
   f2(:,11) = f(:,8);  lb2(:,11) = lb(:,11) ! Agpl @ 925 hPa [Kg/m2]
   f2(:,12) = f(:,9);  lb2(:,12) = lb(:,12) ! Zgeo @ 850 hPa [gpm]
   f2(:,13) = f(:,10); lb2(:,13) = lb(:,13) ! Zgeo @ 500 hPa [gpm]
   f2(:,14) = f(:,11); lb2(:,14) = lb(:,14) ! Zgeo @ 250 hPa [gpm]
   f2(:,15) = f(:,12); lb2(:,15) = lb(:,15) ! Uvel @ 850 hPa [m/s]
   f2(:,16) = f(:,13); lb2(:,16) = lb(:,16) ! Uvel @ 500 hPa [m/s]
   f2(:,17) = f(:,14); lb2(:,17) = lb(:,17) ! Uvel @ 250 hPa [m/s]
   f2(:,18) = f(:,15); lb2(:,18) = lb(:,18) ! Vvel @ 850 hPa [m/s]
   f2(:,19) = f(:,16); lb2(:,19) = lb(:,19) ! Vvel @ 500 hPa [m/s]
   f2(:,20) = f(:,17); lb2(:,20) = lb(:,20) ! Vvel @ 250 hPa [m/s]
   
   f2(:,21) = f(:,18); lb2(:,21) = lb(:,21) ! PREC @ 000 hPa [kg/m2/day]
   f2(:,22) = f(:,19); lb2(:,22) = lb(:,22) ! PREV @ 000 hPa [kg/m2/day]

    do iv=1, scamtec%nvar
       where(f2(:,iv).eq.undef) lb(:,iv) = .false.
    enddo
!------------------------------------------------------------------------------

    DeAllocate(lb)
    DeAllocate(f)
    
!*************************************************************
! Pergunta
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

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    allocate(varfield(nx*ny))

    DO iv = 1, scamtec%nvar 
      
      call interp_Precipitation( kpds, Precipitation_struc%npts, f2(:,iv), lb2(:,iv), scamtec%gridDesc, scamtec%nxpt, scamtec%nypt, varfield, iret)            

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
    
       scamdata(1)%tmpfield(:,iv) = varfield(:)
              

    enddo
    
    !print*,minval(scamdata(1)%tmpfield(:,21)),maxval(scamdata(1)%tmpfield(:,21))
    
    
    DeAllocate(varfield)


  END SUBROUTINE Precipitation_read




  SUBROUTINE interp_Precipitation( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)

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
    character(len=*),parameter :: myname_=myname//'::interp_Precipitation'

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
                         Precipitation_struc%npts,nxpt*nypt,          &
                         Precipitation_struc%rlat1, Precipitation_struc%rlon1, &
                         Precipitation_struc%w111, Precipitation_struc%w121,   &
                         Precipitation_struc%w211, Precipitation_struc%w221,   &
                         Precipitation_struc%n111, Precipitation_struc%n121,   &
                         Precipitation_struc%n211, Precipitation_struc%n221,scamtec%udef,iret)

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

  END SUBROUTINE interp_Precipitation



END MODULE obs_Precipitation
    
