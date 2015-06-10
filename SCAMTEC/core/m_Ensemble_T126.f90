!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_Ensemble_T126.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
! 28jan15                
!\\
!\\
! !INTERFACE:
!

MODULE m_Ensemble_T126

!
! !USES:
!
  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE interp_mod                    ! Interpolation module
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE SCAM_MetForm                  ! Module to conversion of meteorological variables


  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC TYPES:  
!
  type Ensemble_T126_type_dec 

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

  end type Ensemble_T126_type_dec

  type(Ensemble_T126_type_dec) :: Ensemble_T126_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: Ensemble_T126_read ! Function to read files from Ensemble_T126 model
  public :: Ensemble_T126_init ! Function to initilize weights to interpolate fields
!
!
! !REVISION HISTORY:
!  03 May 2012 - J. G. de Mattos - Initial Version
!  06 May 2012 - J. G. de Mattos - Include new fields read
!  17 Oct 2012 - J. G. de Mattos - change UMES to g/kg
!
!
!
! !SEE ALSO:
!   
!
!EOP
!-----------------------------------------------------------------------------!
!
  character(len=*),parameter :: myname='m_Ensemble_T126'

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Ensemble_T126_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE Ensemble_T126_init()
  
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
    character(len=*),parameter :: myname_=myname//'::Ensemble_T126_init'

    !
    ! DEBUG print
    !
    
#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    Allocate(Ensemble_T126_struc%gridDesc(50))

    call Ensemble_T126_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(Ensemble_T126_struc%rlat1(nx*ny))
    Allocate(Ensemble_T126_struc%rlon1(nx*ny))              
    Allocate(Ensemble_T126_struc%n111(nx*ny))
    Allocate(Ensemble_T126_struc%n121(nx*ny))
    Allocate(Ensemble_T126_struc%n211(nx*ny))
    Allocate(Ensemble_T126_struc%n221(nx*ny))
    Allocate(Ensemble_T126_struc%w111(nx*ny))
    Allocate(Ensemble_T126_struc%w121(nx*ny))
    Allocate(Ensemble_T126_struc%w211(nx*ny))
    Allocate(Ensemble_T126_struc%w221(nx*ny))

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(Ensemble_T126_struc%gridDesc, scamtec%gridDesc,            &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)),         &
                                     Ensemble_T126_struc%rlat1,Ensemble_T126_struc%rlon1,      &
                                     Ensemble_T126_struc%n111,Ensemble_T126_struc%n121,        &
                                     Ensemble_T126_struc%n211,Ensemble_T126_struc%n221,        &
                                     Ensemble_T126_struc%w111,Ensemble_T126_struc%w121,        &
                                     Ensemble_T126_struc%w211,Ensemble_T126_struc%w221)


  END SUBROUTINE Ensemble_T126_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Ensemble_T126_domain
!
! !DESCRIPTION: This routine initilize domain parameters of Ensemble_T126 model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE Ensemble_T126_domain()
  
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
    character(len=*),parameter :: myname_=myname//'::Ensemble_T126_domain'
    
    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    Ensemble_T126_struc%gridDesc     = 0 

    Ensemble_T126_struc%gridDesc( 1) = 4         !Input grid type (4=Gaussian)
    Ensemble_T126_struc%gridDesc( 2) = 384       !Number of points on a lat circle
    Ensemble_T126_struc%gridDesc( 3) = 192       !Number of points on a meridian
    Ensemble_T126_struc%gridDesc( 4) = 89.2842   !Latitude of origin
    Ensemble_T126_struc%gridDesc( 5) = 0.0       !Longitude of origin
    Ensemble_T126_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                            !(recall that 10000000 = 128), Table 7
    Ensemble_T126_struc%gridDesc( 7) = -89.2842  !Latitude of extreme point
    Ensemble_T126_struc%gridDesc( 8) = -0.9375   !Longitude of extreme point
    Ensemble_T126_struc%gridDesc( 9) = 0.9375    !N/S direction increment
    Ensemble_T126_struc%gridDesc(10) =  96       !(Gaussian) # lat circles pole-equator
    Ensemble_T126_struc%gridDesc(20) = 0.0  

    Ensemble_T126_struc%npts = Ensemble_T126_struc%gridDesc(2)*Ensemble_T126_struc%gridDesc(3)

  END SUBROUTINE Ensemble_T126_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Ensemble_T126_read
!
! !DESCRIPTION: For a given file name, read fields from a Ensemble_T126 model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!
  
  SUBROUTINE Ensemble_T126_read(fname)
    IMPLICIT NONE
    !
    ! !INPUT PARAMETERS:
    !
    
    character(len=*), intent(IN) :: fname ! File name of the Ensemble_T126 model
    
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
    character(len=*),parameter :: myname_=myname//'::Ensemble_T126_read'
  
    integer :: iret, jret, lugb, gbret
    logical :: file_exists
    integer :: jpds(200),jgds(200),gridDesc(200),kpds(200)
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

    REAL,DIMENSION(:,:), allocatable  :: binario !variavel de leitura  paulo dias
    integer :: ferror !paulo dias
    real    :: undef = -2.56e+33 !paulo dias
    

    !
    ! Parametros para ler os dados do arquivo de Ensemble_T126
    !

    integer :: NV1lev = 6  ! Numero de Variaveis com 1 Nivel
    integer :: NLev   = 28 ! Numero de Niveis
    
    character(10) :: dataini, datafinal 
    
    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
    WRITE(6,'( A,1X,A)')'Open File ::', trim(fname)
#endif
 
    allocate(lb(Ensemble_T126_struc%npts,scamtec%nvar))
    lb = .true.
    lugb = 1

    inquire (file=trim(fname), exist=file_exists)
	
    if (file_exists) then
     
!Verifica se é analise incial o index z que é da leitura começa com 208
! Pensar como fazer isso de forma mais adequada	
		
	    do w=1, 500	
		
		    if(trim(fname(w:w+3)) .eq. 'GPOS')then
	 		    dataini=trim(fname(w+7:w+16))
			    datafinal=trim(fname(w+17:w+26))
		    endif			
		
	    enddo 
	   	
	    if(dataini .eq. datafinal)then
	            y=31
	    else
		    y=34
	    endif
            !print*, '::: Y :::', y	
!-------------------------------------------------------------------------------------------	
       ! abrindo binario Ensemble_T126
	    OPEN (UNIT=lugb,FILE=trim(fname),FORM='unformatted', CONVERT='BIG_ENDIAN',STATUS ='Unknown')  
	    
	    !Alocando memoria para o arquivo binario
	    allocate(binario(Ensemble_T126_struc%npts,y)) 
		
	    do z=1, y
			read(UNIT=lugb)binario(:,z)
			!write(15)binario(:,z)
			!print*,'>>>>>>>>>>>>>  Z',z,minval(binario(:,z)),maxval(binario(:,z)),sum(binario(:,z))/size(binario,1)
			!stop
		enddo 
	   ! Fechando Arquivo	
       close(lugb)

    else
       ferror = 0
       deallocate(lb)
       call perr(myname_,'File Not Found: '//trim(fname),ferror)
       return
    endif
    
    
    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !
    allocate(f2(Ensemble_T126_struc%npts,scamtec%nvar))
    allocate(f(Ensemble_T126_struc%npts,19))

     f(:, :) = undef
    f2(:, :) = undef
   
    !Somente algumas variaveis foram retiradas dos arquivos o resto colocou-se undef
!----------------------------------------------------------------------------
    
   if(y .eq. 31)then
      f(:,1) = undef           ! TGSC  0 99 GROUND/SURFACE COVER TEMPERATURE (K)  == T 925 ABSOLUTE TEMPERATURE  
      f(:,2) = undef           ! T2MT    0 99 TIME MEAN TEMP AT 2-M FROM SFC (K)  == T 850 ABSOLUTE TEMPERATURE  
   else
      f(:,1) = binario(:,33)   ! TGSC  0 99 GROUND/SURFACE COVER TEMPERATURE (K)  == T 925 ABSOLUTE TEMPERATURE  
      f(:,2) = binario(:,34)   ! T2MT    0 99 TIME MEAN TEMP AT 2-M FROM SFC (K)  == T 850 ABSOLUTE TEMPERATURE  
   endif

   f(:,3) = undef              ! T 500 ABSOLUTE TEMPERATURE  
   f(:,4) = binario(:,3)       ! PSNM [hPa]

   f(:,5) = undef              ! Q 925 		  SPECIFIC HUMIDITY
   f(:,6) = undef              ! Q 850 		  SPECIFIC HUMIDITY
   f(:,7) = undef              ! Q 500 		  SPECIFIC HUMIDITY

   f(:,8)  = undef             ! Agpl @ 925 hPa [Kg/m2]
   f(:,9)  = binario(:,10)     ! Zgeo @ 850 hPa [gpm]
   f(:,10) = binario(:,20)     ! Zgeo @ 500 hPa [gpm]
   f(:,11) = binario(:,22)     ! Zgeo @ 250 hPa [gpm]
   f(:,12) = undef             ! Uvel @ 850 hPa [m/s]
   f(:,13) = undef             ! Uvel @ 500 hPa [m/s]
   f(:,14) = undef             ! Uvel @ 250 hPa [m/s]
   f(:,15) = undef             ! Vvel @ 850 hPa [m/s]
   f(:,16) = undef             ! Vvel @ 500 hPa [m/s]
   f(:,17) = undef             ! Vvel @ 250 hPa [m/s]
   if(y .eq. 31)then
        f(:,18) = undef        ! PREC @ 000 hPa [kg/m2/day]
        f(:,19) = undef        ! PREV @ 000 hPa [kg/m2/day]
   else
        f(:,18) = undef        ! PREC @ 000 hPa [kg/m2/day]
        f(:,19) = undef        ! PREV @ 000 hPa [kg/m2/day]
   endif
   deallocate (binario)
        
   
   ! Calculo para Temperatira Virtual
   !f2(:,1) = f(:,1)*(1 + 0.61*(f(:,5)/(1-f(:,5)))) ! Vtmp @ 925 hPa [K]
   !f2(:,2) = f(:,2)*(1 + 0.61*(f(:,6)/(1-f(:,6)))) ! Vtmp @ 850 hPa [K]
   !f2(:,3) = f(:,3)*(1 + 0.61*(f(:,7)/(1-f(:,7)))) ! Vtmp @ 500 hPa [K]
 
 ! do i=1,Ensemble_T126_struc%npts
 !       f2(i, 1) = tv(f(i,1),f(i,5)) ! Vtmp @ 925 hPa [K]
 !       f2(i, 2) = tv(f(i,2),f(i,6)) ! Vtmp @ 850 hPa [K]
 !       f2(i, 3) = tv(f(i,3),f(i,7)) ! Vtmp @ 500 hPa [K]  
 ! enddo

   f2(:, 1) = undef
   f2(:, 2) = undef
   f2(:, 3) = undef
   
   f2(:, 4) = f(:,1)         ! TGSC  0 99 GROUND/SURFACE COVER TEMPERATURE (K)  == T 925 ABSOLUTE TEMPERATURE  
   f2(:, 5) = f(:,2)         ! T2MT    0 99 TIME MEAN TEMP AT 2-M FROM SFC (K)  == T 850 ABSOLUTE TEMPERATURE  
   f2(:, 6) = undef
   f2(:, 7) = f(:,4)         ! PSNM [hPa]
   f2(:, 8) = undef
   f2(:, 9) = undef
   f2(:,10) = undef
   f2(:,11) = undef
   f2(:,12) = f(:, 9)        ! Zgeo @ 850 hPa [gpm]
   f2(:,13) = f(:,10)        ! Zgeo @ 500 hPa [gpm]
   f2(:,14) = f(:,11)        ! Zgeo @ 250 hPa [gpm]
   f2(:,15) = undef
   f2(:,16) = undef
   f2(:,17) = undef
  
    !Preenchendo variavel lb
    do iv=1, scamtec%nvar
       where(f2(:,iv).eq.undef) lb(:,iv) = .false.
    enddo
   
    DeAllocate(f)
       
    !
    ! Interpolando para a grade do SCAMTEC
    !
    
    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx*ny))
    
    DO iv = 1, scamtec%nvar !17 vairaveis
	
       !call interp_Ensemble_T126( kpds, Ensemble_T126_struc%npts,f2(:,iv),lb, scamtec%gridDesc,scamtec%nxpt,scamtec%nypt, varfield) 
       
       call interp_Ensemble_T126( kpds, Ensemble_T126_struc%npts,f2(:,iv),lb(:,iv), scamtec%gridDesc,&
                             scamtec%nxpt,scamtec%nypt, varfield, iret)    

    !
    ! padronizando pontos com undef
    !
       
       where(varfield .eq. undef)varfield = scamtec%udef

       !print*, trim(adjustl(myname_)),iv,minval(varfield(:)),maxval(varfield(:)),sum(varfield(:))/size(varfield,1)
       
    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       scamdata(1)%tmpfield(:,iv) = varfield(:)
       

    enddo
    
    DeAllocate(varfield)
    DeAllocate(lb)
    DeAllocate(f2)

  END SUBROUTINE Ensemble_T126_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_Ensemble_T126
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_Ensemble_T126( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)  

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
    character(len=*),parameter :: myname_=myname//'::interp_Ensemble_T126'

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
                         Ensemble_T126_struc%npts,nxpt*nypt,          &
                         Ensemble_T126_struc%rlat1, Ensemble_T126_struc%rlon1, &
                         Ensemble_T126_struc%w111, Ensemble_T126_struc%w121,   &
                         Ensemble_T126_struc%w211, Ensemble_T126_struc%w221,   &
                         Ensemble_T126_struc%n111, Ensemble_T126_struc%n121,   &
                         Ensemble_T126_struc%n211, Ensemble_T126_struc%n221,scamtec%udef,iret)

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

  END SUBROUTINE interp_Ensemble_T126
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE m_Ensemble_T126
	
