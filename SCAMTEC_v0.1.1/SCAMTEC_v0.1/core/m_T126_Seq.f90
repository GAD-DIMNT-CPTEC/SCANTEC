!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_T126_Seq.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE m_T126_Seq

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
  type T126_Seq_type_dec 

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

  end type T126_Seq_type_dec

  type(T126_Seq_type_dec) :: T126_Seq_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: T126_Seq_read ! Function to read files from T126_Seq model
  public :: T126_Seq_init ! Function to initilize weights to interpolate fields
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
  character(len=*),parameter :: myname='m_T126_Seq'

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  T126_Seq_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE T126_Seq_init()
  
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
    character(len=*),parameter :: myname_=myname//'::T126_Seq_init'

    !
    ! DEBUG print
    !
    
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

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(T126_Seq_struc%gridDesc, scamtec%gridDesc,            &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)),         &
                                     T126_Seq_struc%rlat1,T126_Seq_struc%rlon1,      &
                                     T126_Seq_struc%n111,T126_Seq_struc%n121,        &
                                     T126_Seq_struc%n211,T126_Seq_struc%n221,        &
                                     T126_Seq_struc%w111,T126_Seq_struc%w121,        &
                                     T126_Seq_struc%w211,T126_Seq_struc%w221)


  END SUBROUTINE T126_Seq_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  T126_Seq_domain
!
! !DESCRIPTION: This routine initilize domain parameters of T126_Seq model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE T126_Seq_domain()
  
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
    character(len=*),parameter :: myname_=myname//'::T126_Seq_domain'
    
    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    T126_Seq_struc%gridDesc     = 0 

    T126_Seq_struc%gridDesc( 1) = 4         !Input grid type (4=Gaussian)
    T126_Seq_struc%gridDesc( 2) = 384       !Number of points on a lat circle
    T126_Seq_struc%gridDesc( 3) = 192       !Number of points on a meridian
    T126_Seq_struc%gridDesc( 4) = 89.2842   !Latitude of origin
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
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  T126_Seq_read
!
! !DESCRIPTION: For a given file name, read fields from a T126_Seq model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!
  
  SUBROUTINE T126_Seq_read(fname)
    IMPLICIT NONE
    !
    ! !INPUT PARAMETERS:
    !
    
    character(len=*), intent(IN) :: fname ! File name of the T126_Seq model
    
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
    character(len=*),parameter :: myname_=myname//'::T126_Seq_read'
  
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
    ! Parametros para ler os dados do arquivo de T126_Seq
    !

    integer :: NV1lev = 10! Numero de Variaveis com 1 Nivel
    integer :: NLev   = 18 ! Numero de Niveis
    
    character(10) :: dataini, datafinal 
    
    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
    WRITE(6,'( A,1X,A)')'Open File ::', trim(fname)
#endif
 
    allocate(lb(T126_Seq_struc%npts,scamtec%nvar))
	lb = .true.    	
    lugb = 1

    inquire (file=trim(fname), exist=file_exists)
	
    if (file_exists) then
     
!Verifica se é analise incial o index z que é da leitura começa com 208
! Pensar como fazer isso de forma mais adequada	
		
	    do w=1, 300	
		
		    if(trim(fname(w:w+6)) .eq. 'GPOSDAO')then
			    dataini=trim(fname(w+7:w+16))
			    datafinal=trim(fname(w+17:w+26))
		    endif			
		
	    enddo 
	   	
	    if(dataini .eq. datafinal)then
		    y=208
	    else
		    y=214
	    endif
		
!-------------------------------------------------------------------------------------------	
       ! abrindo binario T126_Seq
	    OPEN (UNIT=lugb,FILE=trim(fname),FORM='unformatted', CONVERT='BIG_ENDIAN',STATUS ='Unknown')  
	    
	    !Alocando memoria para o arquivo binario
	    allocate(binario(T126_Seq_struc%npts,y)) 
		
	    do z=1, y
			read(UNIT=lugb)binario(:,z)
			!write(15)binario(:,z)
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
    allocate(f2(T126_Seq_struc%npts,scamtec%nvar))
    allocate(f(T126_Seq_struc%npts,19))
   
   
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
   if(y .eq. 208)then
        f(:,18) = undef            ! PREC @ 000 hPa [kg/m2/day]
        f(:,19) = undef            ! PREV @ 000 hPa [kg/m2/day]
   else
        f(:,18) = binario(:,209)    ! PREC @ 000 hPa [kg/m2/day]
        f(:,19) = binario(:,210)    ! PREV @ 000 hPa [kg/m2/day]
   endif
   deallocate (binario)
        
   
   ! Calculo para Temperatira Virtual
   !f2(:,1) = f(:,1)*(1 + 0.61*(f(:,5)/(1-f(:,5)))) ! Vtmp @ 925 hPa [K]
   !f2(:,2) = f(:,2)*(1 + 0.61*(f(:,6)/(1-f(:,6)))) ! Vtmp @ 850 hPa [K]
   !f2(:,3) = f(:,3)*(1 + 0.61*(f(:,7)/(1-f(:,7)))) ! Vtmp @ 500 hPa [K]
 
   do i=1,T126_Seq_struc%npts
        f2(i, 1) = tv(f(i,1),f(i,5)) ! Vtmp @ 925 hPa [K]
        f2(i, 2) = tv(f(i,2),f(i,6)) ! Vtmp @ 850 hPa [K]
        f2(i, 3) = tv(f(i,3),f(i,7)) ! Vtmp @ 500 hPa [K]  
  enddo
 
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
   f2(:,16) = f(:,18)                                ! PREC @ 000 hPa [kg/m2/day]
   f2(:,17) = f(:,19)                                ! PREV @ 000 hPa [kg/m2/day]
  
     
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
	
       !call interp_T126_Seq( kpds, T126_Seq_struc%npts,f2(:,iv),lb, scamtec%gridDesc,scamtec%nxpt,scamtec%nypt, varfield) 
       
       call interp_T126_Seq( kpds, T126_Seq_struc%npts,f2(:,iv),lb(:,iv), scamtec%gridDesc,&
                             scamtec%nxpt,scamtec%nypt, varfield, iret)    
	
	!
    ! padronizando pontos com undef
    !
       
       where(varfield .eq. undef)varfield = scamtec%udef
	    
		
    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       scamdata(1)%tmpfield(:,iv) = varfield(:)
       

    enddo
    
    DeAllocate(varfield)
    DeAllocate(lb)
    DeAllocate(f2)

  END SUBROUTINE T126_Seq_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_T126_Seq
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_T126_Seq( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)  

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
    character(len=*),parameter :: myname_=myname//'::interp_T126_Seq'

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
                         T126_Seq_struc%npts,nxpt*nypt,          &
                         T126_Seq_struc%rlat1, T126_Seq_struc%rlon1, &
                         T126_Seq_struc%w111, T126_Seq_struc%w121,   &
                         T126_Seq_struc%w211, T126_Seq_struc%w221,   &
                         T126_Seq_struc%n111, T126_Seq_struc%n121,   &
                         T126_Seq_struc%n211, T126_Seq_struc%n221,scamtec%udef,iret)

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

  END SUBROUTINE interp_T126_Seq
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE m_T126_Seq
	
