MODULE m_brams

  USE scamtec_module
  USE SCAM_dataMOD, only : scamdata
  USE interp_mod
  USE bramsIO
  USE SCAM_OutputMOD , only: write_2d
  use m_radiossonda
  USE SCAM_Utils, only: Refer
  IMPLICIT NONE
  PRIVATE
  
  type brams_type_dec 

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

  end type brams_type_dec

  type(brams_type_dec) :: brams_struc

  public :: brams_read
  public :: brams_init

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='m_brams'

	contains


!#######################################################################################################

 subroutine brams_init()
  
  integer :: nx
  integer :: ny

  character(len=*),parameter :: myname_=myname//'::brams_init'

#ifdef DEBUG
  	write(6,'(     2A)')'Hello from ', myname_
#endif

	allocate(brams_struc%gridDesc(50))

	call brams_domain()

	nx = int(scamtec%gridDesc(2))
	ny = int(scamtec%gridDesc(3))

	allocate(brams_struc%rlat1(nx*ny))
	allocate(brams_struc%rlon1(nx*ny))              
	allocate(brams_struc%n111(nx*ny))
	allocate(brams_struc%n121(nx*ny))
	allocate(brams_struc%n211(nx*ny))
	allocate(brams_struc%n221(nx*ny))
	allocate(brams_struc%w111(nx*ny))
	allocate(brams_struc%w121(nx*ny))
	allocate(brams_struc%w211(nx*ny))
	allocate(brams_struc%w221(nx*ny))

	call bilinear_interp_input(brams_struc%gridDesc, scamtec%gridDesc,        &
                      		   int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                               	   brams_struc%rlat1,brams_struc%rlon1,      &
                               	   brams_struc%n111,brams_struc%n121,        &
                               	   brams_struc%n211,brams_struc%n221,        &
                               	   brams_struc%w111,brams_struc%w121,        &
                               	   brams_struc%w211,brams_struc%w221)


 end subroutine brams_init
 
!####################################################################################################### 

 subroutine brams_domain()

  character(len=*),parameter :: myname_=myname//'::brams_domain'

#ifdef DEBUG
	write(6,'(     2A)')'Hello from ', myname_
#endif


    	brams_struc%gridDesc     = 0 

    	brams_struc%gridDesc( 1) = 0         !Input grid type (4=Gaussian)
    	brams_struc%gridDesc( 2) = 343       !Number of points on a lat circle
    	brams_struc%gridDesc( 3) = 291        !Number of points on a meridian
    	brams_struc%gridDesc( 4) = -47   !Latitude of origin
	brams_struc%gridDesc( 5) = -82.133       !Longitude of origin
    	brams_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                             !(recall that 10000000 = 128), Table 7
    	brams_struc%gridDesc( 7) = 11.14  !Latitude of extreme point
    	brams_struc%gridDesc( 8) = -26.743    !Longitude of extreme point
    	brams_struc%gridDesc( 9) = 0.170     !N/S direction increment (delta y)
    	brams_struc%gridDesc(10) = 0.191       !(Gaussian) # lat circles pole-equator (delta x)
    	brams_struc%gridDesc(20) = 0.0  

    	brams_struc%npts = brams_struc%gridDesc(2)*brams_struc%gridDesc(3)

 end subroutine brams_domain
 
!####################################################################################################### 
!->Inicio do comentario
!->Subrotina: brams_read
!->Descrição: Leitura do arquivo binario do modelo BRAMS
!->Variaveis
!fname			: nome do arquivo 
!undefPtsPerGrid	: 
!vapos			: valores procedentes do arquivo ctl levando em consideração os niveis das variaveis
!unidade		: unit do arquivo
!i,j,k			: variaveis de controle
!ferror			:?
!gbret			:?
!jret			:?
!gridDesc		:?
!kpds			:?
!iv			:?
!nx			: numero de pontos em X
!ny			: numero de pontos em Y
!lb			: valor logicogic paramascar de undef
!f			: matrix com os valord fome
!f2			: matriz temporaria para os modelos
!varpos			: posição da variavel na matriz
!OutFName		: variavel por nomear arquivo de saida da rotina write 2d
!####################################################################################################### 

 subroutine brams_read(fname)

 character(len=*), intent(IN) :: fname
 integer :: ferror

 integer :: iret
 integer :: undefPtsPerGrid
 logical :: file_exists
 integer :: gbret,jret,gridDesc(200),kpds(200)
 integer :: i,j,k
 integer :: iv
 integer :: nx
 integer :: ny
 logical*1, dimension(:), allocatable :: lb
 real, dimension(:,:), allocatable  :: f
 real, dimension(:,:), allocatable :: f2
 real, dimension(:,:), allocatable :: varfield
 character(len=*),parameter :: myname_=myname//'::brams_read'
 integer,dimension(17)::varpos
 integer::unidade
 character(len=50) :: OutFName


#ifdef DEBUG
	write(6,'(     2A)')'Hello from ', myname_
	write(6,'( A,1X,A)')'Open File ::', trim(fname)
#endif



	! valores procedentes do arquivo ctl levando em consideraÃ§Ã£o os niveis das variaveis

	!          Q    Q    Q    T   T   T   P    A    Z    Z   Z   U   U  U   V   V   V
	varpos = (/140, 147, 152, 64, 71, 76, 100, 102, 109, 83, 90, 95, 3, 43, 45, 52, 41 /) !parameter

     
     	undefPtsPerGrid = 0
     
	allocate(lb(brams_struc%npts))
	allocate(f(brams_struc%npts,size(varpos)))
	
	inquire (file=trim(fname), exist=file_exists)
	if(file_exists)then 

		call open_file(fname,unidade,brams_struc%npts,iret)
		if (iret .eq. 0) then

			call read_file(unidade,varpos(1:3),f(:,1:3))	! geo
			
			
			do i = 1, brams_struc%npts
				do iv = 1, 3
					
					if(f(i,iv) .gt. -9999.0) f(i,iv)=f(i,iv)*9.8				! conversÃ£o de m para gpm
					
				end do
			end do

			
			call read_file(unidade,varpos(4:6),f(:,4:6))	! ue_avg

			call read_file(unidade,varpos(7:9),f(:,7:9))	! rv

			
			do i = 1, brams_struc%npts
				do iv = 7, 9
					if(f(i,iv) .gt. -9999.0)f(i,iv)=f(i,iv)/1000.0			! conversÃ£o g/kg para kg/kg
				end do
			end do
		
			call read_file(unidade,varpos(10:12),f(:,10:12)) ! ve_avg
		
			call read_file(unidade,varpos(13:13),f(:,13:13)) ! sea_press
		
			call read_file(unidade,varpos(14:16),f(:,14:16)) ! tempc
			
			do i = 1, brams_struc%npts
				if(f(i,14) .gt. -9999.0 .and. f(i,7) .gt. -9999.0) f(i,14)=(f(i,14)+273.15)*((1+0.608)*(f(i,7))) ! virtual temp.
				if(f(i,15) .gt. -9999.0 .and. f(i,8) .gt. -9999.0) f(i,15)=(f(i,15)+273.15)*((1+0.608)*(f(i,8))) ! virtual temp.
				if(f(i,16) .gt. -9999.0 .and. f(i,9) .gt. -9999.0) f(i,16)=(f(i,16)+273.15)*((1+0.608)*(f(i,9))) ! virtual temp.
			end do
			
			call read_file(unidade,varpos(17:17),f(:,17:17)) ! pwt
			
			do i = 1, brams_struc%npts
				if(f(i,17) .gt. -9999.0)f(i,17) = f(i,17) * 10 ! agpl
			end do
		else
		
			stop 'corrupted file!'
		
		end if

	else
	
		stop 'FILE NOT FOUND!'
	endif
	
	!ATENCAO!
	!-- variaveis do modelo diferem no posicionamento do undef --! 
	!-- ao inves de utilizar uma mascara recomendo controlar computação pela variavel undef --!
	!lb = .true.
	!where(f(:,1) .lt. -9999.999) lb = .false.

	if (Refer%Id .eq. 5) then

		CALL brams_radio(f,brams_struc%npts,size(varpos))


	endif 
	
	allocate(f2(brams_struc%npts,scamtec%nvar))
   
	f2(:, 1) = f(:, 14)				 ! Vtmp @ 925 hPa [K]
 	f2(:, 2) = f(:, 15) 				 ! Vtmp @ 850 hPa [K]
	f2(:, 3) = f(:, 16)				 ! Vtmp @ 500 hPa [K]
	f2(:, 4) = f(:, 13)                                ! PSNM [hPa]
	f2(:, 5) = f(:, 7)				   ! Umes @ 925 hPa [Kg/Kg]
	f2(:, 6) = f(:, 17)				   ! Agpl @ 925 hPa [Kg/m2]
	f2(:, 7) = f(:, 1)				   ! Zgeo @ 850 hPa [gpm]
	f2(:, 8) = f(:, 2)				   ! Zgeo @ 500 hPa [gpm]
	f2(:, 9) = f(:, 3)				   ! Zgeo @ 250 hPa [gpm]
	f2(:,10) = f(:, 4)				   ! Uvel @ 850 hPa [m/s]
	f2(:,11) = f(:, 5)				   ! Uvel @ 500 hPa [m/s]
	f2(:,12) = f(:, 6)				   ! Uvel @ 250 hPa [m/s]
	f2(:,13) = f(:, 10)				   ! Vvel @ 850 hPa [m/s]
	f2(:,14) = f(:, 11)				   ! Vvel @ 500 hPa [m/s]
	f2(:,15) = f(:, 12)				   ! Vvel @ 250 hPa [m/s]


deallocate(f)
allocate(f(brams_struc%npts,scamtec%nvar))
	if (Refer%Id .ne. 5) then
	


    !
    ! invertendo y
    !

	!allocate(f(brams_struc%npts,scamtec%nvar))

    

	nx = int(brams_struc%gridDesc(2))
	ny = int(brams_struc%gridDesc(3))

 
 	k = 0
 	do j = 1, ny
    		do i = 1, nx
       			f(i+(nx*ny)-nx-k,:) = f2(i+k,:)
    		enddo
  		k = k + nx
	enddo
	endif
	f=f2
 	deallocate(f2)


    !
    ! Interpolando para a grade do SCAMTEC
    !

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))
    
    allocate(varfield(nx,ny))
    DO iv = 1, scamtec%nvar

	lb = .false.
	where(f(:,iv) .gt. -9999.0) lb = .true.

       	call interp_brams( kpds, brams_struc%npts,f(:,iv),lb, scamtec%gridDesc,&
                              scamtec%nxpt,scamtec%nypt, varfield)   


    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !
       scamdata(1)%tmpfield(:,:,iv) = varfield(:,:)
print*, maxval(scamdata(1)%tmpfield(:,:,10)),minval(scamdata(1)%tmpfield(:,:,10)),"m_brams"


    enddo


  END SUBROUTINE brams_read

  SUBROUTINE interp_brams( kpds, npts,f,lb,gridDesc, nxpt, nypt, varfield)  

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

    character(len=*),parameter :: myname_=myname//'::interp_brams'

#ifdef DEBUG
    WRITE(6,'(     2A)')'Hello from ', myname_
#endif

    ip    = 0
    ipopt = 0
    km    = 1
    ibi   = 1
    lo    = .true.

    call bilinear_interp(gridDesc,ibi,lb,f,ibo,lo,field1d,   &
                         brams_struc%npts,nxpt*nypt,          &
                         brams_struc%rlat1, brams_struc%rlon1, &
                         brams_struc%w111, brams_struc%w121,   &
                         brams_struc%w211, brams_struc%w221,   &
                         brams_struc%n111, brams_struc%n121,   &
                         brams_struc%n211, brams_struc%n221,scamtec%udef,iret)

    k = 0
    do j = 1, nypt
       do i = 1, nxpt
          varfield(i,j) = field1d(i+k)
       enddo
       k = k + nxpt
    enddo

  END SUBROUTINE interp_brams

END MODULE m_brams
