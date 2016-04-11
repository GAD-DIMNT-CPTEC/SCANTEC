!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_grb_ensemble_t126.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE m_grb_ensemble_t126

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
  type Grb_ensemble_t126_type_dec 

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

  end type Grb_ensemble_t126_type_dec

  type(Grb_ensemble_t126_type_dec) :: Grb_ensemble_t126_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: Grb_ensemble_t126_read ! Function to read files from Grb_ensemble_t126 model
  public :: Grb_ensemble_t126_init ! Function to initilize weights to interpolate fields
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
  character(len=*),parameter :: myname='m_grb_ensemble_t126' 

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Grb_ensemble_t126_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE Grb_ensemble_t126_init()

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
    character(len=*),parameter :: myname_=myname//'::Grb_ensemble_t126_init'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    Allocate(Grb_ensemble_t126_struc%gridDesc(50))

    call Grb_ensemble_t126_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(Grb_ensemble_t126_struc%rlat1(nx*ny))
    Allocate(Grb_ensemble_t126_struc%rlon1(nx*ny))              
    Allocate(Grb_ensemble_t126_struc%n111(nx*ny))
    Allocate(Grb_ensemble_t126_struc%n121(nx*ny))
    Allocate(Grb_ensemble_t126_struc%n211(nx*ny))
    Allocate(Grb_ensemble_t126_struc%n221(nx*ny))
    Allocate(Grb_ensemble_t126_struc%w111(nx*ny))
    Allocate(Grb_ensemble_t126_struc%w121(nx*ny))
    Allocate(Grb_ensemble_t126_struc%w211(nx*ny))
    Allocate(Grb_ensemble_t126_struc%w221(nx*ny))

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(Grb_ensemble_t126_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     Grb_ensemble_t126_struc%rlat1,Grb_ensemble_t126_struc%rlon1,      &
                                     Grb_ensemble_t126_struc%n111,Grb_ensemble_t126_struc%n121,        &
                                     Grb_ensemble_t126_struc%n211,Grb_ensemble_t126_struc%n221,        &
                                     Grb_ensemble_t126_struc%w111,Grb_ensemble_t126_struc%w121,        &
                                     Grb_ensemble_t126_struc%w211,Grb_ensemble_t126_struc%w221)


  END SUBROUTINE Grb_ensemble_t126_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Grb_ensemble_t126_domain
!
! !DESCRIPTION: This routine initilize domain parameters of Grb_ensemble_t126 model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE Grb_ensemble_t126_domain()

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
    character(len=*),parameter :: myname_=myname//'::Grb_ensemble_t126_domain'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    Grb_ensemble_t126_struc%gridDesc     = 0 

    Grb_ensemble_t126_struc%gridDesc( 1) = 4         !Input grid type (4=Gaussian)
    Grb_ensemble_t126_struc%gridDesc( 2) = 384       !Number of points on a lat circle
    Grb_ensemble_t126_struc%gridDesc( 3) = 192       !Number of points on a meridian
    Grb_ensemble_t126_struc%gridDesc( 4) = 89.2842   !Latitude of origin
    Grb_ensemble_t126_struc%gridDesc( 5) = 0.0       !Longitude of origin
    Grb_ensemble_t126_struc%gridDesc( 6) = 128       !8 bits (1 byte) related to resolution
                                                     !(recall that 10000000 = 128), Table 7
    Grb_ensemble_t126_struc%gridDesc( 7) = -89.2842  !Latitude of extreme point
    Grb_ensemble_t126_struc%gridDesc( 8) = -0.9375   !Longitude of extreme point
    Grb_ensemble_t126_struc%gridDesc( 9) =  0.9375    !N/S direction increment
    Grb_ensemble_t126_struc%gridDesc(10) =  96       !(Gaussian) # lat circles pole-equator
    Grb_ensemble_t126_struc%gridDesc(20) =  0.0  

    Grb_ensemble_t126_struc%npts = Grb_ensemble_t126_struc%gridDesc(2)*Grb_ensemble_t126_struc%gridDesc(3)

  END SUBROUTINE Grb_ensemble_t126_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  Grb_ensemble_t126_read
!
! !DESCRIPTION: For a given file name, read fields from a Grb_ensemble_t126 model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE Grb_ensemble_t126_read(fname)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  
    character(len=*), intent(IN) :: fname ! File name of the Grb_ensemble_t126 model

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
    character(len=*),parameter :: myname_=myname//'::Grb_ensemble_t126_read'

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
    integer, dimension(20) :: pds5, pds7
    logical*1, dimension(:,:), allocatable :: lb, lb2
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:),   allocatable :: varfield
    character(10) :: dataini, datafinal 

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

    lugb = 1
    lubi = 0
    j    = 0
    jpds = -1 
    !          Q   Q   Q   T   T   T   T  P   A   Z   Z   Z   U   U   U   V   V   V  PRE PRC(Modifica para Neve)
    pds5 = (/ 51, 51, 51, 11, 11, 11, 11, 2, 54,  7,  7,  7, 33, 33, 33, 34, 34, 34, 61, 64/) !parameter
    pds7 = (/925,850,500,925,850,500,250,000,000,850,500,250,850,500,250,850,500,250,000,000/) !htlev2

    allocate(lb2(Grb_ensemble_t126_struc%npts,scamtec%nvar))
    allocate(lb(Grb_ensemble_t126_struc%npts,size(pds5)))
    allocate(f(Grb_ensemble_t126_struc%npts,size(pds5)))

  
    inquire (file=trim(fname), exist=file_exists)
    
    do i=1, 300	
		
		    if(trim(fname(i:i+3)) .eq. 'GPOS')then
			    dataini=trim(fname(i+7:i+16))
			    datafinal=trim(fname(i+17:i+26))
		    endif			
		
	    enddo 
	   	
	    if(dataini .eq. datafinal)then
		    y=size(pds5)-2
	    else
		    y=size(pds5)
	    endif
	    	    	    
    
    do iv = 1, y!size(pds5)
             
       if (file_exists) then 

          jpds(5) = pds5(iv)
          jpds(7) = pds7(iv)

          lugb    = lugb + iv

          call baopenr(lugb,fname,iret)

          if(iret.eq.0) then
             call getgb(lugb,lubi,Grb_ensemble_t126_struc%npts,j,jpds,jgds,kf,k,kpds, &
                  gridDesc,lb(:,iv),f(:,iv),gbret)

             if (gbret.ne.0)then
                stat = gbret
                call perr(myname_,'getgb("'//     &
                          trim(fname)//'")',gbret &
                         )
                return
             endif

          else
             stat = iret
             call perr(myname_,'baopenr("'//     &
                       trim(fname)//'")',iret &
                      )
             return
          endif

          call baclose(lugb,jret)
          if(jret.ne.0) then
            stat = jret
            call perr(myname_,'deallocate()',jret)
            return
          endif
       else
          stat = -1

          deallocate(f)
          deallocate(lb)
          
          call perr(myname_,'File Not Found: '//trim(fname),stat)
          return

       endif

    enddo

    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !

    allocate(f2(Grb_ensemble_t126_struc%npts,scamtec%nvar))

!   f2(:, 1) = f(:, 4)*(1 + 0.61*(f(:,1)/(1-f(:,1)))) ! Vtmp @ 925 hPa [K]
!   f2(:, 2) = f(:, 5)*(1 + 0.61*(f(:,2)/(1-f(:,2)))) ! Vtmp @ 850 hPa [K]
!   f2(:, 3) = f(:, 6)*(1 + 0.61*(f(:,3)/(1-f(:,3)))) ! Vtmp @ 500 hPa [K]

    do i=1,Grb_ensemble_t126_struc%npts
       f2(i, 1) = tv(f(i,4),f(i,1)); lb2(i,1) = lb(i,1) ! Vtmp @ 925 hPa [K]
       f2(i, 2) = tv(f(i,5),f(i,2)); lb2(i,2) = lb(i,2) ! Vtmp @ 850 hPa [K]
       f2(i, 3) = tv(f(i,6),f(i,3)); lb2(i,3) = lb(i,3) ! Vtmp @ 500 hPa [K]  
    enddo
    
    f2(:, 4) = f(:, 5); lb2(:,4) = lb(:,5)              ! Absolute Temperature @ 850 hPa [K]             4 paulo dias
    f2(:, 5) = f(:, 6); lb2(:,5) = lb(:,6)              ! Absolute Temperature @ 500 hPa [K]             5 paulo dias
    f2(:, 6) = f(:, 7); lb2(:,6) = lb(:,7)              ! Absolute Temperature @ 250 hPa [K]             6 paulo dias
    

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
           
    if (y .eq. size(pds5)-2)then
    f2(:,21) = 0.0; lb2(:,21) = .false.                  ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]     
    f2(:,22) = 0.0; lb2(:,22) = .false.                  ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day] - Modificado para SNOWFALL (64)  
    else
    f2(:,21) = f(:,19); lb2(:,21) = lb(:,19)             ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]     
    f2(:,22) = f(:,20); lb2(:,22) = lb(:,20)             ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day] - Modificado para SNOWFALL (64)
    endif
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

       call interp_Grb_ensemble_t126( kpds, Grb_ensemble_t126_struc%npts,f2(:,iv),lb2(:,iv), scamtec%gridDesc,&
                         scamtec%nxpt,scamtec%nypt, varfield, iret)    

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !

       scamdata(1)%tmpfield(:,iv) = varfield(:)


    Enddo
    !write(99)scamdata(1)%tmpfield(:,1)


    DeAllocate(varfield)

  END SUBROUTINE Grb_ensemble_t126_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_Grb_ensemble_t126
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_Grb_ensemble_t126( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)

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
    character(len=*),parameter :: myname_=myname//'::interp_Grb_ensemble_t126'

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
                         Grb_ensemble_t126_struc%npts,nxpt*nypt,          &
                         Grb_ensemble_t126_struc%rlat1, Grb_ensemble_t126_struc%rlon1, &
                         Grb_ensemble_t126_struc%w111, Grb_ensemble_t126_struc%w121,   &
                         Grb_ensemble_t126_struc%w211, Grb_ensemble_t126_struc%w221,   &
                         Grb_ensemble_t126_struc%n111, Grb_ensemble_t126_struc%n121,   &
                         Grb_ensemble_t126_struc%n211, Grb_ensemble_t126_struc%n221,scamtec%udef,iret)

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

  END SUBROUTINE interp_Grb_ensemble_t126
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE m_grb_ensemble_t126
