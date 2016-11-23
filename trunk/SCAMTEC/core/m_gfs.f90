!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: m_gfs.f90
!
! !DESCRIPTON: This module contains routines and functions to configure,
!              read and interpolate fields of the model to use in SCAMTEC.
!                 
!\\
!\\
! !INTERFACE:
!

MODULE m_gfs

!
! !USES:
!
  USE scamtec_module                ! SCAMTEC types
  USE SCAM_dataMOD, only : scamdata ! SCAMTEC data matrix
  USE interp_mod                    ! Interpolation module
  USE m_die                         ! Error Messages
  USE m_stdio                       ! Module to defines std. I/O parameters
  USE SCAM_MetForm                  ! Module to conversion of meteorological variables
  USE SCAM_Utils
  USE time_module, only: jul2cal, cal2jul ! Time operations
  USE m_ioutil
  USE m_string

  IMPLICIT NONE
  PRIVATE
!
! !PUBLIC TYPES:  
!
  type gfs_type_dec 

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

  end type gfs_type_dec

  type(gfs_type_dec) :: gfs_struc
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public :: gfs_read ! Function to read files from gfs model
  public :: gfs_init ! Function to initilize weights to interpolate fields
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
  character(len=*),parameter :: myname='m_gfs' 

CONTAINS
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  gfs_init
!
! !DESCRIPTION: This function initialize the matrices used to read 
!               and export fields to SCAMTEC
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE gfs_init()

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
    character(len=*),parameter :: myname_=myname//'::gfs_init'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    Allocate(gfs_struc%gridDesc(50))

    call gfs_domain()

    nx = int(scamtec%gridDesc(2))
    ny = int(scamtec%gridDesc(3))

    Allocate(gfs_struc%rlat1(nx*ny))
    Allocate(gfs_struc%rlon1(nx*ny))              
    Allocate(gfs_struc%n111(nx*ny))
    Allocate(gfs_struc%n121(nx*ny))
    Allocate(gfs_struc%n211(nx*ny))
    Allocate(gfs_struc%n221(nx*ny))
    Allocate(gfs_struc%w111(nx*ny))
    Allocate(gfs_struc%w121(nx*ny))
    Allocate(gfs_struc%w211(nx*ny))
    Allocate(gfs_struc%w221(nx*ny))

   !
   ! Initializing arrays of weights for interpolation in the field of SCAMTEC
   !

    call bilinear_interp_input(gfs_struc%gridDesc, scamtec%gridDesc,        &
                               int(scamtec%gridDesc(2)*scamtec%gridDesc(3)), &
                                     gfs_struc%rlat1,gfs_struc%rlon1,      &
                                     gfs_struc%n111,gfs_struc%n121,        &
                                     gfs_struc%n211,gfs_struc%n221,        &
                                     gfs_struc%w111,gfs_struc%w121,        &
                                     gfs_struc%w211,gfs_struc%w221)


  END SUBROUTINE gfs_init
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  gfs_domain
!
! !DESCRIPTION: This routine initilize domain parameters of gfs model
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE gfs_domain()

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
    character(len=*),parameter :: myname_=myname//'::gfs_domain'

    !
    ! DEBUG print
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
#endif

    gfs_struc%gridDesc     = 0 

    gfs_struc%gridDesc( 1) = 0        !Input grid type 
    gfs_struc%gridDesc( 2) = 720      !Number of points on a lat circle
    gfs_struc%gridDesc( 3) = 361      !Number of points on a meridian
    gfs_struc%gridDesc( 4) = -90.000  !Latitude of origin
    gfs_struc%gridDesc( 5) = 0.00     !Longitude of origin
    gfs_struc%gridDesc( 6) = 128      !8 bits (1 byte) related to resolution
                                      !(recall that 10000000 = 128), Table 7
    gfs_struc%gridDesc( 7) = 90.000   !Latitude of extreme point (lat_orig+(resolucao)*num_pontos)
    gfs_struc%gridDesc( 8) = 360.00     !Longitude of extreme point
    gfs_struc%gridDesc( 9) = 0.5      !N/S direction increment
    gfs_struc%gridDesc(10) = 0.5000   !(Gaussian) # lat circles pole-equator
    gfs_struc%gridDesc(11) = 0        !SCANNING MODE FLAG 
   
    gfs_struc%gridDesc(20) = 0  

    gfs_struc%npts = gfs_struc%gridDesc(2)*gfs_struc%gridDesc(3)


  END SUBROUTINE gfs_domain
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  gfs_read
!
! !DESCRIPTION: For a given file name, read fields from a gfs model,
!               interpolates to the SCAMTEC domain and export to SCAMTEC
!               matrices.                
!               
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE gfs_read(fname)
    IMPLICIT NONE
!
! !INPUT PARAMETERS:
!
  
    character(len=*), intent(IN) :: fname ! File name of the gfs model
    integer :: ferror
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
    character(len=*),parameter :: myname_=myname//'::gfs_read'

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
!    integer, dimension(20) :: pds5, pds7
    integer, dimension(20) :: pds7
    character(len=24), dimension(20) :: pds5
    logical*1, dimension(:,:), allocatable :: lb, lb2
    real, dimension(:,:), allocatable :: f
    real, dimension(:,:), allocatable :: f2
    real, dimension(:),   allocatable :: varfield
    character(10) :: dataini, datafinal 
    real    :: undef  = 9.999E20

    integer , parameter :: maxsize = 1548800
    real data(maxsize)
    character*200 invout, meta
    integer :: n, ierr


    !variaveis para somatorio precipitacao
    real, dimension(720*361) :: precgfs       !variavel de leitura  
    real, dimension(720*361) :: precgfs_total !variavel de leitura  
    
    character(len=1024) :: gfsprecipitation  ! Precipitation File Name 
   

    character(len=512) :: gfname2, fmt
    integer            :: gnymd, gnhms,gi
    integer            :: gfymd, gfhms, gaymd, gahms
    integer            :: gquant_arq_ant   !Quantidade de arquivos anterior  
    integer(I4B)       :: gatime , gftime
    real               :: gincr  

    !
    !  0. Hello
    !

#ifdef DEBUG
    WRITE(stdout,'(     2A)')'Hello from ', myname_
    WRITE(stdout,'( A,1X,A)')'Open File ::', trim(fname)
#endif

    !
    !

    lugb = 1
    lubi = 0
    j    = 0
    jpds = -1 
    !          Q   Q   Q   T   T   T   T  P   A   Z   Z   Z   U   U   U   V   V   V  PRE PRC
!    pds5 = (/ 51, 51, 51, 11, 11, 11, 11, 2, 54,  7,  7,  7, 33, 33, 33, 34, 34, 34, 61, 63/) !parameter
!    pds7 = (/925,850,500,925,850,500,250,000,000,850,500,250,850,500,250,850,500,250,000,000/) !htlev2

!obs.* deixar com espaco o valor do vetor
        pds5 = [':SPFH:80 m above ground:925 mb:           ', ':SPFH:80 m above ground:850 mb:           ', ':SPFH:80 m above ground:500 mb:           ', ':TMP:925 mb:            ', &
     &          ':TMP:850 mb:            ', ':TMP:500 mb:            ', ':TMP:250 mb:            ', ':PRMSL:mean sea level:  ', &
     &          ':PWAT:entire atmosphere ', ':HGT:850 mb:            ', ':HGT:500 mb:            ', ':HGT:250 mb:            ', &
     &          ':UGRD:850 mb:           ', ':UGRD:500 mb:           ', ':UGRD:250 mb:           ', ':VGRD:850 mb:           ', &
     &          ':VGRD:500 mb:           ', ':VGRD:250 mb:           ', ':APCP:surface:          ', ':ACPCP:surface:         ' ]
        pds7 = (/925,850,500,925,850,500,250,000,000,850,500,250,850,500,250,850,500,250,000,000/) !htlev2


    allocate(lb2(gfs_struc%npts,scamtec%nvar))
    allocate(lb(gfs_struc%npts,size(pds5)))
    allocate(f(gfs_struc%npts,size(pds5)))

  
    precgfs_total=0
  
    inquire (file=trim(fname), exist=file_exists)
    
    !Calculo para o incremento do tempo
    gincr=((100*hist%acumulo_obs)/24.0)/100.0   
    
    gatime=scamtec%atime    
    
      
    
    if (file_exists) then 
    
       gquant_arq_ant=hist%acumulo_exp/hist%acumulo_obs     !Calculando quantidade de arquivos anterior para abrir
        
        ! loop para somar o acumulo de precipitacao
        gftime=scamtec%ftime
        
        do gi=1, gquant_arq_ant
        
                    
            gaymd = gftime/100
            gahms = MOD(gatime,100) * 10000
        
            !gftime  = gatime   
            gfymd = gftime/100
            gfhms = MOD(gftime,100) * 10000
            
            !print*,'arquivos: ',gquant_arq_ant
            !print*,'atime, ftime',gatime,gftime
             
            !print*,'VENDO DATAS',gaymd, gahms, gfymd,gfhms
            
            
            gfsprecipitation=TRIM(Exper(1)%file)  
                          
            CALL str_template(gfsprecipitation, gaymd, gahms,gfymd, gfhms)
                       
            
#ifdef DEBUG
   WRITE(6,'( A,1X,A)')'Open File ::', trim(gfsprecipitation)
#endif             
        
       
        
    do i=1, maxsize
       data(i) = 0.0
    enddo

    call system('/opt/grads/2.0.a9/bin/wgrib2 '// trim(gfsprecipitation) //' > grib2tab.default.tab')


!   read data sequentially
    do iv = 1, size(pds5)
!       meta=pds5(iv)
!       call grb_rd(fname,'grib2tab.default.tab', meta,'-E',data,maxsize,n,invout,ierr)
       call grb_rd(gfsprecipitation,'grib2tab.default.tab', pds5(iv),'-E',data,maxsize,n,invout,ierr)
       
       if (ierr.eq.0) then
!          write(*,*) maxsize, ierr, data(1),trim(invout)
          f(:,iv)  = data(:)
          lb(:,iv) = .true.
          where(data.eq.undef) 
            lb(:,iv) = .false.
!             write(*,*) 'passou undef'
          end where
!          write(*,*) "f=", data
!          write(*,*) "minval=", minval(data), "maxval=", maxval(data)
!          stop
       else
!          write(*,*) "error meta=" , trim(meta)
          write(*,*) "error meta=" , trim(pds5(iv))
          lb(:,iv) = .false.
       endif
!       if (ierr.eq.0) write(*,*) maxsize, ierr, data(1),trim(invout)
!       if (ierr.ne.0) write(*,*) "error meta=" , trim(meta)
    enddo

    
     print*,'Min/ Max PREC GFS: ',minval(f(:,19)),maxval(f(:,19))
    
     !somatorio precipitacao
     precgfs(:)=f(:,19)
     
     precgfs_total(:)=precgfs_total(:)+precgfs(:)
     

      print*,'Min/ Max PREC GFS: ',minval(precgfs_total(:)),maxval(precgfs_total(:))
                              
      close(lugb)
            
      gftime=jul2cal(cal2jul(gftime)-gincr)
            
     enddo !fim loop precipitacao gfs 
     
   else

       ferror = 0


    endif       


 
    !
    ! Convertendo as Variaveis para as utilizadas no SCAMTEC
    ! * A lista de variaveis e as unidades utilizadas podem ser
    !   obtidas no modulo SCAM_dataMOD.f90
    !

    allocate(f2(gfs_struc%npts,scamtec%nvar))

!   f2(:, 1) = f(:, 4)*(1 + 0.61*(f(:,1)/(1-f(:,1)))) ! Vtmp @ 925 hPa [K]
!   f2(:, 2) = f(:, 5)*(1 + 0.61*(f(:,2)/(1-f(:,2)))) ! Vtmp @ 850 hPa [K]
!   f2(:, 3) = f(:, 6)*(1 + 0.61*(f(:,3)/(1-f(:,3)))) ! Vtmp @ 500 hPa [K]

    do i=1,gfs_struc%npts
       f2(i, 1) = tv(f(i,4),f(i,1)); lb2(i,1) = lb(i,1) ! Vtmp @ 925 hPa [K]
       f2(i, 2) = tv(f(i,5),f(i,2)); lb2(i,2) = lb(i,2) ! Vtmp @ 850 hPa [K]
       f2(i, 3) = tv(f(i,6),f(i,3)); lb2(i,3) = lb(i,3) ! Vtmp @ 500 hPa [K]  
    enddo
    
    f2(:, 4) = f(:, 5); lb2(:,4) = lb(:,5)              ! Absolute Temperature @ 850 hPa [K]            
    f2(:, 5) = f(:, 6); lb2(:,5) = lb(:,6)              ! Absolute Temperature @ 500 hPa [K]            
    f2(:, 6) = f(:, 7); lb2(:,6) = lb(:,7)              ! Absolute Temperature @ 250 hPa [K]            
    

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
     f2(:,22) = 0.0; lb2(:,22) = .false.                  ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]   
    else
     f2(:,21) = precgfs_total(:); lb2(:,21) = lb(:,19)    ! TOTAL PRECIPITATION @ 1000 hPa [kg/m2/day]     
     f2(:,22) = f(:,20); lb2(:,22) = lb(:,20)             ! CONVECTIVE PRECIPITATION @ 1000 hPa [kg/m2/day]   
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

!    do iv=1,size(pds5)+2
!      write(98)f2(:,iv)
!    enddo

    print*,'antes de interpolar GFS: >',minval(f2(:,21)),maxval(f2(:,21)) 
     write(45)f2(:,21)   

    DO iv=1,scamtec%nvar

       !write(*,*) "minval(",iv,")=", minval(f2(:,iv)), "maxval=", maxval(f2(:,iv))

       call interp_gfs( kpds, gfs_struc%npts,f2(:,iv),lb2(:,iv), scamtec%gridDesc,&
                         scamtec%nxpt,scamtec%nypt, varfield, iret)    

    !
    ! Transferindo para matriz temporaria do SCAMTEC
    !

       scamdata(1)%tmpfield(:,iv) = varfield(:)
       

    Enddo

  write(46)scamdata(1)%tmpfield(:,21)
   print*,'depois de interpolar: ',minval(scamdata(1)%tmpfield(:,21)),maxval(scamdata(1)%tmpfield(:,21))


  stop

    DeAllocate(varfield)

  END SUBROUTINE gfs_read
!
!EOC
!
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE:  interp_gfs
!
! !DESCRIPTION: this routine interpolates a givem field to the SCAMTEC domain 
!
!\\
!\\
! !INTERFACE:
!

  SUBROUTINE interp_gfs( kpds, npts,f,lb,gridDesc, nxpt, nypt, field1d, iret)

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
    character(len=*),parameter :: myname_=myname//'::interp_gfs'

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
                         gfs_struc%npts,nxpt*nypt,          &
                         gfs_struc%rlat1, gfs_struc%rlon1, &
                         gfs_struc%w111, gfs_struc%w121,   &
                         gfs_struc%w211, gfs_struc%w221,   &
                         gfs_struc%n111, gfs_struc%n121,   &
                         gfs_struc%n211, gfs_struc%n221,scamtec%udef,iret)



!    k = 0
!    do j = 1, nypt
!       do i = 1, nxpt
!          varfield(i,j) = field1d(i+k)
!       enddo
!       k = k + nxpt
!    enddo

  END SUBROUTINE interp_gfs
!
!EOC
!-----------------------------------------------------------------------------!

END MODULE m_gfs
