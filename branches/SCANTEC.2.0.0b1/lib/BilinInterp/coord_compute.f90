module coord_compute

   implicit none
   private
 
   integer, public, parameter :: R8 = selected_real_kind(15)
   integer, public, parameter :: R4 = selected_real_kind( 6)

   real(r8), parameter :: pi = 3.141592653589793
   real(r8), parameter :: dpr = 180.0/pi

   public :: compute_earth_coord
   public :: compute_grid_coord


   character(len=64),parameter :: myname='Coord_Compute'
   contains
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! !ROUTINE: compute_earth_coord
!  \label{compute_earth_coord}
!
!
! !DESCRIPTION: This subroutine computes the earth coordinates (lat/lon values) 
!               of the specified domain. This routine is based on the grid
!               decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!  The current code recognizes the following projections:
!             (gridDesc(1)=000) equidistant cylindrical
!             (gridDesc(1)=004) gaussian cylindrical
!
!
! !INTERFACE:
!
subroutine compute_earth_coord(gridDesc, Udef, rlon, rlat, iret )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
  real,               intent(in   ) :: Udef      ! value to set invalid output data


!
! !OUTOUT PARAMETERS:
!
  real, dimension(:), intent(inout) :: rlat ! latitudes in degrees
  real, dimension(:), intent(inout) :: rlon ! longitudes in degrees
  integer, optional,  intent(  out) :: iret ! return code (0-success)

!
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!
! !REMARKS:
!
!    - gen_xypts: specifies whether this routine should generate
!                 xpts and ypts.  Example: This routine is called 
!                 twice when computing weights and neighbours for 
!                 the conservative intepolation scheme.  The first 
!                 call must generate xpts and ypts.  The second call
!                 should not.
!
! !SEE ALSO:
!
!   - EarthCoord_latlon_( ) - computes the earth coordinates of a latlon grid
!   - EarthCoord_gauss_( ) - computes the earth coordinates of a gaussian cylindrical grid
!
!EOP
!-------------------------------------------------------------------------!
!BOC
  character(len=64), parameter :: myname_=trim(myname)//' :: compute_earth_coord( )'
  integer :: nret

  if(present(iret)) iret = 0

  select case (int(gridDesc(1)))

     case ( 0 ) ! Equidistant cylindrical
        call EarthCoord_latlon_(gridDesc, Udef, rlon, rlat, nret)
        if(present(iret)) iret = nret
!     case ( 1 ) ! Mercator
!        call EarthCoord_merc_(gridDesc,npts,fill,xpts,ypts,&
!                                      rlon,rlat,nret)
!        if(present(iret)) iret = nret
!     case ( 3 ) ! Lambert Conformal
!        call EarthCoord_lambert_(gridDesc,npts,fill,xpts,ypts,&
!                                         rlon,rlat,nret)
!        if(present(iret)) iret = nret
     case ( 4 ) ! gaussian cylindrical
        call EarthCoord_gauss_(gridDesc, Udef, rlon, rlat, nret)
        if(present(iret)) iret = nret
!     case ( 5 ) ! Polar Stereographic 
!        call EarthCoord_polar_(gridDesc,npts,fill,xpts,ypts,&
!                                    rlon,rlat,nret)
!        if(present(iret)) iret = nret
     case default 
        print*, 'Unrecognized Projection .... '
        print*, 'Program stopping ..'
        stop

  end select


end subroutine compute_earth_coord
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! !ROUTINE: compute_earth_coord
!
!
! !DESCRIPTION: This subroutine computes the grid coordinates (cartesian) 
!               of the specified domain. This routine is based on the grid
!               decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!
!  The current code recognizes the following projections:
!             (gridDesc(1)=000) equidistant cylindrical
!             (gridDesc(1)=004) gaussian cylindrical
!
!
! !INTERFACE:
!
!subroutine compute_grid_coord(gridDesc, rlon, rlat, Udef, &
!                              xpts, ypts, pt, iret            &
!                             )
subroutine compute_grid_coord(gridDesc, rlon, rlat, Udef, &
                              xpts, ypts, iret            &
                             )

!
! !INPUT PARAMETERS:
!

  real, dimension(:), intent(in   ) :: gridDesc ! grid description parameters
  real,               intent(in   ) :: Udef     ! value to set invalid output data
  real, dimension(:), intent(in   ) :: rlat     ! latitudes in degrees
  real, dimension(:), intent(in   ) :: rlon     ! longitudes in degrees


!
! !OUTOUT PARAMETERS:
!

  real, dimension(:), intent(inout) :: xpts ! grid x point coordinates
  real, dimension(:), intent(inout) :: ypts ! grid y point coordinates
!  integer,            intent(inout) :: pt(:,:)
  integer, optional,  intent(  out) :: iret ! return code (0-success)

!
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
!
! !REMARKS:
!
!
! !SEE ALSO:
!
!   - GridCoord_latlon_( ) - computes the grid coordinates of a latlon grid
!   - GridCoord_gauss_( ) - computes the grid coordinates of a gaussian cylindrical grid
!
!EOP
!-------------------------------------------------------------------------!
!BOC
  character(len=64), parameter :: myname_=trim(myname)//' :: compute_grid_coord( )'
  integer :: nret

  if(present(iret)) iret = 0

  select case (int(gridDesc(1)))

     case ( 0 ) ! Equidistant cylindrical
!        call GridCoord_latlon_(gridDesc, Udef, rlon, rlat, &
!                               xpts, ypts, pt, nret            &
!                             )
        call GridCoord_latlon_(gridDesc, Udef, rlon, rlat, &
                               xpts, ypts, nret            &
                              )
        if(present(iret)) iret = nret
!     case ( 1 ) ! Mercator
!        call GridCoord_merc_(gridDesc, rlon, rlat, Udef,&
!                             xpts, ypts, nret           &
!                            )
!        if(present(iret)) iret = nret
!     case ( 3 ) ! Lambert Conformal
!        call GridCoord_lambert_(gridDesc, rlon, rlat, Udef,&
!                                xpts, ypts, nret           &
!                               )
!        if(present(iret)) iret = nret
     case ( 4 ) ! gaussian cylindrical
!        call GridCoord_gauss_(gridDesc, Udef, rlon, rlat,&
!                              xpts, ypts, pt, nret           &
!                             )
        call GridCoord_gauss_(gridDesc, Udef, rlon, rlat,&
                              xpts, ypts, nret           &
                             )
        if(present(iret)) iret = nret
!     case ( 5 ) ! Polar Stereographic 
!        call GridCoord_polar_(gridDesc, rlon, rlat, Udef,&
!                              xpts, ypts, nret           &
!                             )
!        if(present(iret)) iret = nret
     case default 
        print*, 'Unrecognized Projection .... '
        print*, 'Program stopping ..'
        stop

  end select
end subroutine compute_grid_coord
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: EarthCoord_latlon_( )
!
!
! !DESCRIPTION: This subroutine computes the earth coordinates of 
!               the specified domain for an equidistant cylindrical projection.
!               This routine is based on the grid decoding routines in the NCEP
!               interoplation package. 
!
!
! !INTERFACE:
!
  subroutine EarthCoord_latlon_(gDesc, Udef, rlon, rlat, iret )
    !
    ! !INPUT PARAMETERS:
    !

    real, dimension(:), intent(in   ) :: gDesc ! grid description parameters
    real,               intent(in   ) :: Udef  ! value to set invalid output data

    !
    ! !OUTPUT PARAMETERS:
    !

    real, dimension(:), intent(inout) :: rlon  ! longitudes in degrees
    real, dimension(:), intent(inout) :: rlat  ! latitudes in degrees 
    integer, optional,  intent(  out) :: iret  ! return code 
    !     0 - success
    !    96 - wrong projection
    !    97 - npts .ne. Ni*Nj
    !    98 - wrong scanning mode

    !
    ! !REVISION HISTORY: 
    !   04-10-96 Mark Iredell;  Initial Specification
    !   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
    !
    !
    ! NOTES:
    !   gDesc :
    !           (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
    !           (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
    !           (4)   - LA(1) LATITUDE OF ORIGIN
    !           (5)   - LO(1) LONGITUDE OF ORIGIN
    !           (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
    !           (7)   - LA(2) LATITUDE OF EXTREME POINT
    !           (8)   - LO(2) LONGITUDE OF EXTREME POINT
    !           (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
    !           (10)  - DJ LATITUDINAL DIRECTION INCREMENT
    !           (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28 [table 8])
    !                    Values to GDS(11)
    !                     Value  Result
    !                        0    0 0 0
    !                       32    0 0 1
    !                       64    0 1 0
    !                       96    0 1 1
    !                      128    1 0 0
    !                      160    1 0 1
    !                      192    1 1 0
    !                      224    1 1 1
    ! -------------------------------------!
    !    WMO Code table 8 - Scanning mode  !
    !
    ! Bit No.   Value  Meaning
    !    1       0    Points scan in +i direction
    !            1    Points scan in -i direction
    !    2       0    Points scan in -j direction
    !            1    Points scan in +j direction
    !    3       0    Adjacent points in i direction are consecutive (i,j)
    !            1    Adjacent points in j direction are consecutive (j,i)
    !  4-8       0  Reserved
    !
    ! NOTES:
    !
    !   i direction: west to east along a parallel, or left to right along an X-axis.
    !   j direction: south to north along a meridian, or bottom to top along a Y-axis.
    !   -----------------------------------------------
    ! How Can I get :
    !   bit1 = mod(gds/128,2)
    !   bit2 = mod(gds/64,2)
    !   bit3 = mod(gds/32,2)

    !EOP
    !-------------------------------------------------------------------------!
    !BOC

    character(len=64), parameter :: myname_=' :: EarthCoord_latlon_( )'

    real    :: rlat1, rlon1
    real    :: rlat2, rlon2
    real    :: dlon, dlat
    integer :: i, j
    integer :: im, jm, n
    integer :: npts

    integer :: iscan
    integer :: jscan
    integer :: nscan

    integer :: imin, imax, istp
    integer :: jmin, jmax, jstp

    if( present(iret) ) iret = 0

    npts = size(rlon)

    if ( gDesc(1) .ne. 0 ) then

       do n = 1,npts
          rlon(n) = Udef
          rlat(n) = Udef
       enddo

       if( present(iret) ) iret = -96
       return

    endif

    !                                      rlat2,rlon2
    !       +----------------------------------o 
    !     L | \ d                              |
    !     A | | l                              |
    !     T | | a                              |
    !     I | / t                              |
    !     T |                                  |
    !     U |                                  |
    !     D |     dlon                         |
    !     E |    /---\                         |
    !       o----------------------------------+
    ! rlat1,rlon1    L O N G I T U D E 
    !
    !

    im    = int(gDesc(2))
    jm    = int(gDesc(3))

    if(npts.ne.im*jm) then
       if(present(iret)) iret = -97
    endif


    rlon1 = min(gDesc(5),gDesc(8))
    rlon2 = max(gDesc(5),gDesc(8))
    dlon  = abs(gDesc(9))

!    tst   = (rlon2-rlon1)/dlon+1
!    if(tst.ne.im)then
!       write(*,'(A)')'Error at GridDesc. Incompatible number of points'
!       write(*,'(A,1x,I6.1,1x,A,1x,I6.1)')'Number of points in x is',im,'should be',tst
!       write(*,'(A,1x,F15.9)')'Start Longitude:',rlon1
!       write(*,'(A,1x,F15.9)')'End Longitude:',rlon2
!       write(*,'(A,1x,F15.9)')'delta lon:',dlon
!       if(present(iret))iret=-99
!       return
!    endif

    rlat1 = min(gDesc(4),gDesc(7))
    rlat2 = max(gDesc(4),gDesc(7))
    dlat  = abs(gDesc(10))
    
!    tst   = (rlat2-rlat1)/dlat+1
!    if(tst.ne.jm)then
!       write(*,'(A)')'Error at GridDesc. Incompatible number of points'
!       write(*,'(A,1x,I6.1,1x,A,1x,I6.1)')'Number of points in y is',jm,'should be',tst
!       write(*,'(A,1x,F15.9)')'Start Longitude:',rlat1
!       write(*,'(A,1x,F15.9)')'End Longitude:',rlat2
!       write(*,'(A,1x,F15.9)')'delta lon:',dlat
!       if(present(iret))iret=-99
!       return
!    endif

    iscan = mod(nint(gDesc(11))/128,2)
    jscan = mod(nint(gDesc(11))/64,2)
    nscan = mod(nint(gDesc(11))/32,2)


    !
    ! translate grid coordinates to earth coordinates
    ! Here we define lon/lat coordenates in this way
    !    * i direction is defined as west to east along a parallel of latitude, or left to right along an x axis.
    !    * j direction is defined as south to north along a meridian of longitude, or bottom to top along a y axis.
    !

    imin = max(1,im*iscan)
    imax = max(1,im*(1-iscan))     
    istp = sign(1,imax-imin)

    jmin = max(1,jm*jscan)
    jmax = max(1,jm*(1-jscan))
    jstp = sign(1,jmax-jmin)

    if(nscan.eq.0)then
       n = 1
       do j = jmin, jmax, jstp
          do i = imin, imax, istp
             rlon(n) = rlon1 + dlon*(i-1)
             rlat(n) = rlat1 + dlat*(j-1)
             !normalize lon to 0 ... 359
             rlon(n) = mod(rlon(n)+3600.0,360.0)
             n = n + 1
          enddo
       enddo
    else if(nscan .eq. 1)then
       n = 1
       do i = imin, imax, istp
          do j = jmin, jmax, jstp
             rlon(n) = rlon1 + dlon*(i-1)
             rlat(n) = rlat1 + dlat*(j-1)
             !normalize lon to 0 ... 359
             rlon(n) = mod(rlon(n)+3600.0,360.0)
             n = n + 1
          enddo
       enddo
    else

       do n = 1,npts
          rlon(n) = Udef
          rlat(n) = Udef
       enddo

       if( present(iret) ) iret = -98
       return

    endif


  end subroutine EarthCoord_latlon_
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: GridCoord_latlon_( )
!
! !DESCRIPTION: This subroutine computes the grid coordinates of 
!               the specified domain for an equidistant cylindrical rojection.
!               This routine is based on the grid decoding routines in the
!               NCEP interoplation package. 
!
! !INTERFACE:
!
!subroutine GridCoord_latlon_(gDesc, Udef, rlon, rlat, xpts, ypts, pt, iret )
subroutine GridCoord_latlon_(gDesc, Udef, rlon, rlat, xpts, ypts, iret )
    !
    ! !INPUT PARAMETERS:
    !

    real, dimension(:), intent(in   ) :: gDesc ! grid description parameters
    real,               intent(in   ) :: Udef  ! value to set invalid output data
    real, dimension(:), intent(in   ) :: rlon  ! longitudes in degrees
    real, dimension(:), intent(in   ) :: rlat  ! latitudes in degrees 

    !
    ! !OUTPUT PARAMETERS:
    !

    real, dimension(:), intent(inout) :: xpts  ! longitudes grid points
    real, dimension(:), intent(inout) :: ypts  ! latitudes grid points
!    integer, dimension(:,:), intent(inout) :: pt
    integer, optional,  intent(  out) :: iret  ! return code 
    !     0 - success
    !    96 - wrong projection
    !
    ! !REVISION HISTORY: 
    !   04-10-96 Mark Iredell;  Initial Specification
    !   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
    !
    !
    ! NOTES:
    !   gDesc :
    !           (2)   - N(I) NR POINTS ON LATITUDE CIRCLE
    !           (3)   - N(J) NR POINTS ON LONGITUDE MERIDIAN
    !           (4)   - LA(1) LATITUDE OF ORIGIN
    !           (5)   - LO(1) LONGITUDE OF ORIGIN
    !           (6)   - RESOLUTION FLAG (RIGHT ADJ COPY OF OCTET 17)
    !           (7)   - LA(2) LATITUDE OF EXTREME POINT
    !           (8)   - LO(2) LONGITUDE OF EXTREME POINT
    !           (9)   - DI LONGITUDINAL DIRECTION OF INCREMENT
    !           (10)  - DJ LATITUDINAL DIRECTION INCREMENT
    !           (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28 [table 8])
    !                    Values to GDS(11)
    !                     Value  Result
    !                        0    0 0 0
    !                       32    0 0 1
    !                       64    0 1 0
    !                       96    0 1 1
    !                      128    1 0 0
    !                      160    1 0 1
    !                      192    1 1 0
    !                      224    1 1 1
    ! -------------------------------------!
    !    WMO Code table 8 - Scanning mode  !
    !
    ! Bit No.   Value  Meaning
    !    1        0    Points scan in +i direction
    !             1    Points scan in -i direction
    !    2        0    Points scan in -j direction
    !             1    Points scan in +j direction
    !    3        0    Adjacent points in i direction are consecutive (i,j)
    !             1    Adjacent points in j direction are consecutive (j,i)
    !  4-8        0  Reserved
    !
    ! NOTES:
    !
    !   i direction: west to east along a parallel, or left to right along an X-axis.
    !   j direction: south to north along a meridian, or bottom to top along a Y-axis.
    !   -----------------------------------------------
    ! How Can I get :
    !   bit1 = mod(gds/128,2)
    !   bit2 = mod(gds/64,2)
    !   bit3 = mod(gds/32,2)

    !EOP
    !-------------------------------------------------------------------------!
    !BOC

    character(len=64), parameter :: myname_=' :: EarthCoord_latlon_( )'

    real    :: rlat1, rlon1
    real    :: rlat2, rlon2
    real    :: dlon, dlat
    integer :: im, jm, n
    integer :: npts
!    integer :: pt(4)

    integer :: iscan
    integer :: jscan
    integer :: nscan

    integer :: imin, imax, istp
    integer :: jmin, jmax, jstp

    if( present(iret) ) iret = 0

    npts = size(xpts)

    if ( gDesc(1) .ne. 0 ) then

       do n = 1,npts
          xpts(n) = Udef
          ypts(n) = Udef
       enddo

       if( present(iret) ) iret = -96
       return

    endif

    !                                      rlat2,rlon2
    !       +----------------------------------o 
    !     L | \ d                              |
    !     A | | l                              |
    !     T | | a                              |
    !     I | / t                              |
    !     T |                                  |
    !     U |                                  |
    !     D |     dlon                         |
    !     E |    /---\                         |
    !       o----------------------------------+
    ! rlat1,rlon1    L O N G I T U D E 
    !
    !

    im    = int(gDesc(2))
    jm    = int(gDesc(3))

    if(npts.ne.im*jm) then
       if(present(iret)) iret = -97
    endif

    iscan = mod(nint(gDesc(11))/128,2)
    jscan = mod(nint(gDesc(11))/64,2)
    nscan = mod(nint(gDesc(11))/32,2)

    rlon1 = min(gDesc(5),gDesc(8))
    rlon2 = max(gDesc(5),gDesc(8))
    dlon  = abs(gDesc(9))
    !normalize lon to 0 ... 359
    rlon1 = mod(rlon1+3600.0,360.0)
    rlon2 = mod(rlon2+3600.0,360.0)

    rlat1 = min(gDesc(4),gDesc(7))
    rlat2 = max(gDesc(4),gDesc(7))
    dlat  = abs(gDesc(10))

    !
    ! translate grid coordinates to earth coordinates
    ! Here we define lon/lat coordenates in this way
    !    * i direction is defined as west to east along a parallel of latitude, or left to right along an x axis.
    !    * j direction is defined as south to north along a meridian of longitude, or bottom to top along a y axis.
    !

    imin = max(1,im*iscan)
    imax = max(1,im*(1-iscan))     
    istp = sign(1,imax-imin)

    jmin = max(1,jm*jscan)
    jmax = max(1,jm*(1-jscan))
    jstp = sign(1,jmax-jmin)


    if(size(xpts).ne.size(ypts))then
       do n=1,size(xpts)
          xpts(n) = ( (rlon(n) - rlon1) / dlon ) + 1
          xpts(n) = imin+((xpts(n)-1)*istp)

          if(xpts(n).gt.im .or. xpts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif
       enddo

       do n=1,size(ypts)
          ypts(n) = ( (rlat(n) - rlat1) / dlat ) + 1
          ypts(n) = jmin+((ypts(n)-1)*jstp)

          if(ypts(n).gt.jm .or. ypts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif
       enddo
    else
       do n = 1, size(xpts)

          xpts(n) = ( (rlon(n) - rlon1) / dlon ) + 1
          xpts(n) = imin+((xpts(n)-1)*istp)

          if(xpts(n).gt.im .or. xpts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif


          ypts(n) = ( (rlat(n) - rlat1) / dlat ) + 1
          ypts(n) = jmin+((ypts(n)-1)*jstp)

          if(ypts(n).gt.jm .or. ypts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif


!          i1 = int(xpts(n))
!          j1 = int(ypts(n))
!
!          i2 = i1+1
!          j2 = j1+1
!
!          pt(n,1) = (j1-1)*gDesc(2) + i1 ! i1,j1
!          pt(n,2) = (j2-1)*gDesc(2) + i1 ! i1,j2
!          pt(n,3) = (j1-1)*gDesc(2) + i2 ! i2,j1
!          pt(n,4) = (j2-1)*gDesc(2) + i2 ! i2,j2

       enddo
    endif
    
  end subroutine GridCoord_latlon_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: EarthCoord_gauss_( )
!
! !DESCRIPTION: This subroutine computes the earth coordinates of 
!               the specified domain for a gaussian cylindrical projection.
!               This routine is based on the grid decoding routines
!               in the NCEP interoplation package. 
!
! !INTERFACE:
!
subroutine EarthCoord_gauss_(gDesc, Udef, rlon, rlat, iret )

    !
    ! !INPUT PARAMETERS:
    !

    real, dimension(:), intent(in   ) :: gDesc ! grid description parameters
    real,               intent(in   ) :: Udef  ! value to set invalid output data

    !
    ! !OUTPUT PARAMATERS:
    !

    real, dimension(:), intent(inout) :: rlon  ! longitudes in degrees
    real, dimension(:), intent(inout) :: rlat  ! latitudes in degrees 
    integer, optional,  intent(  out) :: iret  ! return code ( .ge. 0 - success)

    !
    ! !REVISION HISTORY: 
    !   04-10-96 Mark Iredell;  Initial Specification
    !   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
    !
    ! !SEE ALSO:
    !
    !  - gausslat: Computes latitude values in gaussian
    !
    !-------------------------------------------------------------------------!
    !BOC

    character(len=64), parameter :: myname_=' :: EarthCoord_gauss_( )'

    real              :: rlat1, rlon1
    real              :: rlat2, rlon2
    real              :: dlon
    real, allocatable :: glat(:)
    integer           :: i, j
    integer           :: im, jm, n
    integer           :: iscan, jscan, nscan
    integer           :: npts
    integer :: imin, imax, istp
    integer :: jmin, jmax, jstp
    !-------------------------------------------------------------------------!

    if(present(iret)) iret = 0
    npts = size(rlon)

    if(gDesc(1).ne.4) then

       if(present(iret) )iret = -96

       do n=1,npts
          rlon(n) = Udef
          rlat(n) = Udef
       enddo

       return

    endif

    im    = int(gDesc(2))
    jm    = int(gDesc(3))
    if(npts.ne.im*jm) then
       if(present(iret)) iret = -97
    endif


    rlon1 = min(gDesc(5),gDesc(8))
    rlon2 = max(gDesc(5),gDesc(8))
    dlon  = gDesc(9)!(mod(hi*(rlon2-rlon1)-1+3600,360.)+1)/(im-1)
    !  print*,mod((rlon2-rlon1)+3600.0,360.0),(im-1),(rlon2-rlon1)/im, rlon2-rlon1,dlon

    rlat1 = min(gDesc(4),gDesc(7))
    rlat2 = max(gDesc(4),gDesc(7))


    iscan = mod(nint(gDesc(11))/128,2)
    jscan = mod(nint(gDesc(11))/64,2)
    nscan = mod(nint(gDesc(11))/32,2)

    !
    ! translate grid coordinates to earth coordinates
    ! Here we define lon/lat coordenates in this way
    !    * i direction is defined as west to east along a parallel of latitude, or left to right along an x axis.
    !    * j direction is defined as south to north along a meridian of longitude, or bottom to top along a y axis.
    !

    imin = max(1,im*iscan)
    imax = max(1,im*(1-iscan))     
    istp = sign(1,imax-imin)

    jmin = max(1,jm*jscan)
    jmax = max(1,jm*(1-jscan))
    jstp = sign(1,jmax-jmin)

    allocate(glat(jm))
    call gausll(glat)

    if(nscan.eq.0)then
       n = 1
       do j = jmin, jmax, jstp
          do i = imin, imax, istp
             rlon(n) = rlon1 + dlon*(i-1)
!             if(n.eq.2)print*,rlon(n)
             rlat(n) = glat(jm-(j-1))
             !normalize lon to 0 ... 359
             rlon(n) = mod(rlon(n),360.0)
!             if(n.eq.2)then
!              print*, rlon(n) + 3600.0, int(rlon(n)+3600.0/360.0)
!             endif
!             rlon(n) = rlon(n) - (int(rlon(n)/360.0)*360.0)!A - (INT(A/P) * P)
!             if(n.eq.2)print*,rlon(n)
             n = n + 1
          enddo
       enddo
    else if(nscan .eq. 1)then
       n = 1
       do i = imin, imax, istp
          do j = jmin, jmax, jstp
             rlon(n) = rlon1 + dlon*(i-1)
             rlat(n) = glat(jm-(j-1))
             !normalize lon to 0 ... 359
             rlon(n) = mod(rlon(n)+3600.0,360.0)
             n = n + 1
          enddo
       enddo
    else

       do n = 1,npts
          rlon(n) = Udef
          rlat(n) = Udef
       enddo

       if( present(iret) ) iret = -98
       return

    endif
    i=2
  end subroutine EarthCoord_gauss_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE: compute_grid_coord_gauss
!
! !DESCRIPTION: This subroutine computes the grid coordinates of 
!               the specified domain for a gaussian cylindrical projection.
!               This routine is based on the grid decoding routines
!               in the NCEP interoplation package.
!
! !INTERFACE:
!
!subroutine GridCoord_gauss_(gDesc, Udef, rlon, rlat, xpts, ypts, pt, iret )
subroutine GridCoord_gauss_(gDesc, Udef, rlon, rlat, xpts, ypts,  iret )

    !
    ! !INPUT PARAMETERS:
    !

    real, dimension(:), intent(in   ) :: gDesc ! grid description parameters
    real,               intent(in   ) :: Udef  ! value to set invalid output data
    real, dimension(:), intent(in   ) :: rlon  ! longitudes in degrees
    real, dimension(:), intent(in   ) :: rlat  ! latitudes in degrees 

    !
    ! !OUTPUT PARAMATERS:
    !

    real, dimension(:), intent(inout) :: xpts  ! longitudes grid points
    real, dimension(:), intent(inout) :: ypts  ! latitudes grid points
!    integer, dimension(:,:), intent(inout) :: pt  ! latitudes grid points
    integer, optional,  intent(  out) :: iret  ! return code ( .ge. 0 - success)

    !
    ! !REVISION HISTORY: 
    !   04-10-96 Mark Iredell;  Initial Specification
    !   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
    !
    ! !SEE ALSO:
    !
    !  - gausslat: Computes latitude values in gaussian
    !
    !-------------------------------------------------------------------------!
    !BOC

    character(len=64), parameter :: myname_=' :: EarthCoord_gauss_( )'

    real              :: rlat1, rlon1
    real              :: rlat2, rlon2
    real              :: dlon
    real, allocatable :: glat(:), glat2(:)
    integer           :: im, jm, n
    integer           :: iscan, jscan, nscan
    integer           :: npts
    integer :: imin, imax, istp
    integer :: jmin, jmax, jstp
    real, allocatable :: ds(:)
    integer :: yo, y1
    !-------------------------------------------------------------------------!

    if(present(iret)) iret = 0
    npts = size(xpts)

    if(gDesc(1).ne.4) then

       if(present(iret) )iret = -96

       do n=1,npts
          xpts(n) = Udef
          ypts(n) = Udef
       enddo

       return

    endif

    im    = int(gDesc(2))
    jm    = int(gDesc(3))
!    if(npts.ne.im*jm) then
!       if(present(iret)) iret = -97
!    endif


    rlon1 = min(gDesc(5),gDesc(8))
    rlon2 = max(gDesc(5),gDesc(8))
    dlon  = gDesc(9)!(mod(hi*(rlon2-rlon1)-1+3600,360.)+1)/(im-1)
    !  print*,mod((rlon2-rlon1)+3600.0,360.0),(im-1),(rlon2-rlon1)/im, rlon2-rlon1,dlon

    rlat1 = min(gDesc(4),gDesc(7))
    rlat2 = max(gDesc(4),gDesc(7))


    iscan = mod(nint(gDesc(11))/128,2)
    jscan = mod(nint(gDesc(11))/64,2)
    nscan = mod(nint(gDesc(11))/32,2)

    !
    ! translate grid coordinates to earth coordinates
    ! Here we define lon/lat coordenates in this way
    !    * i direction is defined as west to east along a parallel of latitude, or left to right along an x axis.
    !    * j direction is defined as south to north along a meridian of longitude, or bottom to top along a y axis.
    !

    imin = max(1,im*iscan)
    imax = max(1,im*(1-iscan))     
    istp = sign(1,imax-imin)

    jmin = max(1,jm*jscan)
    jmax = max(1,jm*(1-jscan))
    jstp = sign(1,jmax-jmin)

    allocate(glat(jm))
    allocate(glat2(jm))
    allocate(ds(jm))
    call gausll(glat)
!    glat = glat(jm:1:-1)

    
    if(size(xpts).ne.size(ypts))then
       do n=1,size(xpts)
          xpts(n) = ( (rlon(n) - rlon1) / dlon ) + 1
          xpts(n) = imin+((xpts(n)-1)*istp)
          if(xpts(n).gt.im .or. xpts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif
       enddo
       do n=1,size(ypts)
          ds      = sqrt((rlat(n)-glat)**2)
          yo      = minloc(ds,dim=1)
          y1      = minloc(ds,mask=ds.ne.ds(yo),dim=1)
          ypts(n) = real(yo) + real(y1-yo) * ((rlat(n)-glat(yo))/(glat(y1)-glat(yo)))
          ypts(n) = jmin+((ypts(n)-1)*jstp)
          if(ypts(n).gt.jm .or. ypts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif
       enddo
    else
       do n = 1, size(xpts)

          xpts(n) = ( (rlon(n) - rlon1) / dlon ) + 1
          xpts(n) = imin+((xpts(n)-1)*istp)

          if(xpts(n).gt.im .or. xpts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif

          ds      = sqrt((rlat(n)-glat)**2)
          yo      = minloc(ds,dim=1)
          y1      = minloc(ds,mask=ds.ne.ds(yo),dim=1)
          ypts(n) = real(yo) + real(y1-yo) * ((rlat(n)-glat(yo))/(glat(y1)-glat(yo)))
          ypts(n) = jmin+((ypts(n)-1)*jstp)

          if(ypts(n).gt.jm .or. ypts(n) .lt. 1)then
             if(present(iret)) iret = iret + 1
          endif
       
!          i1 = int(xpts(n))
!          j1 = int(ypts(n))
!
!          i2 = i1+1
!          j2 = j1+1
!
!          pt(n,1) = (j1-1)*gDesc(2) + i1 ! i1,j1
!          pt(n,2) = (j1-1)*gDesc(2) + i2 ! i1,j2
!          pt(n,3) = (j2-1)*gDesc(2) + i1 ! i2,j1
!          pt(n,4) = (j2-1)*gDesc(2) + i2 ! i2,j2

       enddo
    endif


  end subroutine GridCoord_gauss_
!EOC
!-------------------------------------------------------------------------!
!                                                                         !
!-------------------------------------------------------------------------!
!BOP
! 
! !ROUTINE : gausslat
!
! !DESCRIPTION:  This subroutine computes gaussian latitudes.
!                Computes cosines of colatitude and gaussian weights
!                on the gaussian latitudes.  the gaussian latitudes are at
!                the zeroes of the legendre polynomial of the given order.
!
! !INTERFACE:
SUBROUTINE gausll ( lat_sp )

    IMPLICIT NONE
    REAL, INTENT(inout) :: lat_sp(:)

    REAL (r8) , ALLOCATABLE, DIMENSION(:) :: cosc
    REAL (r8) , ALLOCATABLE, DIMENSION(:) :: gwt
    REAL (r8) , ALLOCATABLE, DIMENSION(:) :: sinc
    REAL (r8) , ALLOCATABLE, DIMENSION(:) :: colat
    REAL (r8) , ALLOCATABLE, DIMENSION(:) :: wos2
    REAL (r8) , ALLOCATABLE, DIMENSION(:) :: lat

    INTEGER                               :: nlat , i

    nlat = size(lat_sp)

    allocate(cosc(nlat))
    allocate(gwt(nlat))
    allocate(sinc(nlat))
    allocate(colat(nlat))
    allocate(wos2(nlat))
    allocate(lat(nlat))

    CALL lggaus(nlat, cosc, gwt, sinc, colat, wos2)

    DO i = 1, nlat
       lat(i) = ACOS(sinc(i)) * dpr
       IF (i.gt.nlat/2) lat(i) = -lat(i)
    END DO

    lat_sp = REAL(lat)

  END SUBROUTINE gausll


  SUBROUTINE lggaus( nlat, cosc, gwt, sinc, colat, wos2 )

    IMPLICIT NONE

    !  LGGAUS finds the Gaussian latitudes by finding the roots of the
    !  ordinary Legendre polynomial of degree NLAT using Newton's
    !  iteration method.

    !  On entry:
    integer :: NLAT ! the number of latitudes (degree of the polynomial)

    !  On exit: for each Gaussian latitude
    !     COSC   - cos(colatitude) or sin(latitude)
    !     GWT    - the Gaussian weights
    !     SINC   - sin(colatitude) or cos(latitude)
    !     COLAT  - the colatitudes in radians
    !     WOS2   - Gaussian weight over sin**2(colatitude)

    REAL (r8) , DIMENSION(nlat) :: cosc , gwt , sinc , colat  , wos2 

    !  Convergence criterion for iteration of cos latitude

    REAL , PARAMETER :: xlim  = 1.0E-14

    INTEGER   :: nzero, i, j
    REAL (r8) :: fi, fi1, a, b, g, gm, gp, gt, delta, c, d

    !  The number of zeros between pole and equator

    nzero = nlat/2

    !  Set first guess for cos(colat)

    DO i=1,nzero
       cosc(i) = SIN( (i-0.5)*pi/nlat + pi*0.5 )
    END DO

    !  Constants for determining the derivative of the polynomial
    fi  = nlat
    fi1 = fi+1.0
    a   = fi*fi1 / SQRT(4.0*fi1*fi1-1.0)
    b   = fi1*fi / SQRT(4.0*fi*fi-1.0)

    !  Loop over latitudes, iterating the search for each root

    DO i=1,nzero
       j=0

       !  Determine the value of the ordinary Legendre polynomial for
       !  the current guess root

       DO
          CALL lgord( g, cosc(i), nlat )

          !  Determine the derivative of the polynomial at this point

          CALL lgord( gm, cosc(i), nlat-1 )
          CALL lgord( gp, cosc(i), nlat+1 )
          gt = (cosc(i)*cosc(i)-1.0) / (a*gp-b*gm)

          !  Update the estimate of the root

          delta   = g*gt
          cosc(i) = cosc(i) - delta

          !  If convergence criterion has not been met, keep trying

          j = j+1
          IF( ABS(delta).GT.xlim ) CYCLE

          !  Determine the Gaussian weights

          c      = 2.0 *( 1.0-cosc(i)*cosc(i) )
          CALL lgord( d, cosc(i), nlat-1 )
          d      = d*d*fi*fi
          gwt(i) = c *( fi-0.5 ) / d
          EXIT

       END DO

    END DO

    !  Determine the colatitudes and sin(colat) and weights over sin**2

    DO i=1,nzero
       colat(i)= ACOS(cosc(i))
       sinc(i) = SIN(colat(i))
       wos2(i) = gwt(i) /( sinc(i)*sinc(i) )
    END DO

    !  If NLAT is odd, set values at the equator

    IF( MOD(nlat,2) .NE. 0 ) THEN
       i       = nzero+1
       cosc(i) = 0.0
       c       = 2.0
       CALL lgord( d, cosc(i), nlat-1 )
       d       = d*d*fi*fi
       gwt(i)  = c *( fi-0.5 ) / d
       colat(i)= pi*0.5
       sinc(i) = 1.0
       wos2(i) = gwt(i)
    END IF

    !  Determine the southern hemisphere values by symmetry

    DO i=nlat-nzero+1,nlat
       cosc(i) =-cosc(nlat+1-i)
       gwt(i)  = gwt(nlat+1-i)
       colat(i)= pi-colat(nlat+1-i)
       sinc(i) = sinc(nlat+1-i)
       wos2(i) = wos2(nlat+1-i)
    END DO

  END SUBROUTINE lggaus


  SUBROUTINE lgord( f, cosc, n )

    IMPLICIT NONE

    !  LGORD calculates the value of an ordinary Legendre polynomial at a
    !  specific latitude.

    !  On entry:
    !     cosc - COS(colatitude)
    !     n      - the degree of the polynomial

    !  On exit:
    !     f      - the value of the Legendre polynomial of degree N at
    !              latitude ASIN(cosc)

    REAL (r8) :: s1, c4, a, b, fk, f, cosc, colat, c1, fn, ang
    INTEGER   :: n, k

    !  Determine the colatitude

    colat = ACOS(cosc)

    c1 = SQRT(2.0_r8)
    DO k=1,n
       c1 = c1 * SQRT( 1.0 - 1.0/(4*k*k) )
    END DO

    fn = n
    ang= fn * colat
    s1 = 0.0
    c4 = 1.0
    a  =-1.0
    b  = 0.0
    DO k=0,n,2
       IF (k.eq.n) c4 = 0.5 * c4
       s1 = s1 + c4 * COS(ang)
       a  = a + 2.0
       b  = b + 1.0
       fk = k
       ang= colat * (fn-fk-2.0)
       c4 = ( a * (fn-b+1.0) / ( b * (fn+fn-a) ) ) * c4
    END DO

    f = s1 * c1

  END SUBROUTINE lgord
end module coord_compute

