module BilinInterp

  use coord_compute, only: compute_earth_coord, compute_grid_coord

  implicit none
  private

  public :: bilinear_interp_init
  public :: bilinear_interp

  interface bilinear_interp_init
     module procedure bilinInterp_init0, &
                      bilinInterp_init1
  end interface

  character(len=64),parameter :: myname='BilinInperp'
contains
  !-------------------------------------------------------------------------!
  !                                                                         !
  !-------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: bilinInterp_init0
  ! \label{bilinInterp_init0}
  !
  !
  ! !DESCRIPTION: 
  !  This subprogram performs issues calls to compute the 
  !  interpolation weights and neighbor information for bilinear 
  !  interpolation,from any grid to any grid for scalar fields. 
  !  The grids are defined by their grid description arrays. 
  !  
  !  The grid description arrays are based on the decoding 
  !  schemes used by NCEP. However, in order to remove the integer
  !  arithmetic employed in the original ipolates, the routines
  !  are rewritten using real number manipulations. The general 
  !  structure remains the same. 
  !    
  !  The current code recognizes the following projections: \\
  !             (gridDesc(1)=0) equidistant cylindrical \\
  !             (gridDesc(1)=1) mercator cylindrical\\
  !             (gridDesc(1)=3) lambert conformal conical\\
  !             (gridDesc(1)=4) gaussian cylindrical (spectral native)\\
  !             (gridDesc(1)=5) polar stereographic azimuthal\\
  !  where gridDesc could be defined for either the input grid or the 
  !  output grid. 
  !
  ! !INTERFACE:
  !
  subroutine bilinInterp_init0 (gridDesci,gridDesco,&
                                w11,w12,w21,w22,    &
                                n11,n12,n21,n22     &
                                )

  !
  ! !INPUT PARAMETERS:
  !
    real, dimension(:), intent(in   ) :: gridDesci ! Grid description parameters of input field
    real, dimension(:), intent(in   ) :: gridDesco ! Grid description parameters of output field 

  !
  ! !OUTPUT PARAMETERS:
  !

!    real,    dimension(:), intent(inout) :: rlat               ! latitudes in degrees of output field
!    real,    dimension(:), intent(inout) :: rlon               ! longitudes in degrees of output field
    real,    dimension(:), intent(inout) :: w11, w12, w21, w22 ! weights to be used for interpolation
    integer, dimension(:), intent(inout) :: n11, n12, n21, n22 ! index of neighbor points

  !
  ! !REVISION HISTORY:
  !   04-10-96  Mark Iredell; Initial Specification
  !   05-27-04  Sujay Kumar : Modified verision with floating point arithmetic 
  !
  ! !SEE ALSO:
  !
  !    compute_earth_coord( ) - Computes the earth coordinates for the output grid
  !    compute_grid_coord( ) - Computes the grid coordinates of the input grid, based
  !                            on the earth coordinates of the output grid. 
  !    get_fieldpos( ) - computes the field position for a given point
  !
  !EOP
  !-------------------------------------------------------------------------!
  !BOC
     character(len=64), parameter :: myname_=trim(myname)//' :: bilinInterp_init0( )'

     real, parameter                 :: fill = -9999.0
     real, allocatable, dimension(:) :: xpts
     real, allocatable, dimension(:) :: ypts
     real, allocatable, dimension(:) :: rlon 
     real, allocatable, dimension(:) :: rlat 
     integer  :: npts
     integer  :: n, nv
     integer  :: i1, i2
     integer  :: j1, j2
     real     :: xi, xf
     real     :: yi, yf

     !
     !-----------------------------------------------------------------------!

     npts = gridDesco(2)*gridDesco(3)

     allocate(xpts(npts)); xpts = fill
     allocate(ypts(npts)); ypts = fill
     allocate(rlon(npts)); rlon = fill
     allocate(rlat(npts)); rlat = fill

     !------------------------------------------------------------------------
     !  Calls the routines to decode the grid description and 
     !  calculates the weights and neighbor information to perform
     !  spatial interpolation. This routine eliminates the need to 
     !  compute these weights repeatedly during interpolation. 
     !------------------------------------------------------------------------

     if(gridDesco(1).ge.0) then
        call compute_earth_coord(gridDesco, fill, rlon, rlat)
     endif

     call compute_grid_coord(gridDesci, rlon, rlat, fill, xpts, ypts, nv)
     
     do n = 1, npts

        xi = xpts(n)
        yi = ypts(n)

        if(xi.ne.fill.and.yi.ne.fill) then

           i1 = int(xi)
           i2 = int(i1+1)
           j1 = int(yi)
           j2 = int(j1+1)
           xf = real(xi-i1)
           yf = real(yi-j1)

           n11(n) = get_fieldpos(i1,j1,gridDesci)
           n21(n) = get_fieldpos(i2,j1,gridDesci)
           n12(n) = get_fieldpos(i1,j2,gridDesci)
           n22(n) = get_fieldpos(i2,j2,gridDesci)

           if(min(n11(n),n21(n),n12(n),n22(n)).gt.0) then
              w11(n) = (1-xf)*(1-yf)
              w21(n) = xf*(1-yf)
              w12(n) = (1-xf)*yf
              w22(n) = xf*yf
           else
              n11(n) = 0
              n21(n) = 0
              n12(n) = 0
              n22(n) = 0
           endif

        else

           n11(n) = 0
           n21(n) = 0
           n12(n) = 0
           n22(n) = 0

        endif
     enddo
  end subroutine bilinInterp_init0
  !-------------------------------------------------------------------------!
  !                                                                         !
  !-------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: bilinInterp_init1
  ! \label{bilinInterp_init1}
  !
  !
  ! !DESCRIPTION: 
  !  This subprogram performs issues calls to compute the 
  !  interpolation weights and neighbor information for bilinear 
  !  interpolation,from any grid to any grid for scalar fields. 
  !  The grids are defined by their grid description arrays. 
  !  
  !  The grid description arrays are based on the decoding 
  !  schemes used by NCEP. However, in order to remove the integer
  !  arithmetic employed in the original ipolates, the routines
  !  are rewritten using real number manipulations. The general 
  !  structure remains the same. 
  !    
  !  The current code recognizes the following projections: \\
  !             (gridDesc(1)=0) equidistant cylindrical \\
  !             (gridDesc(1)=1) mercator cylindrical\\
  !             (gridDesc(1)=3) lambert conformal conical\\
  !             (gridDesc(1)=4) gaussian cylindrical (spectral native)\\
  !             (gridDesc(1)=5) polar stereographic azimuthal\\
  !  where gridDesc could be defined for either the input grid or the 
  !  output grid. 
  !
  ! !INTERFACE:
  !

 subroutine bilinInterp_init1 (gridDesci,          &
                                rlat,rlon,          &
                                w11,w12,w21,w22,    &
                                n11,n12,n21,n22     &
                                )

  !
  ! !INPUT PARAMETERS:
  !
    real, dimension(:), intent(in   ) :: gridDesci ! Grid description parameters of input field
    real, dimension(:), intent(in   ) :: rlat      ! latitudes in degrees of output field
    real, dimension(:), intent(in   ) :: rlon      ! longitudes in degrees of output field

  !
  ! !OUTPUT PARAMETERS:
  !

    real,    dimension(:), intent(inout) :: w11, w12, w21, w22 ! weights to be used for interpolation
    integer, dimension(:), intent(inout) :: n11, n12, n21, n22 ! index of neighbor points

  !
  ! !REVISION HISTORY:
  !   04-10-96  Mark Iredell; Initial Specification
  !   05-27-04  Sujay Kumar : Modified verision with floating point arithmetic 
  !
  ! !SEE ALSO:
  !
  !    compute_earth_coord( ) - Computes the earth coordinates for the output grid
  !    compute_grid_coord( ) - Computes the grid coordinates of the input grid, based
  !                            on the earth coordinates of the output grid. 
  !    get_fieldpos( ) - computes the field position for a given point
  !
  !EOP
  !-------------------------------------------------------------------------!
  !BOC
     character(len=64), parameter :: myname_=trim(myname)//' :: bilinInterp_init0( )'

     real, parameter                 :: fill = -9999.0
     real, allocatable, dimension(:) :: xpts
     real, allocatable, dimension(:) :: ypts
  
     integer  :: npts
     integer  :: n, nv
     integer  :: i1, i2
     integer  :: j1, j2
     real     :: xi, xf
     real     :: yi, yf

     !
     !-----------------------------------------------------------------------!

     npts = size(rlat)

     allocate(xpts(npts)); xpts = fill
     allocate(ypts(npts)); ypts = fill

     !------------------------------------------------------------------------
     !  Calls the routines to decode the grid description and 
     !  calculates the weights and neighbor information to perform
     !  spatial interpolation. This routine eliminates the need to 
     !  compute these weights repeatedly during interpolation. 
     !------------------------------------------------------------------------
  
     call compute_grid_coord(gridDesci, rlon, rlat, fill, xpts, ypts, nv)
     
     do n = 1, npts

        xi = xpts(n)
        yi = ypts(n)

        if(xi.ne.fill.and.yi.ne.fill) then

           i1 = int(xi)
           i2 = int(i1+1)
           j1 = int(yi)
           j2 = int(j1+1)
           xf = real(xi-i1)
           yf = real(yi-j1)

           n11(n) = get_fieldpos(i1,j1,gridDesci)
           n21(n) = get_fieldpos(i2,j1,gridDesci)
           n12(n) = get_fieldpos(i1,j2,gridDesci)
           n22(n) = get_fieldpos(i2,j2,gridDesci)

           if(min(n11(n),n21(n),n12(n),n22(n)).gt.0) then
              w11(n) = (1-xf)*(1-yf)
              w21(n) = xf*(1-yf)
              w12(n) = (1-xf)*yf
              w22(n) = xf*yf
           else
              n11(n) = 0
              n21(n) = 0
              n12(n) = 0
              n22(n) = 0
           endif

        else

           n11(n) = 0
           n21(n) = 0
           n12(n) = 0
           n22(n) = 0

        endif
     enddo
     end subroutine
!  end subroutine bilinInterp_init1
  
  !-------------------------------------------------------------------------!
  !                                                                         !
  !-------------------------------------------------------------------------!
  !BOP
  ! 
  ! !ROUTINE: bilinear_interp
  !  \label{bilinear_interp}
  ! 
  !
  ! !DESCRIPTION: 
  !  This subprogram performs bilinear interpolation
  !  from any grid to any grid for scalar fields. The routine is based
  !  on the spatial interpolation package ipolates from NCEP. 
  !             
  !  The algorithm simply computes (weighted) averages
  !  of bilinearly interpolated points arranged in a square box
  !  centered around each output grid point and stretching
  !  nearly halfway to each of the neighboring grid points.
  !  the grids are defined by their grid description arrays. 
  !  
  !  The grid description arrays are based on the decoding 
  !  schemes used by NCEP. However, in order to remove the integer
  !  arithmetic employed in the original ipolates, the routines
  !  are rewritten using real number manipulations. The general 
  !  structure remains the same. 
  !    
  !  The current code recognizes the following projections: \\
  !             (gridDesc(1)=0) equidistant cylindrical \\
  !             (gridDesc(1)=1) mercator cylindrical\\
  !             (gridDesc(1)=3) lambert conformal conical\\
  !             (gridDesc(1)=4) gaussian cylindrical (spectral native)\\
  !             (gridDesc(1)=5) polar stereographic azimuthal\\
  !  where gridDesc could be defined for either the input grid or the 
  !  output grid. The routine also returns the  
  !  the number of output grid points
  !  and their latitudes and longitudes are also returned.
  !  The input bitmaps will be interpolated to output bitmaps.
  !  output bitmaps will also be created when the output grid
  !  extends outside of the domain of the input grid.
  !  the output field is set to 0 where the output bitmap is off.
  ! 
  !
  ! !INTERFACE:
  !
  subroutine bilinear_interp( ibitmap, iField, UDef, &
                              w11, w12, w21, w22,    &
                              n11, n12, n21, n22,    &
                              obitmap, oField,       &
                              iret,                  &
                              GridDesco, lato, lono  &
                              )

  !
  ! !INPUT PARAMETERS:
  !

    real,    dimension(:), optional, intent(in   ) :: GridDesco          ! Grid description of output field
    real,    dimension(:), optional, intent(in   ) :: lato               ! latitude of output field
    real,    dimension(:), optional, intent(in   ) :: lono               ! longitude of output field
    logical, dimension(:),           intent(in   ) :: ibitmap            ! bitmap of input grid field
    real,    dimension(:),           intent(in   ) :: iField             ! input grid field
    real,    dimension(:),           intent(in   ) :: w11, w12, w21, w22 ! weights to be used for interpolation
    integer, dimension(:),           intent(in   ) :: n11, n12, n21, n22 ! index of neighbor points
    real,                            intent(in   ) :: UDef               ! undefined value to be used

  !
  ! !OUTPUT PARAMETERS:
  !

    logical, dimension(:), intent(  out) :: obitmap ! bitmap of output field
    real,    dimension(:), intent(  out) :: oField  ! output grid field
    integer, optional,     intent(  out) :: iret    ! return code ( 0 - success)

  !
  ! !REVISION HISTORY:
  !   04-10-96  Mark Iredell; Initial Specification
  !   05-27-04  Sujay Kumar : Modified verision with floating point arithmetic 
  !
  ! !SEE ALSO:
  !
  !    polifixs( ) - Apply corrections for poles
  !
  !EOP
  !-------------------------------------------------------------------------!
  !BOC
    character(len=64), parameter :: myname_=trim(myname)//' :: bilinear_interp( )'
    integer :: nopt ! Number of output grid points
    integer :: nipt ! Number of input grid points
    integer :: ibo
    integer :: i

    real, allocatable, dimension(:) :: wo

    if(present(iret)) iret = 0


  !-------------------------------------------------------------------------!
  !  INTERPOLATE WITH OR WITHOUT BITMAPS

    nipt = size(iField)
    nopt = size(oField)

    allocate(wo(nopt))

    !$OMP PARALLEL
    !$OMP DO
    do i=1, nopt
       oField(i) = 0.0
       wo(i)     = 0.0

       if(n11(i).gt.0) then 
          if(ibitmap(n11(i))) then
             oField(i) = oField(i) + w11(i)*iField(n11(i))
             wo(i)     = wo(i) + w11(i)
          endif
       endif

       if(n21(i).gt.0) then
          if(ibitmap(n21(i))) then
             oField(i) = oField(i) + w21(i)*iField(n21(i))
             wo(i)     = wo(i) + w21(i)
          endif
       endif

       if(n12(i).gt.0) then 
          if(ibitmap(n12(i))) then
             oField(i) = oField(i) + w12(i)*iField(n12(i))
             wo(i)     = wo(i) + w12(i)
          endif
       endif

       if(n22(i).gt.0) then 
          if(ibitmap(n22(i))) then
             oField(i) = oField(i) + w22(i)*iField(n22(i))
             wo(i)     = wo(i) + w22(i)
          endif
       endif
    enddo
    !$OMP END DO

    ibo=1
    !$OMP DO
    do i=1,nopt

       obitmap(i) = wo(i).ge.0.5

       if(obitmap(i)) then
          oField(i) = oField(i)/wo(i) 
       else
          ibo=1
          oField(i) = UDef
       endif
    enddo
    !$OMP END DO
    !$OMP END PARALLEL
    if(present(gridDesco))then
       if(gridDesco(1).eq.0) call polfixs(lato, lono, ibo, obitmap, oField)
       iret = 0 
    endif

  end subroutine bilinear_interp
  !-------------------------------------------------------------------------!
  !EOC
  !-------------------------------------------------------------------------!
  !                                                                         !
  !-------------------------------------------------------------------------!
  !BOP
  ! 
  ! !ROUTINE: polfixs 
  !  \label{polfixs}
  !
  ! !DESCRIPTION: This subroutine averages multiple pole scalar values
  !               on a latitude/longitude grid.  bitmaps may be averaged too.

  !
  ! !INTERFACE:
  subroutine polfixs(rlat,rlon,ib,lo,go)

  !
  ! !INPUT PARAMETERS:
  !
     real,    dimension(:), intent(in   ) :: rlat ! Latitudes in degrees
     real,    dimension(:), intent(in   ) :: rlon ! Longitudes in degress
     integer,               intent(in   ) :: ib   ! bitmap flags
  !
  ! !OUTPUT PARAMETERS:
  !
     logical, dimension(:), intent(inout) :: lo   ! bitmaps
     real,    dimension(:), intent(inout) :: go   ! returned field

  !
  ! !REVISION HISTORY:
  !   04-10-96  Mark Iredell; Initial Specification
  !        
  !EOP
  !-------------------------------------------------------------------------!
  !BOC
    character(len=64), parameter :: myname_=trim(myname)//' :: polifixs( )'
    integer         :: n, nm
    real            :: tsp, gnp, gsp, wsp, tnp, wnp
    real, PARAMETER :: rlatnp=89.9995, rlatsp=-89.9995

    nm = size(rlon)    

    wnp=0.0
    gnp=0.0
    tnp=0.0
    wsp=0.0
    gsp=0.0
    tsp=0.0

    !  average multiple pole values
    do n=1,nm
       if(rlat(n).ge.rlatnp) then
          wnp = wnp + 1
          if(ib.eq.0.or.lo(n)) then
             gnp = gnp + go(n)
             tnp = tnp + 1
          endif
       elseif(rlat(n).le.rlatsp) then
          wsp = wsp + 1
          if(ib.eq.0.or.lo(n)) then
             gsp = gsp + go(n)
             tsp = tsp + 1
          endif
       endif
    enddo

    !  distribute average values back to multiple poles
    if(wnp.gt.1) then
       if(tnp.ge.wnp/2) then
          gnp = gnp / tnp
       else
          gnp = 0.0
       endif
       do n=1,nm
          if(rlat(n).ge.rlatnp) then
             if(ib.ne.0) lo(n)=tnp.ge.wnp/2
             go(n) = gnp
          endif
       enddo
    endif
    if(wsp.gt.1) then
       if(tsp.ge.wsp/2) then
          gsp = gsp / tsp
       else
          gsp = 0.0
       endif
       do n=1,nm
          if(rlat(n).le.rlatsp) then
             if(ib.ne.0) lo(n)=tsp.ge.wsp/2
             go(n) = gsp
           endif
       enddo
    endif
  end subroutine polfixs
  !EOC
  !-------------------------------------------------------------------------
  ! 
  !-------------------------------------------------------------------------
  !BOP
  ! 
  ! !ROUTINE: get_fieldpos
  !  \label{get_fieldpos}
  !
  !
  ! !DESCRIPTION:  This subprogram returns the field position for a given 
  !                grid point based on the input grid definition.
  !
  !EOP

  !
  ! !INTERFACE:
  !
  
  function get_fieldpos(i,j,gridDesc) result(field_pos)

  !
  ! !INPUT PARAMETERS:
  !
    real, dimension(:), intent(in   ) :: gridDesc  ! grid description parameters
    integer,            intent(in   ) :: i         ! integer x grid point
    integer,            intent(in   ) :: j         ! integer y grid point
  !
  ! !OUTPUT PARAMETERS:
  !
    integer                           :: field_pos ! integer position in grid field 
                                                   ! to locate grid point
  !
  ! !REVISION HISTORY:
  !   04-10-96  Mark Iredell; Initial Specification
  !   03-11-96  Mark Iredell; Allowed hemispheric grids to wrap over one pole
  !   05-27-04  Sujay Kumar; Modified code with floating point arithmetic
  !   01-17-09  Sujay Kumar; Specified the dlon value separately based on the
  !             input grid projection
  !
  ! !SEE ALSO:
  !
  !
  !
  !EOP
  !-------------------------------------------------------------------------!
  !BOC

    character(len=64), parameter :: myname_=trim(myname)//' :: get_fieldpos( )'

    integer :: im, jm
    integer :: kscan
    integer :: is1
    integer :: nscan
    integer :: ii, jj
    real    :: rlat1,rlat2
    real    :: rlon1,rlon2
    integer :: iscan
    real    :: dlon
    real    :: dlat
    integer :: ig,jg
    
  !  GET GRID DIMENSIONS
    im    = int(gridDesc(2))
    jm    = int(gridDesc(3))
    is1   = 0
    kscan = 0
    nscan = mod(nint(gridDesc(20))/32,2)

  !-------------------------------------------------------------------------!
  !  ACCOUNT FOR WRAPAROUNDS IN EITHER DIRECTION

    select case( int(gridDesc(1)) )
       case(0) 
          dlon = gridDesc(9)
       case(1) 
          dlon = gridDesc(8)
       case(4) 
          dlon = gridDesc(9)
    end select

    ii = i
    jj = j
    if(gridDesc(1).eq.0.or.gridDesc(1).eq.1.or.gridDesc(1).eq.4) then
       rlon1 = gridDesc(5)
       rlon2 = gridDesc(8)
       iscan = mod(nint(gridDesc(20))/128,2)
       ig    = nint(360/abs(dlon))

       if(im.ge.ig) then

          ii=mod(i-1+ig,ig)+1

          if((j.le.0.or.j.ge.jm+1).and.mod(ig,2).eq.0) then

             select case ( int(gridDesc(1)) )

                case (0) ! equidistant cylindrical projection

                   rlat1 = gridDesc(4)
                   rlat2 = gridDesc(7)
                   dlat  = abs(rlat2-rlat1)/(jm-1)
                   if(j.le.0.and.abs(rlat1).gt.90-0.25*dlat) then
                      jj = 2-j
                      ii = mod(ii-1+ig/2,ig)+1
                   elseif(j.le.0.and.abs(rlat1).gt.90-0.75*dlat) then
                      jj = 1-j
                      ii = mod(ii-1+ig/2,ig)+1
                   elseif(j.ge.jm+1.and.abs(rlat2).gt.90-0.25*dlat) then
                      jj = 2*jm-j
                      ii = mod(ii-1+ig/2,ig)+1
                   elseif(j.ge.jm+1.and.abs(rlat2).gt.90-0.75*dlat) then
                      jj = 2*jm+1-j
                      ii = mod(ii-1+ig/2,ig)+1
                   endif

                case(4) ! gaussian cylindrical (spectral native) projection

                   jg = int(gridDesc(10)*2)
                   if(j.le.0.and.jm.eq.jg) then
                      jj = 1-j
                      ii = mod(ii-1+ig/2,ig)+1
                   elseif(j.ge.jm+1.and.jm.eq.jg) then
                      jj = 2*jm+1-j
                      ii = mod(ii-1+ig/2,ig)+1
                   endif

             end select

          endif

       endif

    endif


    if(ii.ge.1.and.ii.le.im.and.jj.ge.1.and.jj.le.jm) then
       if(nscan.eq.0) then
          field_pos = ii+(jj-1)*im
       else
          field_pos = jj+(ii-1)*jm
       endif
    else
       field_pos = 0
    endif

  end function get_fieldpos
 !-------------------------------------------------------------------------!
 !EOC

end module BilinInterp

