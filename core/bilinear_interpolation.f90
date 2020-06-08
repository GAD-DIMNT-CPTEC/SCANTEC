MODULE interp_mod
implicit none

!public :: bilinear_interp
!public :: compute_earth_coord_latlon
!public :: compute_earth_coord_gauss
!public :: gausslat
!public :: get_fieldpos

CONTAINS

subroutine bilinear_interp_input (gridDesci,gridDesco,npts,&
     rlat,rlon,n11,n12,n21,n22,w11,w12,w21,w22)

  implicit none
! !ARGUMENTS:
  real, intent(in)    :: gridDesci(50)
  real                :: gridDesco(50)
  integer             :: npts
  real                :: rlat(npts)
  real                :: rlon(npts)
  integer             :: n11(npts),n12(npts),n21(npts),n22(npts)
  real                :: w11(npts),w12(npts),w21(npts),w22(npts)
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
!  The arguments are: 
!  \begin{description}
!    \item[gridDesci]
!     input grid description parameters 
!    \item[gridDesco]
!     output grid description parameters 
!    \item[npts] 
!     number of points to in the output field
!    \item[rlat]    
!     output latitudes in degrees
!    \item[rlon]    
!     output longitudes in degrees
!    \item[w11,w12,w21,w22]    
!     weights to be used for interpolation
!    \item[n11,n12,n21,n22]    
!     index of neighbor points 
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[compute\_earth\_coord](\ref{compute_earth_coord})\\
!     Computes the earth coordinates for the output grid
!   \item[compute\_grid\_coord](\ref{compute_grid_coord})\\
!     Computes the grid coordinates of the input grid, based
!     on the earth coordinates of the output grid. 
!   \item[get\_field\_pos](\ref{get_field_pos})\\
!     computes the field position for a given point
!  \end{description}
!EOP
  integer             :: n
  integer             :: mo, nv 
  real, parameter     :: fill = -9999.0
  real                :: xpts(npts), ypts(npts)

  integer             :: i1, i2, j1, j2
  real                :: xi, xf, yi, yf
!  integer             :: get_fieldpos

  mo = npts

 !
 ! Verify if input and output grid are in same space
 !
 if (is_same_grid(gridDesci,gridDesco)) then
    n11(:) = 0
    n21(:) = 0
    n12(:) = 0
    n22(:) = 0
    w11(:) = 0
    w21(:) = 0
    w12(:) = 0
    w22(:) = 0
    return
 endif

!#ifdef DEBUG
  write(6,*)'scantec GRID'
  write(6,*)'lati :',GridDescO(4)
  write(6,*)'latf :',GridDescO(7)
  write(6,*)'dy   :',GridDescO(10)
  write(6,*)'loni :',GridDescO(5)-360.000
  write(6,*)'lonf :',GridDescO(8)-360.000
  write(6,*)'dy   :',GridDescO(9)


  write(6,*)'MODEL GRID'
  write(6,*)'lati :',GridDescI(4)
  write(6,*)'latf :',GridDescI(7)
  write(6,*)'dy   :',GridDescI(10)
  write(6,*)'loni :',GridDescI(5)
  write(6,*)'lonf :',GridDescI(8)
  write(6,*)'dy   :',GridDescI(9)
!#endif



  !------------------------------------------------------------------------
  !  Calls the routines to decode the grid description and 
  !  calculates the weights and neighbor information to perform
  !  spatial interpolation. This routine eliminates the need to 
  !  compute these weights repeatedly during interpolation. 
  !------------------------------------------------------------------------
  if(gridDesco(1).ge.0) then
     !
     ! Compute the earth coordinates of scantec grid
     ! INPUT:
     !       GridDescO - Informations about scantec grid
     !       mo        - Number of points of scantec grid
     !       fill      - Undef Value
     ! OUTPUT:
     !       xpts - grid x point coordinates
     !       ypts - grid y point coordinates
     !       rlat - output latitudes in degrees
     !       rlon - output langitudesin degrees
     !       nv   - error code

     call compute_earth_coord(gridDesco, mo,fill,xpts,ypts,rlon,rlat,nv)

     !
     !
  endif
     !
     ! Compute the grid coordinates of INPUT data
     !
  call compute_grid_coord(gridDesci,mo,fill,xpts,ypts,rlon,rlat,nv)
  do n=1,mo
     xi=xpts(n)
     yi=ypts(n)
     if(xi.ne.fill.and.yi.ne.fill) then
        i1=xi
        i2=i1+1
        j1=yi
        j2=j1+1 
        xf=xi-i1
        yf=yi-j1
        n11(n)=get_fieldpos(i1,j1,gridDesci)
        n21(n)=get_fieldpos(i2,j1,gridDesci)
        n12(n)=get_fieldpos(i1,j2,gridDesci)
        n22(n)=get_fieldpos(i2,j2,gridDesci)

        if(min(n11(n),n21(n),n12(n),n22(n)).gt.0) then
           w11(n)=(1-xf)*(1-yf)
           w21(n)=xf*(1-yf)
           w12(n)=(1-xf)*yf
           w22(n)=xf*yf
        else
           n11(n)=0
           n21(n)=0
           n12(n)=0
           n22(n)=0
        endif
     else
        n11(n)=0
        n21(n)=0
        n12(n)=0
        n22(n)=0
     endif
  enddo


end subroutine bilinear_interp_input


! !ROUTINE: bilinear_interp
!  \label{bilinear_interp}
!        
! !REVISION HISTORY:
!   04-10-96  Mark Iredell; Initial Specification
!   05-27-04  Sujay Kumar : Modified verision with floating point arithmetic 
!   10-11-12  joao gerd : Include grid verification
! !INTERFACE:
subroutine bilinear_interp(gridDesco,ibi,li,gi,ibo,lo,go,mi,mo, & 
     rlat,rlon,w11,w12,w21,w22,n11,n12,n21,n22,udef,iret)
!USES:
  implicit none
! !ARGUMENTS: 
  real      :: gridDesco(50)
  integer   :: ibi
  integer   :: ibo
  integer   :: mi
  integer   :: mo
  logical*1 :: li(mi)
  logical*1 :: lo(mo)
  real      :: gi(mi)
  real      :: go(mo)
  real      :: rlat(mo)
  real      :: rlon(mo)
  real      :: w11(mo),w12(mo), w21(mo),w22(mo)
  integer   :: n11(mo),n12(mo),n21(mo),n22(mo)
  real      :: udef
  integer   :: iret
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
!  The arguments are: 
!  \begin{description}
!    \item[gridDesco]
!     output grid description parameters 
!    \item[ibi] 
!     integer input bitmap flags
!    \item[li]
!     logical input bitmaps
!    \item[gi]
!     real input fields to interpolate
!    \item[ibo]
!     integer output bitmap flags
!    \item[lo]
!     logical output bitmaps
!    \item[go]
!     real output fields interpolated
!    \item[mi]
!     integer dimension of input grid fields 
!    \item[mo]
!     integer dimension of output grid fields
!    \item[rlat]    
!     output latitudes in degrees
!    \item[rlon]    
!     output longitudes in degrees
!    \item[w11,w12,w21,w22]    
!     weights to be used for interpolation
!    \item[n11,n12,n21,n22]    
!     index of neighbor points 
!    \item[udef]
!     undefined value to be used
!    \item[iret]
!     return code (0-success)
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[polfixs](\ref{polfixs})\\
!    Apply corrections for poles
!  \end{description}
!EOP

  integer   :: nn
  integer   :: n
  integer   :: s
  real wo(mo)
  real, parameter :: fill=-9999.
  
  
  iret = 0
  
  !
  ! Verify if input and output grid are in the same space. 
  !
  s = sum(n11)+sum(n12)+sum(n21)+sum(n22)+sum(w11)+sum(w12)+sum(w21)+sum(w22)
  if ( s .eq. 0 )then
     lo  = li
     ibo = ibi
     mo  = mi
     go  = gi
     where(.NOT.li)go = udef
     return
  endif

  nn = mo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  INTERPOLATE WITH OR WITHOUT BITMAPS

  do n=1, nn
     go(n)=0.
     wo(n)=0.
     if(li(n11(n))) then
        go(n)=go(n)+w11(n)*gi(n11(n))
        wo(n)=wo(n)+w11(n)
     endif
     if(li(n21(n))) then
        go(n)=go(n)+w21(n)*gi(n21(n))
        wo(n)=wo(n)+w21(n)
     endif
     if(li(n12(n))) then
        go(n)=go(n)+w12(n)*gi(n12(n))
        wo(n)=wo(n)+w12(n)
     endif
     if(li(n22(n))) then
        go(n)=go(n)+w22(n)*gi(n22(n))
        wo(n)=wo(n)+w22(n)
     endif
  enddo

  ibo=1
  do n=1,nn
     lo(n)=wo(n).ge.0.5
     if(lo(n)) then
        go(n)=go(n)/wo(n)
     else
        ibo=1
        go(n)=udef
     endif
  enddo
  if(gridDesco(1).eq.0) call polfixs(nn,mo,rlat,rlon,ibo,lo,go)

end subroutine bilinear_interp
! !ROUTINE: compute_earth_coord
!  \label{compute_earth_coord}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_earth_coord(gridDesc,npts,fill,xpts,ypts,rlon,rlat,nret)

  implicit none
! !ARGUMENTS: 
  real        :: gridDesc(50)
  integer     :: npts
  real        :: fill
  real        :: xpts(npts),ypts(npts)
  real        :: rlat(npts)
  real        :: rlon(npts)
  integer     :: nret
! !DESCRIPTION: 
!  This subroutine computes the earth coordinates (lat/lon values) 
!  of the specified domain. This routine is based on the grid
!  decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!  The current code recognizes the following projections:\\
!             (gridDesc(1)=000) equidistant cylindrical \\
!             (gridDesc(1)=001) mercator cylindrical \\
!             (gridDesc(1)=003) lambert conformal conical \\
!             (gridDesc(1)=004) gaussian cylindrical \\
!             (gridDesc(1)=005) polar stereographic azimuthal \\
!
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     input grid x point coordinates
!    \item[ypts]
!     input grid y point coordinates
!    \item[rlat]    
!     output latitudes in degrees
!    \item[rlon]    
!     output longitudes in degrees
!    \item[nret]
!     return code (0-success)
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[compute\_earth\_coord\_latlon](\ref{compute_earth_coord_latlon})\\
!     computes the earth coordinates of a latlon grid
!   \item[compute\_earth\_coord\_merc](\ref{compute_earth_coord_merc})\\
!     computes the earth coordinates of a mercator grid
!   \item[compute\_earth\_coord\_lambert](\ref{compute_earth_coord_lambert})\\
!     computes the earth coordinates of a lambert conformal grid
!   \item[compute\_earth\_coord\_gauss](\ref{compute_earth_coord_gauss})\\
!     computes the earth coordinates of a gaussian cylindrical grid
!   \item[compute\_earth\_coord\_polar](\ref{compute_earth_coord_polar})\\
!     computes the earth coordinates of a polar stereographic grid
!  \end{description}
!EOP
  integer :: im,jm,kscan,is1,nm,nscan,nn, iopf,n
  integer :: i,j

  im=gridDesc(2)
  jm=gridDesc(3)
  nm=im*jm
  if(nm.le.npts) then
     do n=1,nm
        j=(n-1)/im+1
        i=n-im*(j-1)
        xpts(n)=i
        ypts(n)=j
     enddo
     do n=nm+1,npts
        xpts(n)=fill
        ypts(n)=fill
     enddo
  else
     do n=1,npts
        xpts(n)=fill
        ypts(n)=fill
     enddo
  endif


!  equidistant cylindrical
  if(gridDesc(1).eq.0) then
     call compute_earth_coord_latlon(gridDesc,npts,fill,xpts,ypts,&
          rlon,rlat,nret)
!  Mercator
!  elseif(gridDesc(1).eq.1) then 
!     call compute_earth_coord_merc(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
!  Lambert Conformal
  elseif(gridDesc(1).eq.3) then 
     call compute_earth_coord_lambert(gridDesc,npts,fill,xpts,ypts,&
          rlon,rlat,nret)
!     gaussian cylindrical
  elseif(gridDesc(1).eq.4) then
     call compute_earth_coord_gauss(gridDesc,npts,fill,xpts,ypts,&
          rlon,rlat,nret)
!  Polar Stereographic 
!  elseif(gridDesc(1).eq.5) then
!     call compute_earth_coord_polar(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
!  HRAP
!  elseif(gridDesc(1).eq.6) then
!     call compute_earth_coord_hrap(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
!  EASE cylindrical
!  elseif(gridDesc(1).eq.7) then
!     call compute_earth_coord_ease(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
  endif
end subroutine compute_earth_coord

! !ROUTINE: compute_earth_coord_latlon
!  \label{compute_earth_coord_latlon}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_earth_coord_latlon(gridDesc,npts,fill,xpts,ypts,& 
     rlon,rlat,nret)

  implicit none

! !ARGUMENTS: 
  real            :: gridDesc(50)
  integer         :: npts
  real            :: fill
  real            :: xpts(npts),ypts(npts)
  real            :: rlat(npts)
  real            :: rlon(npts)

! !DESCRIPTION:
!  This subroutine computes the earth coordinates of 
!  the specified domain for an equidistant cylindrical projection.
!  This routine is based on the grid
!  decoding routines in the NCEP interoplation package. 
!  
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     grid x point coordinates
!    \item[ypts]
!     grid y point coordinates
!    \item[rlat]    
!     output latitudes in degrees
!    \item[rlon]    
!     output longitudes in degrees
!    \end{description}
!
!EOP
  integer :: nret
  real :: rlat1,rlon1,rlat2,rlon2,hi,hj,dlon,dlat
  real :: xmin,xmax,ymin,ymax
  integer :: iscan,jscan,nscan,im,jm,iret,n
  integer :: ii
  if(gridDesc(1).eq.0) then
     im    = gridDesc(2)
     jm    = gridDesc(3)
     rlat1 = gridDesc(4)
     rlon1 = gridDesc(5)
     rlat2 = gridDesc(7)
     rlon2 = gridDesc(8)

     if(rlat1.gt.rlat2) then 
        dlat=-gridDesc(9)
     else
        dlat=gridDesc(9)
     endif

     if(rlon1.gt.rlon2) then 
        dlon=-gridDesc(10)
     else
        dlon = gridDesc(10)
     endif

     xmin = 0
     xmax = im+1

     if(im.eq.nint(360/abs(dlon))) xmax=im+2

     ymin = 0
     ymax = jm+1
     nret = 0
!  translate grid coordinates to earth coordinates
     do n=1,npts
        if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
             ypts(n).ge.ymin.and.ypts(n).le.ymax) then

           rlon(n)=rlon1+dlon*(xpts(n)-1)

           if(rlon(n).lt.0) then 
              rlon(n) = 360+rlon(n)
           endif
           rlat(n)=rlat1+dlat*(ypts(n)-1)
           nret=nret+1
        else
           rlon(n)=fill
           rlat(n)=fill
        endif
     enddo


!  projection unrecognized
  else
     iret=-1
     do n=1,npts
        rlon(n)=fill
        rlat(n)=fill
     enddo
  endif

end subroutine compute_earth_coord_latlon


! 
! !ROUTINE: compute_earth_coord_gauss
! \label{compute_earth_coord_gauss}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_earth_coord_gauss(gridDesc,npts,fill,xpts,ypts,&
     rlon,rlat,nret)

  implicit none
! !ARGUMENTS: 
  real            :: gridDesc(50)
  integer         :: npts
  real            :: fill
  real            :: xpts(npts),ypts(npts)
  real            :: rlat(npts)
  real            :: rlon(npts)

! !DESCRIPTION:
!  This subroutine computes the earth coordinates of 
!  the specified domain for a gaussian cylindrical projection.
!  This routine is based on the grid
!  decoding routines in the NCEP interoplation package. 
!  
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     grid x point coordinates
!    \item[ypts]
!     grid y point coordinates
!    \item[rlat]    
!     output latitudes in degrees
!    \item[rlon]    
!     output longitudes in degrees
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[gausslat](\ref{gausslat})\\
!     Computes latitude values in gaussian
!  \end{description}
!EOP

  real, parameter :: pi=3.14159265358979
  integer, parameter :: jgmax=2000
  real :: dpr
  real ::  rlata,rlatb  
  integer :: nret
  integer :: im,jm, jg, j, ja, n
  real :: rlat1,rlon1, rlat2, rlon2
  real :: hi, wb
  real :: dlon
  real :: xmin,xmax,ymin,ymax
  real :: alat(0:jgmax+1),blat(jgmax)
  integer :: iscan,jscan,nscan, iret
  real :: yptsa, yptsb
  integer :: jh, j1, j2
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  dpr =180./pi 
  if(gridDesc(1).eq.4.and.gridDesc(10)*2.le.jgmax) then
     im=gridDesc(2)
     jm=gridDesc(3)
     rlat1=gridDesc(4)
     rlon1=gridDesc(5)
     rlat2=gridDesc(7)
     rlon2=gridDesc(8)
     jg=gridDesc(10)*2
     iscan=mod(nint(gridDesc(11))/128,2)
     jscan=mod(nint(gridDesc(11))/64,2)
     nscan=mod(nint(gridDesc(11))/32,2)
     hi=(-1.)**iscan
     jh=(-1)**jscan
     dlon=hi*(mod(hi*(rlon2-rlon1)-1+3600,360.)+1)/(im-1)
     call gausslat(jg,alat(1),blat)
     do ja=1,jg
        alat(ja)=dpr*asin(alat(ja))
     enddo
     alat(0)=180.-alat(1)
     alat(jg+1)=-alat(0)
     j1=1
     do while(j1.lt.jg.and.rlat1.lt.(alat(j1)+alat(j1+1))/2)
        j1=j1+1
     enddo
     j2=j1+jh*(jm-1)
     xmin=0
     xmax=im+1
     if(im.eq.nint(360/abs(dlon))) xmax=im+2
     ymin=0.5
     ymax=jm+0.5
     nret=0
! translate grid coordinates to earth coordinates
     do n=1,npts
        if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
             ypts(n).ge.ymin.and.ypts(n).le.ymax) then
           rlon(n)=mod(rlon1+dlon*(xpts(n)-1)+3600,360.)
           j=min(int(ypts(n)),jm)
           rlata=alat(j1+jh*(j-1))
           rlatb=alat(j1+jh*j)
           wb=ypts(n)-j
           rlat(n)=rlata+wb*(rlatb-rlata)
           nret=nret+1
        else
           rlon(n)=fill
           rlat(n)=fill
        endif
     enddo

! projection unrecognized
  else
     iret=-1
     
     do n=1,npts
        rlon(n)=fill
        rlat(n)=fill
     enddo
  endif
end subroutine compute_earth_coord_gauss


!BOP
! 
! !ROUTINE: compute_earth_coord_lambert
!  \label{compute_earth_coord_lambert}
!
! !INTERFACE:
subroutine compute_earth_coord_lambert(gridDesc,npts,fill,xpts,ypts,& 
     rlon,rlat,nret)
! !USES:   
  use map_utils

  implicit none
! !ARGUMENTS: 
  real            :: gridDesc(50)
  integer         :: npts
  real            :: fill
  real            :: xpts(npts),ypts(npts)
  real            :: rlat(npts)
  real            :: rlon(npts)
  integer         :: nret

! !DESCRIPTION:
!  This subroutine computes the earth coordinates of 
!  the specified domain for a lambert conformal projection
!  This routine is based on the
!  decoding routines in the NCEP interoplation package and 
!  has been modified the adopted module from the Weather
!  Research and Forecasting (WRF) model. 
!  
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     grid x point coordinates
!    \item[ypts]
!     grid y point coordinates
!    \item[rlat]    
!     output latitudes in degrees
!    \item[rlon]    
!     output longitudes in degrees
!    \item[nret]
!     return code (0-success)
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[map\_set](\ref{map_set})\\
!     Sets the projection to lambert conformal
!   \item[ij\_to\_latlon](\ref{ij_to_latlon})\\
!     Computes the lat lon values for each i,j
!  \end{description}
!EOP 
  type(proj_info) :: proj
  integer :: i
  !
  ! Esta sequencia segue o padrao grib1 veja o PDS na secao 2
  !
  if(griddesc(1).eq.3) then
     call map_set(PROJ_LC,           & ! proj_code
                  gridDesc(4),       & ! lat1
                  gridDesc(5),       & ! lon1
                  gridDesc(8),       & ! dx
                  gridDesc(7),       & ! stdlon
                  gridDesc(12),      & ! truelat1
                  gridDesc(13),      & ! truelat2
                  nint(gridDesc(2)), & ! idim
                  nint(gridDesc(3)), & ! jdim
                  proj)
     do i=1,npts
        call ij_to_latlon(proj,xpts(i),ypts(i),rlat(i),rlon(i))
     enddo
  endif
end subroutine compute_earth_coord_lambert


!BOP
! !ROUTINE: compute_grid_coord
!  \label{compute_grid_coord}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_grid_coord(gridDesc,npts,fill,xpts,ypts,rlon,rlat,nret)
  implicit none
! !ARGUMENTS: 
  real        :: gridDesc(50)
  integer     :: npts
  real        :: fill
  real        :: xpts(npts),ypts(npts)
  real        :: rlat(npts)
  real        :: rlon(npts)
  integer     :: nret
! !DESCRIPTION: 
!  This subroutine computes the grid coordinates (cartesian) of 
!  the specified domain. This routine is based on the grid
!  decoding routines in the ipolates interoplation package. 
!  
!  The input options include :
!  The current code recognizes the following projections:
!             (gridDesc(1)=000) equidistant cylindrical
!             (gridDesc(1)=001) mercator cylindrical
!             (gridDesc(1)=003) lambert conformal conical
!             (gridDesc(1)=004) gaussian cylindrical
!             (gridDesc(1)=005) polar stereographic azimuthal
!
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     output grid x point coordinates
!    \item[ypts]
!     output grid y point coordinates
!    \item[rlat]    
!     input latitudes in degrees
!    \item[rlon]    
!     input longitudes in degrees
!    \item[nret]
!     return code (0-success)
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[compute\_grid\_coord\_latlon](\ref{compute_grid_coord_latlon}\\
!     computes the grid coordinates of a latlon grid
!   \item[compute\_grid\_coord\_merc](\ref{compute_grid_coord_merc}\\
!     computes the grid coordinates of a mercator grid
!   \item[compute\_grid\_coord\_lambert](\ref{compute_grid_coord_lambert}\\
!     computes the grid coordinates of a lambert conformal grid
!   \item[compute\_grid\_coord\_gauss](\ref{compute_grid_coord_gauss}\\
!     computes the grid coordinates of a gaussian cylindrical grid
!   \item[compute\_grid\_coord\_polar](\ref{compute_grid_coord_polar}\\
!     computes the grid coordinates of a polar stereographic grid
!  \end{description}
!EOP
  integer :: im,jm,kscan,is1,nm,nscan,nn, iopf,n
  integer :: i,j

!  equidistant cylindrical
  if(gridDesc(1).eq.0) then
     call compute_grid_coord_latlon(gridDesc,npts,fill,xpts,ypts,&
          rlon,rlat,nret)
!  mercator
!  elseif(gridDesc(1).eq.1) then      
!     call compute_grid_coord_merc(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
!  Lambert Conformal
  elseif(gridDesc(1).eq.3) then      
     call compute_grid_coord_lambert(gridDesc,npts,fill,xpts,ypts,&
          rlon,rlat,nret)
!     gaussian cylindrical
  elseif(gridDesc(1).eq.4) then
     call compute_grid_coord_gauss(gridDesc,npts,fill,xpts,ypts,&
          rlon,rlat,nret)
!  Polar Stereographic 
!  elseif(gridDesc(1).eq.5) then
!     call compute_grid_coord_polar(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
!  HRAP
!  elseif(gridDesc(1).eq.6) then
!     call compute_grid_coord_hrap(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
!  EASE cylindrical
!  elseif(gridDesc(1).eq.7) then
!     call compute_grid_coord_ease(gridDesc,npts,fill,xpts,ypts,&
!          rlon,rlat,nret)
  endif
end subroutine compute_grid_coord
! 
! !ROUTINE: compute_grid_coord_latlon
!  \label{compute_grid_coord_latlon}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_grid_coord_latlon(gridDesc,npts,fill,xpts,ypts,& 
     rlon,rlat,nret)

  implicit none
! !ARGUMENTS: 
  real            :: gridDesc(50)
  integer         :: npts
  real            :: fill
  real            :: xpts(npts),ypts(npts)
  real            :: rlat(npts)
  real            :: rlon(npts)
  integer         :: nret

! !DESCRIPTION:
!  This subroutine computes the grid coordinates of 
!  the specified domain for an equidistant cylindrical rojection.
!  This routine is based on the grid
!  decoding routines in the NCEP interoplation package. 
!  
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     output grid x point coordinates
!    \item[ypts]
!     output grid y point coordinates
!    \item[rlat]    
!     input latitudes in degrees
!    \item[rlon]    
!     input longitudes in degrees
!    \end{description}
!
!EOP
  real :: rlat1,rlon1,rlat2,rlon2,hi,hj,dlon,dlat
  real :: xmin,xmax,ymin,ymax
  integer :: iscan,jscan,nscan,im,jm,iret,n
  integer :: ii
  if(gridDesc(1).eq.0) then
     im    = gridDesc(2)
     jm    = gridDesc(3)
     rlat1 = gridDesc(4)
     rlon1 = gridDesc(5)
     rlat2 = gridDesc(7)
     rlon2 = gridDesc(8)

     if(rlat1.gt.rlat2) then 
        dlat=-gridDesc(9)
     else
        dlat=gridDesc(9)
     endif

     if(rlon1.gt.rlon2) then 
        dlon=-gridDesc(10)
     else
        dlon = gridDesc(10)
     endif

     xmin = 0
     xmax = im+1

     if(im.eq.nint(360/abs(dlon))) xmax=im+2

     ymin = 0
     ymax = jm+1
     nret = 0


     do n=1,npts
        if(abs(rlon(n)).le.360.and.abs(rlat(n)).le.90) then
           if(rlon(n).gt.180) then 
              xpts(n)=1+(rlon(n)-360-rlon1)/dlon
           else
              xpts(n) = 1+(rlon(n)-rlon1)/dlon
           endif
           ypts(n)=1+(rlat(n)-rlat1)/dlat
           if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
                ypts(n).ge.ymin.and.ypts(n).le.ymax) then
              nret=nret+1
           else
              xpts(n)=fill
              ypts(n)=fill
           endif
        else
           xpts(n)=fill
           ypts(n)=fill
        endif
     enddo
  else
     iret=-1
     do n=1,npts
        rlon(n)=fill
        rlat(n)=fill
     enddo
  endif


end subroutine compute_grid_coord_latlon

! 
! !ROUTINE: compute_grid_coord_gauss
! \label{compute_grid_coord_gauss}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   05-27-04 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_grid_coord_gauss(gridDesc,npts,fill,xpts,ypts,&
     rlon,rlat,nret)

  implicit none
! !ARGUMENTS: 
  real            :: gridDesc(50)
  integer         :: npts
  real            :: fill
  real            :: xpts(npts),ypts(npts)
  real            :: rlat(npts)
  real            :: rlon(npts)
  integer         :: nret

! !DESCRIPTION:
!  This subroutine computes the grid coordinates of 
!  the specified domain for a gaussian cylindrical projection.
!  This routine is based on the grid
!  decoding routines in the NCEP interoplation package. 
!  
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     output grid x point coordinates
!    \item[ypts]
!     output grid y point coordinates
!    \item[rlat]    
!     input latitudes in degrees
!    \item[rlon]    
!     input longitudes in degrees
!    \end{description}
!
!  The routines invoked are: 
!  \begin{description}
!   \item[gausslat](\ref{gausslat}\\
!     Computes latitude values in gaussian
!  \end{description}
!EOP
  real, parameter :: pi=3.14159265358979
  integer, parameter :: jgmax=2000
  real :: dpr
  real :: rlata,rlatb
  integer :: im,jm, jg, j, ja, n
  real :: rlat1,rlon1, rlat2, rlon2
  real :: hi, wb
  real :: dlon
  real :: xmin,xmax,ymin,ymax
  real :: alat(0:jgmax+1),blat(jgmax)
  integer :: iscan,jscan,nscan, iret
  real :: yptsa, yptsb
  integer :: jh, j1, j2
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  dpr =180./pi 
  if(gridDesc(1).eq.4.and.gridDesc(10)*2.le.jgmax) then
     im=gridDesc(2)
     jm=gridDesc(3)
     rlat1=gridDesc(4)
     rlon1=gridDesc(5)
     rlat2=gridDesc(7)
     rlon2=gridDesc(8)
     jg=gridDesc(10)*2
     iscan=mod(nint(gridDesc(11))/128,2)
     jscan=mod(nint(gridDesc(11))/64,2)
     nscan=mod(nint(gridDesc(11))/32,2)
     hi=(-1.)**iscan
     jh=(-1)**jscan
     dlon=hi*(mod(hi*(rlon2-rlon1)-1+3600,360.)+1)/(im-1)
     call gausslat(jg,alat(1),blat)
     do ja=1,jg
        alat(ja)=dpr*asin(alat(ja))
     enddo
     alat(0)=180.-alat(1)
     alat(jg+1)=-alat(0)
     j1=1
     do while(j1.lt.jg.and.rlat1.lt.(alat(j1)+alat(j1+1))/2)
        j1=j1+1
     enddo
     j2=j1+jh*(jm-1)
     xmin=0
     xmax=im+1
     if(im.eq.nint(360/abs(dlon))) xmax=im+2
     ymin=0.5
     ymax=jm+0.5
     nret=0
! translate grid coordinates to earth coordinates

     if(abs(dlon-gridDesc(9)).gt.0.01) then
        print*, 'problem with the domain calculations : gdswiz04'
        stop
     endif
     do n=1,npts
        xpts(n)=fill
        ypts(n)=fill
        if(abs(rlon(n)).le.360.and.abs(rlat(n)).le.90) then
           xpts(n)=1+hi*mod(hi*(rlon(n)-rlon1)+3600,360.)/dlon
           ja=min(int((jg+1)/180.*(90-rlat(n))),jg)
           if(rlat(n).gt.alat(ja)) ja=max(ja-2,0)
           if(rlat(n).lt.alat(ja+1)) ja=min(ja+2,jg)
           if(rlat(n).gt.alat(ja)) ja=ja-1
           if(rlat(n).lt.alat(ja+1)) ja=ja+1
           yptsa=1+jh*(ja-j1)
           yptsb=1+jh*(ja+1-j1)
           wb=(alat(ja)-rlat(n))/(alat(ja)-alat(ja+1))
           ypts(n)=yptsa+wb*(yptsb-yptsa)
           if(xpts(n).ge.xmin.and.xpts(n).le.xmax.and. & 
                ypts(n).ge.ymin.and.ypts(n).le.ymax) then
              nret=nret+1
           else
              xpts(n)=fill
              ypts(n)=fill
           endif
        endif
     enddo

! projection unrecognized
  else
     iret=-1
     do n=1,npts
        xpts(n)=fill
        ypts(n)=fill
     enddo     
  endif
end subroutine compute_grid_coord_gauss

!BOP
! 
! !ROUTINE: compute_grid_coord_lambert
!  \label{compute_grid_coord_lambert}
!
! !REVISION HISTORY: 
!   04-10-96 Mark Iredell;  Initial Specification
!   07-15-05 Sujay Kumar; Modified verision with floating point arithmetic. 
!
! !INTERFACE:
subroutine compute_grid_coord_lambert(gridDesc,npts,fill,xpts,ypts,& 
     rlon,rlat,nret)
! !USES: 
  use map_utils

  implicit none
! !ARGUMENTS: 
  real            :: gridDesc(50)
  integer         :: npts
  real            :: fill
  real            :: xpts(npts),ypts(npts)
  real            :: rlat(npts)
  real            :: rlon(npts)
  integer         :: nret

! !DESCRIPTION:
!  This subroutine computes the grid coordinates of 
!  the specified domain for a lambert conformal projection.
!  This routine is based on the grid
!  decoding routines in the NCEP interoplation package. 
!  
!  \begin{description}
!    \item[gridDesc]
!     grid description parameters 
!    \item[npts]
!     integer maximum number of coordinates
!    \item[fill]
!     fill value to set invalid output data
!    \item[xpts]
!     output grid x point coordinates
!    \item[ypts]
!     output grid y point coordinates
!    \item[rlat]    
!     input latitudes in degrees
!    \item[rlon]    
!     input longitudes in degrees
!    \end{description}
!
! !NOTE:
!  This routine is currently unsupported. 
!EOP
  type(proj_info) :: proj
  integer  :: i
  !
  ! Esta sequencia segue o padrao grib1 veja o PDS na secao 2
  !

  if(griddesc(1).eq.3) then
     call map_set(PROJ_LC,           & ! proj_code
                  gridDesc(4),       & ! lat1
                  gridDesc(5),       & ! lon1
                  gridDesc(8),       & ! dx
                  gridDesc(7),       & ! stdlon
                  gridDesc(12),      & ! truelat1
                  gridDesc(13),      & ! truelat2
                  nint(gridDesc(2)), & ! idim
                  nint(gridDesc(3)), & ! jdim
                  proj)
     do i=1,npts
        call latlon_to_ij(proj,rlat(i), rlon(i), xpts(i),ypts(i))
     enddo
  endif  

end subroutine compute_grid_coord_lambert

!---------------------------------------------
! TOOLS
!---------------------------------------------


! 
! !ROUTINE : gausslat
! \label{gausslat}
!
! !REVISION HISTORY:
!   04-16-92 Mark Iredell; Initial Specification
!   10-20-97 Mark Iredell; Increased precision
!   05-14-02 Urzula Jambor; Reduced limit of eps from e-12 to e-7
!
! !INTERFACE:
subroutine gausslat(jmax,slat,wlat)
  implicit none 
! !ARGUMENTS: 
  integer       :: jmax
  real          :: slat(jmax)
  real          :: wlat(jmax)
! !DESCRIPTION:
!   This subroutine computes gaussian latitudes
!   Computes cosines of colatitude and gaussian weights
!   on the gaussian latitudes.  the gaussian latitudes are at
!   the zeroes of the legendre polynomial of the given order.
!
!  The arguments are: 
!  \begin{description}
!    \item[jmax]
!     input number of latitudes
!    \item[slat]
!     cosines of colatitude
!    \item[wlat]
!     gaussian weights
!  \end{description}
!EOP
  real, parameter :: pi=3.14159265358979
  real, parameter :: eps=1.e-7
  integer, parameter :: jz=50
  real :: c
  integer:: jh, jhe, n, j
  real :: spmax, sp, r
  real :: pk(jmax/2),pkm1(jmax/2),pkm2(jmax/2)
  real :: bz(jz)
  data bz        / 2.4048255577,  5.5200781103, & 
       8.6537279129, 11.7915344391, 14.9309177086, 18.0710639679, &
       21.2116366299, 24.3524715308, 27.4934791320, 30.6346064684, & 
       33.7758202136, 36.9170983537, 40.0584257646, 43.1997917132, & 
       46.3411883717, 49.4826098974, 52.6240518411, 55.7655107550, & 
       58.9069839261, 62.0484691902, 65.1899648002, 68.3314693299, & 
       71.4729816036, 74.6145006437, 77.7560256304, 80.8975558711, & 
       84.0390907769, 87.1806298436, 90.3221726372, 93.4637187819, & 
       96.6052679510, 99.7468198587, 102.888374254, 106.029930916, & 
       109.171489649, 112.313050280, 115.454612653, 118.596176630, & 
       121.737742088, 124.879308913, 128.020877005, 131.162446275, & 
       134.304016638, 137.445588020, 140.587160352, 143.728733573, & 
       146.870307625, 150.011882457, 153.153458019, 156.295034268 /

  c=(1.-(2./pi)**2)*0.25
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  jh=jmax/2
  jhe=(jmax+1)/2
  r=1./sqrt((jmax+0.5)**2+c)
  do j=1,min(jh,jz)
     slat(j)=cos(bz(j)*r)
  enddo
  do j=jz+1,jh
     slat(j)=cos((bz(jz)+(j-jz)*pi)*r)
  enddo
  spmax=1.
  do while(spmax.gt.eps)
     spmax=0.
     do j=1,jh
        pkm1(j)=1.
        pk(j)=slat(j)
     enddo
     do n=2,jmax
        do j=1,jh
           pkm2(j)=pkm1(j)
           pkm1(j)=pk(j)
           pk(j)=((2*n-1)*slat(j)*pkm1(j)-(n-1)*pkm2(j))/n
        enddo
     enddo
     do j=1,jh
        sp=pk(j)*(1.-slat(j)**2)/(jmax*(pkm1(j)-slat(j)*pk(j)))
        slat(j)=slat(j)-sp
        spmax=max(spmax,abs(sp))
     enddo
  enddo
  do j=1,jh
     wlat(j)=(2.*(1.-slat(j)**2))/(jmax*pkm1(j))**2
     slat(jmax+1-j)=-slat(j)
     wlat(jmax+1-j)=wlat(j)
  enddo
  if(jhe.gt.jh) then
     slat(jhe)=0.
     wlat(jhe)=2./jmax**2
     do n=2,jmax,2
        wlat(jhe)=wlat(jhe)*n**2/(n-1)**2
     enddo
  endif
  ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  return
end subroutine gausslat

! 
! !ROUTINE: get_field_pos
!  \label{get_field_pos}
!
! !REVISION HISTORY:
!   04-10-96  Mark Iredell; Initial Specification
!   03-11-96  Mark Iredell; Allowed hemispheric grids to wrap over one pole
!   05-27-04  Sujay Kumar; Modified code with floating point arithmetic
!
! !INTERFACE:
function get_fieldpos(i,j,gridDesc) result(field_pos)

  implicit none
! !ARGUMENTS: 

  integer     :: field_pos
  real        ::  gridDesc(50)
  integer     ::  i,j

! !DESCRIPTION: 
!  This subprogram returns the field position for a given grid point
!  based on the input grid definition.
!  The arguments are: 
!  \begin{description}
!    \item[i]
!     integer x grid point
!    \item[j]
!     integer y grid point
!    \item[gridDesc] 
!     grid description parameters 
!    \item[field\_pos]    
!     integer position in grid field to locate grid point
!    \end{description}
!EOP

 
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
  im=gridDesc(2)
  jm=gridDesc(3)
  kscan=0
  is1=0
  nscan=mod(nint(gridDesc(20))/32,2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ACCOUNT FOR WRAPAROUNDS IN EITHER DIRECTION
  ii=i
  jj=j
  if(gridDesc(1).eq.0.or.gridDesc(1).eq.1.or.gridDesc(1).eq.4) then
     rlon1=gridDesc(5)
     rlon2=gridDesc(8)
     iscan=mod(nint(gridDesc(20))/128,2)
     dlon = gridDesc(9)
     ig=nint(360/abs(dlon))
     if(im.ge.ig) then
        ii=mod(i-1+ig,ig)+1
        if((j.le.0.or.j.ge.jm+1).and.mod(ig,2).eq.0) then
           if(gridDesc(1).eq.0) then
              rlat1=gridDesc(4)
              rlat2=gridDesc(7)
              dlat=abs(rlat2-rlat1)/(jm-1)
              if(j.le.0.and.abs(rlat1).gt.90-0.25*dlat) then
                 jj=2-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.le.0.and.abs(rlat1).gt.90-0.75*dlat) then
                 jj=1-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.ge.jm+1.and.abs(rlat2).gt.90-0.25*dlat) then
                 jj=2*jm-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.ge.jm+1.and.abs(rlat2).gt.90-0.75*dlat) then
                 jj=2*jm+1-j
                 ii=mod(ii-1+ig/2,ig)+1
              endif
           elseif(gridDesc(1).eq.4) then
              jg=gridDesc(10)*2
              if(j.le.0.and.jm.eq.jg) then
                 jj=1-j
                 ii=mod(ii-1+ig/2,ig)+1
              elseif(j.ge.jm+1.and.jm.eq.jg) then
                 jj=2*jm+1-j
                 ii=mod(ii-1+ig/2,ig)+1
              endif
           endif
        endif
     endif
  endif
  if(ii.ge.1.and.ii.le.im.and.jj.ge.1.and.jj.le.jm) then
     if(nscan.eq.0) then
        field_pos=ii+(jj-1)*im
     else
        field_pos=jj+(ii-1)*jm
     endif
  else
     field_pos=0
  endif
end function get_fieldpos


! !ROUTINE: polfixs
!  \label{polfixs}
!
! !REVISION HISTORY:
!   04-10-96  Mark Iredell; Initial Specification
!
! !INTERFACE:
subroutine polfixs(nm,nx,rlat,rlon,ib,lo,go)
  implicit none
! !ARGUMENTS: 
  integer         :: nm
  integer         :: nx
  real            :: rlat(nm)
  real            :: rlon(nm)
  integer         :: ib
  logical*1       :: lo(nx)
  real            :: go(nx)

!
! !DESCRIPTION: 
! This subroutine averages multiple pole scalar values
! on a latitude/longitude grid.  bitmaps may be averaged too.
!
!  The arguments are:
!  \begin{description}
!  \item[nm]
!    number of grid points
!  \item[nx]
!    leading dimensition of fields
!  \item[rlat]
!    latitudes in degrees
!  \item[rlon]
!    longitudes in degrees
!  \item[ib]
!    integer bitmap flags
!  \item[lo]
!    logical bitmaps 
!  \item[go]
!    returned scalar value
!  \end{description}
!        
!EOP  
  integer         :: n, k
  real            :: tsp, gnp, gsp, wsp, tnp, wnp
  real, PARAMETER :: rlatnp=89.9995, rlatsp=-89.9995

     wnp=0.0
     gnp=0.0
     tnp=0.0
     wsp=0.0
     gsp=0.0
     tsp=0.0
     !  average multiple pole values
     do n=1,nm
        if(rlat(n).ge.rlatnp) then
           wnp=wnp+1
           if(ib.eq.0.or.lo(n)) then
              gnp=gnp+go(n)
              tnp=tnp+1
           endif
        elseif(rlat(n).le.rlatsp) then
           wsp=wsp+1
           if(ib.eq.0.or.lo(n)) then
              gsp=gsp+go(n)
              tsp=tsp+1
           endif
        endif
     enddo
     !  distribute average values back to multiple poles
     if(wnp.gt.1) then
        if(tnp.ge.wnp/2) then
           gnp=gnp/tnp
        else
           gnp=0.
        endif
        do n=1,nm
           if(rlat(n).ge.rlatnp) then
              if(ib.ne.0) lo(n)=tnp.ge.wnp/2
              go(n)=gnp
           endif
        enddo
     endif
     if(wsp.gt.1) then
        if(tsp.ge.wsp/2) then
           gsp=gsp/tsp
        else
           gsp=0.
        endif
        do n=1,nm
           if(rlat(n).le.rlatsp) then
              if(ib.ne.0) lo(n)=tsp.ge.wsp/2
              go(n)=gsp
           endif
        enddo
     endif
end subroutine polfixs

function is_same_grid(griddesc1,griddesc2) result(flag)
   ! verifica se é a mesma grade
   implicit none
   real, dimension(50) :: gridDesc1
   real, dimension(50) :: gridDesc2
   logical :: flag

   flag = (soma_gd(gridDesc1).eq.soma_gd(gridDesc2))

end function

function soma_gd(gridDesc) result(soma)
   implicit none
   real, dimension(50) :: gridDesc
   real :: soma

   soma = 0

   ! Projeçao
    
   soma = soma + gridDesc(1)

   ! Latitude

   soma = soma + gridDesc(4) + gridDesc(7) + gridDesc(9)

   ! Longitude

   if (gridDesc(5).lt.180)then
      soma = soma + gridDesc(5) + 360.0
   else
      soma = soma + gridDesc(5)
   endif

   if (gridDesc(8).lt.180)then
      soma = soma + gridDesc(8) + 360.0
   else
      soma = soma + gridDesc(8)
   endif

   soma = soma + gridDesc(10)

end function

END MODULE
