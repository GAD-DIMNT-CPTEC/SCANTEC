MODULE scan_Modelplugin
  use scantec_module
  use bilinInterp
  use m_inpak90, only: i90_LoadF,  & 
                       i90_getVal, &
                       i90_gtoken, &
                       i90_gfloat, &
                       i90_gint,   &
                       i90_gline,  &
                       i90_lcase,  &
                       i90_label,  &
                       i90_perr,   &
                       i90_die,    &
                       i90_fullRelease
   use m_ioutil
   use m_constants, only: tinyStr, shortStr, normalStr,&
                          i4, r4

  
  IMPLICIT NONE
  !BOP
  !
  !  !MODULE: scan_Modelplugin
  ! 
  !  !DESCRIPTION: 
  !   The code in this file provides values of indices used to 
  !   to register functions in the plugin modules
  !  
  !  The index definitions are simply a convention
  !  The user may change these options, and the scantec.conf 
  !  should be changed appropriately to ensure that the correct function
  !  is called at run time
  !
  !  This module contains, also, the definition of the functions used 
  !  for open the numerical models states to use for evaluatin an other 
  !  relevant computations using the models, corresponding to each of 
  !  the Model used in scantec.
  !
  !  !REVISION HISTORY: 
  !  25 Oct 2011    J. G. de Mattos  Initial Specification
  !
  !EOP

  PRIVATE

  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  public :: scan_Models_Plugin  

  !---------------------------------------------------------------------
  !
  !---------------------------------------------------------------------
  character(len=*),parameter :: myname='scan_Modelplugin'

Contains
  !-------------------------------------------------------------------
  !BOP
  !
  ! !ROUTINE: scan_models_plugin
  !  \label{scan_models_plugin}
  !
  ! !DESCRIPTION:
  !
  ! This is a custom-defined plugin point for introducing a new Model. 
  ! The interface mandates that the following routines be implemented
  ! and registered for each of the Model that is included in scantec.
  !
  ! !INTERFACE:
    
    !  !REVISION HISTORY: 
    !  25 Oct 2011    J. G. de Mattos  Initial Specification
    !  15 May 2020    J. G. de Mattos  Adapt to more generic 
    !                                  model access
    !
    !------------------------------------------------------------------
    !BOC
  subroutine scan_models_plugin
    !  !REVISION HISTORY: 
    !  18 May 2020    J. G. de Mattos  Initial Specification
    !
    !------------------------------------------------------------------
    !BOC
    character(len=*),parameter :: myname_=myname//'::scan_Models_Plugin'
    real, pointer :: rlat(:) => null()
    real, pointer :: rlon(:) => null()
    real :: GDesc(200)
    integer, pointer :: xdim => null()
    integer, pointer :: ydim => null()
    integer :: i, j, k
    character(len=ShortStr), pointer :: mapping => null()
    type(ModelType), pointer :: Model => null()

    Model => scantec%FirstModel
    do while(associated(Model))
    
       call readModelConf(Model)

       ! compute weights to be used for interpolation
       rlat => Model%getDimVec('ydim:')
       rlon => Model%getDimVec('xdim:')

       xdim => Model%getDimInfo('xdim:')
       ydim => Model%getDimInfo('ydim:')
       mapping => Model%getMapping('xdim:')

       if (i90_lcase(mapping) .eq. 'linear')then
          Model%GDesc = 0
          Model%GDesc( 1) = 0
          Model%GDesc( 2) = xdim
          Model%GDesc( 3) = ydim
          Model%GDesc( 4) = rlat(1)
          Model%GDesc( 5) = rlon(1)
          Model%GDesc( 6) = 128
          Model%GDesc( 7) = rlat(ydim)
          Model%GDesc( 8) = rlon(xdim)
          Model%GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)
          Model%GDesc(10) = (rlat(ydim)-rlat(1))/(ydim-1)
       else
          ! For now support only Gaussian grid
          Model%GDesc = 0
          Model%GDesc( 1) =   4
          Model%GDesc( 2) = xdim
          Model%GDesc( 3) = ydim
          Model%GDesc( 4) = rlat(1)
          Model%GDesc( 5) = rlon(1)
          Model%GDesc( 6) =     128
          Model%GDesc( 7) = rlat(ydim)
          Model%GDesc( 8) = rlon(xdim)
          Model%GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)
          Model%GDesc(10) = ydim/2.0
          
       endif

       deallocate(rlon)
       deallocate(rlat)
       Model => Model%next
    enddo    

  end subroutine scan_models_plugin
!-----------------------------------------------------------------------------!
!EOC
!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: readModelConf - Read model information from a configure file.
!
! !DESCRIPTION: this routine populate a data type grid with informations from
!               each model by read a configure file that contain grid and
!               variable informations.
!
! !INTERFACE:
!
   subroutine readModelConf(Model)

!
! !INPUT PARAMETERS:
!
      type(ModelType), intent(inout) :: Model

! !REVISION HISTORY:
!
!   10 May 2020 - J. G. de Mattos -  Initial code.
!
!EOP
!-----------------------------------------------------------------------------!
!BOC
      character(len=*),parameter :: myname_=myname//' :: readModelConf( )'

      integer(kind = i4)           :: xpts, ypts, zpts
      integer(kind = i4)           :: iret, i
      real(kind = r4)              :: undef
      real(kind = r4)              :: loni, lati, levi
      real(kind = r4)              :: lonf, latf, levf
      real(kind = r4)              :: dx, dy, dz
      real(kind = r4), allocatable :: lon(:), lat(:), lev(:)

      character(len=TinyStr)       :: ftype, dtype
      character(len = normalStr)   :: fileModelConf
      character(len = NormalStr)   :: msg
      character(len = tinyStr)     :: svar
      character(len = tinyStr)     :: mvar
      logical :: found

      type(GridDef), pointer :: DimTmp => null()

      !
      ! Open Configure File for model
      !

      fileModelConf = trim(scantec%tables)//'/'//trim(Model%Name_)//'.model'
      inquire(file=trim(fileModelConf),exist=found)
      if(.not.found)then
         write(msg,'(2A)')'File not found :', trim(fileModelConf)
         call i90_perr(trim(myname_),trim(msg),-1)
         stop
      endif
      call i90_LoadF(trim(fileModelConf), iret)
      if(iret.ne.0)then
         write(msg,'(3A)')'i90_LoadF("',trim(fileModelConf),'")'
         call i90_perr(trim(myname_),trim(msg),iret)
         stop
      endif

#ifdef DEBUG
      write(stdout,'(A)')' '
      write(stdout,'(A,1x,A)')char(27)//'[32;1mGetting model info from:',&
                              trim(Model%Name_)//'.model'//char(27)//'[m'
#endif
      !
      ! Get information about model post-processed files
      !
      
      ! ftype: should be binary, grib or netcdf file
      call i90_getVal('ftype:',Model%fileType_)

      ! undefined value used by model
      call i90_getVal('undef:',Model%undef_)

      ! Grid dimensions

      allocate(Model%FirstGridInfo, stat=iret)
      Model%gridInfo => Model%FirstGridInfo
      DimTmp => Model%gridInfo

      call GetDef(DimTmp,'xdim:',iret)
      if(iret.ne.0)then
         call i90_perr(trim(myname_),'GetDef( xdim ... )', iret)
!         if (.not.present(istat)) stop
!         istat = ierr
         return
      endif
      !
      ! longitude should be at 0 360.0
      !

      do i=1,DimTmp%num
         DimTmp%coord(i) = mod(DimTmp%coord(i)+3600.0,360.0)
      enddo      
      !-------------------------------------------
#ifdef DEBUG
      call GDef_Print(Model%gridInfo)
#endif
      
      allocate(Model%gridInfo%next)
      Model%gridInfo => Model%gridInfo%next
      DimTmp => Model%gridInfo

      call GetDef(DimTmp,'ydim:',iret)
      if(iret.ne.0)then
         call i90_perr(trim(myname_),'GetDef( ydim ... )', iret)
!         if (.not.present(istat)) stop
!         istat = ierr
         return
      endif

#ifdef DEBUG 
      call GDef_Print(Model%gridInfo)
#endif

      allocate(Model%gridInfo%next)
      Model%gridInfo => Model%gridInfo%next
      DimTmp => Model%gridInfo
      
      call GetDef(DimTmp,'zdim:',iret)
      if(iret.ne.0)then
         call i90_perr(trim(myname_),'GetDef( zdim ... )', iret)
!         if (.not.present(istat)) stop
!         istat = ierr
         return
      endif
  
#ifdef DEBUG 
      call GDef_Print(Model%gridInfo)
#endif

      !
      ! Get information about variables
      !
      allocate(Model%FirstVar)
      Model%var => Model%FirstVar
      call i90_label('vars:',iret)
      if(iret.eq.0)call i90_gline(iret)
      if(iret.ne.0)then
         call i90_perr(trim(myname_),'GetDef( variables ... )', iret)
         call i90_die(trim(myname))
      endif
      do while(iret.eq.0)
         ! get variable name and level from scantec
         call i90_gtoken(svar, iret)
         if (iret .ne. 0)then
            call i90_perr(trim(myname_),'some issue with var list ... ', iret)
            call i90_die(trim(myname))
         endif

         Model%var%Sys_ = i90_lcase(trim(svar))

         ! get variable name and level from model
         call i90_gtoken(mvar,iret)

         if(iret.eq.0)then
            select case(mvar(1:1))
               ! if a variable model need be pre-processed to
               ! obtain a scantec var
               case ('@')
                  ! Get function name and arguments
                  Model%var%mod_   = trim(mvar(2:len_trim(mvar)))
                  Model%var%deriv_ = .true.

!                  ! get arguments
!                  allocate(Model%var%FirstFuncArg)
!                  Model%var%funcArg => Model%var%FirstFuncArg
!                  call i90_gtoken(mvar,iret)
!                  if(iret .ne. 0)then
!                     call i90_perr(trim(myname_),'@func (need at least one function arg)', iret)
!                     call i90_die(trim(myname_)) ! não precisa matar o processo, so pular p/ outra var
!                  endif
!                  Model%var%funcArg%str_ = i90_lcase(trim(mvar))
!                  do while(iret .eq. 0)
!
!                     call i90_Gtoken(mvar,iret)
!                     if (iret .eq. 0) then
!                        allocate(Model%var%funcArg%next)
!                        Model%var%funcArg => Model%var%funcArg%next
!                        Model%var%funcArg%str_ = i90_lcase(trim(mvar))
!                     endif
!                     
!                  enddo

               case default
                  Model%var%mod_   = i90_lcase(trim(mvar))
                  Model%var%deriv_ = .false.
            end select
         else
           call i90_perr(trim(myname_),'some issue with model var name ...', iret)
           call i90_perr(trim(myname_),'look inside '//trim(Model%Name_)//'.model')
           call i90_die(trim(myname))
         endif
         ! get next line
         !    - iret =  0, next line ok
         !    - iret = -1, end of buffer (some problem with table)
         !    - iret = +1, end of table
         call i90_gline(iret)
         if(iret.eq.0)then
            allocate(Model%var%next)
            Model%var => Model%var%next
         elseif(iret.lt.0)then
            call i90_perr(trim(myname),'get var list', iret)
            call i90_die(trim(myname))
         endif
      enddo

      call i90_fullRelease(iret)
      if(iret.lt.0)then
         call i90_perr(trim(myname),'i90_fullRelease', iret)
      endif

#ifdef DEBUG
      write(stdout,'(A)')''
      write(stdout,'(A43)')char(27)//'[33;1mList of Model Variables'//char(27)//'[m'
      write(stdout,'(A16,1x,A16)')'SCANTEC','Model'
      Model%var => Model%FirstVar
      do while(associated(Model%var))
         write(stdout,'(A16,1x,A16)')trim(Model%var%Sys_),trim(Model%var%Mod_)
         Model%var => Model%var%next
      enddo
      write(stdout,'(A)')''
#endif


   end subroutine
!EOC
!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: GetDef - Read dimension model information from file.
!
! !DESCRIPTION: this routine read information about dimension of the model from
!               contain grid and variable informations.
!
! !INTERFACE:
!

  subroutine GetDef(GDef, label, ierr)

!
! !INPUT PARAMETERS:
!

    type(GridDef),     intent(inout) :: GDef
    character(len=*),  intent(in   ) :: label
!
! !OUTPUT PARAMETERS:
!
    integer, optional, intent(  out) :: ierr

! !REVISION HISTORY:
!
!   16 May 2013 - J. G. de Mattos - Initial code from m_GrADSfiles.f90
!   10 May 2020 - J. G. de Mattos - adapt to read scantec model info
!
!EOP
!-----------------------------------------------------------------------------!
!BOC

    character(len=*), parameter :: myname_ = myname//' :: GetDef(...)'

    GDef%DName = trim(label)

    call i90_label(trim(label),ierr)
    if(ierr.ne.0)then
       call i90_perr(trim(myname_),'i90_label( '//trim(label)//' ... )', ierr)
       stop
    endif

    GDef%num = i90_Gint(ierr)
    if(ierr.ne.0)then
       call i90_perr(trim(myname_),'i90_Gint( '//trim(label)//'%num ... )', ierr)
       stop
    endif

    call i90_GToken(GDef%mapping,ierr)
    if(ierr.ne.0)then
       call i90_perr(trim(myname_),'i90_GToken( '//trim(label)//'%mapping ... )', ierr)
       stop
    endif

    select case (trim(i90_lcase(GDef%mapping)))

    case('linear')

       Gdef%start_coord = i90_GFloat(ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'i90_GFloat( '//trim(label)//'%start_coord ... )', ierr)
          stop
       endif

       Gdef%incr_coord = i90_GFloat(ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'i90_GFloat( '//trim(label)//'%incr_coord ... )', ierr)
          stop
       endif

       allocate(GDef%coord(GDef%num),stat=ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'Allocate( '//trim(label)//'%coord(:) ... )', ierr)
          stop
       endif

       call GetLinCoords(GDef%start_coord, GDef%incr_coord, GDef%num, GDef%coord, ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'GetLinCoords( '//trim(label)//' ... )', ierr)
          stop
       endif

    case('levels')

       GDef%incr_coord = -1

       allocate(GDef%coord(GDef%num),stat=ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'Allocate( '//trim(label)//'%coord(:) ... )', ierr)
          stop
       endif

       call GetLevelsCoord( GDef%num, GDef%coord, ierr )
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'GetLevelsCoords( '//trim(label)//' ... )', ierr)
          stop
       endif

       GDef%start_coord = GDef%coord(1)

    case default

       call i90_perr(trim(myname_),trim(label)//' not implemented yet!')

    end select


    return
  end subroutine GetDef
!EOC
!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: GetLinCoords - calculate linear coordenates.
!
! !DESCRIPTION: this routine calculate linear coordinates from initial point to
!               npts*incr.
!
! !INTERFACE:
!

  subroutine GetLinCoords( start, incr, npts, coord, istat )
!
! !INPUT PARAMETERS:
!

    real,              intent(in   ) :: start
    real,              intent(in   ) :: incr
    integer,           intent(in   ) :: npts
!
! !OUTPUT PARAMETERS:
!
    real,              intent(inout) :: coord(:)
    integer, optional, intent(  out) :: istat

! !REVISION HISTORY:
!
!   16 May 2013 - J. G. de Mattos - Initial code from m_GrADSfiles.f90
!   10 May 2020 - J. G. de Mattos - adapt to read scantec model info
!
!EOP
!-----------------------------------------------------------------------------!
!BOC

    integer :: i

    do i = 1, npts
       coord(i) = start + incr*(i-1)
    enddo

  end subroutine GetLinCoords
!EOC
!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
!
! !IROUTINE: GetLevelsCoords - read levels coordenates.
!
! !DESCRIPTION: this routine read levels coordinates from configure file.
!               
!
! !INTERFACE:
!

  subroutine GetLevelsCoord( npts, coord, istat )
!
! !INPUT PARAMETERS:
!
    integer,           intent(in   ) :: npts
!
! !OUTPUT PARAMETERS:
!
    real,              intent(inout) :: coord(:)
    integer, optional, intent(  out) :: istat

! !REVISION HISTORY:
!
!   16 May 2013 - J. G. de Mattos - Initial code from m_GrADSfiles.f90
!   10 May 2020 - J. G. de Mattos - adapt to read scantec model info
!
!EOP
!-----------------------------------------------------------------------------!
!BOC

    integer :: i
    integer :: iret

    i = 1
    do while(i.le.npts)
       coord(i) = i90_GFloat(iret)
       if(iret.ne.0) then
          call i90_GLine(iret)
          i = i - 1
       endif
       i = i + 1
    enddo

  end subroutine GetLevelsCoord
!EOC
!-----------------------------------------------------------------------------!
  subroutine GDef_Print( GDef )

    type(GridDef), intent(in) :: GDef

    if (trim(i90_lcase(GDef%mapping)).eq.'linear')then
       write(stdout,'(1x,A,1x,I3,1x,A,1x,2F12.5)') &
            trim(GDef%DName), GDef%num, trim(GDef%mapping), GDef%start_coord, GDef%incr_coord
    else
       write(stdout,'(1x,A,1x,I3,1x,A)') trim(GDef%DName), GDef%num, trim(GDef%mapping)
       call PLevels_(GDef%coord)
    endif

  end subroutine GDef_Print

  subroutine PLevels_(coord)

    real, intent(in   ) :: coord(:)

    integer :: count
    integer :: i
    integer :: n

    n = size(coord)

    count = 0
    do while(count .lt. n )
       write(*,'(8F12.5)')(coord(i),i = count+1,min(count+8,n))
       count = count + 8
    enddo


  end subroutine PLevels_


END MODULE scan_Modelplugin
