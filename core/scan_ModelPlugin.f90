MODULE scan_Modelplugin

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
   use m_constants, only: tinyStr, shortStr, normalStr, LongStr,&
                          i4, r4
   use MathExpress, only: MathOper
   
  implicit none
  private

  public :: scanModel
  public :: ModelType
  public :: EvalVar


  type :: scanModel

     !
     ! Model info
     !
     integer, pointer :: nexp => null()
     type(ModelType), pointer :: CurrModel  => null()
     type(ModelType), pointer :: FirstModel => null()

     ! for mathematical evaluation

     class(MathOper), pointer :: MathEval => null()

     ! directory of config tables

     character(len=normalStr) :: tables

     !
     ! routines
     !
   contains
     private
     procedure, public :: insertModel => insertModel_
     procedure, public :: getModel => getModel_
     procedure, public :: getField => getField_
     procedure, public :: getBitMap => getBitMap_
     procedure, public :: readModelConf => readModelConf_
     
  endType

  type :: ModelType
     character(len=LongStr) :: Name_
     character(len=LongStr) :: FileName_
     character(len=tinyStr) :: FileType_
     character(len=tinyStr) :: ExpName_
     character(len=tinyStr) :: Type_ ! Reference, Experiment, climatology
     real(kind = r4)        :: undef_

     ! grid info
     real                      :: GDesc(200)
     type(GridDef),    pointer :: gridInfo => null()
     type(GridDef),    pointer :: FirstGridInfo => null()

     !interp info to scantec domain
     integer(kind=i4), allocatable :: n11(:), n12(:), n21(:), n22(:)
     real(kind=r4),    allocatable :: w11(:), w12(:), w21(:), w22(:)

     ! Field
     real(kind=r4), pointer :: Field(:,:) => null()
     logical, pointer       :: bitMap(:,:) => null()

     !Vars to evaluate from model
     type(EvalVar), pointer :: var => null()
     type(EvalVar), pointer :: FirstVar => null()

     type(ModelType),  pointer :: next => null()

   contains
     procedure, public :: getModelVar => getModelVar_
     procedure, public :: getDimInfo => getDimInfo_
     procedure, public :: getDimVec => getDimVec_
     procedure, public :: getMapping => getMapping_
  end type ModelType

  type GridDef
     character(len=ShortStr) :: DName       ! dimension name
     character(len=ShortStr) :: mapping     ! Linear, Levels, gauss
     integer                 :: num         ! number of points
     real                    :: start_coord
     real                    :: incr_coord
     real, pointer           :: coord(:) => null()
     type(GridDef), pointer  :: next => null()
  endtype GridDef

  type EvalVar
     character(len=shortStr) :: Sys_ ! Scantec variable name
     character(len=shortStr) :: mod_ ! Model variable name

     ! Field info
     real(kind = r4), pointer :: Field(:) => null()
     logical, pointer         :: bitMap(:) => null()

     type(EvalVar), pointer :: next => null()
  end type EvalVar

  
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


  !-------------------------------------------------------------------
  ! !PUBLIC MEMBER FUNCTIONS:
  !-------------------------------------------------------------------

  !public :: scan_Models_Plugin  

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
!  subroutine scan_models_plugin
!    !  !REVISION HISTORY: 
!    !  18 May 2020    J. G. de Mattos  Initial Specification
!    !
!    !------------------------------------------------------------------
!    !BOC
!    character(len=*),parameter :: myname_=myname//'::scan_Models_Plugin'
!    real, pointer :: rlat(:) => null()
!    real, pointer :: rlon(:) => null()
!    real :: GDesc(200)
!    integer, pointer :: xdim => null()
!    integer, pointer :: ydim => null()
!    integer :: i, j, k
!    character(len=ShortStr), pointer :: mapping => null()
!    type(ModelType), pointer :: Model => null()
!
!    Model => scantec%FirstModel
!    do while(associated(Model))
!    
!       call readModelConf(Model)
!
!       ! compute weights to be used for interpolation
!       rlat => Model%getDimVec('ydim:')
!       rlon => Model%getDimVec('xdim:')
!
!       xdim => Model%getDimInfo('xdim:')
!       ydim => Model%getDimInfo('ydim:')
!       mapping => Model%getMapping('xdim:')
!
!       if (i90_lcase(mapping) .eq. 'linear')then
!          Model%GDesc = 0
!          Model%GDesc( 1) = 0
!          Model%GDesc( 2) = xdim
!          Model%GDesc( 3) = ydim
!          Model%GDesc( 4) = rlat(1)
!          Model%GDesc( 5) = rlon(1)
!          Model%GDesc( 6) = 128
!          Model%GDesc( 7) = rlat(ydim)
!          Model%GDesc( 8) = rlon(xdim)
!          Model%GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)
!          Model%GDesc(10) = (rlat(ydim)-rlat(1))/(ydim-1)
!       else
!          ! For now support only Gaussian grid
!          Model%GDesc = 0
!          Model%GDesc( 1) =   4
!          Model%GDesc( 2) = xdim
!          Model%GDesc( 3) = ydim
!          Model%GDesc( 4) = rlat(1)
!          Model%GDesc( 5) = rlon(1)
!          Model%GDesc( 6) = 128
!          Model%GDesc( 7) = rlat(ydim)
!          Model%GDesc( 8) = rlon(xdim)
!          Model%GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)
!          Model%GDesc(10) = ydim/2.0
!          
!       endif
!
!       deallocate(rlon)
!       deallocate(rlat)
!       Model => Model%next
!    enddo    
!
!  end subroutine scan_models_plugin
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
   subroutine readModelConf_(self)

!
! !INPUT PARAMETERS:
!
     class(scanModel), intent(inout) :: self
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
      character(len = shortStr)    :: svar
      character(len = shortStr)    :: mvar
      logical :: found

      type(GridDef), pointer :: DimTmp => null()
      type(ModelType), pointer :: Model => null()

      !
      ! Open Configure File for model
      !

      fileModelConf = trim(self%tables)//'/'//trim(Model%Name_)//'.model'
      inquire(file=trim(fileModelConf),exist=found)
      if(.not.found)then
         write(msg,'(2A)')'File not found :', trim(fileModelConf)
         call i90_perr(trim(myname_),trim(msg),-1)
         stop 99002
      endif
      call i90_LoadF(trim(fileModelConf), iret)
      if(iret.ne.0)then
         write(msg,'(3A)')'i90_LoadF("',trim(fileModelConf),'")'
         call i90_perr(trim(myname_),trim(msg),iret)
         stop 99003
      endif

#ifdef DEBUG
      write(stdout,'(A)')' '
      write(stdout,'(A,1x,A)')char(27)//'[32;1mGetting model info from:',&
                              trim(Model%Name_)//'.model'//char(27)//'[m'
#endif
      !
      ! Get information about model post-processed files
      !

      Model => self%currModel

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
         if (iret .eq. 0)then
            Model%var%mod_ = i90_lcase(trim(mvar))
         else
            call i90_perr(trim(myname_),'some issue with model var name ...', iret)
            call i90_perr(trim(myname_),'look inside '//trim(Model%Name_)//'.model')
            call i90_die(trim(myname))
         endif
         ! some times user made a mathexpress with blank space
         ! so, get it and merge
         do while (iret .eq. 0)
            call i90_gtoken(mvar,iret)
            if (iret .eq. 0)then
               Model%var%mod_ = trim(Model%var%mod_)//trim(i90_lcase(mvar))
            endif
         enddo

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
      write(stdout,'(A16,1x,A64)')'SCANTEC','Model'
      Model%var => Model%FirstVar
      do while(associated(Model%var))
         write(stdout,'(A16,1x,A64)')trim(Model%var%Sys_),trim(Model%var%Mod_)
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
       stop 99004
    endif

    GDef%num = i90_Gint(ierr)
    if(ierr.ne.0)then
       call i90_perr(trim(myname_),'i90_Gint( '//trim(label)//'%num ... )', ierr)
       stop 99005
    endif

    call i90_GToken(GDef%mapping,ierr)
    if(ierr.ne.0)then
       call i90_perr(trim(myname_),'i90_GToken( '//trim(label)//'%mapping ... )', ierr)
       stop 99006
    endif

    select case (trim(i90_lcase(GDef%mapping)))

    case('linear')

       Gdef%start_coord = i90_GFloat(ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'i90_GFloat( '//trim(label)//'%start_coord ... )', ierr)
          stop 99007
       endif

       Gdef%incr_coord = i90_GFloat(ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'i90_GFloat( '//trim(label)//'%incr_coord ... )', ierr)
          stop 99008
       endif

       allocate(GDef%coord(GDef%num),stat=ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'Allocate( '//trim(label)//'%coord(:) ... )', ierr)
          stop 99009
       endif

       call GetLinCoords(GDef%start_coord, GDef%incr_coord, GDef%num, GDef%coord, ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'GetLinCoords( '//trim(label)//' ... )', ierr)
          stop 99010
       endif

    case('levels')

       GDef%incr_coord = -1

       allocate(GDef%coord(GDef%num),stat=ierr)
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'Allocate( '//trim(label)//'%coord(:) ... )', ierr)
          stop 99011
       endif

       call GetLevelsCoord( GDef%num, GDef%coord, ierr )
       if(ierr.ne.0)then
          call i90_perr(trim(myname_),'GetLevelsCoords( '//trim(label)//' ... )', ierr)
          stop 99012
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
       write(stdout,'(1x,A,1x,I6,1x,A,1x,2F12.5)') &
            trim(GDef%DName), GDef%num, trim(GDef%mapping), GDef%start_coord, GDef%incr_coord
    else
       write(stdout,'(1x,A,1x,I6,1x,A)') trim(GDef%DName), GDef%num, trim(GDef%mapping)
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

  !------------------------------------------------------------!
  !scanType

  function getFirstModel(self) result(Model)
    class(scanModel), intent(in) :: self
    type(ModelType), pointer :: Model

    Model => self%FirstModel
  end function getFirstModel

  subroutine insertModel_(self, runType, ModelName, ExpName, FileName)
    class(scanModel), intent(inout) :: self
    character(len=*), intent(in   ) :: ModelName
    character(len=*), intent(in   ) :: runType
    character(len=*), intent(in   ) :: ExpName
    character(len=*), intent(in   ) :: FileName


    real, pointer :: rlat(:) => null()
    real, pointer :: rlon(:) => null()
    integer, pointer :: xdim => null()
    integer, pointer :: ydim => null()
    character(len=ShortStr), pointer :: mapping => null()
    type(ModelType), pointer :: Model => null()

    

    !verify if is the first model to include
    if(.not.associated(self%FirstModel))then
       allocate(self%nexp); self%nexp = 0
       allocate(self%FirstModel)
       self%currModel => self%FirstModel
    else
       allocate(self%currModel%next)
       self%currModel => self%currModel%next
    endif

    Model => self%currModel

    Model%Name_     = trim(ModelName)
    Model%FileName_ = trim(FileName)
    Model%Type_     = trim(runType)
    Model%ExpName_  = trim(ExpName)

    call self%readModelConf( )

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
       Model%GDesc( 6) = 128
       Model%GDesc( 7) = rlat(ydim)
       Model%GDesc( 8) = rlon(xdim)
       Model%GDesc( 9) = (rlon(xdim)-rlon(1))/(xdim-1)
       Model%GDesc(10) = ydim/2.0      
    endif

    deallocate(rlon)
    deallocate(rlat)

    if(runType .eq. 'Experiment') self%nexp = self%nexp + 1

  end subroutine insertModel_

  function getModel_(self, MType_, MName_) result(Model)
    class(scanModel), intent(inout) :: self
    character(len=*), intent(in   ) :: MType_
    character(len=*), intent(in   ) :: MName_
    type(ModelType), pointer :: Model


    Model => self%FirstModel
    do while(associated(Model))
       if (trim(Model%Name_) .eq. trim(MName_) .and.&
            trim(Model%Type_) .eq. trim(MType_)) return
       Model => Model%next
    enddo

    write(stdout,'(A,3(1x,A))')'Model Name or Model Type not found!',&
         trim(MType_),'->',trim(MName_)
    stop 99024

  end function getModel_

  subroutine getField_(self, ExpName, Field, istat)
    class(scanModel), intent(in) :: self
    character(len=*), intent(in) :: ExpName
    real, pointer, intent(out) :: Field(:,:)
    integer, optional, intent(out) :: istat

    type(ModelType), pointer :: Model => null()

    if(present(istat)) istat = 0
    Model => self%FirstModel
    do while(associated(Model))
       if (trim(ExpName) .eq. trim(Model%ExpName_))then
          Field => Model%Field
          return
       endif
       Model => Model%next
    enddo
    if(present(istat)) then
       istat = -1
    else
       write(stdout,'(A,1x,A)') 'unknow Experient Name',trim(ExpName)
       stop 99025
    endif
  end subroutine getField_


  subroutine getBitMap_(self, ExpName, BitMap, istat)
    class(scanModel), intent(in) :: self
    character(len=*), intent(in) :: ExpName
    logical, pointer, intent(out) :: BitMap(:,:)
    integer, optional, intent(out) :: istat

    type(ModelType), pointer :: Model => null()


    if(present(istat)) istat = 0
    Model => self%FirstModel
    do while(associated(Model))
       if (trim(ExpName) .eq. trim(Model%ExpName_))then
          BitMap => Model%BitMap
          return
       endif
       Model => Model%next
    enddo
    if(present(istat)) then
       istat = -1
    else
       write(stdout,'(A,1x,A)')'unknow Experiment Name',trim(ExpName)
       stop 99026
    endif
  end subroutine getBitMap_

  !------------------------------------------!
  ! ModelType

  function getModelVar_(self,VarName) result(varInfo)
    class(ModelType),  intent(in) :: self
    character(len=*), intent(in) :: VarName
    type(EvalVar), pointer :: varInfo

    type(EvalVar), pointer :: ModelVar => null()

    VarInfo => self%FirstVar
    do while(associated(VarInfo))
       if(trim(VarName).eq.trim(VarInfo%Sys_)) return
       VarInfo => VarInfo%next
    enddo

    write(stdout,'(A,1x,A)')'unknow variable Name',trim(VarName)

  end function getModelVar_


  function getDimInfo_(self, mdim) result(vdim)
    class(ModelType),  intent(in   ) :: self
    character(len=*), intent(in   ) :: mdim
    integer, pointer :: vdim

    type(gridDef), pointer :: grid => null()

    grid => self%FirstGridInfo
    do while(associated(grid))
       if(trim(mdim) .eq. trim(grid%DName))then
          vdim => grid%num
          return
       endif
       grid => grid%next
    enddo

  end function getDimInfo_


  function getDimVec_(self, mdim) result(vdim)
    class(ModelType),  intent(in   ) :: self
    character(len=*), intent(in   ) :: mdim
    real, pointer :: vdim(:)

    type(gridDef), pointer :: grid => null()
    grid => self%FirstGridInfo
    do while(associated(grid))
       if(trim(mdim) .eq. trim(grid%DName))then
          vdim => grid%coord
          return
       endif
       grid => grid%next
    enddo
    write(stdout,'(A,1x,2A,1x,A)')'Dimension not found in',&
         trim(self%Name_),'.model :',trim(mdim)
    stop 99027

  end function getDimVec_

  function getMapping_(self, mdim) result(vdim)
    class(ModelType),  intent(in   ) :: self
    character(len=*), intent(in   ) :: mdim
    character(len=ShortStr), pointer :: vdim

    type(gridDef), pointer :: grid => null()

    grid => self%FirstGridInfo
    do while(associated(grid))
       if(trim(mdim) .eq. trim(grid%DName))then
          vdim => grid%mapping
          return
       endif
       grid => grid%next
    enddo

  end function getMapping_




END MODULE scan_Modelplugin
