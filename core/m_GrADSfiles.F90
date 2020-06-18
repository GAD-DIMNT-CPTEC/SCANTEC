!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !MODULE: m_GrADSfiles - Read-only files in the GrADS format
!
! !DESCRIPTION:
!
! !INTERFACE:

module m_GrADSfiles
  use m_inpak90
  use m_ioutil

  implicit none
  private

  !
  ! Undefine Value
  !
  real, parameter :: UDEF_ = -9.9e+20

  !
  ! Type of data
  !

  integer, parameter :: ntypes = 4 ! # of supported types

  integer, parameter :: tbin = 1 ! binary ieee
  integer, parameter :: tgrib = 2 ! grid
  integer, parameter :: tnetcdf = 3 ! netcdf
  integer, parameter :: tstation = 4 ! station grads format

  !      integer, parameter :: tbufr      = 5
  !      integer, parameter :: tgrib2     = 6
  !      integer, parameter :: thdfsds    = 7
  !      integer, parameter :: thdf5_grid = 8

  character(len=10)  :: tname(ntypes) ! Names defined at GrADS_Default routine

  ! access methods of a Fortran unformatted file
  !---------------------------------------------

  integer, parameter :: iacc_DIRECT = 1
  integer, parameter :: iacc_SEQUENTIAL = 2

  integer, parameter :: stat_DEFINED = 1
  integer, parameter :: stat_UNDEF = 0

  public :: GrADSfiles       ! the class data stucture
  public :: GrADS_open       ! open a GrADSfiles instance
  public :: GrADS_input      ! read a GrADS 2d/3d field
  public :: GrADS_close      ! close a GrADSfiles instance
  public :: GrADS_getVardims ! get the dimensions of a variable
  public :: GrADS_getdim     ! get GrADS dimensions
  public :: GrADS_zdef       ! get the levels of a variable
  public :: GrADS_getVarsName
  public :: GrADS_getVarNLevs
  public :: GrADS_Undef

  ! The current version only has limited supports to a
  ! GrADS file, with restrictions on the grid, time,
  ! and the file format, etc.

  type GrADS_Opt
     logical :: pascals
     logical :: yrev
     logical :: zrev
     logical :: template
     logical :: cal365day
     logical :: byteswapped
     logical :: cray_32bit_ieee
     logical :: endianess
     logical :: iacc
  end Type GrADS_Opt

  type GrADS_DEF
     integer          :: num    ! dimension size
     character(len=6) :: type
     real, pointer    :: vec(:) => null()! vector of coordenates
  end type GrADS_DEF

  type GrADS_TDEF
     integer          :: num   ! Total number of times
     character(len=6) :: type  ! Type of increment
     integer          :: ymd   ! Year, month and day
     integer          :: hms   ! Hour, minute, second
     integer          :: incr  ! increment of time
  end Type GrADS_TDEF

  type GrADS_VAR
     integer              :: id
     character(len=12)    :: name     ! Variable name
     integer              :: levs     ! number of levels by variable
     integer              :: units
     character(len=25)    :: descr    ! descrition of variable
     integer              :: n_rec    ! number of levels by variable
     integer              :: i_rec    ! vars index
     integer, allocatable :: param(:) ! grib parameter table
     character(len=64)    :: desc     ! Variable Description
  end type GrADS_VAR

  type GrADS_obs
     integer, pointer           :: id(:)  => null()  ! Obs Id
     character(len=10), pointer :: name(:)  => null()! Obs Name
     real, pointer              :: vec(:,:) => null() ! Obs Values
  end type GrADS_obs

  type GrADSfiles

     !
     ! Common variables
     !

     character(len=1024)      :: dset    ! the filename for input
     type(GrADS_Opt)          :: opt     ! Grads Options
     real                     :: undef   ! missing value flag
     integer                  :: dtype   ! type of file
     integer                  :: nvars   ! number of variables

     !
     ! if Grid/Model use these variables:
     !

     type(GrADS_DEF)          :: xdef    ! longitudinal dimension
     type(GrADS_DEF)          :: ydef    ! latitudinal dimension
     type(GrADS_DEF)          :: zdef    ! level dimension
     type(GrADS_TDEF)         :: tdef    ! time dimension
     type(GrADS_VAR), pointer :: vars(:) => null() ! Variables

     integer :: irec  ! current location
     integer :: ilen  ! record length
     integer :: nblock
     integer :: lu    ! logical unit if already opened

     real(kind=4), dimension(:, :), pointer :: dbuf => null()

     !
     ! if Observation use these variables:
     !

     integer         :: nobs ! number of observations
     type(GrADS_Obs) :: obs

  end type GrADSfiles

  type gbtab
     integer           :: parm
     character(len=4)  :: name
     character(len=50) :: desc
  end type gbtab

  ! Interface definitions

  interface GrADS_open
     module procedure open_
  end interface GrADS_open

  interface GrADS_close
     module procedure close_
  end interface GrADS_close

  interface GrADS_input
     module procedure &
          input3d_, &
          input2d_, &
          input1d_
  end interface GrADS_input

  interface GrADS_getvardims
     module procedure getdims_
  end interface GrADS_getvardims

  interface GrADS_getDim
     module procedure getdim_
  end interface GrADS_getDim

  interface GrADS_getVarsName
     module procedure getVarsName_
  end interface GrADS_getVarsName

  interface GrADS_getVarNLevs
     module procedure getVarNLevs_
  end interface GrADS_getVarNLevs

  interface GrADS_zdef
     module procedure zdef_
  end interface GrADS_zdef

  interface GrADS_Undef
     module procedure getUndefined_
  end interface

  ! !EXAMPLES: (to do)
  ! !BUGS: (to do)
  !
  !   Output interfaces should be added soon.  See aio_grads.f for more
  !   information.
  !
  ! !SEE ALSO: (to do)
  ! !SYSTEM ROUTINES: (to do)
  !
  ! !REVISION HISTORY:
  !     16Jul96 - J. Guo    - (to do)
  !     16May13 - Joao Gerd - Modified some aspects of code
  !_______________________________________________________________________

  !=======================================================================
  !
  ! !REVISION HISTORY
  !     grads.h - last change: Wed Jul 20 21:00:31 EDT 1994 (ams)
  !                - Original source from A. da Silva
  !    01Dec94 - Jing G. -    added zdef_gr for small values
  !    16Jul96 - Jing G. -    combined to form a module
  ! file: grads.f - last change: Wed Jul 20 21:00:31 EDT 1994 (ams)
  !
  !  Routines to read in GrADS like files.
  !......................................................................

  character(len=*), parameter :: myname = 'm_GrADSfiles'

contains

  !-----------------------------------------------------------------------------!
  !           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
  !-----------------------------------------------------------------------------!
  !
  ! !IROUTINE: GrADS_Default - set some initial values and grads default
  !                            configurations
  ! !DESCRIPTION: (to do)
  ! !INTERFACE:

  subroutine GrADS_Default(gs)
    implicit none

    type(GrADSfiles), intent(out) :: gs

    !
    ! Table with supported file formats
    !

    tname(tbin) = 'bin'
    tname(tgrib) = 'grib'
    tname(tnetcdf) = 'netcdf'
    tname(tstation) = 'station'

    !
    ! Setting default options
    !

    gs%opt%pascals = .false.
    gs%opt%yrev = .false.
    gs%opt%zrev = .false.
    gs%opt%template = .false.
    gs%opt%cal365day = .false.
    gs%opt%byteswapped = .false.
    gs%opt%cray_32bit_ieee = .false.
    gs%opt%endianess = .true. ! .true. -> little, .false. -> big
    gs%opt%iacc = .true. ! .true. -> direct, .false. -> sequential

    !
    ! Setting default parameters
    !

    gs%dset = ' '
    gs%undef = UDEF_
    gs%dtype = -1
    gs%nvars = -1
    gs%nobs = -1
    gs%irec = 0
    gs%ilen = -1
    gs%lu = -1
    gs%nblock = -1

    gs%xdef%num = -1
    gs%ydef%num = -1
    gs%zdef%num = -1
    gs%tdef%num = -1

  end subroutine GrADS_Default
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !-----------------------------------------------------------------------
  !
  ! !IROUTINE: open_ - open an input GrADS "control" file for input
  !
  ! !DESCRIPTION: (to do)
  ! !INTERFACE:

  subroutine open_(gs, ctl_file, stat)

    implicit none

    type(GrADSfiles), intent(out) :: gs
    character(len=*), intent(in)  :: ctl_file ! filename
    integer, optional, intent(out) :: stat     ! status

    ! !EXAMPLES: (to do)
    ! !BUGS: (to do)
    ! !SEE ALSO: (to do)
    ! !SYSTEM ROUTINES: (to do)
    !
    ! !REVISION HISTORY:
    !    21Jan00    - Jing Guo
    !        . Added "direct" access to open_()
    !     16Jul96 - J. Guo    - modified as a Fortran 90 module.
    !    01Dec94 - Jing G.    - added zdef_gr for small values.
    !                - Original source from A. da Silva
    !    11May20 - J.G de Mattos - dapt to get all information
    !                              from vars ctl block
    !_______________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::open_'

    ! Local variables:

    character(len=64) :: str
    integer i, j, k, l, lu

    integer :: tdim, nvars
    integer :: ierr
    character(len=4) :: ext

    if (present(stat)) stat = 0

    !--------------------------------------
    ! Verificando tipo de arquivo de entrada
    !--------------------------------------

    do i = len_trim(ctl_file), 1, -1
       if (ctl_file(i:i) .eq. '.') then
          j = i + 1
          exit
       endif
    end do

    ext = i90_lcase(ctl_file(j:len_trim(ctl_file)))

    select case (ext)

       !      case ('grb', 'grb1')
       !         !
       !         ! Reading grib version 1 file, skip default read
       !         !
       !         lu = gs%lu
       !         if (lu >= 0) close (lu)
       !
       !         gs%lu = luavail()
       !         gs%dtype = tgrib
       !         gs%dset = trim(ctl_file)
       !
       !         ! call OpenGrib1_(gs)
       !
       !         return

    case ('txt', 'dat')
       !
       ! Reading ascii file, skip default read
       !
       lu = gs%lu
       if (lu >= 0) close (lu)

       gs%lu = luavail()
       gs%dtype = tstation
       gs%dset = trim(ctl_file)

       call OpenAscii_(gs)

       return

    case ('ctl')
       !
       ! reading from ctl file
       !

       call OpenCtl_(ctl_file, gs)

    case default
       call i90_perr(trim(myname_), &
            ': Wrong type file: "'//trim(ctl_file)//'" error, ierr =', 99)
       if (.not. present(stat)) call i90_die(myname_)
       stat = 99
       return

    end select

  end subroutine open_

  subroutine openCtl_(ctl, gs, stat)
    character(len=*), intent(in) :: ctl
    type(GrADSfiles), intent(inout) :: gs
    integer, optional, intent(out) :: stat

    character(len=*), parameter :: myname_ = myname//':: openCtl_'

    logical :: formatdefined
    integer :: ierr, val, ntokens, nvars
    integer :: i, j, k
    integer :: lu, ios
    character(len=64) :: str, param
    character(len=64), allocatable :: tokens(:)

    !
    ! parse optional variables
    !

    if(present(stat)) stat = 0

    !----------------------------------------
    ! Use m_inpak90 read the table file
    !----------------------------------------
    call i90_loadf(ctl, ierr)
    if (ierr /= 0) then

       call i90_perr(trim(myname_), ': i90_loadf('//trim(ctl)//')', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return

    endif

    !
    ! Initialize default values
    !

    call GrADS_Default(gs)

    !----------------------------------------
    !  Mandatory GrADS settings:
    !
    !    dset xdef ydef zdef tdef vars
    !----------------------------------------
    ! DSET

    call i90_label('DSET', ierr)
    if (ierr == 0) call i90_gtoken(gs%dset, ierr)
    if (ierr /= 0) then
       call i90_perr(trim(myname_), ': DSET error with "'//trim(ctl)//'"', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif
    if (gs%dset(1:1) == '^') then
       gs%dset = gs%dset(2:)
       i = index(ctl, '/', back=.true.)
       if (i > 0) gs%dset = ctl(1:i)//gs%dset
    endif

    !----------------------------------------
    !DTYPE

    gs%dtype = 1 ! Binary file is default

    call i90_label('DTYPE', ierr)
    if (ierr == 0) then
       call i90_gtoken(str, ierr)
       ! tname are type data name listed above
       !      tname(tbin)     = 'bin'
       !      tname(tgrib)    = 'grib'
       !      tname(tnetcdf)  = 'netcdf'
       !      tname(tstation) = 'station'

       gs%dtype = lindex_(size(tname), tname, str)

       if (gs%dtype .eq. 0) then

          call i90_perr(trim(myname_), ': unsupported data type, "'//trim(str)//'"', -9)
          if (.not. present(stat)) call i90_die(myname_)
          stat = -9
          return

       endif
    else
#ifdef DEBUG
       call i90_perr(trim(myname_), ': setting default data type, "Binary ieee"')
#endif
    endif

    !----------------------------------------
    ! XDEF

    gs%xdef%num = -1

    call GetDef_('XDEF', gs%xdef%num, gs%xdef%type, gs%xdef%vec, ierr)

    if (ierr /= 0) then
       call i90_perr(trim(myname), ': XDEF entry error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif

    !----------------------------------------
    ! YDEF

    gs%ydef%num = -1

    call GetDef_('YDEF', gs%ydef%num, gs%ydef%type, gs%ydef%vec, ierr)

    if (ierr /= 0) then
       call i90_perr(trim(myname), ': YDEF entry error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif

    !----------------------------------------
    ! ZDEF

    gs%zdef%num = -1

    call GetDef_('ZDEF', gs%zdef%num, gs%zdef%type, gs%zdef%vec, ierr)

    if (ierr /= 0) then
       call i90_perr(trim(myname), ': ZDEF entry error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif
    !----------------------------------------
    ! TDEF
    gs%tdef%num = -1

    call i90_label('TDEF', ierr)
    if (ierr == 0) gs%tdef%num = i90_gint(ierr)
    if (ierr /= 0) then
       call i90_perr(trim(myname), ': TDEF entry error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif
    !tdim = gs%tdef%num

    !-----------------------------------------
    ! Optional Settings
    !
    formatdefined = .false.

    call i90_label('OPTIONS', ierr)
    if (ierr == 0) then
       do

          call i90_gtoken(str, ierr)
          if (ierr .ne. 0) exit

          str = i90_lcase(str)
          select case (trim(str))
             !case ('pascals')
             !   gs%opt%pascals = .true.
          case ('yrev')
             gs%opt%yrev = .true.
          case ('zrev')
             gs%opt%zrev = .true.
             !case ('template')
             !   gs%opt%template = .true.
          case ('sequential')
             gs%opt%iacc = .false.
             !case ('cal365day')
             !   gs%opt%cal365day = .true.
          case ('byteswapped')
             gs%opt%byteswapped = .true.
          case ('big_endian')
             gs%opt%endianess = .false.
          case default

             call i90_perr(trim(myname), ': unsupported option, "'//trim(str)//'"', ierr)
             if (.not. present(stat)) call i90_die(myname_)
             stat = -3
             return

          end select

       enddo
    endif

    !----------------------------------------
    ! UNDEF

    gs%undef = UDEF_

    call i90_label('UNDEF', ierr)
    if (ierr == 0) gs%undef = i90_gfloat(ierr)
    if (ierr /= 0) then
       call i90_perr(trim(myname), ': UNDEF entry error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = -3
       return
    endif

    !----------------------------------------
    ! VARS -- ENDVARS

    gs%nvars = -1

    call i90_label('VARS', ierr)
    if (ierr == 0) gs%nvars = i90_gint(ierr)
    if (ierr /= 0) then
       call i90_perr(trim(myname), ': VARS entry error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif

    nvars = gs%nvars

    allocate (gs%vars(nvars), stat=ierr)

    if (ierr /= 0) then
       call i90_perr(trim(myname_), ': allocate(VARS) error', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
    endif

    !     Get variable names and labels
    !     -----------------------------

    do i = 1, nvars

       call i90_gline(ierr)
       if (ierr /= 0) then
          call i90_perr(trim(myname_), ': error to get var list', ierr)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ierr
          return
       endif

       !var index
       gs%vars(i)%id = i

       !get var name
       call i90_gtoken(str, ierr)
       gs%vars(i)%name = trim(str)

       ! get var nlevels
       k = i90_gint(ierr)
       gs%vars(i)%n_rec = max(k, 1)

       !--------------------------------------------------!
       !get var info, if is a grid type, we should get
       !at least one parameter

       call i90_gtoken(str, ierr)
       param = trim(str)
       j = index(str, ',')
       if (j > 0) then
          do while (j > 0)
             call i90_gtoken(str, ierr)
             j = index(str, ',')
             if (j <= 0) then
                ! determine if is a number (should be a integer)
                Read (str, '(I10)', iostat=ios) val
                if (ios .eq. 0) then
                   param = trim(param)//trim(str)
                endif
             else
                param = trim(param)//trim(str)
             endif
          enddo
       endif

       !split parameters

       call split(param, ntokens, tokens, ',')
       !if a grib file will have at least 3 extra parameters
       
       allocate (gs%vars(i)%param(ntokens))
       do j = 1, ntokens
          read (tokens(j), '(I4)') gs%vars(i)%param(j)
       enddo

       !--------------------------------------------------!
       !Get description var
       ierr = 0
       gs%vars(i)%desc = ''
       do while (ierr == 0)
          call i90_gtoken(str, ierr)
          gs%vars(i)%desc = trim(gs%vars(i)%desc)//' '//trim(str)
       enddo

    enddo

    if (gs%dtype .eq. tbin) then
       gs%vars(1)%i_rec = 1
       do k = 2, nvars
          gs%vars(k)%i_rec = gs%vars(k - 1)%i_rec + max(gs%vars(k - 1)%n_rec, 1)
       end do
       gs%nblock = gs%vars(nvars)%i_rec + max(gs%vars(nvars)%n_rec, 1) - 1
    endif

    !----------------------------------------
    call i90_release(ierr)
    if (ierr /= 0) then
       call i90_perr(myname_, 'i90_release()', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif
    !----------------------------------------
    !
    lu = gs%lu
    if (lu >= 0) close (lu)
    gs%lu = -1

    !--------------------------------------------------------
    ! allocate the input buffer

    !      allocate (gs%dbuf(gs%xdef%num, gs%ydef%num), stat=ierr)
    !      if (ierr /= 0) then
    !         write (stderr, '(2a,i5)') myname_, &
    !            ': allocate(gs%dbuf) error, stat =', ierr
    !         if (.not. present(stat)) call die(myname_)
    !         stat = ierr
    !         return
    !      endif
    gs%lu = luavail()
    gs%irec = 1
    call opendset_(gs%lu, gs%dset, gs%opt%iacc, gs%dtype, gs%dbuf, gs%ilen, ierr)
    if (ierr /= 0) then
       call i90_perr(myname_, 'opendset_()', ierr)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ierr
       return
    endif

  end subroutine OpenCtl_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: opendset_ - open a DSET file
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine opendset_(lu, name, iacc, dtype, dbuf, ilen, ierr)
    use m_ioutil, only: opnieee
    implicit none

    integer, intent(in)  :: lu
    character(len=*), intent(in)  :: name
    logical, intent(in)  :: iacc
    integer, intent(in)  :: dtype
    real*4, dimension(:, :), intent(in) :: dbuf

    integer, intent(out) :: ilen
    integer, intent(out) :: ierr

    ! !REVISION HISTORY:
    !     21Jan00    - Jing Guo <guo@dao.gsfc.nasa.gov>
    !        - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::opendset_'
    character(len=16) :: clen

!    ilen = 0
!    inquire (iolength=ilen) dbuf

    select case (dtype)
    case (tbin)
       if (iacc) then

          call opnieee(lu, trim(name), 'old', ierr)!, recl=ilen)
          if (ierr .ne. 0) then
             clen = '****************'
             write (clen, '(i16)', iostat=ierr) ilen
             clen = adjustl(clen)
             call i90_perr(myname_, 'opnieee("'// &
                  trim(name)//'",recl='// &
                  trim(clen)//')', ierr)
             return
          endif

       else

          ilen = 0        ! reset %ilen to avoid confusion
          call opnieee(lu, trim(name), 'old', ierr)
          if (ierr .ne. 0) then
             call i90_perr(myname_, 'opnieee("'// &
                  trim(name)//'")', ierr)
             return
          endif

       endif
    case (tgrib)

       call baopenr(lu, trim(name), ierr)
       if (ierr .ne. 0) then
          call i90_perr(myname_, 'baopenr("'// &
               trim(name)//'")', ierr)
          return
       endif

    case default
       call i90_die(myname_, 'unknown or not supported type yeat: ' &
            //trim(tname(dtype)), -9)

    end select

    !--------------------------------------------------------
  end subroutine opendset_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !-----------------------------------------------------------------------
  !
  ! !IROUTINE: close_ - close a GrADSfiles variable
  !
  ! !DESCRIPTION: (to do)
  !
  ! !INTERFACE:

  subroutine close_(gs, stat)
    use m_ioutil, only: clsieee
    implicit none

    type(GrADSfiles), intent(inout) :: gs
    integer, optional, intent(out)   :: stat

    ! !EXAMPLES: (to do)
    ! !BUGS: (to do)
    ! !SEE ALSO: (to do)
    ! !SYSTEM ROUTINES: (to do)
    !
    ! !REVISION HISTORY:
    !     18Mar97 - Jing Guo <guo@eramus> - initial prototyping and coding
    !_______________________________________________________________________
    character(len=*), parameter :: myname_ = myname//'::close_'

    integer :: lu, i
    integer :: ier, ierr

    if (present(stat)) stat = 0

    lu = gs%lu

    select case (gs%dtype)

    case (tbin, tstation)

       call clsieee(lu, ier)
       if (ier /= 0) then
          call i90_perr(myname_, 'clsieee()', ier)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ier
          return
       endif

    case (tgrib)

       call baclose(lu, ier)
       if (ier /= 0) then
          call i90_perr(myname_, 'baclose()', ier)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ier
          return
       endif

    end select

    if (gs%dtype .eq. tstation) then
       ierr = 0
       deallocate (gs%obs%vec, stat=ier); ierr = ierr + ier
       deallocate (gs%obs%id, stat=ierr); ierr = ierr + ier
       deallocate (gs%obs%name, stat=ierr); ierr = ierr + ier

    else
       ierr = 0
       deallocate (gs%xdef%vec, stat=ierr); ierr = ierr + ier
       deallocate (gs%ydef%vec, stat=ierr); ierr = ierr + ier
       deallocate (gs%zdef%vec, stat=ierr); ierr = ierr + ier
       do i = 1, gs%nvars
          deallocate (gs%vars(i)%param, stat=ierr); ierr = ierr + ier
       enddo
       deallocate (gs%vars, stat=ierr); ierr = ierr + ier
       !deallocate (gs%dbuf, stat = ierr); ierr = ierr + ier
    endif

    if (ier /= 0) then
       call i90_perr(myname_, 'deallocate()', ier)
       if (.not. present(stat)) call i90_die(myname_)
       stat = ier
       return
    endif

    call GrADS_Default(gs)

  end subroutine close_

  subroutine input1d__(gs, vnam, vfld, stat)

    type(GrADSfiles), intent(inout) :: gs      ! the input
    character(len=*), intent(in) :: vnam    ! what variable?
    real, dimension(:), intent(out) :: vfld    ! a 1-d station field
    integer, optional, intent(out) :: stat

    character(len=*), parameter :: myname_ = myname//'::input1d_'
    integer :: ivar

    if (present(stat)) stat = 0
    !
    ! Sanity checks
    !

    ! Check the file status

    if (gs%dset == ' ' .or. &
         gs%lu < 0 .or. &
         gs%nvars <= 0 .or. &
         gs%nobs <= 0 &
         ) then
       call i90_perr(trim(myname_), ': uninitialized type(GrADS)?', 1)
       if (.not. present(stat)) call i90_die(myname_)
       stat = 1
       return
    endif

    ! Check the buffer dimensions

    if (size(vfld, 1) .ne. gs%nobs) then

       write (stderr, '(2a,$)') myname_, ': invalid arguments'
       write (stderr, '(a,2i6,a,$)') ', shape(vfld) = (', shape(vfld), ')'
       write (stderr, '(a,2i6,a,$)') ', gs%nobs = (', gs%nobs, ')'
       write (stderr, *)
       if (.not. present(stat)) call i90_die(myname_)
       stat = 2
       return
    endif

    ! Check/index the requested variable

    ivar = lindex_(gs%nvars, gs%obs%name(:), vnam)
    if (ivar <= 0) then
       write (stderr, '(4a)') myname_, ': unknown variable "', trim(vnam), '"'
       if (.not. present(stat)) call i90_die(myname_)
       stat = 3
       return
    endif

    ! Check the requested time
    ! NOT YET IMPLEMENTED

    !
    ! Get Variable
    !

    vfld(1:gs%nobs) = gs%obs%vec(1:gs%nobs, ivar)

    return

  end subroutine input1d__

subroutine input1d_(gs, vnam, llev, klev, vfld, stat)
    type(GrADSfiles), intent(inout) :: gs    ! the input
    character(len=*), intent(in)    :: vnam    ! what variable?
    integer, intent(in)    :: llev    ! what time?
    integer, intent(in)    :: klev    ! which level?
    real, dimension(:), intent(out) :: vfld    ! a 1-d gridded field
    integer, optional, intent(out)   :: stat

    character(len=*), parameter :: myname_ = myname//'::input1d_'
    real, allocatable :: field(:,:)
    integer :: npts, gs_npts
    integer :: i, j, k

    gs_npts = gs%xdef%num*gs%ydef%num
    npts    = size(vfld)

    if (npts .ne. gs_npts)then
       write (stderr, '(2a,$)') trim(myname_), ': invalid arguments'
       write (stderr, '(a,2i6,a,$)') ', shape(vfld) = (', shape(vfld), ')'
       write (stderr, '(a,2i6,a,$)') ', gs%[x*y]def = (', gs%xdef%num*gs%ydef%num, ')'
       write (stderr, *)
       if (.not. present(stat)) call i90_die(myname_)
       stat = 2
       return
    endif

    allocate(field(gs%xdef%num,gs%ydef%num))
    call input2d_(gs, vnam, llev, klev, Field, stat)
    k = 1
    do j=1,gs%ydef%num
       do i=1, gs%xdef%num
          vfld(k) = Field(i,j)
          k=k+1
       enddo
    enddo


end subroutine input1d_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: input2d_ - input a 2-d field
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine input2d_(gs, vnam, llev, klev, vfld, stat)

    type(GrADSfiles), intent(inout) :: gs    ! the input
    character(len=*), intent(in)    :: vnam    ! what variable?
    integer, intent(in)    :: llev    ! what time?
    integer, intent(in)    :: klev    ! which level?
    real, dimension(:, :), intent(out) :: vfld    ! a 2-d gridded field
    integer, optional, intent(out)   :: stat

    ! !REVISION HISTORY:
    !     18Mar97 - Jing Guo <guo@eramus> - initial prototyping and coding
    !     08Dec98 - Jing Guo <guo@thunder> - modified from read_ with
    !            dbuf for portable input
    !EOP ___________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::input2d_'
    integer :: i, j, k, ierr, nrec, nskp, ivar, lu
    integer :: iklev
    logical :: no_buffer

    !
    ! use to Grib fields
    !

    integer :: npts
    integer :: jpds(200), jgds(200)
    integer :: kpds(200), kgds(200)
    real    :: kf, kh, kg, kfa
    logical*1, allocatable :: lb(:)
    real, allocatable      :: f(:)

    if (present(stat)) stat = 0

    ! Sanity checks

    ! Check the file status

    if (gs%dset == ' ' .or. &
         gs%lu < 0 .or. &
         gs%nvars <= 0 .or. &
         gs%xdef%num <= 0 .or. &
         gs%ydef%num <= 0 .or. &
         gs%zdef%num <= 0 .or. &
         gs%tdef%num <= 0 &
         ) then

       write (stderr, '(2a)') myname_, ': uninitialized type(GrADSfiles)?'
       if (.not. present(stat)) call i90_die(myname_)
       stat = 1
       return
    endif

    ! Check the buffer dimensions

    if (size(vfld, 1) /= gs%xdef%num .or. size(vfld, 2) /= gs%ydef%num) then

       write (stderr, '(2a,$)') myname_, ': invalid arguments'
       write (stderr, '(a,2i6,a,$)') ', shape(vfld) = (', shape(vfld), ')'
       write (stderr, '(a,2i6,a,$)') ', gs%[xy]def = (', gs%xdef%num, gs%ydef%num, ')'
       write (stderr, *)
       if (.not. present(stat)) call i90_die(myname_)
       stat = 2
       return
    endif

    ! Check/index the requested variable

    ivar = lindex_(gs%nvars, gs%vars(:)%name, vnam)
    if (ivar <= 0) then
       write (stderr, '(4a)') myname_, ': unknown variable "', trim(vnam), '"'
       if (.not. present(stat)) call i90_die(myname_)
       stat = 3
       return
    endif

    ! Check the requested time

    if (llev < 1 .or. llev > gs%tdef%num) then
       write (stderr, '(2a,$)') myname_, ': invalid time request'
       write (stderr, '(2(a,i3))') ', llev =', llev, ', gs%ldim =', gs%tdef%num
       if (.not. present(stat)) call i90_die(myname_)
       stat = 4
       return
    endif

    ! Check the requested level

    if (klev < 0 .or. &
         klev > gs%vars(ivar)%n_rec .or. &
         klev > gs%zdef%num &
         ) then
       write (stderr, '(2a,$)') myname_, ': invalid level request'
       write (stderr, '(a,i3,3a,i3,a,i3)') ', klev =', klev, &
            ', gs%n_rec("', trim(vnam), '") =', gs%vars(ivar)%n_rec, &
            ', gs%kdim =', gs%zdef%num
       if (.not. present(stat)) call i90_die(myname_)
       stat = 5
       return
    endif

    !
    ! Get Variable
    !

    select case (gs%dtype)

    case (tbin)
       !--------------------------------------------------------
       ! The open statement used to access binary file in this
       ! module uses standard stream I/O,  a feature
       ! that was intruduced in fortran 2003, so we need obtain
       ! the start position of each field recorded.
       ! For single precision files we need known the size of
       ! each field plus 4-bit and for double precision files
       ! we need known the size of field plus 8-bits. Because this
       ! we have a number 4 at nrec calculus.
       !--------------------------------------------------------
       ! Compute the record number
       if(gs%opt%iacc)then
          !direct access file
          nrec = (gs%vars(ivar)%i_rec + (klev-2))*(gs%xdef%num*gs%ydef%num*4) + 1
       else
          !sequential access file
          nrec = (gs%vars(ivar)%i_rec + (klev-2))*(gs%xdef%num*gs%ydef%num*4+2*4) + 1+4
       endif

!       nrec = (llev - 1)*gs%nblock + gs%vars(ivar)%i_rec + max(klev, 1) - 1

       !--------------------------------------------------------
       ! Read the nrec-th record.  The current position may be
       ! taken into account if the file is sequentially accessed.
       ! See read_() for details.
       read(gs%lu, POS=nrec, iostat=ierr)vfld

!       call read_(gs%lu, gs%opt%iacc, nrec, gs%irec, vfld, gs%dbuf, ierr)

       if (ierr /= 0) then
          call i90_perr(trim(myname_), ': read() ', ierr)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ierr
          return
       endif


    case (tgrib)

       iklev = klev
       npts  = gs%xdef%num*gs%ydef%num
       jpds  = -1
       jgds  = -1
       kpds  = -1
       kgds  = -1

       !         iklev    = min0(gs%vars(ivar)%n_rec,klev)
       !         if (iklev.gt.0)  jpds(7) = gs%zdef%vec(klev)

       !----------------------------------------------!
       ! jpds(5) is the indicator of parameter
       !         each variable has a number!

       jpds(5) = gs%vars(ivar)%param(1)
       jpds(6) = gs%vars(ivar)%param(2)

!       ! Get info about jpds(5) variable.
       call getgbh(gs%lu, 0, 0, jpds, jgds, kg, kf, kh, &
            kpds, kgds, ierr)
       if (ierr .ne. 0)then
         call i90_perr(myname_,'getgbh()',ierr)
         if(.not.present(stat)) call i90_die(myname_)
         stat=ierr
         return          
       endif
!
!       ! KPDS is the unpacked PDS Parameter for jpds(5)
!       ! Here we can do a check for requested level
!       !
!       ! kpds(6) is the type of level see GRIB table 3
!       !
       if (kpds(6) .ne. 100) then
          jpds(7) = kpds(7)
       else
          jpds(7) = gs%zdef%vec(klev)
       endif
       jpds(6) = kpds(6)

       allocate (lb(npts), f(npts), stat=ierr)

       if (ierr /= 0) then
          call i90_perr(myname_, 'allocate()', ierr)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ierr
          return
       endif

       lb = .true.

       call getgb(gs%lu, 0, npts, 0, jpds, jgds, kf, kh, &
            kpds, kgds, lb, f, ierr)

       if (ierr /= 0) then
          
          call i90_perr(trim(myname_),': getgb()', ierr)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ierr
          return
       endif

       k = 0
       do j = 1, gs%ydef%num
          do i = 1, gs%xdef%num
             vfld(i, j) = f(i + k)
          enddo
          k = k + gs%xdef%num
       enddo

    case default

       ierr = -9
       call i90_perr(myname_, &
            ': unsupported data type, "'// &
            trim(tname(gs%dtype))//'"', ierr)
       return

    end select

    if (gs%opt%yrev) then
       vfld = vfld(:, gs%ydef%num:1:-1)
    end if

    !--------------------------------------------------------
  end subroutine input2d_
  !=======================================================================

  function lindex_(nlst, lsts, entr)
    integer, intent(in) :: nlst
    character(len=*), dimension(:), intent(in) :: lsts
    character(len=*), intent(in) :: entr

    integer :: lindex_    ! the result

    !--------------------------------------------------------
    integer :: i

    !--------------------------------------------------------
    lindex_ = 0
    do i = 1, nlst
       if (i90_lcase(entr) == i90_lcase(lsts(i))) then
          lindex_ = i
          return
       endif
    end do
  end function lindex_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: read_ - read the n-th record
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine read__(lu, iacc, nrec, irec, vfld, dbuf, ierr)
    integer, intent(in)  :: lu      ! the input unit
    logical, intent(in)  :: iacc    ! access
    integer, intent(in)  :: nrec    ! which to read
    integer, intent(in)  :: irec    ! where it is now
    real, dimension(:, :), intent(out) :: vfld
    real*4, dimension(:, :), intent(out) :: dbuf
    integer, intent(out) :: ierr

    ! !REVISION HISTORY:
    !     22Jan99 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::read_'
    logical :: no_buffer
    integer :: nskp
    integer :: i
    integer :: nx, ny

    no_buffer = kind(dbuf) == kind(vfld) .and. &
         size(dbuf, 1) == size(vfld, 1) .and. &
         size(dbuf, 2) == size(vfld, 2)

    ierr = -1

    if (iacc) then
       print*,'! Direct read()', ierr

       if (ierr == 0) then
          if (no_buffer) then
             read (lu, rec=nrec, iostat=ierr) vfld
          else
             read (lu, rec=nrec, iostat=ierr) dbuf
             if (ierr == 0) then
                nx = min(size(vfld, 1), size(dbuf, 1))
                ny = min(size(vfld, 2), size(dbuf, 2))
                vfld(1:nx, 1:ny) = dbuf(1:nx, 1:ny)
             endif
          endif
       endif

    else

       print*,'! Sequential skip'

       ierr = 0
       if (nrec < irec) then
          rewind (lu)   ! can we trust backspace()?
          nskp = nrec - 1
       else
          nskp = nrec - irec
       endif
       do i = 1, nskp
          if (ierr == 0) read (lu, iostat=ierr)
       end do

       ! Sequential read()
       if (ierr == 0) then
          if (no_buffer) then
             read (lu, iostat=ierr) vfld
          else
             read (lu, iostat=ierr) dbuf
             if (ierr == 0) then
                nx = min(size(vfld, 1), size(dbuf, 1))
                ny = min(size(vfld, 2), size(dbuf, 2))
                vfld(1:nx, 1:ny) = dbuf(1:nx, 1:ny)
             endif
          endif
       endif
    endif

  end subroutine read__
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: getdims_ get the dimensions of a given variable
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine getdims_(gs, vnam, nlon, nlat, nlev, udef, stat)
    character(len=*), intent(in) :: vnam
    type(GrADSfiles), intent(in) :: gs
    integer, intent(out) :: nlon
    integer, intent(out) :: nlat
    integer, intent(out) :: nlev
    real, optional, intent(out) :: udef
    integer, optional, intent(out) :: stat

    ! !REVISION HISTORY:
    !     08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::getdims_'
    integer :: ivar

    if (present(stat)) stat = 0

    nlon = gs%xdef%num
    nlat = gs%ydef%num

    ivar = lindex_(gs%nvars, gs%vars(:)%name, vnam)
    if (ivar == 0) then
       call i90_perr(trim(myname_), &
            ': unknown variable name, "'//vnam//'"', -1)
       if (.not. present(stat)) call i90_die(myname_)
       stat = -1
       return
    endif

    nlev = gs%vars(ivar)%n_rec
    if (present(udef)) udef = gs%undef

    ! A post-condition?

    if (nlev < 0 .or. nlev > gs%zdef%num) then
       write (stderr, '(4a,i4)') myname_, &
            ': improper number of records, gs%n_rec("', vnam, '") =', nlev
       if (.not. present(stat)) call i90_die(myname_)
       stat = -1
       return
    endif

  end subroutine getdims_

  function getdim_(gs, dimName) result(dimVal)
     type(GrADSfiles), intent(in) :: gs
     character(len=*), intent(in) :: dimName
     integer                      :: dimVal

     character(len=*), parameter :: myname_ = myname//'::getdim_'

     select case(i90_lcase(trim(dimName)))
     case ('xdef')
        dimVal = gs%xdef%num
     case ('ydef')
        dimVal = gs%ydef%num
     case('zdef')
        dimVal = gs%zdef%num
     case('tdef')
        dimVal = gs%zdef%num
     case('nvars')
        dimVal = gs%nVars
     end select
  end function getdim_

  function getVarNLevs_(gs, varName) result(nlevs)
     type(GrADSfiles), intent(in) :: gs
     character(len=*), intent(in) :: varName
     integer                      :: nLevs

     integer                      :: ivar

     character(len=*), parameter :: myname_ = myname//'::getVarNLevs_'

     ivar = lindex_(gs%nvars, gs%vars(:)%name, varName)
     if (ivar == 0) then
        call i90_perr(trim(myname_), &
             ': unknown variable name, "'//varName//'"', -1)
        nlevs = -1
        return
     endif

     nlevs = gs%vars(ivar)%n_rec
    

  end function

  subroutine getVarsName_(gs,varsName)
     type(GrADSfiles), intent(in) :: gs
     character(len=*), allocatable, intent(out) :: varsName(:)

     character(len=*), parameter :: myname_ = myname//'::getVarsName_'
     integer :: i

     allocate(varsName(gs%nVars))
     do i = 1, gs%nVars
        varsName(i) = trim(gs%vars(i)%name)
     enddo
  end subroutine

  function getUndefined_(gs)result(undef)
     type(GrADSfiles), intent(in) :: gs
     real                         :: undef
     character(len=*), parameter :: myname_ = myname//'::getUndefined_'

     undef = gs%undef
     
  end function
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: zdef_ - return the leading levels of ZDEF
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  function zdef_(gs, nlev)
    implicit none
    type(GrADSfiles), intent(in) :: gs
    integer, intent(in) :: nlev
    real, dimension(nlev) :: zdef_

    ! !REVISION HISTORY:
    !     08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::zdef_'

    if (nlev > size(gs%zdef%vec)) then
       write (stderr, '(2a,i4)') myname_, &
            ': improper number of "ZDEF" levels, nlev =', nlev
       call i90_die(myname_)
    endif

    zdef_(1:nlev) = gs%zdef%vec(1:nlev)

  end function zdef_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: input3d_ - input a 3-d field
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine input3d_(gs, vnam, llev, vfld, stat)
    type(GrADSfiles), intent(inout) :: gs
    character(len=*), intent(in) :: vnam
    integer, intent(in) :: llev
    real, intent(out):: vfld(:, :, :)
    integer, optional, intent(out):: stat

    ! !REVISION HISTORY:
    !     08Dec98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::input3d_'

    integer :: nlon, nlat, nlev
    integer :: k, ier

    if (present(stat)) stat = 0

    call getdims_(gs, vnam, nlon, nlat, nlev)

    do k = 1, nlev
       call input2d_(gs, vnam, llev, k, vfld(:, :, k), stat=ier)

       if (ier /= 0) then
          call i90_perr(trim(myname_), ': input2d_()', ier)
          if (.not. present(stat)) call i90_die(myname_)
          stat = ier
          return
       endif
    end do

  end subroutine input3d_
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !                                                                      !
  !-----------------------------------------------------------------------
  !
  ! !IROUTINE: GetLevels_ - Get Levels of ctl
  !
  ! !DESCRIPTION: (to do)
  !
  ! !INTERFACE:

  subroutine GetDef_(def, dim, type, levs, ierr)

    character(len=*), intent(in)   :: def
    integer, intent(out)           :: dim
    character(len=*), intent(out)  :: type
    real, dimension(:), pointer, intent(inout) :: levs
    integer, intent(out)           :: ierr

    ! !EXAMPLES: (to do)
    ! !BUGS: (to do)
    ! !SEE ALSO: (to do)
    ! !SYSTEM ROUTINES: (to do)
    !
    ! !REVISION HISTORY:
    !     16May2013  Joao Gerd <joao.gerd@cptec.inpe.br> - initial prototyping and coding
    !_______________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::GetLevels_'

    character(len=10) :: str
    integer :: i
    real    :: inip
    real    :: delta


    call i90_label(trim(def), ierr)
    if (ierr == 0) dim = i90_gint(ierr)
    if (ierr /= 0) then
       call i90_perr(trim(myname_), ': '//TRIM(def), ierr)
       return
    endif

    if (dim > 0) then
       call i90_gtoken(str, ierr)
       if (ierr /= 0) then
          call i90_perr(trim(myname_),': '//trim(def)//' type', ierr)
          return
       endif

       allocate (levs(1:dim), stat=ierr)
       if (ierr /= 0) then
          call i90_perr(trim(myname_),': allocate(levs)', ierr)
          return
       endif

       type = i90_lcase(str)
       select case (type)
       case ('levels')

          i = 1
          do while (i <= dim)
             levs(i) = i90_gfloat(ierr)
             if (ierr .ne. 0) then
                call i90_gline(ierr)
             else
                i = i + 1
             endif
          end do

          if (ierr /= 0) then
             call i90_perr(trim(myname_), ': '//trim(def)//' level', ierr)
             return
          endif

       case ('linear')
          inip = i90_gfloat(ierr)
          if (ierr /= 0) then
             call i90_perr(trim(myname_), ': '//trim(def)//' level', ierr)
             return
          endif

          delta = i90_gfloat(ierr)
          if (ierr /= 0) then
             call i90_perr(trim(myname_),': '//trim(def)//' delta lev', ierr)
             return
          endif

          do i = 1, dim
             levs(i) = inip + delta*(i - 1)
          enddo

       case default
          call i90_perr(trim(myname_),': unknown '//trim(def)//' type, "'//trim(str)//'"',99)
          return
       end select

    endif
  end subroutine GetDef_
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !                                                                      !
  !-----------------------------------------------------------------------
  !
  ! !IROUTINE: OpenAscii - Read ASCII file with station information
  !
  ! !DESCRIPTION: (to do)
  !
  ! !INTERFACE:

  subroutine OpenAscii_(gs, iret)


    type(GrADSfiles), intent(inout) :: gs
    integer, optional               :: iret

    ! !EXAMPLES: (to do)
    ! !BUGS: (to do)
    ! !SEE ALSO: (to do)
    ! !SYSTEM ROUTINES: (to do)
    !
    ! !REVISION HISTORY:
    !     27Jun2013  Joao Gerd <joao.gerd@cptec.inpe.br> - initial prototyping and coding
    !_______________________________________________________________________

    character(len=*), parameter :: myname_ = myname//'::OpenAscii_'

    integer, parameter        :: len_header = 400
    character(len=len_header) :: header
    character(len=10), allocatable :: bufr1(:)
    integer :: i, j
    integer :: ntoken
    character(len=10) :: form

    open (unit=gs%lu, &
         file=gs%dset, &
         form='formatted' &
         )

    !
    ! Identify variables: one per colunm
    !

    write (form, '(A2,I0,A1)') '(A', len_header, ')'
    read (gs%lu, form) header
    header = trim(header)

    allocate (bufr1(len_header))
    call GetTokens_(header, bufr1, gs%nvars)

    !
    ! Count number of observation
    !

    gs%nobs = 0
    do
       read (gs%lu, *, end=99)
       gs%nobs = gs%nobs + 1
    enddo

    iret = -5
    call i90_perr(trim(myname_), ': error to read ascii file, ios =', iret)
    return

99  continue

    !
    ! Next step: Read observations
    !

    rewind (gs%lu) ! rewind ascii file
    read (gs%lu, *) ! skip header

    allocate (gs%obs%name(gs%nvars))
    allocate (gs%obs%id(gs%nvars))

    do i = 1, gs%nvars
       gs%obs%name(i) = bufr1(i)
       gs%obs%id(i) = i
    enddo

    DeAllocate (bufr1)

    Allocate (gs%obs%vec(gs%nobs, gs%nvars))

    do i = 1, gs%nobs
       read (gs%lu, *) (gs%obs%vec(i, j), j=1, gs%nvars)
    enddo

  end subroutine OpenAscii_
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE: split - parse string into an array using specified delimiters
  !
  ! !DESCRIPTION: parses a string using specified delimiter characters and
  !               store tokens into an allocatable array
  !
  !
  ! !INTERFACE:
  !

  subroutine split(str, ntokens, tokens, del)

    !
    ! !INPUT PARAMETERS:
    !
    character(len=*), intent(in) :: str
    character(len=*), optional, intent(in) :: del
    !
    ! !OUTPUT PARAMETERS:
    !
    integer, intent(out) :: ntokens
    character(len=*), allocatable, intent(out) :: tokens(:)

    ! !REVISION HISTORY:
    !
    !   13 May 2020 - J. G. de Mattos -  Initial code.
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC

    character, parameter :: BLK = achar(32)   ! blank (space)
    character(len=1)     :: delimiter
    integer              :: i, j
    integer              :: StrLen

    ! linked list to store temporary tokens
    type token
       character(len=10)    :: tk
       type(token), pointer :: next => null()
    endtype token
    type(token), pointer :: root => null()
    type(token), pointer :: current => null()

    ! setting up delimter
    delimiter = BLK
    if (present(del)) delimiter = del

    ! get string length
    StrLen = len_trim(str)

    ! at least we has one token
    ntokens = 1

    ! find tokens using delimiter
    allocate (root)
    current => root
    j = 1
    do i = 1, StrLen

       if (str(i:i) == trim(delimiter)) then
          ntokens = ntokens + 1
          current%tk = str(j:i - 1)
          allocate (current%next)
          current => current%next
          j = i + 1
       endif

    enddo
    !get last token
    current%tk = str(j:len_trim(str))

    !copy tokens to output array
    allocate (tokens(ntokens))
    current => root
    do i = 1, ntokens
       tokens(i) = trim(current%tk)
       current => current%next
    enddo

    !
    ! deallocate temporary token list
    !
    current => root%next
    do while (associated(current))
       deallocate (root)
       root => current
       current => root%next
    enddo

  end subroutine split
  !EOC
  !-----------------------------------------------------------------------------!

  subroutine GetTokens_(line, tokens, ntokens, del)

    character(len=*)           :: line
    integer                    :: ntokens
    character(len=*)           :: tokens(:)
    character(len=*), optional :: del

    !--------------------------------------------------------------------!
    !

    integer              :: length
    integer              :: i, j
    character(len=10)    :: delimiter
    character, parameter :: BLK = achar(32)   ! blank (space)

    length = len_trim(line)
    j = 1
    ntokens = 0
    tokens = BLK
    delimiter = BLK
    if (present(del)) delimiter = del

    do while (j .le. length)

       !
       ! loop over next blank
       !

       do while (line(j:j) .eq. trim(delimiter))
          j = j + 1
          if (j .gt. length) exit
       enddo

       !
       ! loop over next token
       !

       ntokens = ntokens + 1
       i = 1
       do while (line(j:j) .ne. trim(delimiter))
          tokens(ntokens) (i:i) = line(j:j)
          j = j + 1
          i = i + 1
          if (j .gt. length) exit
       enddo

    end do

  end subroutine GetTokens_
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !                                                                      !
  !-----------------------------------------------------------------------
  !
  ! !IROUTINE: OpenGrib1_ - Read Grib1 file with without use ctl file
  !
  ! !DESCRIPTION: (to do)
  !
  ! !INTERFACE:

  !subroutine OpenGrib1_( gs,  iret )
  !   use m_stdio,   only : stderr
  !   use m_ioutil,  only : luavail
  !   implicit none
  !
  !   type(GrADSfiles), intent(inout) :: gs
  !   integer, optional               :: iret
  !
  ! !EXAMPLES: (to do)
  ! !BUGS: (to do)
  ! !SEE ALSO: (to do)
  ! !SYSTEM ROUTINES: (to do)
  !
  ! !REVISION HISTORY:
  !     26Feb2014  Joao Gerd <joao.gerd@cptec.inpe.br> - initial prototyping and coding
  !_______________________________________________________________________

  !  character(len=*), parameter :: myname_=myname//'::OpenGrib1_'
  !
  !  integer, parameter        :: len_header = 400
  !  character(len=len_header) :: header
  !  character (len=10), allocatable :: bufr1(:)
  !  integer :: i, j
  !  integer :: lu
  !  integer :: nline
  !  integer :: ntoken
  !  character(len=10) :: form
  !
  !  integer :: jpds(200),jgds(200)
  !  integer :: kpds(200),kgds(200)
  !  real    :: kg,kf,k
  !   real    :: inip, delta
  !
  !
  !  integer                         :: nbf
  !  character(len=200)              :: GribTable
  !  character(len=256)              :: bf
  !  character(len=050), allocatable :: bf2(:)
  !  type(gbtab), allocatable        :: gbt(:)

  !
  ! Open Grib file and discovery fields
  !

  !  call baopenr(gs%lu,trim(gs%dset),ierr)
  !  if(ierr.ne.0) then
  !     call perr(myname_,'baopenr("'//    &
  !     trim(gs%dset)//'")',ierr    )
  !     return
  !  endif

  !  jpds = -1
  !  jgds = -1

  !  call getgbh(1,0,-1,jpds,jgds,kg,kf,k,kpds,kgds,ierr)

  !
  ! populate gs data type
  !
  !
  ! XDEF
  !

  !  gs%xdef%num = kgds(2)
  !  gs%xdef%type='LINEAR'

  !  inip  = kgds(5)
  !  delta = kgds(9)

  !  if(abs(inip).gt.90.0)then
  !     inip  = inip /1000.0
  !     delta = delta / 1000.0
  !  end if

  !  allocate(gs%xdef%vec(kgds(2)))
  !  do i = 1, kgds(2)
  !     gs%xdef%vec(i) = inip + delta*(i-1)
  !  enddo

  !
  ! YDEF
  !

  !  gs%ydef%num = kpds(3)
  !  select case(kgds(1))
  !    case(0)
  !       !
  ! Lat/Lon
  !

  !       gs%xdef%type='LINEAR'
  !       inip  = kgds(4)
  !       delta = kgds(10)
  !       if(abs(inip).gt.360.0)then
  !          inip  = inip / 1000.0
  !          delta = delta / 1000.0
  !       endif
  !       allocate(gs%ydef%vec(kgds(3)))
  !       do i = 1, kgds(3)
  !          gs%ydef%vec(i) = inip + delta*(i-1)
  !       enddo
  !    case(4)
  !
  ! Gaussian
  !

  !       gs%xdef%type='LEVELS'
  !    default
  !       print*,'default'
  !  end select

  !
  ! Reading Grib Table
  !
  !  write(GribTable,'(A,I3.3,A)')'gribtab.',kpds(1),'.tab'
  !  lu = luavail( )
  !  open( unit = lu,              &
  !        file = trim(GribTable), &
  !        action = 'read'         &
  !       )

  !
  ! count lines on grib table
  !
  !  nline = 0
  !  do
  !     read(lu,*,end=88,err=88)
  !     nline = nline + 1
  !  enddo
  !88 continue
  !  nline = nline - 1  ! remove header from count
  !  rewind(lu)
  !
  !  allocate(gbt(nline))

  !
  ! Read reader
  !

  !   read(lu,'(A)',end=99,err=99)bf

  !   do i=1,nline

  !      read(lu,'(A)',end=99,err=99)bf
  !      call GetTokens_(bf,bf2,nbf,":")

  !      read(bf2(1),*)x
  !      gtb(i)%parm = int(x)
  !      gtb(i)%name = trim(bf2(2))
  !      gtb(i)%desc = trim(bf2(3))

  !   enddo
  !   close (lu)

  !
  ! populate gs data type
  !

  !end subroutine

!  subroutine grbtb(gs)
!    integer :: jpds(200), jgds(200)
!    integer :: kpds(200), kgds(200)
!    real    :: kg, kf, k
!    real    :: inip, delta
!
!    integer                         :: nbf
!    character(len=200)              :: GribTable
!    character(len=256)              :: bf
!    character(len=050), allocatable :: bf2(:)
!    type(gbtab), allocatable        :: gbt(:)
!
!    call getgbh(gs%lu, 0, -1, jpds, jgds, kg, kf, k, kpds, kgds, ierr)
!
!    !
!    ! Reading Grib Table
!    !
!
!    write (GribTable, '(A,I3.3,A)') 'gribtab.', kpds(1), '.tab'
!
!    lu = luavail()
!    open (unit=lu, &
!         file=trim(GribTable), &
!         action='read' &
!         )
!
!    !
!    ! count lines on grib table
!    !
!    nline = 0
!    do
!       read (lu, *, end=88, err=88)
!       nline = nline + 1
!    enddo
!88  continue
!    nline = nline - 1  ! remove header from count
!    rewind (lu)
!
!    allocate (gbt(nline))
!
!    !
!    ! Read reader
!    !
!
!    read (lu, '(A)', end=99, err=99) bf
!
!    do i = 1, nline
!
!       read (lu, '(A)', end=99, err=99) bf
!       call GetTokens_(bf, bf2, nbf, ":")
!
!       read (bf2(1), *) x
!       gtb(i)%parm = int(x)
!       gtb(i)%name = trim(bf2(2))
!       gtb(i)%desc = trim(bf2(3))
!
!    enddo
!
!    close (lu)
!
!  end subroutine grbtb

end module m_GrADSfiles
!.
