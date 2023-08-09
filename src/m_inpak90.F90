!-------------------------------------------------------------------------
!         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-------------------------------------------------------------------------
!BOI
!
! !TITLE: Inpak 90 Documentation \\ Version 1.01
!
! !AUTHORS: Arlindo da Silva
!
! !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
! !DATE: June 20, 1996
!
! !INTRODUCTION: Package Overview
!
!      Inpak 90 is a Fortran (77/90) collection of 
!      routines/functions for accessing {\em Resource Files} 
!      in ASCII format. The package is optimized
!      for minimizing formatted I/O, performing all of its string
!      operations in memory using Fortran intrinsic functions.
!
! \subsection{Resource Files}
!
!      A {\em Resource File} is a text file consisting of variable
!     length lines (records), each possibly starting with a {\em label}
!     (or {\em key}), followed by some data. A simple resource file 
!     looks like this:
!
! \begin{verbatim}
! # Lines starting with # are comments which are
! # ignored during processing.
! my_file_names:         jan87.dat jan88.dat jan89.dat
! radius_of_the_earth:   6.37E6  # these are comments too
! constants:             3.1415   25
! my_favourite_colors:   green blue 022 # text & number are OK
! \end{verbatim}
!
!    In this example, {\tt my\_file\_names:} and {\tt constants:}
!    are labels, while {\tt jan87.dat, jan88.dat} and {\tt jan89.dat} are
!    data associated with label {\tt my\_file\_names:}.
!    Resource files can also contain simple tables of the form,
!
! \begin{verbatim}
! my_table_name::
!  1000     3000     263.0   
!   925     3000     263.0
!   850     3000     263.0
!   700     3000     269.0
!   500     3000     287.0
!   400     3000     295.8
!   300     3000     295.8    
! ::
! \end{verbatim}
!
! Resource files are random access, the particular order of the
! records are not important (except between ::'s in a table definition).
!
!    \subsection{A Quick Stroll}
!
!    The first step is to load the ASCII resource (rc) file into
!    memory\footnote{See next section for a complete description
!    of parameters for each routine/function}:
!
! \begin{verbatim}
!       call i90_LoadF ( 'my_file.rc', iret )
! \end{verbatim}
!
!    The next step is to select the label (record) of interest, say
!
! \begin{verbatim}
!       call i90_label ( 'constants:', iret )
! \end{verbatim}
!
!  The 2 constants above can be retrieved with the following code
!  fragment:
! \begin{verbatim}
!       real    r
!       integer i
!       call i90_label ( 'constants:', iret )
!       r = i90_gfloat(iret)    ! results in r = 3.1415
!       i = i90_gint(iret)      ! results in i = 25
! \end{verbatim}
!
!  The file names above can be retrieved with the following
!  code fragment:
! \begin{verbatim}
!       character*20 fn1, fn2, fn3
!       integer      iret
!       call i90_label ( 'my_file_names:', iret )
!       call i90_Gtoken ( fn1, iret )  ! ==> fn1 = 'jan87.dat'
!       call i90_Gtoken ( fn2, iret )  ! ==> fn1 = 'jan88.dat'
!       call i90_Gtoken ( fn3, iret )  ! ==> fn1 = 'jan89.dat'
! \end{verbatim}
!
! To access the table above, the user first must use {\tt i90\_label()} to 
! locate the beginning of the table, e.g.,
!
! \begin{verbatim}
!       call i90_label ( 'my_table_name::', iret )
! \end{verbatim}
!
! Subsequently, {\tt i90\_gline()} can be used to gain access to each
! row of the table. Here is a code fragment to read the above
! table (7 rows, 3 columns):
!
! \begin{verbatim}
!       real          table(7,3)
!       character*20  word
!       integer       iret
!       call i90_label ( 'my_table_name::', iret )
!       do i = 1, 7
!          call i90_gline ( iret )
!          do j = 1, 3
!             table(i,j) = i90_gfloat ( iret )
!          end do                   
!       end do
! \end{verbatim}
!
! Get the idea?
! 
! \newpage
! \subsection{Main Routine/Functions}
!
! \begin{verbatim}
!  ------------------------------------------------------------------
!         Routine/Function                  Description
!  ------------------------------------------------------------------
!  I90_LoadF ( filen, iret )     loads resource file into memory
!  I90_Label ( label, iret )     selects a label (key)
!  I90_GLine ( iret )            selects next line (for tables)
!  I90_Gtoken ( word, iret )     get next token 
!  I90_Gfloat ( iret )           returns next float number (function)
!  I90_GInt ( iret )             returns next integer number (function)
!  I90_GLogical ( iret )         returns next Logical flag (function)
!  i90_AtoF ( string, iret )     ASCII to float (function)
!  i90_AtoI ( string, iret )     ASCII to integer (function)
!  I90_Len ( string )            string length without trailing blanks
!  LabLin ( label )              similar to i90_label (no iret)
!  FltGet ( default )            returns next float number (function)
!  IntGet ( default )            returns next integer number (function)
!  ChrGet ( default )            returns next character (function)
!  TokGet ( string, default )    get next token
!  ------------------------------------------------------------------
! \end{verbatim}
!
! {\em Common Arguments:}
!
! \begin{verbatim}
! character*(*)      filen       file name
! integer            iret        error return code (0 is OK)
! character*(*)      label       label (key) to locate record
! character*(*)      word        blank delimited string
! character*(*)      string      a sequence of characters
! \end{verbatim}
!
! See the Prologues in the next section for additional details.
!
!
!    \subsection{Package History} 
!       Back in the 70's Eli Isaacson wrote IOPACK in Fortran
!       66.  In June of 1987 I wrote Inpak77 using
!       Fortran 77 string functions; Inpak 77 is a vastly
!       simplified IOPACK, but has its own goodies not found in
!       IOPACK.  Inpak 90 removes some obsolete functionality in
!       Inpak77, and parses the whole resource file in memory for
!       performance.  Despite its name, Inpak 90 compiles fine
!       under any modern Fortran 77 compiler.
!
!   \subsection{Bugs} 
!       Inpak 90 is not very gracious with error messages.  
!       The interactive functionality of Inpak77 has not been implemented.
!       The comment character \# cannot be escaped.
!
!  \subsection{Availability}
!
!   This software is available at 
! \begin{verbatim}
!         ftp://niteroi.gsfc.nasa.gov/pub/packages/i90/ 
! \end{verbatim}
!   There you will find the following files:
! \begin{verbatim}
! i90.f      Fortran 77/90 source code
! i90.h      Include file needed by i90.f
! ti90.f     Test code
! i90.ps     Postscript documentation
! \end{verbatim}
! An on-line version of this document is available at
! \begin{verbatim}
!        ftp://niteroi.gsfc.nasa.gov/www/packages/i90/i90.html
! \end{verbatim}
!
!EOI
!-------------------------------------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!
! !REVISION HISTORY:
! 	03Jul96 - J. Guo	- evolved to Fortran 90 module.  The
!		modifications include 1) additional subroutines to
!		dynamically manage the memory, 2) privatized most
!		entries, 3) included "i90.h" into the module source
!		with better initializations, 4) removed blockdata, 5)
!		used a portable opntext() call to avoid I/O portability
!		problems.
!
!		See I90_page() I90_Release(), and I90_LoadF() for
!		details.
!
!	05Aug98	- Jing Guo	-
!		  Removed i90_page() and its references.
!		  Added internal subroutines push_() and pop_().
!		  Modified i90_release().
!		  Added i90_fullrelease().
!		  Removed %loaded.  Check i90_depth instead.
!       06Aug98 - Todling       - made I90_gstr public
!	20Dec98 - Jing Guo	- replaced the description of I90_Gstr
!	28Sep99 - Jing Guo	- Merged with the MPI version with
!				  some addtional changes based on
!				  merging decisions.
!	12Oct99 - Larson/Guo	- Overloaded fltget() to new routines 
!                 getfltsp() and fltgetdp(), providing better support 
!                 for 32 and 64 bit platforms, respectively.
!  21Feb12 - de Mattos - Include GetVal( )
!
!  07Mar17 - de Mattos - revisited all routines
!                        re-write some pieces of code
!                        remove external dependences
!                          - include i90_perr2_ form m_die
!                          - include stdout, stdin, stderr
!  30Oct19 - de Mattos - add I90_GLogical function
!                        add I90_lcase function
!_______________________________________________________________________

module m_inpak90

  implicit none
  private
!
! !PARAMETERS:
!

  !
  ! Constants
  !

  !
  ! Define Standard Input and Output units
  !

  integer, parameter :: stdin  = 5 ! a unit linked to UNIX stdin
  integer, parameter :: stdout = 6 ! a unit linked to UNIX stdout
  integer, parameter :: stderr = 0 ! a unit linked to UNIX stderr

  !
  ! Define inpak90 kinds
  !

  integer, public, parameter :: I2 = selected_int_kind ( 4)  ! kind for 16-bits integer numbers
  integer, public, parameter :: I4 = selected_int_kind ( 9)  ! kind for 32-bits integer numbers
  integer, public, parameter :: I8 = selected_int_kind (14)  ! kind for 64-bits integer numbers
  integer, public, parameter :: R4 = selected_real_kind( 6)  ! kind for 32-bits real numbers
  integer, public, parameter :: R8 = selected_real_kind(15)  ! kind for 64-bits real numbers

!
! !PUBLIC MEMBER FUNCTIONS:
!

  public :: I90_LoadF   ! loads a resource file into memory
  public :: I90_allLoadF! loads/populates a resource file to all PEs
  public :: I90_Release ! Releases one cached resource file
  public :: I90_fullRelease ! Releases the whole stack
  public :: I90_Label   ! selects a label (key)
  public :: I90_GLine   ! selects the next line (for tables)
  public :: I90_Gtoken  ! gets the next token 
  public :: I90_Gstr    ! get a string upto to a "$" or EOL

  public :: I90_AtoF    ! ASCII to float (function)
  public :: I90_AtoI    ! ASCII to integer (function)

  public :: I90_Gfloat  ! returns next float number (function)
  public :: I90_GInt    ! returns next integer number (function)
  public :: I90_GLogical! returns next logical flag (function)

  public :: lablin,rdnext,fltget,intget,getwrd,str2rn,chrget,getstr
  public :: strget
  public :: I90_GetVal
  public :: I90_perr
  public :: I90_die
  public :: I90_lcase
  public :: I90_lua

  interface fltget
     module procedure fltgetsp, fltgetdp
  end interface

  INTERFACE I90_GetVal
    MODULE PROCEDURE GetReal_, AGetReal_,&
                     GetInt_, AGetInt_,&
                     GetToken_, AGetToken_,&
                     GetLogic_, AGetLogic_
  END INTERFACE I90_GetVal

  interface I90_lcase
     module procedure lower_case
  end interface I90_lcase

  interface I90_perr
     module procedure  perr2_, perr4_
  end interface I90_perr

  interface I90_die
     module procedure  die0_, die2_, die4_
  end interface I90_die

  !
  ! Size of buffers
  !

  integer,   parameter :: MaxLineSize = 512
  integer,   parameter :: NBUF_MAX    = 400*(MaxLineSize) ! max size of buffer

  !
  ! keys parameters
  !
  character, parameter :: BLK = achar(32)   ! blank (space)
  character, parameter :: TAB = achar(09)   ! TAB
  character, parameter :: EOL = achar(10)   ! end of line mark (newline)
  character, parameter :: EOB = achar(00)   ! end of buffer mark (null)
  character, parameter :: NUL = achar(00)   ! what it says

  !
  ! linked list to page different files
  !
  type inpak90
     ! May be easily paged for extentable file size (J.G.)

     integer                             :: nbuf      ! actual size of buffer
     character(len=NBUF_MAX),    pointer :: buffer    ! hold the whole file?
     character(len=MaxLineSize), pointer :: this_line ! the current line
     integer                             :: next_line ! index_ for next line on buffer
     type(inpak90),              pointer :: last
  end type inpak90

  character(len=*),parameter :: myname = 'm_inpak90'
  !-----------------------------------------------------------------------

  integer,parameter :: i90_MXDEP = 4
  integer,save      :: i90_depth = 0
  type(inpak90),save,pointer :: i90_now

  !-----------------------------------------------------------------------
contains
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: I90_allLoadF - populate a rooted database to all PEs
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine I90_allLoadF(fname,root,comm,istat)
!    use m_mpif90, only : MP_i90_perr
!    use m_mpif90, only : MP_comm_rank
!    use m_mpif90, only : MP_CHARACTER
!    use m_mpif90, only : MP_INTEGER
    implicit none
    character(len=*),intent(in) :: fname
    integer,intent(in) :: root
    integer,intent(in) :: comm
    integer,intent(out) :: istat

    ! !REVISION HISTORY:
    ! 	28Jul98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*),parameter :: myname_=myname//' :: I90_allLoadF'
    integer :: myID,ier

    istat = 0
    ier   = 0

    !  call MP_comm_rank(comm,myID,ier)
    !  if(ier/=0) then
    !    call MP_i90_perr(myname_,'MP_comm_rank()',ier)
    !    istat=ier
    !    return
    !  endif

    !  if(myID == root) then
    call i90_LoadF(fname,ier)
    if(ier /= 0) then
       call i90_perr(myname_,'i90_LoadF("'//trim(fname)//'")',ier)
       istat=ier
       return
    endif
    !  else
    !    call push_(ier)
    !    if(ier /= 0) then
    !      call i90_i90_perr(myname_,'push_()',ier)
    !      istat=ier
    !      return
    !    endif
    !  endif

    !	! Initialize the buffer on all PEs

    !  call MPI_Bcast(i90_now%buffer,NBUF_MAX,MP_CHARACTER,root,comm,ier)
    !  if(ier /= 0) then
    !    call MP_i90_i90_perr(myname_,'MPI_Bcast(%buffer)',ier)
    !    istat=ier
    !    return
    !  endif

    !  call MPI_Bcast(i90_now%nbuf,1,MP_INTEGER,root,comm,ier)
    !  if(ier /= 0) then
    !    call MP_i90_perr(myname_,'MPI_Bcast(%nbuf)',ier)
    !    istat=ier
    !    return
    !  endif

    i90_now%this_line=' '
    i90_now%next_line=0

  end subroutine I90_allLoadF

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: push_ - push on a new layer of the internal file _i90_now_
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine push_(ier)

    implicit none

    integer,intent(out) :: ier

    ! !REVISION HISTORY:
    ! 	05Aug98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*),parameter :: myname_ = myname//' :: push_( ... )'
    type(inpak90),pointer :: new => null()

    ier = 0

    if(i90_depth <= 0) nullify(i90_now) ! just an initialization

    ! Too many levels

    if(i90_depth >= i90_MXDEP) then
       call i90_perr(myname_,'(overflow)',i90_depth)
       ier=1
       return
    endif

    allocate(new,stat=ier)
    if(ier /= 0) then
       call i90_perr(myname_,'allocate(new)',ier)
       return
    endif

    allocate(new%buffer,new%this_line,stat=ier)
    if(ier /= 0) then
       call i90_perr(myname_,'allocate(new%..)',ier)
       return
    endif

    new%last => i90_now
    i90_now  => new
    nullify(new)

    i90_depth = i90_depth+1
  end subroutine push_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: pop_ - pop off a layer of the internal file _i90_now_
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine pop_(ier)
  
    integer,intent(out) :: ier

    ! !REVISION HISTORY:
    ! 	05Aug98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*),parameter :: myname_=myname//' :: pop_( ... )'
    type(inpak90),pointer :: old

    ier = 0

    if(i90_depth <= 0) then
       call i90_perr(myname_,'(underflow)',i90_depth)
       ier=1
       return
    endif

    old => i90_now%last

    deallocate(i90_now%buffer,i90_now%this_line,stat=ier)
    if(ier /= 0) then
       call i90_perr(myname_,'deallocate(new%..)',ier)
       return
    endif

    deallocate(i90_now,stat=ier)
    if(ier /= 0) then
       call i90_perr(myname_,'deallocate(new)',ier)
       return
    endif

    i90_now => old
    nullify(old)

    i90_depth = i90_depth - 1
  end subroutine pop_

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !-----------------------------------------------------------------------
  !
  ! !ROUTINE: I90_Release - deallocate memory used to load a resource file
  !
  ! !INTERFACE:
  !
  subroutine I90_Release(stat)
    implicit none
    integer,optional, intent(out) :: stat 
    !
    ! !DESCRIPTION:
    !
    !	I90_Release() is used to pair I90_LoadF() to release the memory
    !	used by I90_LoadF() for resourse data input.
    !
    ! !SEE ALSO:
    !
    ! !REVISION HISTORY:
    ! 	03Jul96 - J. Guo	- added to Arlindo's inpak90 for its
    !				  Fortran 90 revision.
    !_______________________________________________________________________
    character(len=*),parameter :: myname_=myname//' :: i90_Release( ... )'
    integer :: ier

    if(present(stat)) stat=0

    ier = 0
    call pop_(ier)
    if(ier/=0) then
       call i90_perr(myname_,'pop_()',ier)
       if(.not.present(stat)) call i90_perr(myname_,'pop_()')
       stat=ier
       return
    endif

  end subroutine I90_Release

  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
  !BOP -------------------------------------------------------------------
  !
  ! !IROUTINE: i90_fullRelease - releases the whole stack led by _i90_now_
  !
  ! !DESCRIPTION:
  !
  ! !INTERFACE:

  subroutine i90_fullRelease(ier)
    implicit none
    integer,intent(out) :: ier

    ! !REVISION HISTORY:
    ! 	05Aug98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
    !EOP ___________________________________________________________________

    character(len=*),parameter :: myname_=myname//' :: i90_fullRelease( ... )'

    ier = 0
    do while(i90_depth > 0)
       call pop_(ier)
       if(ier /= 0) then
          call i90_perr(myname_,'pop_()',ier)
          return
       endif
    end do
    ier=0

  end subroutine i90_fullRelease
  !=======================================================================

  subroutine I90_LoadF ( filen, iret )
    implicit NONE

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_LoadF() --- Loads resource file into memory.
    ! 
    ! !DESCRIPTION: 
    !
    !  Reads resource file, strips out comments, translate TAB's into
    !  blanks, and loads the modified file contents into memory.
    !  Must be called only once for each resource file.
    !
    ! !CALLING SEQUENCE: 
    !
    !     call i90_LoadF ( filen, iret )
    !
    ! !INPUT PARAMETERS: 
    !
    character(len=*) :: filen         ! file name

    ! !OUTPUT PARAMETERS:

    integer       iret                ! Return code:
    !   0    no error
    ! -98    coult not get unit number
    !        (strange!)
    ! -98    talk to a wizzard
    ! -99    out of memory: increase
    !        NBUF_MAX in 'i90.h'
    ! other  iostat from open statement.
    !
    ! !BUGS:  
    !
    !  It does not perform dynamic allocation, mostly to keep vanilla f77
    !  compatibility. Overall amount of static memory is small (~100K
    !  for default NBUF_MAX = 400*256).
    !
    ! !SEE ALSO: 
    !
    !  i90_label()   selects a label (key)
    !
    ! !FILES USED:  
    !
    !  File name supplied on input. The file is opened, read and then closed.
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------
    !EOC
    character(len=*), parameter :: myname_ = myname//' :: i90_loadf( ... )'

    character(len=MaxLineSize) :: line
    integer                    :: lu
    integer                    :: ios
    integer                    :: loop
    integer                    :: ls
    integer                    :: ptr

    iret = 0

    ! Check to make sure there is not too many levels
    ! of the stacked resource files

    if(i90_depth >= i90_MXDEP) then
       call i90_perr(myname_,'(overflow)',i90_depth)
       iret=1
       return
    endif

    !     Open file
    !     ---------

    lu = i90_lua()

    if ( lu .lt. 0 ) then
       iret = -97
       return
    end if

    ! A open through an interface to avoid portability problems.
    ! (J.G.)

    ios = 0
    call opntext(lu,filen,'old',ios)
    if ( ios .ne. 0 ) then
       write(stderr,'(2a,i5)') myname_,': opntext() error, ios =',ios
       iret = ios
       return
    end if

    ! Create a dynamic page to store the file.  It might be expanded
    ! to allocate memory on requests (a link list) (J.G.)

    ! Changed from page_() to push_(), to allow multiple (stacked)
    ! inpak90 buffers.  J.G.

    call push_(ios)	! to create buffer space
    if ( ios .ne. 0 ) then
       write(stderr,'(2a,i5)') myname_,': push_() error, ios =',ios
       iret = ios
       return
    end if

    !     Read to end of file
    !     -------------------
    i90_now%buffer(1:1) = EOL
    ptr = 2                         ! next buffer position
    do loop = 1, NBUF_MAX

       !        Read next line
       !        --------------
       read(lu,'(a)', end=11) line  ! read next line
       call i90_trim ( line )       ! remove trailing blanks
       call i90_pad ( line )        ! Pad with # from end of line

       !        A non-empty line
       !        ----------------
       ls = index_(line,'#' ) - 1    ! line length
       if ( ls .gt. 0 ) then
          if ( (ptr+ls) .gt. NBUF_MAX ) then
             iret = -99
             return
          end if
          i90_now%buffer(ptr:ptr+ls) = line(1:ls) // EOL
          ptr = ptr + ls + 1
       end if

    end do

    iret = -98 ! good chance i90_now%buffer is not big enough 
    return

11  continue

    !     All done
    !     --------
    !      close(lu)
    
    call clstext(lu,ios)
    if(ios /= 0) then
       iret=-99
       return
    endif

    i90_now%buffer(ptr:ptr) = EOB
    i90_now%nbuf = ptr
    i90_now%this_line=' '
    i90_now%next_line=0
    iret = 0

    return
  end subroutine I90_LoadF


  !...................................................................

  subroutine i90_label ( label, iret, lcase )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_Label() --- Selects a label (record).
    ! 
    ! !DESCRIPTION: 
    !
    !  Once the buffer has been loaded with {\tt i90\_loadf()}, this routine
    !  selects a given ``line'' (record/table) associated with ``label''. 
    !  Think of ``label'' as a resource name or data base ``key''.
    !
    ! !CALLING SEQUENCE: 
    !           
    !     call i90_Label ( label, iret )
    !
    ! !INPUT PARAMETERS: 
    !
    character(len=*),  intent(in) :: label ! input label
    logical, optional, intent(in) :: lcase ! case sensitive? (.true./.false.)

    ! !OUTPUT PARAMETERS:

    integer       iret             ! Return code:
    !   0    no error
    !  -1    buffer not loaded
    !  -2    could not find label   
    !
    ! !SEE ALSO: 
    !
    !  i90_loadf()    load file into buffer
    !  i90_gtoken()   get next token
    !  i90_gline()    get next line (for tables)
    !  atof()         convert word (string) to float
    !  atoi()         convert word (string) to integer
    !  
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  08Mar17   de Mattos  add case sensitive label
    !
    !EOP
    !-------------------------------------------------------------------------
    character(len=*),parameter :: myname_=myname//' :: i90_label( ... )'
#ifdef CCE_xc50
    character(len=NBUF_MAX) :: tmpstr
#endif

    integer :: i, j
    logical :: SenCase

    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------
    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    !
    ! Label is case sensitive (default: .false.)
    !

    SenCase = .false.
    if(present(lcase)) SenCase = lcase


    !     Determine whether label exists
    !     ------------------------------    

    i = index_ ( i90_now%buffer(1:i90_now%nbuf), EOL//trim(label), SenCase ) + 1
    if ( i .eq. 1 ) then
       i90_now%this_line = BLK // EOL
       iret = -2
       return
    elseif(i.le.0) then
       call i90_perr(myname_,'invalid index_() return',i)
    end if

    !     Extract the line associated with this label
    !     -------------------------------------------
    i = i + len ( label )

#ifndef CCE_xc50
    j = i + index_(i90_now%buffer(i:i90_now%nbuf),EOL) - 2
#else
    ! has a bug at xc50 cce 8.6.5 that 
    ! we can bypass only by this way
    tmpstr = i90_now%buffer(i:i90_now%nbuf)
    j = i + index_(trim(tmpStr),EOL) - 2
#endif

    i90_now%this_line = i90_now%buffer(i:j) // BLK // EOL

    i90_now%next_line = j + 2

    iret = 0

    return
  end subroutine i90_label

  !...................................................................

  subroutine i90_gline ( iret, line )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_GLine() --- Selects next line.
    ! 
    ! !DESCRIPTION: 
    !
    !     Selects next line, irrespective of of label. If the next line starts
    ! with :: (end of table mark), then it lets the user know. This sequential
    ! access of the buffer is useful to assess tables, a concept introduced
    ! in Inpak 77 by Jing Guo. A table is a construct like this:
    !
    ! \begin{verbatim}
    ! my_table_name::
    !  1000     3000     263.0   
    !   925     3000     263.0
    !   850     3000     263.0
    !   700     3000     269.0
    !   500     3000     287.0
    !   400     3000     295.8
    !   300     3000     295.8    
    ! ::
    ! \end{verbatim}
    !
    ! To access this table, the user first must use {\tt i90\_label()} to 
    ! locate the beginning of the table, e.g.,
    !
    ! \begin{verbatim}
    !       call i90_label ( 'my_table_name::', iret )
    ! \end{verbatim}
    !
    ! Subsequently, {\tt i90\_gline()} can be used to gain acess to each
    ! row of the table. Here is a code fragment to read the above
    ! table (7 rows, 3 columns):
    !
    ! \begin{verbatim}
    !       real          table(7,3)
    !       character*20  word
    !       integer       iret
    !       call i90_label ( 'my_table_name::', iret )
    !       do i = 1, 7
    !          call i90_gline ( iret )
    !          do j = 1, 3
    !             table(i,j) = fltget ( 0. )
    !          end do                   
    !       end do
    ! \end{verbatim}
    !
    !  For simplicity we have assumed that the dimensions of table were
    !  known. It is relatively simple to infer the table dimensions
    !  by manipulating ``iret''.
    !
    ! !CALLING SEQUENCE: 
    !
    !     call i90_gline ( iret )
    !
    ! !INPUT PARAMETERS: 
    !
    !     None.
    !
    ! !OUTPUT PARAMETERS:
    !
    integer       iret             ! Return code:
    !   0    no error
    !  -1    end of buffer reached
    !  +1    end of table  reached

    character(len=MaxLineSize), optional, intent(out) :: line

    ! !SEE ALSO: 
    !
    !  i90_label()    selects a line (record/table)
    !
    ! !REVISION HISTORY: 
    !
    !  10feb95   Guo        Wrote rdnext(), Inpak 77 extension.
    !  19Jun96   da Silva   Original code with functionality of rdnext()
    !
    !EOP
    !-------------------------------------------------------------------------

    integer i, j
#ifdef CCE_xc50
    character(len=NBUF_MAX) :: tmpstr
#endif
    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------
    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    if ( i90_now%next_line .ge. i90_now%nbuf ) then
       iret = -1
       return
    end if

    i = i90_now%next_line
#ifndef CCE_xc50
    j = i + index_(i90_now%buffer(i:i90_now%nbuf),EOL) - 2
#else
    ! has a bug at xc50 cce 8.6.5 that 
    ! we can bypass only by this way
    tmpstr = i90_now%buffer(i:i90_now%nbuf)
    j = i + index_(trim(tmpStr),EOL) - 2
#endif
    i90_now%this_line = i90_now%buffer(i:j) // BLK // EOL

    if ( i90_now%this_line(1:2) .eq. '::' ) then
       iret = 1                        ! end of table
       i90_now%next_line = i90_now%nbuf + 1
       return
    end if

    i90_now%next_line = j + 2
    iret = 0

    if(present(line)) line = i90_now%this_line

    return
  end subroutine i90_gline

  !...................................................................

  subroutine i90_GToken ( token, iret )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_GToken()  --- Gets next token.
    ! 
    ! !DESCRIPTION: 
    !
    !  Get next token from current line. The current line is defined by a
    !  call to {\tt i90\_label()}. Tokens are sequences of characters (including
    !  blanks) which may be enclosed by single (') or double (") quotes. 
    !  If no quotes are present, the token from the current position to the next
    !  blank of TAB is returned.
    !  
    !  {\em Examples of valid token:}
    !
    !  \begin{verbatim}
    !               single_token "second token on line"
    !               "this is a token"
    !               'Another example of a token'
    !               'this is how you get a " inside a token'
    !               "this is how you get a ' inside a token"
    !               This is valid too   # the line ends before the #
    !  \end{verbatim}
    !  The last line has 4 valid tokens: {\tt This, is, valid} and {\tt too}.
    !  
    !  {\em Invalid string constructs:}
    !
    !  \begin{verbatim}
    !               'cannot handle mixed quotes"
    !               'escaping like this \' is not implemented'
    !               'this # will not work because of the #'
    !  \end{verbatim}
    !  The \# character is reserved for comments and cannot be included
    !  inside quotation marks.
    !
    ! !CALLING SEQUENCE: 
    !
    !     call i90_GToken ( token, iret )
    !
    ! !INPUT PARAMETERS: 
    !
    !     None.
    !
    ! !OUTPUT PARAMETERS:
    !
    character*(*) :: token            ! Next token from current line
    integer ::      iret             ! Return code:
    !   0    no error
    !  -1    either nothing left
    !        on line or mismatched
    !        quotation marks.

    ! !BUGS:  
    !
    !     Standard Unix escaping is not implemented at the moment.
    !     
    !
    ! !SEE ALSO: 
    !
    !  i90_label()    selects a line (record/table)
    !  i90_gline()    get next line (for tables)
    !  atof()         convert word (string) to float
    !  atoi()         convert word (string) to integer
    !  
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  19Jul14   de Mattos  Include optional delimiter
    !
    !EOP
    !-------------------------------------------------------------------------

    character*1   ch
    integer       ib, ie

    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------
    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    call i90_trim ( i90_now%this_line )

    ch = i90_now%this_line(1:1)
    if ( ch .eq. '"' .or. ch .eq. "'" ) then
       ib = 2
       ie = index_ ( i90_now%this_line(ib:), ch ) 
    else
       ib = 1
       ie = min(index_(i90_now%this_line,BLK),	&
            index_(i90_now%this_line,EOL)) - 1

    end if

    if ( ie .lt. ib ) then
       token = BLK
       iret = -1
       return
    else
       ! Get the token, and shift the rest of %this_line to
       ! the left

       token = i90_now%this_line(ib:ie) 
       i90_now%this_line = i90_now%this_line(ie+2:)
       iret = 0
    end if
    return
  end subroutine i90_gtoken
  !...................................................................
  subroutine i90_gstr ( string, iret )


    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !
    ! !ROUTINE:  I90\_GStr()
    ! 
    ! !DESCRIPTION: 
    !
    !  Get next string from current line. The current line is defined by a
    !  call to {\tt i90\_label()}. Strings are sequence of characters (including
    !  blanks) enclosed by single (') or double (") quotes. If no quotes
    !  are present, the string from the current position to the end of 
    !  the line is returned.
    !
    !  NOTE: This routine is defined differently from \verb"i90_GTolen()",
    !	 where a {\sl token} is white-space delimited, but this routine
    !	 will try to fetch a string either terminated by a "$" or by the
    !	 end of the line.
    !
    !  {\em Examples of valid strings:}
    !
    !  \begin{verbatim}
    !               "this is a string"
    !               'Another example of string'
    !               'this is how you get a " inside a string'
    !               "this is how you get a ' inside a string"
    !               This is valid too   # the line ends before the #
    !
    !  \end{verbatim}
    !  
    !  {\em Invalid string constructs:}
    !
    !  \begin{verbatim}
    !               'cannot handle mixed quotes"
    !               'escaping like this \' is not implemented'
    !  \end{verbatim}
    !
    !  {\em Obsolete feature (for Inpak 77 compatibility):}
    !
    !  \begin{verbatim}
    !               the string ends after a $ this is another string
    !  \end{verbatim}
    !
    ! !CALLING SEQUENCE: 
    !
    !  \begin{verbatim}
    !     call i90_Gstr ( string, iret )
    !  \end{verbatim}
    !
    ! !INPUT PARAMETERS: 
    !
    character*(*) string           ! A NULL (char(0)) delimited string.

    ! !OUTPUT PARAMETERS:
    !
    integer       iret             ! Return code:
    !   0    no error
    !  -1    either nothing left
    !        on line or mismatched
    !        quotation marks.

    ! !BUGS:  
    !
    !     Standard Unix escaping is not implemented at the moment.
    !     No way to tell sintax error from end of line (same iret).
    !     
    !
    ! !SEE ALSO: 
    !
    !  i90_label()    selects a line (record/table)
    !  i90_gtoken()   get next token
    !  i90_gline()    get next line (for tables)
    !  atof()         convert word (string) to float
    !  atoi()         convert word (string) to integer
    !  
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  01Oct96   Jing Guo	Removed the null terminitor
    !
    !-------------------------------------------------------------------------

    character*1   ch
    integer       ib, ie

    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------
    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    call i90_trim ( i90_now%this_line )

    ch = i90_now%this_line(1:1)
    if ( ch .eq. '"' .or. ch .eq. "'" ) then
       ib = 2
       ie = index_ ( i90_now%this_line(ib:), ch ) 
    else
       ib = 1
       ie = index_(i90_now%this_line,'$')-1  ! undocumented feature!
       if ( ie .lt. 1 ) ie = index_(i90_now%this_line,EOL)-2
    end if

    if ( ie .lt. ib ) then
       !         string = NULL
       iret = -1
       return
    else
       string = i90_now%this_line(ib:ie) ! // NULL
       i90_now%this_line = i90_now%this_line(ie+2:)
       iret = 0
    end if

    return
  end subroutine i90_gstr

  !...................................................................

  function i90_GFloat( iret )

    real(R8) :: i90_GFloat

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: i90_GFloat() --- Returns next float number.
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next float (real number) from the current line.
    !  If an error occurs a zero value is returned.
    !
    ! !CALLING SEQUENCE: 
    !
    !      real  rnumber
    !      rnumber = i90_gfloat ( iret )
    !
    ! !OUTPUT PARAMETERS: 
    !
    integer,intent(out) :: iret    ! Return code:
    !   0    no error
    !  -1    either nothing left
    !        on line or mismatched
    !        quotation marks.
    !  -2    parsing error

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    character(len=MaxLineSize) :: token
    integer                    :: ios
    real(R8)                   :: x

    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------

    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    x = 0.

    call i90_gtoken ( token, iret )
    if ( iret .eq. 0 ) then
       read(token,*,iostat=ios) x	! Does it require an extension?
       if ( ios .ne. 0 ) iret = -2
    end if
    if ( iret .ne. 0 ) x = 0.

    i90_GFloat = x

    return
  end function i90_GFloat

  !...................................................................
  function i90_GLogical( iret )

    logical :: i90_GLogical

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: i90_GLogical() --- Returns next Logical flag.
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next Logical flag from the current line.
    !  If an error occurs a zero value is returned.
    !
    ! !CALLING SEQUENCE: 
    !
    !      real  rnumber
    !      logic = i90_glogical ( iret )
    !
    ! !OUTPUT PARAMETERS: 
    !
    integer,intent(out) :: iret    ! Return code:
    !   0    no error
    !  -1    either nothing left
    !        on line or mismatched
    !        quotation marks.
    !  -2    parsing error

    !
    ! !REVISION HISTORY: 
    !
    !  30Oct2019   de Mattos   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    character(len=MaxLineSize) :: token
    integer       ios
    Logical       x

    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------

    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    x = .false.

    call i90_gtoken ( token, iret )
    if ( iret .eq. 0 ) then
       read(token,*,iostat=ios) x	! Does it require an extension?
       if ( ios .ne. 0 ) iret = -2
    end if
    if ( iret .ne. 0 ) x = .false.
    i90_GLogical = x

    return
  end function i90_GLogical

  !...................................................................

  integer function I90_GInt ( iret )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: I90_GInt() --- Returns next integer number.
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next integer number from the current line.
    !  If an error occurs a zero value is returned.
    !
    ! !CALLING SEQUENCE: 
    !
    !      integer number
    !      number = i90_gint ( default )
    !
    ! !OUTPUT PARAMETERS: 
    !
!    character*(*), optional :: del
    integer :: iret  
    !
    ! Return code:
    !   0    no error
    !  -1    either nothing left
    !        on line or mismatched
    !        quotation marks.
    !  -2    parsing error

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  24may00   da Silva   delcared x as real*8 in case this module is compiled
    !                       with real*4
    !
    !EOP
    !-------------------------------------------------------------------------

    character(len=MaxLineSize) :: token
    real(kind=8)               :: x
    integer                    :: ios

    iret = 0

    !	Make sure that a buffer is defined (JG)
    !	----------------------------------
    if(i90_depth <= 0) then
       iret = -1
       return
    endif

    x = 0

    call i90_gtoken ( token, iret )
    if ( iret .eq. 0 ) then
       read(token,*,iostat=ios) x
       if ( ios .ne. 0 ) iret = -2
    end if
    if ( iret .ne. 0 ) x = 0
    i90_gint = nint(x)

    return
  end function i90_gint

  !...................................................................

  real function i90_AtoF( string, iret )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  i90_AtoF() --- Translates ASCII (string) to float.
    ! 
    ! !DESCRIPTION: 
    !
    !     Converts string to real number. Same as obsolete {\tt str2rn()}.
    !
    ! !CALLING SEQUENCE: 
    !
    !     real  rnumber
    !     rnumber = i90_atof ( string, iret )
    !
    ! !INPUT PARAMETERS: 
    !
    character(len=*),intent(in) :: string           ! a string

    ! !OUTPUT PARAMETERS:
    !
    integer,intent(out) :: iret    ! Return code:
    !   0    no error
    !  -1    could not convert, probably
    !        string is not a number

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    iret = 0

    read(string,*,end=11,err=11) i90_AtoF
    iret = 0
    return
11  iret = -1
    return
  end function i90_AtoF

  !...................................................................

  integer function i90_atoi ( string, iret )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_AtoI() --- Translates ASCII (strings) to integer.
    ! 
    ! !DESCRIPTION: 
    !
    !     Converts string to integer number.
    !
    ! !CALLING SEQUENCE: 
    !
    !     integer number
    !     number = i90_atoi ( string, iret )
    !
    ! !INPUT PARAMETERS: 
    !
    character*(*) string           ! a string

    ! !OUTPUT PARAMETERS:
    !
    integer       iret             ! Return code:
    !   0    no error
    !  -1    could not convert, probably
    !        string is not a number

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    iret = 0

    read(string,*,end=11,err=11) i90_atoi
    iret = 0
    return
11  iret = -1
    return
  end function i90_atoi

  !...................................................................

  integer function i90_Len ( string )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_Len() --- Returns length of string.
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns the length of a string excluding trailing blanks.
    !  It follows that 
    !  \begin{verbatim}
    !              i90_len(string) .le. len(string),
    !  \end{verbatim}
    !  where {\tt len} is the intrinsic string length function.  
    !  Example:
    !  \begin{verbatim}
    !         ls = len('abc  ')       ! results in ls = 5
    !         ls = i90_len ('abc  ')  ! results in ls = 3
    !  \end{verbatim}
    !
    ! !CALLING SEQUENCE: 
    !
    !       integer ls
    !       ls = i90_len ( string )
    !
    ! !INPUT PARAMETERS: 
    !
    character*(*)   string     ! a string
    !
    ! !OUTPUT PARAMETERS:
    !
    !        The length of the string, excluding trailing blanks.
    !
    ! !REVISION HISTORY: 
    !
    !  01Apr94   Guo        Original code (a.k.a. luavail())
    !  19Jun96   da Silva   Minor modification + prologue.
    !
    !EOP
    !-------------------------------------------------------------------------

    integer ls, i, l
    ls = len(string)
    do i = ls, 1, -1
       l = i
       if ( string(i:i) .ne. BLK ) go to 11
    end do
    l = l - 1
11  continue
    i90_len = l
    return
  end function i90_len

  !...................................................................

  function I90_Lua( exclude ) result( iunit )


    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_Lua() --- Returns available logical unit number.
    ! 
    ! !DESCRIPTION: 
    !
    !  Look for an available (not opened) Fortran logical unit for i/o.
    !
    ! !CALLING SEQUENCE: 
    !
    !       integer lu
    !       lu = i90_lua( )
    !
    ! !INPUT PARAMETERS: 
    !

    ! Skip this logical unit      
    integer, optional :: exclude(:)

    !
    ! !RETURN VALUE:
    !

    integer :: iunit   ! The desired unit number if positive, -1 if unsucessful.

    !
    ! !REVISION HISTORY: 
    !
    !  01Apr94   Guo        Original code (a.k.a. luavail())
    !  19Jun96   da Silva   Minor modification + prologue.
    !  11Oct16   de Mattos   
    !EOP
    !-------------------------------------------------------------------------
    !BOC
    !
    character(len=*), parameter :: myname_        = myname//' :: I90_Lua( ... )'
    integer,          parameter :: MaxLogicalUnit = 254

    integer :: i
    integer :: ios
    logical :: inuse


    iunit = -1
    ios   =  0
    inuse = .true.

    find_unit:do while(ios.eq.0.and.inuse)

       iunit = iunit + 1

       ! Test #1, not exclude selected units

       if (present(exclude))then
          do i=1,size(exclude)
             if (iunit.eq.exclude(i)) cycle find_unit
          enddo
       endif

       ! Test #2, reserved units

	    inuse = (iunit.eq.stdout .or. iunit.eq.stdin .or. iunit.eq.stderr)

       ! Test #3, in-use

       if(.not.inuse) inquire(unit=iunit,opened=inuse,iostat=ios)

    end do find_unit

    if(ios .ne. 0) iunit = -1

    return
  end function i90_lua

  !...................................................................

  subroutine i90_pad ( string )


    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_Pad() --- Pad strings.
    ! 
    ! !DESCRIPTION: 
    !
    !     Pads from the right with the comment character (\#). It also
    !  replaces TAB's with blanks for convenience. This is a low level
    !  i90 routine.
    !
    ! !CALLING SEQUENCE: 
    !
    !      call i90_pad ( string )
    !
    ! !INPUT PARAMETERS: 
    !
    character(len=MaxLineSize) :: string       ! input string

    ! !OUTPUT PARAMETERS:            ! modified string
    !
    !      character(len=MaxLineSize) string
    !
    ! !BUGS:  
    !
    !      It alters TAB's even inside strings.
    !
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    integer i

    !     Pad end of string with #
    !     ------------------------
    do i = MaxLineSize, 1, -1 
       if ( string(i:i) .ne. ' ' .and.	&
            string(i:i) .ne. '$' ) go to 11
       string(i:i) = '#'
    end do
11  continue

    !     Replace TAB's with blanks
    !     -------------------------
    do i = 1, MaxLineSize
       if ( string(i:i) .eq. TAB ) string(i:i) = BLK
       if ( string(i:i) .eq. '#' ) go to 21
    end do
21  continue

    return
  end subroutine i90_pad

  !...................................................................

  subroutine I90_Trim ( string )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  I90_Trim() - Removes leading blanks from strings.
    !
    ! !DESCRIPTION: 
    !
    !    Removes blanks and TABS from begenning of string. 
    !    This is a low level i90 routine.
    ! 
    ! !CALLING SEQUENCE: 
    !
    !     call i90_Trim ( string )
    !
    ! !INPUT PARAMETERS: 
    !
    character(len=MaxLineSize) :: string    ! the input string
    !
    ! !OUTPUT PARAMETERS:
    !
    !     character(len=MaxLineSize) :: string    ! the modified string
    !
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    integer     ib, i

    !     Get rid of leading blanks
    !     -------------------------
    ib = 1
    do i = 1, MaxLineSize-1
       if ( string(i:i) .ne. ' ' .and.	&
            string(i:i) .ne. TAB ) go to 21
       ib = ib + 1
    end do
21  continue

    !     String without trailling blanks
    !     -------------------------------
    string = string(ib:)

    return
  end subroutine i90_trim


  !==========================================================================


  !                        -----------------------------
  !                        Inpak 77 Upward Compatibility
  !                        -----------------------------


  subroutine lablin ( label )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE:  Lablin() --- Selects a Label (Inpak 77)
    ! 
    ! !DESCRIPTION: 
    !
    !    Selects a given ``line'' (record/table) associated with ``label''. 
    !    Similar to {\tt i90\_label()}, but prints a message to {\tt stdout}
    !    if it cannot locate the label. Kept for Inpak 77 upward compatibility.
    !
    ! !CALLING SEQUENCE: 
    !
    !     call lablin ( label )
    !
    ! !INPUT PARAMETERS: 

    character(len=*),intent(in) :: label   ! string with label name
    !
    ! !OUTPUT PARAMETERS:
    !
    !    None.
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    integer       iret

    iret = 0
    call i90_label ( label, iret )
    if ( iret .ne. 0 ) then
       write(stderr,'(2a)') 'i90/lablin: cannot find label ', label
    endif

  end subroutine lablin

  !...................................................................

  real function fltgetsp ( default )


    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: FltGetsp() --- Returns next float (Inpak 77, single precision)
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next float (real number, single precision) from the current 
    !  line, or a default value if it fails to obtain the desired number.
    !  Kept for Inpak 77 upward compatibility.
    !
    ! !CALLING SEQUENCE: 
    !
    !      real  rnumber, default
    !      rnumber = fltgetsp ( default )
    !
    ! !INPUT PARAMETERS: 
    !
    real(R4), intent(IN) ::    default       ! default value.

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  12Oct99   Guo/Larson - Built from original FltGet() function.
    !
    !EOP
    !-------------------------------------------------------------------------

    character(len=MaxLineSize) :: token
    real                       :: x
    integer                    :: iret

    x = default
    call i90_gtoken ( token, iret )
    if ( iret .eq. 0 ) then
       read(token,*,iostat=iret) x
    end if
    if ( iret .ne. 0 ) x = default
    fltgetsp = x

    return
  end function fltgetsp

  !...................................................................

  real function fltgetdp ( default )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: FltGetdp() --- Returns next float (Inpak 77)
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next float (real number) from the current line, or a 
    !  default value (double precision) if it fails to obtain the desired 
    !  number.  Kept for Inpak 77 upward compatibility.
    !
    ! !CALLING SEQUENCE: 
    !
    !      real(DP) :: default
    !      real :: rnumber 
    !      rnumber = FltGetdp(default)
    !
    ! !INPUT PARAMETERS: 
    !
    real(R8), intent(IN) ::    default       ! default value.

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  12Oct99   Guo/Larson - Built from original FltGet() function.
    !
    !EOP
    !-------------------------------------------------------------------------

    character (len=MaxLineSize) :: token
    real                        :: x
    integer                     :: iret

    x = default
    call i90_gtoken ( token, iret )
    if ( iret .eq. 0 ) then
       read(token,*,iostat=iret) x
    end if
    if ( iret .ne. 0 ) x = default
    fltgetdp = x

    return
  end function fltgetdp

  !...................................................................

  integer function intget ( default )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: IntGet() --- Returns next integer (Inpak 77). 
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next integer number from the current line, or a default 
    !  value if it fails to obtain the desired number.
    !  Kept for Inpak 77 upward compatibility.
    !
    ! !CALLING SEQUENCE: 
    !
    !      integer number, default
    !      number = intget ( default )
    !
    ! !INPUT PARAMETERS: 
    !
    integer     default       ! default value.

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    character(len=MaxLineSize) :: token
    real                       :: x
    integer                    :: iret

    x = default
    call i90_gtoken ( token, iret )
    if ( iret .eq. 0 ) then
       read(token,*,iostat=iret) x
    end if
    if ( iret .ne. 0 ) x = default
    intget = nint(x)

    return
  end function intget

  !...................................................................

  character function chrget ( default )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: ChrGet() --- Returns next character (Inpak 77).
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next non-blank character from the current line, or a default 
    !  character if it fails for whatever reason.
    !  Kept for Inpak 77 upward compatibility.
    !
    ! !CALLING SEQUENCE: 
    !
    !     character*1 ch, default
    !     ch = chrget ( default )
    !
    ! !INPUT PARAMETERS: 
    !
    character*1    default       ! default value.

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    character(len=MaxLineSize) :: token
    integer       iret

    iret = 0
    call i90_gtoken ( token, iret )
    if ( iret .ne. 0 ) then
       chrget = default
    else
       chrget = token(1:1)
    end if
    !print *, chrget

    return
  end function chrget

  !...................................................................

  subroutine TokGet ( token, default )


    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: TokGet() --- Gets next token (Inpakk 77 like).
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next token from the current line, or a default 
    !  word if it fails for whatever reason.
    !
    ! !CALLING SEQUENCE: 
    !
    !      call TokGet ( token, default )
    !
    ! !INPUT PARAMETERS: 
    !
    character*(*) default     ! default token

    ! !OUTPUT PARAMETERS:
    !
    character*(*) token       ! desired token
    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------

    integer       iret

    iret = 0
    call i90_GToken ( token, iret )
    if ( iret .ne. 0 ) then
       token = default
    end if

    return
  end subroutine tokget

  !........................................................................

  function index__(string,tok,sCase)result(idx)
    character(len=*), intent(in) :: string, tok
    logical, optional            :: SCase
    integer                      :: idx

    integer :: n, lt
    integer :: i
    character(len=len(string)) :: tmpStr
    character(len=len(tok))    :: tmpTok
    logical :: SensitiveCase

    integer :: itop


    SensitiveCase = .false.
    if(present(SCase))SensitiveCase = SCase

    n    = len(string)      ! length of string
    lt   = len(tok)         ! length of token

    if(.not.SensitiveCase)then
       tmpStr = lower_case(string)
       tmpTok = lower_case(tok)
    else
       tmpStr = string
       tmpTok = tok
    endif

  
    idx = - 1
  
    do i = 1, n
      itop = min(i+(lt-1),n)
      
      if ( tmpStr(i:itop) == tmpTok(1:lt) ) then

        idx = i
        return
      end if
  
    end do
  

  end function index__

  integer function index_ (string,tok, SCase)

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !BOP
    !
    ! !ROUTINE: index_ Extension of the Fortran 77 intrinsic "index" for
    !  "string" (input) with length that can exceed 2**15-1 (=MAXLEN).  
    !
    ! !DESCRIPTION: Finds the starting location = "index_", of the first character in "tok" 
    !  within "string", where string is of "arbitrary" length.  If tok occurs more than
    !  once in "string", then the value of index_ is based on the first occurrence of "tok". 
    !
    ! !CALLING SEQUENCE:
    !
    !      index_( string,tok )
    !
    ! !INPUT PARAMETERS:
    !
    character(len=*), intent(in) :: string, tok
    logical, optional            :: SCase
    !
    ! !REVISION HISTORY:
    !
    !  2001Apr25   G. Gaspari   Original code.
    !
    !EOP
    !-------------------------------------------------------------------------
    !BOC
    integer idx, i, n, nlen, lt, ibot, itop
    integer, parameter :: MAXLEN = 32767   ! max size of signed 2-byte integer

    character(len=len(string)) :: tmpStr
    character(len=len(tok))    :: tmpTok
    logical :: SensitiveCase

    SensitiveCase = .false.
    if(present(SCase))SensitiveCase = SCase

    n    = len(string)      ! length of string
    lt   = len(tok)         ! length of token
    i    = 1                ! initialize loop index
    nlen = MAXLEN-lt        ! index requires len(sting)+len(tok)<=MAXLEN 
    itop = min(nlen,n)      ! top of string to index
    ibot = 1                ! bottom of string
    if(.not.SensitiveCase)then
       tmpStr(1:n)  = lower_case(string)
       tmpTok(1:lt) = lower_case(tok)
    else
       tmpStr(1:n)  = string
       tmpTok(1:lt) = tok
    endif

!    idx  = index(string(ibot:itop),tok)  ! set for good, if itop=n (<=MAXLEN)
    idx  = index(tmpStr(ibot:itop),tmpTok(1:lt))  ! set for good, if itop=n (<=MAXLEN)
    do while(idx == 0 .and. itop < n)
       i = i+1
       itop = min(i*MAXLEN-lt,n)      ! subtract lt to find tok at bdry
       ibot = max(1,itop+1-nlen)    ! bottom of string to index
!       idx  = index(string(ibot:itop),tok)   ! idx>=0, since itop-ibot<=MAXLEN
       idx = index(tmpStr(ibot:itop),tmpTok(1:lt))
    end do
    index_ = idx                    ! case where idx = 0, or (i=1 & idx > 0)
    if(idx > 0) index_ = idx - 1 + ibot
    return
  end function index_
  !-------------------------------------------------------------------------
  !EOC

  !====================================================================

  !                          --------------------------
  !                          Obsolete Inpak 77 Routines
  !                              (Not Documented)
  !                          --------------------------

  !...................................................................

  subroutine iniin()
    print *, 		&
         'i90: iniin() is obsolete, use i90_loadf() instead!'
    return
  end subroutine iniin


  !...................................................................

  subroutine iunits ( mifans, moftrm, moferr, miftrm )
    integer mifans, moftrm, moferr, miftrm 
    print *, 		&
         'i90: iunits() is obsolete, use i90_loadf() instead!'
    return
  end subroutine iunits

  !...................................................................

  subroutine getstr ( iret, string )
    character*(*) string 
    integer       iret  !, ls
    call i90_gstr ( string, iret )
    return
  end subroutine getstr

  !...................................................................

  subroutine getwrd ( iret, word )
    character*(*) word
    integer       iret
    call i90_gtoken ( word, iret )
    return
  end subroutine getwrd

  !...................................................................

  subroutine rdnext ( iret )
    integer iret
    call i90_gline ( iret )
    return
  end subroutine rdnext

  !...................................................................

  real function str2rn ( string, iret )
    character*(*) string
    integer iret

    string = ''
    read(string,*,end=11,err=11) str2rn
    iret = 0
    return
11  iret = 1
    return
  end function str2rn

  !...................................................................

  subroutine strget ( string, default )

    !-------------------------------------------------------------------------
    !         NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
    !-------------------------------------------------------------------------
    !
    ! !ROUTINE: StrGet()
    ! 
    ! !DESCRIPTION: 
    !
    !  Returns next string on the current line, or a default 
    !  string if it fails for whatever reason. Similar to {\tt i90\_gstr()}.
    !  Kept for Inpak 77 upward compatibility.
    !
    !  NOTE: This is an obsolete routine. The notion of "string" used
    !        here is not conventional. Please use routine {\tt TokGet()}
    !        instead.
    !
    ! !CALLING SEQUENCE: 
    !
    !      call strget ( string, default )
    !
    ! !INPUT PARAMETERS: 
    !
    character*(*) default      ! default string

    ! !OUTPUT PARAMETERS:

    character*(*) string       ! desired string

    !
    ! !REVISION HISTORY: 
    !
    !  19Jun96   da Silva   Original code.
    !  01Oct96   Jing Guo   Removed the null terminitor
    !
    !-------------------------------------------------------------------------

    integer iret

    iret = 0
    call i90_gstr ( string, iret )
    if ( iret .ne. 0 ) then
       string = default
    end if

    return
  end subroutine strget
!
! Funcoes de interface para o m_inpak90
! add 21 Fev 2014 - Joao Gerd

  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: GetReal_( )
  ! 
  ! !DESCRIPTION:  Returns next float number after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call GetReal_ ( label, vfloat, istat, lcase )
  !
  ! !INTERFACE: 
  !

  subroutine GetReal_( label, value, istat, lcase, default )

    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),  intent(in   ) :: label
    logical, optional, intent(in   ) :: lcase
    real, optional,    intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    real,              intent(  out) :: value
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( )  - selects a label (key)
    !    I90_gfloat( ) - returns next float number (function)
    !
    ! !REVISION HISTORY: 
    !  21 Feb 2014 - J. G. de Mattos - Initial Version
    !  07 Mar 2017 - J. G. de Mattos - add Sensitive Case option
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: GetReal_ (...)'
    integer            :: iret
    character(len=256) :: msg
    logical            :: SenCase

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then
       value = i90_gfloat(iret)
    else
       if(present(default))then
          value = default
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret

    return

  endsubroutine GetReal_
  !
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: AGetReal_( )
  ! 
  ! !DESCRIPTION:  Returns next array of floats numbers after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call AGetReal_ ( label, vfloat, istat, lcase )
  !
  ! !INTERFACE: 
  !

  subroutine AGetReal_( label, value, istat, lcase, default )
    implicit none

    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),  intent(in   ) :: label
    logical, optional, intent(in   ) :: lcase
    real, optional,    intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    real,              intent(inout) :: value(:)
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( )  - selects a label (key)
    !    I90_gfloat( ) - returns next float number (function)
    !
    ! !REVISION HISTORY: 
    !  21 Feb 2014 - J. G. de Mattos - Initial Version
    !  07 Mar 2017 - J. G. de Mattos - add Sensitive Case option
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: AGetReal_ (...)'
    character(len=256) :: msg
    integer            :: iret
    integer            :: i, n
    logical            :: SenCase


    n = size(value,dim=1)

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then

       do i=1,n
          value(i) = i90_gfloat(iret)
       enddo

    else
       if(present(default))then

          do i=1,n
             value(i) = default
          enddo
  
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret

    return


  endsubroutine AGetReal_
  !
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: GetInt_( )
  ! 
  ! !DESCRIPTION:  Returns next integer number after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call GetInt_ ( label, vfloat, istat, lcase )
  !
  ! !INTERFACE: 
  !

  subroutine GetInt_(label, value, istat, lcase, default)
    implicit none
    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),  intent(in   ) :: label
    logical, optional, intent(in   ) :: lcase
    integer, optional, intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    integer,           intent(  out) :: value
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( ) - selects a label (key)
    !    I90_Gint( )  - returns next integer number (function)
    !
    ! !REVISION HISTORY: 
    !  21 Feb 2014 - J. G. de Mattos - Initial Version
    !  07 Mar 2017 - J. G. de Mattos - add Sensitive Case option
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: GetInt_ (...)'

    character(len=256) :: msg
    integer            :: iret
    logical            :: SenCase

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then
       value = i90_gint(iret)
    else
       if(present(default))then
          value = default
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif
    
    if(present(istat)) istat = iret
    return

  endsubroutine GetInt_
  !
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: AGetInt_( )
  ! 
  ! !DESCRIPTION:  Returns next array of integers numbers after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call AGetInt_ ( label, vfloat, istat, lcase )
  !
  ! !INTERFACE: 
  !

  subroutine AGetInt_(label, value, istat, lcase, default)
    implicit none
    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),  intent(in   ) :: label
    logical, optional, intent(in   ) :: lcase
    integer, optional, intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    integer,           intent(inout) :: value(:)
    integer, optional, intent(  out) :: istat
    !
    ! !SEE ALSO:
    !
    !    I90_label( ) - selects a label (key)
    !    I90_Gint( )  - returns next integer number (function)
    !
    ! !REVISION HISTORY: 
    !  21 Feb 2014 - J. G. de Mattos - Initial Version
    !  07 Mar 2017 - J. G. de Mattos - add Sensitive Case option
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !

    character(len=*), parameter :: myname_= myname//' :: AGetInt_ (...)'

    character(len=256) :: msg
    integer            :: iret, irc
    integer            :: i, n
    logical            :: SenCase
 
    n    = size(value, dim=1)

    iret = 0
    irc  = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then
       do i=1,n
          value(i) = i90_gint(irc)
          iret = iret + irc
       enddo
    else
       if(present(default))then
          do i=1,n
             value(i) = default
          enddo
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret
    return

  endsubroutine AGetInt_
  !
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: GetLogic_( )
  ! 
  ! !DESCRIPTION:  Returns next logical flag after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call GetLogic_ ( label, value, istat, lcase, default )
  !
  ! !INTERFACE: 
  !

  subroutine GetLogic_( label, value, istat, lcase, default )

    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),  intent(in   ) :: label
    logical, optional, intent(in   ) :: lcase
    logical, optional, intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    logical,              intent(  out) :: value
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( )  - selects a label (key)
    !    I90_glogical( ) - returns next logical flag (function)
    !
    ! !REVISION HISTORY: 
    !  30 Oct 2019 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: GetReal_ (...)'
    integer            :: iret
    character(len=256) :: msg
    logical            :: SenCase

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then
       value = i90_glogical(iret)
    else
       if(present(default))then
          value = default
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret

    return

  endsubroutine GetLogic_
  !
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: AGetLogic_( )
  ! 
  ! !DESCRIPTION:  Returns next array of logical flags after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call AGetLogic_ ( label, value, istat, lcase, default )
  !
  ! !INTERFACE: 
  !

  subroutine AGetLogic_( label, value, istat, lcase, default )
    implicit none

    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),  intent(in   ) :: label
    logical, optional, intent(in   ) :: lcase
    logical, optional, intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    logical,           intent(inout) :: value(:)
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( )  - selects a label (key)
    !    I90_glogical( ) - returns next logical (function)
    !
    ! !REVISION HISTORY: 
    !  30 Oct 2019 - J. G. de Mattos - Initial Version
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: AGetReal_ (...)'
    character(len=256) :: msg
    integer            :: iret
    integer            :: i, n
    logical            :: SenCase


    n = size(value,dim=1)

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then

       do i=1,n
          value(i) = i90_glogical(iret)
       enddo

    else
       if(present(default))then

          do i=1,n
             value(i) = default
          enddo
  
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret

    return


  endsubroutine AGetLogic_
  !
  !EOC
  !

  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: GetToken_( )
  ! 
  ! !DESCRIPTION:  Returns next token after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call GetToken_ ( label, vtoken, istat, lcase )
  !
  ! !INTERFACE: 
  !

  subroutine GetToken_( label, value, istat, lcase, default)
    implicit none

    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),           intent(in   ) :: label
    logical,          optional, intent(in   ) :: lcase
    character(len=*), optional, intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    character(len=*),  intent(  out) :: value
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( )   - selects a label (key)
    !    I90_Gtoken( )  - get next token
    !
    ! !REVISION HISTORY: 
    !  21 Feb 2014 - J. G. de Mattos - Initial Version
    !  07 Mar 2017 - J. G. de Mattos - add Sensitive Case option
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: GetToken_ (...)'
    
    character(len=256) :: msg
    integer            :: iret
    logical            :: SenCase

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then
       call i90_Gtoken(value,iret)
    else
       if(present(default))then
          value=trim(default)
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret
    return

  endsubroutine GetToken_
  !
  !EOC
  !
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !ROUTINE: AGetToken_( )
  ! 
  ! !DESCRIPTION:  Returns next array of tokens after a string label(key).
  !
  !
  ! !CALLING SEQUENCE: 
  !
  !      call AGetToken_ ( label, vtoken, istat, lcase )
  !
  ! !INTERFACE: 
  !

  subroutine AGetToken_( label, value, istat, lcase, default)

    implicit none
    !
    ! !INPUT PARAMTERS:
    !

    character(len=*),           intent(in   ) :: label
    logical,          optional, intent(in   ) :: lcase
    character(len=*), optional, intent(in   ) :: default

    !
    ! !OUTPUT PARAMETERS:
    !

    character(len=*),  intent(inout) :: value(:)
    integer, optional, intent(  out) :: istat

    !
    ! !SEE ALSO:
    !
    !    I90_label( )   - selects a label (key)
    !    I90_Gtoken( )  - get next token
    !
    ! !REVISION HISTORY: 
    !  21 Feb 2014 - J. G. de Mattos - Initial Version
    !  07 Mar 2017 - J. G. de Mattos - add Sensitive Case option
    !
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    !
    character(len=*), parameter :: myname_= myname//' :: AGetToken_ (...)'

    character(len=256) :: msg
    integer            :: iret
    integer            :: i, n
    logical            :: SenCase
 
    n = size(value, dim=1)

    iret = 0

    SenCase = .false.
    if(present(lcase)) SenCase = lcase

    call i90_label(label, iret, SenCase)

    if (iret .eq. 0 )then
       do i=1,n
          call i90_Gtoken(value(i),iret)
       enddo
    else
       if(present(default))then
          do i=1,n
             value(i) = trim(default)
          enddo
       else
          write(msg,'(3A)')'i90_label("',trim(label),'")' 
          call i90_perr(myname_,trim(msg),iret)
       endif
    endif

    if(present(istat)) istat = iret
    return

  endsubroutine AGetToken_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr2_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine perr2_ (where,message,ier)

      character(len=*), intent(in) :: where
      character(len=*), intent(in) :: message
      integer,optional, intent(in) :: ier

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_= myname//' :: i90_perr( ... )'
  character(len=16) :: cer
  integer :: ios

  cer='*******'
  if(present(ier))then
     write(cer,'(i16)',iostat=ios) ier
     write(stderr,'(5a)') where,': ',&
     trim(message),' error, stat =',trim(adjustl(cer))
  else
      write(stderr,'(3a)') where,': ',trim(message)
  endif

end subroutine perr2_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr4_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine perr4_(where,mesg1,ival1,mesg2,ival2)

      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: mesg1
      integer,intent(in) :: ival1
      character(len=*),intent(in) :: mesg2
      integer,intent(in) :: ival2

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::perr4_'
  character(len=16) :: cval1,cval2
  integer :: ios

  cval1='*******'
  cval2='*******'
  write(cval1,'(i16)',iostat=ios) ival1
  write(cval2,'(i16)',iostat=ios) ival2

  write(stderr,'(10a)') where,': error, ',&
   mesg1,'=',trim(adjustl(cval1)),', ',   &
   mesg2,'=',trim(adjustl(cval2)),'.'

end subroutine perr4_

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: die2_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die0_ (where)

      character(len=*), intent(in) :: where

! !REVISION HISTORY:
! 	14May2020 - J.G. de Matttos - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_= myname//' :: die0_( ... )'
  character(len=64) :: msg


  msg = char(27)//'[31;1m'//trim(where)//char(27)//'[m'
  write(stdout,'(A)')trim(msg)

  stop 99035
  
end subroutine die0_



!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: die2_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die2_ (where,message,ier)

      character(len=*), intent(in) :: where
      character(len=*), intent(in) :: message
      integer,optional, intent(in) :: ier

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_= myname//' :: die2_( ... )'
  character(len=16) :: cer
  character(len=64) :: msg
  integer :: ios

  cer='*******'
  if(present(ier))then
     write(cer,'(i16)',iostat=ios) ier
     write(msg,'(5a)') where,':'//char(27)//'[31;1m Error:'//char(27)//'[m, ',&
     trim(message),', stat = ',char(27)//'[31;1m'//trim(adjustl(cer))//char(27)//'[m'
  else
      write(msg,'(3a)') where,':'//char(27)//'[31;1m Error:'//char(27)//'[m, '//trim(message)
  endif

  write(stdout,'(A)')trim(msg)

  stop 99036
  
end subroutine die2_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: perr4_ - send a simple error message to _stderr_
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine die4_(where,mesg1,ival1,mesg2,ival2)

      character(len=*),intent(in) :: where
      character(len=*),intent(in) :: mesg1
      integer,intent(in) :: ival1
      character(len=*),intent(in) :: mesg2
      integer,intent(in) :: ival2

! !REVISION HISTORY:
! 	27Apr98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::die4_(...)'
  character(len=16) :: cval1,cval2
  character(len=64) :: msg
  integer :: ios

  cval1='*******'
  cval2='*******'
  write(cval1,'(i16)',iostat=ios) ival1
  write(cval2,'(i16)',iostat=ios) ival2

  write(msg,'(10a)') where,':'//char(27)//'[31;1m Error:'//char(27)//'[m, ',&
   mesg1,'=',trim(adjustl(cval1)),', ',   &
   mesg2,'=',trim(adjustl(cval2)),'.'

  write(stdout,'(A)')trim(msg)
  stop 99038

end subroutine die4_

  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: opntext - portablly open a text file
!
! !DESCRIPTION:
!
!	Open a text (ASCII) file.  Under FORTRAN, it is defined as
!	"formatted" with "sequential" access.
!
! !INTERFACE:

    subroutine opntext(lu, fname, status,ier)
      implicit none

      integer,                    intent(in   ) :: lu     ! logical unit number
      character(len=*),           intent(in   ) :: fname  ! filename to be opended
      character(len=*), optional, intent(in   ) :: status ! the value for STATUS=<>
      integer,          optional, intent(  out) :: ier    ! the status


! !REVISION HISTORY:
!
!	02Feb95 - Jing G. - First version included in PSAS and libpsas.a
!  
!EOP
!_______________________________________________________________________

		! local parameter
	character(len=*),parameter :: myname_=myname//'::opntext'
   character(len=10)          :: pos
   character(len=MaxLineSize) :: lst



   if(present(status))then
      lst = lower_case(status)
      if(lst.eq.'append')then
         lst = 'unknown'
         pos = 'append'
      else
         lst = trim(status)
         pos = 'asis'
      endif
   else
      lst = 'unknown'
      pos = 'asis'
   endif

   open(				              &
        unit     = lu,          &
        file     = trim(fname), &
        form     = 'formatted', &
        access   = 'sequential',&
        status   = trim(lst),   &
        position = trim(pos),   &
        iostat   = ier          &
       )

	end subroutine opntext

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: clstext - close a text file opend with an opntext() call
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine clstext(lu,ier,status)
      implicit none

      integer,                    intent(in)  :: lu     ! a logical unit to close
      integer,                    intent(out) :: ier    ! the status
      Character(len=*), optional, intent(In)  :: status ! keep/delete

! !REVISION HISTORY:
! 	09Oct96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
      character(len=*), parameter :: myname_ = myname//'::clsitext'
      Character(Len=6) :: status_
      character(len=MaxLineSize)  :: lst


      status_ = 'KEEP'

      If (present(status)) Then

         lst = lower_case(status)

         Select Case (trim(lst))

         Case ('delete')
            status_ = 'DELETE'
         Case  ('keep')
            status_ = 'KEEP'
         Case Default
            ier = -997
            return
         End Select

      End If

	   close(lu,iostat=ier,status=status_)

	end subroutine clstext

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: lower_case - convert uppercase letters to lowercase.
!
! !DESCRIPTION:
!
! !INTERFACE:

  function lower_case(str) result(lstr)
    implicit none
    character(len=*), intent(in) :: str
    character(len=len(str))      :: lstr

! !REVISION HISTORY:
! 	13Aug96 - J. Guo	- (to do)
!EOP
!_______________________________________________________________________
    integer i
    integer,parameter :: iu2l=ichar('a')-ichar('A')

    lstr=str
    do i=1,len_trim(str)
      if(str(i:i).ge.'A'.and.str(i:i).le.'Z')	&
      	lstr(i:i)=char(ichar(str(i:i))+iu2l)
    end do
  end function lower_case

end module m_inpak90
