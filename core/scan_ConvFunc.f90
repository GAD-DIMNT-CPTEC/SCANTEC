!-----------------------------------------------------------------------------!
!           Group on Data Assimilation Development - GDAD/CPTEC/INPE          !
!-----------------------------------------------------------------------------!
!BOP
!
! !MODULE: scan_MetForm.f90
!
! !DESCRIPTON: Este modulo tem por proposito apresentar rotinas e funcoes para 
!              a conversão de variaveis meteorologicas.
!                 
!\\
!\\
! !INTERFACE:
!

Module scan_ConvFunc
  use m_string, only: split
  implicit none
  private

  public :: convfunc

  type convfunc
     integer :: nargs

     type(arg), pointer :: args => null()
   contains
     procedure :: gArgs => arg_0d, arg_1d, arg_2d
     procedure :: parse

  end type convfunc

  type arg
     character(len=10), pointer :: type_ => null()
     real,      pointer :: r0d      => null()
     real,      pointer :: r1d(:)   => null()
     real,      pointer :: r2d(:,:) => null()

     integer,   pointer :: i0d      => null() 
     integer,   pointer :: i1d(:)   => null() 
     integer,   pointer :: i2d(:,:) => null()

     logical,   pointer :: l0d      => null()
     logical,   pointer :: l1d(:)   => null()
     logical,   pointer :: l2d(:,:) => null()

     type(arg), pointer :: next => null()
  end type arg


  character(len=1024),parameter :: myname='scan_convFunc'


  !
  ! !REVISION HISTORY:
  !  03 jun 2020 - J. G. de Mattos - Initial Version
  !
  ! !SEE ALSO:
  !   
  ! !BUGS:
  !
  !  Nenhum conhecido
  !
  !EOP
  !-----------------------------------------------------------------------------!

Contains

  !  function init_(self) result(iret)
  !  end function init_

  function arg_0d(self,val) result (iret)
    class(convfunc) :: self
    class(*)        :: val
    integer         :: iret

    iret = 0

    !    if(.not.is_empty(self)) iret = self%arg(r0d)
    select type(val)
    type is (real)
       allocate(self%args%r0d)
       self%args%r0d = val
    type is (integer)
       allocate(self%args%i0d)
       self%args%i0d = val
    type is (logical)
       self%args%l0d = val
    end select
  end function arg_0d

  function arg_1d(self,val) result (iret)
    class(convfunc) :: self
    class(*)        :: val(:)
    integer         :: iret

    iret = 0
    !    if(.not.is_empty(self)) iret = self%arg(r0d)

    select type(val)
    type is (real)
       allocate(self%args%r1d(size(val)))
       self%args%r1d = val
    type is (integer)
       allocate(self%args%i1d(size(val)))
       self%args%i1d = val
    type is (logical)
       allocate(self%args%i1d(size(val,1)))
       self%args%l1d = val

    end select

  end function arg_1d

  function arg_2d(self,val) result (iret)
    class(convfunc) :: self
    class(*)        :: val(:,:)
    integer         :: iret

    iret = 0
    !    if(.not.is_empty(self)) iret = self%arg(r0d)

    select type(val)
    type is (real)
       allocate(self%args%r2d(size(val,1),size(val,2)))
       self%args%r2d = val
    type is (integer)
       allocate(self%args%i2d(size(val,1),size(val,2)))
       self%args%i2d = val
    type is (logical)
       allocate(self%args%l2d(size(val,1),size(val,2)))
       self%args%l2d = val

    end select

  end function arg_2d

  subroutine parse(self,strg)
     class(convfunc)  :: self
     character(len=*) :: strg

     
     integer :: ntokens
     character(len=512), allocatable :: tokens(:)
     
     integer :: i, j, k

     call split(strg, ntokens, tokens)

     do i=1,ntokens
        print*, i, trim(tokens(i))
     enddo
     
  end subroutine


End Module scan_ConvFunc
