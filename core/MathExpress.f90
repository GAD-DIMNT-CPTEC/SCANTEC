!-----------------------------------------------------------------------------!
!             Modeling and Development Division - DMD/CPTEC/INPE              !
!-----------------------------------------------------------------------------!
!BOP
! !MODULE: MathExpression - a module to process and evaluate 
!                           evaluate mathematical expression
!                           from a string
!
! !DESCRIPTION: This module constains routines to parsing a
!               mathematical expression in infix notation 
!               from a string and return the value as result.
!
! !INTERFACE:
!
module MathExpress
  use varType
  implicit none
  private
  ! !PUBLIC MEMBER FUNCTIONS:

!  public :: infix2postfix ! convert from infix notation to postfix notation
!  public :: evalPostfix   ! evaluate a equation in postfix notation
  public :: tokenize
  public :: getType
!  public :: indx
  public :: MathOper
  public :: tfunc1
  public :: tfunc2
  public :: tfunc3

  ! !REVISION HISTORY:
  !  
  ! 19 Jun 2020 - J. G. de Mattos - Initial version
  ! 12 Aug 2020 - J. G. de Mattos - Include some type bound
  !                                 methods to better process
  !                                 mathematical operators and
  !                                 functions
  ! 14 Aug 2020 - J. G. de Mattos - Include mathemathical expressions
  !                                 sin, cos, tan, asin, acos, atan, 
  !                                 abs, exp, log, log10, min, max
  !
  ! 19 Aug 2020 - J. G. de Mattos - Include polymorphism to include 
  !                                 external functions
  ! !BUGS:
  !   Not yet!
  !
  !
  ! !SEE ALSO:
  !
  !  https://www.free-online-calculator-use.com/infix-to-postfix-converter.html
  ! 
  !EOP 
  !-----------------------------------------------------------------------------!
  !
  character(len=*), parameter :: myname = 'MathExpression'

  character, parameter :: BLK = char(32)

  type MathOper
     class(operInfo), pointer :: head => null()
     class(operInfo), pointer :: tail => null()
     integer :: nOperator
     contains
        private
        procedure         :: evalPostfixS_, evalPostfixA_
        generic,   public :: evalPostfix => evalPostfixS_, evalPostfixA_
        procedure, public :: Infix2Postfix
        procedure, public :: register => register_
        procedure, public :: printOper => print_
        procedure, public :: initialize => init_

        procedure :: isOper => isOper_
        procedure :: associativity => associativity_
        procedure :: precedence => precedence_
        procedure :: getOper => getOper_
  endtype
  
  type , abstract :: operInfo     
     character(len=10)    :: oper   ! operator
     character(len=2)     :: assoc  ! associativity
     integer              :: preced ! precedence
!     integer              :: nop    ! number of operands
     class(operInfo), pointer :: next => null()
  end type operInfo

  type, extends(operinfo) :: tfunc0
  end type tfunc0

  type, extends(operinfo) :: tfunc1
    ! shall be associated.
    procedure(ifunc1), pointer, nopass :: func
  end type tfunc1
  
  type, extends(operinfo) :: tfunc2
    ! shall be associated.
    procedure(ifunc2), pointer, nopass :: func
  end type tfunc2

  type, extends(operinfo) :: tfunc3
    ! shall be associated.
    procedure(ifunc3), pointer, nopass :: func
  end type tfunc3


  interface tfunc1
     procedure :: ctfunc1
  end interface
  interface tfunc2
     procedure :: ctfunc2
  end interface
  interface tfunc3
     procedure :: ctfunc3
  end interface

  interface
     pure function ifunc1(x) result(result_value)
        real, intent(in) :: x
        real :: result_value
     end function
     pure function ifunc2(x,y) result(result_value)
        real, intent(in) :: x, y
        real :: result_value
     end function
     pure function ifunc3(x,y,z) result(result_value)
        real, intent(in) :: x, y, z
        real :: result_value
     end function

  end interface 


contains
  type(tfunc1) function ctfunc1( o, a, p, f)
    character(len=*), intent(in) :: o
    character(len=*), intent(in) :: a
    integer,          intent(in) :: p
    procedure(ifunc1), optional  :: f

    ctfunc1%oper   = o
    ctfunc1%assoc  = a
    ctfunc1%preced = p
    ctfunc1%func   => f
  end function ctfunc1


  type(tfunc2) function ctfunc2( o, a, p, f)
    character(len=*), intent(in) :: o
    character(len=*), intent(in) :: a
    integer,          intent(in) :: p
    procedure(ifunc2), optional  :: f

    ctfunc2%oper   = o
    ctfunc2%assoc  = a
    ctfunc2%preced = p
    ctfunc2%func   => f
  end function ctfunc2

  type(tfunc3) function ctfunc3( o, a, p, f)
    character(len=*), intent(in) :: o
    character(len=*), intent(in) :: a
    integer,          intent(in) :: p
    procedure(ifunc3), optional  :: f

    ctfunc3%oper   = o
    ctfunc3%assoc  = a
    ctfunc3%preced = p
    ctfunc3%func   => f
  end function ctfunc3

  
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE: infix2postfix
  !
  ! !DESCRIPTION: This routine convert a mathematical infix expression to
  !               a postfix expression (A.K.A., Reverse Polish Notation, 
  !               or RPN) using the stack method.
  !
  !               An Infix Expression (or Infix Notation) is characterized 
  !               by a math expression wherein the operators are placed 
  !               between operands, (as in 2 + 3).
  !
  !               In the case of infix expressions, parentheses (or brackets) 
  !               must be used to indicate the order in which the author 
  !               wants the operations to be executed. Otherwise, the person
  !               solving the problem is left to apply an order of operation 
  !               convention (PEMDAS, BODMAS, etc.) to solve the expression.
  !
  !               As the name implies, a Postfix Expression (or Postfix 
  !               Notation, or Reverse Polish Notation) is characterized by a
  !               math expression wherein the operators are placed after their
  !               operands (2 + 3 infix becomes 2 3 + postfix).
  !
  !               Since each postfix operator is evaluated from left to right, 
  !               this eliminates the need for parenthesis. This is why postfix
  !               expressions are easier for computer algorithms to evaluate 
  !               than infix expression
  !
  !      Instuctions to use:
  !          Enter a mathematical expression in the infix notation
  !
  !          When entering an infix expression, use only the following
  !          list of valid non-numeric characters:
  !
  !             # ^ for exponents (enter as 3^4)
  !             # * for multiplication
  !             # / for division 
  !             # + for addition
  !             # - for subtraction
  !
  !             For entering numeric characters, be sure to insert a zero 
  !             before any number that begins with a decimal point 
  !             (for .5, enter 0.5) and negative numeric characters enter 
  !             between parenteses (enter as (-3) )
  !
  ! !INTERFACE:
  !
  subroutine infix2postfix(self, str, postfix)
    !
    ! !INPUT PARAMETERS:
    !
    class(MathOper),  intent(inout) :: self
    character(len=*), intent(in   ) :: str ! infix mathematical expression
    !
    ! !OUTPUT PARAMETERS:
    !
    character(len=*), allocatable, intent(out) :: postfix(:)
    !
    ! !REVISION HISTORY: 
    !  18 Jun 2020 - J. G. de Mattos - Initial Version
    !
    ! !BUGS:
    !   Not yet!
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    character(len=*), parameter :: myname_ = trim(myname)//':: infix2postfix(...)'

    integer :: i, j, k, l, m, n, o, ii
    integer :: ierr
    integer :: idx
    integer :: top
    integer :: ntokens
    integer :: OperP
    integer :: stackP
    logical :: isOper
    character(len=2)  :: assoc
    character(len=50) :: MathOper
    character(len=32), allocatable :: output(:)
    character(len=32), allocatable :: stack(:)
    character(len=32), allocatable :: token(:)

    ! initialize mathematical operators

    if (.not.associated(self%head))then
       print*,trim(myname_)//' :: error : not initialized ...'
       stop
!       call self%initialize()
    endif

    ! put mathematical operators in a string
!    MathOper = BLK
!    do i=1,size(symbol)
!       MathOper = trim(MathOper)//trim(symbol(i)%o)
!    enddo

    !split infix expression in to tokens
!    call split(str, ntokens, token, MathOper)
    call tokenize(str, ntokens, token)

    allocate(output(ntokens)); output = BLK
    allocate(stack(ntokens)); stack = BLK

    j = 0
    top = 0
    !scan the infix expression from left to right
    do i = 1, ntokens
       isOper = self%isOPer(trim(token(i)))
       ! If the token is an operand, apprend it to output.
       if (.not.isOper) then
          j = j + 1
          output(j) = trim(token(i))
       else if (isOper .and. trim(token(i)).eq.',')then
          do n = top, 1, -1
             if ( trim(stack(n)) .eq. '(') exit
             j = j + 1
             output(j) = trim(stack(n))
             stack(n)  = BLK
             top = top - 1
          enddo
          ! If the token is an opening parenthesis, push it to the stack.
          ! This marks the beginning of an expression that should be 
          ! evaluated separately.
       else if (isOper .and. trim(token(i)).eq.'(')then
          top = top + 1
          stack(top) = trim(token(i))

          !If the token is a closing parenthesis, 
       else if (isOper .and. trim(token(i)).eq.')')then
          ! until an opening parenthesis is encontered at the top
          ! of the stack, pop each operator from the stack and append
          ! to the output expression. This marks the end of the 
          ! operands and operators located within the current set of
          ! parentheses.
          do n = top, 1, -1
             if ( trim(stack(n)) .eq. '(')then
                stack(n) = BLK
                top = top - 1
                exit
             endif
             j = j + 1
             output(j) = trim(stack(n))
             stack(n)  = BLK
             top = top - 1
          enddo
          ! If the token is an operator and stack is empty,
          ! push operator into stack
       else if (isOper .and. top.eq.0)then
          top = top + 1
          stack(top) = trim(token(i))
          ! If is an operator and the stack is not empty, there 
          ! may be following possibilites.
       else if (isOper .and. top.ne.0)then
          ! If the token operator has greater precendence than 
          ! the top most operator of stack, push this token 
          ! operator into stack.

          OperP  = self%precedence(trim(token(i)))
          StackP = self%precedence(trim(stack(top)))

          if(OperP .gt. StackP)then
             top = top + 1
             stack (top) = trim(token(i))
             ! If the token operator and it has lower precedence than 
             ! the top most operator of the stack
          else if (OperP .lt. stackP)then

             ! pop the operator from stack
             j = j + 1
             output(j) = trim(stack(top))
             stack(top)= BLK
             top = top - 1

             ! pop the operator from stack until we find a low
             ! precedence operator than the token or stack is
             ! empty
             do n = top, 1, -1
!                o =  index(trim(MathOper),trim(stack(n)))
!                if ( symbol(o)%p.lt.symbol(idx)%p) exit
                stackP = self%precedence(stack(n))
                if ( stackP.lt.OperP) exit
                j = j + 1
                output(j) = trim(stack(n))
                stack(n)  = BLK
                top = top - 1
             enddo
             top = top + 1
             stack(top) = trim(token(i))

             ! If the token is an operator and it has the same 
             ! precedence as the operator on the top of the stack
!          else if (symbol(idx)%p .eq. symbol(m)%p)then
          else if (OperP .eq. stackP)then
             ! and the operator on top of the stack is 
             ! left-to-right associative
             assoc = self%associativity(stack(top))
!             if (symbol(m)%a .eq. 'lr')then
             if (assoc .eq. 'lr')then
                ! until the operator on top of the stack has lower 
                ! precedence than the token, or until the stack is 
                ! empty, pop each operator from the stack and append 
                ! them to the output. Then push the current token to
                ! the stack.
                do n = top, 1, -1
!                   o =  index(trim(MathOper),trim(stack(n)))
!                   if ( symbol(o)%p.lt.symbol(idx)%p ) exit
                   stackP = self%precedence(stack(n))
                   if ( stackP.lt.OperP) exit
                   j = j + 1
                   output(j) = trim(stack(n))
                   stack(n)  = BLK
                   top = top - 1
                enddo
                top = top + 1
                stack(top) = trim(token(i))
                ! and the operator on top of the stack is right-to-left 
                ! associative, push the current token to the stack. 
!             elseif (symbol(m)%a .eq. 'rl')then
             elseif (assoc .eq. 'rl')then
                top = top + 1
                stack(top) = trim(token(i))
             endif
          endif
       endif
    enddo
    ! all token was evaluated

    ! pop the remaining operator from 
    ! top of the stack and append it to
    ! output
    if (top.gt.0)then
       do n=top,1,-1
          j=j+1
          output(j) = trim(stack(n))
          stack(n)  = BLK
       enddo
    endif
    deallocate(stack)

    ! remove empty elements
    ! from array
    allocate(postfix(j))
    do i=1,j
       postfix(i) = output(i)
    enddo

  end subroutine infix2postfix
  !EOC
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE: evalPostFixS_ - evaluate a postfix expression (Reverse Polish 
  !                           Notation)
  !
  ! !DESCRIPTION: This routine evaluate a mathematical expression in the postfix
  !               notation (Reverse Polish Notation) using the stack method.
  !
  !               When evaluating postfix expressions, using a stack to temporarily 
  !               store operands is necessary because as we are evaluating each 
  !               character of the postfix expression from left to right, we can't 
  !               instantly know an operator's right-hand operand. Therefore we need
  !               to temporarily add (push) operands to the stack and only remove 
  !               (pop) them from the stack once we know an operator's right operand.
  !
  !
  ! !INTERFACE:
  !
  subroutine evalPostFixS_(self, MathExpress, val)!, vars)

    !
    ! !INPUT PARAMETER:
    !
    class(MathOper),  intent(inout) :: self
    character(len=*), intent(in   ) :: MathExpress(:)
!    class(varInfo), optional        :: vars
    ! 
    ! !OUTPUT PARAMETERS:
    !
    real, intent(out) :: val
    !
    ! !REVISION HISTORY: 
    !  18 Jun 2020 - J. G. de Mattos - Initial Version
    !
    ! !BUGS:
    !   Not yet!
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    character(len=*), parameter :: myname_ = trim(myname)//':: evalPostFix(...)'

    integer :: i
    integer :: top
    integer :: ntokens
    real    :: opd1, opd2, opd3
    logical :: isOper
    real, allocatable :: stack(:)
    class(operInfo), pointer :: oper => null()

    ! initialize mathematical operators

    if (.not.associated(self%head))then
       print*,trim(myname_)//' :: error : not initialized ...'
       stop
!       call self%initialize()
    endif
   
    ntokens = size(MathExpress)

    allocate(stack(ntokens)); stack = 0.0
    top = 0
    ! Moving from left to right, one character at a time. 
    do i=1,ntokens
       isOper = self%isOper(trim(MathExpress(i)))

       if(.not.isOper)then
          ! if a character is an operand (number), 
          ! push it to the top of the stack
          top = top + 1
          read(MathExpress(i),*)stack(top)

       else
          ! if a character is an operator (^ * / + -), pop (remove)
          ! the top element from the stack to form the operator's 
          ! right operand, and then pop the next top element from 
          ! the stack to form the operator's left operand. Finally, 
          ! solve the expression formed by the operator and its 
          ! operands, and push the result to the top of the stack. 
          oper => self%getOper(trim(MathExpress(i)))

          select type(oper)

             class is (tfunc1)
                opd1 = stack(top)
                stack(top) = oper%func(opd1)

             class is (tfunc2)
                opd2 = stack(top)
                top  = top - 1
                opd1 = stack(top)
                stack(top) = oper%func(opd1, opd2)

             class is (tfunc3)
                opd3 = stack(top)
                top  = top - 1
                opd2 = stack(top)
                top  = top - 1
                opd1 = stack(top)
                stack(top) = oper%func(opd1, opd2, opd3)
                
          end select

        endif
    enddo
    ! The last element remaining in the stack becomes the result.
    val = stack(top)

  end subroutine evalPostfixS_

  !EOC
  !-----------------------------------------------------------------------------!
  !             Modeling and Development Division - DMD/CPTEC/INPE              !
  !-----------------------------------------------------------------------------!
  !BOP
  !
  ! !IROUTINE: evalPostFixA_ - evaluate a postfix expression (Reverse Polish 
  !                            Notation). This routine return an array
  !
  ! !DESCRIPTION: This routine evaluate a mathematical expression in the postfix
  !               notation (Reverse Polish Notation) using the stack method.
  !
  !               When evaluating postfix expressions, using a stack to temporarily 
  !               store operands is necessary because as we are evaluating each 
  !               character of the postfix expression from left to right, we can't 
  !               instantly know an operator's right-hand operand. Therefore we need
  !               to temporarily add (push) operands to the stack and only remove 
  !               (pop) them from the stack once we know an operator's right operand.
  !
  !
  ! !INTERFACE:
  !
  subroutine evalPostfixA_(self, MathExpress, val, vars, undef)

    !
    ! !INPUT PARAMETER:
    !
    class(MathOper),           intent(inout) :: self
    character(len=*),          intent(in   ) :: MathExpress(:)
    class(variable), optional, intent(in   ) :: vars(:)
    real,            optional, intent(in   ) :: undef
    ! 
    ! !OUTPUT PARAMETERS:
    !
    real, intent(out) :: val(:)
    !
    ! !REVISION HISTORY: 
    !  18 Jun 2020 - J. G. de Mattos - Initial Version
    !
    ! !BUGS:
    !   Not yet!
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC
    character(len=*), parameter :: myname_ = trim(myname)//':: evalPostFix(...)'

    integer :: i, j
    integer :: top
    integer :: ntokens
    real, pointer  :: opd1(:), opd2(:), opd3(:)
    real, pointer  :: result_(:)
    logical :: isOper
    integer :: isVar
    integer :: isize
    class(operInfo), pointer :: oper => null()

    class(variable), allocatable :: stack(:)
    real :: operand

    ! initialize mathematical operators

    if (.not.associated(self%head))then
       print*,trim(myname_)//' :: error : not initialized ...'
       stop
!       call self%initialize()
    endif
   
    isize = size(val)

    ntokens = size(MathExpress)

    allocate(stack(ntokens))!; stack = 0.0
    top = 0
    ! Moving from left to right, one character at a time. 
    do i=1,ntokens
       isOper = self%isOper(trim(MathExpress(i)))

       if(.not.isOper)then
          ! if a character is an operand (number), 
          ! push it to the top of the stack

          top = top + 1
          
          ! verify if is a variable
          isVar = 0
          do j=1,size(vars)
            if (trim(MathExpress(i)) .eq. trim(vars(j)%v%name_))then
               isVar = j
               stack(top) = vars(j)
               exit
            endif
          enddo
          if (isVar .eq. 0)then
             read(MathExpress(i),*)operand
             call stack(top)%put('oper',operand)
          endif

       else
          ! if a character is an operator (^ * / + -), pop (remove)
          ! the top element from the stack to form the operator's 
          ! right operand, and then pop the next top element from 
          ! the stack to form the operator's left operand. Finally, 
          ! solve the expression formed by the operator and its 
          ! operands, and push the result to the top of the stack.
          oper => self%getOper(trim(MathExpress(i)))

          select type(oper)

             class is (tfunc1)

                allocate(opd1(isize))
                call stack(top)%get(opd1)

                allocate(result_(isize))

                if (present(undef))then
                   do j=1,isize
                      if (opd1(j).eq.undef)then
                         result_(j) = undef
                      else
                         result_(j) = oper%func(opd1(j))
                      endif
                   enddo
                else
                   do j=1,isize
                      result_(j) = oper%func(opd1(j))
                   enddo
                endif

                call stack(top)%put('func', result_)

                deallocate(opd1)
                deallocate(result_)

             class is (tfunc2)
                
                allocate(opd2(isize))
                call stack(top)%get(opd2)
                top  = top - 1
                
                allocate(opd1(isize))
                call stack(top)%get(opd1)

                allocate(result_(isize))

                if (present(undef))then
                   do j=1,isize
                      if (opd1(j).eq.undef.or.opd2(j).eq.undef)then
                         result_(j) = undef
                      else
                         result_(j) = oper%func(opd1(j), opd2(j))
                      endif
                   enddo
                else
                   do j=1,isize
                      result_(j) = oper%func(opd1(j), opd2(j))
                   enddo
                endif
                call stack(top)%put('func', result_)

                deallocate(opd2)
                deallocate(opd1)
                deallocate(result_)

             class is (tfunc3)

                allocate(opd3(isize))
                call stack(top)%get(opd3)
                top  = top - 1

                allocate(opd2(isize))
                call stack(top)%get(opd2)
                top  = top - 1

                allocate(opd1(isize))
                call stack(top)%get(opd1)

                allocate(result_(isize))
                if (present(undef))then
                   do j=1,isize
                      if (opd1(j).eq.undef.or.opd2(j).eq.undef.or.opd3(j).eq.undef)then
                         result_(j) = undef
                      else
                         result_(j) = oper%func(opd1(j), opd2(j), opd3(j))
                      endif
                   enddo
                else
                   do j=1,isize
                      result_(j) = oper%func(opd1(j), opd2(j), opd3(j))
                   enddo
                endif
                call stack(top)%put('func', result_)

                deallocate(opd3)
                deallocate(opd2)
                deallocate(opd1)
                deallocate(result_)
                
          end select

        endif
    enddo
    ! The last element remaining in the stack becomes the result.
    call stack(top)%get(val)

  end subroutine evalPostfixA_

  !EOC
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
    !   18 Jun 2020 - J. G. de Mattos -  adpat to split
    !                                    mathematical expressions
    !                                    that will be used to
    !                                    convert to postfix notation
    !
    !
    ! !TODO:
    !     Some corrections on mathematical expression
    !    
    !     If a number (numeric operand) immediately precedes
    !     a left parenthesis (indicating multiplication), the
    !     calculator need attempt to insert a multiplication 
    !     operator between the number and the left parenthesis 
    !     (convert 2(2+1) to 2*(2+1) ).
    !     (convert (2+1)(3+2) to (2+1)*(3+2)
    !
    !EOP
    !-----------------------------------------------------------------------------!
    !BOC

    character(len=48)    :: delimiter
    integer              :: i, j, k
    integer              :: idx
    integer              :: StrLen

    ! linked list to store temporary tokens
    type token
       character(len=10)    :: tk
       type(token), pointer :: next => null()
       type(token), pointer :: prev => null()
    endtype token
    type(token), pointer :: root => null()
    type(token), pointer :: current => null()
    type(token), pointer :: tmp => null()

    character(len=10), pointer :: p => null()
    character(len=10), pointer :: c => null()
    character(len=10), pointer :: n => null()

    ! setting up delimter
    delimiter = BLK
    if (present(del)) delimiter = del

    ! get string length
    StrLen = len_trim(str)

    ! find tokens using delimiter
    allocate (root)
    current => root
    current%tk = BLK
    do i = 1, StrLen

       idx = indx(trim(delimiter),str(i:i))

       if (idx.eq.0)then
          current%tk = trim(current%tk)//str(i:i)
       else
          !----------------------------------------------!
          ! deal with negative values
          ! negative values should be 
          ! between parenteses
          j = indx('+-',str(i:i))
          if( j .ne. 0 .and. i .gt. 0)then
             k = i-1
             if(str(k:k) .eq. '(')then
                current%tk = trim(current%tk)//str(i:i)
                cycle
             endif
          endif
          !
          !----------------------------------------------!
          allocate(tmp)
          tmp%tk = str(i:i)
          tmp%prev => current
          current%next => tmp
          current => current%next

          allocate(tmp)
          tmp%tk = BLK
          tmp%prev => current
          current%next => tmp
          current => current%next
       endif

    enddo

    !remove empty tokens
    current => root
    ntokens = 0
    do while(associated(current))

       if (trim(current%tk) .eq. BLK)then

          if (associated(current%prev))then
             tmp => current%prev
             tmp%next => current%next
             deallocate(current)
             current => tmp
             current => current%next
          else ! if blk is at fist element
             root => current%next
             deallocate(current)
             current => root
          endif

       else
          ntokens = ntokens + 1
          current => current%next
       endif

    enddo


    !copy tokens to output array and
    !deallocate temporary token linked list
    allocate (tokens(ntokens))
    current => root
    do i = 1, ntokens
       tokens(i)  = trim(current%tk)
       current => current%next
       deallocate (root)
       root => current
    enddo

  end subroutine split
  !EOC
  !-----------------------------------------------------------------------------!
  subroutine tokenize(str,ntokens, tokens)
   character(len=*),              intent(in   ) :: str
   integer,                       intent(  out) :: ntokens
   character(len=*), allocatable, intent(  out) :: tokens(:)


   integer          :: i
   integer          :: lenStr
   character(len=3) :: stype

   type token
      character(len=50) :: tok_
      character(len=3)  :: type_
      type(token), pointer :: next =>  null()
   end type
   type(token), pointer :: tkns=> null()
   type(token), pointer :: newToken => null()
   type(token), pointer :: root => null()

   lenStr = len_trim(str)

   ! initialize
   allocate(root)
   tkns => root

   ! if the first character is a
   ! minus signal so we have a
   ! number. This is a special
   ! case. By this we read the first
   ! character outsid loop
   tkns%tok_ = str(1:1)
   if (str(1:1) .eq. '-')then
      tkns%type_ = 'num'
   else
      tkns%type_ = getType(str(1:1))
   endif
   ntokens = 1

   !read next characters at str
   do i=2, lenStr
       stype = getType(str(i:i))

!        if(.not.associated(root))then
!           allocate(root)
!           tkns => root
!          
!           ntokens   = ntokens + 1
!           tkns%tok_ = str(i:i)
!
!           if(str(i:i) .eq. '-')then
!              tkns%type_ = 'num'
!           else
!              tkns%type_ = stype
!           endif
!
!           cycle
!       endif
!       print*, str(i:i),' ',stype,' ', tkns%type_
       if (stype.eq.tkns%type_ .and. stype .ne.'sym')then
          tkns%tok_  = trim(tkns%tok_)//str(i:i)
       else if (tkns%type_ .eq. 'num' .and. str(i:i).eq.'.')then
          tkns%tok_  = trim(tkns%tok_)//str(i:i)
       else if (tkns%type_ .eq. 'cha' .and. stype .eq.'num')then
          tkns%tok_  = trim(tkns%tok_)//str(i:i)
       else if (tkns%type_ .eq. 'cha' .and. str(i:i).eq.':')then
          tkns%tok_  = trim(tkns%tok_)//str(i:i)
       else if (tkns%type_ .eq. 'cha' .and. str(i:i).eq.'_')then
          tkns%tok_  = trim(tkns%tok_)//str(i:i)
       else if (tkns%type_.eq.'sym' .and. tkns%tok_.ne.')' .and. str(i:i).eq.'-')then
          
          ntokens = ntokens + 1

          allocate(newToken)
          newToken%tok_  = str(i:i)
          newToken%type_ = 'num'

          tkns%next => newToken
          tkns => tkns%next

       else
          ntokens = ntokens + 1

          allocate(newToken)
          newToken%tok_  = str(i:i)
          newToken%type_ = stype

          tkns%next => newToken
          tkns => tkns%next
       endif

   enddo

   !copy tokens to output array and
   !deallocate temporary token linked list
   allocate(tokens(ntokens))
   tkns => root
   do i=1,ntokens
      tokens(i) = trim(tkns%tok_)
      tkns => tkns%next
      deallocate(root)
      root => tkns
   enddo


  end subroutine

  function getType (str) result (answer)
     character(len=1) :: str
     character(len=3) :: answer

     if(ischar(str))then
        answer = 'cha'
     else if (isnum(str))then
        answer = 'num'
     else
        answer = 'sym'
     endif
  end function

  function isChar(str) result(answer)
     character(len=1) :: str
     logical          :: answer

     answer = .false.
     if((str >= 'a' .and. str <= 'z') .or. &
        (str >= 'A' .and. str <= 'Z'))answer = .true.

     return
  end function

  function isNum(str) result(answer)
     character(len=1) :: str
     logical          :: answer

     answer = .false.
     if((str>='0' .and. str <= '9'))answer = .true.

     return
  end function

  function isSym(str) result(answer)
     character(len=1) :: str
     logical          :: answer

     answer = .false.
     if(.not.isChar(str) .and. .not.isNum(str))answer = .true.

     return
  end function
  

  function indx(str,substr) result(idx)
    character(len=*), intent(in) :: str
    character(len=*), intent(in) :: substr
    integer                      :: idx

    integer :: i
    integer :: lenStr
    integer :: lenSub
    integer :: endStr

    lenStr = len_trim(str)
    lenSub = len_trim(substr)

    idx = 0
    do i=1,lenStr
       endStr = min(i+lenSub-1,lenStr)
       if(str(i:endStr) .eq. trim(substr)) then
          idx = i
          return
       endif
    enddo

  end function indx

  function isOper_(self,str) result(r)
     class(MathOper)  :: self
     character(len=*) :: str
     logical          :: r

     class(operInfo), pointer :: symb => null()
     integer :: i

     r = .false.
     symb => self%head
     do i=1,self%nOperator
        if(trim(str) .eq. trim(symb%oper)) r = .true.
        symb => symb%next
     enddo
  endfunction

  subroutine register_(self, op_object)
     class(MathOper), intent(inout) :: self
     class(operInfo), pointer, intent(in   ) :: op_object

     if(.not.associated(self%head))then
        self%head => op_object
        self%tail => op_object
        self%nOperator = 1
     else
        self%tail%next => op_object
        self%tail => self%tail%next
        self%nOperator = self%nOperator + 1
     endif

     
  end subroutine

  subroutine init_(self)
     class(MathOper), intent(inout) :: self

     type(tfunc0), pointer :: f0 => null()
     type(tfunc1), pointer :: f1 => null()
     type(tfunc2), pointer :: f2 => null()
        
     allocate(f0); f0 = tfunc0(    ',','  ',0); call self%register(f0)
     allocate(f0); f0 = tfunc0(    '(','  ',0); call self%register(f0)
     allocate(f0); f0 = tfunc0(    ')','  ',0); call self%register(f0)
    
    

     allocate(f1); f1 = tfunc1( 'sqrt','rl',3, sqrt_ );  call self%register(f1)
     allocate(f1); f1 = tfunc1(  'abs','rl',3, abs_  );  call self%register(f1)
     allocate(f1); f1 = tfunc1(  'exp','rl',3, exp_  );  call self%register(f1)
     allocate(f1); f1 = tfunc1(  'log','rl',3, log_  );  call self%register(f1)
     allocate(f1); f1 = tfunc1('log10','rl',3, log10_);  call self%register(f1)
     allocate(f1); f1 = tfunc1(  'sin','rl',3, sin_  );  call self%register(f1)
     allocate(f1); f1 = tfunc1(  'cos','rl',3, cos_  );  call self%register(f1)
     allocate(f1); f1 = tfunc1(  'tan','rl',3, tan_  );  call self%register(f1)
     allocate(f1); f1 = tfunc1( 'asin','rl',3, asin_ );  call self%register(f1)
     allocate(f1); f1 = tfunc1( 'acos','rl',3, acos_ );  call self%register(f1)
     allocate(f1); f1 = tfunc1( 'atan','rl',3, atan_ );  call self%register(f1)

     allocate(f2); f2 = tfunc2(    '+','lr',1, add_ ); call self%register(f2)
     allocate(f2); f2 = tfunc2(    '-','lr',1, sub_ ); call self%register(f2)
     allocate(f2); f2 = tfunc2(    '*','lr',2, plus_); call self%register(f2)
     allocate(f2); f2 = tfunc2(    '/','lr',2, div_ ); call self%register(f2)
     allocate(f2); f2 = tfunc2(    '^','rl',3, elev_); call self%register(f2)
     allocate(f2); f2 = tfunc2(  'mod','rl',3, mod_ ); call self%register(f2)
     allocate(f2); f2 = tfunc2(  'min','rl',3, min_ ); call self%register(f2)
     allocate(f2); f2 = tfunc2(  'max','rl',3, max_ ); call self%register(f2)
        
  end subroutine

SUBROUTINE print_(self)
   CLASS(MathOper) :: self

   class(operInfo), pointer :: tmp => null()

   tmp => self%head
   do while(associated(tmp))
      print*,trim(tmp%oper)
      tmp => tmp%next
   enddo
  
END SUBROUTINE print_

  function associativity_(self,oper) result(r)
     class(MathOper)  :: self
     character(len=*) :: oper
     character(len=2) :: r

     integer :: i
     class(operInfo), pointer :: symbol => null()

     symbol => self%head
     do i=1,self%nOperator
        if (trim(oper) .eq. trim(symbol%oper))then
           r = symbol%assoc
           return
        endif
        symbol => symbol%next
     enddo
  endfunction

  function precedence_(self,oper) result(r)
     class(MathOper)  :: self
     character(len=*) :: oper
     integer          :: r

     integer :: i
     class(operInfo), pointer :: symbol => null()

     symbol => self%head
     do i=1,self%nOperator
        if (trim(oper) .eq. trim(symbol%oper))then
           r = symbol%preced
           return
        endif
        symbol => symbol%next
     enddo
  endfunction

!  function noperand_(self,oper) result(r)
!     class(MathOper)  :: self
!     character(len=*) :: oper
!     integer          :: r
!
!     integer :: i
!     class(operInfo), pointer :: symbol => null()
!
!     symbol => self%head
!     do i=1,self%nOperator
!        if (trim(oper) .eq. trim(symbol%oper))then
!           r = symbol%nop
!           return
!        endif
!        symbol => symbol%next
!     enddo
!  endfunction


  function getOper_(self,oper) result(r)
     class(MathOper)  :: self
     character(len=*) :: oper
     class(operInfo), pointer :: r

     integer :: i
     class(operInfo), pointer :: symbol => null()

     r => self%head
     do i=1,self%nOperator
        if (trim(oper) .eq. trim(r%oper))then
           return
        endif
        r => r%next
     enddo
     r => null()
  endfunction



!  subroutine shunt_(str,debug)
!    character(len=*), intent(in) :: str
!    logical, optional :: debug
!
!    integer :: i, j, k, l, m, n, o
!    integer :: ii, jj, kk
!    integer :: idx
!    integer :: top
!    integer :: ntokens
!    integer :: strLen, stackLen, outpLen
!    character(len=10), allocatable :: tokens(:)
!    character(len=50) :: MathOper
!    character(len=100) :: output
!    character(len=100) :: stack
!    logical :: dbg
!
!    dbg = .false.
!    if (present(debug)) dbg =.true.
!
!    MathOper = ''
!    do i=1,size(symbol)
!       MathOper = trim(MathOper)//trim(symbol(i)%o)
!    enddo
!
!    strLen = len_trim(str)
!    output=''
!    stack=''
!    j = 0
!    top = 0
!    !scan the infix expression from left to right
!    do i = 1, StrLen
!       idx = index(trim(MathOper),str(i:i))
!       m = index(trim(MathOper),stack(top:top))
!       ! If the token is an operand, apprend it to output.
!       if (idx.eq.0) then
!          j = j + 1
!          output(j:j) = str(i:i)
!
!          ! If the token is an opening parenthesis, push it to the stack.
!          ! This marks the beginning of an expression that should be 
!          ! evaluated separately.
!       else if (idx.ne.0 .and. str(i:i).eq.'(')then
!          top = top + 1
!          stack(top:top) = str(i:i)
!
!          !If the token is a closing parenthesis, 
!       else if (idx.ne.0 .and. str(i:i).eq.')')then
!          ! until an opening parenthesis is encontered at the top
!          ! of the stack, pop each operator from the stack and append
!          ! to the output expression. This marks the end of the 
!          ! operands and operators located within the current set of
!          ! parentheses.
!          do n = top, 1, -1
!             if ( stack(n:n) .eq. '(')then
!                top = top - 1
!                exit
!             endif
!             j = j + 1
!             output(j:j) = stack(n:n)
!             top = top - 1
!          enddo
!          ! If the token is an operator and stack is empty,
!          ! push operator into stack
!       else if (idx.ne.0 .and. top.eq.0)then
!          top = top + 1
!          stack(top:top) = str(i:i)
!          ! If is an operator and the stack is not empty, there 
!          ! may be following possibilites.
!       else if (idx.ne.0 .and. top.ne.0)then
!          ! If the token operator has greater precendence than 
!          ! the top most operator of stack, push this token 
!          ! operator into stack.
!          if (symbol(idx)%p .gt. symbol(m)%p)then
!             top = top + 1
!             stack (top:top) = str(i:i)
!             ! If the token operator and it has lower precedence than 
!             ! the top most operator of the stack
!          else if (symbol(idx)%p .lt. symbol(m)%p)then
!
!             ! pop the operator from stack
!             j = j + 1
!             output(j:j) = stack(top:top)
!             top = top - 1
!
!             ! pop the operator from stack until we find a low
!             ! precedence operator than the token or stack is
!             ! empty
!             do n = top, 1, -1
!                o =  index(trim(MathOper),stack(n:n))
!                if ( symbol(o)%p.lt.symbol(idx)%p) exit
!                j = j + 1
!                output(j:j) = stack(n:n)
!                stack(n:n)  = ''
!                top = top - 1
!             enddo
!             top = top + 1
!             stack(top:top) = str(i:i)
!
!             ! If the token is an operator and it has the same 
!             ! precedence as the operator on the top of the stack
!          else if (symbol(idx)%p .eq. symbol(m)%p)then
!             ! and the operator on top of the stack is 
!             ! left-to-right associative
!             if (symbol(m)%a .eq. 'lr')then
!                ! until the operator on top of the stack has lower 
!                ! precedence than the token, or until the stack is 
!                ! empty, pop each operator from the stack and append 
!                ! them to the output. Then push the current token to
!                ! the stack.
!                do n = top, 1, -1
!                   o =  index(trim(MathOper),stack(n:n))
!                   if ( symbol(o)%p.lt.symbol(idx)%p) exit
!                   j = j + 1
!                   output(j:j) = stack(n:n)
!                   top = top - 1
!                   stack(n:n)  = ''
!                enddo
!                top = top + 1
!                stack(top:top) = str(i:i)
!                ! and the operator on top of the stack is right-to-left 
!                ! associative, push the current token to the stack. 
!             elseif (symbol(m)%a .eq. 'rl')then
!                top = top + 1
!                stack(top:top) = str(i:i)
!             endif
!          endif
!       endif
!
!       if(dbg)write(*,'(A10,1x,A10,1x,A10)')str(i:i),trim(output),trim(stack)
!
!    enddo
!    ! all token was evaluated
!
!    if (top.gt.0)then
!       do n=top,1,-1
!          j=j+1
!          output(j:j) = stack(n:n)
!       enddo
!    endif
!    write(*,'(A20,1x,A20)') trim(str),trim(output)
!
!  end subroutine shunt_

  ! wrapper function to be used
  ! intrinsic fortran function can not
  ! be used as type bound procedure by
  ! fortran. So we need create a wrapper
  ! of eatch function used here.

  pure function sqrt_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = sqrt(x)
     return
  end function

  pure function abs_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = abs(x)
     return
  end function

  pure function exp_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = exp(x)
     return
  end function

  pure function log_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = log(x)
     return
  end function

  pure function log10_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = log10(x)
     return
  end function

  pure function sin_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = sin(x)
     return
  end function

  pure function cos_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = cos(x)
     return
  end function

  pure function tan_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = tan(x)
     return
  end function

  pure function asin_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = asin(x)
     return
  end function

  pure function acos_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = acos(x)
     return
  end function

  pure function atan_(x) result(return_value)
     real, intent(in) :: x
     real             :: return_value

     return_value = atan(x)
     return
  end function

  pure function add_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = x + y
     return
  end function

  pure function sub_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = x - y
     return
  end function

  pure function plus_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = x * y
     return
  end function

  pure function div_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = x / y
     return
  end function

  pure function elev_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = x ** y
     return
  end function

  pure function min_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = min(x,y)
     return
  end function

  pure function max_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = max(x,y)
     return
  end function

  pure function mod_(x,y) result(return_value)
     real, intent(in) :: x, y
     real             :: return_value

     return_value = mod(x,y)
     return
  end function


end module MathExpress
