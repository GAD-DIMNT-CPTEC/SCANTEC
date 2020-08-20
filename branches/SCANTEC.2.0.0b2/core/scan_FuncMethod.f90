module scan_FuncMethod
   use scan_MetForm
   implicit none
   type MetFunc
      character(len=16) :: FuncName
      integer           :: nargs
      procedure( ), pointer, nopass :: Func => null()
      type(MetFunc), pointer :: next => null()
   end type
   type(MetFunc), pointer :: FirstFunc => null()
   type(MetFunc), pointer :: currFunc => null()
   contains
      subroutine initFunctions_( )

         call RegisterFunctions( 'svap', 1,  svap)
         call RegisterFunctions( 'vapp', 2,  vapp)
         call RegisterFunctions('hmxr1', 1, hmxr1)
         call RegisterFunctions('hmxr2', 2, hmxr2)
         call RegisterFunctions('umes1', 1, umes1)
         call RegisterFunctions('umes2', 2, umes2)
         call RegisterFunctions('umes3', 3, umes3)
         call RegisterFunctions( 'umrl', 3,  umrl)
         call RegisterFunctions( 'dptp', 2,  dptp)
         call RegisterFunctions('vtmp1', 2, vtmp1)
         call RegisterFunctions('vtmp2', 3, vtmp2)

      end subroutine

      subroutine RegisterFunctions( FuncName, NumArgs, Fct)
         character(len=*), intent(in) :: FuncName
         integer, intent(in) :: NumArgs
         external :: Fct

         if(.not.associated(FirstFunc))then
            allocate(FirstFunc)
            currFunc => FirstFunc
         else
            allocate(currFunc%next)
            currFunc => currFunc%next
         endif

         currFunc%FuncName = trim(FuncName)
         currFunc%nargs = NumArgs
         currFunc%Func => Fct
         
      end subroutine

      subroutine runFunction (FuncName, args, val)
         character(len=*), intent(in) :: FuncName
         real :: args(:)
         real :: val
         
         integer :: nargs
         type(MetFunc), pointer :: tmp => null()

         nargs = size(args)
         tmp => FirstFunc
         do while(associated(tmp))
            if(trim(FuncName).eq.trim(tmp%FuncName))then
               call tmp%Func(args,val)
               return
            endif
            tmp => tmp%next
         enddo

         stop 'some problem'


         
      end subroutine
end module
