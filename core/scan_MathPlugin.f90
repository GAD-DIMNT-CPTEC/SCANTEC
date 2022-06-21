module scan_MathPlugin
   use scantec_module
   use MathExpress
   use scan_MetForm
   
   implicit none

   contains
   subroutine scan_mathPlugin_Init()
      type(tfunc1), pointer :: f1 => null()
      type(tfunc2), pointer :: f2 => null()
      type(tfunc3), pointer :: f3 => null()
      
      allocate(scantec%MathEval)

      call scantec%MathEval%initialize()
      
      allocate(f1); f1 = tfunc1( 'svap','rl',3,  svap); call scantec%MathEval%register(f1)
      allocate(f1); f1 = tfunc1('hmxr1','rl',3, hmxr1); call scantec%MathEval%register(f1)
      allocate(f1); f1 = tfunc1('umes1','rl',3, umes1); call scantec%MathEval%register(f1)
      
      allocate(f2); f2 = tfunc2( 'vapp','rl',3,  vapp); call scantec%MathEval%register(f2)
      allocate(f2); f2 = tfunc2('hmxr2','rl',3, hmxr2); call scantec%MathEval%register(f2)
      allocate(f2); f2 = tfunc2('umes2','rl',3, umes2); call scantec%MathEval%register(f2)
      allocate(f2); f2 = tfunc2( 'tpor','rl',3,  tpor); call scantec%MathEval%register(f2)
      allocate(f2); f2 = tfunc2('vtmp2','rl',3, vtmp2); call scantec%MathEval%register(f2)
      
      allocate(f3); f3 = tfunc3('umes3','rl',3, umes3); call scantec%MathEval%register(f3)
      allocate(f3); f3 = tfunc3( 'umrl','rl',3,  umrl); call scantec%MathEval%register(f3)
      allocate(f3); f3 = tfunc3('vtmp1','rl',3, vtmp1); call scantec%MathEval%register(f3)
   end subroutine

   
end module
