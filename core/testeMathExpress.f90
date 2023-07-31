program teste
   use MathExpress
   use varType
   implicit none


   character(len=50) ::  Mod_
   integer :: i
   integer :: ntokens
   character(len=32), allocatable :: tokens(:)



   Mod_='vtmp1(atmt:925, rhmt:925, 92500)'

   print*, getType('_')
   call tokenize(Mod_,ntokens,tokens)

   print*, trim(Mod_), ntokens
   do i=1,ntokens
      print*,trim(tokens(i)), len_trim(tokens(i))
   enddo
   


end program


