module scan_simpleScores
   implicit none

   contains
   
   function vies(modelA, modelB) result(return_value)
      real :: modelA
      real :: modelB
      real :: return_value

      return_value = modelA - modelB
      
      return
   end function

end modele
