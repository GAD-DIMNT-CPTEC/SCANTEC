program read_clima

  real :: matriz(192,96)

open (unit=10, file='climatologia50yr.jan.bin',form='unformatted',access='direct',recl=96*192*4,status='unknown')

do i=1,100
   read(10,rec=I)matriz
   print*,I,maxval(matriz),minval(matriz)
enddo



end program
