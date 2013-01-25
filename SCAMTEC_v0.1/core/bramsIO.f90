module bramsIO

 implicit none

 contains


!##############################################################################################
!->Inicio do comentario
!->Subrotina: open_file
!->Descrição: Abre arquivo binario especificado se existir
!->Variaveis
!unidade		:unit do arquivo
!tamanho		:tamanho do arquivo, valor a ser recebido pela subrotina
!fname			:nome do arquivo
!arq_lido		:valor do iostat 0 -arquivo lido -1 -arquivo não lido ou não existe
!ex			:variavel logica, verifica existencia do arquivo
!############################################################################################

subroutine open_file(fname,unidade,tamanho,arq_lido)
implicit none

 integer,intent(out)		::unidade		
 integer,intent(in)		::tamanho		
 character(len=1024),intent(in) ::fname	
 integer,intent(out)		::arq_lido			
 logical 			:: ex

	unidade=25

	inquire(exist=ex,file=trim(fname))
	if(ex)then
		open(unidade,file=trim(fname),access='direct',status='old',recl=tamanho*4 ,iostat=arq_lido,convert="little_endian")
		if(arq_lido .ne. 0)then
			print*, "File not found",trim(fname),arq_lido, tamanho,"tamanho"
			stop
		endif
        else
		print*,'erro!!'
		stop
	endif
end subroutine open_file

!###########################################################################################
!->Inicio do comentario
!->Subrotina: read_file
!->Descrição: leitura de binario grads otimizada
!->Variaveis
!unidade	:unit do arquivo
!pos		:posição da variavel
!var		:variavel a ser lida
!###########################################################################################

subroutine read_file(unidade,pos,var)
implicit none

 integer, intent(in)::unidade			
 integer, dimension(:), intent(in) :: pos	
 real,dimension(:,:),intent(out)::var		
 integer::i					

	do i=1,size(pos)	
				
		read(unidade,rec=pos(i))var(:,i) 
		where(var(:,i) .eq. -0.9990000E+34)var(:,i)=-9999.0
		
	enddo


end subroutine read_file

end module bramsIO
