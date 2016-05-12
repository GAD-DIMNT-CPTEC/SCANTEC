module bramsIO

 implicit none

 contains

subroutine abrir_arq(fname,unidade,tamanho,arq_lido)
implicit none

 integer,intent(out)::unidade		!unit do arquivo
 integer,intent(in)::tamanho		!tamanho do arquivo, valor a ser recebido pela subrotina
 character(len=1024),intent(in)::fname	!nome do arquivo
 integer,intent(out)::arq_lido			!valor do iostat 0-arquivo lido -1-arquivo não lido ou não existe
 logical*1 :: ex

	unidade=25
	!abre o arquivo informado pelo read.f90

	inquire(exist=ex,file=trim(fname))
	if(ex)then
	open(unidade,file=trim(fname),access='direct',status='old',recl=tamanho*4 ,iostat=arq_lido,convert="little_endian")
	!open(unidade,file=trim(fname),access='direct',status='old',recl=tamanho ,iostat=arq_lido)
		if(arq_lido .ne. 0)then !verifica de o arquivo existe
			print*, "File not found",trim(fname),arq_lido, tamanho,"tamanho"
			stop

		endif
        else
	print*,'erro!!'
	stop
	endif
end subroutine abrir_arq


subroutine ler_arq(unidade,pos,var)
implicit none

 integer, intent(in)::unidade			!unit do arquivo
 integer, dimension(:), intent(in) :: pos	!posição da variavel
 real*8,dimension(:,:,:),intent(out)::var		!variavel a ser lida
 integer::i					!variavel de controle

	do i=1,size(pos)	!loop variando do valor de i até o tamanho do vetor pos (quantidade de posições)
				
		read(unidade,rec=pos(i))var(i,:,:)	!leitura do arquivo e atribuição do valor da variavel em um determiinado nivel e coordenada
	enddo
	


end subroutine ler_arq

subroutine ler_arq2(unidade,pos,var)
implicit none

 integer, intent(in)::unidade			!unit do arquivo
 integer, dimension(:), intent(in) :: pos	!posição da variavel
 real,dimension(:,:),intent(out)::var		!variavel a ser lida
 integer::i					!variavel de controle

	do i=1,size(pos)	!loop variando do valor de i até o tamanho do vetor pos (quantidade de posições)
				
		read(unidade,rec=pos(i))var(:,i) !leitura do arquivo e atribuição do valor da variavel em um determiinado nivel e coordenada
		
	enddo


end subroutine ler_arq2

end module bramsIO
