program ex_01
implicit none
	integer, parameter :: n = 3 !Tamanho da matriz
	real*8		   :: A(1:n,1:n), b(1,1:n), x(1,1:n)
	integer		   :: i, j !i=linha, j=coluna

do i=1,n !entra com os valores de A(i,j) iniciais, PREENCHER AQUI MATRIZ COM OS VALORES DADOS NO EXERCICIO 1 C
	do j=1,n 
		print*,"Entre com o valor do elemento da matriz A(i,j)(resistências):"	
		write(*,*) "A",i, j	
		read*,A(i,j)
	enddo
enddo
 
do j=1,n !entra com os valores de b(1,j) iniciais, PREENCHER AQUI MATRIZ COM OS VALORES DADOS NO EXERCICIO 1 C
	print*,"Entre com o valor dos elementos da matriz b(1,j)(votagens):"
	write(*,*) "b", i		
	read*,b(1,j)
enddo

call Escreve_as_matrizes_L_U_e_retorna_x (A,b,n)


end program


!-----------------------------------------SUBROTINAS----------------------------------------------------------
subroutine Zera_matrizes(L,U,x,y,n)

	integer :: i, j, n
	real*8  :: L(1:n,1:n), U(1:n,1:n), x(1,1:n), y(1,1:n)
do i=1,n
	do j=1,n
		L(i,j)=0d0
		U(i,j)=0d0	
			
	enddo
enddo
do j=1,n
	x(1,j)=0d0
	y(1,j)=0d0
enddo
end subroutine Zera_matrizes
!--------------------------------------------------------------------------------------------------------------

subroutine Escreve_as_matrizes_L_U_e_retorna_x (A,b,n)
	integer 		:: n  			!Tamanho da matriz
	real*8		   	:: A(1:n,1:n),L(1:n,1:n), U(1:n,1:n), x(1,1:n), y(1,1:n), b(1,1:n)
	integer		   	:: i, j, k
	real*8			:: soma1, soma2, soma3, soma4
Call Zera_matrizes(L,U,x,y,n)
do i=1,n
	do j=1,n
		if (i==j) then
			L(i,j)=1d0
		endif	
	enddo
enddo
!Metodo LU, Transformando a matriz A em duas matrizes L e U Triangulares superiores e inferiores.
do j=1,n
	
	U(1,j)=A(1,j)
	do i=2,j
		soma1=0d0
		do k=1,i-1
		soma1 = soma1 + L(i,k)*U(k,j)
		enddo
		U(i,j)=A(i,j)-soma1
	enddo
	do i=j+1,n
		soma2=0d0
		do k=1,j-1
			soma2 = soma2 + L(i,k)*U(k,j)
		enddo
		L(i,j)=(A(i,j)-soma2)/U(j,j)
	enddo
enddo
!write (*,*)
!do i=1,n
!	do j=1,n
!		write (*,"(f10.1)", advance='no')  L(i,j)
!	enddo
!	write (*,*)
!enddo
!write (*,*)
!write (*,*)
!do i=1,n
!	do j=1,n
!		write (*,"(f10.1)", advance='no')  U(i,j)
!	enddo
!	write (*,*)
!enddo
!write (*,*)

!encontrando y(1,n)

y(1,1)=b(1,1)/l(1,1)
do i=2,n
        soma3=0
	do j=1,i-1
             soma3 = soma3 + L(i,j)*y(1,j)
	enddo
y(1,i)= (b(1,i)-soma3)/L(i,i)
enddo

!encontrando x(1,n)

x(1,n)=y(1,n)/U(n,n)
do i=n-1,1,-1
        soma4 = 0
	do j=i+1,n
		soma4 = soma4 + U(i,j)*x(1,j)
	enddo
	x(1,i) = (y(1,i)-soma4)/U(i,i)
enddo
!write (*,*)
!do j=1,n
!	write (*,"(f10.4)", advance='no')  x(1,j)
!enddo
!write (*,*)
!write (*,*)
!do j=1,n
!	write (*,"(f10.4)", advance='no')  y(1,j)
!enddo
!write (*,*)

!escreve os elementos da matriz na tela
call escreve_na_tela(A,x,n)

end subroutine Escreve_as_matrizes_L_U_e_retorna_x 
!-------------------------------------------------------------

subroutine escreve_na_tela(A,x,n)
	integer :: i, j, n
	real*8	:: A(1:n,1:n), x(1,1:n)
	character(len=22), parameter :: arquivo = "correntes_i1_i2_i3.dat"


!escreve os elementos da matriz na tela
write (*,*)
do i=1,n
	do j=1,n
		write (*,"(f10.3)", advance='no')  A(i,j)
	enddo
	write (*,*)
enddo
Write (*,*)
do j=1,n
	write(*,"(f8.3)",advance='no') x(1,j)
enddo
write(*,*)
open(100,file=arquivo)
do i=1,n
write(100,*) x(1,i)
enddo
close(100)
end subroutine escreve_na_tela












