program ex_02_a
implicit none
	integer, parameter :: n = 13 !Tamanho da matriz
	real*16	   :: A(1:n,1:n), b(1:n), x(1:n), xzinho(1:n)
	integer		   :: i, j, m !i=linha, j=coluna
A=0d0
open(100,file = 'planck.dat')
do i=1,n !le os valores de xzinho e b do arquivo .dat
	read(100,*) xzinho(i), b(i) 
enddo

m = 0
do i=1,n
	m = n-1
	do j=1,n
		A(i,j) = (xzinho(i))**m
		m = m-1
	enddo
enddo

write (*,*) 
do i=1,n
	do j=1,n
		write (*,*)  A(i,j)
	enddo
	write (*,*)
enddo
write(*,*)
call Escreve_as_matrizes_L_U_e_retorna_x (A,b,n)
close(100)
end program ex_02_a
!OBS:Neste programa considerei minha matriz A como sendo a matriz x do roteiro, a matriz b equivale a matriz y do 
!roteiro e os coeficiêntes aqui buscadodos são os valores de x que no roteiro são os an
!----------------------------------------Subrotinas---------------------------------------------------------
subroutine Escreve_as_matrizes_L_U_e_retorna_x (A,b,n)		
	real*16		   	:: A(1:n,1:n),L(1:n,1:n), U(1:n,1:n), x(1:n), y(1:n), b(1:n)
	integer		   	:: i, j, k, n !Tamanho da matriz
	real*16 		:: soma
L=0d0
U=0d0
x=0d0
y=0d0
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
		soma = 0d0
		
		do k=1,i-1
			soma = soma + L(i,k)*U(k,j)
			
		enddo
		U(i,j) = A(i,j) - soma
	enddo
	do i=j+1,n
		soma=0d0
		
		do k=1,j-1
			soma = soma + L(i,k)*U(k,j)
			
		enddo
		L(i,j)=(A(i,j)- soma)/U(j,j)
	enddo
enddo
!--------------------------------------------------------------------------------------------------------
!write (*,*)
!do i=1,n
!	do j=1,n
!		write (*,"(f16.8)", advance='no')  L(i,j)
!	enddo
!	write (*,*)
!enddo
!write (*,*)
!write (*,*)
!do i=1,n
!	do j=1,n
!		write (*,"(f16.8)", advance='no')  U(i,j)
!	enddo
!	write (*,*)
!enddo
!write (*,*)
!----------------------------------------------------------------------------------------------------------
!encontrando y(1,n)

y(1)=b(1)/L(1,1)
do i=2,n
        soma=0
	do j=1,i-1
             soma = soma + L(i,j)*y(j)
	enddo
y(i)= (b(i)-soma)/L(i,i)
enddo

!encontrando x(1,n)

x(n)=y(n)/U(n,n)
do i=n-1,1,-1
        soma = 0
	do j=i+1,n
		soma = soma + U(i,j)*x(j)
	enddo
	x(i) = (y(i)-soma)/U(i,i)
enddo
!write (*,*)
!do j=1,n
!	write (*,"(f16.6)", advance='no')  x(1,j)
!enddo
!write (*,*)
!write (*,*)
!do j=1,n
!	write (*,"(f16.6)", advance='no')  y(1,j)
!enddo
!write (*,*)
!escreve os elementos da matriz na tela
call escreve_na_tela(A,x,n,L,U)
end subroutine Escreve_as_matrizes_L_U_e_retorna_x 
!-------------------------------------------------------------
subroutine escreve_na_tela(A,x,n,L,U)
	integer :: i, j, n
	real*16	:: A(1:n,1:n), x(1:n), L(1:n,1:n), U(1:n,1:n)
	character(len=10), parameter :: arquivo_3 = "MatrizL.dat"
	character(len=10), parameter :: arquivo_4 = "MatrizU.dat"
	character(len=16), parameter :: arquivo_2 = "coeficientes.dat"
	character(len=11), parameter :: arquivo_1 = "matrizA.dat"
!escreve os elementos da matriz na tela
open(150,file=arquivo_3)
write (150,*) "Matriz L:"
do i=1,n
	do j=1,n
		write (150,"(f16.8)", advance='no')  L(i,j)
	enddo
	write (150,*)
enddo
close(150)
open(250,file=arquivo_4)
write (250,*) "Matriz U:"
do i=1,n
	do j=1,n
		write (250,"(f16.8)", advance='no')  U(i,j)
	enddo
	write (250,*)
enddo
close(250)
open(50,file=arquivo_1)
write (50,*) "Matriz A:"
do i=1,n
	do j=1,n
		write (50,"(f16.8)", advance='no')  A(i,j)
	enddo
	write (50,*)
enddo
close(50)
open(300,file=arquivo_2)
do i=1,n
write(300,*) x(i)
enddo
close(300)


end subroutine escreve_na_tela
!-------------------------------------------------------------------------------------
subroutine Zera_matrizes(L,U,x,y,n)

	integer :: i, j, n
	real*16  :: L(1:n,1:n), U(1:n,1:n), x(1:n), y(1:n)
	
do i=1,n
	do j=1,n
		L(i,j)=0
		U(i,j)=0	
			
	enddo
enddo
do j=1,n
	x(j)=0
	y(j)=0
enddo
end subroutine Zera_matrizes
