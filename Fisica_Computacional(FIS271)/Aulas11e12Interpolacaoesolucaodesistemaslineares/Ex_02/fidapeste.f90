program ex_02_a
implicit none
	integer, parameter :: n = 13 !Tamanho da matriz
	!real*16	   :: X(1:n,1:n), y(1:n), a(1:n), xzinho(1:n)
	!integer		   :: i, j, m !i=linha, j=coluna

call Escreve_as_matrizes_L_U_e_retorna_x (n)

end program ex_02_a

subroutine Escreve_as_matrizes_L_U_e_retorna_x (n)	
	real*16		   	:: X(1:n,1:n),y(1:n), L(1:n,1:n), U(1:n,1:n), a(1:n), b(1:n), soma, xzinho(1:n)
	integer		   	:: i, j, k, n !Tamanho da matriz é n
A=0d0
open(100,file = 'planck.dat')
do i=1,n !le os valores de xzinho e y do arquivo .dat
	read(100,*) xzinho(i), y(i) 
enddo
close(100)
m = 0
do i=1,n
	m = n-1
	do j=1,n
		X(i,j) = (xzinho(i))**m
		m = m-1
	enddo
enddo
L=0d0
U=0d0
a=0d0
b=0d0
do i=1,n
	do j=1,n
		if (i==j) then
			L(i,j)=1d0
		endif	
	enddo
enddo
!----------------------Metodo LU-----------------------------------------------------------
do j=1,n
	U(1,j)=X(1,j)
	do i=2,j
		soma = 0d0
		write (*,*) soma
		do k=1,i-1
			soma = soma + L(i,k)*U(k,j)
			write (*,*) soma 
		enddo
		U(i,j) = X(i,j) - soma
	enddo
	do i=j+1,n
		soma=0d0
		write (*,*) soma
		do k=1,j-1
			soma = soma + L(i,k)*U(k,j)
			write (*,*) soma
		enddo
		L(i,j)=(X(i,j)- soma)/U(j,j)
	enddo
enddo
!----------------------------------------------------------------------------------------
write (*,*)
do i=1,n
	do j=1,n
		write (*,"(f16.8)", advance='no')  L(i,j)
	enddo
	write (*,*)
enddo
write (*,*)
write (*,*)
do i=1,n
	do j=1,n
		write (*,"(f16.8)", advance='no')  U(i,j)
	enddo
	write (*,*)
enddo
write (*,*)
write (*,*)
do i=1,n
	do j=1,n
		write (*,"(f16.8)", advance='no')  X(i,j)
	enddo
	write (*,*)
enddo
write (*,*)
write (*,*)
do j=1,n
	write (*,"(f16.8)", advance='no')  y(j)
enddo
write (*,*)

write (*,*)
end subroutine Escreve_as_matrizes_L_U_e_retorna_x


