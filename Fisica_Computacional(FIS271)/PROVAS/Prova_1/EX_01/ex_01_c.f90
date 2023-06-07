!Lincoln Martins de Oliveira 			Matricula:90693 
program ex01c
implicit none

	
	real*8	 :: deltat
	integer, parameter :: N = 20
	integer :: i, k, m
	real*8 :: x, Ei, SM
	character(len=9), parameter :: arquivo = "ex01c.dat"
	real*8 ::  x0, xf, a
	real*8 :: f(0:N-1), g(0:N), h(0:N)
open(100,file = arquivo)
Sm=0d0
print*,"Entre com o valor inicial de M inteiro qualquer"
read*,m
xf = 3.1415926d0/2d0
x0 = 0d0
deltat = (xf - x0)/dfloat(N)
do k=1, m !calcula o valor da expanção para m valores
	h(k)= ((-1)**(k-1))*(x**(2*k-1))/(2*k-1) !não me lembro como implementar fatorial, não foi visto em sala
enddo
do i= 1,N !calcula os valores de f,g,Ei e Sm e escreve no arquivo . dat os valores de x,Ei e Sm Respectivamente
	x = dfloat(i)*deltat
	f(i) = h(k)
	g(i)= sin(x)
	Ei= (f(i)-g(i)) 
	Sm= Sm + Ei
	write(100,*)  x , Ei, Sm
end do
close(100)
end program ex01c
