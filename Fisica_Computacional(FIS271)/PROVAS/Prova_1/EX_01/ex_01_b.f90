!Lincoln Martins de Oliveira 			Matricula:90693 
program ex01b
implicit none

	
	real*8	 :: deltat
	integer, parameter :: N = 20
	integer :: i
	real*8 :: x, Ei
	character(len=9), parameter :: arquivo = "ex01b.dat"
	real*8 ::  x0, xf, a
	real*8 :: f(0:N-1), g(0:N-1)
open(100,file = arquivo) !abre um arquivo .dat e escreve nele os valores de
xf = 3.1415926d0/2d0
x0 = 0d0
deltat = (xf - x0)/dfloat(N)
do i= 1,N 
	x = x0 + dfloat(i)*deltat
	a= -(x**7)/(7d0*6d0*5d0*4d0*3d0*2d0) + (x**9)/(9d0*8d0*7d0*6d0*5d0*4d0*3d0*2d0)
	f(i) = x-(x**3)/(3d0*2d0) + (x**5)/(5d0*4d0*3d0*2d0) +a - (x**11)/(11d0*10d0*9d0*8d0*7d0*6d0*5d0*4d0*3d0*2d0)
	g(i)= sin(x)
	Ei= (f(i)-g(i))
	write(100,*)  x , Ei !escreve no arquivo os valores de x e Ei
end do
close(100)
end program ex01b
