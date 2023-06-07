
program grafico_de_g

implicit none
integer 			:: i
real*16				:: e, a0, h
real*16,	dimension(0:500)	:: g,r
real*8, parameter		:: pi=dacos(-1.d0)
integer, parameter		:: N = 1000
e=1.6021766*10**(-19)
a0=0.53*10**(-10)
h=1/a0*500
r(0)= 0.d0
open(10,file='ex02a.dat')

do i=0,n/2		!Preenche em um vetor de entrada e saida
	r(i)= r(0) + h*i
	g(i)= (e/pi)*exp(-r(i))
	write(10,*) r(i), g(i)		
end do

close(10)

end program grafico_de_g
