!Lincoln Martins de Oliveira
!Este programa deve plotar o gráfico da distribuição de boltzman

program grafico_de_g

implicit none
integer*8			:: i
real*8				:: t1,kb,a,a1,a0,h
real*8,	dimension(0:500)	:: g,e
real*8, parameter		:: pi=dacos(-1.d0)
integer, parameter		:: N = 1000

t1= 10.d0 !kelvin
kb=8.6173324d0*10.d0**(-5.d0)	!constrante de Boltz-mann
a=(3.d0*10.d0**(-13.d0))/(dsqrt(pi)*(kb*t1)**(3.d0/2.d0))
a1=(dsqrt(pi)*(kb*t1)**(3.d0/2.d0))/(3.d0*10.d0**(-13.d0))
a0=a*a1
h=kb*t1/50
e(0)= 0.d0
write(*,*) a1,a,a0		!teste
open(10,file='ex02a.dat')

do i=0,n/2		!Preenche em um vetor de entrada e saida
	e(i)= e(0) + h*i
	g(i)= a0*(e(i)**0.5d0)*dexp(-e(i)/(kb*t1))
	write(10,*) e(i), g(i)		
end do

close(10)

end program grafico_de_g
