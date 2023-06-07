program SoLoQHirO_uyR_ex02b
implicit none
	real*16				:: kb, t1, f, f1, f0, soma, a, h, k, integral1, integral2, ys, y, ye
	integer				:: i, j
	integer, parameter		:: N = 1000
	real*16	:: g(0:n/2), g1(0:n/2), g2(0:n/2), e(0:n/2), g3(0:n/2), g4(0:n/2), g5(0:n/2), z(0:n/2)	
	real*8, parameter		:: pi = dacos(-1.d0)
	character(len=9), parameter 	:: arquivo_1 = "ex02b.dat"
kb = 8.6173324e0*10.e0**(-5.e0)	!constrante de Boltz-mann, unidades em ev/kelvin
t1 = 10.e0
f = (3.d0*10.d0**(13.d0))/(dsqrt(pi)*(kb*t1)**(3.e0/2.e0))
f1 = (dsqrt(pi)*(kb*t1)**(3.e0/2.e0))/(3.e0*10.e0**(13.e0))
f0 = f*f1
a = 4.e0
h = a/n
e(0)= 0.e0
soma = 0.e0
k = 0.e0
open(100,file=arquivo_1)
!---------------------------integral1------------------------------------------------------------------
do i=1, n/2
	e(i) = e(0) + i*h
	!x(i) = 1.e0/Kb*t1
	g(i) = f*((2.e0*e(i) - 2.e0*h)**(0.5e0))*exp(-(2.e0*e(i) - 2.e0*h)/Kb*t1)
	g1(i) = f*((2.e0*e(i) - h)**(0.5e0))*exp(-(2.e0*e(i) - h)/Kb*t1)	
	g2(i) = f*((2.e0*e(i))**(0.5e0))*exp(-(2.e0*e(i))/Kb*t1) 
	k = (h/3.e0)*(g(i) + 4.e0*g1(i)+ g2(i))
	soma = soma + k
	write(*,*) g2(i)
enddo

integral1 = soma
!-----------------------------------------------------------------------------------------------------
!------------------------integral2--------------------------------------------------------------------
k = 0.e0
h = 1.e0/a*n
z(0) = 0.e0
do i=1, n/2
	z(i) = z(0) + i*h
	g3(i) = (f*(1.e0/(2.e0*z(i) - 2.e0*h))**(0.5e0)*exp(-((kb*t1)/(2.e0*z(i) - 2.e0*h))))*((1.e0/(2.e0*z(i) - 2.e0*h))**2.e0)
	!g3(i) = (f0*((2.e0*(1.e0/z(i)) - 2.e0*h)**(0.5e0))*dexp(-(2.e0*(1.e0/z(i)) - 2.e0*h)/(kb*t1)))/((2.e0*(1.e0/z(i)) - 2.e0*h)**2.e0)
	g4(i)= f*(1.e0/(2.e0*z(i) - h))**(0.5e0)*exp(-(1.e0/(2.e0*z(i) - h))/(kb*t1))*((1.e0/(2.e0*z(i) - h))**2.e0)
	!g4(i) = (f0*((2.e0*(1.e0/z(i)) - h)**(0.5e0))*dexp(-(2.e0*(1.e0/z(i)) - h)/(kb*t1)))/((2.e0*(1.e0/z(i)) - h)**2.e0)	
	!g5(i) = (f0*((2.e0*(1.e0/z(i)))**(0.5e0))*dexp(-(2.e0*(1.e0/z(i)))/(kb*t1)))/((2.e0*(1.e0/z(i)))**2.e0)
	g5(i) = f*(1.e0/(2.e0*z(i)))**(0.5e0)*exp(-(1.e0/(2.e0*z(i)))/(kb*t1))*((1.e0/(2.e0*z(i)))**(2.e0))
	k = (h/3.e0)*(g3(i) + 4.e0*g4(i)+ g5(i))
	soma = soma + k
	!write(*,*) g3(i)
enddo
integral2 = soma
!----------------------------------------------------------------------------------------------------------------
y = integral1 + integral2
ye= 0.3e0*y
ys= y - ye
write(*,*) integral1, integral2 , y
write(100,*) "N = ", y 
write(100,*) "Ns = ", ys
write(100,*) "Ne = ", ye
close(100)

end program SoLoQHirO_uyR_ex02b
