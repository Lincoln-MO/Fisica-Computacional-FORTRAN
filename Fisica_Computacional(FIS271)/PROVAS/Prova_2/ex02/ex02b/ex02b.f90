program SoLoQHirO_uyR_ex02b
implicit none
	real*16				:: soma, a, h, k, integral1, integral2, ys, y, ye, ezinho, a0, zzz
	integer				:: i, j
	integer, parameter		:: N = 1000
	real*16	:: g(0:n/2), g1(0:n/2), g2(0:n/2), e(0:n/2), g3(0:n/2), g4(0:n/2), g5(0:n/2), z(0:n/2), x(0:n/2), x1(0:n/2)	
	real*8, parameter		:: pi = dacos(-1.d0)
	character(len=9), parameter 	:: arquivo_1 = "ex02b.dat"
ezinho=1.6021766e0*10.e0**(-19.e0)
a0=0.53e0*10.e0**(-10.e0)
a = 1.e0
h = a/N
e(0)= 0.0000000000000000000000000000001d0
soma = 0.e0
k = 0.e0
zzz=(ezinho/(a0)**3)
!write(*,*) zzz, h
open(100,file=arquivo_1)
!---------------------------integral1------------------------------------------------------------------
do i=1, n/2
	e(i) = e(0) + i*h
	x(i) = 2.e0*e(i)/a0
	g(i) = - 4.e0*(zzz)*((2.e0*e(i)- 2.e0*h)**2)*exp(-(2.e0*x(i)- 2.e0*h))
	g1(i) = - 4.e0*(zzz)*((2.e0*e(i)- h)**2.e0)*exp(-(2.e0*x(i)- h))	
	g2(i) = - 4.e0*(zzz)*((2.e0*e(i))**2.e0)*exp(-(2.e0*x(i)))
	k = (h/3.e0)*(g(i) + 4.e0*g1(i)+ g2(i))
	soma = soma + k
	write(*,*) g2(i)
enddo

integral1 = soma
!-----------------------------------------------------------------------------------------------------
!------------------------integral2--------------------------------------------------------------------
k = 0.e0
h = 1.e0/a*n
z(0) = 0.0000000000000000000000000000001d0
do i=1, n/2
	z(i) = z(0) + i*h
	x1(i) = 2.e0*z(i)/a0
	g3(i) = -4*(ezinho/(a0)**3)*((1/(2*z(i)- 2*h)**2))*exp(-2.e0*((1.e0/(2.e0*x1(i)- 2.e0*h)**2.e0)))
	g4(i)= -4*(ezinho/(a0)**3)*((1/(2*z(i)- h)**2))*exp(-2.e0*((1.e0/(2.e0*x1(i)- h)**2.e0)))
	g5(i) = -4*(ezinho/(a0)**3)*((1/(2*z(i))**2))*exp(-2.e0*((1.e0/(2.e0*x1(i))**2.e0)))
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
write(100,*) "qnuven = ", y 
!write(100,*) "Ns = ", ys
!write(100,*) "Ne = ", ye
close(100)

end program SoLoQHirO_uyR_ex02b
