program SoLoQHirO_uyR
implicit none
	real*8	::  a, b, h, soma, pi, x0, I, E
	integer	:: j
	integer, parameter	:: N = 1000
	real*8	:: f(0:N-1), x(0:N-1)
	character(len=20), parameter :: arquivo_1 = "ex01b_I_E_N=1000.dat"
open(100,file=arquivo_1)
a = 0.e0
pi = 3.1415926e0
b = pi/2.e0
soma = 0.e0
h = (b-a)/N
x0 = 0.e0
do j=0,n-1
	x(j) = x0 + j*h
	f(j) = (sin(x(j)+h) + sin(x(j)))*(h/2.e0)
	soma = soma + f(j)
enddo
I = soma
E = abs(1.e0 - I)
write(100,*)'Iab =', I
write(100,*)'E =', E
close(100)

end program SoLoQHirO_uyR
