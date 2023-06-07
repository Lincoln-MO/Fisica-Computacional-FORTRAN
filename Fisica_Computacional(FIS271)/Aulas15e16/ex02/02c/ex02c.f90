program soloQhirozety
implicit none
real*8	::  y0, w0, r, k, h, tf, t0
	!real*8, allocatable :: g(:), y(:), E(:), w(:), t(:)
	integer	:: i, j
	integer, parameter	:: N = 730
	real*8	:: g(0:N-1), y(0:N-1), E(0:N-1), w(0:N-1), t(0:N-1), Gzona(0:N-1), wbarra(0:N-1)
	character(len=42), parameter :: arquivo_1 = "ex02c_ti_yi_wi_Ei_N=730_k=4200_r=-0.03.dat"

open(250,file=arquivo_1)

t(i)=0.0d0
t0=0d0
tf=365.0d0
y0 =4200.0d0
w0 =4200.0d0
r =-0.030d0
k =4300.0d0
h = (tf - t0)/N
w(0) = w0
do i=0,N-1
	
	t(i)= t0 + i*h  
	y(i)= (k*y0)/(y0+(k-y0)*exp(-r*t(i)))
	g(i)= r*w(i)*(1.d0-(w(i)/k))
	wbarra(i)= w(i) + h*g(i)
	Gzona(i)= r*w(i)*(1.d0-(wbarra(i)/k))
	w(i+1) = w(i) + (h/2.d0)*(g(i)+Gzona(i))
	E(i) = abs(y(i)-w(i))

enddo
t(0)=0.d0

do i=0,N-1
	write (250,*) t(i), y(i), w(i), E(i) 
enddo    
close(250)









end program soloQhirozety
