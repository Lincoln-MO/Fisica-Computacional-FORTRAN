program SoLoQHirO_uyR
implicit none
	real*8	::  y0, w0, r, k, h, tf, t0, wdiferente1, wdiferente2, wdiferente3, p, j, m
	!real*8, allocatable :: g(:), y(:), E(:), w(:), t(:)
	integer	:: i
	integer, parameter	:: N = 365
	real*8	:: g(0:N-1), y(0:N-1), E(0:N-1), w(0:N-1), t(0:N-1)
	real*8	:: a(0:N-1), b(0:N-1), c(0:N-1), d(0:N-1)
	character(len=26), parameter :: arquivo_1 = "ex01_ti_yi_wi_Ei_N=365.dat"
!allocate(y(N))
!allocate(g(N))
!allocate(E(N))
!allocate(w(N))
!allocate(t(N))
open(150,file=arquivo_1)
wdiferente1 = 0.0d0
wdiferente2 = 0.0d0
wdiferente3 = 0.0d0
t(i)=0.0d0
t0=0.d0
tf=365.0d0
y0 =198.0d0
w0 =198.0d0
r =0.030d0
k =4300.0d0
h = (tf - t0)/N
w(0) = w0
do i=0,N-1
	
	t(i) = t0 + i*h  
	y(i) = (k*y0)/(y0+(k-y0)*exp(-r*t(i)))
	g(i) = r*w(i)*(1.d0-(w(i)/k))
	a(i) = h*g(i)
	wdiferente1 = w(i) + (1.d0/2.d0)*a(i)
	m = r*(wdiferente1)*(1.d0 - (wdiferente1)/k)
	b(i) = h*m
	wdiferente2 = w(i) + (1.d0/2.d0)*b(i)
	j = r*(wdiferente2)*(1.d0 - (wdiferente2)/k)
	c(i) = h*j
	wdiferente3 = w(i) + c(i) 
	p = r*(wdiferente3)*(1.d0-(wdiferente3)/k)
	d(i) = h*p
	E(i) = abs(y(i)-w(i))
	w(i+1) = w(i) + (1.d0/6.d0)*(a(i) + 2.d0*b(i) + 2.d0*c(i) + d(i))

enddo
t(0)=0.d0
do i=0,N-1
	write (150,*) t(i), y(i), w(i), E(i) 
enddo    
close(150)
end program SoLoQHirO_uyR
