program SoLoQHirO_uyR
implicit none
	real*8	::  y0, w0, r, k, h, tf, t0
	!real*8, allocatable :: g(:), y(:), E(:), w(:), t(:)
	integer	:: i, j
	integer, parameter	:: N = 730
	real*8	:: g(0:N-1), y(0:N-1), E(0:N-1), w(0:N-1), t(0:N-1)
	character(len=27), parameter :: arquivo_1 = "ex02a_ti_yi_wi_Ei_N=730.dat"
!allocate(y(N))
!allocate(g(N))
!allocate(E(N))
!allocate(w(N))
!allocate(t(N))
open(150,file=arquivo_1)
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
	
	t(i)= t0 + i*h  !atua como contador
	y(i)= (k*y0)/(y0+(k-y0)*exp(-r*t(i)))
	g(i)= r*w(i)*(1.d0-(w(i)/k))
	E(i) = abs(y(i)-w(i))
	w(i+1) = w(i) + h*g(i)

enddo
t(0)=0.d0
do i=0,N-1
	write (150,*) t(i), y(i), w(i), E(i) 
enddo    
close(150)
end program SoLoQHirO_uyR
