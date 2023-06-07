program SoLoQHirO_uyR
implicit none
	real*8	::  h, k1v, k1y, k2v, k2y, k3v, k3y, k4v, k4y, b, m, g 
	integer	:: i
	integer, parameter	:: N = 600
	real*8	::  y(0:N-1), v(0:N-1), t(0:N-1)
	character(len=15), parameter :: arquivo_1 = "ex01a_N=600.dat"


open(150,file=arquivo_1)
m=0.8d0
b=0.4d0
g=9.8d0
t(0)=0.d0
h = 0.01d0

!==========================Para V(t0)=-sqrt(2*(em+1))==================================================================
y(0)=17.2d0
v(0)=-0.64d0
do i=0,N-1   
   t(i) = t(0) + i*h
	k1v=-h*(m*(g-sqrt(2*g*y(i)))/b)
   	!k1v=-h*2*exp(-r(i))*(1-exp(-r(i))) 
	k1y=h*y(i)
   	!k1r = h*v(i)
	k2v=-h*(m*(g-sqrt(2*g*(y(i)+0.5*k1y)))/b)
   	!k2v=-h*2*exp(-(r(i)+0.5*k1r))*(1-exp(-(r(i)+0.5*k1r)))
	k2y=h*(y(i)+0.5*k1y)
   	!k2r = h*(v(i)+0.5*k1v)
	k3v=-h*(m*(g-sqrt(2*g*(y(i)+0.5*k2y)))/b)
   	!k3v=-h*2*exp(-(r(i)+0.5*k2r))*(1-exp(-(r(i)+0.5*k2r)))
	k3y=h*(y(i)+0.5*k2y)
   	!k3r = h*(v(i)+0.5*k2v)
	k4v= -h*(m*(g-sqrt(2*g*(y(i)+0.5*k3y)))/b)
   	!k4v=-h*2*exp(-(r(i)+k3r))*(1-exp(-(r(i)+k3r)))
	k4y= h*(y(i)+0.5*k3y)
   	!k4r =h*(v(i)+k3v)
   	v(i+1)=v(i)+(k1v+2*k2v+2*k3v+k4v)/6 !velocidade
   	y(i+1) = y(i)+(k1y+2*k2y+2*k3y+k4y)/6 !Posição
   write(150,*) t(i),y(i),v(i)
enddo
close(150)
end program SoLoQHirO_uyR
