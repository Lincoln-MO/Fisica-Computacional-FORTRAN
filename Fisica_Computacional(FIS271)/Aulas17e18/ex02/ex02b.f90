program SoLoQHirO_uyR
implicit none
	real*8	:: t0, tf, e, h, fase, k1v, k1r, k2v, k2r, k3v, k3r, k4v, k4r  
	integer	:: i
	integer, parameter	:: N = 200
	real*8	:: r(0:N-1), y(0:N-1), Erro(0:N-1), v(0:N-1), t(0:N-1)
	!real*8	:: a(0:N-1), b(0:N-1), c(0:N-1), d(0:N-1)
	character(len=36), parameter :: arquivo_1 = "ex02_ti_ri_vi_yi_Erroi_-v0_N=200.dat"
	character(len=36), parameter :: arquivo_2 = "ex02_ti_ri_vi_yi_Erroi_+v0_N=200.dat"

open(150,file=arquivo_1)
t0=0.d0
tf=10.0d0
e = -0.8d0
h = (tf - t0)/N
fase = acos(sqrt(0.2))
!==========================Para V(t0)=-sqrt(2*(em+1))==================================================================
v(0)=-sqrt(2*(e+1))
r(0)=0d0
v(0)=-sqrt(2*(e + 1))
do i=0,N-1   
   t(i) = t0 + i*h
   k1v=-h*2*exp(-r(i))*(1-exp(-r(i))) 
   k1r = h*v(i)
   k2v=-h*2*exp(-(r(i)+0.5*k1r))*(1-exp(-(r(i)+0.5*k1r)))
   k2r = h*(v(i)+0.5*k1v)
   k3v=-h*2*exp(-(r(i)+0.5*k2r))*(1-exp(-(r(i)+0.5*k2r)))
   k3r = h*(v(i)+0.5*k2v)
   k4v=-h*2*exp(-(r(i)+k3r))*(1-exp(-(r(i)+k3r)))
   k4r =h*(v(i)+k3v)
   v(i+1)=v(i)+(k1v+2*k2v+2*k3v+k4v)/6
   r(i+1) = r(i)+(k1r+2*k2r+2*k3r+k4r)/6
   y(i) =log(   (     sqrt( e + 1 )*cos(     sqrt(  abs(-2*e)  )*t(i)   -   fase   )  - 1   )   /   e     )
   Erro(i) = abs(y(i)-r(i))
   write(150,*) t(i),r(i),v(i),y(i),Erro(i)
enddo
close(150)
!==================================================================================================================
!=================================Para V(t0)=+sqrt(2*(em+1))=======================================================
open(250,file=arquivo_2)
r(0)=0d0
v(0)= sqrt(2*(e + 1))
do i=0,N-1   
   t(i) = t0 + i*h
   k1v=-h*2*exp(-r(i))*(1-exp(-r(i))) 
   k1r = h*v(i)
   k2v=-h*2*exp(-(r(i)+0.5*k1r))*(1-exp(-(r(i)+0.5*k1r)))
   k2r = h*(v(i)+0.5*k1v)
   k3v=-h*2*exp(-(r(i)+0.5*k2r))*(1-exp(-(r(i)+0.5*k2r)))
   k3r = h*(v(i)+0.5*k2v)
   k4v=-h*2*exp(-(r(i)+k3r))*(1-exp(-(r(i)+k3r)))
   k4r =h*(v(i)+k3v)
   v(i+1)=v(i)+(k1v+2*k2v+2*k3v+k4v)/6
   r(i+1) = r(i)+(k1r+2*k2r+2*k3r+k4r)/6
   y(i) =log(   (     sqrt( e + 1 )*cos(     sqrt(  abs(-2*e)  )*t(i)   -   fase   )  - 1   )   /   e     )
   Erro(i) = abs(y(i)-r(i))
   write(250,*) t(i),r(i),v(i),y(i),Erro(i)
enddo
close(250)



end program SoLoQHirO_uyR
