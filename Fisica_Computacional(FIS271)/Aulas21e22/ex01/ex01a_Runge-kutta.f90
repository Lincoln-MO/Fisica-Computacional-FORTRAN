program SoLoQHirO_uyR
implicit none
	real*16	:: t0, tf, e, h, fase, k1v, k1r, k2v, k2r, k3v, k3r, k4v, k4r, m, d, a, re  
	integer	:: i
	integer, parameter	:: N = 10000
	real*16	:: r(0:N-1), y(0:N-1), Erro(0:N-1), v(0:N-1), t(0:N-1)
	real*16	:: U(0:N-1), K(0:N-1), Emec(0:N-1), vello(0:N-1), Errov(0:N-1)
	character(len=35), parameter :: arquivo_1 = "ex01a_ti_ri_vi_yi_Erroi_N=10000.dat"

open(150,file=arquivo_1)
m=1.e0
d=1.e0
a=1.e0
re=0.e0
t0=0.d0
tf=100.0d0
e = -0.8d0
h = (tf - t0)/N
fase = acos(sqrt(0.2))
!==========================Para V(t0)=-sqrt(2*(em+1))==================================================================
r(0)= 0d0
v(0)=-sqrt(2*abs(e + 1))
do i=0,N-1   
   t(i) = t0 + i*h
   k1v = -h*2.e0*exp(-r(i))*(1.e0-exp(-r(i))) 
   k1r =  h*v(i)
   k2v = -h*2.e0*exp(-(r(i)+0.5e0*k1r))*(1.e0-exp(-(r(i)+0.5e0*k1r)))
   k2r =  h*(v(i)+0.5e0*k1v)
   k3v = -h*2*exp(-(r(i)+0.5*k2r))*(1-exp(-(r(i)+0.5e0*k2r)))
   k3r =  h*(v(i)+0.5e0*k2v)
   k4v = -h*2e0*exp(-(r(i)+k3r))*(1.e0-exp(-(r(i)+k3r)))
   k4r =  h*(v(i)+k3v)
   v(i+1) = v(i)+(k1v+2.e0*k2v+2.e0*k3v+k4v)/6
   r(i+1) = r(i)+(k1r+2*k2r+2*k3r+k4r)/6
   y(i) = log((sqrt( e + 1 )*cos(sqrt(abs(-2*e))*t(i)-fase)- 1)/e)
   vello(i) =(-sqrt(-2*e)*sqrt(e+1)*sin(sqrt(-2*e)*t(i)-fase))/(-1+sqrt(e+1)*cos(sqrt(-2*e)*t(i)-fase)) 		 !SOLUÇÃO EXATA V(t)
   Erro(i) = abs(y(i)-r(i))
   Errov(i) = abs(vello(i)-v(i))

enddo
!=========================Energias_pelo_Runge-kutta===========================================================
do i=1, n-1
	U(i) = -D*(1.e0 - (1.e0 - exp(-a*(r(i)-re)))**2.e0) 	!energia potencial
	K(i) = (m*v(i)**2.e0)/(2.e0)				!energia cinética
	Emec(i) = U(i) + K(i)					!energia mecânica

enddo

do i=0, N-1
	write(150,*) t(i), r(i), v(i), y(i), Erro(i), U(i), K(i), Emec(i), vello(i), Errov(i) !escreve os dados no arquivo .dat associado a unidade 150
enddo

close(150)
!==================================================================================================================


end program SoLoQHirO_uyR
