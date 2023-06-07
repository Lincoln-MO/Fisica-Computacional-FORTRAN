program SoLoQHirO_uyR
implicit none
	real*16	:: tf, ti, deltaT, G, Mh, Ms, modullo, velx, vely, modullov, rlinha, Tcometa, segundosemano, ar0, av0, Tcometaanos
	integer	:: i
	integer, parameter	:: N = 10000
	real*16	:: r_antigo(0:N-1,0:1), v_antigo(0:N-1,0:1), t(0:N-1), acex(0:N-1), acey(0:N-1), r(0:N-1,0:1), v(0:N-1,0:1)
	real*16	:: U(0:N-1), K(0:N-1), Emec(0:N-1), angular(0:N-1)
	real*8, parameter		:: pi=dacos(-1.d0)
	character(len=16), parameter :: arquivo_3 = "ex02_Angular.dat"
	character(len=19), parameter :: arquivo_2 = "ex02_t_U_K_Emec.dat"
	character(len=22), parameter :: arquivo_1 = "ex02_t_rx_ry_vx_vy.dat"
	character(len=14), parameter :: arquivo_0 = "ex02_rx_ry.dat"
	character(len=26), parameter :: arquivo_4 = "ex02_Período_do_cometa.dat"
!=============================dados_iniciais======================================================================

t(0)= 0.e0						!s
tf = 2838240000.0e0 			!s
ti = 0.e0						!s
deltaT = (tf-ti)/float(N)		!s
!deltaT = 345600.e0	
r_antigo(0,0)= (8.80e0)*(10.e0)**(7.e0)
r_antigo(0,1)= 0.e0
v_antigo(0,0)= 0.e0
v_antigo(0,1)= -54.62e0
G = (6.67408e0)*(10.e0)**(-20.e0)		!constante gravitacional
Mh = (2.2e0)*(10.e0)**(14.e0) 			!kg, massa do cometa
Ms = (2.0e0)*(10.e0)**(30.e0)			!kg, massa da estrela
r(0,0)= (8.80e0)*(10.e0)**(7.e0)		!km, x(t) (obs: os valores identificados com indice de coluna 0 estão associados a x)
r(0,1)= 0.e0					!km, y(t) (obs: os valores identificados com indice de coluna 1 estão associados a y)
v(0,0)= 0.e0					!km/s, vx (obs: os valores identificados com indice de coluna 0 estão associados a x)
v(0,1)= -54.62e0				!km/s, vy (obs: os valores identificados com indice de coluna 1 estão associados a y)
modullo = sqrt((r(0,0))**(2.e0) + (r(i,1))**(2.e0))
acex(0) = (-G*Ms*Mh*r(0,0))/(Mh*modullo**(3.e0)) 
acey(0) = (-G*Ms*Mh*r(0,1))/(Mh*modullo**(3.e0)) 
open(50,file=arquivo_0)
open(150,file=arquivo_1)
open(250,file=arquivo_2)
open(450,file=arquivo_3)
!===================================================================================================================================
!=============================Velocit_Verlet=====================================================================================
do i=0, N-1
	t(i)= i*deltaT	
	velx= v(i,0) + (acex(i)*deltaT)/(2.e0)
	vely= v(i,1) + (acey(i)*deltaT)/(2.e0)
	r(i+1,0) = r(i,0) + velx*deltaT 				!rx
	r(i+1,1) = r(i,1) + vely*deltaT 				!ry
	modullo = sqrt((r(i+1,0))**(2.e0) + (r(i+1,1))**(2.e0))		
	acex(i+1) = (-G*Ms*Mh*r(i+1,0))/(Mh*modullo**(3.e0)) 
	acey(i+1) = (-G*Ms*Mh*r(i+1,1))/(Mh*modullo**(3.e0)) 
	v(i+1,0) = velx + (acex(i+1)*deltaT)/2				!vx
	v(i+1,1) = vely + (acey(i+1)*deltaT)/2 				!vy
	
enddo
r(0,0)= (8.80e0)*(10.e0)**(7.e0)
r(0,1)= 0.e0
v(0,0)= 0.e0
v(0,1)= -54.62e0
!=================================================================================================================
do i=0, n-1
	write(150,*) t(i), r(i,0), r(i,1), v(i,0), v(i,1) !escreve os dados no arquivo .dat associado a unidade 150, na ordem t, rx, ry, vx, vy
	write(50,*) r(i,0), r(i,1)
enddo
U=0.e0
K=0.e0
Emec=0.e0
do i=0, N-1
	
	modullo = sqrt((r(i,0))**(2.e0) + (r(i,1))**(2.e0))
	modullov = ((v(i,0))**(2.e0) + (v(i,1))**(2.e0))
	U(i) = (-G*Ms*Mh)/(modullo)					!energia potencial
	K(i) = (Mh*modullov)/(2.e0)					!energia cinética
	Emec(i) = U(i) + K(i)						!energia mecânica
	angular(i)=abs(r(i,0)*Mh*v(i,1)-r(i,1)*Mh*v(i,0))		!Momento angular

enddo

do i=0, n-1
	
	write(250,*) t(i), U(i), K(i), Emec(i) 	!escreve os dados do tempo, energia potencial, cinetica e mecânica no arquivo .dat associado a unidade 250
	write(450,*) t(i), angular(i)		!Momento angular
enddo
close(250)
close(450)
close(150)
close(50)
!====================================Calculo do periodo do cometa==========================================================================
ar0 = r_antigo(0,0)
av0 = abs(v_antigo(0,1))
segundosemano = 31536000			!s
open(1050,file=arquivo_4)
rlinha = (-G*Ms*ar0)/(-2.e0*G*Ms + ar0*av0**(2.e0))
write(1050,*) "rlinha =", rlinha
Tcometa = sqrt(((4.e0*(pi**(2.e0))*rlinha**(3.e0)))/(G*Ms))
Tcometaanos = Tcometa/segundosemano
write(1050,*) "Período do cometa é:",  Tcometaanos ,"anos"
close(1050)
end program SoLoQHirO_uyR

