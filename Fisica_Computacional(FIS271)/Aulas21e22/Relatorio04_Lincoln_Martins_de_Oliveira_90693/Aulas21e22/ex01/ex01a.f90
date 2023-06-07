!Nome:Lincoln Martins de Oliveira				Matricula:90693

program SoLoQHirO_uyR
implicit none
	real*16	:: m, d, a, re, tf, e, fase, deltaT, vel  
	integer	:: i
	integer, parameter	:: N = 10000
	real*16	:: r_antigo(0:N-1), v_antigo(0:N-1), t(0:N-1), ace(0:N-1), r(0:N-1), v(0:N-1), y(0:N-1), Erro(0:N-1), Erro1(0:N-1)
	real*16	:: U(0:N-1), K(0:N-1), Emec(0:N-1), vello(0:N-1), Errov(0:N-1)
	character(len=24), parameter :: arquivo_1 = "ex01_verlet_original.dat"
	character(len=26), parameter :: arquivo_2 = "ex01_velocit_de_verlet.dat"
!=============================dados_iniciais======================================================================
open(150,file=arquivo_1)
m=1.e0
d=1.e0
a=1.e0
re=0.e0
tf=100.e0
t(0)=0.e0
e=-0.8e0
fase = acos(sqrt(0.2))
deltaT=0.01e0
r_antigo(0)=0d0
v_antigo(0)=-sqrt(2*abs(e + 1))
do i=0,n-1
	t(i) = t(0) + i*deltaT
	ace(i) = -2*exp(-r_antigo(i))*(1-exp(-r_antigo(i))) 
	v_antigo(i+1) = v_antigo(i) + ace(i)*deltaT
	r_antigo(i+1) = r_antigo(i) + v_antigo(i)*deltaT + 0.5e0*ace(i)*(deltaT)**2.e0
	
enddo
!==================================================================================================================
!=============================verlet_original======================================================================
r(0)=0.e0
v(0)=-sqrt(2*abs(e + 1))
r(1)= r_antigo(1)
v(1)= v_antigo(1)
do i=1, n-1
	ace(i) = -2*exp(-r(i))*(1-exp(-r(i)))
	r(i+1) = 2.e0*r(i) - r(i-1) + ace(i)*(deltaT)**2.e0
	v(i+1) = ((1.e0)/(2.e0*deltaT))*(r(i+1) - r(i-1))
	y(i) =log((sqrt( e + 1)*cos(sqrt(abs(-2*e))*t(i)-fase)- 1)/e)
	vello(i) =(-sqrt(-2*e)*sqrt(e+1)*sin(sqrt(-2*e)*t(i)-fase))/(-1+sqrt(e+1)*cos(sqrt(-2*e)*t(i)-fase))                  !SOLUÇÃO EXATA V(t)
   	Erro(i) = abs(y(i)-r(i))
	Errov(i) = abs(vello(i)-v(i))
	
enddo
!=================================================================================================================
!=========================Energias_pelo_verlet_original===========================================================
do i=1, n-1
	U(i) = -D*(1.e0 - (1.e0 - exp(-a*(r(i)-re)))**2.e0) 	!energia potencial
	K(i) = (m*v(i)**2.e0)/(2.e0)				!energia cinética
	Emec(i) = U(i) + K(i)					!energia mecânica

enddo

do i=0, n-1
	write(150,*) t(i), r(i), v(i), y(i), Erro(i), U(i), K(i), Emec(i), vello(i), Errov(i) !escreve os dados no arquivo .dat associado a unidade 150
enddo

close(150)
!==================================================================================================================
!======================================velocit_de_verlet===========================================================
r(0)=0.e0
v(0)=-sqrt(2*abs(e + 1))
ace(0) = -2*exp(-r(0))*(1-exp(-r(0))) 
!r(1)= r_antigo(1)
!v(1)= v_antigo(1)
open(250,file=arquivo_2)
do i=0, n-1
	vel = v(i) + (ace(i)*deltaT)/(2.e0)
	r(i+1) = r(i) + vel*deltaT
	ace(i+1) = -2*exp(-r(i+1))*(1-exp(-r(i+1))) 
	v(i+1) = vel + (ace(i+1)*deltaT)/2
	y(i) =log((sqrt( e + 1)*cos(sqrt(abs(-2*e))*t(i)-fase)- 1)/e)
	vello(i) =(-sqrt(-2*e)*sqrt(e+1)*sin(sqrt(-2*e)*t(i)-fase))/(-1+sqrt(e+1)*cos(sqrt(-2*e)*t(i)-fase)) 		 !SOLUÇÃO EXATA V(t)
   	Erro1(i) = abs(y(i)-r(i))
	Errov(i) = abs(vello(i)-v(i))
	
enddo
r(0)=0.e0
!=================================================================================================================
!=====================Energias_pelo_velocit_verlet================================================================
do i=0, n-1
	U(i) = -D*(1.e0 - (1.e0 - exp(-a*(r(i)-re)))**2.e0) 	!energia potencial
	K(i) = (m*v(i)**2.e0)/(2.e0)				!energia cinética
	Emec(i) = U(i) + K(i)					!energia mecânica

enddo

do i=0, n-1
	write(250,*) t(i), r(i), v(i), y(i), Erro1(i), U(i), K(i), Emec(i), vello(i), Errov(i) !escreve os dados no arquivo .dat associado a unidade 250
enddo
close(250)


end program SoLoQHirO_uyR







	
