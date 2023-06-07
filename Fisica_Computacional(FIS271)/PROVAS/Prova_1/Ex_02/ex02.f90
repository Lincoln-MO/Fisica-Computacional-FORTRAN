!Lincoln Martins de Oliveira 			Matricula:90693 
program ex02b
	real*8	:: Vi, V0, deltaV
	integer	:: Ndados
	real*8	:: R, T, V
	real*8	:: a, b
        real*8	:: P(0:Vi)
	integer :: k
	character(len=12), parameter :: arquivo = "ex_02_b_.dat"
print*,"Entre com o valor de Ndados "
	read*,Ndados
R = 8,3144598d0
T = 9d0/27d0 !Temperatura
Vi = 1
v0 = 0.1
deltaV= (Vi - V0)/dfloat(Ndados)
a= 1,3373d0
b=0.0320d0

open(100,file = arquivo)!cria um arquivo .dat
do k=1,Ndados
	Vi=V0 + deltaV*dfloat(k)
	P(k)= (R*T)/(V-b) - ((a)/(V*(V-b)) + ((a*b)/((V**2)*(V-b))

	write(100,*) Vi, P(k) !escreve no arquivo dat ps valores respectivos de Vi e P(k)
enddo	
close(100)
end program ex02b

