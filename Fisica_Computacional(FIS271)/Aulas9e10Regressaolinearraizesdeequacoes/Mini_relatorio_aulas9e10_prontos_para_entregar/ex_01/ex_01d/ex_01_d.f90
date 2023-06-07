
program ex01c_aulas9e10
	real*8	:: E 
	integer	:: ntestesmaximos, npassosrealizados
	real*8	:: y, dy
	real*8	:: En
	real*8	:: raiz
	real*8	:: x01, x02, x03, x04, x05
        real*8	:: xn
	integer :: k
	character(len=42), parameter :: arquivo1 = "ex_01_d_raizes_e_numero_de_passos_x=-4.dat"
	character(len=37), parameter :: arquivo2 = "ex_01_d_xn_npassosrealizados_x=-4.dat"
E = 0.000001d0
ntestesmaximos = 100000
npassosrealizados = 0
open(100,file = arquivo1)!cria um arquivo .dat
!entra com os valores de x iniciais
	
print*,"Entre com o valor inicial de x pedido pelo exercicio"
read*,x01
x = x01

open(200,file = arquivo2) !cria um arquivo .dat
do k = 1,ntestesmaximos ! realiza um pequeno lup "infinito" para encontrar os valores das raizes
	y =(1 + (1 + x**2 )*sin(x/5))/(1 + x**2)
	dy = (((2*x*sin(x/5))+((1+x**2)*cos(x/5))/5)*(1+x**2)-(1+(1+x**2)*sin(x/5))*2*x)/(1+x**2)
	xn = x-(y/dy)
	En = abs((xn -x)/(xn))
	npassosrealizados = npassosrealizados + 1
	write(200,*) xn, npassosrealizados
	if (En < E) then 
		raiz = xn 
		goto 28
	endif
	x = xn
enddo
28 continue
close(200)
write(100,*) "raiz =", raiz, "Número de passos =", npassosrealizados !escreve a raiz no arquivo
close(100)
end program ex01c_aulas9e10













