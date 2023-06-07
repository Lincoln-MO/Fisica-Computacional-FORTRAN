
program ex01c_aulas9e10
	real*8	:: E 
	integer	:: ntestesmaximos 
	real*8	:: y, dy
	real*8	:: En
	real*8	:: raiz
	real*8	:: x01, x02, x03, x04, x05
        real*8	:: xn
	integer :: k, i
	character(len=18), parameter :: arquivo = "ex_01_c_raizes.dat"
E = 0.000001d0
ntestesmaximos = 100000

open(100,file = arquivo)!cria um arquivo .dat
do i=1,5 		!entra com os valores de x iniciais
	if (i==1) then
		print*,"Entre com o valor inicial de x considerando os intervalos -30<x<-23 "
		read*,x01
		x = x01
	elseif (i==2) then
		print*,"Entre com o valor inicial de x considerando os intervalos -22< x <-16 "
		read*,x02
		x = x02
	elseif (i==3) then
		print*,"Entre com o valor inicial de x considerando os intervalos -6< x <-1 "
		read*,x03
		x = x03
	elseif (i==4) then
		print*,"Entre com o valor inicial de x considerando os intervalos 9< x <15 "
		read*,x04
		x = x04
	elseif (i==5) then
		print*,"Entre com o valor inicial de x considerando os intervalos 33< x <40 "
		read*,x05
		x = x05
	endif
	do k = 1,ntestesmaximos ! realiza um pequeno lup "infinito" para encontrar os valores das raizes
		y =(1 + (1 + x**2 )*sin(x/5))/(1 + x**2)
		dy = (((2*x*sin(x/5))+((1+x**2)*cos(x/5))/5)*(1+x**2)-(1+(1+x**2)*sin(x/5))*2*x)/(1+x**2)
		xn = x-(y/dy)
		En = abs((xn -x)/(xn))
		if (En < E) then 
			raiz = xn 
			goto 28
		endif
		x = xn
	enddo
28 continue
write(100,*) "raiz=", raiz !escreve a raiz no arquivo
enddo	
close(100)
end program ex01c_aulas9e10













