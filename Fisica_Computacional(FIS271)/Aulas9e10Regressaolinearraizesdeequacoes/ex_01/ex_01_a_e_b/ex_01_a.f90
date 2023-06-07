
program ex01a_aulas9e10
	real*8	:: E 
	integer	:: ntestesmaximos, soma1, soma2
	real*8  :: xzero1 
	real*8	:: xzero2 
	real*8	:: y, dy
	real*8	:: En
	real*8	:: raiz1, raiz2
	real*8	:: x1, x2
        real*8	:: xn

	integer :: k
	character(len=18), parameter :: arquivo = "ex_01_a_raizes.dat"
	character(len=28), parameter :: arquivo1 = "nxn_de_primeirointervalo.dat"
	character(len=27), parameter :: arquivo2 = "nxn_de_segundointervalo.dat"
E = 0.000001d0
ntestesmaximos = 100000
xzero1 =  40d0
xzero2 = -4d0
soma1=0
soma2=0
open(200,file = arquivo1)
open(300,file = arquivo2)
!print*,""
x1 = xzero1
do k = 1,ntestesmaximos
	y = (3d0 + x1)**(2)-12d0
	dy = 2d0*(3d0 + x1)
	xn = x1-(y/dy)
	En = abs((xn -x1)/(xn))
	soma1=soma1+1
	write(200,*) soma1, xn	
	if (En < E) then 
		raiz1 = xn 
		goto 28
	endif
	x1 = xn
enddo
28 continue	
x2 = xzero2
do k=1,ntestesmaximos
	y = (3d0 + x2)**2 -12d0
	dy = 2d0*(3d0 + x2)
	xn = x2 - (y/dy)
	En = abs((xn -x2)/(xn))
	soma2=soma2+1
	write(300,*) soma2, xn	
	if (En < E) then
		raiz2 = xn 
		goto 45
		
	endif
	x2 = xn
	enddo
45 continue	

open(100,file = arquivo)

	write(100,*) "raiz1 =", raiz1	

	write(100,*) "raiz2 =", raiz2

close(100)
close(200)
close(300)
end program ex01a_aulas9e10













