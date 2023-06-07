


!---------------------------------------------------------programa principal-------------------------------------------------------------------------------
program ex_02b
implicit none

open (200,file= "millikan.dat")	
call ajuste_linear()
close(200)
end program ex_02b
!----------------------------------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------subrotinas-------------------------------------------------------------------------------------------
subroutine ajuste_linear() !Calcula a regressão linear, CA=aa e CL=bb
	real*8	:: A, B, C, D, aa, bb 
	integer	, parameter :: n = 15
	integer :: i
	real*8	:: x(1:n), y(1:n)
	character(len=10), parameter :: arquivo_2 = "Raizas.dat"

A = 0.d0
B = 0.d0
C = 0.d0
D = 0.d0
do i=1,n
	read(200,*) x(i), y(i)
	A = A + x(i)
	B = B + y(i)
	C = C + (x(i))**2
	D = D + x(i)*y(i)

enddo
bb = (B*C-A*D)/(n*C-A**2)
aa = (n*D-A*B)/(n*C-A**2)

open (300,file= arquivo_2)
	write(300,*) "a =", aa
	write(300,*) "b =", bb

close(300)
write(*,*) aa, bb

end subroutine ajuste_linear
!----------------------------------------------------------------------------------------------------------------------------------------------------------
