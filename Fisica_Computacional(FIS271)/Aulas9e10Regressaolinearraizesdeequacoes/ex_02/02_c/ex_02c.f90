


!---------------------------------------------------------programa principal-------------------------------------------------------------------------------
program ex_02c
implicit none

open (200,file = "millikan.dat")	
call ajuste_linear()
close(200)
end program ex_02c
!----------------------------------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------subrotinas-------------------------------------------------------------------------------------------
subroutine ajuste_linear() !Calcula a regressão linear, CA=aa e CL=bb=0
	real*8	:: C, D, aa 
	integer	, parameter :: n = 15
	integer :: i
	real*8	:: x(1:n), y(1:n)
	character(len=10), parameter :: arquivo_2 = "Raizes.dat"

C = 0.d0
D = 0.d0
do i=1,n
	read(200,*) x(i), y(i)
	C = C + (x(i))**2
	D = D + x(i)*y(i)

enddo
aa = D/C

open (300,file= arquivo_2)
	write(300,*) "a =", aa

close(300)
write(*,*) aa 


end subroutine ajuste_linear
!----------------------------------------------------------------------------------------------------------------------------------------------------------
