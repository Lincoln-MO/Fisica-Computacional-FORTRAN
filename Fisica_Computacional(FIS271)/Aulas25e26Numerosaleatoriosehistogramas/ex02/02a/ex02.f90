!Nome: Lincoln Martins de Oliveira 			Matricula: 90693
!Programa para gerar numeros aleatórios 
program soloQHiro
implicit none

	integer*8, parameter		:: N = 101, M=51
	Real*8 , parameter		:: r = 0.995
	integer 			:: i, j
	Real*8,	dimension(0:N)		:: x01, x03, x05, x07, x09 
	character(len=29), parameter 	:: arquivo_01 = "Populacao01_r=0.995_N=100.dat"
 	character(len=34), parameter	:: arquivo_03 = "Populacao01de1-50_r=0.995_N=50.dat"
 	character(len=36), parameter	:: arquivo_05 = "Populacao01de51-100_r=0.995_N=50.dat"
 	character(len=28), parameter	:: arquivo_07 = "Populacao07_r=0.24_N=500.dat"
 	character(len=28), parameter	:: arquivo_09 = "Populacao09_r=0.24_N=500.dat"

x01(0) = 0.1d0
x03(0) = 0.1d0
open(50,file=arquivo_01)
open(150,file=arquivo_03)
open(250,file=arquivo_05)


do i=0, N-1
	x01(i+1) = 4.d0*r*x01(i)*(1.d0-x01(i))
	write(50,*)  i, x01(i)
	
enddo
x05(0)= 7.3001574574238573d0*10.d0**(-2)
do i=0, M-1
	x03(i+1) = 4.d0*r*x03(i)*(1.d0-x03(i))
	x05(i+1) = 4.d0*r*x05(i)*(1.d0-x05(i))


enddo
do i=1 , N-51
	j=50+i
	write(250,*) j, x05(i)
	
	
enddo
do i=0 , M-1
	write(150,*) i, x03(i)
	
enddo
close(50)
close(150)
close(250)


end program soloQHiro
