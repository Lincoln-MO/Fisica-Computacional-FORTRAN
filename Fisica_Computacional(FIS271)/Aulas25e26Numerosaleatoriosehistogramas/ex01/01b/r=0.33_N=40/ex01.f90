!Nome: Lincoln Martins de Oliveira 			Matricula: 90693
!Programa para gerar numeros aleatórios 
program soloQHiro
implicit none

	integer*8, parameter		:: N = 41
	Real*8 , parameter		:: r = 0.33
	integer 			:: i
	Real*8,	dimension(0:N)		:: x01, x03, x05, x07, x09 
	character(len=27), parameter 	:: arquivo_01 = "Populacao01_r=0.33_N=40.dat"
 	character(len=27), parameter	:: arquivo_03 = "Populacao03_r=0.33_N=40.dat"
 	character(len=27), parameter	:: arquivo_05 = "Populacao05_r=0.33_N=40.dat"
 	character(len=27), parameter	:: arquivo_07 = "Populacao07_r=0.33_N=40.dat"
 	character(len=27), parameter	:: arquivo_09 = "Populacao09_r=0.33_N=40.dat"
x01(0) = 0.1d0
x03(0) = 0.3d0
x05(0) = 0.5d0
x07(0) = 0.7d0
x09(0) = 0.9d0
open(50,file=arquivo_01)
open(150,file=arquivo_03)
open(250,file=arquivo_05)
open(350,file=arquivo_07)
open(450,file=arquivo_09)

do i=0, N-1
	x01(i+1) = 4.d0*r*x01(i)*(1.d0-x01(i))
	x03(i+1) = 4.d0*r*x03(i)*(1.d0-x03(i))
	x05(i+1) = 4.d0*r*x05(i)*(1.d0-x05(i))
	x07(i+1) = 4.d0*r*x07(i)*(1.d0-x07(i))
	x09(i+1) = 4.d0*r*x09(i)*(1.d0-x09(i))
	write(50,*)  i, x01(i)
	write(150,*) i, x03(i)
	write(250,*) i, x05(i)
	write(350,*) i, x07(i)
	write(450,*) i, x09(i)
enddo

close(50)
close(150)
close(250)
close(350)
close(450)

end program soloQHiro
