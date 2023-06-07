program soloQHiro
implicit none
	
	integer, parameter		:: ni=1 , nf=100000
	Real*8 , parameter		:: ro = 2
	integer 			:: i, j
	Real*8,	dimension(0:nf)		:: yn, teta, xn
	character(len=6), parameter 	:: arquivo_01 = "yn.dat" 


yn = 0.d0
teta = 0.d0
xn = 0.d0
open(50,file=arquivo_01) 
open(200,file= "N_xn.dat")
open(300,file= "teta.dat")

do i=ni,nf
	read(200,*) xn(i)
	read(300,*) teta(i)
enddo

do i=ni, nf
	yn(i) = ro*dcos(teta(i))*dsqrt(-2.d0*log(1-xn(i)))
	write(50,*)  i, yn(i)
	
enddo

close(50)
close(200)
close(300)


end program soloQHiro
