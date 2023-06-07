!Nome: Lincoln Martins de Oliveira		Matricula: 90693

program pseudo_aleatorio
implicit none
integer, parameter		:: ni=1 , nf=100000
integer*8			:: i, k
real*8				:: r, cont, p, q
real*8,	dimension(0:100000)	:: yn
Real*8 , parameter		:: ro = 2
p= 0.d0
q= 0.d0
cont = 0.d0


	open(100,file= "yn.dat")
	open(20,file="histograma100000.dat")
	
	do i=ni,nf
		read(100,*) yn(i)
	enddo
	do i=0, 59                        ! histograma para ymin=-3*ro
		p= -6.d0 + 0.1d0*i
		q= -6.d0 + 0.1d0*(i+1.d0)
		cont=0.d0	
		do k=ni, nf
			if (p<= yn(k).and. yn(k)< q) then
				cont = cont + 1.d0
			end if
		end do
		cont = cont/10000
		write(20,*) p, cont
		write(20,*) q, cont
	end do

	do i=0, 59                        ! histograma para ymax=3*ro

		p=0.1d0*i
		q=0.1d0*(i+1.d0)
		cont=0.d0	
		do k=ni, nf
			if (p<= yn(k).and. yn(k)< q) then
				cont = cont + 1.d0
			end if
		end do
		cont = cont/10000
		write(20,*) p, cont
		write(20,*) q, cont
	end do
!=======================================================================================================================
	
	close(20)
	close(100)
	
	




end program
