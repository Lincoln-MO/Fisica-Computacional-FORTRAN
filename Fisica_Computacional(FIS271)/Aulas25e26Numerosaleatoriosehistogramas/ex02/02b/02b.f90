!Nome: Lincoln Martins de Oliveira		Matricula: 90693

program pseudo_aleatorio
implicit none
integer*8			:: i,k
real*8				:: r,cont, p,q
real*8,	dimension(0:10000)	:: x01
real*8,	dimension(0:100000)	:: x02
real*8,	dimension(0:1000000)	:: x03
p= 0.d0
q= 0.d0
cont = 0.d0
x01(0)=0.1d0
x02(0)=0.1d0
x03(0)=0.1d0
r=0.995d0

	open(10,file="n10000.dat")
	open(20,file="histograma10000.dat")
	open(30,file="n100000.dat")
	open(40,file="histograma100000.dat")
	open(50,file="n1000000.dat")
	open(60,file="histograma1000000.dat")

		
	do i=0,10000
		
		x01(i+1)= 4.d0*r*x01(i)*(1.d0-x01(i))			
		write(10,*) i, x01(i)		
	end do
	

	do i=0, 9

		p=0.1d0*i
		q=0.1d0*(i+1.d0)
		cont=0.d0	
		do k=1,10000
			if (p<= x01(k).and. x01(k)< q) then
				cont = cont + 1.d0
			end if
		end do
		cont = cont/1000
		write(20,*) p, cont
		write(20,*) q, cont
	end do
!=======================================================================================================================
	p= 0.d0
	q= 0.d0

	do i=0,100000
		
		x02(i+1)= 4.d0*r*x02(i)*(1.d0-x02(i))			
		write(30,*) i, x02(i)		
	end do
	

	do i=0, 9

		p=0.1d0*i
		q=0.1d0*(i+1.d0)
		cont=0.d0	
		do k=1,100000
			if (p<= x02(k).and. x02(k)< q) then
				cont=cont+1.d0
			end if
		end do
		cont=cont/10000
		write(40,*)p, cont
		write(40,*)q, cont
	end do

	
!==========================================================================================
	p= 0.d0
	q= 0.d0

	do i=0,1000000
		
		x03(i+1)= 4.d0*r*x03(i)*(1.d0-x03(i))			
		write(50,*) i, x03(i)		
	end do
	
	do i=0, 9

		p=0.1d0*i
		q=0.1d0*(i+1.d0)
		cont=0.d0	
		do k=1,1000000
			if (p<= x03(k).and. x03(k)< q) then
				cont=cont+1.d0
			end if
		end do
		cont=cont/100000
		write(60,*)p, cont
		write(60,*)q, cont
	end do


	close(10)
	close(20)
	close(30)
	close(40)
	close(50)
	close(60)
	




end program
