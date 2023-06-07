! Exemplo de programa em Fortran
! Autor: L. G. Rizzi
! E-mail: lerizzi at ufv.br
! Ultima atualizacao: 2017.08.15

! Para compilar:   ifort arquivo.f90 -o nome_do_programa.out

! Para executar:   ./nome_do_programa.out

!================================================
! Modulo com parametros e variaveis GLOBAIS

	module TesTe

! Parametros globais

		integer		, parameter :: Ndados     = 60
		real  		, parameter :: distancia1 = 2.7e0	! em metros (precisao simples)
		real*8		, parameter :: distancia2 = 2.7d0	! em metros (dupla precisao)
		character(len=5), parameter :: arquivo    = "dados"	! parametro do tipo "string"
		logical		, parameter :: tachovendo = .FALSE.	! poderia ser .TRUE.

		double precision, parameter :: xmin =  0.001d0
		double precision, parameter :: xmax = 37.000d0

! Variaveis globais

		integer*8 :: Ncontador
		real      :: soma

	end module TesTe




!================================================
! Programa principal

	program priMEIro
	use TesTe	! 
! Parametros locais
	real, parameter :: PI_single = 3.1415926e0	! precisao simples
	double precision, parameter :: PI_double = dacos(-1.d0)	! precisao dupla = real*8 utilizando funcao arcosseno
! Variaveis locais
	integer :: num
	double precision :: f(0:Ndados), t(0:Ndados)

	write(*,*) PI_single, PI_double	! esse escreve os valores de PI e PI2 na tela

	call inicial()	! chama subrotina para inicializar variaveis e escrever .log

	f = 37.d0	! inicializa o vetor "f" atribuindo valores 37.d0 para todas as suas componentes
	t = 2.d0	! inicializa o vetor "t" atribuindo valores 2.d0 para todas as suas componentes

	num=10
	open(num,file="saida_"//arquivo//".dat")	! abre arquivo associado a unidade num=10
	call escreve_vetor_em_arquivo(num,f,t,Ndados)	! chama subortina para escrever dados no arquivo
	close(num)					! fecha o arquivo associado a unidade num=10

! Antes de retirar os comentarios das linhas abaixo certifique-se que existe o arquivo 'entrada_"//arquivo//".dat'
!	open(20,file="entrada_"//arquivo//".dat")	! abre arquivo associado a unidade 20
!	call le_vetor_do_arquivo(20,f,t,Ndados)		! chama subortina para ler dados do arquivo
!	close(20)					! fecha o arquivo associado a unidade 20

	end program priMEIro




!================================================
! SUBROTINAS
!================================================


	subroutine inicial()
	use TesTe

! Eh bom inicializar as variaveis globais com valores nulos

	Ncontator = 0		! Eh bom inicializar as variaveis integer com valor 0
	soma      = 0.d0	! Eh bom inicializar as variaveis real*8  com valor 0.d0

! Eh bom ter um arquivo .log no inicio do programa 
! para ter certeza sobre quais os parametros foram
! utilizados quando o programa foi executado.

	open(330,file="20170816_exemplo.log")	! abre arquivo associado a unidade 330
	write(330,*) "Ndados:", Ndados
	write(330,*) "distancia1:", distancia1
	write(330,*) "distancia2:", distancia2
	write(330,*) "arquivo:", arquivo
	write(330,*) "tachovendo:", tachovendo
	write(330,*) "xmin", xmin
	write(330,*) "xmax", xmax
	close(330)

	end subroutine inicial

!--------------------------------------------------------


	subroutine escreve_vetor_em_arquivo(num,f,t,Ndados)
! Variaveis locais
	integer :: num, Ndados
	integer :: k
	double precision :: f(0:Ndados), t(0:Ndados)
	do k=1,Ndados
		write(num,*) t(k), f(k)	! escreve as duas colunas (linha por linha)
					! no arquivo associado a unidade "num"
	end do
	end subroutine escreve_vetor_em_arquivo

!--------------------------------------------------------

	subroutine le_vetor_do_arquivo(num,f,t,Ndados)
! Variaveis locais
	integer :: num, Ndados
	integer :: k
	double precision :: f(0:Ndados), t(0:Ndados)
	do k=1,Ndados
		read(num,*) t(k), f(k)	! le as duas colunas (linha por linha)
					! do arquivo associado a unidade "num"
	end do
	end subroutine le_vetor_do_arquivo

!--------------------------------------------------------
