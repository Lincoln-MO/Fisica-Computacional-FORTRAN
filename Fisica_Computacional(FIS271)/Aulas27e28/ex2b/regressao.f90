program ex2b
 real*8 A,B,C,D,q, aa, bb,n
  integer Ndados, i

   
  call raizes


end program ex2b

subroutine raizes
  real*8 A,B,C,D,q,n
  integer Ndados, i
 open(1,file="dados1.dat")
  A=0
  B=0
  C=0
  D=0
  Ndados = 100
 

  Do i=1,Ndados
      read(1,*)n,q
     A=A+n
     B=B+q
     C=C+n**2
     D=D+n*q
  enddo
  close(1)
  
  aa = (Ndados*D-A*B)/(Ndados*C-A**2)
  bb = (B*C-A*D)/(Ndados*C-A**2)

  print*,  "a =", aa,"b = ", bb


end subroutine raizes
