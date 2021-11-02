Program DMSolveur

  Use mod_solveur
  Implicit None
  Real(Kind=PR),Dimension(:,:),Allocatable :: A,B,C
  Real(Kind=PR),Dimension(:),Allocatable :: v,x1,x2
  Integer :: i,n
  Real(Kind=PR) :: norm1,norm2

  !Instruction
  n=3
  A=MatExo1(n)
  Allocate(v(1:n))
  Allocate(x1(1:n))
  Allocate(x2(1:n))
  v=1._PR
  Call ResiduMinimum(A,v,x1,1._PR/1000000._PR)
  Call GPO(A,v,x2,1._PR/1000000._PR)
  norm1=Norme(v-ProdMatVect(A,x1))
  norm2=Norme(v-ProdMatVect(A,x2))
  Print *, norm1, norm2
  

End Program DMSolveur
