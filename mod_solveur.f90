Module mod_solveur

  Implicit None
  Integer,parameter :: PR=8
  

Contains

  Function ProdMat(A,B) Result(C)

    Implicit None
    Real(Kind=PR),Dimension(:,:) :: A,B
    Real(Kind=PR),Dimension(1:size(A,1),1:size(A,1)) :: C
    Integer :: i,j
    C=0._PR
    Do i=1,size(A,1)
       Do j=1,size(A,1)
          C(i,j)=C(i,j)+A(i,j)*B(j,i)
       End Do
    End Do

  End Function ProdMat
  
  Function ProdMatVect(Matrice,Vect) Result(P)

    Implicit None
    Real(Kind=PR),Dimension(:) :: Vect
    Real(Kind=PR),Dimension(:,:) :: Matrice
    Real(Kind=PR),Dimension(1:size(Matrice,1)) :: P
    Integer :: i,j

    !Instruction
    P=0
    Do i=1,size(Matrice,2)
       Do j=1,size(Matrice,1)
          P(i)=P(i)+Matrice(i,j)*Vect(j)
       End Do
    End Do

  End Function ProdMatVect

  Function ProdScal(Vect1,Vect2) Result(V)

    Implicit None
    Real(Kind=PR),Dimension(:) :: Vect1,Vect2
    Real(Kind=PR) :: V
    Integer :: k

    !Instruction
    V=0._PR
    Verification : If (size(Vect1)==size(Vect2)) Then
       Do k=1,size(Vect1)
          V=V+Vect1(k)*Vect2(k)
       End Do
    Else 
       Print *,'les deux vect ont des tailes différentes'
    End If Verification

  End Function ProdScal

  Function Norme(Vect) Result(N)

    Implicit None
    Real(Kind=PR),Dimension(:) :: Vect
    Real(Kind=PR) :: N

    !Instruction
    N=sqrt(ProdScal(Vect,Vect))

  End Function Norme

  Function MatExo1(n) Result(A)

    Implicit None
    Real(Kind=PR),Dimension(:,:),Allocatable :: A
    Integer,Intent(In) :: n
    Real(Kind=PR),Dimension(:,:),Allocatable :: B,I
    Real(Kind=PR) :: alpha
    Integer :: k,j

    !Instruction
    Allocate(A(1:n,1:n))
    Allocate(B(1:n,1:n))
    Allocate(I(1:n,1:n))
    alpha=1._PR/Real(n*n)
    I=0._PR
    Identite : Do k=1,n
       I(k,k)=1._PR
    End Do Identite
    Call random_number(B)
    A=I+alpha*ProdMat(Transpose(B),B)

  End Function MatExo1

  Subroutine GPO(A,b,x,tolerance)

    Implicit None
    !Definition des arguments 
    !x à la sortie est l'itération finale et au début correspont à x0
    Real(Kind=PR),Dimension(:,:),Intent(In) :: A
    Real(Kind=PR),Dimension(:),Intent(In) :: b
    Real(Kind=PR),Intent(In) :: tolerance
    Real(Kind=PR),Dimension(:),Intent(Inout) :: x
    !Variable de la procédure
    Integer :: k,kmax
    Real(Kind=PR),Dimension(1:size(x)) :: r,z
    Real(Kind=PR) :: alpha

    !Instruction
    r=b-ProdMatVect(A,x)
    k=0
    Print *,"Choissisez un nombre max d'itération"
    Read *, kmax
    
    Iteration : Do While (k<=kmax .and. Norme(r)>tolerance)
       z=ProdMatVect(A,r)
       alpha=ProdScal(r,r)/ProdScal(z,r)
       x=x+alpha*r
       r=r-alpha*z
       k=k+1
    End Do Iteration
    !x est donc l'itération final à partir d'ici

    Anomalie :If (k>kmax) Then
       Print *,'La tolérance non atteinte',Norme(r)
    End If Anomalie

  End Subroutine GPO

  Subroutine ResiduMinimum(A,b,x,tolerance)

    Implicit None
    !Definition des arguments 
    !x à la sortie est l'itération finale et au début correspont à x0
    Real(Kind=PR),Dimension(:,:),Intent(In) :: A
    Real(Kind=PR),Dimension(:),Intent(In) :: b
    Real(Kind=PR),Intent(In) :: tolerance
    Real(Kind=PR),Dimension(:),Intent(Inout) :: x
    !Variable de la procédure
    Integer :: k,kmax
    Real(Kind=PR),Dimension(1:size(x)) :: r,z
    Real(Kind=PR) :: alpha

    !Instruction
    r=b-ProdMatVect(A,x)
    k=0
    Print *,"Choissisez un nombre max d'itération"
    Read *, kmax
    
    Iteration : Do While (k<=kmax .and. Norme(r)>tolerance)
       z=ProdMatVect(A,r)
       alpha=ProdScal(r,z)/ProdScal(z,z)
       x=x+alpha*r
       r=r-alpha*z
       k=k+1
    End Do Iteration
    !x est donc l'itération final à partir d'ici

    Anomalie :If (k>kmax) Then
       Print *,'La tolérance non atteinte',Norme(r)
    End If Anomalie

  End Subroutine RESIDUMINIMUM
  
End Module mod_solveur
