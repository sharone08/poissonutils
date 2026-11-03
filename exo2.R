#Creation d'une matrice
  #En precisant toutes les dimensions 
matrix(1:6, nrow=2, ncol = 3)
  #En ne precisant qu'une dimension
matrix(1:6, nrow=2)
#ECRITURE D'UNE MATRICE DONNEE
A <-matrix(c(2,0,1,5,2,0,-1,-2,4,3,1,0,0,6,1,4,1,3,2,5),nrow=4, ncol=5)
#Affiher A
A
#OPERATIONS SUR LES MATRICES
B<-array(c(1,3,5,-3,6,8,-1,6,4,8,-2,0), dim=c(3:4))
B
D<-array(c(-1,3,0,-9,7,-4,-2,8,2,0,-6,7), dim=c(3:4))
D
