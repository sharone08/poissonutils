#PARTIE1
#1
intall.packages("ggplot2","dplyr","rio")
library(ggplot2)
#2
  #Methode 1
notes <- c(12.5,8,15,NA,17.5,9,11,NA)
  #Methode2
notes <- array(c(12.5,8,15,NA,17.5,9,11,NA))
 
notes
#3
  #longueur
length(notes)
  #attributs intrinsèques
C <- c(mode(notes),length(notes))
C
  #moyenne
M <- mean(notes)
M
#4
(notes[is.na(notes)] <- 0)
notes
#5
sup<- notes[notes>=10]
sup
#6
etudiants <- paste(c("E"),1:8)
etudiants
#Nommer les élements de notes
Nom <-
  #7
filiere <- c("SDIA","GI","SDIA","GC","GI","SDIA","GC","GI")
filieref <- factor(filiere)
table(filiere)

#PARTIE2
#8
M <- array(c(1:16),dim = c(4,4))
M
#9
Md <- diag(M)
Md
#10
I <- array(c(1,1,1,2,2,3,2,3,4,3,4,4),dim = c(6,2))
M[I] <- 0
M
#11
eigen(M)$val
eigen(M)$vec
#12
t(M)
solve(M)
#13
MTM <- M%*%t(M)
MTM
MM <- M*t(M)
MM
#14
classe <- data.frame(
  etu
  
)

#PARTIE3

#15

library(datasets)
?iris
head(iris)
#16
library(ggplot2)
ggplot(iris,aes(Petal.Width,Petal.Length),color=factor(Species))+
  geom_point()
#17
ggplot(iris,aes(Petal.Width,Petal.Length),color=factor(Species))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title="DESCRIPTION DU PETAL",
    x="Largeur du petal",
    y="Longueur du petal"
  )
#18
ggplot(iris,aes(Sepal.Length))+
  geom_histogram(bins=20)
ggplot(iris,aes(Sepal.Length))+
  geom_histogram(bins=20)+
  facet_wrap(~Species)

#19
ggplot(iris,aes(Sepal.Width))+
  geom_boxplot()+
  facet_wrap(~Species)
  theme_minimal()
#20
  ggplot(iris,aes(Petal.Length),alpha=.4,color=factor(Species))+
    geom_density()+
    facet_grid(~Species)
  
#21
  grah <- ggplot(iris,aes(Sepal.Width))+
    geom_boxplot()+
    facet_wrap(~Species)
  theme_minimal()
  ggsave(iris_boxplot.png,ggplot=graph,width = 8,height = 9)
    



