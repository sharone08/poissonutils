#LISTS AND DATAFRAMES
#DECLARATION D4UNE LISTE
mylist <- list("Douala",5,NA,name="Sharone")
#AFFICHER UNE LISTE
mylist
#DETERMINER LA TAILLE D'UNE LISTE
length(mylist)
#AFFICHER LE PREMIER ELEMENT D'UNE LISTE
mylist[1]
mylist2 <- list("a",mylist)
mylist2[2]
mylist2[[2]]
#ON PEUT CONCATENER LES LISTES
mylist <- c(mylist1,mylist2)

#UN DATAFRAME 
data.frame(
  
)

#POUR CHARGER UN PACKAGE
library(datasets)
require(datasets)
?datasets
library(help = "datasets")
summary(mtcars)
head(mtcars)
plot(mtcars)
#POUR DETACHER UN PACKAGE
detach("package:datasets",unload=TRUE)
head(mtcars)
library(datasets)
mtcars
?tidyverse
