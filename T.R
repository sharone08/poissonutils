#exercices de révision
library(ggplot2)
#faire des graphques : nuage de points+ droite de régression(mtcars)
ggplot(mtcars, aes(x=wt,y=mpg, color=factor(cyl)))+
  geom_point(size=3)+
  geom_smooth(method = "lm",se=FALSE)+
  labs(title="Consommation selon le poids",
       x="poids(1000 lbs")