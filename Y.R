library(datasets)
data()
#charger gapminder
library(gapminder)
library(tidyverse)
#que fait gapminder
?gapminder
str(gapminder)
head(gapminder)
summary(gapminder)
table(gapminder$continent)
aggregate(lifeExp ~ continent, gapminder, median)
plot(lifeExp ~ year, gapminder, subset = country == "Cambodia", type = "b")
plot(lifeExp ~ gdpPercap, gapminder, subset = year == 2007, log = "x")
#Faire une nouvelle representation avec gapminder
gapminder |> 
  filter(continent%in% c("Africa","Europe")) |> 
  filter(gdpPercap <30000) |> 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=year))+
           geom_point() +
  facet_grid(~continent)
library(datasets)
#couleur varie en fonction de la classe
ggplot(mpg,aes(x=cyl, y=displ, color=class))+
  geom_point()

gapminder |> 
  filter(continent%in% c("Africa","Europe")) |> 
  filter(gdpPercap <30000) |> 
  ggplot(aes(x=gdpPercap, y=lifeExp, size=pop, color=year))+
  geom_point() +
  facet_wrap(~continent)+
  labs(title="Esperance de vie en fonction du PIB par habitant", 
  x="PIB par habitant", 
  y="Espérance de vie")

cc
CO2

#REPRESENTONS UN VOLCANO PLOT DE NOTRE CHOIX
?volcano
library(grDevices)
library(graphics)
library(tidyverse)
filled.contour(volcano, color= class) |> 
title(main="Traitement des plantes")
#gapminder |> 
  #filter(type==Quebec) |> 
#ggplot (CO2, aes(x="Type", y="Treatment"))+
 # geom_point()
#require(grDevices); require(graphics)
#filled.contour(volcano, color.palette = terrain.colors, asp = 1) |> 
    #title(main = "volcano data: filled contour map")


#
library(datasets)
? datasets
library(help="datasets")
AirPassengers  
head(AirPassengers  )
head(iris)
head(mtcars)
ggplot(mpg,aes(displ,hwy, color=factor(cyl)))+
  geom_point()
ggplot(mpg,aes(displ,hwy, color=factor(cyl)))+
  geom_line()
ggplot(mpg,aes(displ,hwy, color=factor(cyl)))+
  geom_bar(stat="identity", position="identity",fill=NA)
#geom_smoothpour la loi de régression linéaire
ggplot(mpg,aes(displ,hwy, color=factor(cyl)))+
  geom_point()+ geom_smooth(method="lm")
#geom_smooth()` using formula = 'y ~ x'
#method=lm permet d'afficher une courbe sous forme de droite sans cette methode le graphe montrera une courbe: interpolation linéaire
ggplot(mpg,aes(displ,hwy, color=factor(cyl)))+
  geom_point()+ 
  geom_smooth()+
  facet_wrap(~year)
#geom_smooth()` using method = 'loess' and formula = 'y ~ x'
ggplot(mpg,aes(displ,hwy)+
  geom_point()+ 
  geom_smooth()+
  facet_wrap(~year)+
    theme(legend.position="none")


