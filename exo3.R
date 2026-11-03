#VISUALISATION DES DONNEES AVEC ggplot2- ADVANCED GRAPHICS

  #LA GRAMMAIRE DES GRAPHIQUES 
library(tidyverse)
#IMPORTANT:La grammaire des graphiques reposent sur les data,le mapping et geometry
plot(cars)
plot(mpg)
library(ggplot2)
ggplot(mpg,aes(x=displ,y=hwy))+geom_point()
#colour=color
ggplot(mpg,aes(displ,cty,colour=class))+geom_point()
ggplot(mpg,aes(displ,cty,color=class))+geom_jitter()
ggplot(mpg,aes(drv,hwy))+geom_jitter()
ggplot(mpg,aes(drv,hwy))+geom_boxplot()
ggplot(mpg,aes(drv,hwy))+geom_violin()
ggplot(mpg,aes(manufacturer))+geom_bar()
ggplot(economics,aes(unemploy/pop,uempmed))+geom_path()+geom_point()
ggplot(mpg,aes(displ,hwy,color=factor(cyl)))+geom_point()
library(ggridges)
library(gapminder)
library(patchwor)
library(viridis)
library(hrbrthemes)
library(tidyverse)
theme_set(theme_bw())
gapminder %>%
  filter(continent %in% c ("Africa","Europe"))%>%
  filter(gdpPercap<30000)%>%
  ggplot(aes(x=gdpPercap,
             y=lifeExp,
             size=pop,
             color=year))+
  geom_point()+
  facet_wrap(~continent)+
  labs(title="Life expectancy explained by GDP per capita",
       x="GDP per capita",
       y="Life expectancy")
gapminder       
#facet wrap c'est pour que les 2 graphiques soient sur la meme image
#pour avoir le sens des entetes d'un paquet
head(gapminder)
starwars
p1 <- starwars %>%
  ggplot(aes(x=height))+
           geom_histogram(binwidth = 20,
                          show.legend = F,
                          alpha=.5)+
           labs(title="Histogram",
                x="Height",
                y="Count")
p2 <- starwars %>% 
  ggplot(aes(x=height))+
  geom_density(aes(fill="blue"),
               show.legend = F,
               alpha=.5)+
               labs(
                 title="Density plot",
                 x="Height"
                 y="Probability")
p1/p2
p1|p2