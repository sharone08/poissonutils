#DATA TRANSFORMATION
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
flights
view(flights)
filter(flights, month==1, day==1)
jan1 <-  (filter(flights, month==1, day==1))
filter(flights, month==12, day==24)
df <- tibble(x=c(1,NA,3))
filter(df,x>1)
filter(df,is.na(x)|x>1)
arrange(flights,year,month,day)
flights
select(flights,year:day)
select(flights, -(year:day))
?select
