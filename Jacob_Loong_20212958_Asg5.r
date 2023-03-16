library(tidyverse)
library(ggpubr)

#Q1
library(ISLR2)
Hitters <- na.omit(Hitters)
?Hitters
#League, Division, and NewLeague are categoricla

#Q2a
Hitters %>% 
  ggplot(mapping=aes(x=Years,y=Salary))+
  geom_point()
#Q2b
Hitters %>% 
  ggplot(mapping=aes(x=Years,y=Salary,color=Division))+
  geom_point()

#Q3a
Hitters %>% 
  ggplot(mapping=aes(x=Salary))+
  geom_histogram(bins=20)
#Most players have a low salary while very few players have a comparatively high salary

#Q3b
Hitters %>% 
  ggplot(mapping=aes(x=Salary))+
  geom_density()

#Q3c
ggarrange(Hitters %>% 
            ggplot(mapping=aes(x=Salary))+
            geom_histogram(bins=20),
          Hitters %>% 
            ggplot(mapping=aes(x=Salary))+
            geom_density())
#