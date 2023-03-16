library(tidyverse)
library(nycflights13)
library(gcookbook)
library(MASS)

#Q1a
flights %>% 
  filter(month==2) %>% 
  ggplot(mapping = aes(x=dep_delay)) +
  geom_histogram(binwidth = 10)

#Q1b
flights %>% 
  filter(month==2,dep_delay < 100) %>% 
  ggplot(mapping = aes(x=dep_delay)) +
  geom_histogram(binwidth = 5)

#Q2a
flights %>% 
  filter(month==1,day==1,dep_delay < 180) %>% 
  ggplot(mapping = aes(x=dep_delay,y=arr_delay)) +
  geom_point()

#Q2b
flights %>% 
  filter(month==1,day==1,dep_delay < 10) %>% 
  ggplot(mapping = aes(x=dep_delay,y=arr_delay)) +
  geom_point()

#Q2c
#Graph A shows a more apparent trend between the delay times

#Q2d
flights_filtered_a <- flights %>% 
  filter(month==1,day==1,dep_delay < 180)

flights_filtered_b <- flights %>% 
  filter(month==1,day==1,dep_delay < 10)

x_a <- flights_filtered_a$dep_delay
y_a <- flights_filtered_a$arr_delay

x_b <- flights_filtered_b$dep_delay
y_b <- flights_filtered_b$arr_delay

cor(x_a,y_a,use="complete.obs")
cor(x_b,y_b,use="complete.obs")
#The correlation for the points in graph b 
#is significantly lesser than that of graph a. This
#makes sense, as evident by my observation in #Q2c

#Q3
plot_delay <- function(which_month, which_day, lower_range, upper_range){
  flights %>% 
    filter(month==which_month, day==which_day,
           dep_delay >= lower_range, dep_delay <= upper_range) %>% 
    ggplot(mapping = aes(x=dep_delay,y=arr_delay)) +
    geom_point()
}

#Q4a
flights %>% 
  ggplot(mapping = aes(x=factor(month))) +
  geom_bar()

#Q4b
flights %>% 
  filter(month==1) %>% 
  ggplot(mapping = aes(x=factor(day))) +
  geom_bar()

#Q4c
flights %>% 
  filter(month==1) %>% 
  group_by(day) %>% 
  summarize(mean_arr_delay = mean(arr_delay, na.rm=TRUE)) %>%
  ungroup() %>% 
  ggplot(mapping = aes(x=factor(day),y=mean_arr_delay)) +
  geom_col()

#Q4d
#WIP
flights %>% 
  filter((month == 1 & day == 6 | day == 13 | day == 20 | day == 27) | (month == 2 & day == 3 | day == 10 | day == 17 | day == 24)) %>% 
  group_by(day) %>% 
  summarize(mean_arr_delay=mean(arr_delay,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=factor(day),y=mean_arr_delay)) +
  geom_col() +
  xlab("Saturdays in Jan/Feb") +
  ylab("Average Arrival Delay")

#Q5
flights %>% 
  group_by(month,day) %>% 
  summarize(mean_dep_delay=mean(dep_delay,na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=1:365,y=mean_dep_delay)) +
  geom_line()

#Q6
library(MASS)
birthwt %>% 
  filter(age>=25) %>% 
  ggplot(mapping = aes(x=bwt)) +
  geom_density()

#Q7a
ggplot(data=mpg)
#I see nothing?

#Q7b
mpg %>% 
  ggplot(mapping=aes(x=drv,y=class)) +
  geom_point()
#It doesn't allow me to see trends or relationships

#Q7c
#mapping=aes(...) should go next to data in ggplot while color should go in geom_point(), not in mapping

#Q7d
ggplot(data = mpg,mapping = aes(x = displ, y = hwy)) +
  geom_point(color = "blue")

#Q8
flights %>% 
  group_by(origin) %>% 
  summarize(avg_delay=c(mean(arr_delay,na.rm=TRUE),mean(dep_delay,na.rm=TRUE)),
            type=c("arrival","departure")) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=origin,y=avg_delay,fill=type)) +
  geom_col(position="dodge") +
  ylab("Average Delay") +
  xlab("origin") +
  ggtitle("Average Arrival Delay and Departure Delay by Origins")

#Q9a
diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price=mean(price,na.rm=TRUE)) %>% 
  ungroup()
#The results dont make sense since the price should increase steadily as cut quality increases

#Q9b
diamonds %>% 
  group_by(cut) %>% 
  ggplot(mapping=aes(x=price)) +
  geom_histogram() +
  facet_grid(cut~.,scales="free_y")
ungroup(diamonds)
#This is unexpected, you'd expect that a greater quality cut would yield a higher price

#Q9c
diamonds %>% 
  group_by(cut) %>% 
  filter(carat<=1,carat>=0.9) %>% 
  ggplot(mapping=aes(x=price)) +
  geom_histogram() +
  facet_grid(cut~.,scales="free_y")
ungroup(diamonds)

#Q9d
diamonds %>% 
  filter(cut=="Ideal",clarity=="VS2") %>% 
  ggplot(mapping=aes(y=price,x=carat,color=color)) +
  geom_point(size=0.9)
#As carat increases, as does price
#As color changes from D to J, the price decreases

#Q10a
#se in geom_smooth() displays the confidence interval (CI) around smooth

#Q10b
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

#Q10c
ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy,group=drv),level=0)

#Q10d
ggplot() +
  geom_point(data=mpg,mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data=filter(mpg,drv=="r"),
              mapping = aes(x = displ, y = hwy,group=drv),level=0)

#Q10e
ggplot() +
  geom_point(data=mpg,mapping = aes(x = displ, y = hwy,color=drv)) +
  geom_smooth(data=mpg,mapping = aes(x = displ, y = hwy,color=drv),level=0)

#Q10f
ggplot() +
  geom_point(data=mpg,mapping = aes(x = displ, y = hwy,color=drv)) +
  geom_smooth(data=mpg,mapping = aes(x = displ, y = hwy),level=0)
