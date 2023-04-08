library(tidyverse)
library(ggpubr)

#Q1
library(ISLR2)
Hitters <- na.omit(Hitters)
?Hitters
#League, Division, and NewLeague are categorical

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
#The histogram shows an increase in count at salary around 750 
#while the kernel density does not demonstrate this dramatic increase

#Q4a
lm(Salary ~ CHits, data = Hitters)
#Q4b
#Every 1 increase in CHits there is a 0.382 increase in Salary

#Q5a
CHits_quan <- quantile(Hitters$CHits, c(0.2, 0.4, 0.6, 0.8))
cat_CHits <- rep(0, nrow(Hitters))
cat_CHits[Hitters$CHits <= CHits_quan[1]] <- 1
cat_CHits[Hitters$CHits > CHits_quan[1] & Hitters$CHits <= CHits_quan[2]] <- 2
cat_CHits[Hitters$CHits > CHits_quan[2] & Hitters$CHits <= CHits_quan[3]] <- 3
cat_CHits[Hitters$CHits > CHits_quan[3] & Hitters$CHits <= CHits_quan[4]] <- 4
cat_CHits[Hitters$CHits > CHits_quan[4]] <- 5
cat_CHits <- factor(cat_CHits)
lm(Salary ~ cat_CHits, data = Hitters)
#Q5b
#The first 20% quantile of CHits has a smaller effect on Salary
# than the later quantiles. There is also little difference on Salary
# by the 60% and 80% quantiles

#Q6
set.seed(1)
index <- sample(nrow(Hitters), nrow(Hitters) * 0.5)
train <- Hitters[index, ]
test <- Hitters[-index, ]
#Q6a
fit <- lm(Salary ~ AtBat + Hits + HmRun + Runs + RBI + Walks +
     Years + CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks +
     League + Division + PutOuts + Assists + Errors +
     NewLeague, data = train)
#Q6b
summary(fit)
#AtBat, CRuns, PutOuts, Assists

#Q6c
#For every 1 increase in CHits there is a -0.01708 decrease in Salary

#Q7a
mean((train$Salary - predict(fit,train))^2, na.rm=TRUE)
#Q7b
mean((test$Salary - predict(fit,test))^2, na.rm=TRUE)

#Q8
iris_train <- read.csv("iris_train.csv")
iris_test <- read.csv("iris_test.csv")
iris_train$versicolor <- as.numeric(iris_train$Species == "versicolor")
iris_test$versicolor <- as.numeric(iris_test$Species == "versicolor")

log_fit <- glm(versicolor ~ Sepal.Length + Sepal.Width + 
                 Petal.Length + Petal.Width, data = iris_train, family = binomial)
summary(log_fit)

#Q9
#Sepal.Width would be most important for if the iris is versicolor

#Q10
prob <- predict(log_fit, iris_test, type = "response")
predicted_class <- ifelse(prob > 0.5,1,0)

mean(predicted_class == iris_test$versicolor)
