library(e1071) 
library(car)
calories <- read.csv("D:/STUDY/Excelr Assignment/Assignment 4 - Simple Linear Regression/calories_consumed.csv")
View(calories)
attach(calories)
summary(calories)

# First Moment Business Decision
sd(Weight.gained..grams.)
sd(Calories.Consumed)


# Second Moment Business Decision
var(Weight.gained..grams.)
var(Calories.Consumed)
boxplot(Weight.gained..grams.)
boxplot(Calories.Consumed) 

# Third Moment Business Decision
skewness(Weight.gained..grams.)
hist(Weight.gained..grams.)
hist(Calories.Consumed)

# Fourth Moment Business Decision
kurtosis(Weight.gained..grams.)
kurtosis(Calories.Consumed)

#Scatter plot
plot(calories$Weight.gained..grams.,calories$Calories.Consumed)  # plot(X,Y)
#Correlation Coefficient (r)
cor(Weight.gained..grams., Calories.Consumed)             # cor(X,Y)

# Correlation Plot
library(corrplot)
calories.cor = cor(calories)
corrplot(calories.cor) 

#checking for na Values
is.na(calories)

# Simple Linear Regression model
reg <- lm(Calories.Consumed ~ Weight.gained..grams.) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(calories))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
install.packages("ggplot2")
library(ggplot2)
ggplot(data = calories, aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories, aes(x=Weight.gained..grams., y=pred))

# Logrithamic Model

plot(log(Weight.gained..grams.), Calories.Consumed)
cor(log(Weight.gained..grams.), Calories.Consumed)

reg_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(calories))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

# Exponential Model

plot(Weight.gained..grams., log(Calories.Consumed))

cor(Weight.gained..grams., log(Calories.Consumed))

reg_exp <- lm(log(Calories.Consumed) ~ Weight.gained..grams.)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(Weight.gained..grams., Calories.Consumed)
plot(Weight.gained..grams.*Weight.gained..grams., Calories.Consumed)

cor(Weight.gained..grams.*Weight.gained..grams., Calories.Consumed)

plot(Weight.gained..grams.*Weight.gained..grams., log(Calories.Consumed))

cor(Weight.gained..grams., log(Calories.Consumed))
cor(Weight.gained..grams.*Weight.gained..grams., log(Calories.Consumed))

reg2degree <- lm(log(Calories.Consumed) ~ Weight.gained..grams. + I(Weight.gained..grams.*Weight.gained..grams.))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = calories$Calories.Consumed - expy

sqrt(sum(err^2)/nrow(calories))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = calories, aes(x = Weight.gained..grams. + I(Weight.gained..grams.^2), y = log(Calories.Consumed))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories, aes(x=Weight.gained..grams.+I(Weight.gained..grams.^2), y=logpol))
