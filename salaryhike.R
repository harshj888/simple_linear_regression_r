Salary_Data <- read.csv("D:/STUDY/Excelr Assignment/Assignment 4 - Simple Linear Regression/Salary_Data.csv")
View(Salary_Data)
attach(Salary_Data)
plot(Salary_Data)
summary(Salary_Data)

library(e1071) 
library(car)

# First Moment Business Decision
sd(YearsExperience)
sd(Salary)


# Second Moment Business Decision
var(YearsExperience)
var(Salary)
boxplot(YearsExperience)
boxplot(Salary) 

# Third Moment Business Decision
skewness(YearsExperience)
hist(YearsExperience)
hist(Salary)

# Fourth Moment Business Decision
kurtosis(YearsExperience)
kurtosis(Salary)

#Scatter plot
plot(Salary_Data$YearsExperience,Salary_Data$Salary)  # plot(X,Y)
#Correlation Coefficient (r)
cor(YearsExperience, Salary)             # cor(X,Y)

# Correlation Plot
library(corrplot)
Salary_Data.cor = cor(Salary_Data)
corrplot(Salary_Data.cor) 

#checking for na Values
is.na(Salary_Data)

# Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(Salary_Data))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = Salary_Data, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Salary_Data, aes(x=YearsExperience, y=pred))

# Logrithamic Model

plot(log(YearsExperience), Salary)
cor(log(YearsExperience), Salary)

reg_log <- lm(Salary ~ log(YearsExperience))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(Salary_Data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

# Exponential Model

plot(YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))

reg_exp <- lm(log(Salary) ~ YearsExperience)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(YearsExperience, Salary)
plot(YearsExperience*YearsExperience, Salary)

cor(YearsExperience*YearsExperience, Salary)

plot(YearsExperience*YearsExperience, log(Salary))

cor(YearsExperience, log(Salary))
cor(YearsExperience*YearsExperience, log(Salary))

reg2degree <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = Salary_Data$Salary - expy

sqrt(sum(err^2)/nrow(Salary_Data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = Salary_Data, aes(x = YearsExperience + I(YearsExperience^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Salary_Data, aes(x=YearsExperience+I(YearsExperience^2), y=logpol))

