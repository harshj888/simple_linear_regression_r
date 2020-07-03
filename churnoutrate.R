emp_data <- read.csv("D:/STUDY/Excelr Assignment/Assignment 4 - Simple Linear Regression/emp_data.csv")
View(emp_data)
attach(emp_data)
plot(emp_data)
summary(emp_data)

library(e1071) 
library(car)

# First Moment Business Decision
sd(Churn_out_rate)
sd(Salary_hike)


# Second Moment Business Decision
var(Salary_hike)
var(Churn_out_rate)
boxplot(Salary_hike)
boxplot(Churn_out_rate) 

# Third Moment Business Decision
skewness(Salary_hike)
hist(Salary_hike)
hist(Churn_out_rate)

# Fourth Moment Business Decision
kurtosis(Salary_hike)
kurtosis(Churn_out_rate)

#Scatter plot
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)  # plot(X,Y)
#Correlation Coefficient (r)
cor(Salary_hike, Churn_out_rate)             # cor(X,Y)

# Correlation Plot
library(corrplot)
emp_data.cor = cor(emp_data)
corrplot(emp_data.cor) 

#checking for na Values
is.na(emp_data)

# Simple Linear Regression model
reg <- lm(Churn_out_rate ~ Salary_hike) # lm(Y ~ X)
summary(reg)
pred <- predict(reg)
reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike, y=pred))

# Logrithamic Model

plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)

reg_log <- lm(Churn_out_rate ~ log(Salary_hike))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

# Exponential Model

plot(Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))

reg_exp <- lm(log(Churn_out_rate) ~ Salary_hike)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

confint(reg_exp,level=0.95)
predict(reg_exp,interval="confidence")

# Polynomial model with 2 degree (quadratic model)

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike*Salary_hike, Churn_out_rate)

cor(Salary_hike*Salary_hike, Churn_out_rate)

plot(Salary_hike*Salary_hike, log(Churn_out_rate))

cor(Salary_hike, log(Churn_out_rate))
cor(Salary_hike*Salary_hike, log(Churn_out_rate))

reg2degree <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike*Salary_hike))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = emp_data$Churn_out_rate - expy

sqrt(sum(err^2)/nrow(emp_data))  #RMSE

confint(reg2degree,level=0.95)
predict(reg2degree,interval="confidence")

# visualization
ggplot(data = emp_data, aes(x = Salary_hike + I(Salary_hike^2), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Salary_hike+I(Salary_hike^2), y=logpol))



