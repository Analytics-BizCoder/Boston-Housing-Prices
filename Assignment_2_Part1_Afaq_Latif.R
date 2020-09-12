#Assignment No 2_Part 1
#Regression Analysis 

rm(list=ls())

my.data<-read.csv('Housing_data.csv',header=TRUE,sep=',')

my.data 

str(my.data)

# Exploratory Data Analysis 
any(is.na(my.data))

summary(my.data)

# Visualizing the Target variable MEDV

plot(my.data$MEDV)

ggplot(my.data,aes(x=MEDV))+
  geom_histogram()

ggplot(my.data,aes(MEDV)) +
  stat_density() + 
  theme_bw()

# Visualizing all the variables for btter understanding and to look for outliers
boxplot(my.data)

# Checking variables with outliers seperately 
boxplot(my.data$PCCR)
boxplot(my.data$PRLZ)
boxplot(my.data$AVR)
boxplot(my.data$DIS)
boxplot(my.data$MEDV)

# Checking for Correlation 
cor(my.data)

# Visualizing Correlation 
library(corrplot) #for correlation visualization 

corrplot(cor(my.data),'number')

# Sample Regression Equation for multivariate regression analysis 

# MEDV(Pred)= theta0+thata1*(my.data$PCCR)+theta2*(my.data$PRLZ)+theta3*(my.data$INDUS)+theta4*(my.data$NOX)+
# theta5*(my.data$AVR)+theta6*(my.data$AGE)+theta7*(my.data$DIS)+theta8*(my.data$RAD)+theta9*(my.data$TAX)
  
# Preparing regression model with respect to the equation using lm function 

Regmodel<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,my.data)
summary(Regmodel)

# Step-by-step improving the model 

Regmodel<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX+0,my.data) #Model with intercept removed 
summary(Regmodel)

Regmodel<-lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0,my.data) # Final Model with intercept & INDUS removed 
summary(Regmodel)

# Plotting original vs fitted values

ggplot(my.data,aes(x=1:length(MEDV),y=MEDV))+
  geom_line(col='blue')+
  geom_point(col='blue')+
  geom_line(aes(x=1:length(MEDV),y=fitted.values(Regmodel)),col='red')+
  geom_point(aes(x=1:length(MEDV),y=fitted.values(Regmodel)),col='red')+
  xlab("Observations")+ 
  ylab("Median Value of House in $1000s")+
  ggtitle('Original ("Blue") vs Fitted ("Red") Values')

# Information on the model

coef(Regmodel)

fitted.values(Regmodel)

# Calculating and Checking Residuals in the model

Res <- residuals(Regmodel)

# Plotting residuals and checking for normal distribution 

plot(Res,xlab="Observations",ylab="Residuals",main="Residual Analysis",col="blue",pch=16,ylim=c(-20,20))
abline(a=0,b=0,col="red",lty=2)
abline(3*sd(Res),0,col="red",lty=2)
abline(-3*sd(Res),0,col="red",lty=2)

# Density plotting for distribution check
plot(density(Res))

#Extra Tasks for Answers to Questions

# Making predictions model based on the coefficients 
ypred=coef(Regmodel)[1]*my.data$PCCR+coef(Regmodel)[2]*my.data$PRLZ+coef(Regmodel)[3]*my.data$NOX+coef(Regmodel)[4]*my.data$AVR+
  coef(Regmodel)[5]*my.data$AGE+coef(Regmodel)[6]*my.data$DIS+coef(Regmodel)[7]*my.data$RAD+coef(Regmodel)[8]*my.data$TAX

# Created my own cost function for calculation of errors
mycost<-function(yhat,y){
  cost=sum((yhat-y)^2)/length(y)
  return(cost)
}

#Calculated MSE for the predictions based on model 
mse<- mycost(ypred,my.data$MEDV)


# Created data frame for testing values for fututre housing prices
PCCR<-c(0.03221,0.06733,0.04211,0.05328)
PRLZ<-c(4,5,0,8)
INDUS<-c(4.21,7.21,3.33,2.65)
NOX<-c(0.432,0.443,0.454,0.476)
AVR<-c(6.121,6.783,6.345,6.754)
AGE<-c(64.2,67.8,62.5,59.8)
DIS<-c(4.1300,4.9832,5.0322,6.0412)
RAD<-c(3,2,2,1)
TAX<-c(254,267,231,255)

df<-data.frame(PCCR,PRLZ,INDUS,NOX,AVR,AGE,DIS,RAD,TAX)

#Making the predictions
Pred<-predict(Regmodel,df)

Pred<-round(Pred,digits = 2)
