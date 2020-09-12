 Title: Boston Housing Prices Prediction Model Development in R/ R.Studio

#### Start by cleaning the R environment & Loading the dataset in R


rm(list=ls()) 

housing<-read.csv('Housing_data.csv',header=TRUE,sep=',')

housing 



#### Exploratory Data Analysis

str(housing)

any(is.na(housing))

summary(housing)

#### Data Visualization 

# Plotting of all the variables 
plot(housing)

# Visualizing the Target variable MEDV

plot(housing$MEDV)

# Histogram of the MEDV
ggplot(housing,aes(MEDV))+
  geom_histogram()
# Density Plot of MEDV
ggplot(housing,aes(MEDV)) +
  stat_density() + 
  theme_bw()

# Visualizing all the variables for btter understanding and to look for outliers
boxplot(housing)

# Checking variables with outliers seperately 
boxplot(housing$PCCR)
boxplot(housing$PRLZ)
boxplot(housing$AVR)
boxplot(housing$DIS)
boxplot(housing$MEDV)

### Checking for Correlation 

cor(housing)

# Visualizing Correlation 
library(corrplot) #for correlation visualization 

corrplot(cor(housing),'number')

#### Sample Regression Equation for multivariate regression analysis 

# MEDV(Pred)= theta0+thata1*(housing$PCCR)+theta2*(housing$PRLZ)+theta3*(housing$INDUS)+theta4*(housing$NOX)+
# theta5*(housing$AVR)+theta6*(housing$AGE)+theta7*(housing$DIS)+theta8*(housing$RAD)+theta9*(housing$TAX)

#### Preparing regression model with respect to the equation using lm function 

Regmodel<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX,housing)
summary(Regmodel)

# Step-by-step improving the model 

Regmodel<-lm(MEDV~PCCR+PRLZ+INDUS+NOX+AVR+AGE+DIS+RAD+TAX+0,housing) #Model with intercept removed 
summary(Regmodel)

Regmodel<-lm(MEDV~PCCR+PRLZ+NOX+AVR+AGE+DIS+RAD+TAX+0,housing) # Final Model with intercept & INDUS removed 
summary(Regmodel)

#### Plotting original vs fitted values

ggplot(housing,aes(x=1:length(MEDV),y=MEDV))+
  geom_line(col='blue')+
  geom_point(col='blue')+
  geom_line(aes(x=1:length(MEDV),y=fitted.values(Regmodel)),col='red')+
  geom_point(aes(x=1:length(MEDV),y=fitted.values(Regmodel)),col='red')+
  xlab("Observations")+ 
  ylab("Median Value of House in $1000s")+
  ggtitle('Original ("Blue") vs Fitted ("Red") Values')

#### Information on the model

coef(Regmodel)

fitted.values(Regmodel)

# Calculating and Checking Residuals in the model

Res <- residuals(Regmodel)

#### Plotting residuals and checking for normal distribution 

plot(Res,xlab="Observations",ylab="Residuals",main="Residual Analysis",col="blue",pch=16,ylim=c(-20,20))
abline(a=0,b=0,col="red",lty=2)
abline(3*sd(Res),0,col="red",lty=2)
abline(-3*sd(Res),0,col="red",lty=2)

# Density plotting for distribution check
plot(density(Res))



#### Making predictions model based on the coefficients 

ypred=coef(Regmodel)[1]*housing$PCCR+coef(Regmodel)[2]*housing$PRLZ+coef(Regmodel)[3]*housing$NOX+coef(Regmodel)[4]*housing$AVR+
  coef(Regmodel)[5]*housing$AGE+coef(Regmodel)[6]*housing$DIS+coef(Regmodel)[7]*housing$RAD+coef(Regmodel)[8]*housing$TAX


#### Create own cost function for calculation of errors

mycost<-function(yhat,y){
  cost=sum((yhat-y)^2)/length(y)
  return(cost)
}

#Calculated MSE for the predictions based on model and then rounded to 2 decimel points
mse<- mycost(ypred,housing$MEDV)
mse<-round(mse,digits = 2)


#### Create data frame for testing values for fututre housing prices

PCCR<-c(0.03221,0.06733,0.04211,0.05328)
PRLZ<-c(4,5,0,8)
INDUS<-c(4.21,7.21,3.33,2.65)
NOX<-c(0.432,0.443,0.454,0.476)
AVR<-c(6.121,6.783,6.345,6.754)
AGE<-c(64.2,67.8,62.5,59.8)
DIS<-c(4.1300,4.9832,5.0322,6.0412)
RAD<-c(3,2,2,1)
TAX<-c(254,267,231,255)

#Making a data frame of new values 
df<-data.frame(PCCR,PRLZ,INDUS,NOX,AVR,AGE,DIS,RAD,TAX)

#Making the predictions based on regression model for new data frame 
Pred<-predict(Regmodel,df)

# Values rounded to 2 decimel place
Pred<-round(Pred,digits = 2)

