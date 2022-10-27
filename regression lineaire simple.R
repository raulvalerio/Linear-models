            #########################################
            #   --------   raulvalerio@gmail.com----#
            #  --------  simple Linear regression R #
            #########################################


#########################################
#---------------- Synthetic data--------#
#########################################

set.seed(2)

### ----------  Problem 1 -------------
           
velo1 = rnorm(1000, m= 6, sd = 3)

x= 3 + velo1 +rnorm(1000)

m1= lm(x ~ velo1)   ##  lm( data= data, output ~ variables)

summary(m1)

## Assumptions

plot(m1)

##  linearity

plot( residuals(m1), x )

## independence

plot(residuals(m1) )     ##   m1$residuals

## normality

qqnorm(m1$residuals)
qqline(m1$residuals)


shapiro.test( m1$residuals)    # Shapiro test

## equal variance   / Homocedasticity

plot( m1$fitted.values, m1$residuals)


## testing  homoskedasticity

# load library
library(lmtest)

# perform test,,   Breusch - Pagan
bptest(m1)


### --  what about a polynomial??

velo1 = rnorm(1000, m= 6, sd = 3)

x= 3 + velo1^2 +rnorm(1000)

plot(velo1, x)

m2 = lm(x  ~ velo1)  

sum_m2  = summary( m2)

## anova table
anova(m2)

##  r-squared

summary(m2)$r.squared

sum_m2$adj.r.squared

#  MSE

mean(m2$residuals^2)

##------------ fit poly------

m3 =  lm( x ~ velo1 + I(velo1^2) )   #  velo1 and I(velo1) could be correlated

summary(m3)

summary(m3)$r.squared

model <- lm( x ~ poly(velo1,2))  # producing orthogonal polynomials, better choice

summary(model)

plot(fitted(model),residuals(model))



predic_2 <- predict(model, list(velo1 = velo1), type="response")

predic_2 = predict( model, data.frame( velo1) )

head(predic_2)

plot(velo1, predic_2,col='deepskyblue4',xlab='velo1',main='Observed data')

plot(velo1, x,col='firebrick1')

library(ggplot2)

ggplot() +                    # basic graphical object
  geom_point(aes(x=velo1, y= x, colour="Obs")) +  # first layer
  geom_point(aes(x=velo1, y= predic_2, colour="Pred")) + # second layer 
  labs(title= "Real vrs Predicted")

##---------- Problem 2---------------


# Values of height   ---- predictor
# 151, 174, 138, 186, 128, 136, 179, 163, 152, 131

# Values of weight.  ---- response
# 63, 81, 56, 91, 47, 57, 76, 72, 62, 48

## ----  lm function ---

x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

plot(y,x)

# Apply the lm() function.

## model:     height = a + b*weight

## Proposed model is:  height=  a +  b*weight  + e
## e ~  N(0, s^2)
##
## Ho: b=0
## HA:  b not equal to 0

#  model = lm( formula= var1~ var2 ,data= mydata)

relation <- lm(x~y)

print(relation)    # or simply   relation

summary(relation)

## Equation

#   height = 61.3803 + 1.4153*weight

confint(relation)   # interval confidence

#  coef + (-1,1) * sigma^2 * t(alpha, df)

# Find height of a person with weight 75.
#predict
# predict( object, newdata)

a <- data.frame(y = 75)   # 75 kg

result <-  predict(relation,a)

result

61.3803 + 1.4153*75

plot(y,x,col = "blue",main = "Height & Weight Regression",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Weight (Kg)",ylab = "Height (cm)")

### -----  test model assumptions

par( mfrow=c(2,2))
plot( relation)
par(mfrow=c(1,1))


## element 7,  weight= 76
#  Individually 


## normality
qqnorm( residuals(relation))
qqline( residuals(relation))

# equal variance
 plot(fitted(relation),residuals(relation))

## independece
plot(residuals(relation))    


plot(rstandard(relation))

####################################
## ---  ANOVA   one way         ---#
####################################

head(salary)

