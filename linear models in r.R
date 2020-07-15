####  ------------   problem 1--------------------#

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

qqnorm( residuals(relation))
qqline( residuals(relation))

plot(residuals(relation))    # plot(fitted(relation),residuals(relation))
     
plot(rstandard(relation))
     
#####--------------- problem 2--------------
     
height = c(100, 200, 300, 450, 600, 800, 1000)
distance = c(253, 337, 395, 451, 495, 534, 574) # data from Galileo
##   distance = a + b1*height + b2*height^2 +  e,   e ~ N(0,s^2)

##  Ho: b1 = b2= 0
##  Ha: at least one b not equal 0

model.r = lm(distance ~ height + I(height^2))   # a quadratic model 
model.r     

## Also, it is possible to use 
## mod.r = lm( distance ~ poly(height,2,raw=TRUE))

## raw  --  if true, use raw and not orthogonal polynomials
# Thus, in the raw coding you can only interpret the p-value of height if height^2
# remains in the model
## mod.r

# However, in the orthogonal coding height^2 only captures the quadratic part
# that has not been captured by the linear term.

summary(model.r)
newh = seq(100, 1000, 10)  #  100 110  120 ... 990 1000
newh
fit = 200.211950 + 0.706182*newh - 0.000341*newh^2   
fit   # predicted values for distance
     
predict( model.r, data.frame(height=newh))
     
plot(height, distance, col='red') # original data
lines(newh, fit, lty=1,col='blue') # display best fit 

par( mfrow=c(2,2))
plot( model.r)
par(mfrow=c(1,1))

######### ----------------  problem 3-----------------#
     
   # mpg: miles per gallon
   # disp: displacement
   # hp: horse power
   # wt: weight of the car
     
input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))
           
  # Create the relationship model.
  #  mpg = a + b1*disp + b2*hp + b3*wt + e
  #  e ~ N(0,s^2)      , test these assumptions
mymodel <- lm(mpg~disp+hp+wt, data = input)

 # Show the model.
summary(mymodel)

confint(mymodel)

coef(mymodel)
           
a <- coef(mymodel)[1]
print(a)

## write Equation

# mpg =  37.1055 - 0.000937*disp -0.031156*hp  - 3.80089wt

# Test model assumptions
           
par(mfrow=c(2,2))
plot(mymodel)
par(mfrow=c(1,1))
           
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(mymodel, which = c(1, 2)) # "which" argument optional
par(mfrow=c(1,1))

shapiro.test(residuals(mymodel))   # test normality
           
## predicting new values
myvalues <- data.frame(disp=170, hp=100,wt= 2.4)
result <-  predict(mymodel,myvalues)
result

mymodel2= lm(mpg~hp+wt, data = input)
summary(mymodel2)
plot(mymodel2)

## Hypothesis   Ho: model1 = model2
# anova(reduced, full)

anova(mymodel2,mymodel)

MSE=(summary(mymodel)$sigma)^2 # store MSE for the full model
step(mymodel, scale=MSE, direction="backward")

## other functions
##step <- stepAIC(mymodel, direction="both")


# choose model with smallest AIC
