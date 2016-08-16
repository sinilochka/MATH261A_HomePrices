# change the directory to the folder that contains the data
homedata=read.csv("TeamHomeData.csv",
  colClasses=c('factor','factor','numeric','numeric','numeric','numeric','numeric'))
# the colClasses parameter tells R to load up the csv file with the data
# of the type as indicated

# check what the data looks like
homedata
summary(homedata)

plot(homedata$zip,homedata$price, ylim=c(100000,4000000), cex=0.80)
plot(homedata$type, homedata$price,ylim=c(100000,4000000), cex=0.80)
plot(homedata$beds, homedata$price,ylim=c(100000,4000000), cex=0.80)
plot(homedata$baths, homedata$price,ylim=c(100000,4000000), cex=0.80)
plot(homedata$sqft, homedata$price,ylim=c(100000,4000000), cex=0.80)
plot(homedata$lot, homedata$price,ylim=c(100000,4000000), cex=0.80)

# a simple linear model without the interaction terms created from homedata
full_model=lm(price~., data=homedata)
summary(full_model)
plot(fitted.values(full_model), rstudent(full_model), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(full_model))
abline(0,1)

# this will create a 2-predictor interaction of every variable with every other 
# variable using the homedata 
full_model.2=lm(price~.*., data=homedata)
summary(full_model.2)
plot(fitted.values(full_model.2), rstudent(full_model.2), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(full_model.2))
abline(0,1)
# mse=
(summary(full_model.2)$sigma)^2
summary(full_model.2)$r.squared

# To find the regressors that are significant, we can use forward selection or backward selection.
# First we set up a nullmodel
null_model=lm(price~1, data=homedata)

# forward selection
select_forward=step(null_model, scope=list(lower=null_model, upper=full_model.2), direction="forward")

# backward selection
select_backward=step(full_model.2, direction="backward")

# stepwise selection
select_stepwise=step(null_model, scope=list(lower= null_model, upper= full_model.2), direction = "both")

summary(select_forward)
summary(select_backward)
summary(select_stepwise)

# we need to create the models now

# R suggests using:
forward_fit=lm(formula = price ~ 
   zip + type + beds + baths + sqft + lot + 
   zip:type + zip:beds + zip:baths + zip:sqft + zip:lot +
   type:sqft + type:lot +
   beds:baths + 
   baths:sqft + baths:lot + 
   sqft:lot,
   data = homedata)
# missing 4 predictors = type:beds, type:baths, beds:sqft, beds:lot

summary(forward_fit)

plot(fitted.values(forward_fit), rstudent(forward_fit), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(forward_fit))
abline(0,1)

# R suggests using:
backward_fit=lm(formula = price ~ 
   zip + type + beds + baths + sqft + lot + 
   zip:beds + zip:sqft + zip:lot +
   type:sqft + type:lot +
   beds:baths + 
   baths:sqft + baths:lot + 
   sqft:lot,
   data = homedata)
# missing 6 predictors = zip:type, zip:baths, type:beds, type:baths, beds:sqft, beds:lot

summary(backward_fit)

plot(fitted.values(backward_fit), rstudent(backward_fit), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(backward_fit))
abline(0,1)
# mse=
(summary(backward_fit)$sigma)^2
summary(backward_fit)$r.squared

# R suggests using:
stepwise_fit=lm(formula = price ~ 
   zip + type + beds + baths + sqft + lot +  
   zip:beds + zip:sqft: + zip:lot + 
   type:sqft + type:lot +
   beds:baths + 
   baths:sqft + baths:lot +
   sqft:lot,
   data = homedata)
# missing 6 predictors = zip:type, zip:baths, type:beds, type:baths, beds:sqft, beds:lot
# note the stepwise selection chose a model identical to the backwards selection
 
summary(stepwise_fit)

plot(fitted.values(stepwise_fit), rstudent(stepwise_fit), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(stepwise_fit))
abline(0,1)

# import the new transformed data, where price2 = price^(2/9)
transformedteam=read.csv("TeamHomeData2.csv",
   colClasses=c('factor','factor','numeric','numeric','numeric','numeric','numeric'))

# check the transformed data 
transformedteam
summary(transformedteam)

# create a linear non-interation model using the trasformed data
full_transform=lm(price2~., data=transformedteam)
plot(fitted.values(full_transform), rstudent(full_transform), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(full_transform))
abline(0,1)
# mse=
(summary(full_transform)$sigma)^2
summary(full_transform)$r.squared

# create the full 2-parameter interaction model
full_transform.2=lm(price2~.*., data=transformedteam)
plot(fitted.values(full_transform.2), rstudent(full_transform.2), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(full_transform.2))
abline(0,1)
# mse=
(summary(full_transform.2)$sigma)^2
summary(full_transform.2)$r.squared

full_transform.2.r2=summary(full_transform.2)$r.squared
vif.full_transform.2=1/(1-full_transform.2.r2)
full_transform.2.r2
vif.full_transform.2

X <- as.matrix(cbind(transformedteam$beds, transformedteam$baths, transformedteam$sqft, transformedteam$lot))
SS1 <- sqrt(sum((X[,1] - mean(X[,1]))^2))
SS2 <- sqrt(sum((X[,2] - mean(X[,2]))^2))
SS3 <- sqrt(sum((X[,3] - mean(X[,3]))^2))
SS1 <- sqrt(sum((X[,4] - mean(X[,4]))^2))
W <- scale(X, scale = c(SS1, SS2, SS3, SS4)) # subtracts column means and divides by the square root of the sums of squares
t(W)%*%W
solve(t(W)%*%W)
#Alternatively, we can directly compute the correlation matrix of X
cor(X)
#VIF are found on the diagonal of the inverse of cor(X)
solve(cor(X))

# do the selection process
null_transform=lm(price2~1, data=transformedteam)
t_select_forward=step(null_transform, scope=list(lower=null_transform, upper=full_transform.2), direction="forward")
t_select_backward=step(full_transform.2, direction="backward")
t_select_stepwise=step(null_transform, scope=list(lower=null_transform, upper=full_transform.2), direction = "both")

summary(t_select_forward)
summary(t_select_backward)
summary(t_select_stepwise)


# use the summary of the forward selection to create the forward fit
t.forward_fit=lm(formula = price2 ~ 
    zip + type +  baths + sqft + lot + 
    zip:type + zip:baths + zip:sqft + zip:lot + 
    type:baths + type:sqft + type:lot +
    baths:sqft + baths:lot +
    sqft:lot, 
    data = transformedteam)

# missing 6 predictors = beds, zip:beds, type:beds, beds:baths, beds:sqft, beds:lot

plot(fitted.values(t.forward_fit), rstudent(t.forward_fit), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(t.forward_fit))
abline(0,1)
# mse=
(summary(t.forward_fit)$sigma)^2
summary(t.forward_fit)$r.squared

# use the summary of the backward selection to create the backward fit
t.backward_fit=lm(formula = price2 ~ 
    zip + type + beds + baths + sqft + lot + 
    zip:type + zip:beds + zip:sqft + zip:lot + 
    type:beds + type:baths + type:sqft +
    beds:baths + 
    baths:lot + 
    sqft:lot, 
    data = transformedteam)
# missing 5 predictors = zip:baths, type:lot, beds:sqft, beds:lot, baths:sqft

plot(fitted.values(t.backward_fit), rstudent(t.backward_fit), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(t.backward_fit))
abline(0,1)

# use the summary of the stepwise selection to create the stepwise fit
t.stepwise_fit=lm(formula = price2 ~ 
    zip + type + baths + sqft + lot + 
    zip:type + zip:baths + zip:sqft + zip:lot + 
    type:baths + type:sqft + type:lot +
    baths:sqft + baths:lot +
    sqft:lot,
    data = transformedteam)
# missing 6 predictors = beds, zip:beds, type:beds, beds:baths, beds:sqft, beds:lot
# in the transformed case, stepwise selection chose a model identical to forward selection

plot(fitted.values(t.stepwise_fit), rstudent(t.stepwise_fit), xlab = "predicted values", ylab = "residuals", main = " Externally Studentized Residuals",pch=19)
abline(0,0)
qqnorm(rstudent(t.step_fit))
abline(0,1)
# mse=
(summary(t.stepwise_fit)$sigma)^2
summary(t.stepwise_fit)$r.squared


par(mfrow=c(2,3), mar=c(2.6,2.6,2.6,2.6))
opt <- options(scipen = 10)
plot(homedata$zip,homedata$price, ylim=c(100000,4000000),xlab = "Zip", ylab = "Price", main = "Price vs. Zip", pch=20, cex=0.80)
plot(homedata$type, homedata$price,ylim=c(100000,4000000), xlab = "Type", ylab = "Price", main = "Price vs. Type", pch=20, cex=0.80)
plot(homedata$beds, homedata$price,ylim=c(100000,4000000), xlab = "Bedrooms", ylab = "Price", main = "Price vs. Bedrooms", pch=20, cex=0.80)
plot(homedata$baths, homedata$price,ylim=c(100000,4000000), xlab = "Bathrooms", ylab = "Price", main = "Price vs. Bathrooms", pch=20, cex=0.80)
plot(homedata$sqft, homedata$price,ylim=c(100000,4000000), xlab = "Square Footage", ylab = "Price", main = "Price vs. Square Footage", pch=20, cex=0.80)
plot(homedata$lot, homedata$price,ylim=c(100000,4000000), xlab = "Lot Size", ylab = "Price", main = "Price vs. Lot Size", pch=20, cex=0.80)
