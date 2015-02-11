# Charles Hanley, Feb 7, 2015

setwd("~/Dropbox/School/Year 4 Spring/MachineLearning/hw3/")
library(leaps)
library(boot)
library(FNN)

#===============================================================================
# Part One

# To predict mpg of car models from given data on number of cylinders (cyl),
# engine displacement (disp), horsepower (hp), acceleration 0-60 (accel),
# weight (wt), and model year (year).

# Only linear models are to be used, with whatever transformations or interactions
# are deemed necessary. Performance will be judged by predictions on a test
# data set with mpg values withheld.

train1 <- read.csv("hw03p01train.csv")
train1$cyl <- as.factor(train1$cyl)

predict1 <- read.csv("hw03p01predict.csv")
predict1$cyl <- as.factor(predict1$cyl)

plot(train1)

#-------------------------------------------------------------------------------
# Variable Selection and Transformations

regsubsets.table <- function(regsubset){
    # Make a comparison table from regsubsets() object.
    rsq <- summary(regsubset)$rsq
    adjr2 <- summary(regsubset)$adjr2
    cp <- summary(regsubset)$cp
    bic <- summary(regsubset)$bic
    return(cbind(summary(regsubset)$which,rsq,adjr2,cp,bic))
}

# No transformations:
train1.rsub <- regsubsets(mpg ~ disp + hp + wt + accel + year + cyl,
                          data=train1, nbest=2)
regsubsets.table(train1.rsub)

# Using interactions:
train1.rsub2 <- regsubsets(mpg ~ cyl + (disp + hp + wt + accel + year)^2, 
                           data=train1, nbest=2, nvmax=100)
regsubsets.table(train1.rsub2)

# Squares:
train1.rsub3 <- regsubsets(mpg ~ cyl + disp + I(disp^2) + hp + I(hp^2) + wt +
                               I(wt^2)+ year + I(year^2) + accel + I(accel^2),
                           data=train1, nbest=2, nvmax=100)
regsubsets.table(train1.rsub3)

# Logs
train1.rsub4 <- regsubsets(mpg ~ cyl + disp + log(disp) + hp + log(hp) + wt +
                               log(wt)+ year + log(year) + accel + log(accel),
                           data=train1, nbest=2, nvmax=100)
regsubsets.table(train1.rsub4)

# to the -1:
train1.rsub5 <- regsubsets(mpg ~ disp + I(disp^-1) + hp + I(hp^-1) + wt +I(wt^-1)
                           + year + I(year^-1) + accel + I(accel^-1) + cyl,
                           data=train1, nbest=2, nvmax=100)
regsubsets.table(train1.rsub5)

# ^-1 and interactions:
train1.rsub6 <- regsubsets(mpg ~ (I(1/disp) + I(1/hp) + I(1/wt)
                           + year + accel)^2, data=train1, nbest=2, nvmax=100)
regsubsets.table(train1.rsub6)

# Logs and interaction:
train1.rsub7 <- regsubsets(mpg ~ cyl + (log(disp) + log(hp) + log(wt)
                                  + log(year))^2, data=train1, nbest=2, nvmax=100)
regsubsets.table(train1.rsub7)


#-------------------------------------------------------------------------------
# Cross validate candidate models

cv.glm.iter <- function(data, glmfit, K, n){
    # K-fold cross validate a glm n number of times, return the mean.
    require(boot)
    mse <- rep(0, n)
    for (i in 1:n){
        mse[i] <- cv.glm(data=data, glmfit=glmfit, K=K)$delta[1]
    }
    return(mean(mse))
}

# The transformation X^-1 is deemed appropriate for wt, hp and disp.
# One candidate will include all given variables and all possible interactions.
train1.mod.all <- glm(mpg ~ (I(1/wt) + I(1/hp) + I(1/disp) + accel + year)^2, 
                      data=train1)
cv.glm.iter(data=train1, glmfit=train1.mod.all, K=6, n=50)

# A subset of variables and interactions with a small BIC.
train1.mod.select1 <- glm(mpg ~ I(1/disp) + I(1/wt) + I(1/hp) + I(1/hp):year
                   + I(1/wt):year, data=train1)
cv.glm.iter(data=train1, glmfit=train1.mod.select1, K=6, n=50)

# Another subset of variables and interactions with a small BIC.
train1.mod.select2 <- glm(mpg ~ I(1/hp) + I(1/wt) + accel + I(1/disp):I(1/hp) +
                              I(1/disp):accel + I(1/hp):year + I(1/wt):year,
                          data=train1)
cv.glm.iter(data=train1, glmfit=train1.mod.select2, K=6, n=50)


# Here's the model I've decided on.
train1.modfin <- glm(mpg ~ I(1/hp) + I(1/wt) + accel + I(1/disp):I(1/hp) +
                         I(1/disp):accel + I(1/hp):year + I(1/wt):year +
                         cyl, data=train1)

# Plot predicted against actual (of training data)
plot(train1$mpg ~ predict(train1.modfin))
abline(a=0, b=1)

# Plot residuals
plot(resid(train1.modfin))
abline(a=0, b=0)

# Make predictions on the test dataset.
predvect1 <- predict(train1.modfin, newdata=predict1)
write.table(predvect1, file = "hw03p01mypredictions.csv", row.names=F,
            col.names=F, sep=",")


#===============================================================================
# Part Two

# To predict average winnings per tournament of golfers from data on a number of
# attributes.

train2 <- read.csv("hw03p02train.csv")
predict2 <- read.csv("hw03p02predict.csv")

plot(train2)

#-------------------------------------------------------------------------------
# Rank seems to be the most reliable single predictor. Let's make a linear model
# using only Rank as a baseline.
train2.mod1 <- glm(AWin ~ Rank + log(Rank) + I(log(Rank)^2), data=train2)
cv.glm.iter(train2, train2.mod1, K=8, n=100)

#-------------------------------------------------------------------------------
# Now let's look at models using all the variables, including KNN models.

# Variable selection for linear model
train2.rsub1 <- regsubsets(AWin ~ Age + Dist + Acc + GIR + Putts + SP + Events +
                               log(Rank) + Rank + I(log(Rank)^2),
                 data=train2, nbest=1, nvmax=100)
regsubsets.table(train2.rsub1)

train2.rsub2 <- regsubsets(AWin ~ (Age + Dist + Acc + GIR + Putts + SP + Events +
                                       log(Rank))^2 + Rank + I(log(Rank)^2),
                           data=train2, nbest=1, nvmax=100)
regsubsets.table(train2.rsub2)

train2.rsub3 <- regsubsets(AWin ~ (log(Age) + log(Dist) + log(Acc) + log(GIR) + log(Putts) +
                               log(SP) + log(Events) + Rank)^2 + log(Rank) +
                                   I(log(Rank)^2), data=train2, nbest=1,
                           nvmax=100)
regsubsets.table(train2.rsub3)

train2.rsub4 <- regsubsets(AWin ~ (I(1/Age) + I(1/Dist) + I(1/Acc) + I(1/GIR) +
                               I(1/Putts) + I(1/SP) + I (1/Events) + log(Rank))^2,
                           data=train2, nbest=1, nvmax=100)                           
regsubsets.table(train2.rsub4)

# Cross-validate the candidate model
train2.mod2 <- glm(AWin ~ Age + Events + Rank + log(Rank) + I(log(Rank)^2) + 
                       Age:GIR + Age:log(Rank) + GIR:Putts + GIR:Events +
                       Putts:log(Rank) + Events:log(Rank), data=train2)
cv.glm.iter(train2, train2.mod2, K=160, n=1)
summary(train2.mod2)

# Plot predicted against actual
plot(train2$AWin ~ predict(train2.mod2))
abline(a=0, b=1)

# Plot residuals
plot(train2$AWin ~ resid(train2.mod2))
abline(a=0, b=0)


# Make predictions on test dataset
train2.modfin <- train2.mod2
predvect2 <- predict(train2.modfin, newdata=predict2)
write.table(predvect2, file = "hw03p02mypredictions.csv", row.names=F,
            col.names=F, sep=",")

#-------------------------------------------------------------------------------
# Try knn.reg()

scaled <- cbind(train2$AWin, scale(train2[,1:8]))
colnames(scaled) <- c("AWin", colnames(scaled[,2:9]))

train2.mod3 <- knn.reg(train=scaled, y=scaled[,1], k=3)
train2.mod3$PRESS / 160 # This model is inferior to the linear model.
