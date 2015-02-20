# Charlie Hanley. February 17, 2015.

#===============================================================================
# Preliminaries

setwd("~/Dropbox/School/Year 4 Spring/MachineLearning/hw4")

library(rgl)
library(FNN)
library(MASS)
library(boot)

train1 <- read.csv("hw04p01train.csv")
pred1 <- read.csv("hw04p01predict.csv")

train2 <- read.csv("hw04p02train.csv")
pred2 <- read.csv("hw04p02predict.csv")

train3 <- read.csv("hw04p03train.csv")
pred3 <- read.csv("hw04p03predict.csv")


#===============================================================================
# Part One

# Plot data in 2d and 3d.
plot(train1) # total and freq correlate nearly 1 to 1.
with(train1, plot3d(x=time, y=last, z=total, col=recent+1))

# How does range of training data compare to range of testing data?
summary(train1[,1:4])
summary(pred1)

#-------------------------------------------------------------------------------
# Test knn model including both freq and time, optimizing k.

train1.scaled <- cbind(scale(train1[,1:4]), train1[,5])
colnames(train1.scaled)[5] <- "recent"

prop_corr1 <- rep(0, 30)
for (i in 1:30){
    train1.knn1 <- knn.cv(train=train1.scaled[,1:4], cl=train1.scaled[,5], k=i)
    proportion_correct <- sum(train1.knn1 == train1$recent) / length(train1.knn1)
    prop_corr1[i] <- proportion_correct
}
summary(prop_corr1)
plot(prop_corr1, type="l")
which.max(prop_corr1)

#-------------------------------------------------------------------------------
# Test knn model including freq, not total, optimizing for k.

prop_corr2 <- rep(0, 30)
for (i in 1:30){
    train1.knn2 <- knn.cv(train=train1.scaled[,c(1,2,4)], cl=train1.scaled[,5], k=i)
    proportion_correct <- sum(train1.knn2 == train1$recent) / length(train1.knn2)
    prop_corr2[i] <- proportion_correct
}
summary(prop_corr2)
plot(prop_corr2, type="l")
which.max(prop_corr2)

#-------------------------------------------------------------------------------
# Make predictions with winning model: knn excluding 'total' with k=16

pred1.scaled <- scale(pred1)

pred1.knn <- knn(train=train1.scaled[,c(1,2,4)], test=pred1.scaled[,c(1,2,4)],
                 cl=train1.scaled[,5], k=16)
predvect1 <- as.character(pred1.knn)
write.table(predvect1, file = "hw04p01mypredictions.csv", row.names=F, col.names=F, sep=",")

#===============================================================================
# Part Two

plot(train2)
with(train2, plot3d(x=homework, y=midterm, z=quiz, col=final+1))

#-------------------------------------------------------------------------------
# Test a knn model, optimizing for k.

train2.scaled <- cbind(scale(train2[,1:3]), train2[,4])
colnames(train2.scaled)[4] <- "final"

prop_corr3 <- rep(0, 60)
for (i in 1:60){
    train2.knn <- knn.cv(train=train2.scaled[,1:3], cl=train2.scaled[,4], k=i)
    prop_corr3[i] <- sum(train2.knn == train2$final) / length(train2.knn)
}
summary(prop_corr3)
plot(prop_corr3, type="l")
which.max(prop_corr3)

#-------------------------------------------------------------------------------
# Test a qda model.

train2.qda <- qda(final ~ homework + midterm + quiz, data=train2)
summary(train2.qda)
prop_corr4 <- sum(predict(train2.qda, train2)$class == train2$final) / length(train2$final)
prop_corr4      # Proportion correct is less than the knn model, even testing on
                # the training data.

#-------------------------------------------------------------------------------
# Make predictions with the winning model: knn with k=24

pred2.scaled <- scale(pred2)
pred2.knn <- knn(train=train2.scaled[,1:3], test=pred2.scaled,
                 cl=train2.scaled[,4], k=24)
predvect2 <- as.character(pred2.knn)
write.table(predvect2, file = "hw04p02mypredictions.csv", row.names=F, col.names=F, sep=",")

#===============================================================================
# Part Three

# Make categorical variables factors
for(i in c(1:7, 10:13)){
    train3[,i] <- as.factor(train3[,i])
}

cor(train3)

# Logistic regression with all variables
train3.glm1 <- glm(income ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x13, 
                     data=train3, family=binomial)
summary(train3.glm1)

train3.glm1.pred <- rep(0, nrow(train3))
train3.glm1.pred[train3.glm1$fitted.values > 0.5] <- 1
sum(train3.glm1.pred == train3$income) / length(train3$income)

# Logistic regression with selected variables

train3.glm2 <- glm(income ~ x2+x4+x5+x8+x9+x10+x11+x12+x13, 
                   data=train3, family="binomial")
summary(train3.glm2)

train3.glm2.pred <- rep(0, nrow(train3))
train3.glm2.pred[train3.glm2$fitted.values > 0.5] <- 1
sum(train3.glm2.pred == train3$income) / length(train3$income)

#-------------------------------------------------------------------------------
# Predict from logistic regression using all variables (except 12,
# because there's a new level in the test data).

# Make categorical variables factors
for(i in c(1:7, 10:13)){
    pred3[,i] <- as.factor(pred3[,i])
}

probabilities3 <- predict(train3.glm1, newdata=pred3, type="response")
predvect3 <- rep(0, length(probabilities3))
predvect3[probabilities3 > 0.5] <- 1
write.table(predvect3, file = "hw04p03mypredictions.csv", row.names=F, col.names=F, sep=",")
