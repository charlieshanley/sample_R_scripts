# Charles Hanley. April 08, 2015.

library(randomForest)
library(e1071)
library(FNN)
library(ggplot2)
library(rgl)
setwd("~/Dropbox/School/Year 4 Spring/MachineLearning/exam02")

myColorRamp <- function(colors, values) {
    v <- (values - min(values))/diff(range(values))
    x <- colorRamp(colors)(v)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

#===============================================================================
# One

train1 <- read.csv("e2p1-train.csv")
pred1 <- read.csv("e2p1-predict.csv")

train1.pc <- prcomp(train1[,-14], scale=T)
screeplot(train1.pc)
train1.pc$sdev^2/sum(train1.pc$sdev^2)

plot_df <- data.frame(PC1=train1.pc$x[,1], PC2=train1.pc$x[,2],
                      PC3=train1.pc$x[,3], mv=train1$mv)
ggplot(data=plot_df, aes(x=PC1, y=PC2, color=mv)) + 
    geom_point() + theme_bw() + scale_color_gradientn(colours=c("red","green"))

cols <- myColorRamp(c("red","green"), train1$mv)
with(plot_df, plot3d(x=PC1, y=PC2, z=PC3, col=cols, size=6))

#-------------------------------------------------------------------------------
# Trees

# bag
train1.bag <- randomForest(mv~., data=train1, mtry=13)

#OOB MSE
mean((predict(train1.bag)-train1$mv)^2)

# RF
mse.rf <- vector()
for (i in 1:12){
    train1.rfmod <- randomForest(mv~., data=train1, mtry=i, ntree=1000)
    mse.rf[i] <- mean((predict(train1.rfmod)-train1$mv)^2)
}
plot(mse.rf)

train1.rf5 <- randomForest(mv~., data=train1, mtry=5, ntree=1000)

predvect1 <- predict(train1.rf5, newdata=pred1)
write.table(predvect1, file="e2p1mypredictions.csv", row.names=F, col.names=F, sep=",")

#-------------------------------------------------------------------------------
# linear model

#-------------------------------------------------------------------------------
# knn
train1.knncv <- knn.cv(train=train1[,1:8], cl=train1[,9], k=3)
#nah.

#===============================================================================
# Two

train2 <- read.csv("e2p2-train.csv")
pred2 <- read.csv("e2p2-predict.csv")

train2$d <- as.factor(train2$d)

train2.pc <- prcomp(train2[,-9], scale=T)
screeplot(train2.pc)
train2.pc$sdev^2/sum(train2.pc$sdev^2)

plot_df2 <- data.frame(PC1=train2.pc$x[,1], PC2=train2.pc$x[,2],
                       PC3=train2.pc$x[,3], d=train2$d)
plot_df2$d <- as.numeric(plot_df2$d)
with(plot_df2, plot(PC1, PC2, col=d))
with(plot_df2, plot3d(x=PC1, y=PC2, z=PC3, col=d+1, size=1, type="s"))


#-------------------------------------------------------------------------------
# trees

train2.rfmod <- randomForest(d~., data=train2, mtry=5, ntree=1000)
tail(train2.rfmod$err.rate)
train2.rfmod$confusion

err.rf2 <- vector()
for (i in 1:8){
    train2.rfmod <- randomForest(d~., data=train2, mtry=i, ntree=1000)
    err.rf2[i] <- sum(train2.rfmod$predicted != train2$d)
}
plot(err.rf2)

train2.rf6 <- randomForest(d~., data=train2, mtry=6, ntree=1000)


#-------------------------------------------------------------------------------
# svm
train2.tune <- tune(svm, d~., data=train2, kernel="polynomial", 
                    ranges=list(cost=c(80,90,100,110,120),gamma=c(.001,.005,.01,.02,.03)))
summary(train2.tune)

train2.svm <- svm(d~., data=train2, kernel="radial", cost=100, gamma=0.01)

#-------------------------------------------------------------------------------
predvect2 <- as.character(predict(train2.rf6, newdata=pred2))
write.table(predvect2, file="e2p2mypredictions.csv", row.names=F, col.names=F, sep=",")

