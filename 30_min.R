setwd("~/Desktop/Dropbox/School/Rowing Data")

library(ggplot2)
library(scales)
library(reshape2)
library(robustbase)

erg <- read.csv("sep9_30min.csv", header=T, stringsAsFactors=F)
names(erg) <- c("first","last","hr1","hr2","hr3","split1","split2","split3","Average_Split")


# Must convert from factors to something numeric

erg[c(8,20,25),3:5] <- NA
columns_hr <- c("hr1","hr2","hr3")
for(i in columns_hr){
        erg[,i] <- as.numeric(erg[,i])
}

columns_split <- c("split1","split2","split3","Average_Split")
for(i in columns_split){
        erg[,i] <- as.POSIXct(x=erg[,i], format="%M:%S")
}

# Calculate average heartrate

erg$Average_Heartrate <- (erg$hr1 + erg$hr2 + erg$hr3)/3

################################################################################

# Now for some histograms

AS.hist <- ggplot(erg, aes(x=Average_Split)) + geom_histogram(binwidth=2) +
        scale_x_datetime(labels = date_format("%M:%S"))

AH.hist <- ggplot(erg, aes(x=Average_Heartrate)) + geom_histogram(binwidth=5)

################################################################################
# Does heartrate increase with faster (lower) splits?

AH_AS.model <- lmrob(as.numeric(erg$Average_Split) ~ erg$Average_Heartrate)

AH_AS <- ggplot(erg, aes(x=Average_Heartrate, y=Average_Split)) + geom_point() +
        geom_smooth(method="rlm",se=F) + scale_y_datetime(labels = date_format("%M:%S")) +
        geom_text(aes(195, erg[22,6], label="p-value = 0.093\nR-squared = 0.16"))


################################################################################
# Now let's plot heartrates

hrpr <- erg[1:5]
names(hrpr) <- c("first","last",1,2,3)
hrpr <- melt(hrpr)
hrpr$variable <- as.numeric(hrpr$variable)

firstnames <- hrpr$first

hr <- ggplot(na.rm=T)
for(i in firstnames){
        hr <- hr + geom_line(data=hrpr[hrpr$first==i,], 
                             aes(x=variable,y=value, size=5, alpha=0.005))
}


# Now kernel density plots for heartrates

kdens <- erg[,3:5]
kdens <- melt(kdens)

kdens.plot <- ggplot(kdens, aes(x=value, color=variable)) + geom_density()
freq.poly <- ggplot(kdens, aes(x=value, color=variable)) + 
        geom_freqpoly(binwidth=10) + scale_y_continuous(breaks=c(0,2,4,6,8,10,12))

# Kernel density plots for splits

split.kdens <- erg[,6:8]
#for(i in 1:3) split.kdens[,i] <- as.numeric(split.kdens[,i])
split.kdens <- melt(split.kdens)

split.kdens.plot <- ggplot(split.kdens, aes(x=value, color=variable)) +
        geom_density() + scale_x_datetime(labels = date_format("%M:%S"))

split.freqpoly.plot <- ggplot(split.kdens, aes(x=value, color=variable)) +
        geom_freqpoly(binwidth=2) + scale_x_datetime(labels = date_format("%M:%S"))

################################################################################
# Calculate hr.rise

erg$hr.rise <- erg$hr3 - erg$hr1
erg$split.dif <- erg$split3 - erg$split1
erg$split.dif <- as.numeric(erg$split.dif)

hr.rise.hist <- ggplot(erg, aes(x=hr.rise)) + geom_histogram(binwidth=5)

# Is heartrare rise a proxy for fitness, and so a predictor of Average_Split?
hr.rise_Average_Split <- ggplot(erg, aes(x=hr.rise, y=Average_Split)) + geom_point() # No!


# But difference of split(t=30) - split(t=10) is a predictor of heartrate rise.
hrr_sd <-lmrob(erg$split.dif ~ erg$hr.rise)

hr.rise_split.dif <- ggplot(erg, aes(x=hr.rise, y=split.dif)) + geom_point() +
        geom_smooth(method=rlm)

# Is split.dif a predictor of Average_Split?
split.dif_Average_Split <- ggplot(erg, aes(x=split.dif, y=Average_Split)) + geom_point() +
        geom_smooth(method=lm)

# Now, will heartrate rise be a predictor of fitness if split difference
#       is accounted for?

# That turned out poorly. Maybe I mean this: predict hr.rise from
#       Average_Split and split.dif

mregr <- lm(erg$hr.rise ~ as.numeric(erg$Average_Split) + erg$split.dif)

################################################################################

cortest <- erg[3:12]
for(i in c("split1","split2","split3","Average_Split")){
        cortest[,i] <- as.numeric(cortest[,i])
}

cor(cortest, use="complete.obs")

################################################################################
################################################################################
# Now let's save the plots.
setwd("~/Desktop/Dropbox/School/Rowing Data/plots")
ggsave(filename="AS_hist.jpeg", plot=AS.hist)
ggsave(filename="AH_hist.jpeg", plot=AH.hist)
ggsave(filename="AH_AS.jpeg", plot=AH_AS)
ggsave(filename="hr_kdens.jpeg", plot=kdens.plot)
ggsave(filename="hr_freqpoly.jpeg", plot=freq.poly)
ggsave(filename="split_kdens.jpeg",plot=split.kdens.plot)
ggsave(filename="split_freqpoly.jpeg",plot=split.freqpoly.plot)
