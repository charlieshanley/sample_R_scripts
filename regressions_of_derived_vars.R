# Charles Hanley. 8 December 2014.

# To explore the relationships between precipitation and reservoir water quality
# parameters via regressions of derived variables.


user <- Sys.info()["user"]
if(user=="Cal") path <- "C:/Users/Cal/Documents/CASCADE/2014/UVA_Reservoirs"
if(user=="charles") path <- "~/Dropbox/git/VAreservoirs"
setwd(path)


data <- read.csv("Data/Combined_data/LightTempSonde_daily_allWeather.csv")

#===============================================================================
# Create derived variables

# Days since precip and magnitude of last precip--------------------------------

for(i in 1:length(data$Precip_inches)){
    is_zero <- data$Precip_inches[i]==0
    if(is_zero==F){
        data$days_since[i] <- 0  # when precip is nonzero, days_since is 0
        elapsed <- 0  # reset "elapsed" to zero when we get some precip
        last <- data$Precip_inches[i]  # reset "last" when we get precip
    }  else{
        elapsed <- elapsed + 1  # when precip is zero, add one to "elapsed"
        data$days_since[i] <- elapsed
    }
    data$mag_last[i] <- last  # mag_last will be last nonzero precip
}


# Accumulation periods----------------------------------------------------------

accum <- function(precip, days){
    # Calculates accumulation of precip over the preceding x days.
    # Precip should be vector, days should be integer. Returns vector.
    output <- vector()
    ref <- cumsum(precip)
    for(i in 1:length(precip)){
        if (i < days){
            output[i] <- NA 
        } else if (i == days){
            output[i] <- ref[i]
        }  else
            output[i] <- ref[i] - ref[(i - days)]
    }
    return(output)
}

# Set accumulation columns: accum2, accum3, ... accum7
for (i in 2:7){
    data[,paste0("accum",i)] <- accum(data$Precip_inches, i)
}



#-------------------------------------------------------------------------------
# Copper sulfate treatment: T/F

# What DoY was CuSO4 application?
treatment <- as.Date("2014-10-28") - as.Date("2013-12-31")  # = 301

data$CuSO4 <- ifelse(data$DoY > treatment, TRUE, FALSE)


#===============================================================================
# Run and compare regression models

#-------------------------------------------------------------------------------
# First, a correlation matrix

cor(data[ ,c("LEC","Temp_sonde","pH","BGA_cellsML","ODO","ODOsat","Chl_ugL",
            "Precip_inches","days_since","mag_last","accum3")],
    use="pairwise.complete.obs")

#-------------------------------------------------------------------------------
# plots to suggest the nature (linear or nonlinear) of correlated pairs?

plot(data[,c(12, 15, 20:29)])

#-------------------------------------------------------------------------------
# Construct models

attach(data)

# dependent var = BGA_cellsML
mod1 <- lm(BGA_cellsML ~ Precip_inches)
mod2 <- lm(BGA_cellsML ~ days_since)
mod3 <- lm(BGA_cellsML ~ mag_last)
mod4 <- lm(BGA_cellsML ~ accum2)
mod5 <- lm(BGA_cellsML ~ accum3)
mod6 <- lm(BGA_cellsML ~ accum4)
mod7 <- lm(BGA_cellsML ~ accum5)
mod8 <- lm(BGA_cellsML ~ accum6)
mod9 <- lm(BGA_cellsML ~ accum7)
mod25 <- lm(BGA_cellsML ~ CuSO4)

mod10 <- lm(BGA_cellsML ~ days_since + mag_last)
mod11 <- lm(BGA_cellsML ~ days_since/mag_last)
mod12 <- lm(BGA_cellsML ~ days_since + mag_last + days_since*mag_last)
mod13 <- lm(BGA_cellsML ~ Temp_sonde)
mod14 <- lm(BGA_cellsML ~ days_since + mag_last + Temp_sonde)
mod15 <- lm(BGA_cellsML ~ days_since + mag_last + days_since*mag_last + Temp_sonde)
mod26 <- lm(BGA_cellsML ~ CuSO4 + days_since + mag_last)
mod27 <- lm(BGA_cellsML ~ CuSO4 + Temp_sonde) # When CuSO4 treatment is acounted
# for, temperature is no longer a signficant predictor of BGA.
mod28 <- lm(BGA_cellsML ~ CuSO4 + Temp_sonde + days_since + mag_last +
                days_since*mag_last)


# dependent var = ODO
mod16 <- lm(ODO ~ data$Precip_inches)
mod17 <- lm(ODO ~ days_since)
mod18 <- lm(ODO ~ mag_last)
mod19 <- lm(ODO ~ days_since + mag_last)
mod20 <- lm(ODO ~ days_since*mag_last)
mod21 <- lm(ODO ~ days_since + mag_last + days_since*mag_last)
mod22 <- lm(ODO ~ Temp_sonde)  # Beware artifact of copper sulfate addition late in year
mod23 <- lm(ODO ~ days_since + mag_last + Temp_sonde)
mod24 <- lm(ODO ~ days_since + mag_last + days_since*mag_last + Temp_sonde)
mod29 <- lm(ODO ~ CuSO4)
mod30 <- lm(ODO ~ CuSO4 + Temp_sonde)
mod31 <- lm(ODO ~ CuSO4 + Temp_sonde + Precip_inches)
mod32 <- lm(ODO ~ CuSO4 + Temp_sonde + Precip_inches + days_since + mag_last)
mod33 <- lm(ODO ~ CuSO4 + Temp_sonde + Precip_inches + days_since + mag_last +
                days_since*mag_last)

detach(data)

#===============================================================================
# Consider implementing random forest or other decision tree methods.
