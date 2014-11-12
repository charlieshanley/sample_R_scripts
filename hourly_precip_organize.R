# Charlie Hanley, 2014 Oct 24
# To clean and organize hourly CHO precip data, and to plot with other variables.
#
# Note: This script is suited for the file modified and emailed by Jerry Stenger.
# If, in the future, we grab the data from a publicly available source, the script
# will need to be modified slightly.

library(reshape2)
library(ggplot2)


user <- Sys.info()["user"]

if(user=="Cal") path <- "C:/Users/Cal/Documents/CASCADE/2014/UVA_Reservoirs"
if(user=="charlie") path <- "~/Desktop/Dropbox/git/VAreservoirs"
        
setwd(path)

################################################################################


raw <- read.csv("Data/Weather/CHO_2014_hourly_precip_as_of_Oct20.csv", stringsAsFactors=F)
raw <- raw[-(1:13),-1]
colnames(raw) <- c("date",1:24)

raw <- raw[(grep("2014", raw[,1])) ,]   # Remove blank rows
raw[raw==" " | raw=="  T"] <- NA        # Change "space" and "trace" to NA

row.names(raw) <- NULL



long <- raw
long <- melt(long, id.vars="date")
colnames(long) <- c("date", "hour", "inches")

long$DateTime <- paste0(long$date, long$hour)
long$DateTime <- as.POSIXct(long$DateTime, format="%Y%m%d%H", tz="EST")
long <- long[,c("DateTime","inches")]

wet.long <- long[!is.na(long$inches),]
wet.long$inches <- as.numeric(wet.long$inches)
row.names(wet.long) <- NULL

#write.csv(wet.long, "Data/Weather/clean_CHO_hourly_precip.csv", row.names=F)

################################################################################

# Now to plot precipitation data with other variables.

precip <- read.csv("Data/Weather/clean_CHO_hourly_precip.csv", stringsAsFactors=F)
precip$DateTime <- as.POSIXct(precip$DateTime)
proj.precip <- precip[precip$DateTime > as.POSIXct("2014-09-24 00:00:01"),]

# Testing out
test <- ggplot(proj.precip) +
        geom_bar(aes(y=inches, x=DateTime), stat="identity", 
                alpha=0.5, fill="blue")

#===============================================================================

# Read in sonde data
sonde <- read.csv("Data/Combined_data/sonde_cleaned.csv", stringsAsFactors=F)
sonde$DateTime <- as.POSIXct(sonde$DateTime)

# Remove bad data
potential_out = sonde$SpCond < 0.01
sonde[potential_out, c("Temp", "SpCond", "pH", "BGA_cellsML", "BGA_RFU",
                       "ODOsat", "ODO", "Chl_ugL","Chl_RFU")] = NA

# Format as percent of maximum for plotting with different dataset
sonde$bga_plot <- sonde$BGA_cellsML / max(sonde$BGA_cellsML, na.rm=T)
proj.precip$for_plot <- proj.precip$inches / max(proj.precip$inches)

bga <- ggplot() +
        geom_line(data=sonde, aes(x=DateTime, y=bga_plot), alpha=0.7) +
        geom_bar(data=proj.precip, aes(y=for_plot, x=DateTime), alpha=0.5, 
                 stat="identity", fill="blue") +
#        scale_x_continuous("Date and Time") +
        scale_y_continuous("% of maximum") +
        ggtitle("Blue-green Algae and Hourly Precipitation Time Series")

#ggsave(bga, file= "Figures/bga_and_precip.png")
