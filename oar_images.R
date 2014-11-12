# Charles Hanley
# October 2014
# To download oar blade images for each US university team by scraping URLs.
# Thanks to Clay Ford for web scraping demo

library(RCurl)
library(png)

################################################################################
# Scrape image names from source

rawsource <- readLines("http://www.oarspotter.com/blades/USA/Uni/USA_Uni.html")

start <- grep("cellspacing=\"0\"", rawsource) # Find start of table
end <- grep("yale-m-crewlt-body", rawsource) + 1   # Find end of table

sourcetable <- rawsource[start:end]
rm(start); rm(end); rm(rawsource)

rawunis <- grep("class=\"list\"><IMG SRC=", sourcetable, value=T) # rm extraneous

unis1 <- gsub(pattern='<tr><td class="list"><IMG SRC="', "", rawunis) # rm 1st bit

unis2 <- vector(mode="character", length=length(unis1)) # take only png name
for(i in 1:length(unis1)){
        temp <- strsplit(unis1[i], split=".png", fixed=TRUE)
        temp <- unlist(temp)
        unis2[i] <- temp[1]
}
unis2 <- unis2[!unis2=="../../QuestionMark"]

oar_names <- paste(unis2, "png", sep=".")
# setwd("~/Desktop/Dropbox/School/Rowing Data/network project")
# write.csv(x=oar_names, file="oar_names.csv")

################################################################################
# Load images from URL

prefix <- "http://www.oarspotter.com/blades/USA/Uni/"
oar_urls <- paste0(prefix, oar_names)


pngList <- list()
for(ii in 1:length(oar_urls)){
        tempPNG <- readPNG(getURLContent(oar_urls[ii])) # Downloads & loads PNGs
        tempName <- unis2[ii]
        pngList[[tempName]] <- tempPNG # And assigns them to a list.
}


################################################################################
# Crop and save images

# Crop images to blade only
for(i in 1:length(pngList)){
        pngList[[i]] <- pngList[[i]][,352:436,]
}

# Remove "../../" and "/" from oar_names
oar_names <- gsub("../../", replacement="", x=oar_names)
oar_names <- gsub("/", replacement="_", x=oar_names)

# Save cropped images
setwd("~/Desktop/Dropbox/School/Rowing Data/network project/oar_images")
for(i in 1:length(pngList)){
        writePNG(image=pngList[[i]], target=oar_names[i])
}


# Some names are incorrect. Check for ordering problem between naming and
# downloading. Mining algorithm may have trimmed out images with a mouseover
# feature.

