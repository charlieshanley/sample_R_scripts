---
title: "Brief Introduction to ggplot2"
author: "Charles Hanley"
date: "01/27/2015"
output: html_document
---
***
I have recreated a couple of graphs that you used in lectures in R using ggplot2.
I'll show you how I did it, just to give a taste of what is possible.  
  
First, here are the plots.


```{r, echo=F, message=F, warning=F}
setwd("~/Dropbox/School/Year 4 Spring/Macroecon/")
require(ggplot2)
require(reshape2)
require(directlabels)
require(scales)

gdp <- read.csv("gdp_per_capita.csv")

desired_years <- gdp$Year >= 1950
desired_nations <- c("Poland","Ukraine","Mexico","India","Nicaragua","Somalia")
subset.gdp <- gdp[desired_years,c("Year",desired_nations)]
long.subset.gdp <- melt(subset.gdp, id.var="Year")

figure1 <- ggplot(data=long.subset.gdp, aes(x=Year, y=value, color=variable)) +
    geom_line() +
    scale_x_continuous(breaks=seq(1950,2010,10), limits=c(1950,2015)) +
    scale_y_continuous("Constant Dollars", labels=comma) +
    ggtitle("Real GDP per capita") +
    theme_bw() +
    theme(text=element_text(color="grey50")) +
    theme(axis.title=element_text(size=15)) +
    theme(plot.title=element_text(size=20, color="steelblue"))
direct.label(figure1, method="last.points")
    
```
  
The Maddison-Project, http://www.ggdc.net/maddison/maddison-project/home.htm, 2013 version.

***
Now I'll work through what you need to do to produce that.

```{r}

```

One of the primary advantages of producing graphics this way is reproducibility.
If you get new data and want to plot it in a way you've already done before,
It's as simple as plugging new data into an old script. Also, you'll have an
exact record of what you did to produce the output.  
  
You can also do some exotic things pretty easily. Here are a couple of things
I've done in the past.
```{r}
require(ggplot2)
require(scales)

abund <- read.csv("~/Dropbox/School/PaceLab/Chaoborus Phenology/master_abund.csv")

abund.fract <- ggplot(data = abund, aes(x = DOY)) +
    facet_grid(Lake ~ Year, scales = "free_y") + theme_bw() +
    geom_line(aes(y = fractionFI))

abund.fract


salaries <- read.csv("~/Dropbox/School/Year 4 Fall/D2K/project/sal_by_dept.csv")

violin <- ggplot(data=salaries, aes(y=Salary, x=GDept, fill=GDept, alpha=0.5)) +
  geom_violin(scale="area") +
  scale_y_continuous("Salary (USD)", labels=comma, breaks=(0:7)*50000) +
  scale_x_discrete("Department") +
  theme(legend.position="none") +
  ggtitle("Denisty Estimates of 2014 UVa Faculty Salaries by Department")

violin
```

***  
Here are a few resources for learning more about plotting with ggplot2 in R.

* 
* 
* 

***
This document was generated using [R Markdown](http://rmarkdown.rstudio.com/).
