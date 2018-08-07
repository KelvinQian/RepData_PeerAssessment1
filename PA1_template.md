---
title: "assignment1"
output: md_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

## unzip the dataset

```{r unzip, echo=TRUE}
library("data.table")
library(ggplot2)
setwd("C:\\Users\\Qian Qian\\Desktop\\data analysis\\reproducible research")
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

## read csv data

```{r load, echo=TRUE}
activitydt<-data.table::fread(input="data/activity.csv")
```

## what is mean total number of steps taken per day?

```{r q1.1, echo=TRUE}
Total_steps<-activitydt[,c(lapply(.SD,sum,na.rm=FALSE)), .SDcols=c("steps"), by=.(date)]
head(Total_steps, 10)
```

```{r q1.2, echo=TRUE}
ggplot(Total_steps, aes(x=steps))+
  geom_histogram(fill="green", binwidth = 1000)+
  labs(title="Daily Steps", x="steps", y="frequency")
```

```{r, q1.3, echo=TRUE}
Total_steps[, .(Mean_steps=mean(steps, na.rm=TRUE), Median_steps=median(steps,na.rm=TRUE))]
```

## What is the average daily activity pattern?

```{r q2.1, echo=TRUE}
Intervaldt <- activitydt[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(Intervaldt, aes(x = interval , y = steps)) + geom_line(color="blue", size=1) + labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

```{r q2.2, echo=TRUE}
Intervaldt[steps == max(steps), .(max_interval = interval)]
```

## Imputing missing values

```{r q3.1, echo=TRUE}
activitydt[is.na(steps), .N ]

# alternative solution
nrow(activitydt[is.na(steps),])
```

```{r q3.2, echo=TRUE}
# Filling in missing values with median of dataset. 
activitydt[is.na(steps), "steps"] <- activitydt[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

```{r q3.3, echo=TRUE}
data.table::fwrite(x = activitydt, file = "data/tidyData.csv", quote = FALSE)
```

```{r q3.4, echo=TRUE}
# total number of steps taken per day
Total_steps2 <- activitydt[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

# mean and median total number of steps taken per day
Total_steps2[, .(Mean_steps = mean(steps), Median_steps = median(steps))]

ggplot(Total_steps2, aes(x = steps)) + geom_histogram(fill = "green", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

## Are there differences in activity patterns between weekdays and weekends?

```{r q4.1, echo=TRUE}
activitydt <- data.table::fread(input = "data/activity.csv")
activitydt[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activitydt[, `Day of Week`:= weekdays(x = date)]
activitydt[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activitydt[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activitydt[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activitydt, 10)
```

```{r q4.2, echo=TRUE}
activitydt[is.na(steps), "steps"] <- activitydt[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
Intervaldt <- activitydt[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(Intervaldt , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Daily steps by week", x = "Interval", y = "number of steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

