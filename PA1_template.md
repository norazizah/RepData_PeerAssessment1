---
title: "Reproducible Research"
author: "Azizah"
date: "September 21, 2015"
output: pdf_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r echo=TRUE}
library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')
library(data.table)
library(ggplot2)
```


Loading the data
```{r echo = TRUE}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activityData <- read.csv('activity.csv')
```

preprocessing the data

```{recho = TRUE }
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")
activityData$interval <- as.factor(activityData$interval)
```

```{r}
# Transform the date attribute to an actual date format
activityData$date <- as.POSIXct(activityData$date, format="%Y-%m-%d")

# Compute the weekdays from the date attribute
activityData <- data.frame(date=activityData$date, 
                           weekday=tolower(weekdays(activityData$date)), 
                           steps=activityData$steps, 
                           interval=activityData$interval)

# Compute the day type (weekend or weekday)
activityData<- cbind(activityData, 
                      daytype=ifelse(activityData$weekday == "saturday" | 
                                     activityData$weekday == "sunday", "weekend", 
                                     "weekday"))

# Create the final data.frame
activity <- data.frame(date=activityData$date, 
                       weekday=activityData$weekday, 
                       daytype=activityData$daytype, 
                       interval=activityData$interval,
                       steps=activityData$steps)

# Clear the workspace
rm(activityData)
```


What is mean total number of steps taken per day
```{r echo = TRUE}
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

Make a histogram of the total number of steps taken each day
```{r echo = TRUE}
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(activityData$steps, by=list(activityData$date), FUN=sum, na.rm=TRUE)

# Rename the attributes
names(sum_data) <- c("date", "total")
```

Make a histogram of the total number of steps taken each day
```{r  echo = TRUE}
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```

Calculate and report the mean and median total number of steps taken per day
```{r echo = TRUE}
mean(sum_data$total)
median(sum_data$total)
```

What is the average daily activity pattern

```{r echo = TRUE}
mean_data <- aggregate(activityData$steps, 
                       by=list(activityData$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

# Rename the attributes
names(mean_data) <- c("interval", "mean")
```

Make a time series plot
```{r echo = TRUE}

# Compute the means of steps accross all days for each interval
mean_data <- aggregate(activityData$steps, 
                       by=list(activityData$interval), 
                       FUN=mean, 
                       na.rm=TRUE)

# Rename the attributes
names(mean_data) <- c("interval", "mean")
```

compute the time series
```{r echo=TRUE}
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r echo = TRUE }
max_pos <- which(mean_data$mean == max(mean_data$mean))

# We lookup the value of interval at this position
max_interval <- mean_data[max_pos, 1]

# Clear the workspace
rm(max_pos, mean_data)
```


Calculate and report the total number of missing values in the dataset

```{r echo = TRUE}
rm(max_interval)

# We use the trick that a TRUE boolean value is equivalent to 1 and a FALSE to 0.
NA_count <- sum(is.na(activityData$steps))
```

Devise a strategy for filling in all of the missing values in the dataset.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r echo = TRUE}
# Clear the workspace
rm(NA_count)

# Find the NA positions
na_pos <- which(is.na(activityData$steps))

# Create a vector of means
mean_vec <- rep(mean(activityData$steps, na.rm=TRUE), times=length(na_pos))
```

Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r echo=TRUE}
# Replace the NAs by the means
activityData[na_pos, "steps"] <- mean_vec

# Clear the workspace
rm(mean_vec, na_pos)
```

Make a histogram of the total number of steps taken each day
```{r echo = TRUE}
# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(activityData$steps, by=list(activityData$date), FUN=sum)

# Rename the attributes
names(sum_data) <- c("date", "total")

# Compute the histogram of the total number of steps each day
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```


Calculate and report the mean and median total number of steps taken per day.
```{r echo = TRUE }
mean(sum_data$total)
median(sum_data$total)
```

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day
```{r echo = TRUE}
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

```{r}
head(activityData)
```


Make a panel plot containing a time series plot
```{r echo = TRUE}
# Clear the workspace
rm(sum_data)

# Load the lattice graphical library
library(lattice)

# Compute the average number of steps taken, averaged across all daytype variable
mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)

# Rename the attributes
names(mean_data) <- c("daytype", "weekday", "interval", "mean")

```

```{r echo=mean_data}

```

```{r echo=TRUE}
# Compute the time serie plot
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```


```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
