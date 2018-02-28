# Data Understanding

# load the data
train <- read.csv('/home/nora/Documents/Titanic/train.csv', stringsAsFactors = F)
test  <- read.csv('/home/nora/Documents/Titanic/test.csv', stringsAsFactors = F)

# display data type
sapply(train, class)

# Data Quality Report

# Totals
data.quality0 <- as.data.frame(sapply(test, function(x) (sum(!is.na(x)) + sum(is.na(x)))))
colnames(data.quality0)<-"Total Values"
data.quality0 <-t(data.quality0)

# Missing Values
data.quality <- as.data.frame(sapply(test, function(x) sum(is.na(x))))
colnames(data.quality)<-"Missing"
data.quality <-t(data.quality)

# Not Missing
data.quality1 <- as.data.frame(sapply(test, function(x) sum(!is.na(x))))
colnames(data.quality1)<-"Not missing"
data.quality1 <-t(data.quality1)

# Percent Missing
data.quality2 <- as.data.frame(sapply(test, function(x)
  round(
    100*sum(is.na(x))/(sum(!is.na(x)) + sum(is.na(x))), digits=2)
))
colnames(data.quality2)<-"Percent Missing"
data.quality2 <-t(data.quality2)

# Bind data frames together
data<-do.call(rbind, list(data.quality, data.quality1, data.quality2, data.quality0))

# library for export
library(gridExtra)
#define image name
jpeg("~/missing_test.jpeg", width = 800, height=600)
#make grid table
a<-grid.table(data)
# print to file
dev.off()



