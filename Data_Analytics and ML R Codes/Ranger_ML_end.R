library(ranger)
library(caret)
#Author: Deha Ay
library(neuralnet)
library(dplyr)
library(data.table)
library(cowplot)


data <- fread("/Users/dehaay/Desktop/Finalized_data/END_finalized_data/end_data_for_ML.csv")
data <-subset (data, select = -c(temp)) ##remove temp, dont remove data now for better checking later
data <- data[1:nrow(data),]

colnam <- colnames(data)
data <-subset (data, select = -c(`Pershing Square South`))
melt_data <- melt(data, id = c(colnam[1:11]))
#View(melt_data)

####PREAPEARING THE DATA -ANY DATA MODIFICATION GOES IN TO HERE#####

####ALTERNATIVE ONE
## has each station data in chunks, one finishes for the whole date and the others starts
data_set_1 <- melt_data
data_set_1_peek <- data_set_1
catgries <- unclass(factor(data_set_1$variable))
data_set_1$variable <- catgries
data_set_1 <-subset (data_set_1, select = -c(date))

##MIGHT WANT TO CONSIDER: removing day OR (all weather data except the descript.)
#View(data_set_1)

cc <- factor(data_set_1$variable)
View(data.frame(levels(catgries)))

## all stations are ordered by the day
data_set_2 <- melt_data[order(date,variable),]
data_set_2_peek <- data_set_2
catgries <- unclass(factor(data_set_2$variable))
data_set_2$variable <- catgries
data_set_2 <-subset (data_set_2, select = -c(date))

#View(data_set_2)

####No need for normalizing in RF
########    SAMPLING    ########
index <- createDataPartition(data_set_1$hour_interval, p =0.80, list = FALSE)

train_1 <- data_set_1[index,]
test_1 <- data_set_1[-index,]

train_2 <- data_set_2[index,]
test_2 <- data_set_2[-index,]


################################
set.seed(123)


train_1 <-subset (train_1, select = -c(weather_desc,day))
test_1 <- subset (test_1, select = -c(weather_desc,day))
str(test_1)

train_2 <-subset (train_2, select = -c(weather_desc,day))
test_2 <- subset (test_2, select = -c(weather_desc,day))
str(train_2)

fit <- ranger(value ~ ., 
              data = train_1, 
              num.trees = 73,
              max.depth = 35,mtry = 7,importance='impurity',splitrule = "extratrees")

fit

p2 <- predict(fit, test_1[,1:9])

x <- data.frame(actual = test_1[,10], predict = round(p2$predictions))


plot(x$value, x$predict, xlim=c(min(round(p2$predictions)),max(round(p2$predictions))) 
     , ylim = c(min(round(p2$predictions)),max(round(p2$predictions)))
     ,xlab="Actual Values", ylab="Predicted Values"
     ,col = "blue",
     cex = 1,     
     lwd = 1   )
abline(0,1,col="black", lwd=4, lty=1)


MSE <- sum((x$value - x$predict)^2) / nrow(x)
MSE

rsqred <- cor(x$value, x$predict)^2
ranger::importance(fit)/10000

xxx <- ranger::importance(fit) / sum(ranger::importance(fit)) * 100
xxx

beepr::beep(1)


saveRDS(fit, "/Users/dehaay/Desktop/Finalized_MLweb/checksize.rds")







