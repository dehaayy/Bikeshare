#Author: Deha Ay
library(neuralnet)
library(MASS)
library(dplyr)
library(data.table)
library(ggplot2)
data <- fread("/Users/dehaay/Desktop/FINAL DATA/FinalMLData.csv")
data <- data[,-1]
View(data)
colnam <- colnames(data)

data<-select(data,-c(weather_desc))

data<-select(data,-c(`West St & Liberty St`, `1 Ave & E 68 St`, `7 Ave & Central Park South` ,
                     `8 Ave & W 31 St` , `Broadway & W 25 St` , `E 33 St & 1 Ave` ,
                     `Pier 40 - Hudson River Park`, `W 30 St & 10 Ave`, `West St & Liberty St` , `Grand St & Elizabeth St`))


colnam <- colnames(data)
melt_data <- melt(data, id = c(colnam[1:8]))
colnames(melt_data) <- c("a","b","c","d","e","f","g","h","i","pre")
View(melt_data)
catgries <- unclass(factor(melt_data$i))
melt_data$i <- catgries


# maxs <- sapply(melt_data, max) 
# mins <- sapply(melt_data,min)
# 
# scaled <- as.data.frame(scale(melt_data, center = mins, 
#                               scale = maxs - mins))

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

scaled <- as.data.frame(lapply(melt_data, min_max_norm))

index <- sample(1:nrow(data), round(0.80 * nrow(data)))
train_ <- scaled[index,]
test_ <- scaled[-index,]


nn <- neuralnet(pre ~ .  , 
                data = train_,
                linear.output = TRUE, lifesign = 'full', rep = 1 , threshold=0.01 )
pr.nn <- neuralnet::compute(nn, test_[,1:9])

# Compute mean squared error
pr.nn_ <- pr.nn$net.result

test.r <- (test_$pre)

MSE.nn <- sum((test.r - round(pr.nn_))^2) / nrow(test_)
MSE.nn

df <- data.frame(x = test.r,y=pr.nn_) * (max(melt_data$pre) - min(melt_data$pre)) + min(melt_data$pre)
ggplot(df,aes(x=x,y=y)) + geom_point(alpha = 0.3)
View(df)




