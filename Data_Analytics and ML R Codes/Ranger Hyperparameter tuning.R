library(ranger)
library(caret)
#Author: Deha Ay
library(data.table)
library(ggplot2)
library(caret)
library(cowplot)


data <- fread("/Users/dehaay/Desktop/Finalized_data/END_finalized_data/end_data_for_ML.csv")
data <-subset (data, select = -c(temp)) ##remove temp, dont remove data now for better checking later
data <- data[sample(1:nrow(data),1000),]
#74100
#65100
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

# train_2$hour_interval <- as.factor(train_2$hour_interval)
# train_2$day <- as.factor(train_2$day)
# train_2$weekday <- as.factor(train_2$weekday)
# train_2$month <- as.factor(train_2$month)
# train_2$variable <- as.factor(train_2$variable)
# train_2 <-subset (train_2, select = -c(weather_desc,day))
# str(train_2)

train_2 <-subset (train_2, select = -c(weather_desc,day))
test_2 <- subset (test_2, select = -c(weather_desc,day))




##### HYPER PARAMATER TUNING for mtry ##### 
sample_size = 2000
tree_num <- sample(c(50:75),sample_size,replace = TRUE)
depth_max <- sample(c(35:75),sample_size,replace = TRUE)
mtrymax <- sample(c(4:9),sample_size,replace = TRUE)
#
vector <- c()
for (ee in 1:sample_size) {
  fit <- ranger(value ~ ., 
                data = train_2, 
                num.trees = tree_num[ee],
                max.depth = depth_max[ee],mtry = mtrymax[ee] ,importance='impurity')
  
  
  p2 <- predict(fit, test_2[,1:9])
  
  x <- data.frame(actual = test_2[,10], predict = round(p2$predictions))
  
  
  #plot(x$value, x$predict, xlim=c(min(round(p2$predictions)),max(round(p2$predictions))) , ylim = c(min(round(p2$predictions)),max(round(p2$predictions)))    )
  
  #abline(0,1)
  print(ee)
  
  
  
  MSE <- sum((x$value - x$predict)^2) / nrow(x)
  vector <-append(vector, MSE)
  
}

plot(c(1:sample_size),vector,type="l",xlab = "Max #of Depths", ylab ="MSE", main = "Hyper Parameterization -Max #of Depth")
#Most optimal node
maxdpth <- dpt[which(vector == min(vector))]
min(vector)
index <- which(vector == min(vector))

paste0(tree_num[index]," ",depth_max[index]," ",mtrymax[index])

















##### HYPER PARAMATER TUNING for mtry ##### 
dpt<- seq(1,9,1)
#
vector <- c()
for (ee in dpt) {
  fit <- ranger(value ~ ., 
                data = train_2, 
                num.trees = 73,
                max.depth = 35,mtry = ee ,importance='impurity')
  
  
  p2 <- predict(fit, test_2[,1:9])
  
  x <- data.frame(actual = test_2[,10], predict = round(p2$predictions))
  
  
  #plot(x$value, x$predict, xlim=c(min(round(p2$predictions)),max(round(p2$predictions))) , ylim = c(min(round(p2$predictions)),max(round(p2$predictions)))    )
  
  #abline(0,1)
  print(ee)
  
  
  
  MSE <- sum((x$value - x$predict)^2) / nrow(x)
  vector <-append(vector, MSE)
  
}

plot(dpt,vector,type="l",xlab = "Max #of Depths", ylab ="MSE", main = "Hyper Parameterization -Max #of Depth")
#Most optimal node
maxdpth <- dpt[which(vector == min(vector))]
min(vector)
maxdpth












# ##### HYPER PARAMATER TUNING for max depth ##### 
# dpt<- seq(10,100,1)
# #
# vector <- c()
# for (ee in dpt) {
#   fit <- ranger(value ~ ., 
#                 data = train_2, 
#                 num.trees = 73,
#                 max.depth = ee, mtry = 9,
#                 importance='impurity')
#   
#   p2 <- predict(fit, test_2[,1:9])
#   
#   x <- data.frame(actual = test_2[,10], predict = round(p2$predictions))
#   
#   
#   #plot(x$value, x$predict, xlim=c(min(round(p2$predictions)),max(round(p2$predictions))) , ylim = c(min(round(p2$predictions)),max(round(p2$predictions)))    )
#   
#   #abline(0,1)
#   print(ee)
#   
#   
#   
#   MSE <- sum((x$value - x$predict)^2) / nrow(x)
#   vector <-append(vector, MSE)
#   
# }
# 
# plot(dpt,vector,type="l",xlab = "Max #of Depths", ylab ="MSE", main = "Hyper Parameterization -Max #of Depth")
# #Most optimal node
# maxdpth <- dpt[which(vector == min(vector))]
# min(vector)
# maxdpth
# 
# beepr::beep(1)
# 
# 
# 
# 
# 
# 
# 
# ##### ##HYPER PARAMATER TUNING for num trees ##### 
# dpt<- seq(20,100,5)
# #
# vector2 <- c()
# for (ee in dpt) {
#   fit <- ranger(value ~ ., 
#                 data = train_2, 
#                 num.trees = ee,
#                 max.depth = 55,
#                 importance='impurity')
#   p2 <- predict(fit, test_2[,1:9])
#   
#   x <- data.frame(actual = test_2[,10], predict = round(p2$predictions))
#   
#   
#   # plot(x$value, x$predict, xlim=c(min(round(p2$predictions)),max(round(p2$predictions))) 
#   #      , ylim = c(min(round(p2$predictions)),max(round(p2$predictions)))    )
#   # abline(0,1)
#   print(ee)
#   
#   
#   MSE <- sum((x$value - x$predict)^2) / nrow(x)
#   rsqred <- cor(x$value, x$predict)^2
#   vector2 <-append(vector2, rsqred)
#   
# }
# 
# plot(dpt,vector,type="l")
# 
# #Most optimal node
# dpt[which(vector2 == max(vector2))]
# 
# max(vector2)
# 
# plot(dpt,vector2,type="l",xlab = "#of TREE", ylab ="R^2", main = "Hyper Parameterization -Max #of Depth")
# 
# beepr::beep(1)
# 
# 
# 
# 
# # 
# # ##### ##HYPER PARAMATER TUNING for num trees ##### 
# # dpt<- seq(500,2000,100)
# # #
# # vector2 <- c()
# # for (ee in dpt) {
# #   fit <- ranger(value ~ ., 
# #                 data = train_2, 
# #                 num.trees = ee,
# #                 max.depth = 20,
# #                 importance='impurity')
# #   p2 <- predict(fit, test_2[,1:11])
# #   
# #   x <- data.frame(actual = test_2[,12], predict = round(p2$predictions))
# #   
# #   
# #   plot(x$value, x$predict, xlim=c(min(round(p2$predictions)),max(round(p2$predictions))) 
# #        , ylim = c(min(round(p2$predictions)),max(round(p2$predictions)))    )
# #   abline(0,1)
# #   
# #   
# #   
# #   MSE <- sum((x$value - x$predict)^2) / nrow(x)
# #   vector2 <-append(vector2, MSE)
# #   
# # }
# # 
# # plot(dpt,vector2,type="l")
# # #Most optimal node
# # dpt[which(vector2 == min(vector2))]
# # min(vector2)
# # 
# # beepr::beep(1)
# 
# 
# 
# 
# 
