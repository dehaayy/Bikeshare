#Author: Deha Ay
#Contributors: Deha Ay
library(neuralnet)
library(MASS)
library(dplyr)
library(data.table)
#sapply(data[,11:46],function(c)sum(c==0)/34807)


data <- fread("/Users/dehaay/Desktop/FINAL DATA/FinalMLData.csv")

predic_station <- data$`W 41 St & 8 Ave`

data <- data[,2:10]
data$`W 41 St & 8 Ave` <- predic_station

data<-select(data,-c(weather_desc))
data<-select(data,-c(year))
look <-data


                      #algorithm = "backprop" with all numeric values
                      #YEAR IS EXTRACTED OUT YOU MIGHT WANT TO PUT IT BACK LATER
colnames(data) <- c("a","b","c","d","e","f","h","pre")

# Normalize the data
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, 
                              scale = maxs - mins))

# Split the data into training and testing set


index <- sample(1:nrow(data), round(0.75 * nrow(data)))
train_ <- scaled[index,]
test_ <- scaled[-index,]

train_org <- train_ * (max(data$pre) - min(data$pre)) + min(data$pre)

#View(train_)
#View(test_)
#View(test_)

                   #YEAR IS EXTRACTED OUT YOU MIGHT WANT TO PUT IT BACK LATER

# Build Neural Network
nn <- neuralnet(pre ~ a + b + c + d + e 
                + f + h  , 
                data = train_, hidden = c(8,8,4,3),
                linear.output = TRUE, lifesign = 'full', rep = 1 , threshold=0.1 )

# Predict on test data
pr.nn <- neuralnet::compute(nn, test_[,1:8])


# Compute mean squared error
pr.nn_ <- pr.nn$net.result * (max(data$pre) - min(data$pre)) + min(data$pre)

test.r <- (test_$pre) * (max(data$pre) - min(data$pre)) + min(data$pre)

MSE.nn <- sum((test.r - round(pr.nn_))^2) / nrow(test_)
MSE.nn

plot(test.r , round(pr.nn_), col = "red", main = 'Reals vs Predicted')




#Raw
#plot(test_$pre,pr.nn$net.result  , col = "red", main = 'Reals vs Predicted')
#cor(test_$pre , pr.nn$net.result)


beep(2)

# Plot the neural network
plot(nn)

#This propb gotta be test.r not test_$pr


plot(test.r , round(pr.nn_), col = "red", main = 'Real vs Predicted')
#abline(0, 450, lwd = 2)



#plot(test_$pre ,pr.nn$net.result, col = "red", main = 'Real vs Predicted')



index = 254
paste0("Original: ", test.r[index], "     we got: ", pr.nn_[index])

cor(test.r , round(pr.nn_))

cor(test.r , pr.nn_)

real_vs_pre <- data.frame("Actual" = test.r , "Preditced" = round(pr.nn_) )


View(real_vs_pre)

strw <- nn$weights

