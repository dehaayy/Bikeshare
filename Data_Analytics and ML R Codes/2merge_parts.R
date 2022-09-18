library(readxl)
library(lubridate)
library(plyr)
library(fasttime)
library(data.table)


path_to_dir_folder <- "/Users/dehaay/Desktop/untitled folder 2/Start"

sub_paths <- list.files(path=path_to_dir_folder, pattern=NULL, all.files=FALSE, full.names=TRUE)
sub_paths

previous <- data.frame()

#using fread instead of read.csv
for( i in sub_paths) {
  print(i)
  data_being_read <- na.omit(fread(i))
  
  data <- rbind(previous, na.omit(data_being_read) )
  
  previous <- data
  
}
View(data)

full_date <- paste0(data$month,"/",data$day,"/",data$year," ",data$hour_interval)
date <- as.character(as.POSIXct(full_date, format = "%m/%d/%Y %H", tz = "America/New_York"))
data <- cbind(date,data)
data<- data[order(data$date),]



fwrite(data,"/Users/dehaay/Desktop/trial output/Start_trail.csv", row.names = FALSE)


View(data)


checkzerper <-function(x){
  return(  length(which(x == 0))/ length(x)  )
}

test_0 <- data[,7:20]
apply(test_0,2,checkzerper) * 100 ##perecent of zeros



#Percent data among all columns that is zero
test_data <- data[53554:length(data$date),7:20] #2019-07-22 00:00:00   53554
length(which(unlist(c(test_data)) == 0)) / length(unlist(c(test_data))) 

test_0 <- test_data
apply(test_0,2,checkzerper) * 100 ##perecent of zeros


##use to remove specific columns and test
test_data <- subset(test_data, select = -c(`6 Ave & W 34 St`, `W 89 St & Columbus Ave`)) 
length(which(unlist(c(test_data)) == 0)) / length(unlist(c(test_data))) 
