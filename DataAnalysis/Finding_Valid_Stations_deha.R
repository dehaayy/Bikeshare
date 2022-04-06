#Author: Deha Ay
#Contributors: Deha Ay

#install.packages("readxl")
#install.packages("lubridate")
#Install all the packages
library(readxl)
library(lubridate)
library(plyr)
library(fasttime)
library(data.table)


path_to_dir_folder <- "/Users/dehaay/Desktop/CitiBike_FullData"

sub_paths <- list.files(path=path_to_dir_folder, pattern=NULL, all.files=FALSE, full.names=TRUE)
sub_paths

previous <- data.frame()

#using fread instead of read.csv
for( i in sub_paths) {
  print(i)
  
  data_being_read <- na.omit(fread(i))
  
  ##Corrects and old formatted data
  if(colnames(data_being_read)[1] == "tripduration") {
    print("OLD FILE")
    data_being_read <- subset(data_being_read, select = -c(1,15, 14))
    data_being_read$rideable_type <- "old_data"
    data_being_read <- data.frame(data_being_read$bikeid, data_being_read$rideable_type, data_being_read$starttime,
                                  data_being_read$stoptime,data_being_read$`start station name`,data_being_read$`start station id`,
                                  data_being_read$`end station name`, data_being_read$`end station id`, data_being_read$`start station latitude`,
                                  data_being_read$`start station longitude`,data_being_read$`end station latitude`,
                                  data_being_read$`end station longitude`,data_being_read$usertype)
    colnames(data_being_read) <- c("ride_id",	"rideable_type",	"started_at"	,
                                   "ended_at",	"start_station_name",	"start_station_id",	
                                   "end_station_name",	"end_station_id",	"start_lat",	"start_lng",	"end_lat" ,	"end_lng",	"member_casual")
  } else if (colnames(data_being_read)[1] == "Trip Duration") {
    print("MID OLD FILE")
    data_being_read <- subset(data_being_read, select = -c(1,15, 14))
    data_being_read$rideable_type <- "old_data"
    
    data_being_read <- data.frame(data_being_read$`Bike ID`, data_being_read$rideable_type, data_being_read$`Start Time`,
                                  data_being_read$`Stop Time`, data_being_read$`Start Station Name`, data_being_read$`Start Station ID`,
                                  data_being_read$`End Station Name`, data_being_read$`End Station ID`, data_being_read$`Start Station Latitude`,
                                  data_being_read$`Start Station Longitude`,data_being_read$`End Station Latitude`,
                                  data_being_read$`End Station Longitude`,data_being_read$`User Type`)
    colnames(data_being_read) <- c("ride_id",	"rideable_type",	"started_at"	,
                                   "ended_at",	"start_station_name",	"start_station_id",	
                                   "end_station_name",	"end_station_id",	"start_lat",	"start_lng",	"end_lat" ,	"end_lng",	"member_casual")
    
  } else if(colnames(data_being_read)[1] != "ride_id") {
    stop("Unknown Data Type Please Record The File Name, We Might Need To Update The Code")
    
  }
  
  data <- rbind(previous, na.omit(data_being_read) )
  previous <- data
  
}
######################Data is Merged at this point in "data"###################

data <- data[order(data$started_at),]
data <- data[-c(which(data$end_station_name == "")),]

start_to_end_st <- data.frame( start = data$start_station_name, end = data$end_station_name )
start_frequency_summary <- data.frame(table(start_to_end_st))

end_to_start_st <- data.frame( end = data$end_station_name,  start = data$start_station_name)
end_frequency_summary <- data.frame(table(end_to_start_st))


min_hourly_allowed <- 7 #what is the minimum allowed transactions to be considered ##Gotta work on the details

#focused_frequency <- start_frequency_summary[which(start_frequency_summary$Freq > min_hourly_allowed),]

##A POSSIBLE PROBLEM HERE with Frequency summary, not really, we have to decide 
#Under what condition we will be considering that station valid, currently 

total_transaction_start_station <- data.frame(tapply(start_frequency_summary$Freq,start_frequency_summary$start,  FUN=sum))
#total_transaction_start_station$station_name <- rownames(total_transaction_start_station) #formatting the data adding the rownames as column for later call

total_transaction_end_station <- data.frame(tapply(end_frequency_summary$Freq,end_frequency_summary$end,  FUN=sum))
#total_transaction_start_station$station_name <- rownames(total_transaction_start_station)

###################### Frequency Table is created at this point ###################
frequency_summary <- start_frequency_summary

main_dataset <- data ##Picks the dataset we will be using, separate data set for easier tracking

hour_cutoff_rows <- cumsum(rle(main_dataset$hour_interval)$lengths) #Index numbers for where hour_index change (for dividing the data into chunks)

on_average_hourly_trans <- floor(total_transaction_start_station/(length(sub_paths) * 730.5))
on_average_hourly_trans$station_name <- rownames(on_average_hourly_trans)

valid_stations <- c(on_average_hourly_trans[which(on_average_hourly_trans$tapply.start_frequency_summary.Freq..start_frequency_summary.start.. >= min_hourly_allowed),]$station_name)
#length(valid_stations)

fwrite(total_transaction_start_station,"/Users/dehaay/Desktop/2022Report/total_transaction_start_station.csv", row.names = TRUE)

fwrite(total_transaction_end_station,"/Users/dehaay/Desktop/2022Report/total_transaction_end_station.csv", row.names = TRUE)

fwrite(on_average_hourly_trans,"/Users/dehaay/Desktop/2022Report/start_on_average_hourly_trans.csv", row.names = TRUE)

fwrite(data.frame(valid_stations),"/Users/dehaay/Desktop/2022Report/valid_stations2.csv", row.names = TRUE)



