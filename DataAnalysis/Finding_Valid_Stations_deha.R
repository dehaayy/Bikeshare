##Author: Deha Ay
##Contributors: Deha Ay
##THIS DATA ONLY PRODUCES THE REPORT OF THE START STATION DATA


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
  
  ########Corrects and old formatted data########
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
  ################################################  
  
  if (!is.POSIXct(data_being_read$started_at)) {
    print("The odd date format  fixed")
    data_being_read$started_at <- lubridate::mdy_hms(data_being_read$started_at)
    data_being_read$ended_at <- lubridate::mdy_hms(data_being_read$ended_at)
    data_being_read$ride_id <- as.character(data_being_read$ride_id)
    data_being_read$start_station_id <- as.character(data_being_read$start_station_id)
    data_being_read$end_station_id <- as.character(data_being_read$end_station_id)
  }
  
  data <- rbind(previous, na.omit(data_being_read) )
  
  
  previous <- data
  
}
######################Data is Merged at this point in "data"###################

data <- data[order(data$started_at),]
copy <- data

if (length(which(data$end_station_name == "")) > 0) {
  data <- data[-c(which(data$end_station_name == "")),]
}


start_to_end_st <- data.frame( start = data$start_station_name, end = data$end_station_name )
start_frequency_summary <- data.frame(table(start_to_end_st))

end_to_start_st <- data.frame( end = data$end_station_name,  start = data$start_station_name)
end_frequency_summary <- data.frame(table(end_to_start_st))


min_hourly_allowed <- 7 #what is the minimum allowed transactions to be considered ##Gotta work on the details

#focused_frequency <- start_frequency_summary[which(start_frequency_summary$Freq > min_hourly_allowed),]

##A POSSIBLE PROBLEM HERE with Frequency summary, not really, we have to decide 
#Under what condition we will be considering that station valid, currently 

total_transaction_start_station <- data.frame(out = tapply(start_frequency_summary$Freq,start_frequency_summary$start,  FUN=sum))
total_transaction_start_station$station_name <- rownames(total_transaction_start_station) #formatting the data adding the rownames as column for later call

total_transaction_end_station <- data.frame(inn = tapply(end_frequency_summary$Freq,end_frequency_summary$end,  FUN=sum))
total_transaction_end_station$station_name <- rownames(total_transaction_end_station)

total_transaction_df <- merge(total_transaction_start_station,total_transaction_end_station, by = "station_name")
View(total_transaction_df)

total_net_flow_df <- data.frame(station_name = total_transaction_df$station_name,net = (total_transaction_df$inn - total_transaction_df$out))

daily_average_netflow <- floor(total_net_flow_df$net/(length(sub_paths) * 30.4167)) #Daily estimated net flow 
##get the data frame 

daily_average_netflow <- data.frame(station_name = total_transaction_df$station_name, daily_net = daily_average_netflow )

top_positive_sorted <-  daily_average_netflow[order(daily_average_netflow$daily_net, decreasing = TRUE),]
top_negative_sorted <-  daily_average_netflow[order(daily_average_netflow$daily_net, decreasing = FALSE),]


positive_flow_stations <- top_positive_sorted[1:7,]$station_name
negative_flow_stations <- top_negative_sorted[1:7,]$station_name

valid_stations <- unique(c(positive_flow_stations,negative_flow_stations))

fwrite(top_positive_sorted,"/Users/dehaay/Desktop/valid_stations/top_positive_sorted.csv", row.names = TRUE)

fwrite(total_net_flow_df,"/Users/dehaay/Desktop/valid_stations/total_net_flow_df.csv", row.names = TRUE)

fwrite(total_transaction_start_station,"/Users/dehaay/Desktop/valid_stations/total_transaction_start_station.csv", row.names = TRUE)

fwrite(data.frame(valid_stations),"/Users/dehaay/Desktop/valid_stations/valid_stations.csv", row.names = TRUE)

View(total_net_flow_df)
View(top_positive_sorted)
View(valid_stations)

