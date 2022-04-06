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
