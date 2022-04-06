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

data <- data[-c(which(data$end_station_name == "")),]


#here the library lubridate is used, swap "ymd_hms" with "as.POSIXct" if the library is not accepted
data$hour_interval <- as.numeric(format(ymd_hms(data$started_at), format = "%H"))
data$month <- as.numeric(format(ymd_hms(data$started_at), format = "%m"))
data$day <- as.numeric(format(ymd_hms(data$started_at), format = "%d"))
data$year <- as.numeric(format(ymd_hms(data$started_at), format = "%Y"))

##Weekdays start from Monday(1) ends at Sunday (7)
data$weekday <- lubridate::wday(data$started_at, week_start = 1 )
#Alternativeifelse(wday(data$started_at)==1,7,wday(data$started_at)-1)

######################Data is broken down into date details at this point###################


###################### Frequency Table Start building ###################
#creates a table from start station to end station transaction count

start_to_end_st <- data.frame( start = data$start_station_name, end = data$end_station_name )
start_frequency_summary <- data.frame(table(start_to_end_st))

end_to_start_st <- data.frame( end = data$end_station_name,  start = data$start_station_name)
end_frequency_summary <- data.frame(table(end_to_start_st))


#min_hourly_allowed <- 7 #what is the minimum allowed transactions to be considered ##Gotta work on the details

#focused_frequency <- start_frequency_summary[which(start_frequency_summary$Freq > min_hourly_allowed),]

##A POSSIBLE PROBLEM HERE with Frequency summary, not really, we have to decide 
#Under what condition we will be considering that station valid, currently 

total_transaction_start_station <- data.frame(tapply(start_frequency_summary$Freq,start_frequency_summary$start,  FUN=sum))

total_transaction_end_station <- data.frame(tapply(end_frequency_summary$Freq,end_frequency_summary$end,  FUN=sum))
#total_transaction_end_station$station_name <- rownames(total_transaction_end_station)

###################### Frequency Table is created at this point ###################
#frequency_summary <- start_frequency_summary

main_dataset <- data ##Picks the dataset we will be using, separate data set for easier tracking

hour_cutoff_rows <- cumsum(rle(main_dataset$hour_interval)$lengths) #Index numbers for where hour_index change (for dividing the data into chunks)

on_average_hourly_trans <- floor(total_transaction_start_station/(length(sub_paths) * 730.5))
on_average_hourly_trans$station_name <- rownames(on_average_hourly_trans)

#total_transaction_start_station$station_name <- rownames(total_transaction_start_station) #formatting the data adding the rownames as column for later call
#total_transaction_end_station$station_name <- rownames(total_transaction_end_station)


##Station names that pass the treshold
#valid_stations <- c(on_average_hourly_trans[which(on_average_hourly_trans$tapply.start_frequency_summary.Freq..start_frequency_summary.start.. >= min_hourly_allowed),]$station_name)

##We have to commanize this column among all data sets thus I built a different model to come up with these
valid_stations <- c("1 Ave & E 68 St","Broadway & W 25 St","11 Ave & W 41 St","7 Ave & Central Park South","E 13 St & Avenue A","E 33 St & 1 Ave","Grand St & Elizabeth St","West St & Liberty St","1 Ave & E 16 St","E 10 St & Avenue A"
                    ,"Pier 40 - Hudson River Park","W 30 St & 10 Ave","12 Ave & W 40 St","6 Ave & W 33 St","8 Ave & W 31 St","8 Ave & W 33 St","9 Ave & W 22 St","Broadway & E 14 St","Carmine St & 6 Ave","Central Park S & 6 Ave"
                    ,"Christopher St & Greenwich St","Cleveland Pl & Spring St","E 17 St & Broadway","Grand Army Plaza & Central Park S","Greenwich Ave & 8 Ave","Lafayette St & E 8 St","Pershing Square North","University Pl & E 14 St","W 18 St & 6 Ave"
                    ,"W 21 St & 6 Ave","W 24 St & 7 Ave","W 31 St & 7 Ave","W 33 St & 7 Ave","W 4 St & 7 Ave S","W 41 St & 8 Ave","West St & Chambers St")
valid_stations <- valid_stations[order(as.character(valid_stations))]

#https://convert.town/column-to-comma-separated-list
###length(valid_stations)

#Creates an empty data.frame for the FINAL DATA SET
by_hour_data<- data.frame(matrix(ncol = length(valid_stations) + 5, nrow = 0))
#Plus 5 is the addition of all time columns


start_row <- 1
end_row <- hour_cutoff_rows[1]

for (i in c(1: length(hour_cutoff_rows) - 1 )){
  
  #each hour's data chunk
  df <- (main_dataset[start_row:end_row,])
  
  valid_station_df <- data.frame(valid_stations) #empty dataframe of the valid stations that are above the min_hourly_allowed
  
  #Each chunk's all start station count
  all_station_count_df <- data.frame(table(df$start_station_name))
  #Each chunk's only the valid start station count, the dataframe is not yet finalized
  all_station_count_df <- all_station_count_df[ is.element(all_station_count_df$Var1,c(valid_stations)) , ]
  
  names(all_station_count_df)[names(all_station_count_df) == "Var1"] <- "valid_stations"
  
  #Finalized each chunk's valid start station count
  valid_station_count_df <- merge(all_station_count_df, valid_station_df, by='valid_stations', all.y  = TRUE)
  valid_station_count_df[is.na(valid_station_count_df)] <- 0
  
  #it is sorted so every piece of data is recorded under the same accurate column name.
  sorted_valid_station_count_df <- valid_station_count_df[order(as.character(valid_station_count_df$valid_stations)),]
  
  
  valid_freq_df<- t(sorted_valid_station_count_df$Freq)
  
  
  colnames(valid_freq_df) <- valid_stations
  
  #FOR DEBUGING
  # print("--------------------")
  # print(df$hour_interval[1])
  # print(valid_station_count_df)
  # print("--------------------")
  
  #adds each row to the final data set
  by_hour_data[i+1,] <- data.frame(df$hour_interval[1], df$day[1],df$weekday[1],df$month[1], df$year[1], valid_freq_df)
  
  
  start_row <- end_row + 1
  end_row <- hour_cutoff_rows[i + 2]
  
}

colnames(by_hour_data) <- c("hour_interval","day","weekday","month","year",valid_stations)


View(by_hour_data)

fwrite(by_hour_data,"/Users/dehaay/Desktop/CitiBike_Outputs/deha_citibikeMLreprt.csv", row.names = FALSE)




##Checking for the accuracy of the table values: 
length(which(data[which(data$hour_interval == 9 
                        & data$day == 26 
                        & data$weekday == 1  
                        & data$month == 8 
                        & data$year == 2013  ) ,]$start_station_name
                        == "Lafayette St & E 8 St"))
            #make sure you change the start_station to end_station if you are looking for that#

