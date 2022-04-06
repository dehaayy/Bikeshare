#Author: Deha Ay
#Contributor: Deha Ay
library(tidyverse)
library(lubridate)
library(plyr)
library(fasttime)
library(data.table)

path_to_dir_folder <- "/Users/dehaay/Desktop/Weather Data Base"

sub_paths <- list.files(path=path_to_dir_folder, pattern=NULL, all.files=FALSE, full.names=TRUE)
sub_paths

previous <- data.frame()
all_weather_df_names <- c()
#using fread instead of read.csv
for( i in sub_paths) {
  
  df <- na.omit(fread(i))
  temp_name <- substr(i, 41, tail(unlist(gregexpr(".csv", i)) - 1, n=1))
  #all_weather_df_names <- append(all_weather_df_names, temp_name, after = length(all_weather_df_names))
  assign(paste0(temp_name), df) #Writes them into a properly named df

}

##Start here trying to convert everything to Isreal's time. 

all_dates <- data.frame(seq(as.POSIXct("2012-10-01 13:00:00",tzone = "Asia/Tel_Aviv")
                            , as.POSIXct("2017-10-28 00:00:00" , tzone = "Asia/Tel_Aviv"), by="hour"))





colnames(all_dates) <- c("datetime")

time_in_isreal <- data.frame(force_tz(all_dates$datetime, tzone = "Asia/Tel_Aviv"))
colnames(time_in_isreal) <- c("datetime")

converted_NY_time <- data.frame(format(time_in_isreal$datetime, tz="America/New_York"))
colnames(converted_NY_time) <- c("datetime")
converted_NY_time <- na.omit(unique(converted_NY_time))
#This is for everysingle hour for this is the time range
#View(converted_NY_time)
#################Found the Time Domain 

humid<- data.frame( format(force_tz(humidity$datetime, tzone = "Asia/Tel_Aviv") , tz="America/New_York") , humidity$`New York`)
colnames(humid) <- c("datetime","x")
#LIGHT SAVINGS
humid <-aggregate(humid$`x`,by=list(datetime= humid$datetime),data=humid,FUN=mean)
colnames(humid) <- c("datetime","NY humidty")
#View(humid)


temp<- data.frame( format(force_tz(temperature$datetime, tzone = "Asia/Tel_Aviv") , tz="America/New_York") , temperature$`New York` - 273.15)
colnames(temp) <- c("datetime","x")
#LIGHT SAVINGS
temp <-aggregate(temp$`x`,by=list(datetime= temp$datetime),data=temp,FUN=mean)
#colnames(temp) <- c("datetime","NY temp")

windsp<- data.frame( format(force_tz(wind_speed$datetime, tzone = "Asia/Tel_Aviv") , tz="America/New_York") , wind_speed$`New York`)
colnames(windsp) <- c("datetime","x")
windsp <-aggregate(windsp$`x`,by=list(datetime= windsp$datetime),data=windsp,FUN=mean)
colnames(windsp) <- c("datetime","NY windspeed")
#View(windsp)

winddire<- data.frame( format(force_tz(wind_direction$datetime, tzone = "Asia/Tel_Aviv") , tz="America/New_York") , wind_direction$`New York`)
colnames(winddire) <- c("datetime","x")
winddire <-aggregate(winddire$`x`,by=list(datetime= winddire$datetime),data=winddire,FUN=mean)
colnames(winddire) <- c("datetime","NY winddire")
#View(windsp)

weather_desc<- data.frame( format(force_tz(weather_description$datetime, tzone = "Asia/Tel_Aviv") , tz="America/New_York") , weather_description$`New York`)
colnames(weather_desc) <- c("datetime","x")
dup_row <- which(duplicated(weather_desc$datetime))
weather_desc<- weather_desc[-c(dup_row),]
colnames(weather_desc) <- c("datetime","weather_desc")



final <- merge(x = converted_NY_time , y = humid , by = "datetime", all.x = TRUE)
final <- merge(x = final , y = temp , by = "datetime", all.x = TRUE)
final <- merge(x = final , y = windsp , by = "datetime", all.x = TRUE)
final <- merge(x = final , y = winddire , by = "datetime", all.x = TRUE)
final <- merge(x = final , y = weather_desc , by = "datetime", all.x = TRUE)


#View(final) ##With NA values


final_weather <- final %>% fill("NY temp", .direction = 'up')
final_weather <- final_weather %>% fill("NY humidty", .direction = 'up')
final_weather <- final_weather %>% fill("NY windspeed", .direction = 'up')
final_weather <- final_weather %>% fill("NY winddire", .direction = 'up')
final_weather <- final_weather %>% fill("weather_desc", .direction = 'up')

View(final_weather) #NA values being filled 
##### weather is finalized here


df <- na.omit(fread("/Users/dehaay/Desktop/Finalized Data Parts/2013June_2015Aug/deha_citibike_NETFLOW13June_15Aug.csv"))
df2 <-na.omit(fread("/Users/dehaay/Desktop/Finalized Data Parts/2015Sept_2017Dec/deha_citibike_NETFLOW15Sept_17Dec.csv"))
df_total <- rbind(df , df2 )
df_datetime <- format(ymd_hms(paste0(df_total$year ,"-",df_total$month, "-" ,df_total$day, " ",df_total$hour_interval , ":00:00")), format = "%Y-%m-%d %H:%M:%S")
df_total <- cbind(datetime = df_datetime,df_total)

merged_df <- merge(finalized_weather , df_total , all.x = FALSE)
merged_df<- merged_df[order(as.Date(merged_df$datetime),merged_df$hour_interval),]

##replacing date time to the fist column
replacing_datetime <- subset (merged_df, select = -datetime)

merged_df <- cbind(datetime = merged_df$datetime , replacing_datetime)
rownames(merged_df) <- c(1:nrow(merged_df)) #Later for it to not cause error, assign the row names alphabetically
View(merged_df)


#####ENDED
# DATA LOSS DUE TO DAY LIGHT SAVING -> 4

fwrite(merged_df,"/Users/dehaay/Desktop/FINAL DATA/FinalMLData.csv", row.names = FALSE)


