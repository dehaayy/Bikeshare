#Once installed comment this line out

#install.packages("jsonlite")

#Epoch Unix Timestamp
#WEBSITE TO RETRIVE THE JSON LINKS:
   #https://gbfs.cogobikeshare.com/gbfs/gbfs.json 

library("jsonlite")



  
station_information <- as.data.frame(jsonlite::fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json"))
View(station_information)

station_status <- as.data.frame(jsonlite::fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_status.json"))
View(station_status)

free_bike_status <- jsonlite::fromJSON("https://gbfs.citibikenyc.com/gbfs/en/free_bike_status.json")






last_updated_full_date <- as.POSIXct(station_information$last_updated, origin="1970-01-01")


lu_time <- format(last_updated_full_date, format = "%H:%M:%S")
lu_date <- format(last_updated_full_date, format = "%m/%d/%Y")

last_updated_full_date


#Variables
 #lu_time : Last Updated time is extracted -> hour minute second only
 # lu_date: Last updated date is extracted