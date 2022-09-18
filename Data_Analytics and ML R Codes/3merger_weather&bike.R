bike_data <- fread("/Users/dehaay/Desktop/ie332 bikeshare project/Data_Analytics and ML R Codes/test_Start.csv")
weather_data <- fread("/Users/dehaay/Desktop/ie332 bikeshare project/Finalized_data/fullweatherdata.csv")

View(bike_data)
View(weather_data)

str(bike_data$date)
str(weather_data$date)


mergy <- merge(bike_data,weather_data, by= "date", all.x = TRUE)

weather_part <- mergy[,21:26]
date_part <- mergy[,]
bike_part <- mergy[,]
final_data <- cbind(date = mergy$date,date_part,weather_part,bike_part)
final_data <- na.omit(final_data)

View(final_data)

mergy <- mergy[!duplicated(mergy)]
View(mergy)
fwrite(mergy,"/Users/dehaay/Desktop/ie332 bikeshare project/CitiBike_Outputs/test_start_ML.csv", row.names = FALSE)
