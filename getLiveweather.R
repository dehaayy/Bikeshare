#Once installed comment this line out

#install.packages("jsonlite")

#Epoch Unix Timestamp
#WEBSITE TO RETRIVE THE JSON LINKS:
#https://gbfs.cogobikeshare.com/gbfs/gbfs.json 

library("jsonlite")


weather_info <- as.data.frame(jsonlite::fromJSON("https://api.openweathermap.org/data/2.5/weather?lat=40.730610&lon=-73.935242&appid=4a7c1c3a34bdf586deb2dc335fe4458d"))
View(weather_info)