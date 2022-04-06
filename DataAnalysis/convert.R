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

