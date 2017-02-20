# Project Blind Haste
# Michael Schulte-Mecklenbeck, Emanuel de Bellis
# loading and cleaning data files
  
# blank slate
  rm(list = ls())

  ############### UPDATE ###############
# set working directory
  setwd("BlindHaste/R")
  ############### UPDATE ###############

# load packages
  library(dplyr)
  library(tidyr)
  
# data preparation --------
  ### read the file into a dataframe (list)
  raw.wide <- read.csv("../data/traffic/MasterRaw.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
  
  ### data stuff
  # merge date and time into a POSIX variable
  raw.wide$date_2  <- paste(raw.wide[,"date"], raw.wide[,"time"], sep=".")
  raw.wide$date_time <- strptime(as.character(raw.wide$date_2), "%d.%m.%Y.%H", tz="Europe/Zurich") # str -> "2008-08-26 00:00:00 CET"
  raw.wide$date_time <- as.POSIXct(raw.wide$date_time) # convert into POSIXct
  raw.wide <- subset(raw.wide, select=-c(date_2)) # date_2 not needed
  
  raw.wide$date_ <- strptime(as.character(raw.wide$date), "%d.%m.%Y", tz="Europe/Zurich") # str -> "2008-08-26 00:00:00 CET"
  raw.wide$date_ <- as.POSIXct(raw.wide$date_) # convert into POSIXct
  
  ### split up date into year, month, day
    raw.wide$year <- format(raw.wide$date_time, "%Y")
    # extract month
    raw.wide$month <- format(raw.wide$date_time, "%b")
    # extract day
    raw.wide$day <- format(raw.wide$date_time, "%wday")
    # extract number of day 
    raw.wide$day_num <- format(raw.wide$date_time, "%d")  
    
    # check for number of measured cars going into the merging process
    # subset dataframe
   testsum <-  raw.wide %>%
      dplyr::select(date_, X15,X20,X25, X30, X35, X40, X45, X50, X55, X60, X61, X65, X70, X71, X75, X80, X85, X90, X91) %>%
      dplyr::filter(date_ < '2009-08-24')
    
# merging maxspeed=30 ----
  # merge X61 with X65, X70, X71, X75
  sum(is.na(raw.wide$X71)[raw.wide$maxspeed==30]) # many maxspeed=30 measures do not differentiate between 65,70,75 (e.g., Brunaustrasse_58)
  raw.wide[raw.wide$maxspeed==30,]$X61 <- 
    rowSums(raw.wide[raw.wide$maxspeed==30,c("X65","X70","X71","X75")], na.rm=TRUE)
  # replace redundant information by NA
  raw.wide[raw.wide$maxspeed==30,]$X65 <- NA
  raw.wide[raw.wide$maxspeed==30,]$X70 <- NA
  raw.wide[raw.wide$maxspeed==30,]$X71 <- NA
  raw.wide[raw.wide$maxspeed==30,]$X75 <- NA
  
  # merge X30 with X20, X25
  sum(!is.na(raw.wide$X25)[raw.wide$maxspeed==30])
  raw.wide[raw.wide$maxspeed==30,]$X30 <- 
    rowSums(raw.wide[raw.wide$maxspeed==30,c("X15","X20","X25","X30")], na.rm=TRUE)
  # replace redundant information by NA
  raw.wide[raw.wide$maxspeed==30,]$X15 <- NA
  raw.wide[raw.wide$maxspeed==30,]$X20 <- NA
  raw.wide[raw.wide$maxspeed==30,]$X25 <- NA

# merging maxspeed=50 ----
  # merge X61 with X65, X70, X75, X80, X85, X90, X91
  sum(is.na(raw.wide$X91)[raw.wide$maxspeed==50]) # important: to have more speed brackets, it might be better to exclude measures with X70=NA and X75=NA (if not dichotomizing)
  raw.wide[raw.wide$maxspeed==50,]$X61 <- 
    rowSums(raw.wide[raw.wide$maxspeed==50,c("X65","X70","X75","X80","X85","X90","X91")], na.rm=TRUE)
  # replace redundant information by NA
  raw.wide[raw.wide$maxspeed==50,]$X65 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X70 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X75 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X80 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X85 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X90 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X91 <- NA
  
  # merge X50 with X20, X25, X30, X35, X40, X45
  sum(!is.na(raw.wide$X30)[raw.wide$maxspeed==50]) # most maxspeed=50 measures differentiate between 30,35,40,45,50 - might be interesting for traffic jam issue
  sum(!is.na(raw.wide$X20)[raw.wide$maxspeed==50]) # however, only few differentiate between 20,25,30  
  raw.wide[raw.wide$maxspeed==50,]$X50 <- 
    rowSums(raw.wide[raw.wide$maxspeed==50,c("X15","X20","X25","X30","X35","X40","X45","X50")], na.rm=TRUE)
  # replace redundant information by NA
  raw.wide[raw.wide$maxspeed==50,]$X15 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X20 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X25 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X30 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X35 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X40 <- NA
  raw.wide[raw.wide$maxspeed==50,]$X45 <- NA
  
  ### get distribution over maxspeed
  table(raw.wide$maxspeed) # only about two measurements for maxspeed=60!
  
  raw.long <- gather(raw.wide, "bracket", value, c(4:22)) # 1027710
  
  # count and delete resulting NAs
  sum(is.na(raw.long$value)) # almost half of the rows are NAs (625997/ 1,020,794)
  raw.long <- raw.long[!(is.na(raw.long$value)),] # 403974
  
  # rename variables
  colnames(raw.long)[names(raw.long) == 'bracket'] <- "speed"
  colnames(raw.long)[names(raw.long) == 'value'] <- "freq" 
  
  # transfer structure of speed into numeric
  raw.long$speed <- as.numeric(gsub("X","", as.character(raw.long$speed)))
  
  # subset to 30 and 50 zone
  raw.long3050 <- filter(raw.long, maxspeed == 30 | maxspeed == 50) #357204
  
  # dichotomize speed into normal and speeding behavior for 
  # 25, 35, 55
  raw.long3050$speed_dich <- 0
  raw.long3050[raw.long3050$maxspeed == 30,]$speed_dich <- ifelse(raw.long3050[raw.long3050$maxspeed == 30,]$speed<=35, 
                                                          raw.long3050[raw.long3050$maxspeed == 30,]$speed_dich<-"normal", 
                                                          raw.long3050[raw.long3050$maxspeed == 30,]$speed_dich<-"speeding")
  raw.long3050[raw.long3050$maxspeed == 50,]$speed_dich <- ifelse(raw.long3050[raw.long3050$maxspeed == 50,]$speed<=55, 
                                                          raw.long3050[raw.long3050$maxspeed == 50,]$speed_dich<-"normal", 
                                                          raw.long3050[raw.long3050$maxspeed == 50,]$speed_dich<-"speeding")
  # percent of speeders
  100/sum(raw.long3050$freq, na.rm=TRUE) * sum(raw.long3050[raw.long3050$speed_dich=="speeding",]$freq, na.rm=TRUE)

  # same with accurate cutoff (acc)
  # 20, 30, 50
  raw.long3050$speed_dich_acc <- 0
  raw.long3050[raw.long3050$maxspeed == 30,]$speed_dich_acc <- ifelse(raw.long3050[raw.long3050$maxspeed == 30,]$speed<=30, 
                                                              raw.long3050[raw.long3050$maxspeed == 30,]$speed_dich_acc<-"normal", 
                                                              raw.long3050[raw.long3050$maxspeed == 30,]$speed_dich_acc<-"speeding")
  raw.long3050[raw.long3050$maxspeed == 50,]$speed_dich_acc <- ifelse(raw.long3050[raw.long3050$maxspeed == 50,]$speed<=50, 
                                                              raw.long3050[raw.long3050$maxspeed == 50,]$speed_dich_acc<-"normal", 
                                                              raw.long3050[raw.long3050$maxspeed == 50,]$speed_dich_acc<-"speeding")
  # percent of speeders
  100/sum(raw.long3050$freq, na.rm=TRUE) * sum(raw.long3050[raw.long3050$speed_dich_acc=="speeding",]$freq, na.rm=TRUE)
    

# split data frames by weekdays/weekends ----
  # convert to character string
  raw.long3050$day <- as.character(raw.long3050$day)
  
  raw.long3050$daytype <- 'weekday'
  raw.long3050$daytype <- ifelse(raw.long3050$day=='0day' | raw.long3050$day=='6day' | raw.long3050$holiday==1, 'weekend_holiday', 'weekday')
  
  # compute sum over rows, i.e. overall measurement (cars) per hour
  raw.wide$cars_per_hour <- with(raw.wide, rowSums(cbind(X30,X35,X40,X45,X50,X55,X60,X65,X70,X71), na.rm = TRUE))
  
  # remove data after 2009
  raw.long3050 <- raw.long3050[(raw.long3050$date_<="2009-08-24"),]
  
### save data frames -----
  saveRDS(raw.wide, file=("../data/rawwide_check.RDS"))
  saveRDS(raw.long, file=("../data/rawlong_check.RDS"))
  saveRDS(raw.long3050, file=("../data/rawlong3050_check.RDS")) #233008

  