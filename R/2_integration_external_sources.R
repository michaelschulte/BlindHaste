# Project Blind Haste
# Michael Schulte-Mecklenbeck, Emanuel de Bellis
# integrating external data files into processed rawdata

# blank slate
rm(list = ls())

# set working directory %UPDATE%
setwd("BlindHaste/R") #Michael

# load packages
library(dplyr)
library(tidyr)

# load data
   raw.long3050 <- readRDS('../data/rawlong3050_check.RDS') # same as raw.wide but with one row per speed bracket (233008 rows)  
# holidays 2008-2009  ----

   ### identify public holidays (dates from www.feiertagskalender.ch)
   raw.long3050$holiday <- 0
   
   raw.long3050[raw.long3050$date_ == as.POSIXct("2008-03-21"),]$holiday <- 1 
   raw.long3050[raw.long3050$date_ == as.POSIXct("2008-03-24"),]$holiday <- 1
   raw.long3050[raw.long3050$date_ == as.POSIXct("2008-03-31"),]$holiday <- 1 
   raw.long3050[raw.long3050$date_ == as.POSIXct("2008-05-12"),]$holiday <- 1
    
   raw.long3050[raw.long3050$date_ == as.POSIXct("2009-04-13"),]$holiday <- 1
   raw.long3050[raw.long3050$date_ == as.POSIXct("2009-05-21"),]$holiday <- 1
   raw.long3050[raw.long3050$date_ == as.POSIXct("2009-06-01"),]$holiday <- 1
   raw.long3050[raw.long3050$date_ == as.POSIXct("2009-04-10"),]$holiday <- 1
   
# sunshine & precipitation (2007-2013) ----
# resolution = 1 hour
  
# sum of precipitation  per hour (mm)
# sum of sunshine per hour (min)  
  
# read data
  SunRainZurich <- read.csv("../data/meteoswiss/sunshine.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
# convert into POSIX format and account for time zone difference
  SunRainZurich$date_time <- as.POSIXct(as.character(SunRainZurich$time), "%Y%m%d%H", tz="UTC")
  attributes(SunRainZurich$date_time)$tzone <- "Europe/Zurich"
# delete unnecessary columns and rename them
  SunRainZurich <- subset(SunRainZurich, select=-c(time))
  SunRainZurich <- dplyr::rename(SunRainZurich, sunshine = sonnenschein)
  SunRainZurich <- dplyr::rename(SunRainZurich, precipitation = niederschlag)
   
  # replace - by NA (coercion Warning is ok - converts <NA> or - into NA)
  SunRainZurich$sunshine <- as.numeric(as.character(SunRainZurich$sunshine))
  SunRainZurich$precipitation <- as.numeric(as.character(SunRainZurich$precipitation))
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, SunRainZurich, by.x=c("date_time"), by.y=c("date_time"), all.x=TRUE)
# remove SunRainZurich as it is merged into raw.long
  rm(SunRainZurich)
  
# brightness (2007-2009) & radiation (2007-2012) --------
# resolution = 1 hour
  
# load data
  BrightZurich <- read.csv("../data/meteoswiss/brightness_radiation.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
# convert into POSIX format and account for time zone difference
  BrightZurich$date_time <- as.POSIXct(as.character(BrightZurich$time), "%Y%m%d%H", tz="UTC")
  attributes(BrightZurich$date_time)$tzone <- "Europe/Zurich"
# delete unnecessary columns
  BrightZurich <- subset(BrightZurich, select=-c(time))
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, BrightZurich, by.x=c("date_time"), by.y=c("date_time"), all.x=TRUE)
# convert to numeric (coercion Warning is ok - converts <NA> or - into NA)
  raw.long3050$radiation <- as.numeric(as.character(raw.long3050$radiation))
  raw.long3050$brightness_mv_sma <- as.numeric(as.character(raw.long3050$brightness_mv_sma))
  raw.long3050$brightness_mv_klo <- as.numeric(as.character(raw.long3050$brightness_mv_klo))
  raw.long3050$brightness_mv_smaklo <- as.numeric(as.character(raw.long3050$brightness_mv_smaklo))
# conversion from mv to lx
  raw.long3050$brightness_lx_sma <- 0
  raw.long3050$brightness_lx_sma <- 10^(((raw.long3050$brightness_mv_sma)+245)/100)
  raw.long3050$brightness_lx_klo <- 0
  raw.long3050$brightness_lx_klo <- 10^(((raw.long3050$brightness_mv_klo)+245)/100)
  raw.long3050$brightness_lx_smaklo <- 0
  raw.long3050$brightness_lx_smaklo <- 10^(((raw.long3050$brightness_mv_smaklo)+245)/100)
# transform radiation to take log10 later on (which is not possible with zeros); 
# 10 because the range of radiation is [-9,1003]
  raw.long3050$radiation <- raw.long3050$radiation+10
# remove BrightZurich as it is merged into raw.long
  rm(BrightZurich)  

### air pressure & temperature (2007-2013) --------
  # resolution = 1 hour
  
# load data
  AirTempZurich <- read.csv("../data/meteoswiss/pressure.txt", header=TRUE, sep=",", stringsAsFactors = FALSE)
# convert into POSIX format and account for time zone difference
  AirTempZurich$date_time <- as.POSIXct(as.character(AirTempZurich$time), "%Y%m%d%H", tz="UTC")
  attributes(AirTempZurich$date_time)$tzone <- "Europe/Zurich"
# delete unnecessary columns and rename them
  AirTempZurich <- subset(AirTempZurich, select=-c(time, tre200bn,tre200b0))
  AirTempZurich <- rename(AirTempZurich,air_pressure=luftdruck)
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, AirTempZurich, by.x=c("date_time"), by.y=c("date_time"), all.x=TRUE)
# convert to num
  raw.long3050$air_pressure <- as.numeric(as.character(raw.long3050$air_pressure))
  raw.long3050$temp <- as.numeric(as.character(raw.long3050$temp))
# remove SunRainZurich as it is merged into raw.long
  rm(AirTempZurich) 
  
# visibility (2007-2013) ----
  # resolution = 1 hour
  
# load data
  VisZurich <- read.csv("../data/meteoswiss/visibility.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
# convert into POSIX format and account for time zone difference
  VisZurich$date_time <- as.POSIXct(as.character(VisZurich$time), "%Y%m%d%H", tz="UTC")
  attributes(VisZurich$date_time)$tzone <- "Europe/Zurich"
# delete unnecessary columns
  VisZurich <- subset(VisZurich, select=-c(time, eye_measure))
# delete rows with visibility=NA to reduce size of the data frame
  VisZurich <- VisZurich[!is.na(VisZurich$visibility),]
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, VisZurich, by.x=c("date_time"), by.y=c("date_time"), all.x=TRUE)
# convert to num
  raw.long3050$visibility <- as.numeric(as.character(raw.long3050$visibility))
# remove VisZurich as it is merged into raw.long
  rm(VisZurich) 
  
### brightness, clouds, dog (2007-2012) -------- 
  
  # load data
  BrightZurich <- read.csv("../data/meteoswiss/brightness.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
  # convert into POSIX format and account for time zone difference
  BrightZurich$date_time <- as.POSIXct(as.character(BrightZurich$time), "%Y%m%d%H%M", tz="UTC")
  attributes(BrightZurich$date_time)$tzone <- "Europe/Zurich"
  # remove rows with NA (data are on 10 minute resolution but measured only every houer)
  BrightZurich <- BrightZurich[!is.na(BrightZurich$visibility_synop),]
  # delete unnecessary columns
  BrightZurich <- subset(BrightZurich, select=-c(time))
  # change name of brightness to distinguish from other brightness measures
  names(BrightZurich)[names(BrightZurich)=="brightness_lx_sma"] <- "brightness_lx_sma_10min"
  # merge the two data frames
  raw.long3050 <- merge(raw.long3050, BrightZurich, by.x=c("date_time"), by.y=c("date_time"), all.x=TRUE)
  # convert to num
  raw.long3050$brightness_lx_sma_10min <- as.numeric(as.character(raw.long3050$brightness_lx_sma_10min))
  raw.long3050$visibility_synop <- as.numeric(as.character(raw.long3050$visibility_synop))
  raw.long3050$fog <- as.numeric(as.character(raw.long3050$fog))
  raw.long3050$clouds <- as.numeric(as.character(raw.long3050$clouds))
  # remove BrightZurich as it is merged into raw.long
  rm(BrightZurich) 
  
### sunrise-sunset (2007-2009)  --------
# resolution = 1 hour (actually 1 minute)
  
# load data
  SunriseZurich <- read.csv("../data/sunrise_sunset/Zurich_2007-2012.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
# extract hour from exact time
  SunriseZurich$sunrise_hour <- substr(SunriseZurich$sunrise_min,1,1)
  SunriseZurich$sunset_hour <- substr(SunriseZurich$sunset_min,1,2)
# convert to numeric
  SunriseZurich$sunrise_hour <- as.numeric(SunriseZurich$sunrise_hour)
  SunriseZurich$sunset_hour <- as.numeric(SunriseZurich$sunset_hour)
# string to data and bring into same format as raw.long
  SunriseZurich$date <- strptime(as.character(SunriseZurich$Date), "%d.%m.%Y")
# convert to POSIXct (not clear why this wasn't needed for brightness!)
  SunriseZurich$date <- as.POSIXct(SunriseZurich$date)
# account for daylight saving time (DST)
# create new variable DST stating whether daylight saving time (1) or standard time (0)
  SunriseZurich$DST <- as.POSIXlt(SunriseZurich$date)$isdst
# create new variable sunrise/sunset and add one hour if DST=1
  SunriseZurich$sunrise <- 0
  SunriseZurich$sunrise <- ifelse(SunriseZurich$DST==0, SunriseZurich$sunrise_hour, SunriseZurich$sunrise_hour + 1)
  SunriseZurich$sunset <- 0
  SunriseZurich$sunset <- ifelse(SunriseZurich$DST==0, SunriseZurich$sunset_hour, SunriseZurich$sunset_hour + 1)
# delete unnecessary columns
  SunriseZurich <- subset(SunriseZurich, select=-c(sunrise_min,sunset_min,sunrise_hour,sunset_hour,Date))
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, SunriseZurich, by.x=c("date_"), by.y=c("date"), all.x=TRUE)
# create variable indicating whether day (1) or night (0)
  raw.long3050$day_night <- 0
  raw.long3050$day_night <- ifelse(((raw.long3050$time>=raw.long3050$sunrise) & (raw.long3050$time<=raw.long3050$sunset)), 1, 0)
  raw.long3050$day_night2 <- 0
  raw.long3050$day_night2 <- ifelse(((raw.long3050$time>raw.long3050$sunrise) & (raw.long3050$time<raw.long3050$sunset)), 1, 0)
# remove SunRainZurich as it is merged into raw.long
  rm(SunriseZurich)
  
### particulates (2007-2012) -------- 
  # resolution = 1 hour

# load data
  PartZurich <- read.csv("../data/particulates/particulates_hourly.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
# convert into POSIX format and account for time zone difference
  PartZurich$date_time <- as.POSIXct(as.character(PartZurich$time), "%d.%m.%Y %H:%M", tz="UTC")
  attributes(PartZurich$date_time)$tzone <- "Europe/Zurich"
# delete unnecessary columns
  PartZurich <- subset(PartZurich, select=-c(time))
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, PartZurich, by.x=c("date_time"), by.y=c("date_time"), all.x=TRUE)
# convert to num
  raw.long3050$part_ZUE <- as.numeric(as.character(raw.long3050$part_ZUE)) # 526 NAs
  raw.long3050$part_ZBW <- as.numeric(as.character(raw.long3050$part_ZBW)) # 18163 NAs
  raw.long3050$part_ZSS <- as.numeric(as.character(raw.long3050$part_ZSS)) # 1537 NAs
  raw.long3050$part_ZSW <- as.numeric(as.character(raw.long3050$part_ZSW)) # 35235 NAs
# remove PartZurich as it is merged into raw.long
  rm(PartZurich)
  
### snow (2007-2012) -------- 
# resolution = 1 day
  
# load data
  SnowZurich <- read.csv("../data/meteoswiss/snowdaily.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
# string to data and bring into same format as raw.long
  SnowZurich$date <- strptime(as.character(SnowZurich$time), "%Y%m%d")
# convert to POSIXct
  SnowZurich$date <- as.POSIXct(SnowZurich$date)
# delete unnecessary columns
  SnowZurich <- subset(SnowZurich, select=-c(time))
# merge the two data frames
  raw.long3050 <- merge(raw.long3050, SnowZurich, by.x=c("date_"), by.y=c("date"), all.x=TRUE)
# convert to num
  raw.long3050$snowrain <- as.numeric(as.character(raw.long3050$snowrain)) # 73/2174 = 1
  raw.long3050$snowblanket <- as.numeric(as.character(raw.long3050$snowblanket))  # 254/2190 = 1
  raw.long3050$snowfall <- as.numeric(as.character(raw.long3050$snowfall)) # 216/2176 = 1
# remove SnowZurich as it is merged into raw.long
  rm(SnowZurich)
  
# there are emoty date/time rows, which we remove
  # 28.03.2010, 30.03.2008 - for each of these days several removals (no other days concerned)
  raw.long3050 <- raw.long3050[which(!is.na(raw.long3050$date_time)),]
  head(raw.long3050)

# save raw.long --------
  saveRDS(raw.long3050, file='../data/rawlong3050_check.RDS')