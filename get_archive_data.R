library(lubridate)
library(dplyr)
library(units)
library(opensensmapr)

#Get all senseboxes
boxes = osem_boxes()

#Filter boxes to Nord Rhein Westphalen
nrw_boxes <<- boxes %>%
  filter(lon >= 5.8664 & lon <= 9.4623 & lat >= 50.3276 & lat <= 52.5325) %>%
  filter(!is.na(lastMeasurement)) %>%
  filter(as.Date(lastMeasurement) == Sys.Date()) %>%
  filter(exposure == "outdoor")

#Get box measurements archive based on the box id and the desired phenomenon
getRecord <- function(id, phenom, from, to) { #from and to in format 'MM-DD-YYYY'
  sensors = ~ phenomenon %in% phenom #Choose phenomena
  dat = osem_measurements_archive(
          id,
          as.POSIXlt(from), #From
          as.POSIXlt(to), #To
          sensorFilter = sensors
      )
  return(dat)
}

df_total = data.frame() #Data frame containing all data

#Iterate through archives for each box
for (i in 1:nrow(nrw_boxes)) {
  print(i)
  
  box = osem_box(nrw_boxes$X_id[i])
  print(nrw_boxes$X_id[i]) 
  
  #Some box archives produce server errors or format errors which ends the osem_measurements_archive function. 
  #try bypasses these errors so the program can run fully and skip the values that return server or format errors 
  record = try(getRecord(box, 'PM10', '06-01-2019', '06-30-2019')) # From and to parameter in 'MM-DD-YYYY'
  if("try-error" %in% class(record)) next
  
  #Add id, lon, and lat. These need to be manually added
  record$id <- rep(nrw_boxes$X_id[i],nrow(record))
  record$lon <- rep(nrw_boxes$lon[i],nrow(record))
  record$lat <- rep(nrw_boxes$lat[i],nrow(record))
  #Bind ith box to df_total
  if(ncol(record) == 5){
    df_total <<- rbind(df_total, record)
  } else {
    next
  }
}

#Clean outliers based off cook's distance. In this example I am using PM10 values for the model and cleaning
dust_model <- lm(PM10 ~ createdAt, data = df_total, na.action = na.omit)
cooksd <- cooks.distance(dust_model)
dust_influential <- as.numeric(names(cooksd)[(cooksd > (4/nrow(df_total)))]) 
df_total$PM10[dust_influential] 
influential_df <- data.frame(PM10 = df_total$PM10[dust_influential])

clean_dust <<- df_total %>%
  filter(!df_total$PM10 %in% influential_df$PM10) %>%
  select(createdAt, PM10, id, lat, lon)



