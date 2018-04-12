
#' Reads a csv output file from BatExplorer
#' 
#' \code{read_batexplorer} retruns a formatted tibble with consistent column names.
#' 
#' 
read_batexplorer <- function(FileName, MetaData) {

  
  
  # For BatLoggerFile
  
  # Load csv file of data (Excel or CSV?) CSV has less size limits and more transferable
  
  # Use readr package to load CSV with column Names
  
  suppressMessages(LoggerData <- readr::read_csv(FileName, col_names= T))
  
  #CHECK DATA #################################################
  #Check Required columns are present
  
  Stopif_TRUE <- FALSE # code note - make correct length vector
  
  Stopif_TRUE[1] <- Check_Static_Column(LoggerData, "Timestamp")
  Stopif_TRUE[2] <- Check_Static_Column(LoggerData, "Species Text")
  Stopif_TRUE[3] <- Check_Static_Column(LoggerData, "Calls [#]")
  Stopif_TRUE[4] <- Check_Static_Column(LoggerData, "Latitude [WGS84]")
  Stopif_TRUE[5] <- Check_Static_Column(LoggerData, "Longitude [WGS84]")

  if(TRUE %in% Stopif_TRUE) {
    stop("Data not present")
  }
  
  #END CHECK DATA #############################################
  
  #Species is decribed with scientific name 
  LoggerData$ScientificName  <- LoggerData$`Species Text`  
  
  #Accept just Scientific names on the list
  LoggerData$ScientificName   <- Convert_to_Sci(LoggerData$ScientificName)
  
  #Call/pulses (not used with initial visualisation); but a useful metric 
  LoggerData$CallsPulses <- LoggerData$`Calls [#]` 
  
  
  # DateTime is used for each bat observation; a better description than TimeStamp
  #First convert character to time
  LoggerData$DateTime <- lubridate::dmy_hms(LoggerData$Timestamp)
  
  # Time zone is set to "Europe/London" - see https://www.iana.org/time-zones
  # Allows for daylight saving
  LocalTime <- "Europe/London"
  LoggerData$DateTime <- lubridate::with_tz(LoggerData$DateTime, LocalTime)
  
  LoggerData$Latitude <- LoggerData$`Latitude [WGS84]`
  LoggerData$Longitude <- LoggerData$`Longitude [WGS84]`
  
  # Add meta data from vector
  
  
  LoggerData$SiteName <- MetaData[1]
  LoggerData$DetectorLocation <- MetaData[2]
  LoggerData$Consultant <- MetaData[3]
  LoggerData$DetectorMake <- MetaData[4]
  LoggerData$DetectorModel <- MetaData[5]
  LoggerData$AnalysisSoftware <- MetaData[6]
  LoggerData$SoftwareVersion <- MetaData[7]
  LoggerData$PassDefinition <- MetaData[8]
  LoggerData$ClassificationMethod <- MetaData[9]
  LoggerData$DataSensitivity <- MetaData[10]
  LoggerData$Notes <- MetaData[11]
  
  #Select only the columns needed
  
  LoggerData <- LoggerData %>% 
    select(SiteName,
           DetectorLocation,
           Consultant,
           DateTime, 
           ScientificName, 
           CallsPulses, 
           Latitude, 
           Longitude,
           DetectorMake,
           DetectorModel,
           AnalysisSoftware,
           SoftwareVersion,
           PassDefinition,
           ClassificationMethod,
           DataSensitivity,
           Notes,
           -`Calls [#]`,
           -`Mean Peak Frequency [kHz]`)
  
  return(LoggerData)
  
}



# Check functions
# Check columns are there
Check_Static_Column <- function(df, ColumnName) {
  
  if(!ColumnName %in% colnames(df)) {
    
    cat(stringr::str_c("Required data column ", ColumnName, " is missing\n"))
    
    ErrorFound <- T
    
  } else {
    
    ErrorFound <- F
    
  }
  
  return(ErrorFound)
}


## Check date is ymd formate and return TRUE
is_date_ymd <- function(d) {
  
  #Remove NAs from vector
  d <- d[!is.na(d)]
  date_ymd <- lubridate::ymd(d)
  if (is.na(date_ymd)) {
    ymd_order <- FALSE # order is not dmy
  } else {
    
    ymd_order <- TRUE
  }
  
  return(ymd_order)
  
}

#' Reads a csv output file from Kaleidoscope
#' 
#' \code{read_kaleidoscope} retruns a formatted tibble with consistent column names.
#' 
#' 
read_kaleidoscope <- function(FileName, MetaData, long, lat) {
  
  # Use readr package to load CSV with column Names
  
  suppressMessages(KData <- readr::read_csv(FileName, col_names= T))
  
  
  #CHECK DATA #################################################
  #Check Required columns are present
  
  Stopif_TRUE <- FALSE # code note - make correct length vector
  
  Stopif_TRUE[1] <- Check_Static_Column(KData, "DATE")
  Stopif_TRUE[2] <- Check_Static_Column(KData, "TIME")
  Stopif_TRUE[3] <- Check_Static_Column(KData, "MANUAL ID") # accepts Manual only 
  Stopif_TRUE[4] <- Check_Static_Column(KData, "PULSES")
  
  
  
  if(TRUE %in% Stopif_TRUE) {
    stop("Data not present")
  }
  
  #END CHECK DATA #############################################
  
  
  #Species is decribed with scientific name 
  #Use MANUAL ID for species list
  # Use funtion Convert_to_Sci to convert code to Scientific Name
  KData$ScientificName  <- Convert_to_Sci(KData$`MANUAL ID`)
  
  #Determine Date formate
  
  ymd <- is_date_ymd(KData$DATE)
  
  #Make Date Time from DATE and TIME
  if(ymd) {
    KData$DateTime <- lubridate::ymd_hms(stringr::str_c(as.character(KData$DATE), 
                                                        as.character(KData$TIME), sep= " "))
  } else {
    KData$DateTime <- lubridate::dmy_hms(stringr::str_c(as.character(KData$DATE), 
                                                        as.character(KData$TIME), sep= " "))
  }
  
  # Time zone is set to "Europe/London" - see https://www.iana.org/time-zones
  # Allows for daylight saving
  LocalTime <- "Europe/London"
  KData$DateTime <- lubridate::with_tz(KData$DateTime, LocalTime)
  
  #Call/pulses (not used with initial visualisation); but a useful metric 
  KData$CallsPulses <- KData$PULSES 
  
  #Select only the columns needed
  
  ####Asume file has Long/Lat
  #Use stopifnot....
  
  KData$Latitude <- Lat
  KData$Longitude <- Long
  
  KData$SiteName <- MetaData[1]
  KData$DetectorLocation <- MetaData[2]
  KData$Consultant <- MetaData[3]
  KData$DetectorMake <- MetaData[4]
  KData$DetectorModel <- MetaData[5]
  KData$AnalysisSoftware <- MetaData[6]
  KData$SoftwareVersion <- MetaData[7]
  KData$PassDefinition <- MetaData[8]
  KData$ClassificationMethod <- MetaData[9]
  KData$DataSensitivity <- MetaData[10]
  KData$Notes <- MetaData[11]
  
  #Select only the columns needed
  
  KData <- KData %>% 
    select(SiteName,
           DetectorLocation,
           Consultant,
           DateTime, 
           ScientificName, 
           CallsPulses, 
           Latitude, 
           Longitude,
           DetectorMake,
           DetectorModel,
           AnalysisSoftware,
           SoftwareVersion,
           PassDefinition,
           ClassificationMethod,
           DataSensitivity,
           Notes)
  
  return(KData)
  
}
