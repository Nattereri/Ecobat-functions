#<----------------------------------------------------------------------
#Function - For getting sunset sun rise and night length (used by Exit_Reentry_Times)
#<----------------------------------------------------------------------
NightData <- function(StartDate, DurationDays, lat, long) {
  
  # Function to return sunset sunrise and night length with long/lat and night
  #Aruments list (fist Night, duratiojn , lon, lat)
  
  #Works with night so and an extra day to get dawn
  
  #Get sunrise and sunset times for period of static placement
  
  if(DurationDays == 0){DurationDays <- 1}
  
  SunTimes <- sunrise.set(lat, long, StartDate, timezone = "Europe/London", num.days = DurationDays + 1)
  
  ## Table of Night and sun rise/set and times (based on EmergenceData data frame)
  
  # Take away last row
  SunSets <- SunTimes %>% 
    slice(1:(n() - 1)) %>% 
    pull(sunset) 
  
  # Take away first row  
  SunRises <- SunTimes %>% 
    slice(2:n()) %>% 
    pull(sunrise) 
  
  Nights <- seq.Date(from=StartDate, length.out = DurationDays, by='days')
  
  NightLength <- round_hms(as.hms(-(SunSets - SunRises)), 60)
  
  #NightLength <- 
  
  NightStats <- tibble(Nights, SunSets, SunRises, NightLength)
  
  return(NightStats)
  
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|



#<----------------------------------------------------------------------
#Function - to make emergance times from bat observations
#<----------------------------------------------------------------------
Exit_Reentry_Times <- function(EmergenceData) {
  
  #Need Observation data Night, Long, Lat, Species
  
  #Use NightData to calculate the suntimes merge with bat observations 
  
  #argument is just Emergence data (Night, Long, Lat, Species)
  
  DurationDays <- as.integer(max(EmergenceData$Night) - min(EmergenceData$Night)) + 1 
  StartDate <- min(EmergenceData$Night)
  
  lat <- median(EmergenceData$Latitude, na.rm = TRUE)
  long <- median(EmergenceData$Longitude, na.rm = TRUE)
  
  TheNightData <- NightData(StartDate, DurationDays, lat, long)
  
  #Calculate the total survey length
  Cal_night_length <- TheNightData %>% 
    slice(1:(n()- 1)) %>% 
    summarise(night_length = sum(as.numeric(NightLength) /(60 * 60))) %>% 
    pull(night_length)
  
  TheNightData$survey_length <- Cal_night_length
  
  
  names(TheNightData) <- c("Night", "SunSet", "SunRise", "NightLength", "SurveyLength")
  
  #merge suntimes and observations by night
  Merged_data <-  dplyr::left_join(EmergenceData, TheNightData, by = "Night")
  
  #Text of Observation Time to display on plotly
  Merged_data$ObsTimeTxt <- stringr::str_sub(as.character(Merged_data$DateTime), start = 11, end = 16)
  Merged_data$ObsTimeTxt < stringr::str_c(Merged_data$ObsTimeTxt, " hrs")
  
  #Formatted text of Night to display on plotly
  Merged_data$NightTxt <- Fm_date(Merged_data$Night, 1)                                       
  
  #Calculate Seconds, minutes and hours after sunset
  Merged_data$ObsPostSS <- as.numeric(Merged_data$DateTime) - as.numeric(Merged_data$SunSet) # Seconds
  Merged_data$ObsPostSS_hr <- as.integer(Merged_data$ObsPostSS/3600) # Hours
  Merged_data$ObsPostSS_min <- as.integer(Merged_data$ObsPostSS/60) # Minutes
  
  #Calculate Seconds, minutes and hours before sunrise
  Merged_data$ObsPreSR <- as.numeric(Merged_data$SunRise) - as.numeric(Merged_data$DateTime)# Seconds
  Merged_data$ObsPreSR_hr <- as.integer(Merged_data$ObsPreSR/3600) # Hours
  Merged_data$ObsPreSR_min <- as.integer(Merged_data$ObsPreSR/60) # Minutes  
  
  return(Merged_data)
  
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|

#<----------------------------------------------------------------------
#Function - that plots Bat Emergence Times and First Observations
#<----------------------------------------------------------------------

Exit_Reentry_Graph <- function(E_Graph_Data, Dusk, Sci_Name, Alpha) {
  
  ##Function that draws Bat Emergence Times and First Observations - using only
  ## the Emergence arguments (Night, Long, Lat, Species) with DateTime to allow anotated
  #html graphs 
  
  #Switches to select what type of graph
  
  # Dusk = TRUE ; plot dusk emergence -(to plot dawn set to FALSE
  
  if(missing(Dusk)) {
    Dusk <- TRUE
  }
  
  if(missing(Sci_Name)) {
    Sci_Name <- TRUE
  }
  
  if(missing(Alpha)) {
    Alpha <- 0.9
  }
  
  E_Graph_Data <- Exit_Reentry_Times(E_Graph_Data)
  
  #Retrieve reference emergence times
  EData <- readxl::read_excel("Data//BatEmergence.xlsx", sheet = 1, col_names= T)
  
  if (Dusk) { 
    
    #Observations 90 mins after dusk
    E_Graph_Data <- E_Graph_Data %>%
      dplyr::select(NightTxt, ObsTimeTxt, Scientific, ObsPostSS_min) %>%
      filter(ObsPostSS_min <= 90) 
    
    GTitle <- "Bat Emergence Times and First Observations"
    yLab <- "Time after sunset (mins)"
    Caption <- "First 90 minutes after sunset"
    
  } else {
    
    #Observations 90 mins before dusk
    E_Graph_Data <- E_Graph_Data %>%
      dplyr::select(NightTxt, ObsTimeTxt, Scientific, ObsPreSR_min) %>%
      filter(ObsPreSR_min <= 90)
    
    GTitle <- "Dawn Re-entry Times"
    yLab <- "Time before sunrise (mins)"
    Caption <- "All Observations 90 minutes before sunset"
    
  }
  
  #Join data
  E_Graph_Data <- dplyr::left_join(E_Graph_Data, EData, by = "Scientific")
  
  
  #EmergenceTable$`Species Observed` <- EmergenceTable$BatCommon
  
  NumEmerged <- nrow(E_Graph_Data)
  
  if (NumEmerged  < 1) {
    NoBats <- "* __Note:- no bats observed in first 90 minutes after sunset.__"
    
    return(NoBats)
  }
  
  colScale <- SpeciesColourScale(E_Graph_Data$Scientific, Fill = F)
  
  E_Graph_Data <- E_Graph_Data[order(E_Graph_Data$Scientific), ]
  
  if (Dusk) {
    
    p <- ggplot(E_Graph_Data, aes(x=Scientific, y=ObsPostSS_min, colour=Scientific)) + 
      geom_jitter(size=8, alpha=Alpha) +
      geom_linerange(aes(x=Scientific, ymin=Lower, ymax=Upper), size = 5, colour="floralwhite") +
      labs(list(title = GTitle, 
                y = yLab, 
                caption = Caption), 
           colour = "white") +
      scale_y_continuous(breaks=c(0, 15, 30, 45, 60, 75, 90)) +
      coord_flip()
    
    
  } else {
    
    p <- ggplot(E_Graph_Data, aes(x=Scientific, y=ObsPreSR_min, colour=Scientific)) +
      geom_jitter(size=8, alpha=Alpha) +
      geom_hline(yintercept=0, linetype="dashed", size=2, colour="darkgoldenrod3", alpha=0.9) +
      annotate("text", x = 1, y = 3, label = "Sunrise", angle = 90, colour = "darkgoldenrod3") +
      labs(list(title = GTitle, 
                y = yLab, 
                caption = Caption), 
           colour = "white") +
      #scale_y_continuous() +
      scale_y_reverse(breaks=c(0, 15, 30, 45, 60, 75, 90)) +
      coord_flip()
    
    
    
  }
  
  p <- p + theme_bw() +
    colScale +
    theme (legend.position = "none") +
    #theme (axis.title.y = element_blank())
    theme(plot.title = element_text(colour = "white", face = "bold", size = 16)) +
    theme(plot.caption = element_text(colour = "white", face = "bold")) +
    theme (axis.title.y = element_blank()) +
    theme (axis.title.x = element_text(colour = "white", size = 12)) +
    #theme (axis.text.y = element_blank()) +
    theme(axis.text.x = element_text(hjust = 1, colour = "white", face = "bold")) +
    theme(axis.text.y = element_text(size = 12, colour = "white", face = "bold.italic")) +
    theme(panel.background = element_rect(fill = "midnightblue")) +
    theme(panel.grid.major.x = element_line(colour = "grey70", linetype = "dotted"), 
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank()) +
    theme(plot.background = element_rect(fill = "grey70")) +
    theme (axis.ticks = element_blank())
  
  return(p)
  
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|