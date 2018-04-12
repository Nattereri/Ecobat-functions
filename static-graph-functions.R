#<----------------------------------------------------------------------
# ActivePlot2017a - simple time line plot for gridExtra
# This version used the Scientific column for bat species
# Plot one nights activity with sunset and sunrise and date at dusk
#<----------------------------------------------------------------------
ActivePlot2017a <- function(Data, Day_i, SunSet, SunRise, Alpha, panel.fill) {
  
  Month <- lubridate::month(lubridate::as_date(Day_i))
  
  #x range on graph
  xDusk <- lubridate::as_date(Day_i) + lubridate::hours(19)
  xDawn <- lubridate::as_date(Day_i + lubridate::days(1)) + lubridate::hours(5)
  
  if (Month >= 10) {
    
    #x range on graph for winter?
    xDusk <- lubridate::as_date(Day_i) + lubridate::hours(17)
    xDawn <- lubridate::as_date(Day_i + lubridate::days(1)) + lubridate::hours(7) 
    
  }
  
  #xDusk <- as.Date(Day_i) + lubridate::hours(20)
  #xDawn <- as.Date(Day_i + lubridate::days(1)) + lubridate::hours(6)
  
  #y scale (i.e. no scale)
  Data$BatPass <- 1
  
  #Location for text
  yinter <- 1 * Data$BatPass
  
  #Get bat colours
  colScale <- SpeciesColourScale(Data$Scientific, Fill = F)
  #colScale <- BatColours3(Data)
  
  #Graph parameters
  lineCol <-  "black"
  lineCol1 <- "orangered"
  lineCol2 <- "deeppink"
  textCol <- "mintcream"
  
  if(is.null(panel.fill)){textCol <- "black"}
  
  Glabel <- Fm_date(SunSet, 3)
  
  #print(SunSet)
  #print(min(Data$DateTime))
  
  p <- ggplot(Data, aes(x=DateTime, y=BatPass, colour=Scientific)) + 
    geom_jitter(size=8, alpha=Alpha) +
    #geom_beeswarm(size=8, alpha=Alpha) +
    ylim(0.5, 1.5) + 
    xlim(xDusk, xDawn) +
    geom_vline(xintercept=as.numeric(SunSet), linetype="dotdash", 
               size=1, colour=lineCol2, alpha=0.9) + 
    geom_vline(xintercept=as.numeric(SunRise), linetype="dotdash", 
               size=1, colour=lineCol1, alpha=0.9) +
    annotate("text", x = xDusk , y = yinter + 0.15, 
             label = Glabel, color = textCol)
  
  
  
  p <- p + theme_bw() +
    colScale +
    labs(colour = "Species Recorded") +
    theme (legend.position = "none") + 
    theme (axis.title.y = element_blank()) +
    theme (axis.title.x = element_blank()) +
    theme (axis.text.y = element_blank()) +
    theme (axis.text.x = element_blank()) +
    theme (panel.grid = element_blank()) +
    theme (axis.ticks = element_blank(), 
           panel.background = element_rect(fill = panel.fill))
  
  return(p)
  
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|

#<----------------------------------------------------------------------
# Function that Makes the Static outGraphs 
# Includes getting the sunset and sunrise for each Night
#<----------------------------------------------------------------------
Static_outGraphs <- function(StaticData, Alpha, panel.fill) {
  
  #Use max day because survey finish at dawn the next day (i.e. survey started night/day before)
  Days <- seq(from=as.Date(min(StaticData$Night)), to=as.Date(max(StaticData$Night)),by='days')
  
  outGraph <- vector("list", length(Days))
  
  
  #Get sunrise and sunset times for period of static placement
  lat <- median(StaticData$Latitude)
  long <- median(StaticData$Longitude)
  
  SunTimes <- sunrise.set(lat, long, min(StaticData$Night), timezone = LocalTime, num.days = length(Days) + 1)
  
  for ( i in seq_along(Days) ) {
    #print(i)
    #print(Days[i])
    
    #cat("\r", i,  " of ", length(Days))
    
    outGraph[[i]] <-  ActivePlot2017a(StaticData, Days[i], SunTimes[i,2],  SunTimes[i + 1,1], Alpha, panel.fill) 
  }
  
  return(outGraph)
  
} 

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|

#<----------------------------------------------------------------------
# Function that Makes the legend for the Static outGraphs out put
# Includes getting the sunset and sunrise for each Night
#<----------------------------------------------------------------------

Static_legend <- function(StaticData, Alpha, panel.fill) {
  
  #Icrease alpha on legend so it can be seen
  Alpha <- Alpha + 0.3
  if(Alpha > 1) {Alpha <- 1}

  #Data for legend remove NAs
  legData <- StaticData %>% 
    filter(!is.na(Scientific))
  
  #y scale (i.e. no scale)
  legData$BatPass <- 1
  
  #panel.fill <- "midnightblue"
  
  colScale <- SpeciesColourScale(legData$Scientific, Fill = F)
  
  p <- ggplot(legData, aes(x=DateTime, y=BatPass, colour=Scientific)) + 
    geom_jitter(size = 8, alpha = Alpha) +
    ylim(0.5, 1.5) +
    colScale +
    theme_bw() + 
    theme(legend.key = element_rect(fill = panel.fill),
          legend.text = element_text(face = 'bold.italic'))
  
  legend <- get_legend(p)
  
  return(legend)
  
}    
#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|

#<----------------------------------------------------------------------
# Functions that automate the need for NightGrid see below
# Makes Code below for any number of ouGraphs upto 22?
#<----------------------------------------------------------------------
#
# grid.arrange(legend,
#                outGraph[[1]], 
#                outGraph[[2]],
#                outGraph[[3]],
#                outGraph[[4]],
#                outGraph[[5]],
#                outGraph[[6]],
#                outGraph[[7]],
#                outGraph[[8]],
#                outGraph[[9]],
#                outGraph[[10]],
#                outGraph[[11]],
#                ncol = 2, 
#                widths = c(2, 6),
#                layout_matrix = cbind(c(1), c(2,3,4,5,6,7,8,9,10,11,12)))




#Make outGraph[[1]] string
NrGraph_String <- function(NrDays) {
  
  #Make outGraph[[1]] string
  NrGraph <- ""
  
  for ( i in (1:(NrDays - 1)) ) {
    
    Graph <- stringr::str_c("outGraph[[" , toString(i), "]], ")
    NrGraph <- stringr::str_c(NrGraph, Graph )
    
  }
  
  NrGraph <- stringr::str_c(NrGraph, "outGraph[[" , toString(NrDays), "]], ")
  
  return(NrGraph)
  
}

#Make layout_matrix string
Matrix_String <- function(NrDays) {
  
  matrix_text <- ""
  
  for ( i in (1:(NrDays - 1)) ) {
    
    mx <- stringr::str_c(toString(i + 1), ", ")
    matrix_text <- stringr::str_c(matrix_text, mx)
    
  }
  
  matrix_text <- stringr::str_c("layout_matrix = cbind(c(1), c(", matrix_text, toString(NrDays + 1), "))")
  
  return(matrix_text)
  
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|


#<----------------------------------------------------------------------
# Function that Wraps all the functions that plot Static Nightly Bat Activity  
#<----------------------------------------------------------------------
Static_Nightly_Activity <- function(StaticData, Alpha, panel.fill) {
  
  if(missing(Alpha)) {
    Alpha <- 0.75 # Point alpha turn down for high bat numbers
  }
  
  if(missing(panel.fill)) {
    panel.fill <- "midnightblue" # background colour
  }
  # Function that Makes the Static outGraphs 
  # Includes getting the sunset and sunrise for each Night
  outGraph <- Static_outGraphs(StaticData, Alpha, panel.fill)
  
  # Function that Makes the legend for the Static outGraphs out put
  # Includes getting the sunset and sunrise for each Night
  legend <- Static_legend(StaticData, Alpha, panel.fill)
  
  #Number of Nights of Activity
  NrNights <- length(outGraph)
  
  #Make outGraph[[1]] string
  NrGraph <- NrGraph_String(NrNights)
  #Make layout_matrix string
  matrix_text<- Matrix_String(NrNights)
  
  layout <- stringr::str_c(" ncol = 2, widths = c(2, 5), ", matrix_text) 
  
  myoptions <- stringr::str_c(NrGraph, layout )
  
  
  eval(parse(text = paste("grid.arrange(legend,", myoptions,")")))
  
}
#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|
