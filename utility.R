###########################################################################
#Formats a POSdate or Date for Reporting.
Fm_date <- function(Format.Date, style){
  
  if(style == 1) {
    
    stringr::str_c( lubridate::day(Format.Date) , 
                    lubridate::month(Format.Date, label=TRUE), 
                    lubridate::year(Format.Date), sep=" ")
    
  } else if(style == 2) {
    
    #Abbr month and date only
    stringr::str_c( lubridate::day(Format.Date), 
                    lubridate::month(Format.Date, label=TRUE, abbr=FALSE), 
                    lubridate::year(Format.Date), sep=" ")
    
  } else if(style == 3) {
    
    stringr::str_c( lubridate::day(Format.Date) , 
                    lubridate::month(Format.Date, label=TRUE), sep=" ")
    
  } else if(style == 4) {
    #Full date e.g. Thursday 3 August 2017
    stringr::str_c( lubridate::wday(Format.Date, label=TRUE, abbr=FALSE),
                    lubridate::day(Format.Date),
                    lubridate::month(Format.Date, label=TRUE, abbr=FALSE), 
                    lubridate::year(Format.Date), sep=" ")
    
  } else if(style == 5) {
    #Full date and Time e.g. Thu 3 Aug 2017 20:13:56
    stringr::str_c( lubridate::wday(Format.Date, label=TRUE, abbr=TRUE),
                    " ",
                    lubridate::day(Format.Date),
                    " ",
                    lubridate::month(Format.Date, label=TRUE, abbr=TRUE), 
                    " ",
                    lubridate::year(Format.Date), 
                    " ",
                    stringr::str_pad(lubridate::hour(Format.Date), 2, side = "left", pad = "0"),
                    ":",
                    stringr::str_pad(lubridate::minute(Format.Date), 2, side = "left", pad = "0"),
                    "hrs",
                    sep="")
    
  } else if(style == 6) {
    
    #Simple date 
    stringr::str_c( lubridate::day(Format.Date), 
                    lubridate::month(Format.Date), 
                    stringr::str_sub(lubridate::year(Format.Date), start=3, end=4), 
                    sep="-")
    
  } else if(style == 7) {
    
    # date (month and Year)
    stringr::str_c( lubridate::month(Format.Date, label=TRUE, abbr=FALSE), 
                    lubridate::year(Format.Date), 
                    sep=" ")
    
  }
}


#<----------------------------------------------------------------------


#Lookup function - make bat colours for plotting from Scientific Name 
#Based on http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=12
# Needs some revsion - not recomended to display more than than 12 colours 
Bat_Pallet <- function(BatList, Pallet){
  
  if(missing(Pallet)) {
    Pallet <- 2
  }
  
  if (Pallet == 1 ) {
    
    Bat_Colour <- ""
    Bat_Colour[BatList=="Barbastella barbastellus"] <- "#1f78b4" #*1 old:"#a6cee3"
    Bat_Colour[BatList=="Myotis alcathoe"] <- "#A52A2A"
    Bat_Colour[BatList=="Myotis bechsteinii"] <- "#7FFF00"
    Bat_Colour[BatList=="Myotis brandtii"] <- "#b2df8a" # "#8B0000"
    Bat_Colour[BatList=="Myotis mystacinus"] <- "#6a3d9a" # old "#8A2BE2"
    Bat_Colour[BatList=="Myotis nattereri"] <- "#ff7f00" # old "#8A2BE2"
    Bat_Colour[BatList=="Myotis daubentonii"] <- "#a6cee3" #*2 old: "#1f78b4"
    Bat_Colour[BatList=="Myotis spp."] <- "#BCEE68"
    Bat_Colour[BatList=="Plecotus auritus"] <- "#8B0000" #*6 old: "#e31a1c"
    Bat_Colour[BatList=="Plecotus spp."] <- "#8B0000" #*6 old: "#e31a1c"
    Bat_Colour[BatList=="Plecotus austriacus"] <- "#000000"
    Bat_Colour[BatList=="Pipistrellus pipistrellus"] <- "#ffff99" #*11
    Bat_Colour[BatList=="Pipistrellus nathusii"] <- "#8A2BE2" #*7 old "#fdbf6f"
    Bat_Colour[BatList=="Pipistrellus pygmaeus"] <- "#b15928" #*12
    Bat_Colour[BatList=="Pipistrellus spp."] <- "#fdbf6f" #old "#008B8B"
    Bat_Colour[BatList=="Rhinolophus ferrumequinum"] <- "#e31a1c" #*3 old: "#b2df8a"
    Bat_Colour[BatList=="Rhinolophus hipposideros"] <- "#33a02c" #*4
    Bat_Colour[BatList=="Nyctalus noctula"] <- "#cab2d6" #*9
    Bat_Colour[BatList=="Nyctalus leisleri"] <- "#fb9a99" #*5
    Bat_Colour[BatList=="Nyctalus spp."] <- "#EEE8CD" 
    Bat_Colour[BatList=="Eptesicus serotinus"] <- "#008B8B" #*10 old"#6a3d9a"
    Bat_Colour[BatList=="Pipistrelle Social"] <- "#000000" 
    
  } else if(Pallet == 2) {
    
    Bat_Colour <- ""
    Bat_Colour[BatList=="Alcathoe"] <- "#A52A2A"
    Bat_Colour[BatList=="Barbastelle"] <- "#1f78b4" #*1 old:"#a6cee3"
    Bat_Colour[BatList=="Bechstein's"] <- "#7FFF00"
    Bat_Colour[BatList=="Brandt's"] <- "#b2df8a" # "#8B0000"
    Bat_Colour[BatList=="Brown long-eared"] <- "#8B0000" #*6 old: "#e31a1c"
    Bat_Colour[BatList=="Long-eared"] <- "#8B0000" #*6 old: "#e31a1c"
    Bat_Colour[BatList=="Common pipistrelle"] <- "#ffff99" #*11
    Bat_Colour[BatList=="Daubenton's"] <- "#a6cee3" #*2 old: "#1f78b4"
    Bat_Colour[BatList=="Greater horseshoe"] <- "#e31a1c" #*3 old: "#b2df8a"
    Bat_Colour[BatList=="Grey long-eared"] <- "#000000"
    Bat_Colour[BatList=="Lesser horseshoe"] <- "#33a02c" #*4
    Bat_Colour[BatList=="Leisler's"] <- "#fb9a99" #*5
    Bat_Colour[BatList=="Myotis Sp."] <- "#BCEE68"
    Bat_Colour[BatList=="Nathusius' pipistrelle"] <- "#8A2BE2" #*7 old "#fdbf6f"
    Bat_Colour[BatList=="Natterer's"] <- "#ff7f00" #*8
    Bat_Colour[BatList=="Noctule"] <- "#cab2d6" #*9
    Bat_Colour[BatList=="Pipistrelle Sp."] <- "#fdbf6f" #old "#008B8B"
    Bat_Colour[BatList=="Serotine"] <- "#008B8B" #*10 old"#6a3d9a"
    Bat_Colour[BatList=="Soprano pipistrelle"] <- "#b15928" #*12
    Bat_Colour[BatList=="Whiskered"] <- "#6a3d9a" # old "#8A2BE2"
    Bat_Colour[BatList=="Nyctalus Sp."] <- "#EEE8CD"
    Bat_Colour[BatList=="Pipistrelle Social"] <- "#000000"
    
    
  }
  
  return(Bat_Colour)
}

##############################################################################
#################### Function assign Bat Colours for plotting
# It returns scale_fill_manual(name = ""Species Observed",values = myColors)
SpeciesColourScale <- function(BatSpeciesSci, Fill) {
  
  if(missing(Fill)) {
    Fill <- T
  }
  
  BatColour <- Bat_Pallet(BatSpeciesSci, Pallet = 1)
  #BatColour <- Bat_Pallet(BatSpeciesSci)
  BatColour <- stringr::str_c(BatColour, BatSpeciesSci)
  batcolours <- levels(as.factor(BatColour))
  myColors <- stringr::str_sub(batcolours, start=1, end=7)
  names(myColors) <- stringr::str_sub(batcolours, start=8, end=stringr::str_length(batcolours))
  
  if (Fill) {
    colScale <- scale_fill_manual(name = "Species" ,values = myColors) 
  } else {
    colScale <- scale_colour_manual(name = "Species" ,values = myColors) 
  }
  
  return(colScale)
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|



#<----------------------------------------------------------------------
#Function - Get plot Legend for plotting with gridExtra
#<----------------------------------------------------------------------

get_legend <- function(myggplot){
  
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  
  return(legend)
  
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|