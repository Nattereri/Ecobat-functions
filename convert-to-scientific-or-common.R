#<----------------------------------------------------------------------
#Function to convert names (all types) to Scientific nomenclature
#<----------------------------------------------------------------------


Convert_to_Sci <- function(BatName){
  
  #Keep Scientific name
  BatScientific <- ""
  BatScientific[BatName=="Myotis alcathoe"] <- "Myotis alcathoe"
  BatScientific[BatName=="Barbastella barbastellus"] <- "Barbastella barbastellus"
  BatScientific[BatName=="Myotis bechsteinii"] <- "Myotis bechsteinii"
  BatScientific[BatName=="Myotis brandtii"] <- "Myotis brandtii"
  BatScientific[BatName=="Myotis daubentonii"] <- "Myotis daubentonii"
  BatScientific[BatName=="Rhinolophus ferrumequinum"] <- "Rhinolophus ferrumequinum"
  BatScientific[BatName=="Rhinolophus hipposideros"] <- "Rhinolophus hipposideros"
  BatScientific[BatName=="Nyctalus leisleri"] <- "Nyctalus leisleri"
  BatScientific[BatName=="Plecotus auritus"] <- "Plecotus auritus"
  BatScientific[BatName=="Plecotus austriacus"] <- "Plecotus austriacus"
  BatScientific[BatName=="Pipistrellus nathusii"] <- "Pipistrellus nathusii"
  BatScientific[BatName=="Myotis nattereri"] <- "Myotis nattereri"
  BatScientific[BatName=="Nyctalus noctula"] <- "Nyctalus noctula"
  BatScientific[BatName=="Eptesicus serotinus"] <- "Eptesicus serotinus"
  BatScientific[BatName=="Pipistrellus pipistrellus"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="Pipistrellus pygmaeus"] <- "Pipistrellus pygmaeus"
  BatScientific[BatName=="Pipistrellus spp."] <- "Pipistrellus spp."
  BatScientific[BatName=="Myotis mystacinus"] <- "Myotis mystacinus"
  BatScientific[BatName=="Pipistrellus spp."] <- "Pipistrellus spp."
  BatScientific[BatName=="Pipistrellus sp."] <- "Pipistrellus spp."
  BatScientific[BatName=="Nyctalus spp."] <- "Nyctalus spp."
  BatScientific[BatName=="Nyctalus sp."] <- "Nyctalus spp."
  BatScientific[BatName=="Nyctalus sp"] <- "Nyctalus spp."
  BatScientific[BatName=="Myotis spp."] <- "Myotis spp."
  
  
  #Common Names
  BatScientific[BatName=="Alcathoe"] <- "Myotis alcathoe"
  BatScientific[BatName=="Barbastelle"] <- "Barbastella barbastellus"
  BatScientific[BatName=="Bechstein's"] <- "Myotis bechsteinii"
  BatScientific[BatName=="Brandt's"] <- "Myotis brandtii"
  BatScientific[BatName=="Daubenton's"] <- "Myotis daubentonii"
  BatScientific[BatName=="Greater horseshoe"] <- "Rhinolophus ferrumequinum"
  BatScientific[BatName=="Lesser horseshoe"] <- "Rhinolophus hipposideros"
  BatScientific[BatName=="Leisler's"] <- "Nyctalus leisleri"
  BatScientific[BatName=="Brown long-eared"] <- "Plecotus auritus"
  BatScientific[BatName=="Grey long-eared"] <- "Plecotus austriacus"
  BatScientific[BatName=="Plecotus sp."] <- "Plecotus spp."
  BatScientific[BatName=="Nathusius' pipistrelle"] <- "Pipistrellus nathusii"
  BatScientific[BatName=="Natterer's"] <- "Myotis nattereri"
  BatScientific[BatName=="Noctule"] <- "Nyctalus noctula"
  BatScientific[BatName=="Serotine"] <- "Eptesicus serotinus"
  BatScientific[BatName=="Common pipistrelle"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="Soprano pipistrelle"] <- "Pipistrellus pygmaeus"
  BatScientific[BatName=="Pipistrelle species"] <- "Pipistrellus spp."
  BatScientific[BatName=="Whiskered"] <- "Myotis mystacinus"
  BatScientific[BatName=="Pipistrelle Sp."] <- "Pipistrellus spp."
  BatScientific[BatName=="Pipistrelle Sp"] <- "Pipistrellus spp."
  BatScientific[BatName=="Pipistrelle sp."] <- "Pipistrellus spp."
  BatScientific[BatName=="Pipistrelle sp"] <- "Pipistrellus spp."
  BatScientific[BatName=="Nyctalus Sp."] <- "Nyctalus spp.."
  BatScientific[BatName=="Leisler's or Noctule"] <- "Nyctalus spp."
  BatScientific[BatName=="Myotis sp."] <- "Myotis spp." 
  BatScientific[BatName=="Myotis sp"] <- "Myotis spp." 
  BatScientific[BatName=="Myotis Sp."] <- "Myotis spp."
  BatScientific[BatName=="Myotis species"] <- "Myotis spp."
  BatScientific[BatName=="Whiskered or Brandt's"] <- "Myotis spp."
  
  
  #Wildlife Accoustics Codes
  BatScientific[BatName=="BABA"] <- "Barbastella barbastellus"
  BatScientific[BatName=="EPSE"] <- "Eptesicus serotinus"
  BatScientific[BatName=="EPTSER"] <- "Eptesicus serotinus"
  BatScientific[BatName=="MYsp"] <- "Myotis spp."
  BatScientific[BatName=="NYLE"] <- "Nyctalus leisleri"
  BatScientific[BatName=="NYCLEI"] <- "Nyctalus leisleri"
  BatScientific[BatName=="NYNO"] <- "Nyctalus noctula"
  BatScientific[BatName=="NYCNOC"] <- "Nyctalus noctula"
  BatScientific[BatName=="NyctSP"] <- "Nyctalus spp."
  BatScientific[BatName=="PI"] <- "Pipistrellus"
  BatScientific[BatName=="PINA"] <- "Pipistrellus nathusii"
  BatScientific[BatName=="PIPNAT"] <- "Pipistrellus nathusii"
  BatScientific[BatName=="PIPI"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="PIPPIP"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="PIPY"] <- "Pipistrellus pygmaeus"
  BatScientific[BatName=="PIPPYG"] <- "Pipistrellus pygmaeus"
  BatScientific[BatName=="PLAUR"] <- "Plecotus auritus"
  BatScientific[BatName=="PLEAUR"] <- "Plecotus auritus"
  BatScientific[BatName=="PLEC"] <- "Plecotus spp."
  BatScientific[BatName=="RHFE"] <- "Rhinolophus ferrumequinum"
  BatScientific[BatName=="RHHI"] <- "Rhinolophus hipposideros"
  BatScientific[BatName=="MYNA"] <- "Myotis nattereri"
  BatScientific[BatName=="MYBR"] <- "Myotis brandtii"
  BatScientific[BatName=="MYDAU"] <- "Myotis daubentonii"
  
  #Richard Green Codes
  BatScientific[BatName=="Bbar"] <- "Barbastella barbastellus"
  BatScientific[BatName=="Rfer"] <- "Rhinolophus ferrumequinum"
  BatScientific[BatName=="Rhip"] <- "Rhinolophus hipposideros"
  BatScientific[BatName=="Plec"] <- "Plecotus spp."
  BatScientific[BatName=="Pnat"] <- "Pipistrellus nathusii"
  BatScientific[BatName=="Nnoc"] <- "Nyctalus noctula"
  BatScientific[BatName=="Eser"] <- "Eptesicus serotinus"
  BatScientific[BatName=="Ppip"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="Ppyg"] <- "Pipistrellus pygmaeus"
  BatScientific[BatName=="Mysp"] <- "Myotis spp."
  
  
  #Misc
  BatScientific[BatName=="45 pip"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="45 Pip"] <- "Pipistrellus pipistrellus"
  BatScientific[BatName=="55 pip"] <- "Pipistrellus pygmaeus"
  BatScientific[BatName=="BLEB"] <- "Plecotus auritus"
  BatScientific[BatName=="Big bat sp."] <- NA
  BatScientific[BatName=="Noise"] <- NA
  BatScientific[BatName=="NOISE"] <- NA
  BatScientific[BatName=="NoID"] <- NA
  
  return(BatScientific)
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|


#<----------------------------------------------------------------------
#Lookup function - make Common Name from Scientific Name
#<----------------------------------------------------------------------
Bat_Sci_Com <- function(BatSciList){
  
  # UK species list
  # Barbastella barbastellus, Eptesicus serotinus, Myotis alcathoe, Myotis bechsteinii, Myotis brandtii, 
  # Myotis daubentonii, Myotis mystacinus, Myotis nattereri, Nyctalus leisleri, Nyctalus noctula, Pipistrellus nathusii, 
  # Pipistrellus pipistrellus, Pipistrellus pygmaeus, Plecotus auritus, Plecotus austriacus, Rhinolophus ferrumequinum, 
  # Rhinolophus hipposideros, Nyctalus spp., Myotis spp., Pipistrellus spp.
  
  
  BatCommon <- ""
  BatCommon[BatSciList=="Myotis alcathoe"] <- "Alcathoe"
  BatCommon[BatSciList=="Myotis spec."] <- "Myotis Sp."
  BatCommon[BatSciList=="Myotis Sp." ] <- "Myotis Sp."
  BatCommon[BatSciList=="Myotis spp." ] <- "Myotis Sp."
  BatCommon[BatSciList=="Barbastella barbastellus"] <- "Barbastelle"
  BatCommon[BatSciList=="Myotis bechsteinii"] <- "Bechstein's"
  BatCommon[BatSciList=="Myotis brandtii"] <- "Brandt's"
  BatCommon[BatSciList=="Myotis daubentonii"] <- "Daubenton's"
  BatCommon[BatSciList=="Rhinolophus ferrumequinum"] <- "Greater horseshoe"
  BatCommon[BatSciList=="Rhinolophus hipposideros"] <- "Lesser horseshoe"
  BatCommon[BatSciList=="Nyctalus leisleri"] <- "Leisler's"
  BatCommon[BatSciList=="Plecotus auritus"] <- "Brown long-eared"
  BatCommon[BatSciList=="Plecotus austriacus"] <- "Grey long-eared"
  BatCommon[BatSciList=="Plecotus"] <- "Long-eared"
  BatCommon[BatSciList=="Pipistrellus nathusii"] <- "Nathusius' pipistrelle"
  BatCommon[BatSciList=="Myotis nattereri"] <- "Natterer's"
  BatCommon[BatSciList=="Nyctalus noctula"] <- "Noctule"
  BatCommon[BatSciList=="Eptesicus serotinus"] <- "Serotine"
  BatCommon[BatSciList=="Pipistrellus pipistrellus"] <- "Common pipistrelle"
  BatCommon[BatSciList=="Pipistrellus pygmaeus"] <- "Soprano pipistrelle"
  BatCommon[BatSciList=="Myotis mystacinus"] <- "Whiskered"
  BatCommon[BatSciList=="Pipistrellus spec."] <- "Pipistrelle Sp."
  BatCommon[BatSciList=="Pipistrellus spp."] <- "Pipistrelle Sp."
  BatCommon[BatSciList=="Nyctalus spec."] <- "Nyctalus Sp."
  BatCommon[BatSciList=="Nyctalus spp."] <- "Nyctalus Sp."
  
  
  
  
  return(BatCommon)
}

#<-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|