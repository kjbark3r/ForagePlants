##########################################
#### CHECKING OUT NSERP DIET RESULTS #####
#### IN ORDER TO DEFINE FORAGE PLANTS ####
#########  KJB  July 2016   ##############
##########################################

## PACKAGES

library(dplyr)
library(RODBC)

## WD

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\ForagePlants"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\ForagePlants"

	if (file.exists(wd_workcomp)) {
	  setwd(wd_workcomp)
	} else {
	  if(file.exists(wd_laptop)) {
		setwd(wd_laptop)
	  } else {
		  cat("Are you SURE you got that file path right?\n")
		  }
	  }

#Connect to Access phenology database (work computer or laptop)
if (file.exists(wd_workcomp)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology.accdb")
  } else {
    if(file.exists(wd_laptop)) {
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                               dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")
    } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

## READ IN DATA

combo <- read.csv("diet-combined.csv")
jun <- read.csv("diet-jun.csv")
julaug <- read.csv("diet-julaug.csv")
septoct <- read.csv("diet-septoct.csv")
#need 32-bit R for this part
spp <- sqlQuery(channel, paste("select PlantCode, NameScientific
                                 from NSERP_SP_list"))
  spp <- rename(spp, Species = NameScientific)
  spp$Species <- sub("\\.", "", spp$Species) #remove trailing periods
  spp$Species <- trimws(spp$Species) #remove leading/trailing spaces


## CAN SKIP RUNNING CODE BETWEEN THESE LINES
#########################################################
## COMPARE COMBINED SUMMARY TO BIMONTHLY SUMMARIES

all <- bind_rows(combo, jun) %>%
  bind_rows(julaug) %>%
  bind_rows(septoct)

allplants <- unique(all$Species)
comboplants <- as.character(unique(combo$Species))

# 62 forage plants included in top 95% cumavg diet 
  # from all bimonthly sampling periods put together
# 53 forage plants included in combined summary

(diff <- setdiff(allplants, comboplants))

# above determined plants "left out" from combined summary
# now looking at how important they were in each sampling pd

# "Potentilla sp  leaf" 
## this is "unique" just because of the "leaf" addition
  jun[jun$Species %in% "Potentilla sp  leaf",] 
  julaug[julaug$Species %in% "Potentilla sp  leaf",] #HERE
  septoct[septoct$Species %in% "Potentilla sp  leaf",] 

# "Flower" 
## can't use; not associated with a species
  jun[jun$Species %in% "Flower",] 
  julaug[julaug$Species %in% "Flower",] 
  septoct[septoct$Species %in% "Flower",] 

# "Seed Nut" 
## can't use; not associated with a species
  jun[jun$Species %in% "Seed Nut",] #nope
  julaug[julaug$Species %in% "Seed Nut",]
  septoct[septoct$Species %in% "Seed Nut",] #nope

# "Chamerion angustifolium"  
  jun[jun$Species %in% "Chamerion angustifolium",] 
  julaug[julaug$Species %in% "Chamerion angustifolium",] #HERE
  septoct[septoct$Species %in% "Chamerion angustifolium",] 

# "Amelanchier sp  stem" 
  jun[jun$Species %in% "Amelanchier sp  stem",] 
  julaug[julaug$Species %in% "Amelanchier sp  stem",] #HERE
  septoct[septoct$Species %in% "Amelanchier sp  stem",] 

# "Penstemon sp " - THIS ONE SEEMS IMPORTANT
# ADD TO FORAGE SPECIES LIST
  jun[jun$Species %in% "Penstemon sp ",] 
  julaug[julaug$Species %in% "Penstemon sp ",] 
  septoct[septoct$Species %in% "Penstemon sp ",] 

# "Moss"
## not included in biomass measurements
  jun[jun$Species %in% "Flower",] 
  julaug[julaug$Species %in% "Flower",] 
  septoct[septoct$Species %in% "Flower",] 

# "Rosa sp  leaf" 
  jun[jun$Species %in% "Rosa sp  leaf",] 
  julaug[julaug$Species %in% "Rosa sp  leaf",] 
  septoct[septoct$Species %in% "Rosa sp  leaf",] 

# "Castilleja sp "
  jun[jun$Species %in% "Castilleja sp ",] 
  julaug[julaug$Species %in% "Castilleja sp ",] 
  septoct[septoct$Species %in% "Castilleja sp ",] 

## SKIP RUNNING CODE BETWEEN THESE LINES
#############################################################
  
  
## CREATE LIST OF FORAGE SPECIES

forage <- as.data.frame(septoct[septoct$Species %in% "Penstemon sp ",]) %>%
  bind_rows(combo) %>%  #add penstemmon 
  filter(!grepl("Other|Composite", Species)) #remove non-species
forage$Species <- gsub('leaf| stem', '', forage$Species)#remove plant parts
forage$Species <- trimws(forage$Species) #remove leading/trailing spaces
forage <- left_join(forage, spp, by = "Species")  #add spp code

#still missing some species codes due to inability to ID past genus
#will add these manually for sake of time
write.csv(forage, file = "foragespecies.csv", row.names = FALSE)


##########
## FIGURING OUT HOW TO HANDLE GENUS-ONLY IDS

genusonly <- filter(forage, grepl(" sp", Species)) #all non-species
  # 28 forage plants (~57%) are only IDd to genus
  # see how many of the ones IDd to species have many other species in that genus
speciesonly <- filter(forage, !grepl(" sp", Species))
     