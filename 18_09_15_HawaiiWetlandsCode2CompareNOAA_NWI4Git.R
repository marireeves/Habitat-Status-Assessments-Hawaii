#Appendix M. Code to compare NOAA and NWI Wetlands

#Mari K Reeves
#18_05_30_SummarizeWetlandHabitatsForHSAs

#This code will intersect the NWI and NOAA polygon layers with Land Ownership, Habitat Quality, Reserve Areas and Critical Habitats
#and generate sumamary tables and figures for the biologists doing the Habitat Status Assessment Reports. 

#The NOAA layer was a polygon file that I generated from the rasters for the wetlands project

#I'm not filtering the NWI layer at all for this assessment


rm(list = ls()) #remove all past worksheet variables

set.seed(333)

# Read in Base Packages ---------------------------------------------------
pckg <- c("dplyr", "tidyr","RColorBrewer", "ggplot2",
          "parallel","gdalUtils","rgdal","rgeos", 
          "spatial","sp", "raster", "maptools", "spatialEco", 
          "SpatialPack", "spatstat", "microbenchmark",  "devtools",
          "snowfall", "tictoc") 

# READING IN PACKAGES
for(i in 1:length(pckg)){
  if ((!pckg[i] %in% installed.packages())==T) {
    install.packages(pckg[i], repos="http://cran.us.r-project.org", 
                     dependencies = T)
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }else{
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }
}

#Define Directories
#BaseDir <- "J:/PIOGIS12/APPS/SH_CC/Mari/HSAs/"
BaseDir<-"C:/Users/marireeves/Documents/HSAs/"
WorkingDir<-paste0(BaseDir, "HabitatStatusAssessmentsR/Hawaii")
SHADir<-paste0(BaseDir, "SHADataFiles/")
ConservationDir<-paste0(WorkingDir,"/ShapeFiles4R" )
OutDir<-paste0(ConservationDir, "/CoastalWetlandsStreamsOut")
WetDir<-paste0(OutDir, "/StreamsWetlandsCoastal")
ReportOut<-paste0(BaseDir, "HSAReports/18_06_24_CompNOAANWIWetlands" )
setwd(ReportOut)

#Set CRS for session
BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
sysCRS<-"+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#colorblind friendly palatte:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Bring in files and transform them into the sysCRS - send them to a new folder - only need to run this once, so commented it out

#neededshps<-list.files(ConservationDir, pattern = ".shp$")

# #habitatsplit<-function(hab){
# for (myshp in neededshps){
#   #get rid of the shp extension
#   lyr <- sub(".shp$", "", myshp)#This gets rid of the .shp extension in the list of files
#   b<-readOGR(dsn = ConservationDir, layer = lyr)#read in the shpfile
#   print(proj4string(b))#check CRS
#   #transform it to the sysCRS
#   g<-spTransform(b, CRS(sysCRS))
#   print(proj4string(g))#recheck the spatial projections
#   #send it back out to a different file
#   outname<-paste0("18_05_24_sysCRS",lyr)
#   writeOGR(g, dsn = OutDir, layer = outname, driver="ESRI Shapefile", overwrite_layer = T)
#   }


#Bring these files back in for intersection operations. 
list.files(OutDir, full.names = T, pattern = ".shp")
own<-readOGR(dsn = OutDir, layer = "18_05_24_sysCRS18_05_03_DissolveOwnerIslandl")
owndata<-own@data
ownBuffer<-gBuffer(own, byid=TRUE, width=0)#fix most gisValid errors 


reserves<-readOGR(dsn = OutDir, layer = "18_05_24_sysCRSReserves")

crithab<-readOGR(dsn = OutDir, layer = "18_05_24_sysCRSCriticalHabitat")
crithabdata<-crithab@data
#clean up the reserve layer to delete reserves we don't think are important from a conservation perspective
types2keep<-c( "Marine Life Conservation District",  "National Historical Park", 
               "National Park", "National Wildlife Refuge", "Natural Area Reserve", 
               "Private Preserve", "State Wilderness Park","The Nature Conservancy Preserve",
               "Wildlife Sanctuary")
myreserves<-reserves[reserves$Type_Defin %in% types2keep,]
myreservedata<-myreserves@data


#write functions to intersect the streams, wetlands, and streams with reserves, critical habitat, and land ownership, calculate and 
#plot length in each status. I think I may just need to treat each of these guys separately and not do this in a loop. Because the 
#incoming datasets are so different. Bleh. 

wetplaces<-list.files(WetDir, pattern = ".shp")
# 
# 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Did this earlier...
# 
# #read in NOAA Polygons and a csv to say which to keep or drop
#
list.files(OutDir)

noaa<-readOGR(dsn = OutDir, layer = "18_06_12_BestNOAAEWetlandFileEver")
levels(noaa$wtlndty)
noaa$wtlndty<-as.character(noaa$wtlndty)
noaa$wtlndty<-gsub("Scrub/Shrub", "Scrub Shrub", noaa$wtlndty)
noaa$wtlndty<-as.factor(noaa$wtlndty)

noaadata<-noaa@data
names(noaadata)
noaasummary<-aggregate(noaadata$Area_m, by = list(noaadata$island), FUN = sum)
names(noaasummary)<-c("Island", "aream2")
noaasummary$acres<-round(noaasummary$aream2*0.0002471, digits = 0)
noaasummary$aream2<- NULL
names(noaasummary)<- c("Island", "Acres of Wetlands on Island")
write.csv(noaasummary, file = "noaawetlandacresisland.csv")
writeOGR(noaa, dsn = OutDir, layer = "18_08_01_BestNOAAWetlandFileEver", driver="ESRI Shapefile", overwrite_layer = T)

#bring in nwi and see if they map onto each other
nwi<-readOGR(dsn = WetDir, layer = "18_05_24_sysCRSHI_Wetlands"  )
nwidata<-nwi@data

nwilut<-data.frame(table(nwidata$WETLAND_TY))

levels(nwilut$Var1)

#drop the Rivers and the Estuarine and Marine Deepwater, retaining all other categories for this analysis. 
nwilut$KEEP_DROP<-ifelse(nwilut$Var1 == "Estuarine and Marine Deepwater" | nwilut$Var1 ==  "Riverine" , "DROP", "KEEP")

nwiKeep<-nwilut[nwilut$KEEP_DROP == "KEEP",]

#Filter to keep only the wetlands I want
nwiwet<-nwi[nwi@data$WETLAND_TY %in% nwiKeep$Var1,]
nwiwet$aream2<-gArea(nwiwet, byid = T)

#check if that worked
nwiwetdata<-nwiwet@data
#there are fewer of them, yes. 
plot(nwiwet, col = "red", border = "red")

nwiwet$Shape_Area<-round(nwiwet@data$Shape_Area, digits = 2)
nwiwet$aream2<-round(nwiwet@data$aream2, digits = 2)
writeOGR(nwiwet, dsn = OutDir, layer = "18_08_01_NWI4HSAsNoError", driver="ESRI Shapefile", overwrite_layer = T)

#The big marine areas are gone
plot(noaa, col = "blue", border = "blue", add = T)

names(nwiwetdata)



save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_23NWINOAAWetlandsComp.RData")


write.csv(nwiwetdata, file = "18_06_23_nwiwet.csv")
write.csv(noaadata, file = "18_06_23_noaawet.csv")

noaawet<-noaa
#Calculate Areas and make an area statewide variable to use
noaawet$Area_m<-gArea(noaawet, byid = T)
noaawetdata<-noaawet@data
noaawetlandstotalarea<-gArea(noaawet)#161,763,248 meters


nwiwet$Area_m<-gArea(nwiwet, byid = T)
nwiwetlandstotalarea<-gArea(nwiwet) #650,756,448
nwiwetdata<-nwiwet@data

#Add in Island to table  
#bring in a coastlines shape for an over statement...
list.files(ConservationDir)
coast<-readOGR(dsn = ConservationDir, layer =  "Coastline2500m")


#transform it into the sysCRS

print(proj4string(coast))#check CRS
#transform it to the sysCRS
coast<-spTransform(coast, CRS(sysCRS))
print(proj4string(coast))#recheck the spatial projections
#send it back out to a different file
writeOGR(coast, dsn = OutDir, layer = "coast2500sysCRS", driver="ESRI Shapefile", overwrite_layer = F)
#run intersection to assign island names to wetland tables. 

#Keep only islands of interest...The 2500m buffer makes it so smaller islands are included with Mains

#Get rid of Lehua and other islands we don't want to avoid double counting

levels(coast$Label)
islands2keep<-c(  "Big Island" ,"Kahoolawe", "Kauai","Lanai" ,"Maui","Molokai","Niihau","Oahu"     )
coast<-coast[coast$Label %in% islands2keep,]
coastdata<-coast@data
#change name Big Island to Hawaii
coast$island<-sub("Big Island", "Hawaii", coast$Label)

nwiisland<-raster::intersect(nwiwet, coast )
plot(nwiisland)
nwiislanddata<-nwiisland@data

noaaisland<-raster::intersect(noaawet, coast)
noaaislanddata<-noaaisland@data
#calculate Island Areas within main tables
nwiwetlandareas<-aggregate(nwiisland$Area_m, by = list(nwiisland$island), FUN = sum)
names(nwiisland@data)
names(nwiwetlandareas)<-c("island", "wetlandareaonisland")
nwiwetter<-merge(nwiisland, nwiwetlandareas)

#Fix names of scrub shrub to join with forested and get rid of dash
nwiwetter$WetlandType<-sub("Freshwater Forested/Shrub Wetland", "Freshwater Forested and Scrub Shrub Wetland", nwiwetter$WETLAND_TY)

nwiwetter$WetlandType<-sub("Lake", "Open Water", nwiwetter$WetlandType)
nwiwetter$WetlandType<-sub("Freshwater Pond", "Open Water", nwiwetter$WetlandType)


levels(as.factor(nwiwetter$WetlandType))

#create a column for nwi to use later
nwiwetter$dataset<-"nwi"

noaawetlandareas<-aggregate(noaaisland$Area_m, by = list(noaaisland$Island), FUN = sum)

names(noaaisland@data)
names(noaawetlandareas)<-c("Island", "wetlandareaonisland")
noaawetter<-merge(noaaisland, noaawetlandareas, by = "Island")

#create a dataset column for noaa to use later
noaawetter$dataset<-"noaa"
#add gridcode descriptions and call it wetland type to match NWI and edit categories in 
#so that these match across datasets
levels(nwiwetter$WETLAND_TY)
# Keep Open Water
names(noaawetter)

checkme<-noaawetter@data
levels(noaawetter$wtlndty)
noaawetter$WetlandType<-sub("Water" , "Open Water", noaawetter$wtlndty)
# Keep Unconsolidated shore
noaawetter$WetlandType<-sub("19", "Unconsolidated Shore", noaawetter$WetlandType)
# Lump all estuarine wetlands and estuarine aquatic beds into Estuarine and Marine 
#Wetlands category
noaawetter$WetlandType<-sub("23", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("18", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("17", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("16", "Estuarine and Marine Wetland" , noaawetter$WetlandType)


#Lump all the following into freshwater Forested and Shrub Scrub Wetlands, 
#gridcodes 13, 14, palustrine forested and scrub shrub

noaawetter$WetlandType<-sub("13", "Freshwater Forested and Scrub Shrub Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("14", "Freshwater Forested and Scrub Shrub Wetland" , noaawetter$WetlandType)

# Lump palustrine emergent wetlands and palustrine aquatic beds to Freshwater 
#Emergent Wetland category
noaawetter$WetlandType<-sub("15",  "Freshwater Emergent Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("22",  "Freshwater Emergent Wetland" , noaawetter$WetlandType)


levels(as.factor(noaawetter$WetlandType))

#aggregate these guys by variables we're interested in summarizing:
#Wetland Type, island, island area, WetlandType

agnwi<-aggregate(nwiwetter$Area_m, by = list(nwiwetter$island, nwiwetter$WetlandType, nwiwetter$dataset, nwiwetter$wetlandareaonisland), FUN = sum)  
names(agnwi)<-c("island", "wetlandtype", "dataset", "wetlandareaonisland", "Area_m")


agno<-aggregate(noaawetter$Area_m, by = list(noaawetter$island.2, noaawetter$WetlandType, noaawetter$dataset, noaawetter$wetlandareaonisland), FUN = sum) 
names(agno)<-c("island", "wetlandtype", "dataset", "wetlandareaonisland", "Area_m")
#bind the aggregated files together so I only need to work with one data file for plots and tables

wetlands<-rbind(agnwi, agno)

#finally. One dataset that I can facet  

names(wetlands)
wetlands$acres<-round(wetlands$Area_m*0.0002471, digits = 0)

#convert the wetland area on island column to acres and round it too
wetlands$WetlandAcresOnIsle<-round(wetlands$wetlandareaonisland*0.0002471, digits = 0)
#drop the m2 area columns
wetlands<-wetlands[,c("island","wetlandtype","dataset", "acres","WetlandAcresOnIsle")]

wetlands$propwetlandsisle<-round((wetlands$acres/wetlands$WetlandAcresOnIsle)*100, digits = 0)
wetlands$dataset<-sub("noaa", "NOAA - CCAP", wetlands$dataset)
wetlands$dataset<-sub("nwi", "USFWS - NWI", wetlands$dataset)

#create a few comparison plots for the different datasets
#acres of wetlands on each island
str(wetlands)
wetlands$wetlandtype<-as.factor(wetlands$wetlandtype)
wetlands$island<-as.factor(wetlands$island)
wetlands$dataset<-as.factor(wetlands$dataset)

a<-ggplot(wetlands, aes(wetlands$dataset, wetlands$acres, fill = wetlands$wetlandtype))
datacompare<- a+geom_bar(stat="identity", position = "stack") + 
  labs(fill = "Wetland Type", title = "Wetland Areas by Type - Comparing Datasets", x = "Wetlands Dataset", y = "Area (Acres)")+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CompareWetlandAreaTypeData.jpg", plot=datacompare, device = "jpg" )


b<-ggplot(wetlands, aes(wetlands$island, wetlands$acres, fill = wetlands$dataset))
wettypecompare<- b+geom_bar(stat="identity", position = "dodge") + 
  labs(fill = "Wetlands Dataset", title = "Wetland Areas by Dataset - Comparing Islands", x = "Island", y = "Area (Acres)")+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CompareWetlandDatasetsbyIsland.jpg", plot=wettypecompare, device = "jpg" )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
#summarize land ownership for wetlands habitats
#Mari K Reeves
#18_05_30_SummarizeWetlandHabitatsForHSAs

#This code will intersect the NWI and NOAA polygon layers with Land Ownership, Habitat Quality, Reserve Areas and Critical Habitats
#and generate sumamary tables and figures for the biologists doing the Habitat Status Assessment Reports. 

#The NOAA layer was a polygon file that I generated from the rasters for the wetlands project

#I'm not filtering the NWI layer at all for this assessment


rm(list = ls()) #remove all past worksheet variables

set.seed(333)

# Read in Base Packages ---------------------------------------------------
pckg <- c("dplyr", "tidyr","RColorBrewer", "ggplot2",
          "parallel","gdalUtils","rgdal","rgeos", 
          "spatial","sp", "raster", "maptools", "spatialEco", 
          "SpatialPack", "spatstat", "microbenchmark",  "devtools",
          "snowfall", "tictoc") 

# READING IN PACKAGES
for(i in 1:length(pckg)){
  if ((!pckg[i] %in% installed.packages())==T) {
    install.packages(pckg[i], repos="http://cran.us.r-project.org", 
                     dependencies = T)
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }else{
    print(pckg[i])
    do.call("library", list(pckg[i]))
  }
}

#Define Directories
#BaseDir <- "J:/PIOGIS12/APPS/SH_CC/Mari/HSAs/"
BaseDir<-"C:/Users/marireeves/Documents/HSAs/"
WorkingDir<-paste0(BaseDir, "HabitatStatusAssessmentsR/Hawaii")
SHADir<-paste0(BaseDir, "SHADataFiles/")
ConservationDir<-paste0(WorkingDir,"/ShapeFiles4R" )
OutDir<-paste0(ConservationDir, "/CoastalWetlandsStreamsOut")
WetDir<-paste0(OutDir, "/StreamsWetlandsCoastal")
setwd(OutDir)

#Set CRS for session
BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
sysCRS<-"+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#colorblind friendly palatte:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Bring in files and transform them into the sysCRS - send them to a new folder - only need to run this once, so commented it out

#neededshps<-list.files(ConservationDir, pattern = ".shp$")

# #habitatsplit<-function(hab){
# for (myshp in neededshps){
#   #get rid of the shp extension
#   lyr <- sub(".shp$", "", myshp)#This gets rid of the .shp extension in the list of files
#   b<-readOGR(dsn = ConservationDir, layer = lyr)#read in the shpfile
#   print(proj4string(b))#check CRS
#   #transform it to the sysCRS
#   g<-spTransform(b, CRS(sysCRS))
#   print(proj4string(g))#recheck the spatial projections
#   #send it back out to a different file
#   outname<-paste0("18_05_24_sysCRS",lyr)
#   writeOGR(g, dsn = OutDir, layer = outname, driver="ESRI Shapefile", overwrite_layer = T)
#   }


#Bring these files back in for intersection operations. 
list.files(OutDir, full.names = T, pattern = ".shp")
own<-readOGR(dsn = OutDir, layer = "18_05_24_sysCRS18_05_03_DissolveOwnerIslandl")
owndata<-own@data
ownBuffer<-gBuffer(own, byid=TRUE, width=0)#fix most gisValid errors 


reserves<-readOGR(dsn = OutDir, layer = "18_05_24_sysCRSReserves")

crithab<-readOGR(dsn = OutDir, layer = "18_05_24_sysCRSCriticalHabitat")
crithabdata<-crithab@data
#clean up the reserve layer to delete reserves we don't think are important from a conservation perspective
types2keep<-c( "Marine Life Conservation District",  "National Historical Park", 
               "National Park", "National Wildlife Refuge", "Natural Area Reserve", 
               "Private Preserve", "State Wilderness Park","The Nature Conservancy Preserve",
               "Wildlife Sanctuary")
myreserves<-reserves[reserves$Type_Defin %in% types2keep,]
myreservedata<-myreserves@data

crithab<-readOGR(dsn = OutDir, layer = "CriticalHabitatsysCRSOnePoly")
crithabdata<-crithab@data

#write functions to intersect the streams, wetlands, and streams with reserves, critical habitat, and land ownership, calculate and 
#plot length in each status. I think I may just need to treat each of these guys separately and not do this in a loop. Because the 
#incoming datasets are so different. Bleh. 

wetplaces<-list.files(WetDir, pattern = ".shp")
# 
# 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Did this earlier...
# 
# #read in NOAA Polygons and a csv to say which to keep or drop
#
list.files(WetDir)

noaa<-readOGR(dsn = WetDir, layer = "18_06_12_BestNOAAEWetlandFileEver")


noaaDesc<-read.csv(paste0(WetDir, "/18_05_28_HSA_NOAA2KEEPDROP.csv"))
noaaDrop<-noaaDesc[noaaDesc$KEEP_DROP == "DROP",]
noaaKeep<-noaaDesc[noaaDesc$KEEP_DROP == "KEEP",]

#Filter to keep only the wetlands
noaawet<-noaa[noaa@data$gridcode %in% noaaKeep$NOAA_CAT,]
#
#
# #reproject the wetland file
noaawet<-spTransform(noaawet, CRS(sysCRS))

#save this to file so we can restart R and not bring in the whole NOAA poly file.

writeOGR(noaawet, dsn = WetDir, layer = "18_05_28_NOAAWetlands", driver="ESRI Shapefile", overwrite_layer = T)



list.files(WetDir)
noaawet<-readOGR(dsn = WetDir, layer = "18_05_28_NOAAWetlands")

# #check this worked
plot(noaa)

#bring in nwi and see if they map onto each other
nwi<-readOGR(dsn = WetDir, layer = "18_05_24_sysCRSHI_Wetlands"  )
plot(nwi, add = T, col = "red")
nwidata<-nwi@data

nwilut<-data.frame(table(nwidata$WETLAND_TY))
write.csv(nwilut, file = "nwi2keep.csv")


#drop the Rivers and the Estuarine and Marine Deepwater, retaining all other categories for this analysis. 
#oops, I did this in Excel by habit. could have should have done it here...oh well. Missed opportunity
nwikeepdrop<-read.csv(paste0(OutDir, "/nwi2keep.csv"))

nwiKeep<-nwikeepdrop[nwikeepdrop$KEEP_DROP == "KEEP",]

#Filter to keep only the wetlands I want
nwiwet<-nwi[nwi@data$WETLAND_TY %in% nwiKeep$WETLAND_TY,]


#check if that worked
nwiwetdata<-nwiwet@data
#there are fewer of them, yes. 
plot(nwiwet, col = "red", add = T)
#The big marine areas are gone
plot(noaawet, col = "blue")
noaawetdata<-noaawet@data

load("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_05_31_PostNOAAWetlandsWorkspace.RData")
noaawetdata<-noaawet@data



write.csv(nwiwetdata, file = "18_05_30_nwiwet.csv")
write.csv(noaawetdata, file = "18_05_30_noaawet.csv")

#Calculate Areas and make an area statewide variable to use
noaawet$Area_m<-gArea(noaawet, byid = T)
noaawetdata<-noaawet@data
noaawetlandstotalarea<-gArea(noaawet)#931,240,823 meters


nwiwet$Area_m<-gArea(nwiwet, byid = T)
nwiwetlandstotalarea<-gArea(nwiwet) #65,075,6448
nwiwetdata<-nwiwet@data

#Add in Island to table  
#bring in a coastlines shape for an over statement...
list.files(ConservationDir)
coast<-readOGR(dsn = ConservationDir, layer =  "Coastline2500m")


#transform it into the sysCRS

print(proj4string(coast))#check CRS
#transform it to the sysCRS
coast<-spTransform(coast, CRS(sysCRS))
print(proj4string(coast))#recheck the spatial projections
#send it back out to a different file
writeOGR(coast, dsn = OutDir, layer = "coast2500sysCRS", driver="ESRI Shapefile", overwrite_layer = T)
#run intersection to assign island names to wetland tables. 

#Keep only islands of interest...The 2500m buffer makes it so smaller islands are included with Mains

#Get rid of Lehua and other islands we don't want to avoid double counting

levels(coast$Label)
islands2keep<-c(  "Big Island" ,"Kahoolawe", "Kauai","Lanai" ,"Maui","Molokai","Niihau","Oahu"     )
coast<-coast[coast$Label %in% islands2keep,]
coastdata<-coast@data
#change name Big Island to Hawaii
coast$island<-sub("Big Island", "Hawaii", coast$Label)

nwiisland<-raster::intersect(nwiwet, coast )
plot(nwiisland)
nwiislanddata<-nwiisland@data

noaaisland<-raster::intersect(noaawet, coast)
noaaislanddata<-noaaisland@data
#calculate Island Areas within main tables
nwiwetlandareas<-aggregate(nwiisland$Area_m, by = list(nwiisland$island), FUN = sum)
names(nwiisland@data)
names(nwiwetlandareas)<-c("island", "wetlandareaonisland")
nwiwetter<-merge(nwiisland, nwiwetlandareas)

#Fix names of scrub shrub to join with forested and get rid of dash
nwiwetter$WetlandType<-sub("Freshwater Forested/Shrub Wetland", "Freshwater Forested and Scrub Shrub Wetland", nwiwetter$WETLAND_TY)

nwiwetter$WetlandType<-sub("Lake", "Open Water", nwiwetter$WetlandType)
nwiwetter$WetlandType<-sub("Freshwater Pond", "Open Water", nwiwetter$WetlandType)


levels(as.factor(nwiwetter$WetlandType))

#create a column for nwi to use later
nwiwetter$dataset<-"nwi"

noaawetlandareas<-aggregate(noaaisland$Area_m, by = list(noaaisland$island), FUN = sum)

names(noaaisland@data)
names(noaawetlandareas)<-c("island", "wetlandareaonisland")
noaawetter<-merge(noaaisland, noaawetlandareas)

#create a dataset column for noaa to use later
noaawetter$dataset<-"noaa"
#add gridcode descriptions and call it wetland type to match NWI and edit categories in 
#so that these match across datasets
levels(nwiwetter$WETLAND_TY)
# Keep Open Water
noaawetter$WetlandType<-sub("21", "Open Water", noaawetter$gridcode)
# Keep Unconsolidated shore
noaawetter$WetlandType<-sub("19", "Unconsolidated Shore", noaawetter$WetlandType)
# Lump all estuarine wetlands and estuarine aquatic beds into Estuarine and Marine 
#Wetlands category
noaawetter$WetlandType<-sub("23", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("18", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("17", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("16", "Estuarine and Marine Wetland" , noaawetter$WetlandType)


#Lump all the following into freshwater Forested and Shrub Scrub Wetlands, 
#gridcodes 13, 14, palustrine forested and scrub shrub

noaawetter$WetlandType<-sub("13", "Freshwater Forested and Scrub Shrub Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("14", "Freshwater Forested and Scrub Shrub Wetland" , noaawetter$WetlandType)

# Lump palustrine emergent wetlands and palustrine aquatic beds to Freshwater 
#Emergent Wetland category
noaawetter$WetlandType<-sub("15",  "Freshwater Emergent Wetland" , noaawetter$WetlandType)
noaawetter$WetlandType<-sub("22",  "Freshwater Emergent Wetland" , noaawetter$WetlandType)


levels(as.factor(noaawetter$WetlandType))

#aggregate these guys by variables we're interested in summarizing:
#Wetland Type, island, island area, WetlandType

agnwi<-aggregate(nwiwetter$Area_m, by = list(nwiwetter$island, nwiwetter$WetlandType, nwiwetter$dataset, nwiwetter$wetlandareaonisland), FUN = sum)  
names(agnwi)<-c("island", "wetlandtype", "dataset", "wetlandareaonisland", "Area_m")
agno<-aggregate(noaawetter$Area_m, by = list(noaawetter$island, noaawetter$WetlandType, noaawetter$dataset, noaawetter$wetlandareaonisland), FUN = sum) 
names(agno)<-c("island", "wetlandtype", "dataset", "wetlandareaonisland", "Area_m")
#bind the aggregated files together so I only need to work with one data file for plots and tables

wetlands<-rbind(agnwi, agno)

#finally. One dataset that I can facet by dataset 

names(wetlands)
wetlands$acres<-round(wetlands$Area_m*0.0002471, digits = 0)

#convert the wetland area on island column to acres and round it too
wetlands$WetlandAcresOnIsle<-round(wetlands$wetlandareaonisland*0.0002471, digits = 0)
#drop the m2 area columns
wetlands<-wetlands[,c("island","wetlandtype","dataset", "acres","WetlandAcresOnIsle")]

wetlands$propwetlandsisle<-round((wetlands$acres/wetlands$WetlandAcresOnIsle)*100, digits = 0)
wetlands$dataset<-sub("noaa", "NOAA - CCAP", wetlands$dataset)
wetlands$dataset<-sub("nwi", "USFWS - NWI", wetlands$dataset)

#create a few comparison plots for the different datasets
#acres of wetlands on each island
str(wetlands)
wetlands$wetlandtype<-as.factor(wetlands$wetlandtype)
wetlands$island<-as.factor(wetlands$island)
wetlands$dataset<-as.factor(wetlands$dataset)

a<-ggplot(wetlands, aes(wetlands$dataset, wetlands$acres, fill = wetlands$wetlandtype))
datacompare<- a+geom_bar(stat="identity", position = "stack") + 
  labs(fill = "Wetland Type", title = "Wetland Areas by Type - Comparing Datasets", x = "Wetlands Dataset", y = "Area (Acres)")+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CompareWetlandAreaTypeData.tif", plot=datacompare, device = "tiff" )


b<-ggplot(wetlands, aes(wetlands$island, wetlands$acres, fill = wetlands$dataset))
wettypecompare<- b+geom_bar(stat="identity", position = "dodge") + 
  labs(fill = "Wetlands Dataset", title = "Wetland Areas by Dataset - Comparing Islands", x = "Island", y = "Area (Acres)")+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CompareWetlandDatasetsbyIsland.tif", plot=wettypecompare, device = "tiff" )


load("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_05_31_preintersectworkspacewetlands.RData")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Run the Raster to Poly Extractions. these take 26 hours to run

habqual<-raster("C:/Users/marireeves/Documents/HSAs/SHADataFiles/CarbonAssessment2017VegLayers/CAH_HabStatus/CAH_HabStatus.tif")
proj4string(habqual)
proj4string(nwiwetter)
plot(habqual)
tic()
intersectqualni<-raster::extract(habqual, nwiwetter)

toc()


#make a copy of extracted data to work with:
nwiqualintersect<-intersectqualni

# # Get class counts for each polygon
nwi.counts <- lapply(nwiqualintersect,table)
# 
# # Calculate class percentages for each polygon
nwi.pct <- lapply(nwi.counts, FUN=function(x){ x / sum(x) } ) 
# 
# # Create a data.frame where missing classes are NA

class.df<-plyr::ldply(nwi.pct, rbind)

# # Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0   
names(class.df) <- paste("class", names(class.df),sep="")

# # Add back to polygon data
nwiwetterdata<-nwiwetter@data
nwiwetterdata <- cbind(nwiwetterdata, class.df)
head(nwiwetterdata)
write.csv(nwiwetterdata, file = "nwihabqualextract.csv")

tic()
intersectqualno<-raster::extract(habqual, noaawetter )

toc()

noaaqualintersect<-intersectqualno


# # Get class counts for each polygon
noaa.counts <- lapply(noaaqualintersect,table)
# 
# # Calculate class percentages for each polygon
noaa.pct <- lapply(noaa.pct, FUN=function(x){ x / sum(x) } ) 
# 
# # Create a data.frame where missing classes are NA

class.df2<-plyr::ldply(noaa.pct, rbind)

# # Replace NA's with 0 and add names
class.df2[is.na(class.df2)] <- 0   
names(class.df2) <- paste("class", names(class.df2),sep="")

# # Add back to polygon data
noaawetterdata<-noaawetter@data
noaawetterdata <- cbind(noaawetterdata, class.df2)
head(noaawetterdata)

write.csv(noaawetterdata, file = "noaahabqualextract.csv")

save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/18_06_04_PostExtractWorkspace.RData")

#make a list of these data tables to process
priceextract<-c("nwiwetterdata", "noaawetterdata" )
#define columns to keep in each layer
names(noaawetterdata)
moneyshot<-c("island", "wetlandareaonisland","WetlandType","dataset", "Area_m", "class1","class2","class3","class4")


for (spendy in priceextract){
  w<-get(spendy)[,moneyshot] #get the file from the global env and keep only the columns in moneyshot
  #  drop unwanted variables
  
  #convert the wetland area on island column to acres and round it too
  w$WetlandAcresOnIsle<-round(w$wetlandareaonisland*0.0002471, digits = 0)
  w$acres<-round(w$Area_m*0.0002471, digits = 2)
  w$dataset<-sub("noaa", "NOAA - CCAP", w$dataset)
  w$dataset<-sub("nwi", "USFWS - NWI", w$dataset)
  #calculate area proportion of wetlands on island
  w$propwetlandsisle<-round((w$acres/w$WetlandAcresOnIsle)*100, digits = 2)
  #rename the "class" columns and calculate acreages in each category per polygon
  # Class descriptions from the metadata: If Habqual was already distrubed (category = 1), 
  # then it was NEVER overwritten as bare earth; instead it remained classified as disturbed. 
  # Lastly, the TIGER roads layer was buffered and converted into a raster of category 1 (distrubed). 
  # The roads raster was then mosaic'ed on top of Habqual to expand the distrubed class to include roads
  # adjacent disturbed areas.This layer has four mapped values: 1 = heavily disturbed areas including agriculture and urban developments; 
  # 2 = mixed native-alien dominated plant communities; 3 = native dominated vegetation; 
  # and 4 = bare lands or &lt;5% plant cover.ReferencesARIS B.V. 2014, GRID Editor for ArcMap. ARIS B.V., Netherlands.
  
  w$AcresDisturbed<-round(w$class1*w$acres, digits = 5)
  w$AcresBarren<-round(w$class4*w$acres, digits = 5)
  w$AcresNative<-round(w$class3*w$acres, digits = 5)
  w$AcresNonNative<-round(w$class2*w$acres, digits = 5)
  
  
  
  #drop the m2 area columns and the "class" columns
  
  w<-w[,c("island","WetlandType","dataset", "acres","WetlandAcresOnIsle", "propwetlandsisle", "AcresDisturbed", "AcresBarren", "AcresNative", "AcresNonNative")]
  #give better names
  
  names(w)<-c("Island","Wetland Type","Dataset", "Area (Acres)","Total Wetland Area on Island (Acres)", "Area (% of Wetland Area on Island", "Area Disturbed (Acres)", "Area Barren (Acres)", "Area Native Dominated (Acres)", "Area Non-Native Dominated (Acres)")
  
  #save the raw data
  #write file to drive
  outname2<-sub("wetterdata", "habqualraw.csv", spendy)
  write.csv(w, file = outname2)
  
  u<-aggregate(list(w$`Area Barren (Acres)`, w$`Area Disturbed (Acres)`, w$`Area Native Dominated (Acres)`, w$`Area Non-Native Dominated (Acres)`), by = list(w$Island, w$`Wetland Type`,w$Dataset, w$`Total Wetland Area on Island (Acres)` ), FUN = sum)
  names(u)<-c("Island","Wetland Type","Dataset","Total Wetland Area on Island (Acres)", "Area Barren (Acres)", "Area Disturbed (Acres)","Area Native Dominated (Acres)", "Area Non-Native Dominated (Acres)")
  
  u$`Area Barren (Acres)`<-round(u$`Area Barren (Acres)`, digits = 1)
  u$`Area Native Dominated (Acres)`<-round(u$`Area Native Dominated (Acres)`, digits = 1)
  u$`Area Non-Native Dominated (Acres)`<-round(u$`Area Non-Native Dominated (Acres)`, digits = 1)
  u$`Area Disturbed (Acres)`<-round(u$`Area Disturbed (Acres)`, digits = 1)
  
  u$propbarren<-round((u$`Area Barren (Acres)` /u$`Total Wetland Area on Island (Acres)`  )*100, digits = 1)
  u$propnative<-round((u$`Area Native Dominated (Acres)`/u$`Total Wetland Area on Island (Acres)`)*100, digits = 1)
  u$propnonnative<-round((u$`Area Non-Native Dominated (Acres)`/u$`Total Wetland Area on Island (Acres)`)*100, digits = 1)
  u$propdisturbed<-round((u$`Area Disturbed (Acres)`/u$`Total Wetland Area on Island (Acres)`)*100, digits = 1)
  
  
  #drop the wetland area on island column
  u<-u[, c( "Island","Wetland Type","Dataset","Area Barren (Acres)","Area Disturbed (Acres)"  ,            
            "Area Native Dominated (Acres)","Area Non-Native Dominated (Acres)","propbarren"   ,                       
            "propnative","propnonnative","propdisturbed")]           
  
  #rename variables
  names(u)<-c("Island","Wetland Type","Dataset",
              "Area Barren (Acres)","Area Disturbed (Acres)"  ,            
              "Area Native Dominated (Acres)","Area Non-Native Dominated (Acres)","Barren (% of Wetland Area on Island)"  ,                        
              "Native Dominated  (% of Wetland Area on Island)","Non-Native Dominataed (% of Wetland Area on Island)"  , "Disturbed (% of Wetland Area on Island)"   ) 
  
  #Create a couple tables while these are numeric to use later
  areas<-u[,c( "Island","Wetland Type", "Dataset","Area Barren (Acres)" ,"Area Disturbed (Acres)",
               "Area Native Dominated (Acres)","Area Non-Native Dominated (Acres)"      )]
  
  percentages<-u[,c( "Island","Wetland Type", "Dataset", "Barren (% of Wetland Area on Island)"  ,
                     "Native Dominated  (% of Wetland Area on Island)","Non-Native Dominataed (% of Wetland Area on Island)",
                     "Disturbed (% of Wetland Area on Island)"    )]    
  
  
  arealong<-gather(areas, key = "Descriptor", value = "Acres", 4:7)
  arealong$HabitatQuality<-sub("Area ", "", arealong$Descriptor)
  arealong$HabitatQuality<-sub("\\(Acres)", "", arealong$HabitatQuality)
  arealong <- select(arealong, -Descriptor)
  
  pctlong<-gather(percentages, key = "Descriptor", value = "Percent of Wetland Habitat on Island", 4:7)
  pctlong$HabitatQuality<-sub("\\(% of Wetland Area on Island)", "", pctlong$Descriptor)
  pctlong <- select(pctlong, -Descriptor)
  
  
  #reorder Habitat Quality Variables So they Plot in more intuitive colors
  str(arealong)
  arealong$HabitatQuality<-as.factor(arealong$HabitatQuality)
  levels(arealong$HabitatQuality)
  arealong$HabitatQuality <- ordered(arealong$HabitatQuality,levels = c("Disturbed ","Barren ","Non-Native Dominated ","Native Dominated "))
  #check this worked
  levels(arealong$HabitatQuality)   
  
  #now we can make pretty
  
  
  k<-ggplot(arealong, aes(arealong$Island, arealong$Acres ,  fill = arealong$HabitatQuality))
  pretty1<-k+geom_bar(stat = "identity",  position = "stack")+
    labs(fill = "Habitat Quality", title = "Wetlands Summary", x = "Island", y = "Area (Acres)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  
  plotname1<- sub("wetterdata", "QualPlot.tif", spendy)
  ggsave(filename=plotname1, plot=pretty1, device = "tiff" )
  
  
  u$`Area Barren (Acres)`[u$`Area Barren (Acres)` <1]<-"<1"
  u$`Area Native Dominated (Acres)`[u$`Area Native Dominated (Acres)` <1]<-"<1"
  u$`Area Non-Native Dominated (Acres)`[u$`Area Non-Native Dominated (Acres)` <1]<-"<1"
  u$`Area Disturbed (Acres)`[u$`Area Disturbed (Acres)` <1]<-"<1"
  
  u$`Barren (% of Wetland Area on Island)`  [u$`Barren (% of Wetland Area on Island)`  <1]<-"<1"
  u$`Native Dominated  (% of Wetland Area on Island)` [u$`Native Dominated  (% of Wetland Area on Island)` <1]<-"<1"
  u$`Non-Native Dominataed (% of Wetland Area on Island)`  [u$`Non-Native Dominataed (% of Wetland Area on Island)` <1]<-"<1"
  u$`Disturbed (% of Wetland Area on Island)`  [u$`Disturbed (% of Wetland Area on Island)` <1]<-"<1"
  
  #write file to drive
  outname<-sub("wetterdata", "habqualsummary.csv", spendy)
  write.csv(u, file = outname)
  
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Run the Poly to Poly Intersections~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#summarize land ownership for wetlands habitats - These take 10 hours to run

typeownni<-raster::intersect(nwiwetter, ownBuffer )
typeownni$newarea_m<-gArea(typeownni, byid = T)
typeownnidata<-typeownni@data
write.csv(intersectownnidata, file = "intersectownnidata.csv")
typeownni$keyinfo<-typeownni$BigstOwner
toc()
tic()
typeownno<-raster::intersect(noaawetter, ownBuffer)
typeownno$newarea_m<-gArea(typeownno, byid = T)
typeownnodata<-typeownno@data
write.csv(intersectownnodata, file = "intersectownnodata.csv")
typeownno$keyinfo<-typeownno$BigstOwner
toc()
tic()

intersectresni<-raster::intersect(nwiwetter, myreserves)
intersectresni$newarea_m<-gArea(intersectresni, byid = T)
intersectresnidata<-intersectresni@data
write.csv(intersectresnidata, file = "intersectresnidata.csv")
intersectresni$keyinfo<-intersectresni$Type_Defin
toc()
tic()
intersectresno<-raster::intersect(noaawetter, myreserves)
intersectresno$newarea_m<-gArea(intersectresno, byid = T)
intersectresnodata<-intersectresno@data
write.csv(intersectresnodata, file = "intersectresnodata.csv")
intersectresno$keyinfo<-intersectresno$Type_Defin
toc()

tic()

intersectchni<-raster::intersect(nwiwetter, crithab)
intersectchni$newarea_m<-gArea(intersectchni, byid = T)
intersectchnidata<-intersectchni@data
write.csv(intersectchnidata, file = "intersectchnidata.csv")
intersectchni$keyinfo<-"Designated as Critical Habitat"

toc()
tic()
intersectchno<-raster::intersect(noaawetter, crithab)
intersectchno$newarea_m<-gArea(intersectchno, byid = T)
intersectchnodata<-intersectchno@data
write.csv(intersectchnodata, file = "intersectchnodata.csv")
intersectchno$keyinfo<-"Designated as Critical Habitat"

toc()

save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_01_post_wetland_intersections_workspace.RData")
#write a function to take in all these extraction files, retain only needed columns, and save them as shpfiles

#how do I call the things I want to keep here, when some of the variables differ across datasets, and the numbers are not the same either?
#I think I just have to rename one thing manually...the extracted attribute to "keyinfo"

#make a list of keeper variables for each outfile:
keepers<-c("island", "wetlandareaonisland","WetlandType","dataset","newarea_m", "keyinfo" )


extractions <- c("typeownni","typeownno", "intersectownni", "intersectownno", "intersectresni", "intersectresno", "intersectchni" , "intersectchno") 
for (ex in extractions[1]){
  #  drop unwanted variables
  j<-get(ex)[,keepers]
  #convert the wetland area on island column to acres and round it too
  j$WetlandAcresOnIsle<-round(j$wetlandareaonisland*0.0002471, digits = 0)
  j$acres<-round(j$newarea_m*0.0002471, digits = 2)
  j$dataset<-sub("noaa", "NOAA - CCAP", j$dataset)
  j$dataset<-sub("nwi", "USFWS - NWI", j$dataset)
  #calculate area proportion of wetlands on island
  j$propwetlandsisle<-round((j$acres/j$WetlandAcresOnIsle)*100, digits = 2)
  #drop the m2 area columns
  
  j<-j[,c("island","WetlandType","dataset", "keyinfo", "acres","WetlandAcresOnIsle", "propwetlandsisle")]
  #give better names
  names(j)<-c("Island","Wetland Type","Dataset","Summary Variable" , "Area (Acres)","Total Wetland Area on Island (Acres)", "Area (% of Wetland Area on Island")
  
  jdata<-j@data
  
  d<-aggregate(j$`Area (Acres)`, by = list(j$Island, j$`Wetland Type`,j$Dataset, j$`Total Wetland Area on Island (Acres)`, j$`Summary Variable`  ), FUN = sum)
  names(d)<-c("Island","Wetland Type","Dataset","Total Wetland Area on Island (Acres)","Summary Variable" ,"Area (Acres)")
  d$acres<-as.numeric(d$`Area (Acres)`)
  d$propwetlandsisle<-round((d$`Area (Acres)` /d$`Total Wetland Area on Island (Acres)`  )*100, digits = 0)
  d$propwetlandsisle[d$propwetlandsisle <1]<-"<1"
  d$`Area (Acres)`<-round(d$`Area (Acres)`, digits = 0)
  #d$`Area (Acres)`[d$`Area (Acres)`<1]<-"<1"
  names(d)<-c("Island","Wetland Type","Dataset","Total Wetland Area on Island (Acres)","Summary Variable" ,"Area (Acres)","Area (% of Wetland Area on Island", "acres")
  
  a<-ggplot(d, aes(d$Island, d$`Area (Acres)`, fill = d$`Summary Variable` ))
  
  pretty<-a+geom_bar(stat = "identity",  position = "stack")+
    labs(fill = "Summary Variable", title = "Wetlands Summary", x = "Island", y = "Area (Acres)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  
  
  
  outname<-paste0(ex, '_hope')
  outnamecsv<-paste0(OutDir, "/", outname, ".csv")
  outnamed<-paste0(OutDir, "/", "varsummary", outname, ".csv")
  outnamed<-sub("intersect", "", outnamed)
  plotname<-sub(".csv", ".tif", outnamed)
  ggsave(filename=plotname, plot=pretty, device = "tiff")
  write.csv(d, file = outnamed)
  write.csv(jdata, file = outnamecsv)
  writeOGR(j, dsn = OutDir, layer = outname , driver="ESRI Shapefile", overwrite_layer = T)
  
}


load("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_01_post_wetland_intersections_workspace.RData")


save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_04_EndOfWetlandsProcessingSpace.RData")




