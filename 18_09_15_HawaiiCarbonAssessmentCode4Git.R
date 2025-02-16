# Appendix K. Code for all  Hawaii Carbon Assessment Based Figures and Tables
#Mari K Reeves
#18_06_14_SummarizePIFWO Polygons
#This code takes the most recent USGS 30 m pixel scale land cover assessment and 
#lumps it into PIFWO habitat category and summarizes the area of each one
#breaks these down by sub-habitat and summarizes areas of those

#Since it was faster, I also converted the file to polygons in GIS and dissolved those 
#polygons by raster value so I can do a spatial extraction to the land cover polygons 
#to get summaries below. 


rm(list = ls()) #remove all past worksheet variables

set.seed(333)

# Read in Base Packages ---------------------------------------------------
pckg <- c("dplyr", "tidyr","RColorBrewer", "ggplot2","curl", 
          "RCurl","parallel","gdalUtils","rgdal","rgeos",  
          "spatial","sp", "raster", "maptools", "spatialEco", 
          "SpatialPack", "spatstat", "microbenchmark",
          "snowfall", "stringi", "stringr", "tictoc") 

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
LandOwnDir<-paste0(SHADir,"LandOwnership/HawaiiParcels/statewide" )
ConservationDir<-paste0(WorkingDir,"/ShapeFiles4R" )
HabitatDir<-paste0(ConservationDir, "/HabitatPolygons")
HabCatDir<-paste0(SHADir, "HawaiiHabitats/HabCatShapefiles")
IsleHabDir<-paste0(SHADir, "HawaiiHabitats/IslandHabitatFiles")
CoastDir<-paste0(SHADir, "Coastlines")
OutDir<-paste0(WorkingDir, "/Test")
list.files(ConservationDir)
ConsDirOut<-paste0(WorkingDir, "/ConsDirOut")
ReportOut<-paste0(BaseDir, "/HSAReports/18_06_14_OtherHabitatsFiguresTables")
setwd(ReportOut)

BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

#I changed this to match the habitat files below...
#sysCRS<-"+proj=utm +zone=4 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

#go online and get the landcover file:https://www.sciencebase.gov/catalog/item/592dee56e4b092b266efeb6b


# download and unzip 
# download.file("https://www.sciencebase.gov/catalog/item/592dee56e4b092b266efeb6b",
#               destfile = "carbon.zip")  #this doesn't work yet, because I can't get the right link from GS site

# carbonLC<-raster(paste0(WorkingDir, "/CAH_LandCover.jpg"))
# plot(carbonLC)
#proj4string(carbonLC)# "+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# #make this into the systemCRS
sysCRS<-"+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
# 
# #bring in the data table that goes with this file (I had to export this from GIS) and categorize 
# the PIFWO habitats, to serve as a crosswalk...This file now has  the PIFWO Habitats in it. 
#mycarbon<-read.csv(paste0(WorkingDir, "/LandCoverCategoriesCarbonAssessment.csv"))
#make a copy
#carboncopy<-mycarbon
# 
# #useful: http://rspatial.org/spatial/rst/8-rastermanip.html
#That's why I did the polygon conversion in ArcGIS and am working with the 
#files imported below, which are the habitats as polygons and dissolved polygons
#dealing with memory in R was too much for this giant polygon file. 
#had to do the spatial overlays in GIS, and am working with a csv file exported from Arc
#After making the carbon assessment rasters into polygons, joining the habitat type and habitat 
#quality data together and also to the coastlines layer (to assign an island name, over was taking forever
#in R) then exporting that dataframe with ~1.5 million records from Arc to bring it into R as follows:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bring Data file in from Yeti Run ~~~~~~~~~~~~~~~~~~~~~
# 
list.files(HabitatDir, pattern = ".shp$", full.names = T)
#read in the habitat file and assign a CRS.(exported this as csv in GIS)
#habitats<-readOGR(dsn = HabitatDir, layer = "18_05_22_HabsWithConsArea")
#reservedata<-habitats@data
#write.csv(reservedata, file = paste0(HabitatDir, "/habitatpolyareasReserveAreas.csv"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bring in pre-historic conservation area~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#These were generated by the nature conservancy, but we don't have reference

prehistoric<-readOGR(dsn = ConservationDir, layer = "state_naturalcom_beforecontact")
plot(prehistoric)

#put it in the sysCRS
prehistoric<- spTransform(prehistoric, CRS(sysCRS))
prehistoric$area_m<-gArea(prehistoric, byid = T)
prehistoricdata<-prehistoric@data

levels(prehistoricdata$COMMUNITY )
prehistoricdata$COMMUNITY<-as.character(prehistoricdata$COMMUNITY)
prehistoricdata$COMMUNITY[prehistoricdata$COMMUNITY ==  "isle" ] <- "Isle"
prehistoricdata$COMMUNITY[prehistoricdata$COMMUNITY == "water"  ]<- "Open Water"
prehistoricdata$COMMUNITY<-as.factor(prehistoricdata$COMMUNITY)
levels(prehistoricdata$COMMUNITY)

preisl<-aggregate(prehistoricdata$area_m, by = list( prehistoricdata$ISLAND), FUN = sum)
names(preisl)<-c("Isl", "Area_m")
levels(preisl$Isl)
island<-c( "Hawaii", "Kauai", "Kahoolawe" ,"Lanai" ,"Maui", "Molokai" ,"Niihau", "Oahu")
preisl<-cbind(preisl, island)

precom<-prehab<-aggregate(prehistoricdata$area_m, by = list(prehistoricdata$COMMUNITY), FUN = sum)
names(precom)<-c("Community", "Area_m")


#convert sqmeters to Acres and Sq Miles
precom$acres<-round(precom$Area_m*0.0002471, digits = 0)
precom$sqmi<-round(precom$Area_m*0.0000003861, digits = 0)



#drop area in meters, rename columns and write out to csv
names(precom)
precom<-precom[,c("Community", "acres","sqmi"   )]
names(precom)<-c("Vegetation Comunity", "Area (Acres of Habitat Before Human Contact)", "Area (Square Miles of Habitat Before Human Contact)")
precom$`Area (Square Miles of Habitat Before Human Contact)`[precom$`Area (Square Miles of Habitat Before Human Contact)` <1]<- "<1"
write.csv(precom, file = "EstimatedAreaPrecontactTNCLayers.csv")

#Bind island names and total area to the file
names(preisl)<-c( "ISLAND" , "TotalIslandArea_m", "island")
names(prehistoricdata)
pre<-merge(prehistoricdata, preisl, by = "ISLAND")

names(pre)
prehab<-aggregate(pre$area_m, by = list(pre$COMMUNITY, pre$island, pre$TotalIslandArea_m), FUN = sum)
names(prehab)<-c("Community", "Island", "totalarea_m", "communityarea_m")
#convert sqmeters to Acres and Sq Miles
prehab$acres_com<-round(prehab$communityarea_m*0.0002471, digits = 0)
prehab$sqmi_com<-round(prehab$communityarea_m*0.0000003861, digits = 0)

prehab$acres_tot<-round(prehab$totalarea_m*0.0002471, digits = 0)
prehab$sqmi_tot<-round(prehab$totalarea_m*0.0000003861, digits = 0)

prehab$proptotalarea<-round((prehab$acres_com/prehab$acres_tot)*100)

#drop, reorganize, rename, write out
names(prehab)
prehab<-prehab[,c("Community","Island" ,"acres_com",  "sqmi_com", "proptotalarea"   )]
names(prehab)<-c("Vegetation Comunity", "Island", "Area (Acres of Habitat Before Human Contact)", "Area (Square Miles of Habitat Before Human Contact)",  "Area (% of Total Island Area)")

prehab$`Area (Acres of Habitat Before Human Contact)` [prehab$`Area (Acres of Habitat Before Human Contact)` <1]<- "<1"
prehab$`Area (% of Total Island Area)`[prehab$`Area (% of Total Island Area)`<1]<- "<1"
prehab$`Area (Square Miles of Habitat Before Human Contact)`[prehab$`Area (Square Miles of Habitat Before Human Contact)` <1]<- "<1"

write.csv(prehab, file = "PreContactSummaryByIsland.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#not done because did it last time, spatial file is huge and takes too much memory~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# proj4string(habitats)<-sysCRS
# habitatcopy<-habitats
habdata<-read.csv("C:/Users/marireeves/Documents/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/HabitatPolygons/18_05_22_HabPolyExport.csv" )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Reformat the Data to make nice Maps and Tables

#make a copy
habcopy<-habdata
#cut unwanted columns out of habdata and reassign variable names 
names(habdata)

#convert conservation area in hectares to acres
habdata$ReserveAcres<-habdata$ConsAreaHA*2.471
habdata<-habdata[,c("PIFWOHAB_1", "PIFWOHAB_2" ,"Descriptio","BigstOwner",
                    "Island"  ,  "AreaAcres", "ReserveAcres" )]
names(habdata)<-c("vegclass", "PIFWO_Habitat", "HabQual", "owner", "island", "AreaAcres", "ReserveAcres" )


#Get rid of Lehua

levels(habdata$island)
islands2keep<-c( "Hawaii","Kahoolawe", "Kauai","Lanai" ,"Maui","Molokai","Niihau","Oahu"     )
habdata<-habdata[habdata$island %in% islands2keep,]

#let's summarize by these, by habitat, then maybe create a summary with simpler (fewer) categories
unique(habdata$PIFWO_Habitat)
habdata$habtype<-habdata$PIFWO_Habitat
#clean up the habitats so they make better names

#Clean up the habitat names to remove slashes and dashes
# habdata$habtype<-sub("-", " ",  habdata$habtype)
#habdata$habtype<-sub(" ", "", habdata$habtype)
#habdata$habtype<-sub(" ", "", habdata$habtype)#get rid of where there were double spaces
#add a column converting area to miles

habdata$Area_SqMi<-habdata$AreaAcres*0.001563
#check that areas are right
sum(habdata$Area_SqMi)#interesting, that implies no spatial overlap in polygons! 
#also checking this by intersecting the layer with itself in GIS, but this may take another 
#20 hours to complete...
ArchipelagoAcres<-sum(habdata$AreaAcres)

#First error, there are polygons with no habitat assignment
#replace with words "No Data"
habdata$habtype<-sub("^ $", "No Data", habdata$habtype)

habdata$vegclass<-sub("^ $", "No Data", habdata$vegclass)

#replace missing data in hab qual with "No Data"
habdata$HabQual<-sub("^ $", "No Data", habdata$HabQual)

#replace missing data in Land Owner with "No Data"
habdata$owner<-sub("^ $", "No Data", habdata$owner)

#change state dhhl to state in land ownership
habdata$owner
habdata$owner<-sub( "Govt. State DHHL", "Govt. State", habdata$owner)


#rename Non-Forest to ScrubShrub
habdata$habtype<-sub("Non-Forest", "Grasslands and Shrublands", habdata$habtype)

#capitalize "data" in the land ownership column
habdata$owner<-sub("No data", "No Data", habdata$owner)

nohab<-habdata[habdata$habtype == "No Data",]
unclassifiedacres<-sum(nohab$AreaAcres) # 1323.794
#It's only 1300 acres unclassified, this is nothing, out of 4,099,205 ArchipelagoAcres
#so could just drop these for now (or forever) and not worry about them
habdata$habtype<-as.factor(habdata$habtype)

levels(habdata$habtype)
#create a column in habdata for % of Island area
islandareas<-aggregate(habdata$AreaAcres~habdata$island, FUN = sum)
names(habdata)
names(islandareas)<-c("island", "IslandAreaAcres")
habdata<-left_join(habdata, islandareas)

#other late in the game data changes


#Reorder islands from East to west in figures - doing this tomorrow, not a priority
levels(habdata$island)
habdata$island<-as.character(habdata$island)#get rid of pre-lehua factor levels
habdata$island<-as.factor(habdata$island)
levels(habdata$island)
habdata$island  <- ordered(habdata$island ,levels = c( "Hawaii"  , "Maui", "Kahoolawe",  "Lanai",  "Molokai", "Oahu" , "Kauai" , "Niihau" )) 
levels(habdata$island)#check that worked


# Alien - change to non-native in veg class and habitat quality columns
levels(habdata$HabQual)
habdata$HabQual<-sub("Alien", "Non-native", habdata$HabQual)
habdata$HabQual<-sub("alien", "non-native", habdata$HabQual)
habdata$HabQual<-sub(" /", "", habdata$HabQual)

levels(habdata$vegclass)
habdata$vegclass<-sub("Alien", "Non-native", habdata$vegclass)
habdata$vegclass<-sub("alien", "non-native", habdata$vegclass)

#order these to plot nicely
habdata$HabQual<-as.factor(habdata$HabQual)
habdata$HabQual <- ordered( habdata$HabQual  ,levels = c( "Heavily Disturbed" , "Bare or <5% Vegetation", 
                                                          "Native Non-native Mix",  "Native Dominated",  "No Data" )) 
levels(habdata$HabQual)
#Make file size smaller for all the figures and make them into jpgs(global change in code)
#replace tiff with jpg -- check 
#replace ".tif" with ".jpg" -- check


#make a list of these habitats to use in function
habitatlist<-unique(habdata$habtype)


#Fix color pallette to colorblind friendly:http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Plot Vegetation Types by Island
#SimpleVegetationSummary<-function(myhabitat){
for(myhabitat in habitatlist){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$AreaAcres, by = list(subtable$habtype ,
                                                   subtable$vegclass, subtable$owner, 
                                                   subtable$island, subtable$IslandAreaAcres), FUN = sum)
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  summary$PercentIslandArea<-(summary$x/summary$Group.5)*100
  summary<-summary[,c("Group.1","Group.2","Group.3","Group.4","Group.5","x",                
                      "PercentIslandArea" ,"PercentArchArea"  )]
  names(summary)<-c("Habitat", "Vegetation Class", "Land Ownership", "Island", "Total Island Area (Acres)","Area (Acres)",
                    "Area (% of Island)", "Area (% of State of Hawaii)")
  summary$`Area (Acres)`<-round(summary$`Area (Acres)`, digits = 0)
  summary$`Area (% of State of Hawaii)`  <-round(summary$`Area (% of State of Hawaii)`, digits = 0)
  summary$`Total Island Area (Acres)`<-round(summary$`Total Island Area (Acres)`, digits = 0)
  summary$`Area (% of Island)`  <-round(summary$`Area (% of Island)` , digits = 0)
  
  
  vc<-ggplot(summary, aes(summary$Island, summary$`Area (Acres)`, fill = summary$`Vegetation Class`  ))
  plotname<-paste0(ReportOut, "/Hawaii", myhabitat, "VegetationSummaryPlot.jpg")
  
  plottitle<-paste0(myhabitat, " Vegetation Summary")
  prettyvc<-vc+geom_bar(stat = "identity",  position = "fill")+
    labs(fill = "Vegetation Class", title = plottitle, x = "Island", y = "Area (Proportion of Total Habitat on Island)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  
  ggsave(filename=plotname, plot=prettyvc, device = "jpg")
  
  
  
  summary$`Area (% of State of Hawaii)`[summary$`Area (% of State of Hawaii)`<1]<-"<1"
  summary$`Area (Acres)`[summary$`Area (Acres)`<1]<-"<1"
  
  #Reorder vegetation categories per Cheryl's email - this is harder than it sounds to cheryl in the loop, so waiting for now
  
  
  #Check Vegetation class table (seems to have assigned the wrong name - add hab qual, drop vegetation class)
  
  
  summary$`Area (% of Island)` [summary$`Area (% of Island)` <1]<-"<1"
  tablename<-paste0(ReportOut, "/Hawaii", myhabitat, "VegetationTypeTable.csv")
  write.csv(summary, file = tablename)    
  
}

#lapply(habitatlist, SimpleVegetationSummary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#SimpleHabQualSummary<-function(myhabitat){
for(myhabitat in habitatlist){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$AreaAcres, by = list(subtable$habtype,
                                                   subtable$HabQual, subtable$owner, 
                                                   subtable$island, subtable$IslandAreaAcres), FUN = sum)
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  summary$PercentIslandArea<-(summary$x/summary$Group.5)*100
  #drop island area and reorganize columns
  summary<-summary[,c("Group.1","Group.2","Group.3","Group.4","x",                
                      "PercentIslandArea" ,"PercentArchArea"  )]
  names(summary)<-c("Habitat", "Habitat Quality", "Land Ownership", "Island", "Area (Acres)",
                    "Area (% of Island)","Area (% of State of Hawaii)")
  summary$`Area (Acres)`<-round(summary$`Area (Acres)`, digits = 0)
  summary$`Area (Acres)`[summary$`Area (Acres)`<1]<-"<1"
  summary$`Area (% of State of Hawaii)`<-round(summary$`Area (% of State of Hawaii)`, digits = 0)
  summary$`Area (% of State of Hawaii)`[summary$`Area (% of State of Hawaii)`<1]<-"<1"
  summary$`Area (% of Island)`  <-round(summary$`Area (% of Island)` , digits = 0)
  summary$`Area (% of Island)` [summary$`Area (% of Island)` <1]<-"<1"
  tablename<-paste0(ReportOut, "/Hawaii", myhabitat, "QualityTable.csv")
  write.csv(summary, file = tablename)  
  
  
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area, fill = summary$`Habitat Quality` ))
  plotname<-paste0(ReportOut, "/Hawaii", myhabitat, "HabitatQualityPlot.jpg")
  plottitle<-paste0(myhabitat, " Habitat Quality Summary")
  pretty<-a+geom_bar(stat = "identity", position = "fill")+
    labs(fill = "Habitat Quality", title = plottitle, x = "Island", y = "Area (Proportion of Total Habitat on Island)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "jpg")
  
}

#lapply(habitatlist, SimpleHabQualSummary)

rm(subtable)
rm(summary)
rm(a)
rm(pretty)
#Make a plot of Land Ownership by island
SimpleOwnershipSummary<-function(myhabitat){
  # for(myhabitat in habitatlist){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$AreaAcres, by = list(subtable$habtype,
                                                   subtable$owner, 
                                                   subtable$island, subtable$IslandAreaAcres), FUN = sum)
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  summary$PercentIslandArea<-(summary$x/summary$Group.4)*100
  #drop island area and reorganize columns
  summary<-summary[,c("Group.1","Group.2","Group.3","x",                
                      "PercentIslandArea" ,"PercentArchArea"  )]
  
  
  names(summary)<-c("Habitat", "Land Ownership", "Island", "Area (Acres)",
                    "Area (% of Island)","Area (% of State of Hawaii)")
  summary$`Area (Acres)`<-round(summary$`Area (Acres)`, digits = 0)
  summary$`Area (Acres)`[summary$`Area (Acres)`<1]<-"<1"
  summary$`Area (% of State of Hawaii)`<-round(summary$`Area (% of State of Hawaii)`, digits = 0)
  summary$`Area (% of State of Hawaii)`[summary$`Area (% of State of Hawaii)`<1]<-"<1"
  summary$`Area (% of Island)`  <-round(summary$`Area (% of Island)` , digits = 0)
  summary$`Area (% of Island)` [summary$`Area (% of Island)` <1]<-"<1"
  tablename<-paste0(ReportOut, "/Hawaii", myhabitat, "OwnTable.csv")
  write.csv(summary, file = tablename)           
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area, fill = summary$`Land Ownership`))
  plotname<-paste0(ReportOut, "/Hawaii", myhabitat, "LandOwnershipPlot.jpg")
  plottitle<-paste0(myhabitat, " Land Ownership Summary")
  pretty<-a+geom_bar(stat = "identity", position = "fill")+
    labs(fill = "Land Ownership", title = plottitle, x = "Island", y = "Area (Proportion of Total Habitat on Island)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "jpg")
  
}

lapply(habitatlist, SimpleOwnershipSummary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Make a plot of habitats by island


SimpleIslandSummary<-function(myhabitat){
  # for(myhabitat in habitatlist){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$AreaAcres, by = list(subtable$habtype,
                                                   subtable$island, subtable$IslandAreaAcres), FUN = sum)
  
  summary$PercentIslandArea<-(summary$x/summary$Group.3)*100
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  #drop island area and reorganize columns
  summary<-summary[,c("Group.1","Group.2","x",                
                      "PercentIslandArea" ,"PercentArchArea"  )]
  
  names(summary)<-c("Habitat",  "Island", "Area (Acres)",
                    "Area (% of Island)","Area (% of State of Hawaii)")
  summary$`Area (Acres)`<-round(summary$`Area (Acres)`, digits = 0)
  summary$`Area (Acres)`[summary$`Area (Acres)`<1]<-"<1"
  summary$`Area (% of State of Hawaii)`<-round(summary$`Area (% of State of Hawaii)`, digits = 0)
  summary$`Area (% of State of Hawaii)`[summary$`Area (% of State of Hawaii)`<1]<-"<1"
  summary$`Area (% of Island)`  <-round(summary$`Area (% of Island)` , digits = 0)
  summary$`Area (% of Island)` [summary$`Area (% of Island)` <1]<-"<1"
  tablename<-paste0(ReportOut, "/Hawaii", myhabitat, "IslandTable.csv")
  write.csv(summary, file = tablename)           
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area))
  plotname<-paste0(ReportOut, "/Hawaii", myhabitat, "IslandPlot.jpg")
  plottitle<-paste0(myhabitat, " Area Summary")
  pretty<-a+geom_bar(stat = "identity")+
    labs(title = plottitle, x = "Island", y = "Area (Acres)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "jpg")
  
}

lapply(habitatlist, SimpleIslandSummary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Summarize Reserve Status by Habitat Type, HabQual, Veg Class and Ownership
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("~/HSAs/HSAReports/18_06_14_OtherHabitatsFiguresTables/18_06_14_latenight.RData")
# reservedata<-read.csv("C:/Users/marireeves/Documents/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/HabitatPolygons/habitatpolyareasReserveAreas.csv")

#check acreages in habdata
rescheck<-aggregate(habdata$ReserveAcres~habdata$island, FUN = sum)
names(rescheck)
rescheck$sqmi<-rescheck$`habdata$ReserveAcres`*0.001563
islandareas$sqmi<-islandareas$IslandAreaAcres*0.001563

#Plot statewide reserve habitat quality

# r1<-ggplot(habdata, aes(habdata$habtype, habdata$ReserveAcres, fill = habdata$HabQual))
# resstatewide<-r1+geom_bar(stat = "identity")+
#   labs(title = "Quality of Reserve Lands Statewide", x = "Habitat Type", y = "Area (Acres)" , fill = "Habitat Quality") +
#   theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
#   scale_fill_manual(values=cbPalette)
# 
# ggsave(filename="StatewideReserveHabitatQuality.jpg", plot=resstatewide, device = "jpg")

#ReserveAreaSummary<-function(myhabitat){
for (myhabitat in habitatlist ){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$ReserveAcres, by = list(subtable$habtype, subtable$island, subtable$HabQual, 
                                                      subtable$IslandAreaAcres), FUN = sum)
  
  
  summary$PercentIsleArea<-(summary$x/summary$Group.4)*100
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  
  
  names(summary)<-c("Habitat",  "Island", "Habitat Quality",  "Island Area (Acres)", "Area in Reserves (Acres)",
                    "Reserve Area (% of Island)","Reserve Area (% of State of Hawaii)")
  
  #reorder these to make more sense
  
  summary<-summary[, c("Island", "Habitat", "Habitat Quality", "Area in Reserves (Acres)", "Island Area (Acres)",
                       "Reserve Area (% of Island)","Reserve Area (% of State of Hawaii)"  )]
  
  summary$`Area in Reserves (Acres)`<-round(summary$`Area in Reserves (Acres)`, digits = 0)   
  summary$`Island Area (Acres)`<-round(summary$`Island Area (Acres)`, digits = 0)  
  summary$`Reserve Area (% of Island)`<-round(summary$`Reserve Area (% of Island)`, digits = 0)  
  summary$`Reserve Area (% of State of Hawaii)`<-round(summary$`Reserve Area (% of State of Hawaii)`, digits = 0)
  
  # summary<-summary[, c("Island", "Habitat", "Habitat Quality", "Area in Reserves (Acres)",
  #                       "Reserve Area (% of Island)",
  #                      "Reserve Area (% of State of Hawaii)" )]
  
  
  res<-ggplot(summary, aes(summary$Island, summary$`Area in Reserves (Acres)`, fill = summary$`Habitat Quality`))
  plotname<-paste0(ReportOut, "/Hawaii", myhabitat, "ReserveQualityPlot.jpg")
  plottitle<-paste0(myhabitat, " Reserve Quality Summary")
  prettyrhythm<-res+geom_bar(stat = "identity", position = "stack")+
    labs(title = plottitle, x = "Island", y = "Area in Reserves (Acres) " , fill = "Habitat Quality") +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=prettyrhythm, device = "jpg")
  
  
  
  summary$`Area in Reserves (Acres)`[summary$`Area in Reserves (Acres)` <1]<- "<1"
  
  summary$`Reserve Area (% of Island)`[ summary$`Reserve Area (% of Island)` <1]<- "<1"
  
  summary$`Reserve Area (% of State of Hawaii)`[summary$`Reserve Area (% of State of Hawaii)`<1 ]<- "<1"
  
  
  
  tablename<-paste0(ReportOut, "/Hawaii", myhabitat, "ReserveStatusTable.csv")
  write.csv(summary, file = tablename)
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Summarize the Critical Habitat Extraction (run separately and late)
#read in file - fred did this extraction separately because we couldn't run it on yeti

chsimple<-read.csv(paste0(HabitatDir, "/18_06_14_HSA_CH_14Jun2018.csv"))
#recategorize a few variables
names(chsimple)
levels(chsimple$HSA)


#get rid of lehua

levels(chsimple$Isle)
islands2keep<-c( "Hawaii","Kahoolawe", "Kauai","Lanai" ,"Maui","Molokai","Niihau","Oahu"     )
chsimple<-chsimple[chsimple$Isle %in% islands2keep,]

#Reorder islands from East to west in figures - doing this tomorrow, not a priority
levels(chsimple$Isle)
chsimple$Isle<-as.character(chsimple$Isle)#get rid of pre-lehua factor levels
chsimple$Isle<-as.factor(chsimple$Isle)
levels(chsimple$Isle)
chsimple$Isle  <- ordered(chsimple$Isle ,levels = c( "Hawaii"  , "Maui", "Kahoolawe",  "Lanai",  "Molokai", "Oahu" , "Kauai" , "Niihau" )) 
levels(chsimple$Isle)#check that worked

#rename Non-Forest to ScrubShrub
chsimple$HSA<-sub("Non-Forest", "Grasslands and Shrublands", chsimple$HSA)

#convert ha to acres
chsimple$acres<-round(chsimple$Ha*2.471, digits = 2)
chsimple$IslandAreaAcres<-round(as.numeric(chsimple$IslandAreaAcres), digits = 0)

#merge in total island area
levels(chsimple$Isle)
names(islandareas)
names(chsimple)<-c("island" ,"HSA","Ha","acres")
chsimple<-merge(chsimple, islandareas, by = "island")

chsimplehablist<-unique(chsimple$HSA)

for(myhabitat in chsimplehablist){
  subtablech<-chsimple[chsimple$HSA == myhabitat,]
  summarych<-aggregate(subtablech$acres , by = list(subtablech$island, subtablech$HSA,   subtablech$IslandAreaAcres), FUN = sum)
  summarych$PercentIslandArea<-round((summarych$x/summarych$Group.3)*100, digits = 0)
  summarych$percentarchipelagoarea<-round((summarych$x/ArchipelagoAcres)*100, digits = 0)
  names(summarych)<-c("Island", "Habitat", "Total Island Area (Acres)","Area (Acres)",
                      "Area (% of Island)", "Area (% of State of Hawaii)")
  
  
  summarych$`Area (Acres)`<-round(summarych$`Area (Acres)`, digits = 0)
  
  
  chplot<-ggplot(summarych, aes(summarych$Island, summarych$`Area (Acres)`  ))
  plotnamech<-paste0(ReportOut, "/Hawaii", myhabitat, "CriticalHabitatSummaryPlot.jpg")
  
  plottitlech<-paste0(myhabitat, " Critical Habitat Summary")
  prettych<-chplot+geom_bar(stat = "identity",  position = "stack")+
    labs(title = plottitlech, x = "Island", y = "Area (Acres of Habitat Type in Critical Habitat)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  
  ggsave(filename=plotnamech, plot=prettych, device = "jpg")
  
  
  
  summarych$`Area (% of State of Hawaii)`[summarych$`Area (% of State of Hawaii)`<1]<-"<1"
  summarych$`Area (Acres)`[summarych$`Area (Acres)`<1]<-"<1"
  
  #Reorder vegetation categories per Cheryl's email - this is harder than it sounds to cheryl in the loop, so waiting for now
  
  
  #Check Vegetation class table (seems to have assigned the wrong name - add hab qual, drop vegetation class)
  
  
  summarych$`Area (% of Island)` [summarych$`Area (% of Island)` <1]<-"<1"
  tablenamech<-paste0(ReportOut, "/Hawaii", myhabitat, "CriticalHabitatSummaryTable.csv")
  write.csv(summarych, file = tablenamech)    
  
}

#lapply(habitatlist, SimpleVegetationSummary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Summarize Vegetation Class by Habitat Quality

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VegQualSummary<-function(myhabitat){
  #for(myhabitat in habitatlist){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$AreaAcres, by = list(subtable$habtype,
                                                   subtable$HabQual, subtable$vegclass ,
                                                   subtable$island, subtable$IslandAreaAcres), FUN = sum)
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  summary$PercentIslandArea<-(summary$x/summary$Group.5)*100
  #drop island area and reorganize columns
  summary<-summary[,c("Group.1","Group.2","Group.3","Group.4","x",                
                      "PercentIslandArea" ,"PercentArchArea"  )]
  names(summary)<-c("Habitat", "Habitat Quality", "Vegetation Type", "Island", "Area (Acres)",
                    "Area (% of Island)","Area (% of State of Hawaii)")
  summary$`Area (Acres)`<-round(summary$`Area (Acres)`, digits = 0)
  summary$`Area (% of State of Hawaii)`<-round(summary$`Area (% of State of Hawaii)`, digits = 0)
  summary$`Area (% of Island)`  <-round(summary$`Area (% of Island)` , digits = 0)
  summary$`Area (Acres)`<-as.numeric(summary$`Area (Acres)`)
  
  
  
  vq<-ggplot(summary, aes(summary$`Vegetation Type`  ,  summary$`Area (Acres)`, fill = summary$`Habitat Quality` ))
  plotname<-paste0(ReportOut, "/Hawaii", myhabitat, "VegClassQualityPlot.jpg")
  plottitle<-paste0(myhabitat, " Habitat Quality by Vegetation Type")
  prettyvegqual<-vq+geom_bar(stat = "identity", position = "fill")+
    labs(fill = "Habitat Quality", title = plottitle, x = "Vegetation Type", y = "Area (Proportion of Total Vegetation Type in State)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10))+
    scale_fill_manual(values=cbPalette)+
    scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 15, simplify = FALSE), paste, collapse="\n"), limits=unique(summary$`Vegetation Type` ))
  ggsave(filename=plotname, plot=prettyvegqual, device = "jpg")
  
  summary$`Area (Acres)`[summary$`Area (Acres)`<1]<-"<1"
  summary$`Area (% of State of Hawaii)`[summary$`Area (% of State of Hawaii)`<1]<-"<1"
  summary$`Area (% of Island)` [summary$`Area (% of Island)` <1]<-"<1"
  tablename<-paste0(ReportOut, "/Hawaii", myhabitat, "VegQualityTable.csv")
  write.csv(summary, file = tablename)  
}

lapply(habitatlist, VegQualSummary)

save.image("~/HSAs/HSAReports/18_06_14_OtherHabitatsFiguresTables/18_06_20_EndofRunData.RData")

