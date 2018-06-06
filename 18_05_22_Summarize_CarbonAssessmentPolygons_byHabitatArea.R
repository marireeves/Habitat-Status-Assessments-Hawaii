#Mari K Reeves
#18_05_03_SummarizePIFWO Polygons
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
setwd(WorkingDir)
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


BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

#I changed this to match the habitat files below...
#sysCRS<-"+proj=utm +zone=4 +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"

#go online and get the landcover file:https://www.sciencebase.gov/catalog/item/592dee56e4b092b266efeb6b


# download and unzip 
# download.file("https://www.sciencebase.gov/catalog/item/592dee56e4b092b266efeb6b",
#               destfile = "carbon.zip")  #this doesn't work yet, because I can't get the right link from GS site
list.files(HabitatDir)
#unzip("carbon.zip", exdir = ".") # '.' just means current directory
list.files(WorkingDir, pattern = ".csv$")

# carbonLC<-raster(paste0(WorkingDir, "/CAH_LandCover.tif"))
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
list.files(HabitatDir, pattern = ".csv$", full.names = T)
#read in the habitat file and assign a CRS.(exported this as csv in GIS)
#habitats<-readOGR(dsn = HabitatDir, layer = "18_05_22_HabsWithConsArea")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~not done~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Bring in pre-historic conservation area
#prehistoric<-readOGR(dsn = HabitatDir, layer = "18_05_22_HabsWithConsArea")
#not done~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



#make a list of these habitats to use in function
habitatlist<-unique(habdata$habtype)
 

#Fix color pallette to colorblind friendly:http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Plot Vegetation Types by Island
SimpleVegetationSummary<-function(myhabitat){
#for(myhabitat in habitatlist){
  subtable<-habdata[habdata$habtype == myhabitat,]
  summary<-aggregate(subtable$AreaAcres, by = list(subtable$habtype ,
                                                   subtable$vegclass, subtable$owner, 
                                                   subtable$island, subtable$IslandAreaAcres), FUN = sum)
  summary$PercentArchArea<-(summary$x /ArchipelagoAcres)*100
  summary$PercentIslandArea<-(summary$x/summary$Group.5)*100
  summary<-summary[,c("Group.1","Group.2","Group.3","Group.4","x",                
                       "PercentIslandArea" ,"PercentArchArea"  )]
  names(summary)<-c("Habitat", "Vegetation Class", "Land Ownership", "Island", "Area (Acres)",
                   "Area (% of Island)", "Area (% of State of Hawaii)")
  summary$`Area (Acres)`<-round(summary$`Area (Acres)`, digits = 0)
  summary$`Area (Acres)`[summary$`Area (Acres)`<1]<-"<1"
  summary$`Area (% of State of Hawaii)`  <-round(summary$`Area (% of State of Hawaii)`, digits = 0)
  summary$`Area (% of State of Hawaii)`[summary$`Area (% of State of Hawaii)`<1]<-"<1"
  summary$`Area (% of Island)`  <-round(summary$`Area (% of Island)` , digits = 0)
  summary$`Area (% of Island)` [summary$`Area (% of Island)` <1]<-"<1"
    tablename<-paste0(OutDir, "/Hawaii", myhabitat, "VegetationTypeTable.csv")
  write.csv(summary, file = tablename)     
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area, fill = summary$`Vegetation Class`  ))
  plotname<-paste0(OutDir, "/Hawaii", myhabitat, "VegetationSummaryPlot.tif")
  plottitle<-paste0(myhabitat, " Vegetation Summary")
  pretty<-a+geom_bar(stat = "identity",  position = "fill")+
    labs(fill = "Vegetation Class", title = plottitle, x = "Island", y = "Area (Proportion of Total Habitat on Island)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "tiff")
}

lapply(habitatlist, SimpleVegetationSummary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SimpleHabQualSummary<-function(myhabitat){
  #for(myhabitat in habitatlist){
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
  tablename<-paste0(OutDir, "/Hawaii", myhabitat, "QualityTable.csv")
  write.csv(summary, file = tablename)           
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area, fill = summary$`Habitat Quality` ))
  plotname<-paste0(OutDir, "/Hawaii", myhabitat, "HabitatQualityPlot.tif")
  plottitle<-paste0(myhabitat, " Habitat Quality Summary")
  pretty<-a+geom_bar(stat = "identity", position = "fill")+
    labs(fill = "Habitat Quality", title = plottitle, x = "Island", y = "Area (Proportion of Total Habitat on Island)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "tiff")
  
}

lapply(habitatlist, SimpleHabQualSummary)


#Make a plot of Land Ownership by island
SimpleOwnershipSummary<-function(myhabitat){
  #for(myhabitat in habitatlist){
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
  tablename<-paste0(OutDir, "/Hawaii", myhabitat, "OwnTable.csv")
  write.csv(summary, file = tablename)           
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area, fill = summary$`Land Ownership`))
  plotname<-paste0(OutDir, "/Hawaii", myhabitat, "LandOwnershipPlot.tif")
  plottitle<-paste0(myhabitat, " Land Ownership Summary")
  pretty<-a+geom_bar(stat = "identity", position = "fill")+
    labs(fill = "Land Ownership", title = plottitle, x = "Island", y = "Area (Proportion of Total Habitat on Island)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "tiff")
  
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
  tablename<-paste0(OutDir, "/Hawaii", myhabitat, "IslandTable.csv")
  write.csv(summary, file = tablename)           
  summary$area<-as.numeric(summary$`Area (Acres)`)
  a<-ggplot(summary, aes(summary$Island, summary$area))
  plotname<-paste0(OutDir, "/Hawaii", myhabitat, "IslandPlot.tif")
  plottitle<-paste0(myhabitat, " Area Summary")
  pretty<-a+geom_bar(stat = "identity")+
    labs(title = plottitle, x = "Island", y = "Area (Acres)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename=plotname, plot=pretty, device = "tiff")
  
}

lapply(habitatlist, SimpleIslandSummary)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Summarize Reserve Status by Habitat Type, HabQual, Veg Class and Ownership
r1<-ggplot(habdata, aes(habdata$habtype, habdata$ReserveAcres, fill = habdata$HabQual))
r1+geom_bar(stat = "identity")+
  labs(title = "Quality of Reserve Lands Statewide", x = "Habitat Type", y = "Area (Acres)" , fill = "Habitat Quality") +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)


#ReserveAreaSummary<-function(myhabitat){
  for (myhabitat in habitatlist){
    subtable<-habdata[habdata$habtype == myhabitat,]  
    plottitle<-paste0("Quality of ", myhabitat, " Reserve Areas by Island")
    r1<-ggplot(subtable, aes(subtable$island, subtable$ReserveAcres, fill = subtable$HabQual))
    pretty<-r1+geom_bar(stat = "identity")+
      labs(title = plottitle, x = "Habitat Type", y = "Area (Acres)" , fill = "Habitat Quality") +
      theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
      scale_fill_manual(values=cbPalette)
    plotname<-paste0(OutDir, "/Hawaii", myhabitat, "ReservePlot.tif")
    ggsave(filename=plotname, plot=pretty, device = "tiff")
    restable<-aggregate(subtable$ReserveAcres, by = list(subtable$vegclass, subtable$owner,  subtable$island,
                                                          subtable$IslandAreaAcres, subtable$AreaAcres), FUN = sum)
                        
    
    restable$PercentofIslandReserved<-(restable$x/restable$Group.4)*100
    
    
    restable$PercentArchArea<-(restable$x /ArchipelagoAcres)*100
    #drop island area and reorganize columns
    restable<-restable[,c("Group.1","Group.2","Group.3",
                          "x","PercentofIslandReserved", "PercentArchArea" )]
    names(restable)<-c("Vegetation Class", "Owner" ,"Island", "Area (Acres)",
                       "Area (% of Island)", "Area (% of State of Hawaii)")
    
 
    restable$`Area (Acres)`<-round(restable$`Area (Acres)`, digits = 0)
    restable$`Area (Acres)`[restable$`Area (Acres)`<1]<-"<1"
    restable$`Area (% of State of Hawaii)`<-round(restable$`Area (% of State of Hawaii)`, digits = 0)
    restable$`Area (% of State of Hawaii)`[restable$`Area (% of State of Hawaii)`<1]<-"<1"
    restable$`Area (% of Island)`  <-round(restable$`Area (% of Island)` , digits = 0)
    restable$`Area (% of Island)` [restable$`Area (% of Island)` <1]<-"<1"
    
    
   
    tablename<-paste0(OutDir, "/Hawaii", myhabitat, "ReserveTable.csv")
    write.csv(restable, file = tablename)        
}

