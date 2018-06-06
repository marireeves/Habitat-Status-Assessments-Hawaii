#Mari K Reeves
#18_05_23_SummarizeCoastalWetlandsStreamHabitatsForHSAs

#This code will intersect the linear features (coastal and streams) and the wetlands, which are poorly represented in the 
#Carbon Assessment Landcover layers, intersect them with LandOwnership, Habitat Quality, Reserve Areas and Critical Habitats
#and generate sumamary tables for the biologists doing the Habitat Status Assessment Reports. 

#Bring in layer files for wetlands, coastal veg quality, streams, landownership, and reserve status and put them in the SYSCRS

#Run gIntersection with these layers in sequence to summarize linear measure (m) in each of the following:
#Habitat Quality from Carbon Assessment Rasters



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

#Bring in files and transform them into the sysCRS - send them to a new folder - only need to run this once, so commented it out

neededshps<-list.files(ConservationDir, pattern = ".shp$")

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

crithab<-readOGR(dsn = OutDir, layer = "CriticalHabitatsysCRSOnePoly")
crithabdata<-crithab@data
#clean up the reserve layer to delete reserves we don't think are important from a conservation perspective
types2keep<-c( "Marine Life Conservation District",  "National Historical Park", 
               "National Park", "National Wildlife Refuge", "Natural Area Reserve", 
               "Private Preserve", "State Wilderness Park","The Nature Conservancy Preserve",
               "Wildlife Sanctuary")
myreserves<-reserves[reserves$Type_Defin %in% types2keep,]
myreservedata<-myreserves@data


#write functions to intersect the streams, wetlands, and coastal with reserves, critical habitat, and land ownership, calculate and 
#plot length in each status. I think I may just need to treat each of these guys separately and not do this in a loop. Because the 
#incoming datasets are so different. Bleh. 

# wetplaces<-list.files(WetDir, pattern = ".shp")
# 
# 
# #wetowners<-function(wet in wetplaces){
# for (wet in wetplaces[1]){
#   #get rid of the shp extension
#  lyr <- sub(".shp", "", wet)#This gets rid of the .shp extension in the list of files
  coastal<-readOGR(dsn = WetDir, layer = "18_05_24_sysCRSCoastal_Vegetation_Survey_2013-2015")#read in the shpfile
  coastaldata<-coastal@data
  plot(coastal)
  levels(coastal$Island)
  write.csv(coastaldata, file = "coastal.csv")
  
  coastal$length_m<-gLength(coastal, byid = T)
  coastalmeters<-sum(coastal$length_m)
  coastal$length_km<-coastal$length_m/1000
  #Summarize the habitat quality for coastal habitats - rename some things 
  levels(coastal$Veg_Catego) #"Blue"    "Green 1" "Green 2" "Orange"  "Purple"  "Red"     "Yellow" 
  levels(coastal$Status_Cat)# "Blue - High quality"       "Green - Very high quality" "Red - Poor quality"        "Yellow - Moderate quality"
  coastal$qual<-sub("Blue - High quality", "High Quality", coastal$Status_Cat)
  coastal$qual<-sub("Green - Very high quality", "Very High Quality", coastal$qual)
  coastal$qual<-sub("Red - Poor quality", "Poor Quality", coastal$qual)
  coastal$qual<-sub("Yellow - Moderate quality", "Moderate Quality", coastal$qual)
  coastaldata<-coastal@data
  
  
  #create a column in coastal for % of coastline length
  islandlength<-aggregate(coastal$length_km~coastal$Island , FUN = sum)
  names(coastal)
  names(islandlength)<-c("Island", "IslandLengthkm")
  coastal<-merge(coastal, islandlength)
  #check that worked
 coastaldata<-coastal@data
 
  
  coastalkm<-sum(coastal$length_km)
  coastalqual<-aggregate(coastal$length_km, by = list(coastal$qual, coastal$Island, coastal$IslandLengthkm), FUN = sum)
  names(coastalqual)<-c("Habitat Quality", "Island"," IslandLength" ,"Coastline Length (km)")
  coastalqual$`Coastline Length (km)`<-round(coastalqual$`Coastline Length (km)`, digits = 0)
  coastalqual$perctotallength<-round(100*(coastalqual$`Coastline Length (km)`/coastalkm), digits = 0)
  coastalqual$percentIslandLength<-round(100*(coastalqual$`Coastline Length (km)`/coastalqual$` IslandLength`  ), digits = 0)
coastalqual$percentIslandLength[coastalqual$percentIslandLength <1]<-"<1"
coastalqual$perctotallength[coastalqual$perctotallength <1]<-"<1"
coastalqual<-coastalqual[,c("Habitat Quality","Island" ,"Coastline Length (km)", "perctotallength" ,     
                          "percentIslandLength")]  
 names(coastalqual)<-c("Habitat Quality","Island" ,"Coastline Length (km)", "Length (% of Total State Coastline)"  ,    
                       "Length (% of Total Island Coastline" )
 write.csv(coastalqual, file = "coastalqualitystatewide.csv")
 
 #Make a figure for coastal habitat quality by Island
 
 #load color blind palette
 cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
 
 #reorder Habitat Quality Variables So they Plot in more intuitive colors
 str(coastalqual)
 coastalqual$`Habitat Quality`<-as.factor(coastalqual$`Habitat Quality`)
 levels(coastalqual$`Habitat Quality`)
 
 #sizes <- ordered(sizes, levels = c("small", "medium", "large"))
 coastalqual$`Habitat Quality` <- ordered(coastalqual$`Habitat Quality`,levels = c("Poor Quality","Moderate Quality","High Quality","Very High Quality"))
 #check this worked
 levels(coastalqual$`Habitat Quality`)
 
 a<-ggplot(coastalqual, aes(coastalqual$Island, coastalqual$`Coastline Length (km)`, fill = coastalqual$`Habitat Quality`))
 plottitle<-"Coastal Habitat Quality Summary"
 pretty<-a+geom_bar(stat = "identity",  position = "fill")+
   labs(fill = "Habitat Quality", title = plottitle, x = "Island", y = "Length (% of Total State Coastline)" ) +
   theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
   scale_fill_manual(values=cbPalette)
 ggsave(filename="CoastalHabitatQualitySummary.tif", plot=pretty, device = "tiff" )
 
 
 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #summarize land ownership for coastal habitats
  
  

  intersect1<-raster::intersect(coastal, ownBuffer )
  plot(intersect1, col = "red", add = T)
  
  intersect1$length_m<-gLength(intersect1, byid = T)
  interdata<-intersect1@data
  names(interdata)
  coastalownqual<-intersect1[c( "BigstOwner","Island.2" ,  "IslandLengthkm", "length_m"    )]
  coastalowndata<-coastalownqual@data
  coastalowndata$length_km<-coastalowndata$length_m/1000

  clowndata<-aggregate(coastalowndata$length_km, by = list(coastalowndata$BigstOwner, coastalowndata$Island.2, coastalowndata$IslandLengthkm), FUN = sum)
  names(clowndata)<-c("Land Owner", "Island"," IslandLength" ,"Coastline Length (km)")
  clowndata$`Coastline Length (km)`<-round(clowndata$`Coastline Length (km)`, digits = 0)
  clowndata$perctotallength<-round(100*(clowndata$`Coastline Length (km)`/coastalkm), digits = 0)
  clowndata$percentIslandLength<-round(100*(clowndata$`Coastline Length (km)`/clowndata$` IslandLength`  ), digits = 0)
  clowndata$percentIslandLength[clowndata$percentIslandLength <1]<-"<1"
  clowndata$perctotallength[clowndata$perctotallength <1]<-"<1"
  clowndata<-clowndata[,c("Land Owner","Island" ,"Coastline Length (km)", "perctotallength" ,     
                              "percentIslandLength")]  
  names(clowndata)<-c("Land Ownership","Island" ,"Coastline Length (km)", "Length (% of Total State Coastline)"  ,    
                        "Length (% of Total Island Coastline" )
  
  #capitalize "data" in the land ownership column
  clowndata$`Land Ownership`<-as.character(clowndata$`Land Ownership`)
  clowndata$`Land Ownership`<-sub("No data", "No Data", clowndata$`Land Ownership`)
  
  write.csv(clowndata, file = "coastalownershipstatewide.csv")
  
  #Make a figure for coastal habitat quality by Island
  
  #load color blind palette
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
  
  # # #reorder Land Ownership Variables private plots in grey
  # str(clowndata)
  # clowndata$`Land Ownership`<-as.factor(clowndata$`Land Ownership`)
  # levels(clowndata$`Land Ownership`)
  # 
  # #sizes <- ordered(sizes, levels = c("small", "medium", "large"))
  # clowndata$`Land Ownership` <- ordered(clowndata$`Land Ownership`,levels = c(  "Private","Govt. Federal","Govt. Local","Govt. State","Govt. State DHHL" ,"No Data"))
  # #check this worked
  # levels(clowndata$`Land Ownership`)
  # 
  a<-ggplot(clowndata, aes(clowndata$Island, clowndata$`Coastline Length (km)`, fill = clowndata$`Land Ownership`))
  plottitle<-"Coastal Land Ownership Summary"
  pretty<-a+geom_bar(stat = "identity",  position = "fill")+
    labs(fill = "Land Ownership", title = plottitle, x = "Island", y = "Length (% of Island Coastline)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename="CoastalLandOwnershipSummary.tif", plot=pretty, device = "tiff" )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Summarize Reserve Status for Coastal Habitats
  
  intersect2<-raster::intersect(coastal, myreserves)
  intersect2$length_m<-gLength(intersect2, byid = T)
  inter2data<-intersect2@data
  
  names(inter2data)
  
  coastalreserves<-intersect2[c(  "Type_Defin"  ,"Island.2" ,  "IslandLengthkm", "length_m"    )]
  coastalresdata<-coastalreserves@data
  coastalresdata$length_km<-coastalresdata$length_m/1000

  
  creserves<-aggregate(coastalresdata$length_km, by = list(coastalresdata$Type_Defin  , coastalresdata$Island.2, coastalresdata$IslandLengthkm), FUN = sum)
  names(creserves)<-c("Reserve Type", "Island"," IslandLength" ,"Coastline Length (km)")
  creserves$`Coastline Length (km)`<-round(creserves$`Coastline Length (km)`, digits = 0)
  creserves$perctotallength<-round(100*(creserves$`Coastline Length (km)`/coastalkm), digits = 0)
  creserves$percentIslandLength<-round(100*(creserves$`Coastline Length (km)`/creserves$` IslandLength`  ), digits = 0)
  creserves$percentIslandLength[creserves$percentIslandLength <1]<-"<1"
  creserves$perctotallength[creserves$perctotallength <1]<-"<1"
  creserves<-creserves[,c("Reserve Type","Island" ,"Coastline Length (km)", "perctotallength" ,     
                              "percentIslandLength")]  
  names(creserves)<-c("Reserve Type","Island" ,"Coastline Length in Reserve (km)", "Length (% of Island Coastline)"  ,    
                        "Length (% of Total Island Coastline" )
  write.csv(creserves, file = "creservesitystatewide.csv")
  
  #Make a figure for coastal Reserve Type by Island
  
 
  # #reorder Reserve Type Variables So they Plot in more intuitive colors
  # str(creserves)
  # creserves$`Reserve Type`<-as.factor(creserves$`Reserve Type`)
  # levels(creserves$`Reserve Type`)
  # 
  # #sizes <- ordered(sizes, levels = c("small", "medium", "large"))
  # creserves$`Reserve Type` <- ordered(creserves$`Reserve Type`,levels = c("Poor Quality","Moderate Quality","High Quality","Very High Quality"))
  # #check this worked
  # levels(creserves$`Reserve Type`)
  # 
  a<-ggplot(creserves, aes(creserves$Island, creserves$`Coastline Length (km)`, fill = creserves$`Reserve Type`))
  plottitle<-"Coastal Reserve Type Summary"
  pretty<-a+geom_bar(stat = "identity",  position = "stack")+
    labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (km)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename="CoastalReserveTypeSummary.tif", plot=pretty, device = "tiff" )
  
 
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  #Summarize Critical Habitat for Coastal Habitats
  intersect3<-raster::intersect(coastal, crithab)
  inter3data<-intersect3@data

  intersect3$length_m<-gLength(intersect3, byid = T)
  inter3data<-intersect3@data
  
  names(inter3data)
  
  coastalCH<-intersect3[c(  "Island" ,  "IslandLengthkm", "length_m"    )]
  coastalCHdata<-coastalCH@data
  coastalCHdata$length_km<-coastalCHdata$length_m/1000
  
  
  ch<-aggregate(coastalCHdata$length_km, by = list(coastalCHdata$Island, coastalCHdata$IslandLengthkm), FUN = sum)
  names(ch)<-c("Island"," IslandLength" ,"Coastline Length (km)")
  ch$`Coastline Length (km)`<-round(ch$`Coastline Length (km)`, digits = 0)
  ch$perctotallength<-round(100*(ch$`Coastline Length (km)`/coastalkm), digits = 0)
  ch$percentIslandLength<-round(100*(ch$`Coastline Length (km)`/ch$` IslandLength`  ), digits = 0)
  ch$percentIslandLength[ch$percentIslandLength <1]<-"<1"
  ch$perctotallength[ch$perctotallength <1]<-"<1"
  ch<-ch[,c("Island" ,"Coastline Length (km)", "perctotallength" ,     
                          "percentIslandLength")]  
  names(ch)<-c("Island" ,"Length of Coastline Designated Critical Habitat (km)", "Length (% of Total State Coastline)"  ,    
                      "Length (% of Total Island Coastline" )
  write.csv(ch, file = "chitystatewide.csv")
  
  #Make a figure for coastal Reserve Type by Island
  
  
  # #reorder Reserve Type Variables So they Plot in more intuitive colors
  # str(ch)
  # ch$`Reserve Type`<-as.factor(ch$`Reserve Type`)
  # levels(ch$`Reserve Type`)
  # 
  # #sizes <- ordered(sizes, levels = c("small", "medium", "large"))
  # ch$`Reserve Type` <- ordered(ch$`Reserve Type`,levels = c("Poor Quality","Moderate Quality","High Quality","Very High Quality"))
  # #check this worked
  # levels(ch$`Reserve Type`)
  # 
  a<-ggplot(ch, aes(ch$Island, ch$`Coastline Length (km)`, fill = ch$`Reserve Type`))
  plottitle<-"Coastal Reserve Type Summary"
  pretty<-a+geom_bar(stat = "identity",  position = "stack")+
    labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (km)" ) +
    theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
    scale_fill_manual(values=cbPalette)
  ggsave(filename="CoastalReserveTypeSummary.tif", plot=pretty, device = "tiff" )
  
  
  
  
  #Summarize Stream Layers 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# #read in NOAA Polygons and a csv to say which to keep or drop
# 
# noaa<-readOGR(dsn = WetDir, layer = "StatewideAllPoly")
# 
# 
# noaaDesc<-read.csv(paste0(WetDir, "/18_05_28_HSA_NOAA2KEEPDROP.csv"))
# noaaDrop<-noaaDesc[noaaDesc$KEEP_DROP == "DROP",]
# noaaKeep<-noaaDesc[noaaDesc$KEEP_DROP == "KEEP",]
# 
# #Filter to keep only the wetlands
# noaawet<-noaa[noaa@data$gridcode %in% noaaKeep$NOAA_CAT,]
# 
# 
# #reproject the wetland file
# noaawet<-spTransform(noaawet, CRS(sysCRS))
# #check this worked
# plot(noaawet)
# 
# #bring in nwi and see if they map onto each other
# nwi<-readOGR(dsn = WetDir, layer = "18_05_24_sysCRSHI_Wetlands.shp"  )
# plot(nwi, add = T, col = "red")

#save this to file so we can restart R and not bring in the whole NOAA poly file. 

#writeOGR(g, dsn = WetDir, layer = "18_05_28_NOAAWetlands", driver="ESRI Shapefile", overwrite_layer = T)