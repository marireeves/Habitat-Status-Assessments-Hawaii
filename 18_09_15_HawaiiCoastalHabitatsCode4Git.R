#Hawaii Coastal Habitats Code
#Mari K Reeves
#18_06_21_SummarizeCoastalHabitatsForHSAs

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
ReportOut<-paste0(BaseDir, "/HSAReports/18_06_21_Coastal")
setwd(ReportOut)

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
write.csv(coastaldata, file = "coastal_rawdata.csv")

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
names(coastalqual)<-c("Habitat Quality","Island" ,"Coastline Length (km)", "Length (% of State Coastline)"  ,    
                      "Length (% of Island Coastline" )
write.csv(coastalqual, file = "CoastalHabitatQuality.csv")


#Reorder Islands from East to west in figures 
levels(coastalqual$Island)
levels(coastalqual$Island)
coastalqual$Island<-as.character(coastalqual$Island)#get rid of pre-lehua factor levels
coastalqual$Island  <- ordered(coastalqual$Island ,levels = c( "Hawaii"  , "Maui", "Kahoolawe",  "Lanai",  "Molokai", "Oahu" , "Kauai" , "Niihau" )) 
coastalqual$Island<-as.factor(coastalqual$Island)
levels(coastalqual$Island)#check that worked


#load color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
#Make a figure for coastal habitat quality by Island
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
  labs(fill = "Habitat Quality", title = plottitle, x = "Island", y = "Length (Proportion of Total Island Coastline)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CoastalHabitatQualitySummary.jpg", plot=pretty, device = "jpg" )


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
names(clowndata)<-c("Land Ownership","Island" ,"Coastline Length (km)", "Length (% of State Coastline)"  ,    
                    "Length (% of Island Coastline" )

#capitalize "data" in the land ownership column
clowndata$`Land Ownership`<-as.character(clowndata$`Land Ownership`)
clowndata$`Land Ownership`<-sub("No data", "No Data", clowndata$`Land Ownership`)

write.csv(clowndata, file = "CoastalLandOwnership.csv")

#Reorder Islands from East to west in figures 
levels(clowndata$Island)
levels(clowndata$Island)
clowndata$Island<-as.character(clowndata$Island)#get rid of pre-lehua factor levels
clowndata$Island  <- ordered(clowndata$Island ,levels = c( "Hawaii"  , "Maui", "Kahoolawe",  "Lanai",  "Molokai", "Oahu" , "Kauai" , "Niihau" )) 
clowndata$Island<-as.factor(clowndata$Island)
levels(clowndata$Island)#check that worked

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
clown<-ggplot(clowndata, aes(clowndata$Island, clowndata$`Coastline Length (km)`, fill = clowndata$`Land Ownership`))
plottitle<-"Coastal Land Ownership Summary"
prettyclown<-clown+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Land Ownership", title = plottitle, x = "Island", y = "Length (Proportion of Island Coastline)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CoastalLandOwnershipSummary.jpg", plot=prettyclown, device = "jpg" )

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

creserves<-creserves[,c("Reserve Type","Island" ,"Coastline Length (km)", "perctotallength" ,     
                        "percentIslandLength")]  
names(creserves)<-c("Reserve Type","Island" ,"Coastline Length in Reserve (km)", "Length (% of State Coastline)"  ,    
                    "Length (% of Island Coastline" )


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

#Reorder Islands from East to west in figures 
levels(creserves$Island)
creserves$Island<-as.character(creserves$Island)#get rid of pre-lehua factor levels
creserves$Island  <- ordered(creserves$Island ,levels = c( "Hawaii"  , "Maui", "Kahoolawe",  "Lanai",  "Molokai", "Oahu" , "Kauai" , "Niihau" )) 
creserves$Island<-as.factor(creserves$Island)
levels(creserves$Island)#check that worked
# 
cres<-ggplot(creserves, aes(creserves$Island, creserves$`Coastline Length in Reserve (km)` , fill = creserves$`Reserve Type`))
plottitle<-"Coastal Reserve Type Summary"
prettycr<-cres+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (Proportion of Habitat in Reserves)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CoastalReserveTypeSummary.jpg", plot=prettycr, device = "jpg" )

creserves$`Length (% of Island Coastline` [creserves$`Length (% of Island Coastline`  <1]<-"<1"
creserves$`Length (% of State Coastline)`  [creserves$`Length (% of State Coastline)` <1]<-"<1"
write.csv(creserves, file = "CoastalReserveTypesStatewide.csv")

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
names(ch)<-c("Island" ,"Length of Coastline Designated Critical Habitat (km)", "Length (% of State Coastline)"  ,    
             "Length (% of Island Coastline" )
write.csv(ch, file = "CriticalHabitatSummary.csv")

#Make a figure for coastal Reserve Type by Island
#Reorder Islands from East to west in figures 
levels(ch$Island)
levels(ch$Island)
ch$Island<-as.character(ch$Island)#get rid of pre-lehua factor levels
ch$Island  <- ordered(ch$Island ,levels = c( "Hawaii"  , "Maui", "Kahoolawe",  "Lanai",  "Molokai", "Oahu" , "Kauai" , "Niihau" )) 
ch$Island<-as.factor(ch$Island)
levels(ch$Island)#check that worked


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
chplot<-ggplot(ch, aes(ch$Island, ch$`Length of Coastline Designated Critical Habitat (km)` ))
plottitle<-"Coastal Critical Habitat Summary"
prettych<-chplot+geom_bar(stat = "identity",  position = "stack")+
  labs( title = plottitle, x = "Island", y = "Length (km)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="CoastalCriticalHabitatSummary.jpg", plot=prettych, device = "jpg" )


save.image("~/HSAs/HSAReports/18_06_21_Coastal/18_06_21_EndOfCodeWorkspace.RData")

