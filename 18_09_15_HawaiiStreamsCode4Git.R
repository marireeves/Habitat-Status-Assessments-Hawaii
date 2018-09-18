# Hawaii Streams Code
#Mari K Reeves
#18_06_14_SummarizeStreamHabitatsForHSAs

#This code will intersect the streams with Land Ownership, Habitat Quality, Reserve Areas and Critical Habitats
#and generate sumamary tables and figures for the biologists doing the Habitat Status Assessment Reports. 

#It's not done yet because I'm figuring out which layer to use for streams. 


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
ReportOut<-paste0(BaseDir, "/HSAReports/18_06_14_StreamsFiguresTables")
setwd(ReportOut)


#Set CRS for session
BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
sysCRS<-"+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

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

crithab<-readOGR(dsn = OutDir, layer = "CriticalHabitatsysCRSOnePoly")
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
# #wetowners<-function(wet in wetplaces){
# for (wet in wetplaces[1]){
#   #get rid of the shp extension
#  lyr <- sub(".shp", "", wet)#This gets rid of the .shp extension in the list of files
streams<-readOGR(dsn = WetDir, layer = "18_05_24_sysCRSstate_darstreams")#read in the shpfile
streamsdata<-streams@data
plot(streams)
write.csv(streamsdata, file = "streams.csv")

streams$length_m<-gLength(streams, byid = T)
streamsmeters<-sum(streams$length_m)
streams$length_km<-streams$length_m/1000

habqual<-raster("C:/Users/marireeves/Documents/HSAs/SHADataFiles/CarbonAssessment2017VegLayers/CAH_HabStatus/CAH_HabStatus.jpg")
proj4string(habqual)
proj4string(streams)
plot(habqual)
tic()
streamsqualprice<-raster::extract(habqual, streams)
#make a copy of extracted data to work with:
streamqualqualintersect<-streamsqualprice

# # Get class counts for each polygon
stream.counts <- lapply(streamqualqualintersect,table)
# 
# # Calculate class percentages for each polygon
stream.pct <- lapply(stream.counts, FUN=function(x){ x / sum(x) } ) 
# 
# # Create a data.frame where missing classes are NA

class.df<-plyr::ldply(stream.pct, rbind)

# # Replace NA's with 0 and add names
class.df[is.na(class.df)] <- 0   
names(class.df) <- paste("class", names(class.df),sep="")

# # Add back to polygon data
streamdata<-streams@data
streamdata <- cbind(streamdata, class.df)
head(streamdata)
write.csv(streamdata, file = "streamhabqualraw.csv")

load("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_06_StreamWorkspace.RData")

toc()



#create a column in streams for % of stream length on island
islandstreamlength<-aggregate(streams$length_km~streams$IslandName , FUN = sum)
names(streams)
names(islandstreamlength)<-c("IslandName", "IslandStreamLengthkm")
write.csv(islandstreamlength, file = "islandstreamlength.csv")
streams<-merge(streams, islandstreamlength, by = "IslandName")
#check that worked
streamsdata<-streams@data


streamskm<-sum(streams$length_km)

streamstype<-aggregate(streamsdata$length_km, by = list(streamsdata$TYPE, streamsdata$IslandName, streamsdata$IslandStreamLengthkm), FUN = sum)
names(streamstype)<-c("Stream Type", "Island","Total Stream Length on Island" ,"Stream Length (km)")
streamstype$`Stream Length (km)` <-round(streamstype$`Stream Length (km)` , digits = 0)
streamstype$`Total Stream Length on Island`  <-round(streamstype$`Total Stream Length on Island` , digits = 0)
streamstype$perctotallength<-round(100*(streamstype$`Stream Length (km)` /streamskm), digits = 0)
streamstype$percentIslandLength<-round(100*(streamstype$`Stream Length (km)` /streamstype$`Total Stream Length on Island`   ), digits = 0)


#Make a figure for streams Stream Type by Island

#load color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#reorder Stream Type Variables So they Plot in more intuitive colors
#And lump some things
str(streamstype)
levels(streamstype$`Stream Type`)
streamstype$`Stream Type`<-as.character(streamstype$`Stream Type`)
streamstype$`Stream Type`[streamstype$`Stream Type`==  "L. BANK"]<-"BANK"
streamstype$`Stream Type`[streamstype$`Stream Type`==  "LEFT BANK"]<-"BANK"
streamstype$`Stream Type`[streamstype$`Stream Type`==  "RIGHT BANK"]<-"BANK"

streamstype$`Stream Type`[streamstype$`Stream Type`==  "LAKE"]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
streamstype$`Stream Type`[streamstype$`Stream Type`==  "POND"]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
streamstype$`Stream Type`[streamstype$`Stream Type`==   "POOL" ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
streamstype$`Stream Type`[streamstype$`Stream Type`==    "RESERVOIR"  ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
streamstype$`Stream Type`[streamstype$`Stream Type`==   "LAKE OR POND"  ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
streamstype$`Stream Type`[streamstype$`Stream Type`==   "DAM OR WEIR"   ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"

streamstype$`Stream Type`[streamstype$`Stream Type`==    "ROCK"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"
streamstype$`Stream Type`[streamstype$`Stream Type`==     "ISLAND"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"
streamstype$`Stream Type`[streamstype$`Stream Type`==     "MISC"    ]<- "ISLAND, ISLET, ROCK, OR OTHER"
streamstype$`Stream Type`[streamstype$`Stream Type`==     "ISLET"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"


streamstype$`Stream Type`<-as.factor(streamstype$`Stream Type`)
levels(streamstype$`Stream Type`)

#sizes <- ordered(sizes, levels = c("small", "medium", "large"))
streamstype$`Stream Type` <- ordered(streamstype$`Stream Type`,levels = c( "INTERMITTENT" ,"PERENNIAL",  "NON-PERENNIAL",
                                                                           "ISLAND, ISLET, ROCK, OR OTHER","BANK",   
                                                                           "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"  ))      
#check this worked                                   

levels(streamstype$`Stream Type`)

st1<-ggplot(streamstype, aes(streamstype$Island, streamstype$`Stream Length (km)` , fill = streamstype$`Stream Type`))
plottitle<-"Stream Type Summary"
prettyst1<-st1+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Stream Type", title = plottitle, x = "Island", y = "Length (Proportion of Total Stream Length on Island)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="StreamsTypeSummary.jpg", plot=prettyst1, device = "jpg" )


streamstype$percentIslandLength[streamstype$percentIslandLength <1]<-"<1"
streamstype$perctotallength[streamstype$perctotallength <1]<-"<1"

names(streamstype)<-c("Stream Type","Island" ,"Total Stream Length on Island (km)", "Length in Stream Type (km)"  ,    
                      "Length (% of Total Statewide Stream Length)", "Length (% of Total Stream Length on Island)" )
write.csv(streamstype, file = "streamtypesummary.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~Summarize Habitat Quality for Different Stream Lengths Based on USGS Veg layers~~~~~~~~~~~~~~~~~~~~~

#start with streamdata, not streamsdata. confusing I know, sorry. I lack creativity in naming objects sometimes. 

#make a list of these data tables to process

#define columns to keep in each layer
names(streamdata)

#create a column in streams for % of stream length on island

streamdata<-merge(streamdata, islandstreamlength, by = "IslandName")
names(streamdata)

moneyshot<-c("IslandName", "IslandStreamLengthkm.x" ,  "TYPE","length_km", "class1","class2","class3","class4")



w<-streamdata[ ,moneyshot]
#  drop unwanted variables


#rename the "class" columns and calculate acreages in each category per polygon
# Class descriptions from the metadata: If Habqual was already distrubed (category = 1), 
# then it was NEVER overwritten as bare earth; instead it remained classified as disturbed. 
# Lastly, the TIGER roads layer was buffered and converted into a raster of category 1 (distrubed). 
# The roads raster was then mosaic'ed on top of Habqual to expand the distrubed class to include roads
# adjacent disturbed areas.This layer has four mapped values: 1 = heavily disturbed areas including agriculture and urban developments; 
# 2 = mixed native-alien dominated plant communities; 3 = native dominated vegetation; 
# and 4 = bare lands or &lt;5% plant cover.ReferencesARIS B.V. 2014, GRID Editor for ArcMap. ARIS B.V., Netherlands.

w$KmDisturbed<-round(w$class1*w$length_km, digits = 5)
w$KmBarren<-round(w$class4*w$length_km, digits = 5)
w$KmNative<-round(w$class3*w$length_km, digits = 5)
w$KmNonNative<-round(w$class2*w$length_km, digits = 5)

#Drop the proportional class variables and reorganize columns
w<-w[,c( "IslandName", "TYPE"  , "length_km" ,"IslandStreamLengthkm.x",  "KmDisturbed","KmBarren",
         "KmNative","KmNonNative"     )]


#give better names

names(w)<-c("Island","Stream Type", "Length (km)","Total Stream Length on Island (km)", "Length Disturbed (km)", 
            "Length Barren (km)", "Length Native Dominated (km)", "Length Non-Native Dominated (km)")




#save the raw data

write.csv(w, file = "StreamHabitatQualityRaw.csv")


#reorder Stream Type Variables So they Plot in more intuitive colors
#And lump some things
str(w)
levels(w$`Stream Type`)
w$`Stream Type`<-as.character(w$`Stream Type`)
w$`Stream Type`[w$`Stream Type`==  "L. BANK"]<-"BANK"
w$`Stream Type`[w$`Stream Type`==  "LEFT BANK"]<-"BANK"
w$`Stream Type`[w$`Stream Type`==  "RIGHT BANK"]<-"BANK"

w$`Stream Type`[w$`Stream Type`==  "LAKE"]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
w$`Stream Type`[w$`Stream Type`==  "POND"]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
w$`Stream Type`[w$`Stream Type`==   "POOL" ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
w$`Stream Type`[w$`Stream Type`==    "RESERVOIR"  ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
w$`Stream Type`[w$`Stream Type`==   "LAKE OR POND"  ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
w$`Stream Type`[w$`Stream Type`==   "DAM OR WEIR"   ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"

w$`Stream Type`[w$`Stream Type`==    "ROCK"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"
w$`Stream Type`[w$`Stream Type`==     "ISLAND"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"
w$`Stream Type`[w$`Stream Type`==     "MISC"    ]<- "ISLAND, ISLET, ROCK, OR OTHER"
w$`Stream Type`[w$`Stream Type`==     "ISLET"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"


w$`Stream Type`<-as.factor(w$`Stream Type`)
levels(w$`Stream Type`)

#sizes <- ordered(sizes, levels = c("small", "medium", "large"))
w$`Stream Type` <- ordered(w$`Stream Type`,levels = c( "INTERMITTENT" ,"PERENNIAL",  "NON-PERENNIAL",
                                                       "ISLAND, ISLET, ROCK, OR OTHER","BANK",   
                                                       "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"  ))      
#check this worked                                   

levels(w$`Stream Type`)

u<-aggregate(list(w$`Length Barren (km)`, w$`Length Disturbed (km)`, w$`Length Native Dominated (km)`, w$`Length Non-Native Dominated (km)`   ), by = list(w$Island, w$`Stream Type` , w$`Total Stream Length on Island (km)`  ), FUN = sum)
names(u)<-c("Island","Stream Type","Total Stream Length on Island (km)","Length Barren (km)", "Length Disturbed (km)", 
            "Length Native Dominated (km)", "Length Non-Native Dominated (km)")

u$`Total Stream Length on Island (km)`<-round(u$`Total Stream Length on Island (km)`, digits = 1)

u$`Length Barren (km)` <-round(u$`Length Barren (km)`, digits = 1)
u$`Length Disturbed (km)` <- round(u$`Length Disturbed (km)` , digits = 1)
u$`Length Native Dominated (km)` <-round(u$`Length Native Dominated (km)`, digits = 1)
u$`Length Non-Native Dominated (km)`<-round(u$`Length Non-Native Dominated (km)`, digits = 1)

u$propbarren<-round((u$`Length Barren (km)` /u$`Total Stream Length on Island (km)`  )*100, digits = 1)
u$propnative<-round((u$`Length Native Dominated (km)` /u$`Total Stream Length on Island (km)` )*100, digits = 1)
u$propnonnative<-round((u$`Length Non-Native Dominated (km)` /u$`Total Stream Length on Island (km)` )*100, digits = 1)
u$propdisturbed<-round((u$`Length Disturbed (km)`/u$`Total Stream Length on Island (km)` )*100, digits = 1)




#rename variables
names(u)<-c("Island","Stream Type","Total Stream Length on Island (km)",
            "Length Barren (km)","Length Disturbed (km)","Length Native Dominated (km)",
            "Length Non-Native Dominated (km)","Barren (% of Total Stream Length on Island)","Native (% of Total Stream Length on Island)" ,                       
            "Non-Native (% of Total Stream Length on Island)","Disturbed (% of Total Stream Length on Island)"   ) 

#Create a couple tables while these are numeric to use later
lengths<-u[,c( "Island","Stream Type","Total Stream Length on Island (km)",
               "Length Barren (km)","Length Disturbed (km)","Length Native Dominated (km)",
               "Length Non-Native Dominated (km)" )]

percentages<-u[,c( "Island","Stream Type","Total Stream Length on Island (km)",
                   "Barren (% of Total Stream Length on Island)","Native (% of Total Stream Length on Island)" ,                       
                   "Non-Native (% of Total Stream Length on Island)","Disturbed (% of Total Stream Length on Island)"  )]    


lengthlong<-gather(lengths, key = "Descriptor", value = "Length (km)", 4:7)
lengthlong$HabitatQuality<-sub("Length ", "", lengthlong$Descriptor)
lengthlong$HabitatQuality<-sub("\\(km)", "", lengthlong$HabitatQuality)
lengthlong <- within(lengthlong, rm(Descriptor))

pctlong<-gather(percentages, key = "Descriptor", value = "Percent of Total Stream Length on Island", 4:7)
pctlong$HabitatQuality<-sub("\\(% of Total Stream Length on Island)", "", pctlong$Descriptor)
pctlong <-within(pctlong, rm(Descriptor))


#reorder Habitat Quality Variables So they Plot in more intuitive colors
str(lengthlong)
lengthlong$HabitatQuality<-as.factor(lengthlong$HabitatQuality)
levels(lengthlong$HabitatQuality)
lengthlong$HabitatQuality <- ordered(lengthlong$HabitatQuality,levels = c("Disturbed ","Barren ","Non-Native Dominated ","Native Dominated "))
#check this worked
levels(lengthlong$HabitatQuality)   

#now we can make pretty


st2<-ggplot(lengthlong, aes(lengthlong$Island, lengthlong$`Length (km)`  ,  fill = lengthlong$HabitatQuality))
prettyst2<-st2+geom_bar(stat = "identity",  position = "stack")+
  labs(fill = "Habitat Quality", title = "Stream Habitat Quality Summary", x = "Island", y = "Length (km)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)


ggsave(filename= "StreamHabQualLengths.jpg", plot=prettyst2, device = "jpg" )


prettyst3<-st2+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Habitat Quality", title = "Stream Habitat Quality Summary", x = "Island", y = "Length (Proportion of Total Stream Length on Island)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)


ggsave(filename= "StreamHabQualPercents.jpg", plot=prettyst3, device = "jpg" )


u$`Length Barren (km)` [ u$`Length Barren (km)` <1]<-"<1"
u$`Length Disturbed (km)` [ u$`Length Disturbed (km)` <1]<-"<1"
u$`Length Native Dominated (km)` [ u$`Length Native Dominated (km)`<1]<-"<1"
u$`Length Non-Native Dominated (km)` [u$`Length Non-Native Dominated (km)` <1]<-"<1"

u$`Barren (% of Total Stream Length on Island)`  [ u$`Barren (% of Total Stream Length on Island)`<1]<-"<1"
u$`Disturbed (% of Total Stream Length on Island)`  [ u$`Disturbed (% of Total Stream Length on Island)` <1]<-"<1"
u$`Native (% of Total Stream Length on Island)`  [u$`Native (% of Total Stream Length on Island)`  <1]<-"<1"
u$`Non-Native (% of Total Stream Length on Island)` [ u$`Non-Native (% of Total Stream Length on Island)` <1]<-"<1"

#write file to drive

write.csv(u, file = "StreamHabQualSummary.csv")












#post extraction pre-processing save

save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_07_PostIntersectStreamsData.RData")


#summarize land ownership for streams habitats
load ("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_07_StreamWorkspaceEndofCode.RData")


intersect1<-raster::intersect(streams, ownBuffer )


intersect1$length_m<-gLength(intersect1, byid = T)
interdata<-intersect1@data
names(interdata)
streamsownqual<-intersect1[c( "BigstOwner","Island" ,  "IslandStreamLengthkm", "length_m"    )]
streamsowndata<-streamsownqual@data
streamsowndata$length_km<-streamsowndata$length_m/1000

clowndata<-aggregate(streamsowndata$length_km, by = list(streamsowndata$BigstOwner, streamsowndata$Island, streamsowndata$IslandStreamLengthkm), FUN = sum)
names(clowndata)<-c("Land Owner", "Island"," IslandLength" ,"Stream Length (km)")
clowndata$`Stream Length (km)`<-round(clowndata$`Stream Length (km)`, digits = 0)
clowndata$perctotallength<-round(100*(clowndata$`Stream Length (km)`/streamskm), digits = 0)
clowndata$percentIslandLength<-round(100*(clowndata$`Stream Length (km)`/clowndata$` IslandLength`  ), digits = 0)



names(clowndata)<-c("Land Ownership","Island" ,"Total Stream Length on Island (km)","Stream Length (km)", "Length (% of Total State Stream)"  ,    
                    "Length (% of Total Island Stream" )
#capitalize "data" in the land ownership column
clowndata$`Land Ownership`<-as.factor(clowndata$`Land Ownership`)
levels(clowndata$`Land Ownership`)
clowndata$`Land Ownership`<-as.character(clowndata$`Land Ownership`)
clowndata$`Land Ownership`<-sub("No data", "No Data", clowndata$`Land Ownership`)

#change State DHHL to State
clowndata$`Land Ownership`<-sub("Govt. State DHHL",  "Govt. State", clowndata$`Land Ownership`)

clowndata$`Land Ownership`<-as.factor(clowndata$`Land Ownership`)
levels(clowndata$`Land Ownership`)
#Make a figure for streams habitat quality by Island

#load color blind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

# # #reorder Land Ownership Variables private plots in grey
# str(clowndata)
# clowndata$`Land Ownership`<-as.factor(clowndata$`Land Ownership`)
# levels(clowndata$`Land Ownership`)
# 
# #sizes <- ordered(sizes, levels = c("small", "medium", "large"))
clowndata$`Land Ownership` <- ordered(clowndata$`Land Ownership`,levels = c(  "Private","Govt. Federal","Govt. Local","Govt. State","No Data"))
#check this worked
levels(clowndata$`Land Ownership`)
# 
st3<-ggplot(clowndata, aes(clowndata$Island, clowndata$`Stream Length (km)`, fill = clowndata$`Land Ownership`))
plottitle<-"Streams Land Ownership Summary"
prettyst3<-st3+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Land Ownership", title = plottitle, x = "Island", y = "Length (Proportion of Total Island Stream Length)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="streamsLandOwnershipSummary.jpg", plot=prettyst3, device = "jpg" )

prettyst4<-st3+geom_bar(stat = "identity",  position = "stack")+
  labs(fill = "Land Ownership", title = plottitle, x = "Island", y = "Length (km)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="streamsLandOwnershipSummaryKm.jpg", plot=prettyst4, device = "jpg" )


clowndata$`Total Stream Length on Island (km)`<-round(clowndata$`Total Stream Length on Island (km)`, digits = 0)
clowndata$`Stream Length (km)` [clowndata$`Stream Length (km)` <1]<-"<1"
clowndata$`Length (% of Total Island Stream`  [clowndata$`Length (% of Total Island Stream`   <1]<-"<1"
clowndata$`Length (% of Total State Stream)`[clowndata$`Length (% of Total State Stream)`  <1]<-"<1"

#fix a couple name typos

names(clowndata)<- c("Land Ownership","Island","Total Stream Length on Island (km)",
                     "Stream Length in Land Ownership Category (km)","Length (% of Total State Stream Length)","Length (% of Total Island Stream Length)"  )

write.csv(clowndata, file = "StreamLandOwnershipSummary.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Summarize Reserve Status for streams Habitats

intersect2<-raster::intersect(streams, myreserves)
intersect2$length_m<-gLength(intersect2, byid = T)
inter2data<-intersect2@data

names(inter2data)

streamsreserves<-intersect2[c(  "Type_Defin"  ,"Island" ,  "IslandStreamLengthkm", "length_m"    )]
streamsresdata<-streamsreserves@data
streamsresdata$length_km<-streamsresdata$length_m/1000


creserves<-aggregate(streamsresdata$length_km, by = list(streamsresdata$Type_Defin  , streamsresdata$Island, streamsresdata$IslandStreamLengthkm), FUN = sum)
names(creserves)<-c("Reserve Type", "Island"," IslandLength" ,"Stream Length (km)")
creserves$`Stream Length (km)`<-round(creserves$`Stream Length (km)`, digits = 0)
creserves$perctotallength<-round(100*(creserves$`Stream Length (km)`/streamskm), digits = 0)
creserves$percentIslandLength<-round(100*(creserves$`Stream Length (km)`/creserves$` IslandLength`  ), digits = 0)


names(creserves)<-c("Reserve Type","Island" ,"Total Stream Length on Island (km)", "Stream Length in Reserve (km)",
                    "Length (% of Total State Stream Length)"  ,    
                    "Length (% of Total Island Stream Length)" )


#Make a figure for streams Reserve Type by Island


# #reorder Reserve Type Variables So they Plot in more intuitive colors
str(creserves)
creserves$`Reserve Type`<-as.factor(creserves$`Reserve Type`)
levels(creserves$`Reserve Type`)
#convert to a character to drop levels we screened out earlier then back to a factor
creserves$`Reserve Type`<-as.character(creserves$`Reserve Type`)
creserves$`Reserve Type`<-as.factor(creserves$`Reserve Type`)
levels(creserves$`Reserve Type`)
#sizes <- ordered(sizes, levels = c("small", "medium", "large"))
# creserves$`Reserve Type` <- ordered(creserves$`Reserve Type`,levels = c("Poor Quality","Moderate Quality","High Quality","Very High Quality"))
# #check this worked
# levels(creserves$`Reserve Type`)
# 
st4<-ggplot(creserves, aes(creserves$Island, creserves$`Stream Length in Reserve (km)`, fill = creserves$`Reserve Type` ))
plottitle<-"Streams Reserve Type Summary"
prettyst5<-st4+geom_bar(stat = "identity",  position = "stack")+
  labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (km)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="streamsReserveTypeSummaryKm.jpg", plot=prettyst5, device = "jpg" )

prettyst6<-st4+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (Proportion of Total Island Stream Length in Reserves)"  ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="streamsReserveTypeSummaryPercent.jpg", plot=prettyst6, device = "jpg" )


creserves$`Total Stream Length on Island (km)`<-round(creserves$`Total Stream Length on Island (km)`, digits=0)
creserves$`Total Stream Length on Island (km)`[ creserves$`Total Stream Length on Island (km)`  <1]<-"<1"
creserves$`Length (% of Total State Stream Length)`  [ creserves$`Length (% of Total State Stream Length)` <1]<-"<1"
creserves$`Length (% of Total Island Stream Length)`  [ creserves$`Length (% of Total Island Stream Length)`  <1]<-"<1"
creserves$`Stream Length in Reserve (km)` [creserves$`Stream Length in Reserve (km)` <1]<-"<1"


write.csv(creserves, file = "StreamReserveStatusSummary.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
#Summarize Critical Habitat for streams Habitats
intersect3<-raster::intersect(streams, crithab)
inter3data<-intersect3@data

intersect3$length_m<-gLength(intersect3, byid = T)
inter3data<-intersect3@data

names(inter3data)

streamsCH<-intersect3[c(  "IslandName" , "TYPE" , "IslandStreamLengthkm", "length_m"    )]
streamsCHdata<-streamsCH@data
streamsCHdata$length_km<-streamsCHdata$length_m/1000


ch<-aggregate(streamsCHdata$length_km, by = list(streamsCHdata$IslandName, streamsCHdata$TYPE, streamsCHdata$IslandStreamLengthkm), FUN = sum)
names(ch)<-c("Island", "Stream Type", "IslandLength" ,"Stream Length (km)")
ch$`Stream Length (km)`<-round(ch$`Stream Length (km)`, digits = 0)
ch$perctotallength<-round(100*(ch$`Stream Length (km)`/streamskm), digits = 0)
ch$percentIslandLength<-round(100*(ch$`Stream Length (km)`/ch$`IslandLength`  ), digits = 0)


names(ch)<-c("Island" , "Stream Type"  ,"Total Stream Length on Island (km)", "Length of Stream Designated Critical Habitat (km)", 
             "Length (% of Total State Stream Length)"  ,    
             "Length (% of Total Island Stream Length)" )



#reorder Stream Type Variables So they Plot in more intuitive colors
#And lump some things
str(ch)
levels(ch$`Stream Type`)
ch$`Stream Type`<-as.character(ch$`Stream Type`)
ch$`Stream Type`[ch$`Stream Type`==  "L. BANK"]<-"BANK"
ch$`Stream Type`[ch$`Stream Type`==  "LEFT BANK"]<-"BANK"
ch$`Stream Type`[ch$`Stream Type`==  "RIGHT BANK"]<-"BANK"

ch$`Stream Type`[ch$`Stream Type`==  "LAKE"]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
ch$`Stream Type`[ch$`Stream Type`==  "POND"]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
ch$`Stream Type`[ch$`Stream Type`==   "POOL" ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
ch$`Stream Type`[ch$`Stream Type`==    "RESERVOIR"  ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
ch$`Stream Type`[ch$`Stream Type`==   "LAKE OR POND"  ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"
ch$`Stream Type`[ch$`Stream Type`==   "DAM OR WEIR"   ]<- "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"

ch$`Stream Type`[ch$`Stream Type`==    "ROCK"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"
ch$`Stream Type`[ch$`Stream Type`==     "ISLAND"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"
ch$`Stream Type`[ch$`Stream Type`==     "MISC"    ]<- "ISLAND, ISLET, ROCK, OR OTHER"
ch$`Stream Type`[ch$`Stream Type`==     "ISLET"   ]<- "ISLAND, ISLET, ROCK, OR OTHER"


ch$`Stream Type`<-as.factor(ch$`Stream Type`)
levels(ch$`Stream Type`)

#sizes <- ordered(sizes, levels = c("small", "medium", "large"))
ch$`Stream Type` <- ordered(ch$`Stream Type`,levels = c( "INTERMITTENT" ,"PERENNIAL",  "NON-PERENNIAL",
                                                         "ISLAND, ISLET, ROCK, OR OTHER","BANK",   
                                                         "LAKE, POND, POOL, RESERVOIR, DAM OR WEIR"  ))      
#check this worked                                   

levels(w$`Stream Type`)
# 
st5<-ggplot(ch, aes(ch$Island, ch$`Length of Stream Designated Critical Habitat (km)` , fill = ch$`Stream Type`))
plottitle<-"Streams Critical Habitat Summary"
prettyst7<-st5+geom_bar(stat = "identity",  position = "stack")+
  labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (km)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="streamsCritHabSummarykm.jpg", plot=prettyst7, device = "jpg" )


prettyst8<-st5+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Reserve Type", title = plottitle, x = "Island", y = "Length (Proportion of Total Island Stream Length in Critical Habitat)"  ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)
ggsave(filename="streamsCritHabSummaryPercent.jpg", plot=prettyst8, device = "jpg" )


ch$`Total Stream Length on Island (km)`<-round(ch$`Total Stream Length on Island (km)`, digits = 0)  

ch$`Total Stream Length on Island (km)`[ ch$`Total Stream Length on Island (km)` <1]<-"<1"
ch$`Length (% of Total Island Stream Length)` [ ch$`Length (% of Total Island Stream Length)` <1]<-"<1"
ch$`Length (% of Total State Stream Length)` [ ch$`Length (% of Total State Stream Length)` <1]<-"<1"
ch$`Length of Stream Designated Critical Habitat (km)`[ch$`Length of Stream Designated Critical Habitat (km)`<1]<-"<1"
write.csv(ch, file = "StreamsCriticalHabitatSummary.csv")

save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/18_06_14_StreamsWorkspaceEndofCode.RData")

