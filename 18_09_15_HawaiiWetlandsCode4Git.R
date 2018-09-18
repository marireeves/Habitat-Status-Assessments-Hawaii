#Hawaii Wetlands Code
#Mari K Reeves
#18_05_30_SummarizeWetlandHabitatsForHSAs

#This code will intersect the NWI and NOAA polygon layers with Land Ownership, Habitat Quality, Reserve Areas and Critical Habitats
#and generate sumamary tables and figures for the biologists doing the Habitat Status Assessment Reports. 

#The NOAA layer was a polygon file that I generated from the rasters for the wetlands project, but this stupid file was corrupted and 
#all the gridcode values were assigned to the wrong habitat and I don't understand why the raster to polygon function does this 
#sometimes, but I'm tired enough of the problem I am going back to the base rasters with this code. ARRGH!



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
CCAPDir<-paste0(ConservationDir, "/NOAACCAPRasters/noaa_ccap_rasters")
CCAPOut<-paste0(ConservationDir, "/NOAACCAPRasters")
ReportOut<-paste0(BaseDir, "/HSAReports/18_06_14_WetlandsFiguresTables")
setwd(ReportOut)

#Set CRS for session
BigIslandCRS<- "+proj=utm +zone=5 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
OtherIslandCRS<- "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs"
sysCRS<-"+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
#colorblind friendly palatte:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")

#Bring in files and transform them into the sysCRS - send them to a new folder - only need to run this once, so commented it out

neededshps<-list.files(CCAPOut, pattern = ".shp$")

#habitatsplit<-function(hab){
for (myshp in neededshps){
  #get rid of the shp extension
  lyr <- sub(".shp$", "", myshp)#This gets rid of the .shp extension in the list of files
  b<-readOGR(dsn = CCAPOut, layer = lyr)#read in the shpfile
  print(proj4string(b))#check CRS
  #transform it to the sysCRS
  g<-spTransform(b, CRS(sysCRS))
  print(proj4string(g))#recheck the spatial projections
  #send it back out to a different file
  #clip it to the coastline
  outname<-paste0("18_06_11_sysCRS",lyr)
  writeOGR(g, dsn = OutDir, layer = outname, driver="ESRI Shapefile", overwrite_layer = T)
}


#Bring these files back in for intersection operations. 
list.files(OutDir,  pattern = ".shp")
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

habqual<-raster("C:/Users/marireeves/Documents/HSAs/SHADataFiles/CarbonAssessment2017VegLayers/CAH_HabStatus/CAH_HabStatus.tif")

#write functions to intersect the streams, wetlands, and streams with reserves, critical habitat, and land ownership, calculate and 
#plot length in each status. I think I may just need to treat each of these guys separately and not do this in a loop. Because the 
#incoming datasets are so different. Bleh. 


# 
# 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Write a function to bring in each CCAP raster and summarize acres in each gridcode in the most recent layer
#This took forever and needed to do queries and convert to polygons for calcs in ArcGIS

# CCAPFiles<-list.files(CCAPDir, '\\.img$', full.names=T)
# 
# noaaDesc<-read.csv(paste0(WetDir, "/18_05_28_HSA_NOAA2KEEPDROP.csv"))
# noaaDrop<-noaaDesc[noaaDesc$KEEP_DROP == "DROP",]
# noaaKeep<-noaaDesc[noaaDesc$KEEP_DROP == "KEEP",]
# 
# noaaDesc$Value<-noaaDesc$NOAA_CAT
# 
# 
# for(filez in CCAPFiles){
#   z<-stack(filez)
#   print(proj4string(z))#check the projection
#   myres<-res(z)#get the resolution of z (m2 per pixel) for count to area conversion
#   sqmperpixel<-myres[1]*myres[2]#assign  m2 per pixel to a variable so we can use it for math
#   
#   y<-freq(z)#count pixels in each value category
#   y<-data.frame(y)#turn in into a data frame
#   names(y)<-c("value", "count")#give the columns names
#   
#   y$aream2<-y$count*sqmperpixel #convert counts to areas
#   y$acres<-y$aream2*0.0002471
#   names(y)<-c("Value" , "pixel_count" , "aream2", "acres")
#   
#   #bring in the NOAA crosswalk and join the values to the areas
#   j<-merge(y, noaaDesc, by = "Value")
#   #save this table to double check areas
#   jname<-sub("C:/Users/marireeves/Documents/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/NOAACCAPRasters/noaa_ccap_rasters/hi_", "", filez)
#   jname<-sub("_20*.*img", "CCAP.csv", jname)
#   write.csv(j, file=jname)
#   #Filter to keep only the wetlands
#   noaawet<-j[j$Value %in% noaaKeep$NOAA_CAT,]
#   noaawet$island<-sub("CCAP.csv", "", jname)
#   summaryname<-sub("CCAP.csv", "CCAP_summary.csv", jname)
#   write.csv(noaawet, file = summaryname)
#  
# }
# 
# 
# 
# wetlandsummaries<-list.files(CCAPOut, pattern = "summary.csv", full.names = T)
# 
# wetsummaries<-lapply(wetlandsummaries, read.csv, header = T)
# 
# 
# wetlandsummarynohawaii<-do.call(rbind, wetsummaries)
# names(wetlandsummarynohawaii)
# w.summary<-wetlandsummarynohawaii[,c( "pixel_count","acres","NOAA_CAT","DESCRIPTION", "island" )]
# w.summary$sqmi<-w.summary$acres*0.001563
# 
# sum(w.summary$sqmi)
# levels(w.summary$DESCRIPTION)
# 
# wet.no.openwater<-w.summary[w.summary$DESCRIPTION != "open water - hopefully all of these are wetlands" ,]
# wetarea<-sum(wet.no.openwater$sqmi)

# mycells<-z %in% noaaKeep$NOAA_CAT
# plot(mycells)
# wet <- mask(z, mycells, maskvalue=0, updatevalue=NA) 
# plot(wet)
# #the next line of code takes an unbelievably long time and tons of memory ugkh and then totally doesn't work
# q<-rasterToPolygons(wet, n=4, na.rm=TRUE, digits=12, dissolve=TRUE)
# plot(q)
# writeOGR(q, dsn = CCAPOut, layer = "niihauwetlands",  driver="ESRI Shapefile", overwrite_layer = T)
# 


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#bring in nwi and see if they map onto each other
nwi<-readOGR(dsn = WetDir, layer = "18_05_24_sysCRSHI_Wetlands"  )

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



write.csv(nwiwetdata, file = "18_05_30_nwiwet.csv")

#Calculate Areas and make an area statewide variable to use


nwiwet$Area_m<-gArea(nwiwet, byid = T)
nwiwetlandstotalarea<-gArea(nwiwet) #65,075,6448
nwiwetdata<-nwiwet@data

#Add in Island to table  
#bring in a coastlines shape for an over statement...
list.files(ConservationDir)

coast<-readOGR(dsn = ConservationDir, layer =  "state_coast")

#transform it into the sysCRS

print(proj4string(coast))#check CRS
#transform it to the sysCRS
coast<-spTransform(coast, CRS(sysCRS))
print(proj4string(coast))#recheck the spatial projections
#send it back out to a different file
writeOGR(coast, dsn = OutDir, layer = "coastsysCRS", driver="ESRI Shapefile", overwrite_layer = T)
#run intersection to assign island names to wetland tables. 

#Keep only islands of interest...The 2500m buffer makes it so smaller islands are included with Mains

#Get rid of Lehua and other islands we don't want to avoid double counting

levels(coast$Label)
islands2keep<-c(  "Big Island" ,"Kahoolawe", "Kauai","Lanai" ,"Maui","Molokai","Niihau","Oahu"     )
coast<-coast[coast$Label %in% islands2keep,]
coastdata<-coast@data
#change name Big Island to Hawaii
coast$island<-sub("Big Island", "Hawaii", coast$Label)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wetplaces<-list.files(OutDir, pattern = "18_06_11_sysCRS*.*.shp")
#Write a loop to bring in each island file

for (wet in wetplaces){
  #get rid of the shp extension
  lyr <- sub(".shp$", "", wet)#This gets rid of the .shp extension in the list of files
  b<-readOGR(dsn = OutDir, layer = lyr)#read in the shpfile
  
  #clip it to the coastline and #Assign an island name to the datafile
  clip <- raster::intersect(b, coast) 
  plot(clip)
  plot(coast, col = "red", add = T)
  #compute wetland areas by island
  clip$aream2<-gArea(clip, byid = T)
  clip<-clip[,c(2,3 ,13, 14)]
  names(clip@data)<-c("gridcode", "wetlandtype", "island", "Area_m")
  clipdata<-clip@data
  outname<-paste0(lyr, "coastlineclipped.shp")
  writeOGR(clip, dsn = OutDir, layer = outname, driver="ESRI Shapefile", overwrite_layer = T)
  
  #intersect it with land ownership and write data to file
  
  tic()
  typeownno<-try(raster::intersect(clip, ownBuffer))
  if(class(typeownno) %in% c('NULL', 'try-error')){
    next
  }else{
    typeownno$newarea_m<-gArea(typeownno, byid = T)
    typeownno$keyinfo<-typeownno$BigstOwner
    typeownnodata<-typeownno@data
    ownname<-paste0(lyr, "own.csv")
    write.csv(typeownnodata, file = ownname)
    
    toc()
  }
}


for (wetter in wetplaces){
  #get rid of the shp extension
  lyr <- sub(".shp$", "", wetter)#This gets rid of the .shp extension in the list of files
  b<-readOGR(dsn = OutDir, layer = lyr)#read in the shpfile
  
  #clip it to the coastline and #Assign an island name to the datafile
  clip <- raster::intersect(b, coast) 
  
  
  tic()
  intersectresno<-try(raster::intersect(clip, myreserves))
  if(class(intersectresno) %in% c('NULL', 'try-error')){
    next
  }else{
    intersectresno$newarea_m<-gArea(intersectresno, byid = T)
    intersectresnodata<-intersectresno@data
    intersectresno$keyinfo<-intersectresno$Type_Defin
    resname<-paste0(lyr, "reserves.csv")
    write.csv(intersectresnodata, file = resname)
  }
  toc()
  # 
}


for (wettest in wetplaces){
  #get rid of the shp extension
  lyr <- sub(".shp$", "", wettest)#This gets rid of the .shp extension in the list of files
  b<-readOGR(dsn = OutDir, layer = lyr)#read in the shpfile
  
  #clip it to the coastline and #Assign an island name to the datafile
  clip <- raster::intersect(b, coast) 
  
  # #intersect it with critical habitat
  # 
  tic()
  intersectchno<-try(raster::intersect(clip, crithab))
  if(class(intersectchno) %in% c('NULL', 'try-error')){
    next
  }else{
    intersectchno$newarea_m<-gArea(intersectchno, byid = T)
    intersectchnodata<-intersectchno@data
    intersectchno$keyinfo<-"Designated as Critical Habitat"
    chname<-paste0(lyr, "ch.csv")
    write.csv(intersectchnodata, file = chname)
    
    toc()
    
  }
}


for (wetwipe in wetplaces[6]){
  #get rid of the shp extension
  lyr <- sub(".shp$", "", wetwipe)#This gets rid of the .shp extension in the list of files
  b<-readOGR(dsn = OutDir, layer = lyr)#read in the shpfile
  
  #clip it to the coastline and #Assign an island name to the datafile
  clip <- raster::intersect(b, coast) 
  #intersect it with habitat quality and write data to file
  
  tic()
  intersectqualno<-raster::extract(habqual, clip )
  
  toc()
  
  noaaqualintersect<-intersectqualno
  
  
  # # Get class counts for each polygon
  noaa.counts <- lapply(noaaqualintersect,table)
  # 
  # # Calculate class percentages for each polygon
  noaa.pct <- lapply(noaa.counts, FUN=function(x){ x / sum(x) } ) 
  # 
  # # Create a data.frame where missing classes are NA
  
  class.df2<-plyr::ldply(noaa.pct, rbind)
  
  # # Replace NA's with 0 and add names
  class.df2[is.na(class.df2)] <- 0   
  names(class.df2) <- paste("class", names(class.df2),sep="")
  
  # # Add back to polygon data
  clip$aream2<-gArea(clip, byid = T)
  clipdata<-clip@data
  clipdata <- cbind(clipdata, class.df2)
  head(clipdata)
  qualname<-paste0(lyr, "qual.csv")
  write.csv(clipdata, file = qualname)
  
  
  
}


#end of looper
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Start the figure and tables

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Merge Critical Habitat Data into one file~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

list.files(CCAPOut)

CHTables<-list.files(paste0(CCAPOut, "/CritHabTables"), full.names = T)

chin<-lapply(CHTables, read.csv, header = F)

crithabpolydata<-do.call(rbind, chin)

crithaball<-crithabpolydata[,c(3,4,14,18)]

names(crithaball)<-c("gridcode","Class_Name","island","newarea_m")

#population[order(population$age),]
crithaball<-crithaball[order(crithaball$Class_Name),]

#remove those old title rows 
ch<-crithaball[crithaball$island != "island",]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Merge Ownership Data into one file

OwnTables<-list.files(paste0(CCAPOut, "/OwnershipTables"), full.names = T)

owin<-lapply(OwnTables, read.csv, header = T)

ownpolydata<-do.call(rbind, owin)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Merge Reserve Data into one file

ReserveTables<-list.files(paste0(CCAPOut, "/Reserves"), full.names = T)

resin<-lapply(ReserveTables, read.csv, header = T)

respolydata<-do.call(rbind, resin)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Merge Habitat Quality Data

QualTables<-list.files(paste0(CCAPOut, "/HabQualTables"), full.names = T)


#the columns ended up with the same names this time, but in the wrong order, so the solution we just did last time (removing the header row
#on import, would be a disaster, because it would mix up the results of the different classes. )


#Write a loop to reorder the columns

for (csv in QualTables){
  a<-read.csv(csv, header = T)
  col_order <- c("X","Id","gridcode","Class_Name" ,"OBJECTID","AREA","PERIMETER","Shape_Leng" ,"Shape_Area" ,"Island",    
                 "Label","Acres","Area_KM_2" , "island","aream2","class1","class2" , "class3" ,"class4" )
  a<-a[,col_order]
  write.csv(a, file = csv)
}

QualTables<-list.files(paste0(CCAPOut, "/HabQualTables"), full.names = T)

qualin<-lapply(QualTables, read.csv, header = T)

qualpolydata<-do.call(rbind, qualin)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Merge the clipped Shapefiles into a statewide file


noaaclippedDir<-"C:/Users/marireeves/Documents/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/CoastalWetlandsStreamsOut/CoastlineClippedFiles"
noaacliplist<-list.files(noaaclippedDir, pattern = ".shp.shp",
                         full.names = T)       

noaaget <- lapply(noaacliplist, readOGR)

BestNOAAEWetlandFileEver<-Reduce(union, noaaget)

plot(BestNOAAEWetlandFileEver)

writeOGR(BestNOAAEWetlandFileEver, dsn = OutDir, layer = "18_06_12_BestNOAAEWetlandFileEver", driver="ESRI Shapefile", overwrite_layer = T)
noaawet<-BestNOAAEWetlandFileEver

#Calculate Areas and make an area statewide variable to use
noaawet$Area_m<-gArea(noaawet, byid = T)
noaawetdata<-noaawet@data
noaawetlandstotalarea<-gArea(noaawet)# 161,763,248 meters


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
# 
# levels(coast$Label)
# islands2keep<-c(  "Big Island" ,"Kahoolawe", "Kauai","Lanai" ,"Maui","Molokai","Niihau","Oahu"     )
# coast<-coast[coast$Label %in% islands2keep,]
# coastdata<-coast@data
# #change name Big Island to Hawaii
# coast$island<-sub("Big Island", "Hawaii", coast$Label)
# 
# nwiisland<-raster::intersect(nwiwet, coast )
# plot(nwiisland)
# nwiislanddata<-nwiisland@data
# 
# noaaisland<-raster::intersect(noaawet, coast)
# noaaislanddata<-noaaisland@data
# #calculate Island Areas within main tables
# nwiwetlandareas<-aggregate(nwiisland$Area_m, by = list(nwiisland$island), FUN = sum)
# names(nwiisland@data)
# names(nwiwetlandareas)<-c("island", "wetlandareaonisland")
# nwiwetter<-merge(nwiisland, nwiwetlandareas)
# 
# #Fix names of scrub shrub to join with forested and get rid of dash
# nwiwetter$WetlandType<-sub("Freshwater Forested/Shrub Wetland", "Freshwater Forested and Scrub Shrub Wetland", nwiwetter$WETLAND_TY)
# 
# nwiwetter$WetlandType<-sub("Lake", "Open Water", nwiwetter$WetlandType)
# nwiwetter$WetlandType<-sub("Freshwater Pond", "Open Water", nwiwetter$WetlandType)
# 
# 
# levels(as.factor(nwiwetter$WetlandType))
# 
# #create a column for nwi to use later
# nwiwetter$dataset<-"nwi"

noaawetlandareas<-aggregate(noaawet$Area_m, by = list(noaawet$island), FUN = sum)

names(noaawet@data)
names(noaawetlandareas)<-c("island", "wetlandareaonisland")
noaawetter<-merge(noaawet, noaawetlandareas, by = "island")

#create a dataset column for noaa to use later
noaawetter$dataset<-"noaa"
#add gridcode descriptions and call it wetland type to match NWI and edit categories in 
# #so that these match across datasets
# levels(nwiwetter$WETLAND_TY)
# # Keep Open Water
# noaawetter$WetlandType<-sub("21", "Open Water", noaawetter$gridcode)
# # Keep Unconsolidated shore
# noaawetter$WetlandType<-sub("19", "Unconsolidated Shore", noaawetter$WetlandType)
# # Lump all estuarine wetlands and estuarine aquatic beds into Estuarine and Marine
# #Wetlands category
# noaawetter$WetlandType<-sub("23", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
# noaawetter$WetlandType<-sub("18", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
# noaawetter$WetlandType<-sub("17", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
# noaawetter$WetlandType<-sub("16", "Estuarine and Marine Wetland" , noaawetter$WetlandType)
# 
# 
# #Lump all the following into freshwater Forested and Shrub Scrub Wetlands,
# #gridcodes 13, 14, palustrine forested and scrub shrub
# 
# noaawetter$WetlandType<-sub("13", "Freshwater Forested and Scrub Shrub Wetland" , noaawetter$WetlandType)
# noaawetter$WetlandType<-sub("14", "Freshwater Forested and Scrub Shrub Wetland" , noaawetter$WetlandType)
# 
# # Lump palustrine emergent wetlands and palustrine aquatic beds to Freshwater
# #Emergent Wetland category
# noaawetter$WetlandType<-sub("15",  "Freshwater Emergent Wetland" , noaawetter$WetlandType)
# noaawetter$WetlandType<-sub("22",  "Freshwater Emergent Wetland" , noaawetter$WetlandType)
# 
# 
# levels(as.factor(noaawetter$WetlandType))

#aggregate these guys by variables we're interested in summarizing:
#Wetland Type, island, island area, WetlandType

# agnwi<-aggregate(nwiwetter$Area_m, by = list(nwiwetter$island, nwiwetter$WetlandType, nwiwetter$dataset, nwiwetter$wetlandareaonisland), FUN = sum)  
# names(agnwi)<-c("island", "wetlandtype", "dataset", "wetlandareaonisland", "Area_m")
names(noaawetter)<-c("island","gridcod","WetlandType","Area_m","wetlandareaonisland" ,"dataset"  )  
agno<-aggregate(noaawetter$Area_m, by = list(noaawetter$island, noaawetter$WetlandType, noaawetter$dataset, noaawetter$wetlandareaonisland), FUN = sum) 
names(agno)<-c("island", "wetlandtype", "dataset", "wetlandareaonisland", "Area_m")
#bind the aggregated files together so I only need to work with one data file for plots and tables

# wetlands<-rbind(agnwi, agno)

# #finally. One dataset that I can facet by dataset 
# 
# names(wetlands)
# wetlands$acres<-round(wetlands$Area_m*0.0002471, digits = 0)
# 
# #convert the wetland area on island column to acres and round it too
# wetlands$WetlandAcresOnIsle<-round(wetlands$wetlandareaonisland*0.0002471, digits = 0)
# #drop the m2 area columns
# wetlands<-wetlands[,c("island","wetlandtype","dataset", "acres","WetlandAcresOnIsle")]
# 
# wetlands$propwetlandsisle<-round((wetlands$acres/wetlands$WetlandAcresOnIsle)*100, digits = 0)
# wetlands$dataset<-sub("noaa", "NOAA - CCAP", wetlands$dataset)
# wetlands$dataset<-sub("nwi", "USFWS - NWI", wetlands$dataset)
# 
# #create a few comparison plots for the different datasets
# #acres of wetlands on each island
# str(wetlands)
# wetlands$wetlandtype<-as.factor(wetlands$wetlandtype)
# wetlands$island<-as.factor(wetlands$island)
# wetlands$dataset<-as.factor(wetlands$dataset)
# 
# a<-ggplot(wetlands, aes(wetlands$dataset, wetlands$acres, fill = wetlands$wetlandtype))
# datacompare<- a+geom_bar(stat="identity", position = "stack") + 
#   labs(fill = "Wetland Type", title = "Wetland Areas by Type - Comparing Datasets", x = "Wetlands Dataset", y = "Area (Acres)")+
#   scale_fill_manual(values=cbPalette)
# ggsave(filename="CompareWetlandAreaTypeData.tif", plot=datacompare, device = "tiff" )
# 
# 
# b<-ggplot(wetlands, aes(wetlands$island, wetlands$acres, fill = wetlands$dataset))
# wettypecompare<- b+geom_bar(stat="identity", position = "dodge") + 
#   labs(fill = "Wetlands Dataset", title = "Wetland Areas by Dataset - Comparing Islands", x = "Island", y = "Area (Acres)")+
#   scale_fill_manual(values=cbPalette)
# ggsave(filename="CompareWetlandDatasetsbyIsland.tif", plot=wettypecompare, device = "tiff" )

save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/NOAACCAPRasters/18_06_12_PostExtractionDataWetlands.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Run the Raster to Poly Extractions. these take 26 hours to run


load("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/NOAACCAPRasters/.RData")
#summarize the habitat quality data

ReportOut<-paste0(BaseDir, "/HSAReports/18_06_14_WetlandsFiguresTables")
setwd(ReportOut)


#define columns to keep in each layer
qual<-qualpolydata
qual<-merge(qual, noaawetlandareas, by = "island")
names(qual)

qual<-qual[,c("island","wetlandareaonisland", "Class_Name" , "aream2","class1","class2","class3","class4" )  ]
names(qual)<-c("island", "wetlandareaonisland","WetlandType", "Area_m", "class1","class2","class3","class4")
w<-qual


#convert the wetland area on island column to acres and round it too
w$WetlandAcresOnIsle<-round(w$wetlandareaonisland*0.0002471, digits = 0)
w$acres<-round(w$Area_m*0.0002471, digits = 2)

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

w<-w[,c("island","WetlandType","acres","WetlandAcresOnIsle", "propwetlandsisle", "AcresDisturbed", "AcresBarren", "AcresNative", "AcresNonNative")]
#give better names

names(w)<-c("Island","Wetland Type", "Area (Acres)","Total Wetland Area on Island (Acres)", "Area (% of Wetland Area on Island", "Area Disturbed (Acres)", "Area Barren (Acres)", "Area Native Dominated (Acres)", "Area Non-Native Dominated (Acres)")

#save the raw data

write.csv(w, file = "18_06_12_HabitatQualityWetlandsRaw.csv")

u<-aggregate(list(w$`Area Barren (Acres)`, w$`Area Disturbed (Acres)`, w$`Area Native Dominated (Acres)`, w$`Area Non-Native Dominated (Acres)`), by = list(w$Island, w$`Wetland Type`, w$`Total Wetland Area on Island (Acres)` ), FUN = sum)
names(u)<-c("Island","Wetland Type","Total Wetland Area on Island (Acres)", "Area Barren (Acres)", "Area Disturbed (Acres)","Area Native Dominated (Acres)", "Area Non-Native Dominated (Acres)")

u$`Area Barren (Acres)`<-round(u$`Area Barren (Acres)`, digits = 1)
u$`Area Native Dominated (Acres)`<-round(u$`Area Native Dominated (Acres)`, digits = 1)
u$`Area Non-Native Dominated (Acres)`<-round(u$`Area Non-Native Dominated (Acres)`, digits = 1)
u$`Area Disturbed (Acres)`<-round(u$`Area Disturbed (Acres)`, digits = 1)

u$propbarren<-round((u$`Area Barren (Acres)` /u$`Total Wetland Area on Island (Acres)`  )*100, digits = 1)
u$propnative<-round((u$`Area Native Dominated (Acres)`/u$`Total Wetland Area on Island (Acres)`)*100, digits = 1)
u$propnonnative<-round((u$`Area Non-Native Dominated (Acres)`/u$`Total Wetland Area on Island (Acres)`)*100, digits = 1)
u$propdisturbed<-round((u$`Area Disturbed (Acres)`/u$`Total Wetland Area on Island (Acres)`)*100, digits = 1)



#rename variables
names(u)<-c("Island","Wetland Type","Total Wetland Area on Island (Acres)",
            "Area Barren (Acres)","Area Disturbed (Acres)"  ,            
            "Area Native Dominated (Acres)","Area Non-Native Dominated (Acres)","Barren (% of Wetland Area on Island)"  ,                        
            "Native Dominated  (% of Wetland Area on Island)","Non-Native Dominataed (% of Wetland Area on Island)"  , "Disturbed (% of Wetland Area on Island)"   ) 

#Create a couple tables while these are numeric to use later
areas<-u[,c( "Island","Wetland Type", "Total Wetland Area on Island (Acres)","Area Barren (Acres)" ,"Area Disturbed (Acres)",
             "Area Native Dominated (Acres)","Area Non-Native Dominated (Acres)"      )]

percentages<-u[,c( "Island","Wetland Type", "Total Wetland Area on Island (Acres)", "Barren (% of Wetland Area on Island)"  ,
                   "Native Dominated  (% of Wetland Area on Island)","Non-Native Dominataed (% of Wetland Area on Island)",
                   "Disturbed (% of Wetland Area on Island)"    )]    


arealong<-gather(areas, key = "Descriptor", value = "Acres", 4:7)
arealong$HabitatQuality<-sub("Area ", "", arealong$Descriptor)
arealong$HabitatQuality<-sub("\\(Acres)", "", arealong$HabitatQuality)
arealong <- within(arealong, rm(Descriptor))

pctlong<-gather(percentages, key = "Descriptor", value = "Percent of Wetland Habitat on Island", 4:7)
pctlong$HabitatQuality<-sub("\\(% of Wetland Area on Island)", "", pctlong$Descriptor)
pctlong <- within(pctlong, rm(Descriptor))


#reorder Habitat Quality Variables So they Plot in more intuitive colors
str(arealong)
arealong$HabitatQuality<-as.factor(arealong$HabitatQuality)
levels(arealong$HabitatQuality)
arealong$HabitatQuality <- ordered(arealong$HabitatQuality,levels = c("Disturbed ","Barren ","Non-Native Dominated ","Native Dominated "))
#check this worked
levels(arealong$HabitatQuality)   

#now we can make pretty


k<-ggplot(arealong, aes(arealong$Island, arealong$Acres ,  fill = arealong$HabitatQuality))
pretty1<-k+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Habitat Quality", title = "Wetlands Habitat Quality Summary", x = "Island", y = "Area (% of Total Wetland Area on Island)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)


ggsave(filename= "HabitatQualityWetlandsPlot.jpg", plot=pretty1, device = "jpg" )


u$`Area Barren (Acres)`[u$`Area Barren (Acres)` <1]<-"<1"
u$`Area Native Dominated (Acres)`[u$`Area Native Dominated (Acres)` <1]<-"<1"
u$`Area Non-Native Dominated (Acres)`[u$`Area Non-Native Dominated (Acres)` <1]<-"<1"
u$`Area Disturbed (Acres)`[u$`Area Disturbed (Acres)` <1]<-"<1"

u$`Barren (% of Wetland Area on Island)`  [u$`Barren (% of Wetland Area on Island)`  <1]<-"<1"
u$`Native Dominated  (% of Wetland Area on Island)` [u$`Native Dominated  (% of Wetland Area on Island)` <1]<-"<1"
u$`Non-Native Dominataed (% of Wetland Area on Island)`  [u$`Non-Native Dominataed (% of Wetland Area on Island)` <1]<-"<1"
u$`Disturbed (% of Wetland Area on Island)`  [u$`Disturbed (% of Wetland Area on Island)` <1]<-"<1"

#write file to drive

write.csv(u, file = "WetlandHabitatQualitySummary.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Work up the Land Ownership Data 

own<-ownpolydata
names(own)
own<-merge(own, noaawetlandareas, by = "island")
#make a list of keeper variables for each outfile:

j<-own[,c("island" ,  "wetlandareaonisland",  "wetlandtype" ,  "Area_m" , "keyinfo" )]

names(j)<-c("island", "wetlandareaonisland","WetlandType","newarea_m", "keyinfo" )

#reclassify the State DHHL ownership to State
levels(j$keyinfo)
j$keyinfo<-as.character(j$keyinfo)
j$keyinfo[j$keyinfo ==  "Govt. State DHHL"]<-  "Govt. State" 
j$keyinfo[j$keyinfo ==  "No data" ]<- "No Data"
j$keyinfo<-as.factor(j$keyinfo)

#convert the wetland area on island column to acres and round it too
j$WetlandAcresOnIsle<-round(j$wetlandareaonisland*0.0002471, digits = 0)
j$acres<-round(j$newarea_m*0.0002471, digits = 2)

#calculate area proportion of wetlands on island
j$propwetlandsisle<-round((j$acres/j$WetlandAcresOnIsle)*100, digits = 2)
#drop the m2 area columns

j<-j[,c("island","WetlandType", "keyinfo", "acres","WetlandAcresOnIsle", "propwetlandsisle")]
#give better names
names(j)<-c("Island","Wetland Type","Land Ownership" , "Area (Acres)","Total Wetland Area on Island (Acres)", "Area (% of Wetland Area on Island")
write.csv(j, file = "WetlandsLandOwnershipDataRaw.csv")

d<-aggregate(j$`Area (Acres)`, by = list(j$Island, j$`Wetland Type`, j$`Total Wetland Area on Island (Acres)`, j$`Land Ownership`  ), FUN = sum)
names(d)<-c("Island","Wetland Type","Total Wetland Area on Island (Acres)","Land Ownership" ,"Area (Acres)")
#d$acres<-as.numeric(d$`Area (Acres)`)
d$propwetlandsisle<-round((d$`Area (Acres)` /d$`Total Wetland Area on Island (Acres)`  )*100, digits = 0)

d$`Area (Acres)`<-round(d$`Area (Acres)`, digits = 0)

names(d)<-c("Island","Wetland Type","Total Wetland Area on Island (Acres)","Land Ownership" ,"Area (Acres)","Area (% of Wetland Area on Island)")



a<-ggplot(d, aes(d$Island, d$`Area (Acres)`, fill = d$`Land Ownership` ))

pretty<-a+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Land Ownership", title = "Wetland Land Ownership Summary", x = "Island", y = "Area (% of Total Wetland Area on Island)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)

ggsave(filename= "WetlandsLandOwnershipSummary.jpg", plot=pretty, device = "jpg")

d$`Area (% of Wetland Area on Island)`  [d$`Area (% of Wetland Area on Island)`  <1]<-"<1"
d$`Area (Acres)`[d$`Area (Acres)`<1]<-"<1"


write.csv(d, file = "WetlandsLandOwnershipSummary.csv" )


save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/NOAACCAPRasters/18_06_13.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Work up Critical Habitat Data


chdata<-merge(ch, noaawetlandareas, by = "island")

#make a list of keeper variables for each outfile:
names(chdata)

ch1<-chdata[,c("island" ,  "wetlandareaonisland",  "Class_Name"  ,    "newarea_m"   )]

names(ch1)<-c("island", "wetlandareaonisland","WetlandType", "newarea_m")


#convert the wetland area on island column to acres and round it too
ch1$WetlandAcresOnIsle<-round(ch1$wetlandareaonisland*0.0002471, digits = 0)
ch1$newarea_m<-as.numeric(ch1$newarea_m)
ch1$acres<-round(ch1$newarea_m*0.0002471, digits = 2)

#calculate area proportion of wetlands on island
ch1$propwetlandsisle<-round((ch1$acres/ch1$WetlandAcresOnIsle)*100, digits = 2)
#drop the m2 area columns
names(ch1)
ch1<-ch1[,c("island","WetlandType", "acres","WetlandAcresOnIsle", "propwetlandsisle")]
#give better names
names(ch1)<-c("Island","Wetland Type", "Area (Acres)","Total Wetland Area on Island (Acres)", "Area (% of Wetland Area on Island")
write.csv(ch1, file = "WetlandsCriticalHabitatDataRaw.csv")

ch2<-aggregate(ch1$`Area (Acres)`, by = list(ch1$Island, ch1$`Wetland Type`, ch1$`Total Wetland Area on Island (Acres)`  ), FUN = sum)
names(ch2)<-c("Island","Wetland Type","Total Wetland Area on Island (Acres)","Area (Acres)")
#ch2$acres<-as.numeric(ch2$`Area (Acres)`)
ch2$propwetlandsisle<-round((ch2$`Area (Acres)` /ch2$`Total Wetland Area on Island (Acres)`  )*100, digits = 0)

ch2$`Area (Acres)`<-round(ch2$`Area (Acres)`, digits = 0)

names(ch2)<-c("Island","Wetland Type","Total Wetland Area on Island (Acres)","Area (Acres)","Area (% of Wetland Area on Island)")

#clean up and reorder variables
#Change "Water" to Open Water
levels(ch2$`Wetland Type`)
ch2$`Wetland Type`<-as.character(ch2$`Wetland Type`)
ch2$`Wetland Type`[ch2$`Wetland Type`==  "Water" ]<-  "Open Water" 
ch2$`Wetland Type`[ch2$`Wetland Type` == "Palustrine Scrub/Shrub Wetland"]<-"Palustrine Scrub Shrub Wetland"
ch2$`Wetland Type`<-as.factor(ch2$`Wetland Type`)

#Reorder these so open water plots in Blue
ch2$`Wetland Type`<-ordered(ch2$`Wetland Type`, levels = c("Palustrine Aquatic Bed","Palustrine Emergent Wetland"  ,  "Palustrine Forested Wetland"   ,
                                                           "Palustrine Scrub Shrub Wetland" ,  "Unconsolidated Shore" ,"Open Water"  ))


#check that worked
levels(ch2$`Wetland Type`)


c<-ggplot(ch2, aes(ch2$Island, ch2$`Area (Acres)`, fill = ch2$`Wetland Type` ))

prettych<-c+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Land Ownership", title = "Wetland Critical Habitat Summary", x = "Island", y = "Area (Proportion of Total Wetland Acres in Critical Habitat)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)

ggsave(filename= "WetlandsCriticalHabitatSummary.jpg", plot=prettych, device = "jpg")

ch2$`Area (% of Wetland Area on Island)`  [ch2$`Area (% of Wetland Area on Island)`  <1]<-"<1"
ch2$`Area (Acres)`[ch2$`Area (Acres)`<1]<-"<1"


write.csv(ch2, file = "WetlandsCriticalHabitatSummary.csv" )


save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/ShapeFiles4R/NOAACCAPRasters/18_06_13.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Work up Reserve Data

resdata<-respolydata

names(resdata)
resdata<-merge(respolydata, noaawetlandareas, by = "island")

#make a list of keeper variables for each outfile:
names(resdata)

res1<-resdata[,c("island" ,  "wetlandareaonisland",  "Class_Name"  ,    "Type_Defin" ,  "newarea_m"   )]

names(res1)<-c("island", "wetlandareaonisland","WetlandType", "Reserve Type", "newarea_m")


#convert the wetland area on island column to acres and round it too
res1$WetlandAcresOnIsle<-round(res1$wetlandareaonisland*0.0002471, digits = 0)
res1$newarea_m<-as.numeric(res1$newarea_m)
res1$acres<-round(res1$newarea_m*0.0002471, digits = 2)

#calculate area proportion of wetlands on island
res1$propwetlandsisle<-round((res1$acres/res1$WetlandAcresOnIsle)*100, digits = 2)
#drop the m2 area columns
names(res1)
res1<-res1[,c("island","WetlandType",  "Reserve Type" ,"acres","WetlandAcresOnIsle", "propwetlandsisle")]
#give better names
names(res1)<-c("Island","Wetland Type",  "Reserve Type" ,"Area (Acres)","Total Wetland Area on Island (Acres)", "Area (% of Wetland Area on Island")
write.csv(res1, file = "WetlandsReserveStatusDataRaw.csv")

res2<-aggregate(res1$`Area (Acres)`, by = list(res1$Island, res1$`Wetland Type`, res1$`Reserve Type`, res1$`Total Wetland Area on Island (Acres)`  ), FUN = sum)
names(res2)<-c("Island","Wetland Type", "Reserve Type" ,"Total Wetland Area on Island (Acres)","Area (Acres)")

res2$propwetlandsisle<-round((res2$`Area (Acres)` /res2$`Total Wetland Area on Island (Acres)`  )*100, digits = 0)

res2$`Area (Acres)`<-round(res2$`Area (Acres)`, digits = 0)

names(res2)<-c("Island","Wetland Type","Reserve Type" ,"Total Wetland Area on Island (Acres)","Area (Acres)","Area (% of Wetland Area on Island)")

#clean up and reorder variables
#Change "Water" to Open Water
# levels(res2$`Wetland Type`)
# res2$`Wetland Type`<-as.character(res2$`Wetland Type`)
# res2$`Wetland Type`[res2$`Wetland Type`==  "Water" ]<-  "Open Water" 
# res2$`Wetland Type`[res2$`Wetland Type` == "Palustrine Scrub/Shrub Wetland"]<-"Palustrine Scrub Shrub Wetland"
# res2$`Wetland Type`<-as.factor(res2$`Wetland Type`)
# 
# #Reorder these so open water plots in Blue
# res2$`Wetland Type`<-ordered(res2$`Wetland Type`, levels = c("Palustrine Aquatic Bed","Palustrine Emergent Wetland"  ,  "Palustrine Forested Wetland"   ,
#                                                            "Palustrine Scrub Shrub Wetland" ,  "Unconsolidated Shore" ,"Open Water"  ))
# 
# 
# #check that worked
# levels(res2$`Wetland Type`)


res3<-ggplot(res2, aes(res2$Island, res2$`Area (Acres)`, fill = res2$`Reserve Type` ))

prettyres<-res3+geom_bar(stat = "identity",  position = "fill")+
  labs(fill = "Land Ownership", title = "Wetland Reserve Status Summary", x = "Island", y = "Area (Proportion of Total Wetland Acres in Reserves)" ) +
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))+
  scale_fill_manual(values=cbPalette)

ggsave(filename= "WetlandsReserveStatusSummary.jpg", plot=prettyres, device = "jpg")

res2$`Area (% of Wetland Area on Island)`  [res2$`Area (% of Wetland Area on Island)`  <1]<-"<1"
res2$`Area (Acres)`[res2$`Area (Acres)`<1]<-"<1"


write.csv(res2, file = "WetlandsReserveStatusSummary.csv" )
save.image("~/HSAs/HabitatStatusAssessmentsR/Hawaii/18_06_14_WetlandsWorkspaceFinal.RData")

