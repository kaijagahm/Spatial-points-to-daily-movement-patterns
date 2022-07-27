#initialize working space
rm(list=ls()) #clean workspace
graphics.off() #close all plots

########################    coflight 2021   ##################

#### loading packages #######
library(move)#for downloading data
library(mapproj);library(ggmap) #these packages are necessary to work with google maps
library(spatsoc);library("asnipe");library("igraph"); # for working with the SN parts
library(reshape);library(data.table) #for the manual section where i build the SN myself
library(adehabitatLT);
library(moveVis)
source('C:/XXXXX/MyCodes/createDirectedMatrices.R')

#### key paramterer values ######
MaxSpeedPermited=120 #in movebank units (m/s) anything above this will be filtered

VulturesToPlotMap=10:15 
DistThreshold=1000 #---at what distance two indi are considered interacting
TimeThreshold='10 minutes' # timegroups - temporally overlapping
MinCoocurForValue=2; #minimal number of coocurences for considering a viable pair- 

############# other variables specified##########

MaxSpeedPermited=120 #in movebank units (m/s??) anything above this will be filtered
roostbuffer = 50 # buffer around roosting polygons (in metres)
feedBuff = 100 # buffer around feeding stations (in metres)

################### reading data from movebank ###################

Password = "XXXXXX"
MB.LoginObject=movebankLogin(username='NitikaSharma',password=Password);rm(Password)

## Extracting Dec 2020 onwards data from the study="Ornitela_Vultures_Gyps_fulvus_TAU_UCLA_Israel" in MoveBank

MoveStackDataset=getMovebankData(study=1252551761, login=MB.LoginObject,
                                 includeExtraSensors=FALSE, deploymentAsIndividuals=FALSE,removeDuplicatedTimestamps=TRUE,
                                 timestamp_start="20200828120100000", ##Starting on December 1 2018 to include in breeding commencemnet
                                 timestamp_end="20210814120100000")#animalName



#### just loooking on raw downloaded data ####
show(MoveStackDataset)
summary(MoveStackDataset)

unstacked=split(MoveStackDataset) #splitting MoveBank data into individuals 
#sanity checks
length(unstacked)
class(unstacked)
unstacked[[3]]@data
class(unstacked[[8]])

citations(MoveStackDataset)
equalProj(MoveStackDataset)
n.locs(MoveStackDataset)
timeLag(MoveStackDataset)
#too heavy but works: 
#plot(MoveStackDatasetOhad,  lwd=2, xlab="location_long", ylab="location_lat",col='blue',pch=5)

############# very basic filtering. stage 1  ##############
## creating an empty metadata storage df
TagsMetaData=setNames(data.frame(matrix(ncol =8, nrow = length(unstacked))), 
                      c('name',"InitialPoints", "PercentThrown", "N_locs","TrackDurationDays","DaysBetweenStartEnd",'FirstDay','LastDay'))

#### Extracting Breeding season data: December 2020 to June 2021 ##########

library(lubridate)

BreedingFlight_2021 <- lapply(unstacked, subset,ground_speed>5 & 
                                month(timestamp)==1 | month(timestamp)==2| month(timestamp)==3|
                                month(timestamp)==4| month(timestamp)==5| 
                                month(timestamp)==6|(month(timestamp)==12 & year(timestamp)==2020))##Also included Dec 2020 in breeding season 

BreedingFlight_2021 <- BreedingFlight_2021[sapply(BreedingFlight_2021, function(x) dim(x)[1]) > 0] ##only keep non-empty dataframes
length(BreedingFlight_2021)

hist(unique(date(BreedingFlight_2021[[8]]@data$timestamp)), breaks = 13)

min(date(BreedingFlight_2021[[1]]@data$timestamp))
max(date(BreedingFlight_2021[[1]]@data$timestamp))

######################
## converting back to a movestack
Breeding_MS<-moveStack(BreedingFlight_2021)
tz(MoveStackDatasetOhad_2021)
idData(Breeding_MS)
Breeding_DF <- methods::as(Breeding_MS, "data.frame")
Breeding_DF$DateOnly<-as.Date(as.character(Breeding_DF$timestamp));
head(Breeding_DF)

#########Removing Out of Israel Breeding locations before removing vultures recorded for inadequate duration #########
####################Pruning data to only select points falling inside Israel polygon  ################

FileName='CutOffRegion.kml'
LayerName='Regional_polygon'
Outline = readOGR(dsn="C:/XXXXX/KML_Files/CutOffRegion.kml", 
                  layer="CutOffRegion.kml")

plot(Outline)

class(Outline)
head(allLastLocs) ## WGS84
Outline@polygons  ##  WGS84

####  convert all Last Locations to a spatial points object

xy <- Breeding_DF[,c("coords.x1","coords.x2")]

Sp_InIsrael_Dataset <- SpatialPointsDataFrame(coords = xy, data = Breeding_DF,
                                              proj4string = CRS(projection(Breeding_MS)))

#pts_in<-Sp_allLastLoc_AllID[!is.na(over(Sp_allLastLoc_AllID,Outline)),]
Breed_INIsrael<-Sp_InIsrael_Dataset[complete.cases(over(Sp_InIsrael_Dataset, Outline)), ]
plot(Outline)
plot(Breed_INIsrael, col="red", add=TRUE)

##now only keeping the rows with data from within Israel
head(Breed_INIsrael)

#MoveStackDataset2021[[-which(namesIndiv(MoveStackDataset2020)%in%all_INIsrael$ID)]]
Dataset_BreedInIsrael<-as.data.frame(Breed_INIsrael)##renaming only the InIsrael data of 2021 that falls within Israel as the dataset that will be used henceforth 

####Removing vultures that have fewer than 1/3rd of their duration recorded INSIDE ISRAEL cut-off region ######
library(plyr)
IdByDateLocs_Br<-plyr::ddply(Dataset_BreedInIsrael, .(Dataset_BreedInIsrael$trackId, Dataset_BreedInIsrael$DateOnly), nrow)
colnames(IdByDateLocs_Br)<-c("trackId","DateOnly","no_Relocs")
DaysRecorded_Br<-as.data.frame(table(IdByDateLocs_Br$trackId))
colnames(DaysRecorded_Br)<-c("trackId", "DaysRec")
hist(DaysRecorded_Br$DaysRec)
max(DaysRecorded_Br$DaysRec)*(1/3) #1/3 of 213 max days

plot(DaysRec~trackId, data= DaysRecorded_Br, las=3, cex.axis=0.5)

Vultures_min71Days_Br<-subset(DaysRecorded_Br, DaysRec>max(DaysRecorded_Br$DaysRec)*(1/3) &
                                trackId != "J53w" & trackId !="T59w" & trackId != "T61w")
Vultures_min71Days_Br$trackId<-factor(Vultures_min71Days_Br$trackId)


########## Subsetting Dataset to only include vultures that were recorded long enough ######
nrow(Dataset_BreedInIsrael)
length(unique(Dataset_BreedInIsrael$trackId))
length(unique(Dataset_BreedInIsrael$trackId))

Dataset_BrFlight_MinDays<-Dataset_BreedInIsrael[Dataset_BreedInIsrael$trackId %in% Vultures_min71Days_Br$trackId,]
Dataset_BrFlight_MinDays$trackId<-factor(Dataset_BrFlight_MinDays$trackId)
length(unique(Dataset_BrFlight_MinDays$trackId))

table(Dataset_BrFlight_MinDays$trackId)

#######################################################################
# use df2move to convert the data.frame into a moveStack
projection(MoveStackDatasetOhad_2021)
Movestacked_BrFlight<-df2move(Dataset_BrFlight_MinDays,
                              proj = projection(MoveStackDatasetOhad_2021), 
                              x = "coords.x1", y = "coords.x2", time = "timestamps", track_id = "trackId",
                              data = Dataset_BrFlight_MinDays)


extent(Movestacked_BrFlight)

class(Movestacked_BrFlight)
str(Movestacked_BrFlight)
summary(Movestacked_BrFlight)
colnames(Dataset_BrFlight_MinDays)
unstacked_Brflight<-split(Movestacked_BrFlight)

length(unstacked_Brflight)

## a loop on tags. ###### #
for (indv in 1:length(unstacked_Brflight) ){## loop on individuals, now in separate Move objects
  TagsMetaData$InitialPoints[indv]=  dim(unstacked_Brflight[[indv]]@data)[1]  ##number of fixes for an individual
  
  TagsMetaData$name[indv]=          names(unstacked_Brflight)[indv]
  plot(unstacked_Brflight[[indv]],col='blue', type='b',main=paste("Indiv=",indv, ', name=',TagsMetaData$name[indv],sep=' '))#just simple plot of this individual's points
  #dim(unstacked_Brflight[[indv]]@coords)
  
  ## removing unneeded columns
  VarsToRemove <- names(unstacked_Brflight[[indv]]@data) %in% c("sensor_type_id","taxon_canonical_name","nick_name","earliest_date_born","sensor","optional",
                                                                "sensor_type","mw_activity_count","eobs_accelerations_raw","eobs_acceleration_sampling_frequency_per_axis",
                                                                "eobs_acceleration_axes","argos_valid_location_algorithm","argos_sensor_4","argos_sensor_3","argos_sensor_2",
                                                                "argos_sensor_1","argos_semi_minor","argos_semi_major","argos_pass_duration","argos_orientation","argos_nopc",
                                                                "argos_lat1","argos_lat2","1084088","argos_lon1","argos_lon2","argos_nb_mes","argos_nb_mes_120",
                                                                "eobs_key_bin_checksum","eobs_fix_battery_voltage","eobs_battery_voltage","eobs_status",
                                                                "eobs_start_timestamp","eobs_type_of_fix","eobs_used_time_to_get_fix","eobs_temperature",
                                                                "gps_dop","magnetic_field_raw_x","magnetic_field_raw_y","magnetic_field_raw_z","ornitela_transmission_protocol",
                                                                "tag_voltage","algorithm_marked_outlier","argos_altitude","argos_best_level","argos_lc","argos_iq",
                                                                "argos_gdop","argos_error_radius","argos_calcul_freq","location_lat.1","location_long.1","timestamps","height_raw",
                                                                "barometric_pressure","barometric_height","battery_charging_current","eobs_activity","manually_marked_outlier",
                                                                "eobs_activity_samples", "acceleration_raw_y", "battery_charge_percent", "data_decoding_software","gps_vdop","height_above_ellipsoid",
                                                                'acceleration_raw_x','acceleration_raw_z',"acceleration_raw_z","eobs_horizontal_accuracy_estimate","eobs_speed_accuracy_estimate");  
  
  
  unstacked_Brflight[[indv]]@data=unstacked_Brflight[[indv]]@data[!VarsToRemove]  ##all columns except above listed ones
  dim(unstacked_Brflight[[indv]]@data)#checking if colunms removed
  
  ## filtering: choosing indices to keep for this individual
  
  indx=1:dim(unstacked_Brflight[[indv]]@data)[1] #starting with all points a
  if(sum(unstacked_Brflight[[indv]]@data$heading <= 360,na.rm=T)){#do i have heading data or this one?
    
    indx=intersect(indx,which(unstacked_Brflight[[indv]]@data$heading <= 360))} #if yes, now index include only points with realistic heading 
  
  if(sum(unstacked_Brflight[[indv]]@data$ground_speed<=MaxSpeedPermited,na.rm=T)){#below threshhold speed?
    indx=intersect(indx,which(unstacked_Brflight[[indv]]@data$ground_speed<=120))}
  
  if(sum(unstacked_Brflight[[indv]]@data$gps_satellite_count>=3,na.rm=T)){#enough satellite numbers?
    indx=intersect(indx,which(unstacked_Brflight[[indv]]@data$gps_satellite_count>=3))}
  
  
  ## subsetting the different slots of this move object
  print(paste("indiv",indv,"name",TagsMetaData$name[indv],'. I throw out', TagsMetaData$InitialPoints[indv]-length(indx), 'points, out of',TagsMetaData$InitialPoints[indv]))
  TagsMetaData$PercentThrown[indv]=(TagsMetaData$InitialPoints[indv]-length(indx))/TagsMetaData$InitialPoints[indv]
  
  unstacked_Brflight[[indv]]@timestamps=unstacked_Brflight[[indv]]@timestamps[indx]
  unstacked_Brflight[[indv]]@sensor=unstacked_Brflight[[indv]]@sensor[indx]
  unstacked_Brflight[[indv]]@data=unstacked_Brflight[[indv]]@data[indx,]
  if(dim(unstacked_Brflight[[indv]]@data)[1]>1){ ##Nitika = to avoid stalling the loop if not enough data locations
    unstacked_Brflight[[indv]]@coords=unstacked_Brflight[[indv]]@coords[indx,]
    unstacked_Brflight[[indv]]@bbox[1,]=range(unstacked_Brflight[[indv]]@coords[,1]);unstacked_Brflight[[indv]]@bbox[2,]=range(unstacked_Brflight[[indv]]@coords[,2])
    
    
    ## collecting metadata and plotting fitered track: 
    TagsMetaData$N_locs[indv]=  dim(unstacked_Brflight[[indv]]@data)[1]
    TagsMetaData$TrackDurationDays[indv]=  length(unique(as.Date(as.character(unstacked_Brflight[[indv]]@data$timestamp))))
    TagsMetaData$FirstDay[indv]=as.character(min(as.Date(as.character(unstacked_Brflight[[indv]]@data$timestamp))));
    TagsMetaData$LastDay[indv]= as.character(max(as.Date(as.character(unstacked_Brflight[[indv]]@data$timestamp))));
    TagsMetaData$DaysBetweenStartEnd[indv]=as.Date(TagsMetaData$LastDay[indv])-as.Date(TagsMetaData$FirstDay[indv]);
    lines(unstacked_Brflight[[indv]],col='red')
    #plot(unstacked_Brflight[[indv]], type="o", col=3, lwd=2, pch=20, xlab="location_long", ylab="location_lat")
    
    ##logging metadata
    
    head(timeLag(unstacked_Brflight[[indv]], units="mins"))
    head(timestamps(unstacked_Brflight[[indv]]))
  }
}#loop on individuals


FlightF_Dataset=as.data.frame(AllBackStacked_flight)
####################Pruning data to only select points falling inside Israel polygon  ################

setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files")
FileName='CutOffRegion.kml'
LayerName='Regional_polygon'
Outline = readOGR(dsn="C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/KML_Files/CutOffRegion.kml", 
                  layer="CutOffRegion.kml")

plot(Outline)

class(Outline)
head(allLastLocs) ## WGS84
Outline@polygons  ##  WGS84

####  convert all Last Locations to a spatial points object

xy <- FlightF_Dataset[,c("location_long","location_lat")]

Sp_FlightF_Dataset <- SpatialPointsDataFrame(coords = xy, data = FlightF_Dataset,
                                             proj4string = CRS(projection(AllBackStacked_flight)))

#pts_in<-Sp_allLastLoc_AllID[!is.na(over(Sp_allLastLoc_AllID,Outline)),]
all_INIsrael<-Sp_FlightF_Dataset[complete.cases(over(Sp_FlightF_Dataset, Outline)), ]
plot(Outline)
plot(all_INIsrael, col="red", add=TRUE)

##now only keeping the rows with data from within Israel
head(all_INIsrael)

Dataset_flightF<-as.data.frame(all_INIsrael)##renaming only the flight data of 2021 that falls within Israel as the dataset that will be used henceforth 

##CheckProjections as done earlier before proceeding


#### find Maximum displacement from first flight location of the date#####

Dataset_flightF$date<-date(Dataset_flightF$timestamps)

VultureID<-as.character(unique(Dataset_flightF$trackId))
#For each vulture, for each date, 

library(sf)
minFixes = 2
AllMaxDisp<-data.frame()
TotFlightDurn<-data.frame()


for(i in 1: length(VultureID)){
  
  Vul1<-subset(Dataset_flightF, trackId == VultureID[i])
  
  ##Find time difference between consecutive timestamps within a day, 
  #it gives NA if the timestamps are from different dates
  
  Vul1[ , timediff := timestamps - shift(timestamps), by = date] 
  
  
  ##
  
  FixesPerIDPerDate<-as.data.frame(Vul1 %>% dplyr::count(ID, date))
  hist(FixesPerIDPerDate$n, xlim = c(0,50), breaks = 50)
  boxplot(FixesPerIDPerDate$n)
  
  max(FixesPerIDPerDate$n)
  
  head(Vul1)
  
  for (j in 1: length(unique(FixesPerIDPerDate$date))){
    
    Vul1Date1<-subset(Vul1, date ==unique(FixesPerIDPerDate$date)[j])
    
    ## make spatial points in UTM to sf object
    Vul1Date1$sf_geometry <- (st_as_sf(Vul1Date1, coords = c("Easting", "Northing"), crs = utmN))$geometry 
    
    #
    library(sf)
    MaxDisp_Vult1<-max(as.numeric(as.character(st_distance(x = Vul1Date1$sf_geometry, 
                                                           y = Vul1Date1$sf_geometry[Vul1Date1$timestamps==min(Vul1Date1$timestamps)], 
                                                           by_element = TRUE))))
    
    AllMaxDisp<-as.data.frame(rbind(AllMaxDisp, cbind(ID = VultureID[i], date = as.character(unique(Vul1$date)[j]), MaxDisplacement = round(MaxDisp_Vult1,2))))
    
    
    ####total flight time for the date for that vulture
    
    TotFlightDurn<-as.data.frame(rbind(TotFlightDurn, cbind(ID = VultureID[i], date = as.character(unique(Vul1$date)[j]), TotDuration = round(sum(Vul1Date1$timediff, na.rm = TRUE),2))))
    
    
  }
}

write.csv(AllMaxDisp, "MinDur2021_BreedingMaxDisplacementsPerDatePerVulture.csv")
write.csv(TotFlightDurn, "MinDur2021_Breeding_TotFlightDurn.csv")
