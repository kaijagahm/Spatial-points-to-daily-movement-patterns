############### Merging all movement metrics ###########
setwd("XXXX/MovtPersonality")
Prop_KDE<-read.csv("Vultures2019_AllPropAreas.csv")
head(Prop_KDE)
nrow(Prop_KDE)

Activity<-read.csv("Non0_Avg_FlightDispTime.csv")
head(Activity)
nrow(Activity)

RoostShannon<-read.csv("Breeding_RoostFidelity_ShannonIndex.csv")


head(RoostShannon)
colnames(RoostShannon)<-c("ID","Shannon.Roost")
nrow(RoostShannon)

KDE_Activity<-left_join(Prop_KDE, Activity, by= "ID")
KDEProp_Activity_RoostFidelity<-left_join(KDE_Activity, RoostShannon, by="ID")

head(KDEProp_Activity_RoostFidelity)
Movt_Indices<-KDEProp_Activity_RoostFidelity



###Importing Multilayer versatility for vultures 2019 inter,intralayer#############

versatility_Br2019<-read.csv("BreedingVersatility.csv", header=T)

versatility2019<-subset(versatility_Br2019, Layer == "1-Multi")[,-c(11,14:16)]


Versatility3_MovtPersonality<-left_join(KDEProp_Activity_RoostFidelity, versatility2019, by = c("ID" = "Label"))
head(Versatility3_MovtPersonality)


##################    Centrality Per Layer #############

dat0 = read.csv("Centrality_single_3lyrs.csv", header=T)
head(dat0)
dat = dat0[,-c(11,14:16)]

LayerIDs<-as.data.frame(cbind(c(1:3, "Aggr"), c("CoFlight","CoRoosting","CoFeeding","Aggregate")))
colnames(LayerIDs)<-c("Layer","LayerName")


dat1<-left_join(dat, LayerIDs, by = "Layer")

library(tidyr)
str(dat1)
dat_wide<-as.data.frame(dat1[,-c(1,2,5,6,8,9)] %>%
                          pivot_wider(names_from = c(LayerName), 
                                      values_from = c(Degree, Strength, PageRank, Hub, Authority)))


head(as.data.frame(dat_wide))
nrow(dat_wide)
tail(dat_wide)


###Joining the Versatility, Movt personality and Centrality dataframes #####

data<-read.csv("Versatility3_MovtPersonality.csv", header=T)
head(data)

Versatility_Centrality_Movt<-left_join(data, dat_wide, by="Label")

str(Versatility_Centrality_Movt)
summary(Versatility_Centrality_Movt)

write.csv(Versatility_Centrality_Movt, "Versatility3_Centrality3_Movt.csv")


#####   Adding metadata on age, sex and origin ########


AgeThresh=5
setwd("C:/Users/nitik/Box Sync/Manuscript4_Vulture Data/VulturesCodes-master/MyOutputs/All5Multilayer2019")
metaData<-read.csv("whoswho_vultures300920.csv")
head(metaData)
sub_meta = subset(metaData, Vertex !="") ##only the tags that have been mapped
nrow(sub_meta)
str(sub_meta)
sub_meta$years.at.deployment<-as.numeric(as.character(sub_meta$years.at.deployment))
sub_meta$AgeClass<-ifelse(sub_meta$years.at.deployment<AgeThresh, "J","A")
head(sub_meta)
str(sub_meta)

Metadata=sub_meta[,c(1,2,14,16,17,18,19,20,26,183)]
head(Metadata)
summary(Metadata)

GrandData_2019_3lyrs<-left_join(Metadata, Versatility_Centrality_Movt, by =c("Vertex"="Label"))

head(GrandData_2019_3lyrs)
write.csv(GrandData_2019_3lyrs, "GrandMergedData3Layers.csv")


############ Boxplots by Age, sex, origin ##########

head(GrandData_2019_3lyrs)
GrandData_2019_3lyrs$Origin_title

boxplot(Degree~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Degree,col=3)
boxplot(Strength~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Strength,col=3)
boxplot(PageRank~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$PageRank,col=3)
boxplot(Hub~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Hub,col=3)

boxplot(Degree_CoFlight~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Degree_CoFlight,col=3)
boxplot(Strength_CoFlight~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Strength_CoFlight,col=3)
boxplot(PageRank_CoFlight~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$PageRank_CoFlight,col=3)
boxplot(Hub_CoFlight~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Hub_CoFlight,col=3)

boxplot(Degree_CoRoosting~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Degree_CoRoosting,col=3)
boxplot(Strength_CoRoosting~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Strength_CoRoosting,col=3)
boxplot(PageRank_CoRoosting~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$PageRank_CoRoosting,col=3)
boxplot(Hub_CoRoosting~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Hub_CoRoosting,col=3)

boxplot(Degree_CoFeeding~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Degree_CoFeeding,col=3)
boxplot(Strength_CoFeeding~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Strength_CoFeeding,col=3)
boxplot(PageRank_CoFeeding~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$PageRank_CoFeeding,col=3)
boxplot(Hub_CoFeeding~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Hub_CoFeeding,col=3)

boxplot(Degree_Aggregate~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Degree_Aggregate,col=3)
boxplot(Strength_Aggregate~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Strength_Aggregate,col=3)
boxplot(PageRank_Aggregate~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$PageRank_Aggregate,col=3)
boxplot(Hub_Aggregate~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Hub_Aggregate,col=3)

boxplot(areas50~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$areas50,col=3)
boxplot(areas95~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$areas95,col=3)
boxplot(Movt.Personality~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Movt.Personality,col=3)
boxplot(AvgDisplacementPerDay~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$AvgDisplacementPerDay,col=3)
boxplot(AvgFlightDurPerDay~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$AvgFlightDurPerDay,col=3)
boxplot(Shannon.Roost~AgeClass, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$AgeClass), GrandData_2019_3lyrs$Shannon.Roost,col=3)



######## vs. sex ##########

head(GrandData_2019_3lyrs)
sex_GrandData<-subset(GrandData_2019_3lyrs, sex =="f"|sex=="m")
sex_GrandData$sex<-factor(as.character(sex_GrandData$sex))

head(sex_GrandData)
sex_GrandData$Origin_title

boxplot(Degree~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Degree,col=3)
boxplot(Strength~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Strength,col=3)
boxplot(PageRank~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$PageRank,col=3)
boxplot(Hub~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Hub,col=3)

boxplot(Degree_CoFlight~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Degree_CoFlight,col=3)
boxplot(Strength_CoFlight~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Strength_CoFlight,col=3)
boxplot(PageRank_CoFlight~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$PageRank_CoFlight,col=3)
boxplot(Hub_CoFlight~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Hub_CoFlight,col=3)

boxplot(Degree_CoRoosting~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Degree_CoRoosting,col=3)
boxplot(Strength_CoRoosting~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Strength_CoRoosting,col=3)
boxplot(PageRank_CoRoosting~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$PageRank_CoRoosting,col=3)
boxplot(Hub_CoRoosting~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Hub_CoRoosting,col=3)

boxplot(Degree_CoFeeding~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Degree_CoFeeding,col=3)
boxplot(Strength_CoFeeding~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Strength_CoFeeding,col=3)
boxplot(PageRank_CoFeeding~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$PageRank_CoFeeding,col=3)
boxplot(Hub_CoFeeding~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Hub_CoFeeding,col=3)

boxplot(Degree_Aggregate~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Degree_Aggregate,col=3)
boxplot(Strength_Aggregate~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Strength_Aggregate,col=3)
boxplot(PageRank_Aggregate~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$PageRank_Aggregate,col=3)
boxplot(Hub_Aggregate~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Hub_Aggregate,col=3)

boxplot(areas50~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$areas50,col=3)
boxplot(areas95~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$areas95,col=3)
boxplot(Movt.Personality~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Movt.Personality,col=3)
boxplot(AvgDisplacementPerDay~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$AvgDisplacementPerDay,col=3)
boxplot(AvgFlightDurPerDay~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$AvgFlightDurPerDay,col=3)
boxplot(Shannon.Roost~sex, data = sex_GrandData)
points(factor(sex_GrandData$sex), sex_GrandData$Shannon.Roost,col=3)



###### origin #########
GrandData_2019_3lyrs<-GrandData_2019_3lyrs %>%
  group_by(Origin_title = stri_trans_totitle(origin)) %>%
  mutate(rn = row_number())

head(GrandData_2019_3lyrs)
GrandData_2019_3lyrs$Origin_title

boxplot(Degree~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Degree,col=3)
boxplot(Strength~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Strength,col=3)
boxplot(PageRank~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$PageRank,col=3)
boxplot(Hub~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Hub,col=3)

boxplot(Degree_CoFlight~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Degree_CoFlight,col=3)
boxplot(Strength_CoFlight~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Strength_CoFlight,col=3)
boxplot(PageRank_CoFlight~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$PageRank_CoFlight,col=3)
boxplot(Hub_CoFlight~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Hub_CoFlight,col=3)

boxplot(Degree_CoRoosting~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Degree_CoRoosting,col=3)
boxplot(Strength_CoRoosting~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Strength_CoRoosting,col=3)
boxplot(PageRank_CoRoosting~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$PageRank_CoRoosting,col=3)
boxplot(Hub_CoRoosting~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Hub_CoRoosting,col=3)

boxplot(Degree_CoFeeding~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Degree_CoFeeding,col=3)
boxplot(Strength_CoFeeding~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Strength_CoFeeding,col=3)
boxplot(PageRank_CoFeeding~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$PageRank_CoFeeding,col=3)
boxplot(Hub_CoFeeding~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Hub_CoFeeding,col=3)

boxplot(Degree_Aggregate~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Degree_Aggregate,col=3)
boxplot(Strength_Aggregate~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Strength_Aggregate,col=3)
boxplot(PageRank_Aggregate~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$PageRank_Aggregate,col=3)
boxplot(Hub_Aggregate~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Hub_Aggregate,col=3)

boxplot(areas50~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$areas50,col=3)
boxplot(areas95~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$areas95,col=3)
boxplot(Movt.Personality~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Movt.Personality,col=3)
boxplot(AvgDisplacementPerDay~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$AvgDisplacementPerDay,col=3)
boxplot(AvgFlightDurPerDay~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$AvgFlightDurPerDay,col=3)
boxplot(Shannon.Roost~Origin_title, data = GrandData_2019_3lyrs)
points(factor(GrandData_2019_3lyrs$Origin_title), GrandData_2019_3lyrs$Shannon.Roost,col=3)

