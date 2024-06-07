# compare the harbour names and link datasets
# WGINOSE 
# Katell Hamon Wageningen Economic Research

rm(list=ls());gc()

# libraries
library(vmstools)
library(gdata)
library(sp)
library(ggmap)

setwd("S:/Onderzoek/Projecten/Lopende projecten/2022 WGINOSE//")

PathIn <- "./Data/"
PathOut <- "./Results/"

# additional function get map of 
showmap <- function(country,df){
  map <- get_googlemap(country, markers = df, path = df, scale = 2)
  ggmap(map, extent = 'device')
  
}

# uk habours from ARina
ukharbs <- read.csv(paste0(PathIn,"Ports_coordinates_UK.csv"))
str(ukharbs)
head(ukharbs)
ukharbs <- within(ukharbs,{harb <- tolower(Port)})

# RDB harbour descriptions
RDBharbs <- read.csv(paste0(PathIn,"RDB Harbour LOCODE code list.csv"))

head(RDBharbs)
subset(RDBharbs,Harbour..LOCODE.=="FRBM7")

# harbours in the effort and landings data
land <- read.csv(paste0(dataPath,"2022 06 21 WGECON RDB CL data FACEUlvl5.csv"))
eff  <- read.csv(paste0(dataPath,"2022 06 21 WGECON RDB CE Effort data FACEUlvl5.csv"))

summary(land)
summary(eff)

# rename landing COuntry
names(land)[1] <- "LandingCountry"
names(eff)[1] <- "FlagCountry"

# transform character in numeric 
for (i in c("FishingSoakingTime_hrs","KWDays","DaysAtSea")){
  eff[,i] <- as.numeric(eff[,i])
}
for (i in c("OfficialLandingCatchWeight_kg","OfficialLandingValue_Euro")){
  land[,i] <- as.numeric(land[,i])
}

# transform character in factors 
for(i in names(land)[sapply(land,class)=="character"])
  land[,i] <- as.factor(land[,i])

for(i in names(eff)[sapply(eff,class)=="character"])
  eff[,i] <- as.factor(eff[,i])

land

# make the harbour list to clean up in next script.
harbnam <- unique(eff[c("Harbour","HarbourDesc","Year")])

harbnam <- merge(harbnam, unique(land[c("LandingCountry","Harbour","HarbourDesc")]),all=T)
#save(harbnam,file=paste0(resultPath,"harbournames.Rdata"))
#load(paste0(PathOut,"harbournames.Rdata")) 
summary(harbnam)
  # select years of interest
yrs <- 2017:2021
harbnam <- subset(harbnam, Year %in%yrs)
harbdf <- unique(harbnam[c("Harbour","HarbourDesc")]) #1059 harbours

# vmstools harbours
data("harbours")
head(harbours)
harbours <- within(harbours,harb <- tolower(harbour))

fullharb <- data.frame()


# 1 - find harbours in rdb in one of the other db
sum(duplicated(harbdf$Harbour)) # one code one description

harbdf <- merge(harbdf["Harbour"],RDBharbs,by.x="Harbour",by.y="Harbour..LOCODE.",all.x=T)
harbdf <- within(harbdf,harb <- tolower(Description))
nomatch <- harbdf 
fullharbnam <- c("Harbour","Description","lon","lat","range")

# overlap uk data
harbdfuk <- merge(nomatch,ukharbs,all.x=T) # missing 667 ports
summary(harbdfuk)

nomatch <- subset(harbdfuk,is.na(Port),c(harb,Harbour,Description))

harbdfuk <- subset(harbdfuk,!is.na(Port),c(Harbour,Description,Longitude,Latitude))
harbdfuk <- within(harbdfuk,{lon <- Longitude
                             lat <- Latitude
                             range <- NA
})

fullharb <- rbind(fullharb,harbdfuk[fullharbnam])
dup1 <- fullharb[fullharb$Harbour %in% fullharb[duplicated(fullharb$Harbour),"Harbour"],]

# overlap vmstools data
harbdfvms <- merge(nomatch,harbours,all.x=T) # missing 346 ports
summary(harbdfvms)
nomatch <- subset(harbdfvms,is.na(lon),c(harb,Harbour,Description))

fullharb <- rbind(fullharb,subset(harbdfvms,!is.na(lon),c(Harbour,Description,lon,lat,range)))

# check for duplicates
dup2 <- fullharb[fullharb$Harbour %in% fullharb[duplicated(fullharb$Harbour),"Harbour"],]
dup2$duplicate <- NA

for (i in unique(dup2$Harbour)){
  dup2[dup2$Harbour==i,"duplicate"] <- ifelse(all(spDists(as.matrix(dup2[dup2$Harbour==i,c("lat","lon")]),longlat=T)<5),T,F)  
}
if (nrow(unique(subset(dup2,!duplicate,Harbour)))>0){
  print(subset(dup2,!duplicate))
  # df <- round(data.frame(
  #   x = subset(dup2,!duplicate)$lon,
  #   y = subset(dup2,!duplicate)$lat
  # ), digits = 2)
  # showmap("Germany",df)
}

# if less than 5km apart choose the first one
fullharb <- subset(fullharb,!(substring(Harbour,1,2)=="DE"&lon>15))
fullharb <- fullharb[!duplicated(fullharb$Harbour),]

# 2- fuzzy match
library(fuzzyjoin); library(dplyr);

 # ... with vmstools
fuzzharb <- stringdist_join(nomatch, harbours, 
                by = "harb",
                mode = "left",
                ignore_case = FALSE, 
                method = "jw", 
                max_dist = 99, 
                distance_col = "dist") %>%
  group_by(harb.x) %>%
  slice_min(order_by = dist, n = 1)
write.csv(fuzzharb,file=paste0(PathOut,"fuzzymatchVMS_1.csv"))
# check by hand, add a column "check" and put 1 when validated

fuzzyharb_chk <- read.csv(paste0(PathOut,"fuzzymatchVMS_1_chkd.csv"))
fullharb <- rbind(fullharb,subset(fuzzyharb_chk, check==1)[names(fullharb)])

# check for duplicates
dup3 <- fullharb[fullharb$Harbour %in% fullharb[duplicated(fullharb$Harbour),"Harbour"],]
dup3$duplicate <- NA

for (i in unique(dup3$Harbour)){
  dup3[dup3$Harbour==i,"duplicate"] <- ifelse(all(spDists(as.matrix(dup3[dup3$Harbour==i,c("lat","lon")]),longlat=T)<5),T,F)  
}
if (nrow(unique(subset(dup3,!duplicate,Harbour)))>0){
  print(subset(dup3,!duplicate))
}
fullharb <- subset(fullharb,!(duplicated(Harbour)& Harbour %in% subset(dup3,duplicate)$Harbour))
fullharb <- subset(fullharb,!(Harbour=="GBESH"&lat>51))
fullharb <- subset(fullharb,!(Harbour=="GBBIF"&lat>54))

nomatch <- rename.vars(unique(subset(fuzzyharb_chk, check==0 &!Harbour %in% fullharb$Harbour,c(harb.x,Harbour,Description))),"harb.x","harb")


# ... ukharbours
fuzzharb <- stringdist_join(nomatch, ukharbs, 
                            by = "harb",
                            mode = "left",
                            ignore_case = FALSE, 
                            method = "jw", 
                            max_dist = 99, 
                            distance_col = "dist") %>%
  group_by(harb.x) %>%
  slice_min(order_by = dist, n = 1)
write.csv(fuzzharb,file=paste0(PathOut,"fuzzymatchVMS_2.csv"))
# check by hand, add a column "check" and put 1 when validated

fuzzyharb_chk <- read.csv(paste0(PathOut,"fuzzymatchVMS_2_chkd.csv"))
fuzzyharb_chk <- within(fuzzyharb_chk,{lon <- Longitude
                                       lat <- Latitude
                                       range <- NA
})

fullharb <- rbind(fullharb,subset(fuzzyharb_chk, check==1)[names(fullharb)])

# check for duplicates
dup4 <- fullharb[fullharb$Harbour %in% fullharb[duplicated(fullharb$Harbour),"Harbour"],]
dup4$duplicate <- NA

for (i in unique(dup4$Harbour)){
  dup4[dup4$Harbour==i,"duplicate"] <- ifelse(all(spDists(as.matrix(dup4[dup4$Harbour==i,c("lat","lon")]),longlat=T)<5),T,F)  
}
if (nrow(unique(subset(dup4,!duplicate,Harbour)))>0){
  print(subset(dup4,!duplicate))
}
fullharb <- subset(fullharb,!(duplicated(Harbour)& Harbour %in% subset(dup4,duplicate)$Harbour))

nomatch <- rename.vars(unique(subset(fuzzyharb_chk, check==0,c(harb.x,Harbour,Description))),"harb.x","harb")

nomatch <- nomatch[order(nomatch$Description),]
write.csv(nomatch,file=paste0(PathOut,"nomatch.csv"))

# x <- "ng"
# harbours[grepl(x,row.names(harbours),ignore.case=T),]
# ukharbs[grepl(x,ukharbs$Port,ignore.case=T),]
nomatch_chk <- read.csv(paste0(PathOut,"nomatch_handfilled.csv"))
nomatch_chk <- rename.vars(nomatch_chk,"X.1","Harbour")
fullharb <- rbind(fullharb,nomatch_chk[names(fullharb)])
summary(fullharb)

# check for duplicates
dup5 <- fullharb[fullharb$Harbour %in% fullharb[duplicated(fullharb$Harbour),"Harbour"],]
dup5$duplicate <- NA

for (i in unique(dup5$Harbour)){
  dup5[dup5$Harbour==i,"duplicate"] <- ifelse(all(spDists(as.matrix(dup5[dup5$Harbour==i,c("lat","lon")]),longlat=T)<5),T,F)  
}
if (nrow(unique(subset(dup5,!duplicate,Harbour)))>0){
  print(subset(dup5,!duplicate))
}
fullharb <- subset(fullharb,!(duplicated(Harbour)& Harbour %in% subset(dup5,duplicate)$Harbour))

write.csv(fullharb,file=paste0(PathOut,"fullHarbourList_NS_2017-2020.csv"),row.names = F)

save(fullharb,file=paste0(PathOut,"fullHarbourList_NS_2017-2020.Rdata"))

summary(fullharb)

