# Data exploration WGINOSE
# Katell Hamon - WUR katell.hamon@wur.nl
# 29/03/2022

rm(list=ls());gc()
# libraries and paths ----

library(gdata)
library(ggplot2)
library(RColorBrewer)
library(corrplot)

ext <- "jpeg"
dataPath <- "C:/Users/hamon004/OneDrive - WageningenUR/projects/2022_WGINOSE/Data/"

resultPath <- "C:/Users/hamon004/OneDrive - WageningenUR/projects/2022_WGINOSE/Results/"
if(!dir.exists(resultPath)) dir.create(resultPath)

# read in data ----
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

# correct UK
UKland <- c("SCT","WLS","NIR","ENG","CHA")
eff$FlagCountry <- as.factor(replace(as.character(eff$FlagCountry),eff$FlagCountry%in% UKland,"GBR"))
land$LandingCountry <- as.factor(replace(as.character(land$LandingCountry),land$LandingCountry %in% UKland,"GBR"))
land$FlagCountry <- as.factor(replace(as.character(land$FlagCountry),land$FlagCountry%in% UKland,"GBR"))

# check missing data ----

# effort & NULL
naeff <- subset(eff,is.na(DaysAtSea))
table(naeff$Year)
table(eff$Year)
table(naeff$Year,naeff$FlagCountry)

summary(subset(naeff,Year>2016))

summary(land)
# a lot of missing harbours
sort(unique(land$Harbour))
naHarb <- c("NULL","BE999","DE999","DEXXX","DK999","FR999","GB999","IE999","NL999")
summary(subset(land, Harbour %in% naHarb))
table(land$LandingCountry)
gc()

table(land$FlagCountry) # missing norway
table(land$Harbour,land$LandingCountry) # missing norway
harb <- as.data.frame(table(land$Harbour))

subset(harb, grepl("NO",Var1))

table(land$StatisticalRectangle)
table(land$Area)

x11()
ggplot(land,aes(x=Year,y=OfficialLandingCatchWeight_kg,fill=FlagCountry))+geom_bar(stat="identity", width=1) 
x11()
ggplot(land,aes(x=Year,y=OfficialLandingCatchWeight_kg,fill=LandingCountry))+geom_bar(stat="identity", width=1) 


# effort & NULL
naeff <- subset(eff,DaysAtSea=="NULL"|is.na(DaysAtSea))
table(naeff$Year)
table(eff$Year)
round(table(naeff$Year)/table(eff$Year)*100)
table(naeff$FlagCountry,naeff$Year)
table(naeff$Year,naeff$VesselLengthCategory)
summary(naeff)
summary(eff)

# tables Arina ----
# Select data in right region ----
# -- - - - - - - - - - - -  - - -
load(paste0(resultPath,"nsrectshape.Rdata"))
head(nsecorect@data)
effrect <- merge(eff,nsecorect@data[c("ICESNAME","AREA_KM2","PERCENTAGE")],by.x="StatisticalRectangle",by.y="ICESNAME",all.x=T)
summary(effrect)

# Select years ----
# -- -- -- --- -- 
effly <- subset(eff,Year>2016&Year<2021)
landly <- subset(land, Year>2016&Year<2021)

# correct data with % rect ----
# -- -- -- --- -- -- -- -- --
effly <- merge(effly,nsecorect@data[c("ICESNAME","AREA_KM2","PERCENTAGE")],by.x="StatisticalRectangle",by.y="ICESNAME",all.x=T)
effly <- within(effly, {FishingSoakingTime_hrs <- FishingSoakingTime_hrs * PERCENTAGE / 100 
KWDays                 <- KWDays * PERCENTAGE / 100
DaysAtSea              <- DaysAtSea * PERCENTAGE / 100

})
landly <- merge(landly,nsecorect@data[c("ICESNAME","AREA_KM2","PERCENTAGE")],by.x="StatisticalRectangle",by.y="ICESNAME",all.x=T)
landly <- within(landly, {OfficialLandingCatchWeight_kg <- OfficialLandingCatchWeight_kg * PERCENTAGE / 100 
OfficialLandingValue_Euro     <- OfficialLandingValue_Euro * PERCENTAGE / 100

})
# effort days at sea
head(eff)
#effly <- subset(eff,Year>2016)
tapply(effly$DaysAtSea, as.list(effly[c("FlagCountry","Year","VesselLengthCategory")]), FUN=sum,na.rm=T)

# landings by country
head(land)
#landly <- subset(land,Year>2016)
tapply(landly$OfficialLandingCatchWeight_kg, as.list(landly[c("FlagCountry","Year")]), FUN=sum,na.rm=T)
tapply(landly$OfficialLandingValue_Euro, as.list(landly[c("FlagCountry","Year")]), FUN=sum,na.rm=T)

tapply(landly$OfficialLandingCatchWeight_kg, as.list(landly[c("FlagCountry","Year")]), FUN=sum,na.rm=T)/tapply(landly$OfficialLandingCatchWeight_kg, as.list(landly[c("FlagCountry","Year")]), FUN=sum,na.rm=T)[,"2019"]
tapply(landly$OfficialLandingValue_Euro, as.list(landly[c("FlagCountry","Year")]), FUN=sum,na.rm=T)/tapply(landly$OfficialLandingValue_Euro, as.list(landly[c("FlagCountry","Year")]), FUN=sum,na.rm=T)[,"2019"]


mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))



x11()
ggplot(landly,aes(x=Year,y=OfficialLandingCatchWeight_kg,fill=FlagCountry))+geom_bar(stat="identity", width=1)+scale_fill_manual(values=mycolors) 
ggsave(paste0(resultPath,"landingskg_byCountries.",ext))
x11()
ggplot(landly,aes(x=Year,y=OfficialLandingValue_Euro,fill=FlagCountry))+geom_bar(stat="identity", width=1) +scale_fill_manual(values=mycolors) 
ggsave(paste0(resultPath,"landingseur_byCountries.",ext))


# effort by country
head(eff)
aggregate(effly[c("FishingSoakingTime_hrs","KWDays","DaysAtSea")],by=as.list(effly[c("Year","FlagCountry")]),FUN=sum,na.rm=T)

effg <- subset(effly, Year<2020)
tapply(effg$DaysAtSea,list(effg$FlagCountry,effg$FishingActivityCategoryEuropeanLvl5),FUN=sum,na.rm=T)
unique(effg$FishingActivityCategoryEuropeanLvl5)
passive <- c("GTR_DEF","GNS_DEF","FPO_MOL","FPO_CRU","LHP_FIF","GNS_CRU","FPN_CAT","FYK_CAT","FPN_CRU","GNS_SPF","LLS_DEF","LHM_DEF","LLS_DWS","GND_SPF","GNS_DWS","LHP_CEP","HMD_MOL","FYK_DEF","GND_DEF","LLD_DWS","FPO_FIF","LHM_FIF","LLD_DEF","SB_FIF")
active_pel <- c("PS_SPF","OTB_SPF","OTM_SPF","PTM_SPF","PTB_SPF","PTM_LPF")
active_dem <- c("OTB_DEF","TBB_DEF","TBB_CRU","OTB_CRU","OTB_MOL","OTM_DEF","OTB_MCD","SSC_DEF","DRB_MOL","PTB_DEF","PTM_DEF","SDN_DEF","PTB_CRU","OTT_MOL","OTB_DWS","TBB_MOL","OTT_MCD","OTT_CRU","OTT_DEF","OTT_DWS")

effg <- within(effg,{ Gear <- replace(as.character(FishingActivityCategoryEuropeanLvl5),FishingActivityCategoryEuropeanLvl5%in%passive,"passive")
                      Gear <- replace(Gear,Gear %in% active_pel, "active_pelagics")
                      Gear <- replace(Gear,Gear %in% active_dem,"active_demersal")
                      Gear <- as.factor(replace(Gear,Gear =="MIS_MIS","unknown"))})            

windows()
g <- ggplot(data=subset(effg,!FlagCountry %in% c("IRL","LTU","POL")),aes(x=FlagCountry,fill=Gear,weight=DaysAtSea/3000))
g+geom_bar()+xlab("Country")+ylab("thousand days at sea")+scale_fill_brewer(palette="Spectral")+theme_bw()
ggsave(paste0(resultPath,"effort_byCountries_gears.",ext),width=5,height=3)

# IRELAND effort analysis
effirl <- subset(eff,FlagCountry=="IRL")
effirl <- aggregate(effirl[c("FishingSoakingTime_hrs","KWDays","DaysAtSea")],by=as.list(effirl[c("Year","FlagCountry")]),FUN=sum,na.rm=T)
effirl <- within(effirl,{soakrate <- FishingSoakingTime_hrs/DaysAtSea
kwrate <- KWDays/DaysAtSea})   
effirl
efffr <- subset(eff,FlagCountry=="FRA")
efffr <- aggregate(efffr[c("FishingSoakingTime_hrs","KWDays","DaysAtSea")],by=as.list(efffr[c("Year","FlagCountry")]),FUN=sum,na.rm=T)
efffr <- within(efffr,{soakrate <- FishingSoakingTime_hrs/DaysAtSea
kwrate <- KWDays/DaysAtSea})   
efffr

hist(effirl$soakrate)
hist(effirl$kwrate)

#ICES rect
effnullrect <- subset(eff,StatisticalRectangle=="NULL"&Year>2016&VesselLengthCategory!="<10")
table(effnullrect$FlagCountry,effnullrect$Area)



# Make tables data quality ----
# -- -- -- -- -- -- -- -- --
# missing rectangles  DNK in Baltic <10m - LTU &POL
summary(effly)
misrectcount <- aggregate(effly["DaysAtSea"],as.list(effly[c("StatisticalRectangle","FlagCountry","Area","VesselLengthCategory")]),FUN=sum,na.rm=T)
misrectcount <- subset(misrectcount,StatisticalRectangle=="NULL")
effarea <- aggregate(effly["DaysAtSea"],as.list(effly[c("FlagCountry","Area","VesselLengthCategory")]),FUN=sum,na.rm=T)
misrectcount <- merge(misrectcount,rename.vars(effarea,"DaysAtSea","totDaysAtSea"),all=T)
misrectcount <- within(misrectcount,{norectDaysAtSea <- replace(DaysAtSea,is.na(DaysAtSea),0)
                                     percMissingdas <- round(norectDaysAtSea/totDaysAtSea*100,0)
                                     DaysAtSea      <- NULL})
subset(misrectcount,StatisticalRectangle=="NULL")

misrectAreaVL <- aggregate(misrectcount[c("norectDaysAtSea", "totDaysAtSea")],as.list(misrectcount[c("Area","VesselLengthCategory")]),FUN=sum)
misrectAreaVL <- within(misrectAreaVL,{percMissingdas <- round(norectDaysAtSea/totDaysAtSea*100,0)})
misrectAreaVL

misrectArea <- aggregate(misrectcount[c("norectDaysAtSea", "totDaysAtSea")],as.list(misrectcount[c("Area")]),FUN=sum)
misrectArea <- within(misrectArea,{percMissingdas <- round(norectDaysAtSea/totDaysAtSea*100,0)})

misrectArea

misrectAreaC <- aggregate(misrectcount[c("norectDaysAtSea", "totDaysAtSea")],as.list(misrectcount[c("Area","FlagCountry")]),FUN=sum)
misrectAreaC <- within(misrectAreaC,{percMissingdas <- round(norectDaysAtSea/totDaysAtSea*100,0)})

misrectAreaC


# make barchart x=country, fill=vessellengthwith rect or missing, facet zone

# table country year <10m/no harbours/harbour for effort and landings kg/eur
load(paste0(resultPath,"fullHarbourList_NS_2017-2020.Rdata"))
head(fullharb)
fullharb[duplicated(fullharb$Harbour),]
head(landly)
summary(landly)
landlyH <- aggregate(landly[c("OfficialLandingCatchWeight_kg","OfficialLandingValue_Euro")],by=as.list(landly[c("LandingCountry","FlagCountry","Year","Area","Harbour","HarbourDesc","VesselLengthCategory")]),FUN=sum,na.rm=T)

landlyH <- merge(landlyH,fullharb[!duplicated(fullharb$Harbour),],all.x=T)
landlyH <- within(landlyH,{ValidHarb <- !is.na(lat)
                           Above10m  <- ifelse(VesselLengthCategory %in% c("<10","NULL"),FALSE,TRUE)})

head(landlyH)
NLharb <- subset(landlyH,LandingCountry=="NLD")
NLharb <- tapply(NLharb$OfficialLandingCatchWeight_kg,list(as.character(NLharb$HarbourDesc),NLharb$Year),FUN=sum)
NLharb <- cbind(NLharb,tot=rowSums(NLharb,na.rm=T))
NLharb <- as.data.frame(NLharb[order(-NLharb[,"tot"]),])
NLharb$HarbourDesc <- row.names(NLharb)
NLharb <- merge(NLharb,fullharb[c("Description","Harbour")],by.x="HarbourDesc",by.y="Description")
write.csv(NLharb,paste0(resultPath,"landingskg_NL_harbours.csv"))

landlyH <- aggregate(landlyH[c("OfficialLandingCatchWeight_kg","OfficialLandingValue_Euro")],by=as.list(landlyH[c("LandingCountry","Year","Above10m","ValidHarb")]),FUN=sum,na.rm=T)
landlyH <- within(landlyH,{catchkgPerc <- round(OfficialLandingCatchWeight_kg/ave(OfficialLandingCatchWeight_kg,LandingCountry,Year,FUN=sum)*100)
                           catcheurPerc <- round(OfficialLandingValue_Euro/ave(OfficialLandingValue_Euro,LandingCountry,Year,FUN=sum)*100)
                           inclData    <- paste(ifelse(Above10m,"Above10m","Below10m"),ifelse(ValidHarb,"ValidHarbour","NoValidHarbour"),sep="_")
})

summary(landlyH)

landkgH <- reshape(landlyH,idvar=c("LandingCountry","Above10m","ValidHarb"),direction="wide",timevar = "Year",v.names="catchkgPerc",drop=c("OfficialLandingCatchWeight_kg","OfficialLandingValue_Euro","catcheurPerc"))

ggplot(landlyH,aes(x=Year,weight=catchkgPerc,fill=inclData))+geom_bar()+facet_wrap(~LandingCountry)+ylab("percentage catch (kg)")
ggsave(paste0(resultPath,"landingsperc_byCountries_harbours.",ext))
ggplot(landlyH,aes(x=Year,weight=OfficialLandingCatchWeight_kg,fill=inclData))+geom_bar()+facet_wrap(~LandingCountry)+ylab("catch (kg)")+labs(title ="catch per landing country")
ggsave(paste0(resultPath,"landingskg_byCountries_harbours.",ext))

# effort
efflyH <- aggregate(effly[c("DaysAtSea")],by=as.list(effly[c("FlagCountry","Year","Area","Harbour","HarbourDesc","VesselLengthCategory")]),FUN=sum,na.rm=T)

efflyH <- merge(efflyH,fullharb[!duplicated(fullharb$Harbour),],all.x=T)
efflyH <- within(efflyH,{ValidHarb <- !is.na(lat)
                         Above10m  <- ifelse(VesselLengthCategory %in% c("<10","NULL"),FALSE,TRUE)
                         LandingCountry <- substring(Harbour,1,2)})
efflyH <- aggregate(efflyH[c("DaysAtSea")],by=as.list(efflyH[c("LandingCountry","Year","Above10m","ValidHarb")]),FUN=sum,na.rm=T)
efflyH <- within(efflyH,{effortPerc <- round(DaysAtSea/ave(DaysAtSea,LandingCountry,Year,FUN=sum)*100)
inclData    <- paste(ifelse(Above10m,"Above10m","Below10m"),ifelse(ValidHarb,"ValidHarbour","NoValidHarbour"),sep="_")
                         
})

summary(efflyH)

effkgH <- reshape(efflyH,idvar=c("LandingCountry","Above10m","ValidHarb"),direction="wide",timevar = "Year",v.names="catchkgPerc",drop=c("OfficialeffingCatchWeight_kg","OfficialeffingValue_Euro","catcheurPerc"))

ggplot(efflyH,aes(x=Year,weight=effortPerc,fill=inclData))+geom_bar()+facet_wrap(~LandingCountry)+ylab("percentage effort (days at sea)")
ggsave(paste0(resultPath,"effortperc_byCountries_harbours.",ext))
ggplot(efflyH,aes(x=Year,weight=DaysAtSea,fill=inclData))+geom_bar()+facet_wrap(~LandingCountry)+ylab("effort (days at sea)")+labs(title ="Effort per landing country")
ggsave(paste0(resultPath,"effort_byCountries_harbours.",ext))

# add Norwegian data ----
# -- -- -- -- -- -- -- --
head(effly)
head(landly)
nordat <- read.csv2(file=paste0(PathIn,"ICES_Norway_catchdata.csv"),dec=".")
norhab <- subset(norhab,year>2016 &year<2021)
norhab <- within(norhab,{landingharbor_id <- as.factor(landingharbor_id)})

# Make maps ----
# -- -- -- -- --
load(paste0(resultPath,"fullHarbourList_NS_2017-2020.Rdata"))
efflyH <- merge(effly,fullharb)
efflyHVL <- aggregate(DaysAtSea ~ Year + Harbour +lon +lat +VesselLengthCategory,data=efflyH, sum,na.rm=T)
efflyHVL <- reshape(efflyHVL,idvar=c("Year","Harbour","lon","lat"),direction="wide",timevar="VesselLengthCategory",v.names="DaysAtSea")
efflyHVL <- rename.vars(efflyHVL,grep("DaysAtSea",names(efflyHVL),value=T),sapply(strsplit(grep("DaysAtSea",names(efflyHVL),value=T),"\\."),function(x) x[2]))

# choose vessel sizes
VLs <- c("10-<12","12-<18","18-<24","24-<40",">40")

# remove south american points
efflyHVL <- subset(efflyHVL,lat >0)

# choose years
yrs <- 2017:2019
efflyHVL <- subset(efflyHVL,Year %in% yrs)
efflyHVL <- aggregate(efflyHVL[VLs],by=as.list(efflyHVL[c("Harbour","lon","lat")]), sum,na.rm=T)

# calculate radius
efflyHVL$radius <- rowSums(efflyHVL[VLs],na.rm=T)


library(scatterpie)
world <- map_data('world')
p <- ggplot(world, aes(long, lat))+geom_map(map=world, aes(map_id=region), fill=NA, color="black",xlim=min) +
  coord_quickmap() 
p + geom_scatterpie(aes(x=lon, y=lat, group=Harbour,r=radius/5000),cols=VLs,
                    data=efflyHVL, alpha=.8) #+
  geom_scatterpie_legend(efflyH$DaysAtSea, x=-160, y=-55)
# Make tables ----
# -- -- -- -- --
# table landings proportion of flagged country landed in countries
tabCountr <- round(sweep(tapply(landly$OfficialLandingCatchWeight_kg, as.list(landly[c("FlagCountry","LandingCountry")]), FUN=sum,na.rm=T),1,
            tapply(landly$OfficialLandingCatchWeight_kg, as.list(landly[c("FlagCountry")]), FUN=sum,na.rm=T),"/"),2)
for (i in rownames(tabCountr)){
  if (!i %in% colnames(tabCountr)) tabCountr <- cbind(tabCountr,matrix(nrow=nrow(tabCountr),dimnames = list(dimnames(tabCountr)[[1]],i)))
}
tabCountr[is.na(tabCountr)] <- 0
tabCountr[row.names(tabCountr),row.names(tabCountr)]
X11()
corrplot(tabCountr[row.names(tabCountr),c(row.names(tabCountr),colnames(tabCountr)[which(!colnames(tabCountr) %in% row.names(tabCountr))])],col.lim=c(0,1),title = "landings in kg")
savePlot(filename = paste0(resultPath,"landingskg_countryLandings_byCountries.",ext),type=ext)
# table landings proportion of flagged country landed in countries
tabCountr <- round(sweep(tapply(landly$OfficialLandingValue_Euro, as.list(landly[c("FlagCountry","LandingCountry")]), FUN=sum,na.rm=T),1,
                         tapply(landly$OfficialLandingValue_Euro, as.list(landly[c("FlagCountry")]), FUN=sum,na.rm=T),"/"),2)
for (i in rownames(tabCountr)){
  if (!i %in% colnames(tabCountr)) tabCountr <- cbind(tabCountr,matrix(nrow=nrow(tabCountr),dimnames = list(dimnames(tabCountr)[[1]],i)))
}
tabCountr[is.na(tabCountr)] <- 0
tabCountr[row.names(tabCountr),row.names(tabCountr)]
corrplot(tabCountr[row.names(tabCountr),row.names(tabCountr)],col.lim=c(0,1),title = "landings in euro")
savePlot(filename = paste0(resultPath,"landingseuro_countryLandings_byCountries.",ext),type=ext)


# top species
top15kg <- sort(tapply(landly$OfficialLandingCatchWeight_kg,landly$Species,FUN=sum,na.rm=T),decreasing = T)[1:15]
top15eur <- sort(tapply(landly$OfficialLandingValue_Euro,landly$Species,FUN=sum,na.rm=T),decreasing = T)[1:15]

landly <- within(landly,{topspp <- ifelse(Species %in% unique(c(names(top15kg),names(top15eur))),as.character(Species),"Other")
                         topspp <- factor(topspp,levels=c(unique(c(names(top15kg),names(top15eur))),"Other"))})

ggplot(landly,aes(x=Year,weight=OfficialLandingCatchWeight_kg,fill=topspp))+geom_bar()+facet_wrap(~LandingCountry)+ylab("Species landings")
ggsave(paste0(resultPath,"landingspp_byCountries.",ext))

ggplot(landly,aes(x=Year,weight=OfficialLandingCatchWeight_kg,fill=topspp))+geom_bar(position="fill")+facet_wrap(~LandingCountry)+ylab("Species landings")
ggsave(paste0(resultPath,"landingspp_prop_byCountries.",ext))


ggplot(landly,aes(x=Year,weight=OfficialLandingValue_Euro,fill=topspp))+geom_bar()+facet_wrap(~LandingCountry)+ylab("Species landings (euro)")
ggsave(paste0(resultPath,"landingspp_euro_byCountries.",ext))

ggplot(landly,aes(x=Year,weight=OfficialLandingValue_Euro,fill=topspp))+geom_bar(position="fill")+facet_wrap(~LandingCountry)+ylab("Species landings (euro)")
ggsave(paste0(resultPath,"landingspp_euro_prop_byCountries.",ext))

# per area
ggplot(landly,aes(x=Year,weight=OfficialLandingCatchWeight_kg,fill=topspp))+geom_bar()+facet_wrap(~Area)+ylab("Species landings")
ggsave(paste0(resultPath,"landingspp_byArea.",ext))

ggplot(landly,aes(x=Year,weight=OfficialLandingCatchWeight_kg,fill=topspp))+geom_bar(position="fill")+facet_wrap(~Area)+ylab("Species landings")
ggsave(paste0(resultPath,"landingspp_prop_byArea.",ext))


ggplot(landly,aes(x=Year,weight=OfficialLandingValue_Euro,fill=topspp))+geom_bar()+facet_wrap(~Area)+ylab("Species landings (euro)")
ggsave(paste0(resultPath,"landingspp_euro_byArea.",ext))

ggplot(landly,aes(x=Year,weight=OfficialLandingValue_Euro,fill=topspp))+geom_bar(position="fill")+facet_wrap(~Area)+ylab("Species landings (euro)")
ggsave(paste0(resultPath,"landingspp_euro_prop_byArea.",ext))
