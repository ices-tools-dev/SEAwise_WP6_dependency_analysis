# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# +++                                                      +++
# +++          EXPLORING RDBES DATA                        +++
# +++                                                      +++
# +++           Authors: Jochen Depestele                  +++
# +++           Creation date: 27 June 2024                +++
# +++                                                      +++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# 
rm(list=ls())
tosave=F
#
# SET-UP ####
## Libraries ----
library(data.table)
library(ggplot2)
# library(mapmixture)
library(gridExtra)
library(dplyr)
library(scatterpie)
library(mapplots)

## Directories ----
OneDrivedir <- file.path(Sys.getenv("OneDrive"))
datadir <- file.path(paste0(OneDrivedir,"/000_data"))
outdir <- c("./output")
indir <- c("./input")
igndir <- c("./drafts_to_be_ignored_for_git")

## Functions and preferences ----
theme_set(theme_bw())

# LOAD DATA ####
lst_files <- list.files(paste0(datadir,"/fisheries/CONFIDENTIAL_SEAwise_landings_effort"),
                        pattern = ".csv$",  # file names ending with csv
                        recursive = T,      # search subdirectories
                        full.names = T)     # return the full path
##CE data ----
##> reading ce data ====
ce <- lapply(lst_files[grepl("CE ",lst_files)], fread)
setnames(ce[[1]],
         names(ce[[1]]),
         c("CEvesselFlagCountry", "CEyear", "CEarea", "CEstatisticalRectangle", "CEnationalFishingActivity", 
           "FishingActivityCategoryEuropeanLvl5", # old name
           "CEmetier6", "CElandingLocation", "landingLocationHarbourDesc", "CEvesselLengthCategory", 
           "FishingTimeHoursSum", # old name, no match in RDBES
           "CEofficialkWDaysAtSeaSum", "CEofficialDaysAtSeaSum")) # KWDaysSum and DaysAtSeaSum are assumed the official days?
numcols <- c("CEyear",
             "FishingTimeHoursSum",
             "CEofficialDaysAtSeaSum", "CEscientificDaysAtSeaSum", "CEofficialFishingDaysSum", "CEscientificFishingDaysSum", "CEofficialkWDaysAtSeaSum", 
             "CEscientifickWDaysAtSeaSum")
ce <- lapply(ce, function(dtx){
  selcols_dtx <- numcols[numcols %in% colnames(dtx)]
  dtx[dtx=="NULL"] <- NA # avoids the warnings message in converting "NULL" characters to numeric NAs
  dtx[,(selcols_dtx):= lapply(.SD, as.numeric), .SDcols = selcols_dtx]
  return(dtx)})
ce <- rbindlist(ce, use.names=T, fill=T)

##> Converting hours to days ====
# => only for 2020
table(ce[is.na(CEscientificFishingDaysSum)]$CEyear, useNA="always")
# Conversion of fishing hours to days is only done when there are no fishing days reported 
table(ce[is.na(CEscientificFishingDaysSum),]$CEscientificFishingDaysSum, useNA="always") # 38882 NA records
ce[is.na(CEscientificFishingDaysSum),]$CEscientificFishingDaysSum <- round(ce[is.na(CEscientificFishingDaysSum),]$FishingTimeHoursSum/24,2)
table(ce[is.na(CEscientificFishingDaysSum),]$CEscientificFishingDaysSum, useNA="always") # 33098 NA records

##CL data ----
cl <- suppressWarnings(lapply(lst_files[grepl("CL ",lst_files)], fread)) # RDB data contains one single-line footer that can be omitted
setnames(cl[[1]],
         names(cl[[1]]),
         c("CLlandingCountry", "CLvesselFlagCountry", "CLyear", "CLarea","CLstatisticalRectangle", 
           "CLspeciesCode", "CLspeciesCodeDescription", 
           "SpeciesDesc", # old name from RDB
           "CLnationalFishingActivity", 
           "FishingActivityCategoryEuropeanLvl5", # old name from RDB
           "CLmetier6", "CLlandingLocation", "landingLocationHarbourDesc", 
           "CLvesselLengthCategory", "CLofficialWeightSum", "CLLandingsValueSum"))
numcols <- c("CLyear",
             "CLofficialWeightKg", "CLscientificWeightKg", "CLLandingsValueSum")
cl <- lapply(cl, function(dtx){
  selcols_dtx <- numcols[numcols %in% colnames(dtx)]
  dtx[dtx=="NULL"] <- NA # avoids the warnings message in converting "NULL" characters to numeric NAs
  dtx[,(selcols_dtx):= lapply(.SD, as.numeric), .SDcols = selcols_dtx]
  return(dtx)})
cl <- rbindlist(cl, use.names=T, fill=T)
setnames(cl,c("CLofficialWeightSum", "CLscientificWeightSum"),
         c("CLofficialWeightKg", "CLscientificWeightKg"))

# EXPLORE DATA: official vs scientific ####
# LET's ONLY USE THE OFFICIAL DATA FOR 2021+2022 (based on underpinning below)
## Region ----
sort(unique(ce$CEarea))
CSdivs <- c("27.7.b", "27.7.c", "27.7.c.1", "27.7.c.2", 
            "27.7.e", "27.7.f", "27.7.g", "27.7.h", 
            "27.7.j", "27.7.j.2", "27.7.k", "27.7.k.1", "27.7.k.2")


## CE Effort: official vs scientific data ----
# => CONCLUSION: USE THE OFFICIAL REPORTED EFFORT + let's only work with 2021+2022.
##> kW Days at Sea ====
ce[,diff_offsci_kWDaysAtSeaSum := CEofficialkWDaysAtSeaSum - CEscientifickWDaysAtSeaSum]
ce[,diffcat_offsci_kWDaysAtSeaSum := ifelse(diff_offsci_kWDaysAtSeaSum!=0, "diff","match")]
table(ce$diffcat_offsci_kWDaysAtSeaSum,useNA = "always")
# diff  match   <NA> 
# 884 102515  38882 
table(ce$CEarea, ce$diffcat_offsci_kWDaysAtSeaSum, useNA = "always")
table(ce[CEarea %in% CSdivs]$diffcat_offsci_kWDaysAtSeaSum,useNA = "always")
# No match in the North Sea, but only 1 record differs between official and scientific data in the Celtic Sea
# diff match  <NA> 
#    1 20928  9649

##> Days at Sea ====
# Same conclusions as for kW*days at sea => no differences between official and scientific datasets in Celtic Seas FLBEIA region
ce[,diff_offsci_DaysAtSeaSum := CEofficialDaysAtSeaSum - CEscientificDaysAtSeaSum]
ce[,diffcat_offsci_DaysAtSeaSum := ifelse(diff_offsci_DaysAtSeaSum!=0, "diff","match")]
table(ce$diffcat_offsci_DaysAtSeaSum,useNA = "always")
table(ce$CEarea, ce$diffcat_offsci_DaysAtSeaSum, useNA = "always")
table(ce[CEarea %in% CSdivs]$diffcat_offsci_DaysAtSeaSum,useNA = "always")

##> Fishing days ====
# All Fishing days are matching, but 2020 only has NAs, because
# we converted Fishing hours to days, and accounted them only in the scientific column
ce[,diff_offsci_FishingDaysSum := CEofficialFishingDaysSum - CEscientificFishingDaysSum]
ce[,diffcat_offsci_FishingDaysSum := ifelse(diff_offsci_FishingDaysSum!=0, "diff","match")]
unique(ce$diffcat_offsci_FishingDaysSum)
table(ce$diffcat_offsci_FishingDaysSum,useNA = "always")
table(ce$CEarea, ce$diffcat_offsci_FishingDaysSum, useNA = "always")
table(ce[CEarea %in% CSdivs]$diffcat_offsci_FishingDaysSum,useNA = "always")
table(ce$CEyear, ce$diffcat_offsci_FishingDaysSum, useNA = "always")
# There are no official Fishing Days in 2020
ce[,
   .(length(which(!is.na(CEscientificFishingDaysSum))),
     length(which(is.na(CEscientificFishingDaysSum)))), # Many NAs in 2020!
   by=.(CEyear)]
ce[,
   .(length(which(!is.na(CEofficialFishingDaysSum))),
     length(which(is.na(CEofficialFishingDaysSum)))),
   by=.(CEyear)]

##> rm all added groups ====
rmcols <- names(ce)[grepl("diff",names(ce))]
ce[,c(rmcols) := NULL]

## CL Landings: official vs scientific data ----
# => CONCLUSION: USE THE OFFICIAL LANDED WEIGHT.
##> Landings weight ====
# There is a perfect match for all regions in 2021 and 2022, but there is no scientific weight in 2020
cl[,diff_offsci_Weight := CLofficialWeightKg - CLscientificWeightKg]
cl[,diffcat_offsci_Weight := ifelse(diff_offsci_Weight!=0, "diff","match")]
table(cl$diffcat_offsci_Weight,useNA = "always")
# match   <NA>  
# 956462 317500
table(cl$CLyear, cl$diffcat_offsci_Weight, useNA = "always")
cl[,
   .(length(which(!is.na(CLofficialWeightKg))),
     length(which(is.na(CLofficialWeightKg)))), # Less records in 2020 but none are NA
   by=.(CLyear)]
cl[,
   .(length(which(!is.na(CLscientificWeightKg))), # There is NO scientific weight in 2020 !
     length(which(is.na(CLscientificWeightKg)))), 
   by=.(CLyear)]
cl[,
   sum(CLofficialWeightKg, na.rm=T)/1000, # Total tonnes landed differs at max by 100k between years
   by=.(CLyear)]

##> rm all added groups ====
rmcols <- names(cl)[grepl("diff",names(cl))]
cl[,c(rmcols) := NULL]


# RESTRICT DATA to Celtic seas ####
## Restrict to Celtic Seas region ----
cecs <- ce[CEarea %in% CSdivs]
clcs <- cl[CLarea %in% CSdivs]
## Restrict to 2021 and 2022 ----
selyears <- c(2021:2022)
cecs <- cecs[CEyear %in% selyears]
clcs <- clcs[CLyear %in% selyears]

# EXPLORE DATA: Spatial exploration of 2021-2022 official data ####
## Select FLBEIA spp ----
clcs[,CLfl:=paste(CLvesselFlagCountry, CLvesselLengthCategory, sep="_&_")]
clcs[,CLmetfl:=paste(CLfl, CLmetier6, sep="_&_")]
selFLBEIAspp <- c("COD","HAD","HKE","MEG","MON","SOL","WHG","NEP")
selFLBEIAsppCode <- c(126438, 126555, 127160, 126436, 126437, 126484, 127146, 107254) # c("COD","HAD","HKE","MEG","MON","SOL","WHG","NEP")
clcs[, CLflbeiasppCode := ifelse(CLspeciesCode %in%selFLBEIAsppCode, CLspeciesCode, "OTH")]
selcols <- c("CLspeciesCode", "CLspeciesFaoCode")
sppFAO <- unique(clcs[CLflbeiasppCode %in% selFLBEIAsppCode,..selcols][order(CLspeciesCode)]) # there are NAs for FaoCode of FLBEIAspp
sppFAO <- sppFAO[!is.na(CLspeciesFaoCode)]
setkey(sppFAO, CLspeciesCode)
setkey(clcs, CLspeciesCode)
clcs[i=sppFAO, CLspeciesFaoCode:= i.CLspeciesFaoCode]
unique(clcs[CLflbeiasppCode %in% selFLBEIAsppCode,..selcols][order(CLspeciesCode)]) # there are no NAs for FaoCode of FLBEIAspp

clcs[, CLflbeiaspp := ifelse(CLspeciesFaoCode %in% selFLBEIAspp, CLspeciesFaoCode, "OTH")]

clcs_flbeia <- clcs[,
                    .(lan_kg = sum(CLofficialWeightKg, na.rm=T)),
                    by=.(CLyear,CLstatisticalRectangle, CLmetfl, CLflbeiaspp)]
clcs_flbeia_wide <- dcast(clcs_flbeia, CLyear+CLstatisticalRectangle+CLmetfl ~ CLflbeiaspp, value.var = "lan_kg")

clcs_flbeia_wide[, all_flbeiaspp_na := rowSums(is.na(.SD)) == length(selFLBEIAspp), .SDcols = selFLBEIAspp]
clcs_flbeia_wide[, CLmetfl_flbeia := ifelse(all_flbeiaspp_na, "OTH", CLmetfl)] # All metfl without flbeiaspp are collapsed to "other" metier-fleets
table(ifelse(clcs_flbeia_wide$CLmetfl_flbeia=="OTH","OTH","FBLEIA_metfleets"))
# FBLEIA_metfleets              OTH 
#             4341             4853
clcs_flbeia_wide <- clcs_flbeia_wide[,
                                     lapply(.SD, sum),
                                     by=.(CLyear,CLstatisticalRectangle, CLmetfl_flbeia),
                                     .SDcols=c(selFLBEIAspp,"OTH")]
table(ifelse(clcs_flbeia_wide$CLstatisticalRectangle=="-9","rects_MISSING","rects"))

# The missing rectangles only account for <1% of the landings for any of the species
# so we can remove the missing rectangles from the Celtic Seas analysis
clcs_flbeia_wide %>%
  mutate(rectMISSING=ifelse(CLstatisticalRectangle=="-9","missing","rectOK")) %>%
  group_by(CLyear, rectMISSING) %>%
  summarise(across(all_of(c(selFLBEIAspp,"OTH")), function(x)sum(x,na.rm=T))) %>%
  tidyr::pivot_longer(cols=all_of(c(selFLBEIAspp,"OTH"))) %>%
  tidyr::pivot_wider(names_from=rectMISSING) %>%
  mutate(perc=missing/rectOK*100)

## Pie maps by rect ----
clcs_flbeia_wide <- clcs_flbeia_wide[CLstatisticalRectangle!="-9"]
setorderv(clcs_flbeia_wide, cols=c("CLyear","CLstatisticalRectangle","CLmetfl_flbeia"))
selspp <- c(selFLBEIAspp,"OTH")
clcs_flbeia_wide <- clcs_flbeia_wide %>% gtools::na.replace(0)

selcols <- c("CLyear","CLstatisticalRectangle")
clcs_rects <- clcs_flbeia_wide[,..selcols]
clcs_rects[,`:=`(Lat=mapplots::ices.rect(CLstatisticalRectangle)[,2],
                 Lon=mapplots::ices.rect(CLstatisticalRectangle)[,1])]


agglvl <- c("met5", "CLmetfl_flbeia", "country","loa","met6")
temp <- copy(clcs_flbeia_wide)
temp[,c("country","loa","met6") := tstrsplit(CLmetfl_flbeia,"_&_",fixed=T)]
temp[CLmetfl_flbeia=="OTH",c("loa","met6"):="OTH"]
temp[,met5 := ifelse(met6=="OTH","OTH",substr(met6,1,7))]
# unique(temp$met5)


## Pie maps by harbours ----
ports <- get(load("./harbours_with_coordinates.RData")) # Results from 02_harbour_mapping.R (not provided)
setDT(ports)

unique(tolower(clcs$landingLocationHarbourDesc)[!tolower(clcs$landingLocationHarbourDesc) %in% ports$harbour_to])

clcs[,landingLocationHarbourDesc := tolower(landingLocationHarbourDesc)]
setkey(clcs, landingLocationHarbourDesc)
setnames(ports, "harbour_to", "landingLocationHarbourDesc")
setkey(ports, landingLocationHarbourDesc)
clcs <- ports[check==T][clcs]

clcs_flbeia2 <- clcs[,
                     .(lan_kg = sum(CLofficialWeightKg, na.rm=T)), # CLscientificWeightKg
                     by=.(CLyear, lon, lat, CLstatisticalRectangle, landingLocationHarbourDesc, CLmetfl, CLflbeiaspp)]
clcs_flbeia_port <- dcast(clcs_flbeia2, CLyear+lon+lat+landingLocationHarbourDesc+CLstatisticalRectangle+CLmetfl ~ CLflbeiaspp, 
                          value.var = "lan_kg")
clcs_flbeia_port[, all_flbeiaspp_na := rowSums(is.na(.SD)) == length(selFLBEIAspp), .SDcols = selFLBEIAspp]

clcs_flbeia_port[, CLmetfl_flbeia := ifelse(all_flbeiaspp_na, "OTH", CLmetfl)] # All metfl without flbeiaspp are collapsed to "other" metier-fleets
table(ifelse(clcs_flbeia_port$CLmetfl_flbeia=="OTH","OTH","FBLEIA_metfleets"))
# FBLEIA_metfleets              OTH
#             9384            11832

table(clcs_flbeia_port$all_flbeiaspp_na, substr(clcs_flbeia_port$CLmetfl,1,2)) # => All irish vessels were kicked out when using FAO codes only
# BE   DE   DK   ES   FR   GB   GG   IE   JE   LT   NL   PL
# FALSE  555   12   12  850 6226 2434    4 2741   16    0  124    1
# TRUE     1   92   81  306 4424 1902   45 1237   17    8  111   17

sort(unique(substr(clcs_flbeia_port$CLmetfl_flbeia,1,2))) # Ireland has disappeared !!!

clcs_flbeia_port <- clcs_flbeia_port[,
                                     lapply(.SD, sum),
                                     by=.(CLyear,lon,lat,CLstatisticalRectangle, landingLocationHarbourDesc, CLmetfl_flbeia),
                                     .SDcols=c(selFLBEIAspp,"OTH")]
length(unique(clcs_flbeia_port$landingLocationHarbourDesc)) # 455 described ports

table(ifelse(clcs_flbeia_port$CLstatisticalRectangle=="-9","rects_MISSING","rects"))
# The missing rectangles only account for <1% of the landings for any of the species
# so we can remove the missing rectangles from the Celtic Seas analysis
clcs_flbeia_port %>%
  mutate(rectMISSING=ifelse(CLstatisticalRectangle=="-9","missing","rectOK")) %>%
  group_by(CLyear, rectMISSING) %>% # , CLstatisticalRectangle
  summarise(across(all_of(c(selFLBEIAspp,"OTH")), function(x)sum(x,na.rm=T))) %>%
  tidyr::pivot_longer(cols=all_of(c(selFLBEIAspp,"OTH"))) %>%
  tidyr::pivot_wider(names_from=rectMISSING) %>%
  mutate(perc=missing/rectOK*100)

clcs_flbeia_port <- clcs_flbeia_port[CLstatisticalRectangle != "-9"]
unique(clcs_flbeia_port[(is.na(lat)|is.na(lon)),]$landingLocationHarbourDesc)
colSums(clcs_flbeia_port[(is.na(lat)|is.na(lon)),..selFLBEIAspp],na.rm=T) / colSums(clcs_flbeia_port[!(is.na(lat)|is.na(lon)),..selFLBEIAspp],na.rm=T)*100
# => 4% of cod is landed in missing harbours => Add the harbours and save the object
missport <- readRDS("./missing_harbours_ChatGPT4_20240619_vMod01.RDS") # Cite: OpenAI. (2024). Personal communication with ChatGPT (GPT-4) on 19 June 2024.
missport[,port:=gsub(" $","",port)]
missport[,port:=tolower(port)]
# Add some manually:
clcs_flbeia_port[grepl("sidm", landingLocationHarbourDesc)]$lat <- as.numeric(missport[port=="sidmouth"]$lat)
clcs_flbeia_port[grepl("sidm", landingLocationHarbourDesc)]$lon <- as.numeric(missport[port=="sidmouth"]$lon)
clcs_flbeia_port[grepl("portscatho", landingLocationHarbourDesc)]$lat <- as.numeric(missport[port=="portscatho"]$lat)
clcs_flbeia_port[grepl("portscatho", landingLocationHarbourDesc)]$lon <- as.numeric(missport[port=="portscatho"]$lon)
# Add some from GPT-4:
setkey(missport, port)
setkey(clcs_flbeia_port, landingLocationHarbourDesc)
missport <- missport[clcs_flbeia_port[(is.na(lat)|is.na(lon)),"landingLocationHarbourDesc"]]
clcs_flbeia_port[is.na(lat), lat := as.numeric(missport$lat)]
clcs_flbeia_port[is.na(lon), lon := as.numeric(missport$lon)]
100*colSums(clcs_flbeia_port[(is.na(lat)|is.na(lon)),..selFLBEIAspp],na.rm=T) / colSums(clcs_flbeia_port[!(is.na(lat)|is.na(lon)),..selFLBEIAspp],na.rm=T)
# => <1% is landed in missing harbours
# => check port descriptions which do NOT have lat-lon:
unique(clcs_flbeia_port[(is.na(lat)|is.na(lon)),]$landingLocationHarbourDesc)

clcs_flbeia_port <- clcs_flbeia_port[!(is.na(lat)|is.na(lon)),] # remove the "ports" with no lat-lon

agglvl <- c("met5", "CLmetfl_flbeia", "country","loa","met6", "CLstatisticalRectangle") # , "landingLocationHarbourDesc"
temp <- copy(clcs_flbeia_port)
temp[,c("country","loa","met6") := tstrsplit(CLmetfl_flbeia,"_&_",fixed=T)]
temp[CLmetfl_flbeia=="OTH",c("loa","met6"):="OTH"]
temp[,met5 := ifelse(met6=="OTH","OTH",substr(met6,1,7))]
# unique(temp$met5)

temp[,`:=`(lat_rect=mapplots::ices.rect(CLstatisticalRectangle)[,2],
           lon_rect=mapplots::ices.rect(CLstatisticalRectangle)[,1])]
setnames(temp, c("lon","lat"), paste(c("lon","lat"),"harbour",sep="_"))

##> MAPPING ====
for(selloc in c("harbour","rect")){
  cat(paste("*\n**\nMAPPING LANDINGS FOR",selloc,"\n\n"))
  temp_loc <- copy(temp)
  setnames(temp_loc,paste(c("lon","lat"),selloc,sep="_"), c("lon","lat"))
  for(selagg in c("all", "country","loa","met5")){
    cat(paste(" \nMapping",selagg,"for",selloc,"landings\n"))
    if(selagg=="all"){
      selcat <- c("CLyear","lon","lat", ifelse(selloc=="harbour","landingLocationHarbourDesc","CLstatisticalRectangle")) # selagg=="all" => no additional aggregation level
    }else{
      selcat <- c("CLyear","lon","lat", ifelse(selloc=="harbour","landingLocationHarbourDesc","CLstatisticalRectangle"),selagg)
    }
    for(sppfocus in c(1,2)){
      selspp <- list(selFLBEIAspp,c(selFLBEIAspp,"OTH"))[[sppfocus]]
      cat(paste("  => Mapping",ifelse(sppfocus==1,"FLBEIAspp","FLBEIA_&_OTHspp"),"for",selagg,"of",selloc,"landings\n"))
      temp_agg <- temp_loc[,
                           lapply(.SD,function(x)sum(x,na.rm=T)),
                           by=selcat, # Aggregate all landings by year, rectangle and aggregation level
                           .SDcols = selspp] # focus on FLBEIA spp with or without 'other' spp
      
      for(whichyear in c(2021,2022)){
        temp_agg2 <- temp_agg[CLyear==whichyear]
        if(selagg=="all"){
          allcats <- "all"
        }else{
          allcats <- unique(temp_agg2[[selagg]])
        }
        for(whichcat in allcats){
          if(selagg=="all"){
            toplot <- temp_agg2
          }else{
            toplot <- temp_agg2[which(temp_agg2[[selagg]]==whichcat)]
          }
          selcols <- c(selcat, selspp)
          toplot <- toplot[,..selcols]
          
          toplot <- toplot[rowSums(toplot[,..selspp])>0]
          if(selloc=="harbour"){
            seltop <- round(length(unique(toplot$landingLocationHarbourDesc))/3, 0) # top 30% ports in terms of quantities landed for this sub-category
            lanperc_topports <- sum(sort(rowSums(toplot[,..selspp], na.rm=T),decreasing=T)[1:seltop], na.rm=T)/
              sum(sort(rowSums(temp_agg2[,..selspp], na.rm=T),decreasing=T), na.rm=T)*100 # X% of total landings
          }else{
            seltop<-1
            lanperc_topports <- sum(toplot[,..selspp])/sum(temp_agg2[,..selspp])*100
          }
          
          if(lanperc_topports>1){
            
            toplot_top <- toplot[rowSums(toplot[,..selspp]) %in% sort(rowSums(toplot[,..selspp]),decreasing=T)[1:seltop]] # top 25 ports in landings
            toplot_nottop <- toplot[!rowSums(toplot[,..selspp]) %in% sort(rowSums(toplot[,..selspp]),decreasing=T)[1:seltop]] # top 25 ports in landings
            lantot_top <- rowSums(toplot_top[,..selspp])
            
            worldmap <- map_data ("world")
            
            if(selloc=="harbour"){
              mapplotport <- ggplot() +
                geom_tile(data=unique(clcs_rects[CLyear==2021]), aes(x=Lon,y=Lat,height=0.5, width=1),
                          fill="#CDFFF9", colour="white", linewidth = 0.5) +
                # geom_tile(data=unique(clcs_rects[CLyear==2021 & CLstatisticalRectangle=="34D4"]), aes(x=Lon,y=Lat,height=0.5, width=1), fill="darkgreen") +
                # geom_point(data=unique(clcs_rects[CLyear==2021 & CLstatisticalRectangle=="34D4"]), aes(x=Lon,y=Lat), col="white") +
                geom_map(data = worldmap, map = worldmap, aes(map_id=region), col = "grey25", fill ="grey95") +
                geom_point(data = toplot_nottop,
                           aes(x=lon,y=lat),
                           col="grey25") +
                geom_scatterpie(aes(x=lon,y=lat,group=landingLocationHarbourDesc,
                                    ## radius is based on the normalized landings (min-max scaling)
                                    r=rep((lantot_top - min(lantot_top)) / # subtract min for min-max scaling
                                            diff(range(lantot_top))+0.5        # divide by min-max difference
                                          , length(selspp))),
                                ## radius could also be standardized on sqrt-transformed landings (sqrt -> to get normal distribution; standardized -> not between 0-1)
                                ## then min-max scaling is applied
                                # r=rep(((sqrt(lantot_top)-mean(sqrt(lantot_top)) / sd(sqrt(lantot_top))) - # Standardized sqrt-transformed landings
                                #                                                                                 # for vizualization: 
                                #          min(sqrt(lantot_top)-mean(sqrt(lantot_top)) / sd(sqrt(lantot_top)))) / # subtract min for min-max scaling (normalization of standardized sqrt-transformed data)
                                #         diff(range((sqrt(lantot_top)-mean(sqrt(lantot_top)) / sd(sqrt(lantot_top))))) # divide by min-max difference
                                #       , length(selspp))),
                                cols=selspp,alpha=0.75,
                                data = toplot_top,
                                legend_name = "FLBEIAspecies") +
                scale_x_continuous(breaks=seq(-20,12, by=2),
                                   minor_breaks = seq(-20, 12, by = 1),limits=c(-20,12)) +
                scale_y_continuous(breaks = seq(40, 60, by = 1),
                                   minor_breaks = seq(40, 60, by = 0.5),limits=c(40,60)) + # by=0.5 # gives ICES rects
                # geom_text(data = toplot[rowSums(toplot[,..selspp]) %in% sort(rowSums(toplot[,..selspp]),decreasing=T)[1:3]], # top 3
                #           aes(x=rep(-15,3),y=c(57,56.5,56), label=landingLocationHarbourDesc),
                #           col="grey25", check_overlap = TRUE,nudge_y=-0.25, hjust=0) +
                scale_fill_manual(values=if("OTH"%in% selspp){pals::kelly(11)[-c(1,2)]}else{ pals::kelly(11)[-c(1,2,9)]}) +
                labs(title = paste0("Landings in fishing harbours coming from the demersal Celtic Seas, \nselecting",ifelse(sppfocus==1," FLBEIA spp "," FLBEIA + OTH spp "),"in ",whichyear,ifelse(whichcat=="all","",paste(" for",whichcat))), 
                     subtitle=paste0("Pie charts show the top 30% (N=",seltop, ") harbours in terms of landed quantities for ",whichcat,"\naccounting for a total percentage of ",round(lanperc_topports,1)," % of the total landings in ",whichyear),
                     caption = "Demersal Celtic Seas = FLBEIA region", 
                     x = "Longitude", y = "Latitude") +
                theme(legend.position = "bottom")+ 
                theme(panel.grid.major=element_line(linewidth=0.5),
                      panel.grid.minor=element_line(linewidth=0.25, linetype = 2))
              # theme(panel.grid = element_line(linewidth = 0.5))
            }else{
              mapplotrect <- ggplot() +
                geom_tile(data=unique(clcs_rects[CLyear==2021]), aes(x=Lon,y=Lat,height=0.5, width=1),
                          fill="#CDFFF9", colour="white", linewidth = 0.5) +
                geom_map(data = worldmap, map = worldmap, aes(map_id=region), col = "grey25", fill ="grey95") +
                geom_scatterpie(aes(x=lon,y=lat,group=CLstatisticalRectangle, r=0.23),
                                cols=selspp,alpha=0.75,
                                data = toplot,
                                legend_name = "FLBEIAspecies") +
                scale_x_continuous(breaks=seq(-18,0, by=2),
                                   minor_breaks = seq(-18, 0, by = 1),limits=c(-18,0)) +
                scale_y_continuous(breaks = seq(47, 56, by = 1),
                                   minor_breaks = seq(47, 56, by = 0.5),limits=c(47,56)) + # by=0.5 # gives ICES rects
                scale_fill_manual(values=if("OTH"%in% selspp){pals::kelly(11)[-c(1,2)]}else{ pals::kelly(11)[-c(1,2,9)]}) +
                labs(title = paste0("Landings in ICES rectangles coming from the demersal Celtic Seas, \nselecting",ifelse(sppfocus==1," FLBEIA spp "," FLBEIA + OTH spp "),"in ",whichyear,ifelse(whichcat=="all","",paste(" for",whichcat))), 
                     subtitle=paste0("Pie charts show landed quantities for ",whichcat,", accounting for a total percentage of ",round(lanperc_topports,2)," % of the total landings in ",whichyear),
                     caption = "Demersal Celtic Seas = FLBEIA region", 
                     x = "Longitude", y = "Latitude") +
                theme(legend.position = "bottom")+ 
                theme(panel.grid.major=element_line(linewidth=0.5),
                      panel.grid.minor=element_line(linewidth=0.25, linetype = 2))
            }
            if(tosave)suppressWarnings(ggsave(paste0(igndir,"/piecharts/",selloc,"/",ifelse(sppfocus==1,"FLBEIAspp/","FLBEIA_&_OTHspp/"),
                                                     whichyear,"/",selagg,"/",ifelse(selloc=="harbour","LandingsbyPORT_","LandingsbyRECT_"),
                                                     ifelse(whichcat=="all","all",whichcat),".png"), unit="cm",width=24,height=18))
            if(selloc=="harbour"){mapplotport}else{mapplotrect}
            while (!is.null(dev.list()))  dev.off()
          } # end check if toplot has non-emptyrows
        } # end whichaggcat loop
      } # end whichyear loop
    } # end sppfocus loop
  } # end selagg loop
} # end selloc loop




