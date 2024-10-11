
#Load packages and background data/functions
library(here)
knitr::opts_chunk$set(echo = FALSE, warning=FALSE, message=FALSE)
source(here::here("R/_commonR.r"))
source(here::here("R/reportPlotFunctions.r"))

load(here("data", "bw", "bwBin_adrift.rdata"))
bin39<-bwBin39_adrift %>%
  mutate (Season = markSeason(bwBin39_adrift$UTC),
          Region = map_chr(bwBin39_adrift$Latitude, find_region),
          Species =  "MC"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(species == "MC")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
bin39<- bin39[,c(1, 2, 5, 6)]
bin39$CallType <- "Hubb's beaked whale"
bin39 <- bin39[!is.na(bin39$Region),]

#No detections of bw43
binBb<-bwBinBb_adrift%>%
  mutate (Season = markSeason(bwBinBb_adrift$UTC),
          Region = map_chr(bwBinBb_adrift$Latitude, find_region),
          Species = "BB"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(species == "BB")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binBb<- binBb[,c(1, 2, 5, 6)]
binBb$CallType <- "Baird's beaked whale"
binBb <- binBb[!is.na(binBb$Region),]


binMs<-bwBinMs_adrift%>%
  mutate (Season = markSeason(bwBinMs_adrift$UTC),
          Region = map_chr(bwBinMs_adrift$Latitude, find_region),
          Species = "MS"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(species == "MS")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binMs<- binMs[,c(1, 2, 5, 6)]
binMs$CallType <- "Stejneger's beaked whale"
binMs <- binMs[!is.na(binMs$Region),]

binZc<-bwBinZc_adrift%>%
  mutate (Season = markSeason(bwBinZc_adrift$UTC),
          Region = map_chr(bwBinZc_adrift$Latitude, find_region),
          Species = "ZC"
  )%>%
  group_by(Season, Region) %>%
  dplyr::summarise(
    HourlyEffort=sum(pctEff),
    sppPres=length(which(species == "ZC")),
    HourlyProb = sppPres/HourlyEffort,
    n=n()
  )
binZc<- binZc[,c(1, 2, 5, 6)]
binZc$CallType <- "Goose-beaked whale"
binZc <- binZc[!is.na(binZc$Region),]

dfSummary <- rbind(bin39, binBb, binMs, binZc)

detTbl <- dfSummary %>%
  pivot_wider(names_from = "Season", values_from = c("HourlyProb", "n"))%>%
  mutate (Region = fct_relevel(Region, c("Oregon", "Humboldt", "San Francisco", "Morro Bay")))%>%
  arrange(Region)%>%
  gt(
    groupname_col = "CallType",
    rowname_col = "Region"
  )%>%
  bwDetTableTheme508()
detTbl
# gtsave(detTbl, filename = here("output", "bw_detTbl.html"))


