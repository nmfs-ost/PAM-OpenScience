
### Load Libraries
library("here")
library("readr")
library("dplyr")
library("tidyverse")
library("knitr")
library("magrittr")
library("gt")
library("gtExtras")
library("webshot2")
library("gtsummary")
library("stringr")
library("zoo")
library("magick")
library("tinytex")


### Create a Theme for Big Report Tables
tableTheme <- function(gt_tbl) {
  gt_tbl %>%
    opt_align_table_header(align = 'left')%>% 
    tab_options(
      column_labels.background.color = "#DBE5F1",
      heading.align = 'left',
      column_labels.font.size = 14,
      column_labels.font.weight = 'bold',
      data_row.padding = px(6),
      stub.font.weight = 'bold'
    ) %>%
    tab_style(
      style = cell_text(font = 'Arial', align = 'left', size = 12), 
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text( font = 'Arial', align = 'center', weight="bold"),
      locations = list(cells_column_spanners(), cells_column_labels())
    )
}

### Theme for Species Detection Report Tables
DetTableTheme <- function(gt_tbl) {
  gt_tbl %>%
    opt_align_table_header(align = 'left')%>% 
    tab_options(
      column_labels.background.color = "#DBE5F1",
      heading.align = 'left',
      column_labels.font.size = 14,
      column_labels.font.weight = 'bold',
      data_row.padding = px(6),
      column_labels.padding = px(6),
      stub.font.weight = 'bold'
    ) %>%
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_stub()
    )%>%
    tab_style(
      style=cell_text(weight = "bold"),
      locations = cells_row_groups()
    )%>%
    tab_style(
      style = cell_text(font = 'Arial', align = 'center', size = 12), 
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text( font = 'Arial', align = 'center', weight="bold"),
      locations = list(cells_column_spanners(), cells_column_labels())
    )%>%
    tab_spanner(
      label = "Upwelling", 
      columns = c('Summ_Upwelling', 'HourlyProb_Upwelling')
    )%>%
    tab_spanner(
      label = "Post-Upwelling", 
      columns = c('Summ_Post-Upwelling', 'HourlyProb_Post-Upwelling')
    ) %>%
    tab_spanner(
      label = "Winter", 
      columns = c('Summ_Winter', 'HourlyProb_Winter')
    ) %>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) %>%
    cols_label(
      "Summ_Upwelling" ~ "N (%)",
      "HourlyProb_Upwelling" ~ "HourlyProb",
      "Summ_Post-Upwelling" ~ "N (%)",
      "HourlyProb_Post-Upwelling" ~ "HourlyProb",
      "Summ_Winter" ~ "N (%)",
      "HourlyProb_Winter" ~ "HourlyProb"
    )
}

### 508 Compliant Theme for Single call type Species Detection Report Tables
DetTableTheme508 <- function(gt_tbl) {
  gt_tbl %>%
    opt_align_table_header(align = 'left')%>% 
    fmt_number(
      columns = c("HourlyProb_Upwelling", "HourlyProb_Post-Upwelling", 
                  "HourlyProb_Winter"),
      decimals = 2)%>%
    tab_options(
      column_labels.background.color = "#DBE5F1",
      heading.align = 'left',
      column_labels.font.size = 14,
      column_labels.font.weight = 'bold',
      data_row.padding = px(6),
      column_labels.padding = px(6),
      stub.font.weight = 'bold'
    ) %>%
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_stub()
    )%>%
    tab_style(
      style=cell_text(weight = "bold"),
      locations = cells_row_groups()
    )%>%
    tab_style(
      style = cell_text(font = 'Arial', align = 'center', size = 12), 
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text( font = 'Arial', align = 'center', weight="bold"),
      locations = list(cells_column_spanners(), cells_column_labels())
    )%>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) %>%
    cols_merge(
      columns = c("HourlyProb_Upwelling", "n_Upwelling"),
      pattern = "{1} ({2})"
    )%>%
    cols_merge(
      columns = c("HourlyProb_Post-Upwelling", "n_Post-Upwelling"),
      pattern = "{1} ({2})"
    )%>%
    cols_merge(
      columns = c("HourlyProb_Winter", "n_Winter"),
      pattern = "{1} ({2})"
    )%>%
    cols_label(
      "HourlyProb_Upwelling" ~ "Upwelling",
      "HourlyProb_Post-Upwelling" ~ "Post-Upwelling",
      "HourlyProb_Winter" ~ "Winter"
    )%>%
    cols_align_decimal()%>%
    cols_hide(c("n_Upwelling", "n_Post-Upwelling", "n_Winter"))
}

### 508 Compliant Theme for Species w/ variable call types Detection Report Tables
bwDetTableTheme508 <- function(gt_tbl) {
  gt_tbl %>%
    opt_align_table_header(align = 'left')%>% 
    fmt_number(
      columns = c("HourlyProb_Upwelling", "HourlyProb_Post-Upwelling", 
                  "HourlyProb_Winter"),
      decimals = 4)%>%
    tab_options(
      column_labels.background.color = "#DBE5F1",
      heading.align = 'left',
      column_labels.font.size = 14,
      column_labels.font.weight = 'bold',
      data_row.padding = px(6),
      column_labels.padding = px(6),
      stub.font.weight = 'bold'
    ) %>%
    tab_style(
      style = cell_text(style = "italic"),
      locations = cells_stub()
    )%>%
    tab_style(
      style=cell_text(weight = "bold"),
      locations = cells_row_groups()
    )%>%
    tab_style(
      style = cell_text(font = 'Arial', align = 'center', size = 12), 
      locations = cells_body()
    ) %>%
    tab_style(
      style = cell_text( font = 'Arial', align = 'center', weight="bold"),
      locations = list(cells_column_spanners(), cells_column_labels())
    )%>%
    sub_missing(
      columns = everything(),
      rows = everything(),
      missing_text = "--"
    ) %>%
    cols_merge(
      columns = c("HourlyProb_Upwelling", "n_Upwelling"),
      pattern = "{1} ({2})"
    )%>%
    cols_merge(
      columns = c("HourlyProb_Post-Upwelling", "n_Post-Upwelling"),
      pattern = "{1} ({2})"
    )%>%
    cols_merge(
      columns = c("HourlyProb_Winter", "n_Winter"),
      pattern = "{1} ({2})"
    )%>%
    cols_label(
      "HourlyProb_Upwelling" ~ "Upwelling",
      "HourlyProb_Post-Upwelling" ~ "Post-Upwelling",
      "HourlyProb_Winter" ~ "Winter"
    )%>%
    cols_align_decimal()%>%
    cols_hide(c("n_Upwelling", "n_Post-Upwelling", "n_Winter"))
}


### create dataframe of regions
site<-read.csv(here("data", "Deployment Details - Site List.csv"))[1:15,1:8] 

find_region <- function(lat) {
  # region <- site$VZone.Name[which(lat >= site$Lat_min & lat < site$Lat_max)]
  region <- site$Adrift.Regions[which(lat >= site$Lat_min & lat < site$Lat_max)]
  if (length(region) == 0) {
    return(NA)  # Return NA if no region found
  } else {
    return(region)
  }
}


###Prep Detection Dataset all_Det
#Note- No longer used for detection date, reserving here for easy access
# allDet <- read.csv(here("data", "AllDetections_wGPS.csv"))
# allDet <- separate_wider_delim(allDet, cols = "DriftName", delim = ("_"), 
#                                names = c("survey", "drift"), cols_remove = FALSE)
# allDet$season <- markSeason(allDet$UTC) #add seasons
# allDet$region <- map_chr(allDet$Latitude, find_region) #add region using function defined in _commonR.R

#Below are functions from other documents, copied here

markRegion <- function(x) {
  # mby <- c('MBY', 'MOB', 'SF', 'SFB')
  # hum <- c('HUM', 'Crescent City')
  # ore <- c('ORE')
  # x[x %in% mby] <- 'MBY'
  # x[x %in% hum] <- 'HUM'
  # x[x %in% ore] <- 'ORE'
  # annes codes
  MorroBay<-c('MBY','MOB')
  Humboldt<-c('Crescent City','HUM', 'MND')
  Oregon<-c('ORE', 'COL')
  SanFrancisco<-c('HMB','SF','SFB')
  x[x %in% MorroBay] <- 'MorroBay'
  x[x %in% Humboldt] <- 'Humboldt'
  x[x %in% Oregon] <- 'Oregon'
  x[x %in% SanFrancisco] <- 'SanFrancisco'
  # factor(x, levels=c('Oregon', 'Humboldt', 'SanFrancisco','MorroBay'))
  x
}
# separate forcing for pascal/cces to align with adrift
markPCRegion <- function(x) {
  isHum <- c('CCES_007', 'PASCAL_002')
  isSf <- c('CCES_010', 'PASCAL_003')
  isMb <- c('PASCAL_004', 'PASCAL_021')
  isOregon <- c('PASCAL_001')
  x$region <- NA
  x$region[grepl('ADRIFT', x$DriftName)] <- markRegion(x$DeploymentSite[grepl('ADRIFT', x$DriftName)])
  x$region[x$DriftName %in% isHum] <- 'Humboldt'
  x$region[x$DriftName %in% isSf] <- 'SanFrancisco'
  x$region[x$DriftName %in% isOregon] <- 'Oregon'
  x$region[x$DriftName %in% isMb] <- 'MorroBay'
  x$region <- factor(x$region, levels=c('Oregon', 'Humboldt', 'SanFrancisco','MorroBay'))
  x
}

markSeason <- function(x) {
  season <- c(rep('Winter', 2),
              rep('Upwelling', 4),
              rep('Post-Upwelling', 5),
              'Winter')
  factor(season[month(x)], levels=c('Upwelling', 'Post-Upwelling', 'Winter'))
}

