#ReadMe

#Run this script to get the csv cleaned and prepped, though this does not pull in the raw. Too messy.

#updates
#April 2019: got 2000-2002 + 2018 from Stephanie and Lauren, adding that in.

library(knitr)
library(ggplot2)
library(magrittr)
library(lubridate)
library(reshape2)
library(xtable)
library(dplyr)
library(readr)

##Load stranding data, remove unnecessary columns, make pinniped/cetacean designation, fix common name to remove comma
setwd("~/Documents/Research/Cet_Strandings/PNW Strands Manuscript")

case <- function(x)
  paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

# data <- read.csv("Cet_Stranding_Data_CleaningInput.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)
# data <- read.csv("OR_WA_Cetacean_all_NEW.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

# update from Stephanie, sent in email October 11 2020, includes 2019 data
data <- read.csv("Cetacean_stranding_data_2000-2019_FINAL_10-11-20.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)

cetacean_data <- data %>%
  # rename(City = City...from.strandings.table) %>%
    # transform(Interaction.Type =
    #           ifelse(Fishery.Interaction == 'Y', 'Fisheries',
    #                  ifelse(Shot == 'Y', 'Gunshot',
    #                         ifelse(Boat.Collision == 'Y', 'Boat',
    #                                ifelse(Other.Human.Interaction == 'Y', "Other", 'NA'))))) %>%
  transform(Common.Name = gsub("'", '', Common.Name)) %>%
  transform(Cetacean.Common.Name = 
              ifelse(grepl('nidentified', Common.Name) | grepl('UNKNOWN', Common.Name) | 
                       grepl('UNSPECIFIED', Common.Name) | grepl('UNIDENTIFIED', Common.Name), 'Unidentified',
                     ifelse(grepl('HYBRID', Common.Name), 'Hybrid',
                     ifelse(Common.Name == 'Porpoise, harbor' | Common.Name == 'Harbor Porpoise', 'Harbor porpoise',
                            ifelse(Common.Name == 'Dolphin, common, short-beaked', 'Short-beaked common dolphin',
                            ifelse(Common.Name == 'Dolphin, common, long-beaked', 'Long-beaked common dolphin',
                            ifelse(Common.Name == 'Dolphin, northern right whale', 'Northern right whale dolphin',
                            ifelse(Common.Name == 'Dolphin, Pacific white-sided', 'Pacific white-sided dolphin',
                            ifelse(Common.Name == 'Dolphin, Rissos', 'Rissos',
                            ifelse(Common.Name == 'Dolphin, bottlenose', 'Bottlenose dolphin',
                            ifelse(Common.Name == 'Dolphin, striped', 'Striped dolphin',
                            ifelse(Common.Name == 'Porpoise, Dalls', 'Dalls porpoise',
                            ifelse(Common.Name == 'Whale, Bairds beaked', 'Bairds beaked whale', 
                            ifelse(Common.Name == 'Whale, Blainvilles beaked', 'Blainvilles beaked whale', 
                            ifelse(Common.Name == 'Whale, Cuviers beaked', 'Cuviers beaked whale', 
                            ifelse(Common.Name == 'Whale, fin', 'Fin whale', 
                            ifelse(Common.Name == 'Whale, gray', 'Gray whale', 
                            ifelse(Common.Name == 'Whale, humpback' | Common.Name == 'Humpback Whale', 'Humpback whale', 
                            ifelse(Common.Name == 'Whale, killer', 'Killer whale', 
                            ifelse(Common.Name == 'Whale, minke', 'Minke whale', 
                            ifelse(Common.Name == 'Whale, pygmy sperm', 'Pygmy sperm whale', 
                            ifelse(Common.Name == 'Whale, Stejnegers beaked', 'Stejnegers beaked whale',
                            ifelse(Common.Name == 'Whale, sperm' | Common.Name == 'Sperm Whale', 'Sperm whale', 'Other'))))))))))))))))))))))) %>%
  transform(Water.Body = ifelse(County == 'Pacific' | County == 'Grays Harbor' | 
                                  County == 'Clallam' & Affiliation == 'Makah Fisheries Management' & City == 'La Push' & City == 'Clallam Bay' & City == 'Neah Bay' & 
                                  City == 'Olympic National Park' & City != 'Sequim' & City != 'Port Angeles' & City != 'Port Townsend' & 
                                  Affiliation != 'Feiro Marine Life Center' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' | 
                                  County == 'Jefferson' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' &
                                  City == 'Dosewallips' & City == 'Port Ludlow' & City != 'Brinnon' & City != 'Quilcene' & City != 'Port Townsend', 'WA_Coast',
                                ifelse(State == 'OR', 'OR_Coast', 
                                       ifelse(State == 'CA', 'CA Coast', 'Inland_WA')))) %>%
  dplyr::select(-c(mean_lat, mean_long))

sp <- cetacean_data %>% group_by(Cetacean.Common.Name, Common.Name) %>%
  dplyr::summarize(cnt = n_distinct(National.Database.Number))

#Regional ifelse statement cleaning
test <- cetacean_data %>%
  filter(is.na(Water.Body)) %>%
  summarize(n_distinct(National.Database.Number))


cetacean_data$Age.Class[is.na(cetacean_data$Age.Class)] <- "NA"
cetacean_data$Age.Class <- gsub("NA", "Unid", cetacean_data$Age.Class)
cetacean_data$Age.Class <- gsub("UNKNOWN", "Unid", cetacean_data$Age.Class)
cetacean_data$Age.Class <- gsub("PUP/CALF", "CALF", cetacean_data$Age.Class)
cetacean_data$Age.Class <- case(cetacean_data$Age.Class)

cetacean_data$Sex[is.na(cetacean_data$Sex)] <- "NA"
cetacean_data$Sex <- gsub("NA", "Unid", cetacean_data$Sex)
cetacean_data$Sex <- gsub("UNKNOWN", "Unid", cetacean_data$Sex)
cetacean_data$Sex <- case(cetacean_data$Sex)


# #lat/long cleaning

cetacean_data$Longitude <- gsub("\\-", "", cetacean_data$Longitude)
cetacean_data$Longitude <- gsub("\\-", "", cetacean_data$Longitude)
cetacean_data$Longitude <- gsub("\\_", "", cetacean_data$Longitude)
#cetacean_data$Longitude <- gsub("/", "", cetacean_data$Longitude)
#cetacean_data$Longitude <- gsub("\\ ", ".", cetacean_data$Longitude)
#cetacean_data$Latitude <- gsub("/", "", cetacean_data$Latitude)
#cetacean_data$Latitude <- gsub(" ", ".", cetacean_data$Latitude)

cetacean_data$Longitude <- parse_number(cetacean_data$Longitude)
cetacean_data$Longitude <- as.numeric(cetacean_data$Longitude)
cetacean_data$Longitude <- cetacean_data$Longitude * (-1)

cetacean_data$Latitude <- as.numeric(cetacean_data$Latitude)

mean_lat_long <- cetacean_data %>%
  filter(County != 'NA') %>%
  #transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  group_by(County) %>%
  dplyr::summarize(mean_lat = mean(Latitude, na.rm = T), mean_long = mean(Longitude, na.rm = T))
 
# #Applying mean lat/longs for erroneous or missing
cetacean_data <- cetacean_data %>%
  merge(mean_lat_long, by = 'County', all = T) %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  transform(Long_error = mean_long - as.numeric(Longitude)) %>%
  transform(Lat_error = mean_lat - as.numeric(Latitude)) %>%
  transform(long_corr = ifelse(is.na(Longitude) |
                                 Long_error < -1.5 | Long_error > 1.2, mean_long, Longitude)) %>%
  transform(lat_corr = ifelse(is.na(Latitude) |
                                Lat_error < -.5 | Lat_error > .5, mean_lat, Latitude))

#only 2 missing lat/longs, most values replaced were in Island, which is only 0.5 latitude long
# sum(is.na(cetacean_data$long_corr))
# testreplace <- cetacean_data[cetacean_data$Latitude != cetacean_data$lat_corr,]
# testreplace <- testreplace %>% filter(!is.na(Latitude))

#California data

write.csv(cetacean_data, file = "~/Documents/Research/Cet_Strandings/PNW Strands Manuscript/cetacean_data_CleaningOuput10_25_20updated.csv", row.names = F)



#just HP strandings

# update from Stephanie, sent in email October 11 2020, includes 2019 data
data <- read.csv("Harbor_porpoise_2000-2019_WA_only_10-11-20.csv", header = TRUE, 
                 na.strings = "", stringsAsFactors = FALSE)

HP_data <- data %>%
  # rename(City = City...from.strandings.table) %>%
  # transform(Interaction.Type =
  #           ifelse(Fishery.Interaction == 'Y', 'Fisheries',
  #                  ifelse(Shot == 'Y', 'Gunshot',
  #                         ifelse(Boat.Collision == 'Y', 'Boat',
  #                                ifelse(Other.Human.Interaction == 'Y', "Other", 'NA'))))) %>%
  transform(Common.Name = gsub("'", '', Common.Name)) %>%
  transform(Cetacean.Common.Name = 
              ifelse(grepl('nidentified', Common.Name) | grepl('UNKNOWN', Common.Name) | 
                       grepl('UNSPECIFIED', Common.Name) | grepl('UNIDENTIFIED', Common.Name), 'Unidentified',
                     ifelse(grepl('HYBRID', Common.Name), 'Hybrid',
                            ifelse(Common.Name == 'Porpoise, harbor' | Common.Name == 'Harbor Porpoise', 'Harbor porpoise',
                                   ifelse(Common.Name == 'Dolphin, common, short-beaked', 'Short-beaked common dolphin',
                                          ifelse(Common.Name == 'Dolphin, common, long-beaked', 'Long-beaked common dolphin',
                                                 ifelse(Common.Name == 'Dolphin, northern right whale', 'Northern right whale dolphin',
                                                        ifelse(Common.Name == 'Dolphin, Pacific white-sided', 'Pacific white-sided dolphin',
                                                               ifelse(Common.Name == 'Dolphin, Rissos', 'Rissos',
                                                                      ifelse(Common.Name == 'Dolphin, bottlenose', 'Bottlenose dolphin',
                                                                             ifelse(Common.Name == 'Dolphin, striped', 'Striped dolphin',
                                                                                    ifelse(Common.Name == 'Porpoise, Dalls', 'Dalls porpoise',
                                                                                           ifelse(Common.Name == 'Whale, Bairds beaked', 'Bairds beaked whale', 
                                                                                                  ifelse(Common.Name == 'Whale, Blainvilles beaked', 'Blainvilles beaked whale', 
                                                                                                         ifelse(Common.Name == 'Whale, Cuviers beaked', 'Cuviers beaked whale', 
                                                                                                                ifelse(Common.Name == 'Whale, fin', 'Fin whale', 
                                                                                                                       ifelse(Common.Name == 'Whale, gray', 'Gray whale', 
                                                                                                                              ifelse(Common.Name == 'Whale, humpback' | Common.Name == 'Humpback Whale', 'Humpback whale', 
                                                                                                                                     ifelse(Common.Name == 'Whale, killer', 'Killer whale', 
                                                                                                                                            ifelse(Common.Name == 'Whale, minke', 'Minke whale', 
                                                                                                                                                   ifelse(Common.Name == 'Whale, pygmy sperm', 'Pygmy sperm whale', 
                                                                                                                                                          ifelse(Common.Name == 'Whale, Stejnegers beaked', 'Stejnegers beaked whale',
                                                                                                                                                                 ifelse(Common.Name == 'Whale, sperm' | Common.Name == 'Sperm Whale', 'Sperm whale', 'Other'))))))))))))))))))))))) %>%
  transform(Water.Body = ifelse(County == 'Pacific' | County == 'Grays Harbor' | 
                                  County == 'Clallam' & Affiliation == 'Makah Fisheries Management' & City == 'La Push' & City == 'Clallam Bay' & City == 'Neah Bay' & 
                                  City == 'Olympic National Park' & City != 'Sequim' & City != 'Port Angeles' & City != 'Port Townsend' & 
                                  Affiliation != 'Feiro Marine Life Center' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' | 
                                  County == 'Jefferson' & Affiliation != 'Port Townsend Marine Science Center/ East Jefferson Co. MMSN' &
                                  City == 'Dosewallips' & City == 'Port Ludlow' & City != 'Brinnon' & City != 'Quilcene' & City != 'Port Townsend', 'WA_Coast',
                                ifelse(State == 'OR', 'OR_Coast', 
                                       ifelse(State == 'CA', 'CA Coast', 'Inland_WA')))) %>%
  dplyr::select(-c(mean_lat, mean_long))

sp <- HP_data %>% group_by(Cetacean.Common.Name, Common.Name) %>%
  dplyr::summarize(cnt = n_distinct(National.Database.Number))

#Regional ifelse statement cleaning
test <- HP_data %>%
  filter(is.na(Water.Body)) %>%
  summarize(n_distinct(National.Database.Number))


HP_data$Age.Class[is.na(HP_data$Age.Class)] <- "NA"
HP_data$Age.Class <- gsub("NA", "Unid", HP_data$Age.Class)
HP_data$Age.Class <- gsub("UNKNOWN", "Unid", HP_data$Age.Class)
HP_data$Age.Class <- gsub("PUP/CALF", "CALF", HP_data$Age.Class)
HP_data$Age.Class <- case(HP_data$Age.Class)

HP_data$Sex[is.na(HP_data$Sex)] <- "NA"
HP_data$Sex <- gsub("NA", "Unid", HP_data$Sex)
HP_data$Sex <- gsub("UNKNOWN", "Unid", HP_data$Sex)
HP_data$Sex <- case(HP_data$Sex)


# #lat/long cleaning

HP_data$Longitude <- gsub("\\-", "", HP_data$Longitude)
HP_data$Longitude <- gsub("\\-", "", HP_data$Longitude)
HP_data$Longitude <- gsub("\\_", "", HP_data$Longitude)
#HP_data$Longitude <- gsub("/", "", HP_data$Longitude)
#HP_data$Longitude <- gsub("\\ ", ".", HP_data$Longitude)
#HP_data$Latitude <- gsub("/", "", HP_data$Latitude)
#HP_data$Latitude <- gsub(" ", ".", HP_data$Latitude)

HP_data$Longitude <- parse_number(HP_data$Longitude)
HP_data$Longitude <- as.numeric(HP_data$Longitude)
HP_data$Longitude <- HP_data$Longitude * (-1)

HP_data$Latitude <- as.numeric(HP_data$Latitude)

mean_lat_long <- HP_data %>%
  filter(County != 'NA') %>%
  #transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  group_by(County) %>%
  dplyr::summarize(mean_lat = mean(Latitude, na.rm = T), mean_long = mean(Longitude, na.rm = T))

# #Applying mean lat/longs for erroneous or missing
HP_data <- HP_data %>%
  merge(mean_lat_long, by = 'County', all = T) %>%
  transform(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
  transform(Long_error = mean_long - as.numeric(Longitude)) %>%
  transform(Lat_error = mean_lat - as.numeric(Latitude)) %>%
  transform(long_corr = ifelse(is.na(Longitude) |
                                 Long_error < -1.5 | Long_error > 1.2, mean_long, Longitude)) %>%
  transform(lat_corr = ifelse(is.na(Latitude) |
                                Lat_error < -.5 | Lat_error > .5, mean_lat, Latitude))

write.csv(HP_data, file = "~/Documents/Research/Cet_Strandings/PNW Strands Manuscript/HP_data_CleaningOuput10_25_20updated.csv", row.names = F)


