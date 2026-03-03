source("aci_functions.R")
library(tidyverse)
library(data.table)

library(tidyverse)
library(data.table)
library(osmdata)
library(ggmap)
library(ggthemes)
library(cowplot)
library(ggh4x)


bb_paris <- getbb("Paris")
bb_paris[1] <- 2.241749
bb_paris[3] <- 2.426782
paris_tiles <- get_map(bb_paris, source = "stamen")

utrecht_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"
path_to_buildings <- file.path(utrecht_dir, "R5/building_as_origins_reduced.csv")


setwd("D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris")

inperiph <- fread("D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/R5/building_as_origins_periph.csv")
#
inperiph <- as.character(inperiph$id)
#
buildings_together <- fread(path_to_buildings) %>%
  left_join(fread("codeact_per_bat_2.csv"),
    by = c("id" = "from_id")
  ) %>%
  filter(id %in% inperiph)
#
#
# filtered_out_amenities <- c("AB102","AC102","AF102","AA102","AF104","AB106",
#                              "AB103","AB104","AB105","AF103","AB101","af104",
#                              "AF104","aa101","AA101","Af104","Af102","af102",
#                              "SB301","CI201","SA506","AD107", "AD105")
#
filtered_out_amenities <- c(
  "AB102", "AC102", "AF102", "AA102", "AF104", "AB106",
  "AB103", "AB104", "AB105", "AF103", "AB101", "af104",
  "AF104", "aa101", "AA101", "Af104", "Af102", "af102",
  "SB301", ""
)
# # #Exclude ambulances and infirmières because there is no local consumption
# #
voitures_a_aggreger <- c("CG102", "CG103", "CG104", "CG201", "CG202")
# #
motos_a_aggreger <- c("CG106", "CG204")
# #
# # ########## TO DO : TO Upper ce403 , cb111 and aggregate
# # ########## Figure out if agg is better or presence is better
# # ########## Which are best to aggregate with CA115? All organic foods
# #
# tobio <- c("CA202","CH303","CC202","CA202","CC201","CA301","CA103","CA108","CA302", "CC202")
# #
# #
#
buildings_together <- buildings_together %>% filter(!CODE_ACTIVITE %in% filtered_out_amenities)
#
#
buildings_together[buildings_together$CODE_ACTIVITE %in% voitures_a_aggreger, ]$CODE_ACTIVITE <- "Car related"
buildings_together[buildings_together$CODE_ACTIVITE %in% motos_a_aggreger, ]$CODE_ACTIVITE <- "Motorbike related"
#
buildings_together$CODE_ACTIVITE <- toupper(buildings_together$CODE_ACTIVITE)
#
buildings_together <- buildings_together %>%
  group_by(id, CODE_ACTIVITE, lon, lat) %>%
  summarise(
    Year14 = sum(Year14, na.rm = T),
    Year17 = sum(Year17, na.rm = T),
    Year20 = sum(Year20, na.rm = T),
    Year23 = sum(Year23, na.rm = T)
  )
#
gc()

# fwrite(buildings_together, "codeact_per_bat_cleaned.csv")

# Filter out drive piéton (only 2020, 5 of them total..) and CBD (Only 2023 (no)) and Pop-up Store (no)??
# Pole emploi ofc SA506, hotel de préfecture too
filter_again <- c("", "CA305", "SA506", "CI201")
# All medical amenities missing in 2023
# -> Ignore medical


########################### START HERE #####################################


# bad_ids <- as.data.frame(as.character(unique(ACI_all_rolling[ACI_all_rolling$diversity < 20,]$id)))
# names(bad_ids)[1] = "id"
# fwrite(bad_ids,"bad_ids.csv")

bad_ids <- fread("bad_ids.csv")

buildings_together <- fread("codeact_per_bat_cleaned.csv") %>%
  filter(!(CODE_ACTIVITE %in% filter_again) & !(substr(CODE_ACTIVITE, 1, 1) == "A") & !(id %in% bad_ids$id))

names <- fread("D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/Translated_amenity_types_2020.csv")


gc()




message("Computing TCI...")
TCI_all <- TCI(buildings_together %>% filter(!CODE_ACTIVITE == "CC203"), RCA = F)
TCI_all_RCA <- TCI(buildings_together %>% filter(!CODE_ACTIVITE == "CC203"), RCA = T)

TCI_all_rolling <- TCI(buildings_together,
  RCA = F, rolling = T
)
TCI_all_rolling <- left_join(TCI_all_rolling, names, by = c("id" = "CODE_ACTIVITE"))


TCI_table <- TCI_all_rolling %>%
  fwrite(TCI_all_rolling, "TCI_all_rolling.csv")

message("Computing ACI...")
ACI_all_rolling <- ACI_fixed(buildings_together,
  tci_data = TCI_all_rolling, RCA = F,
  years = c("All"), full_sum = F, rolling = T
)

fwrite(ACI_all_rolling, "ACI_all_rolling.csv")
message("Saved ACI_all_rolling.csv and TCI_all_rolling.csv!")
