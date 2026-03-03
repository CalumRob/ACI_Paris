# export_pmtiles_geojson.R
# This script prepares the minimal building dataset for PMTiles and Supabase
# 1. Joins the building geometries with the ACI/TCI scores.
# 2. Computes the top 3 POSITIVE and NEGATIVE contributors to the building's ACI.
# 3. Exports the spatial data to GeoJSON for tippecanoe (MapLibre PMTiles).
# 4. Exports the summary data directly to Supabase.
source(file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "utils.R"))
library(tidyverse)
library(data.table)
library(sf)
library(DBI)
library(RPostgres)

# --- Configuration ---
# Update these paths based on where your final data lives
ACI_SCORES_CSV <- "ACI_all_rolling.csv"
TCI_SCORES_CSV <- "TCI_all_rolling.csv"
RAW_AMENITY_COUNTS_CSV <- "codeact_per_bat_cleaned.csv"

OUTPUT_GEOJSON <- "paris_buildings_aci.geojson"


aci_data <- fread(proj_path(ACI_SCORES_CSV))
tci_data <- fread(proj_path(TCI_SCORES_CSV))
raw_counts <- fread(proj_path(RAW_AMENITY_COUNTS_CSV))

head(aci_data)
head(tci_data)
# ACI is the count-weighted average of present amenities' TCI.
# The absolute contribution of a specific amenity (of a certain type)
# to a building's ACI sum can be captured by its count * (TCI - ACI).
# Positive values mean the amenity pulls the building's average UP.
# Negative values mean the amenity pulls the building's average DOWN.

# Example for a given year (e.g. 2023):

joined_data <- raw_counts %>%
  select(-lon,-lat) %>%
  pivot_longer(.,cols = c("Year14","Year17","Year20","Year23"), names_to = "Year", values_to = "count") %>%
  mutate(Year = as.integer(paste0("20",substr(Year,5,6)))) %>%
  inner_join(tci_data %>% 
               rename(CODE_ACTIVITE = id,
                      tci_unscaled = Complexity_unscaled) %>% 
               select(CODE_ACTIVITE, tci_unscaled, Year), 
             by=c("CODE_ACTIVITE", "Year"))

joined_data <- aci_data %>% 
  rename(aci_unscaled = Complexity_unscaled) %>%
  select(id, aci_unscaled, Year) %>%
  inner_join(.,joined_data,
             by=c("id", "Year")) %>%
  group_by(id, Year) %>%
  mutate(contribution = count * (tci_unscaled - aci_unscaled)) %>%
  ungroup()


final_contributors <- joined_data %>%
  group_by(id, Year) %>%
  arrange(desc(contribution)) %>%
  mutate(rank = row_number(),
         rev_rank = rev(rank)) %>%
  filter(rank <= 3 | rev_rank <= 3) %>%
  mutate(label = case_when(
    rank == 1 ~ "first",
    rank == 2 ~ "second",
    rank == 3 ~ "third",
    rev_rank == 1 ~ "last",
    rev_rank == 2 ~ "seclast",
    rev_rank == 3 ~ "thirdlast"
  )) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(id, Year),
    names_from = label,
    values_from = c(CODE_ACTIVITE, count),
    names_glue = "{label}_{.value}"
  )


final_contributors2 <- final_contributors %>%
  pivot_wider(., id_cols = id, names_from = Year, names_prefix = "Year", values_from = c(3:14))

#cols_to_filter <- grep("contribution", names(final_contributors2), value = TRUE)


map_data <- aci_data %>% 
  rename(aci = Complexity) %>%
  select(id, Year, aci, diversity, ubiquity) %>%
  pivot_wider(., names_from = Year, names_prefix = "Year", values_from = c(3:5))


map_data <- inner_join(map_data, final_contributors2, by = "id")

map_data <- inner_join(map_data, raw_counts %>% select(id,lon,lat) %>% distinct(), by = "id")

map_sf <- st_as_sf(map_data, crs = 4326, coords = c("lon","lat"))

st_write(map_sf, OUTPUT_GEOJSON)


# --- 3. Join with Geometries ---
# cat("Joining spatial data...\n")
# final_sf <- buildings %>%
#   inner_join(map_data, by="id_bat") %>%
#   st_transform(4326)

# --- 4. Export ---
# cat("Exporting to GeoJSON for tippecanoe...\n")
# st_write(final_sf, OUTPUT_GEOJSON, delete_dsn = TRUE)
# system(sprintf("tippecanoe -o paris_aci.pmtiles -zg --drop-densest-as-needed %s", OUTPUT_GEOJSON))

# cat("Exporting summary data to Supabase...\n")
# supabase_data <- final_sf %>% st_drop_geometry()
# con <- get_db_conn_supa()
# dbWriteTable(con, "building_summaries", supabase_data, row.names = FALSE, overwrite = TRUE)
# dbDisconnect(con)

cat("Done!\n")
