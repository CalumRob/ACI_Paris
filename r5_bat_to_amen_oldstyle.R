# --- Libraries ---
# Ensure these are installed:
# install.packages(c("r5r", "tidyverse", "sf", "rJava", "readr", "data.table")) # data.table for fread/fwrite if preferred
options(java.parameters = '-Xmx8G')
library(rJava)   # For r5r and explicit garbage collection

library(r5r)
library(tidyverse) # Includes dplyr, stringr, etc.
library(sf)
library(readr)   # For read_csv/write_csv (can be swapped with data.table::fread/fwrite)
library(data.table) # Uncomment if you prefer fread/fwrite

setwd("D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris")


########### UNIQUE IDs per amen (each year)

data_base_dir = "D:/Users/Calum/Documents/La_Rochelle/DATA"
utrecht_dir = "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"
current_dir = here::here()


bdcom_dir <- file.path(data_base_dir, "BDCom")

path_to_amen <- file.path(bdcom_dir, "BDCOM_14_to_23_cleaned.csv")

path_to_buildings <- file.path(utrecht_dir,"R5/building_as_origins_reduced.csv")

origins <- fread(path_to_buildings)%>% 
  mutate(id = as.character(id))

chunks <- split(origins, rep(1:10, each = nrow(origins) / 10))

date_string <- paste(as.Date("2021-04-04"), paste(paste(15,00,00,sep=":"),00,sep="."),sep = " ")
date <- as.POSIXct(date_string,tz="",format="%Y-%m-%d %H:%M:%OS")

out_all <- c("AA101","AA102","CI201","SA506",
             "AB102","AC102","AF102","AA102","AF104","AB106",
             "AB103","AB104","AB105","AF103","AB101","af104",
             "AF104","aa101","AA101","Af104","Af102","af102",
             "SB301", "",
             "","CA305","SA506","CI201")

any(is.na(all_data$X))
Paris_sp_sf <- st_as_sf(fread(path_to_amen) %>% 
                          mutate(id = as.character(id)) %>%
                          dplyr::filter(!(toupper(CODE_ACTIVITE) %in% out_all) & !is.na(X))%>% 
                          dplyr::select(id,X,Y,CODE_ACTIVITE), coords = c("X","Y"), crs = 2154)

Paris_sp_sf <- st_transform(Paris_sp_sf, 4326)

r5_core <- setup_r5(
  paste0(current_dir,"/R5/OSM"),
  verbose = FALSE,
  temp_dir = FALSE,
  elevation = "NONE",
  overwrite = F
)

for(chunk in 1:10){
  new_origins <- chunks[[chunk]]
  travel_time <- travel_time_matrix(r5r_core = r5_core, origins = new_origins, destinations = Paris_sp_sf, mode = "WALK",
                                    verbose = F, progress = T, max_trip_duration = 16, walk_speed = 4)
  
  print(chunk)
  fwrite(travel_time,paste0("R5/results_oldstyle_4kmh_26min_2/",as.character(chunk),".csv"),row.names = F)
  
  gc()
  rJava::.jgc(R.gc = TRUE)
}


r5r::stop_r5(r5_core)
rJava::.jgc(R.gc = TRUE)
rm(travel_time)
gc()

together <- data.table(from_id = "character", to_id = "character",)
for(file in 1:10){
  
}

