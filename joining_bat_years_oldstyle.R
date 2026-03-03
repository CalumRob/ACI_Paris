# --- Libraries ---
# install.packages(c("data.table", "tidyverse", "here", "fs"))
library(data.table)
library(tidyverse) # For dplyr if preferred, and purrr for map_dfr
library(here)      # For robust path management (optional, but good practice)
library(fs)        # For directory listing


current_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris"


utrecht_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"
data_base_dir <- "D:/Users/Calum/Documents/La_Rochelle/DATA"

TRAVEL_TIME_THRESHOLD_MIN <- 15

ttm_path = file.path(current_dir,"R5/results_oldstyle_4kmh_26min_2")
# 
all_chunk_files <- list.files(ttm_path, full.names = TRUE)
# 
all_data <- data.table()
for(file in all_chunk_files){
   current <- fread(file)
   all_data <- rbind(all_data, current)
 }
# 
fwrite(all_data, paste0(ttm_path,"/Combined.csv"))
gc()


all_data = fread(paste0(ttm_path,"/Combined.csv"))

head(all_data)
nrow(all_data)
all_data <- all_data[travel_time_p50 <= TRAVEL_TIME_THRESHOLD_MIN & !is.na(travel_time_p50)]
nrow(all_data)

gc()


amen_info <- fread(file.path(data_base_dir,"BDCom/BDCOM_14_to_23_cleaned.csv"))

out_all <- c("AA101","AA102","CI201","SA506",
             "AB102","AC102","AF102","AA102","AF104","AB106",
             "AB103","AB104","AB105","AF103","AB101","af104",
             "AF104","aa101","AA101","Af104","Af102","af102",
             "SB301", "",
             "","CA305","SA506","CI201")


amen_info <- amen_info[!is.na(X) & !is.na(Y) & !(CODE_ACTIVITE %in% out_all)]
setnames(amen_info, "id", "to_id")

all_data <- left_join(all_data, amen_info %>% select(-c(X,Y)), by = "to_id")

gc()

all_data <- all_data %>% distinct()

gc()

all_data <- all_data %>%
  group_by(from_id, CODE_ACTIVITE) %>%
  summarise(Year14 = sum(Year_2014),
            Year17 = sum(Year_2017),
            Year20 = sum(Year_2020),
            Year23 = sum(Year_2023))

gc()

head(all_data)

fwrite(all_data, paste0(current_dir,"/codeact_per_bat_2.csv"))

gc()


