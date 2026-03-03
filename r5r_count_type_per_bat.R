# --- Libraries ---
# install.packages(c("data.table", "tidyverse", "here", "fs"))
library(data.table)
library(tidyverse) # For dplyr if preferred, and purrr for map_dfr
library(here)      # For robust path management (optional, but good practice)
library(fs)        # For directory listing


bio <- fread("D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/BDCOM_2023.csv") %>% filter(bio == 1) %>%
  group_by(codact) %>% summarise(count = n())

names <- readxl::read_xlsx("D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/BDCOM_2023_OD.xlsx")

bio <- left_join(bio, names %>% select(`Code activité (224 postes)`, `Libellé activité (224 postes)`),
                 by = c("codact" = "Code activité (224 postes)"))

bio20 <- fread("D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/BDCOM_2020.csv") %>% filter(BIO == "oui") %>%
  group_by(CODE_ACTIVITE) %>% summarise(count = n())

names <- readxl::read_xlsx("D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/BDCOM_2023_OD.xlsx")

bio20 <- left_join(bio20, names %>% select(`Code activité (224 postes)`, `Libellé activité (224 postes)`),
                   by = c("CODE_ACTIVITE" = "Code activité (224 postes)"))

bio17 <- fread("D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/BDCOM_2017.csv") %>% filter(BIO == "oui") %>%
  group_by(CODE_ACTIVITE) %>% summarise(count = n())

names <- readxl::read_xlsx("D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/BDCOM_2023_OD.xlsx")

bio17 <- left_join(bio17, names %>% select(`Code activité (224 postes)`, `Libellé activité (224 postes)`),
                   by = c("CODE_ACTIVITE" = "Code activité (224 postes)"))


tobio <- c("CA202","CH303","CC202","CA202","CC201","CA301","CA103","CA108","CA302")


current_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris"
setwd(current_dir)
# --- Configuration ---
# Base directory where your R5 project and data reside
# Using here() makes paths relative to your R project root if you're using one.
# Otherwise, set absolute paths.
# base_project_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"
# data_base_dir    <- "D:/Users/Calum/Documents/La_Rochelle/DATA"

# For demonstration, let's assume the script is in the project root or use absolute paths
base_project_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht" # Assumes R project root is the Utrecht folder
# If not using an R project, or script is elsewhere:
# base_project_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht" 
data_base_dir <- "D:/Users/Calum/Documents/La_Rochelle/DATA"
data_base_dir_to_use <- "D:/Users/Calum/Documents/La_Rochelle/DATA"


# Travel time threshold in minutes
TRAVEL_TIME_THRESHOLD_MIN <- 15

# Years to process
YEARS_TO_PROCESS <- c(2014, 2017, 2020, 2023) # Add/remove years as needed

# Suffix of the TTM results folders (must match what was generated previously)
# Example: if folder is "results_buildings_to_com_2020_WALK_4kmh_26min"
# the common part is "results_buildings_to_com_" and parameters are specific.
# Let's construct this based on the parameters used for generating TTMs.
# If your TTMs were generated with WALK, 4kmh, 26min max_trip_duration:
TTM_MODE <- "WALK"
TTM_WALK_SPEED_KMH <- 4.0
TTM_MAX_TRIP_DURATION_MIN <- 26
TTM_FOLDER_SUFFIX <- "custom_suffix" # If you used an additional custom suffix

# Function to construct the TTM folder name based on parameters
get_ttm_folder_name <- function(year, mode, speed, duration, suffix = "") {
  paste0(
    "results_buildings_to_com_", year,
    "_", paste(mode, collapse="-"),
    "_", speed, "kmh",
    "_", duration, "min",
    if (nzchar(suffix)) paste0("_", suffix) else ""
  )
}

# Output directory for the aggregated results
aggregated_output_dir <- file.path(current_dir, "R5", "accessibility_counts")
dir.create(aggregated_output_dir, recursive = TRUE, showWarnings = FALSE)

# Codes to exclude (same as in the TTM generation script)
CODES_TO_EXCLUDE = c("AA101", "AA102", "CI201", "SA506", "SB301")


# --- Helper Function to Load and Prepare Destination Info (ID and CODE_ACTIVITE) ---
get_destination_info_for_year <- function(year, data_root_dir, codes_to_exclude) {
  message(paste("Loading destination info for year:", year))
  dest_dt <- NULL
  
  bdcom_data_path <- file.path(data_root_dir, "BDCom")
  
  if (year == 2014) {
    dest_csv_path <- file.path(bdcom_data_path, "commercesparis.csv")
    if (!fs::file_exists(dest_csv_path)) {
      warning(paste("2014 destinations CSV not found:", dest_csv_path)); return(NULL)
    }
    dest_dt <- fread(dest_csv_path, sep = ";", encoding = "UTF-8", select = c("X", "Y", "CODE ACTIVITE"))
    setnames(dest_dt, "CODE ACTIVITE", "CODE_ACTIVITE")
    dest_dt$BIO = NA# Standardize column name
  } else {
    dest_csv_path <- file.path(bdcom_data_path, paste0("BDCOM_", year, ".csv"))
    if (!fs::file_exists(dest_csv_path)) {
      warning(paste(year, "destinations CSV not found:", dest_csv_path)); return(NULL)
    }
    if(year == 2023){
      dest_dt <- fread(dest_csv_path, encoding = "UTF-8", select = c("X", "Y", "codact","bio"))
      setnames(dest_dt, "codact", "CODE_ACTIVITE")
      setnames(dest_dt, "bio", "BIO")}
    else{dest_dt <- fread(dest_csv_path, encoding = "UTF-8", select = c("X", "Y", "CODE_ACTIVITE","BIO"))}
  }
  
  # Filter out NAs in coordinates and excluded codes
  dest_dt <- dest_dt[!is.na(X) & !is.na(Y)]
  dest_dt <- dest_dt[!toupper(CODE_ACTIVITE) %in% toupper(codes_to_exclude)]
  
  dest_dt[, to_id := as.character(seq.int(nrow(dest_dt)))]
  
  dest_dt$CODE_ACTIVITE = ifelse(dest_dt$BIO %in% c("oui",1) & dest_dt$CODE_ACTIVITE %in% tobio, 
                                 paste0(dest_dt$CODE_ACTIVITE,"_Bio"), dest_dt$CODE_ACTIVITE)
  
  dest_dt[,BIO:=NULL]
  
  return(dest_dt[, .(to_id, CODE_ACTIVITE)])
}


# --- Main Processing Loop ---
all_years_accessibility <- list()

#YEARS_TO_PROCESS = c("2023")
for (current_year in YEARS_TO_PROCESS) {
  message(paste("\n--- Processing Year:", current_year, "---"))
  
  # 1. Construct path to the TTM results for the current year
  ttm_folder_name <- get_ttm_folder_name(
    year = current_year, 
    mode = TTM_MODE, 
    speed = TTM_WALK_SPEED_KMH, 
    duration = TTM_MAX_TRIP_DURATION_MIN,
    suffix = TTM_FOLDER_SUFFIX
  )
  ttm_results_path <- file.path(current_dir, "R5", ttm_folder_name)
  
  if (!fs::dir_exists(ttm_results_path)) {
    warning(paste("TTM results directory not found for year", current_year, ":", ttm_results_path, "\nSkipping this year."))
    next
  }
  
  combined_ttm_filename <- paste0("ALL_ttm_", ttm_folder_name, ".csv")
  combined_ttm_file_path <- file.path(ttm_results_path, combined_ttm_filename)
  
  if (!fs::file_exists(combined_ttm_file_path)) {
    warning(paste("Combined TTM file not found for year", current_year, ":", combined_ttm_file_path, "\nSkipping this year."))
    next
  }
  
  message(paste("Found combined TTM file for year", current_year, ":", combined_ttm_file_path))
  
  # 3. Load destination info (to_id and CODE_ACTIVITE) for the current year
  # Make sure data_base_dir is correctly defined at the top
  dest_info_dt <- get_destination_info_for_year(current_year, data_base_dir_to_use, CODES_TO_EXCLUDE)
  
  
  if (is.null(dest_info_dt) || nrow(dest_info_dt) == 0) {
    warning(paste("No destination info loaded for year", current_year, ". Skipping."))
    next
  }
  
  # 4. Read and process TTM chunks
  # Efficiently read all CSVs into one data.table
  # Specify colClasses to ensure 'from_id' and 'to_id' are read as character, matching dest_info_dt$to_id
  # and 'min_travel_time' as numeric/integer.
  col_classes_ttm <- c(from_id = "character", to_id = "character", min_travel_time = "numeric")
  ttm_full_dt <- fread(combined_ttm_file_path, select = names(col_classes_ttm), colClasses = col_classes_ttm)
  
  if (nrow(ttm_full_dt) == 0) {
    warning(paste("No data loaded from TTM chunk files for year", current_year, ". Skipping."))
    next
  }
  
  message(paste("Loaded", nrow(ttm_full_dt), "rows from TTM files for year", current_year))
  
  # 5. Filter TTM by travel time threshold
  ttm_filtered_dt <- ttm_full_dt[min_travel_time <= TRAVEL_TIME_THRESHOLD_MIN & !is.na(min_travel_time)]
  
  if (nrow(ttm_filtered_dt) == 0) {
    message(paste("No OD pairs within", TRAVEL_TIME_THRESHOLD_MIN, "minutes for year", current_year, ". No output for this year."))
    next
  }
  
  message(paste(nrow(ttm_filtered_dt), "OD pairs within", TRAVEL_TIME_THRESHOLD_MIN, "minutes for year", current_year))
  
  # 6. Join with destination info to get CODE_ACTIVITE
  # Ensure 'to_id' is character in both tables for the join
  setkey(ttm_filtered_dt, to_id)
  setkey(dest_info_dt, to_id)
  
  # Perform the merge (join)
  # We only need from_id and CODE_ACTIVITE from the joined table for counting
  accessible_dest_dt <- dest_info_dt[ttm_filtered_dt, .(from_id, CODE_ACTIVITE), on = "to_id", nomatch = 0]
  # nomatch = 0 ensures it's an inner join; rows in ttm_filtered_dt without a match in dest_info_dt are dropped.
  # This shouldn't happen if dest_info_dt correctly represents all possible to_ids from TTM generation.
  
  if (nrow(accessible_dest_dt) == 0) {
    message(paste("No destinations after joining with CODE_ACTIVITE for year", current_year, ". This might indicate an issue with 'to_id' matching."))
    next
  }
  
  # 7. Group by origin ID (from_id) and CODE_ACTIVITE, then count
  accessibility_counts_dt <- accessible_dest_dt[, .(count = .N), by = .(from_id, CODE_ACTIVITE)]
  setnames(accessibility_counts_dt, "from_id", "origin_id") # Rename for clarity
  
  message(paste("Calculated accessibility counts for", nrow(accessibility_counts_dt), "origin-CODE_ACTIVITE pairs for year", current_year))
  
  # 8. Save the result for the current year
  output_filename <- file.path(aggregated_output_dir, paste0("accessibility_counts_", current_year, "_", TRAVEL_TIME_THRESHOLD_MIN, "min.csv"))
  fwrite(accessibility_counts_dt, output_filename)
  message(paste("Saved results for year", current_year, "to:", output_filename))
  
  # Optional: Store in the list for combined processing later if needed
  all_years_accessibility[[as.character(current_year)]] <- accessibility_counts_dt
  
  # Clean up to save memory before next iteration
  rm(ttm_full_dt, ttm_filtered_dt, dest_info_dt, accessible_dest_dt, accessibility_counts_dt)
  gc()
}

message("\n--- All years processed. ---")

# Example: If you want to combine all years into one large data.table (if memory allows)
# if (length(all_years_accessibility) > 0) {
#   combined_accessibility_dt <- rbindlist(all_years_accessibility, idcol = "year")
#   fwrite(combined_accessibility_dt, file.path(aggregated_output_dir, paste0("ALL_YEARS_accessibility_counts_", TRAVEL_TIME_THRESHOLD_MIN, "min.csv")))
#   message("Saved combined results for all processed years.")
# }