# --- Libraries ---
# install.packages(c("data.table", "tidyverse", "here", "fs"))
library(data.table)
library(tidyverse) # For year extraction if needed, though data.table can do it
library(here)      # For robust path management (optional)
library(fs)        # For file system operations

# --- Configuration ---
# Base directory where your R5 project and accessibility counts reside
current_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris"
setwd(current_dir)

# Directory where the individual year accessibility count files are stored
# This should be the 'aggregated_output_dir' from the previous script
accessibility_counts_dir <- file.path(current_dir, "R5", "accessibility_counts")

# Travel time threshold used to generate the count files (must match filenames)
TRAVEL_TIME_THRESHOLD_MIN <- 15

# Output file name for the combined data
combined_output_filename <- paste0("ALL_YEARS_ACCESSIBILITY", TRAVEL_TIME_THRESHOLD_MIN, "min.csv")
combined_output_filepath <- file.path(accessibility_counts_dir, combined_output_filename)


# --- Main Processing ---

message(paste("Looking for accessibility count files in:", accessibility_counts_dir))

# 1. List all relevant accessibility count files
# Files are expected to be named like: accessibility_counts_YYYY_TTmin.csv
file_pattern <- paste0("accessibility_counts_\\d{4}_", TRAVEL_TIME_THRESHOLD_MIN, "min\\.csv$")
count_files <- fs::dir_ls(accessibility_counts_dir, regexp = file_pattern, recurse = FALSE)

if (length(count_files) == 0) {
  stop(paste("No accessibility count files found matching the pattern in:", accessibility_counts_dir,
             "\nPlease check the directory and TRAVEL_TIME_THRESHOLD_MIN setting."))
}

message(paste("Found", length(count_files), "accessibility count files to process:"))
print(basename(count_files))

# 2. Loop through each file, read it, modify, and collect into a list
list_of_yearly_data <- list()

for (file_path in count_files) {
  current_filename <- basename(file_path)
  message(paste("Processing file:", current_filename))
  
  # Extract year from filename (e.g., from "accessibility_counts_2020_15min.csv")
  # Using str_extract from stringr (tidyverse) or base R's regmatches/gregexpr
  year_match <- stringr::str_extract(current_filename, "(?<=accessibility_counts_)\\d{4}(?=_)")
  
  if (is.na(year_match)) {
    warning(paste("Could not extract year from filename:", current_filename, ". Skipping this file."))
    next
  }
  current_year <- as.integer(year_match)
  year_suffix <- substr(as.character(current_year), 3, 4) # Last two digits of the year
  
  # Read the data using data.table
  # Expected columns: origin_id, CODE_ACTIVITE, count
  dt <- fread(file_path)
  
  if (!all(c("origin_id", "CODE_ACTIVITE", "count") %in% names(dt))) {
    warning(paste("File", current_filename, "does not contain expected columns (origin_id, CODE_ACTIVITE, count). Skipping."))
    next
  }
  
  # Modify the data as requested:
  # a. Append last two digits of the year to 'origin_id' (let's create a new ID for this, or modify if sure)
  #    To avoid very long IDs if origin_id is already complex, let's consider the use case.
  #    If origin_id is purely numeric, converting to character first is important.
  #    Let's rename origin_id to building_id as requested.
  #    Original origin_id is preserved if needed, a new year-specific id is created.
  
  dt[, origin_id := paste0(as.character(origin_id), "_", year_suffix)]
  # The request was: "append the last two digits of the year at the end of each id."
  # This implies modifying the 'origin_id' itself.
  # And "The grouped output will therefore have 4 columns : building_id (the origin id, or from_id), CODE_ACTIVITE,  count and year"
  # This suggests the 'building_id' column should be the original origin_id, and the year suffix is just for identification during merge if needed
  # but the final output column is 'building_id' which is the *original* origin ID.
  
  # Let's stick to the final output spec:
  # building_id (original origin_id), CODE_ACTIVITE, count, year
  
  setnames(dt, "origin_id", "building_id") # Rename to final column name
  
  # b. Add a 'year' column
  dt[, year := current_year]
  
  # Select and reorder columns to match final desired output
  dt <- dt[, .(building_id, CODE_ACTIVITE, count, year)]
  
  list_of_yearly_data[[as.character(current_year)]] <- dt
  
  message(paste("Processed data for year", current_year, "from", current_filename))
}

# 3. Combine all yearly data tables into one
if (length(list_of_yearly_data) == 0) {
  stop("No data was processed successfully. Combined file will not be created.")
}

message("\nCombining data from all processed years...")
combined_dt <- rbindlist(list_of_yearly_data, use.names = TRUE, fill = TRUE)
# use.names = TRUE and fill = TRUE are good practice, though columns should be consistent here.

if (nrow(combined_dt) == 0) {
  stop("Combined data table is empty. Something went wrong.")
}

message(paste("Combined data table has", nrow(combined_dt), "rows."))
message("Summary of 'year' column in combined data:")
print(table(combined_dt$year, useNA = "ifany"))

# 4. Save the combined data table
fwrite(combined_dt, combined_output_filepath)
message(paste("\nSuccessfully combined data and saved to:", combined_output_filepath))
message("Final columns are: ", paste(names(combined_dt), collapse = ", "))

# --- End of Script ---

##### Wide version 

combined_dt_wide = combined_dt %>% select(building_id, CODE_ACTIVITE,count) %>% 
  pivot_wider(.,names_from = "CODE_ACTIVITE", values_from = "count")

fwrite(combined_dt_wide, paste0(combined_output_filepath,"_wide.csv"))

