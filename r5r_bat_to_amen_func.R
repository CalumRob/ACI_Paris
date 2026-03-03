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

# install.packages("rJavaEnv")
# rJavaEnv::rje_consent(provided = TRUE)
# rJavaEnv::java_quick_install(version = 21)
# 
# r5_dir = "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/R5/Street_networks/2023"
# r5_core <- setup_r5(
#   data_path = r5_dir, # Ensure this dir contains your osm.pbf
#   verbose = TRUE,     # Get more output
#   overwrite = TRUE,   # Force rebuild (alternative to manual deletion)
#   temp_dir = FALSE,   # Keep as is for now
#   elevation = "NONE"  # Keep as is
# )
# 
# # Test origin (replace with actual Paris coordinates)
# test_origin <- data.frame(
#   id = "test_o1",
#   lon = 2.3522, # Approx. center of Paris
#   lat = 48.8566
# )
# 
# # Test destination (replace with actual Paris coordinates)
# test_destination <- data.frame(
#   id = "test_d1",
#   lon = 2.3422, 
#   lat = 48.8500
# )
# 
# test_ttm <- travel_time_matrix(
#        r5r_core = r5_core,
#        origins = test_origin,
#        destinations = test_destination,
#        mode = "WALK",
#        walk_speed = 4.0,
#        max_trip_duration = 30,
#        verbose = TRUE,
#        progress = TRUE)



########### UNIQUE IDs per amen (each year)


#dest_csv_path
#dest_csv_path <- file.path(file.path(my_data_dir,"BDCom"), "commercesparis.csv")

dest_data_raw <- readr::read_delim(dest_csv_path, delim = ";", show_col_types = FALSE)

# --- Helper Function to Prepare Points for r5r (from previous refactoring) ---
prepare_points_for_r5r <- function(sf_object, id_col_name) {
  if (!inherits(sf_object, "sf")) {
    stop("Input must be an sf object.")
  }
  if (!id_col_name %in% names(sf_object)) {
    stop(paste("ID column '", id_col_name, "' not found in sf object.", sep=""))
  }
  if (st_crs(sf_object)$epsg != 4326) {
    message("Transforming sf object to EPSG:4326 for r5r.")
    sf_object <- st_transform(sf_object, crs = 4326)
  }
  
  coords <- st_coordinates(sf_object)
  
  df <- tibble(
    id = as.character(sf_object[[id_col_name]]),
    lon = coords[, "X"],
    lat = coords[, "Y"]
  )
  return(df)
}


# --- Main Function to Calculate Travel Times from Buildings to Commerce ---
calculate_building_to_commerce_travel_times <- function(
    destination_year,
    base_project_dir = "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht",
    output_project_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris",
    data_base_dir = "D:/Users/Calum/Documents/La_Rochelle/DATA",
    origins_input_filename = "R5/building_as_origins_reduced.csv", # Relative to base_project_dir
    java_memory_gb = 8,
    num_origin_chunks = 100,
    start_chunk = 1,
    end_chunk = num_origin_chunks,
    r5r_travel_mode = "WALK",
    r5r_walk_speed_kmh = 4.0, # km/h
    r5r_max_trip_duration_min = 20, # minutes
    #r5r_departure_datetime = NULL, # POSIXct datetime object, required for TRANSIT, ignored for WALK
    codes_to_exclude = c("AA101", "AA102", "CI201", "SA506", "SB301"), # CODE_ACTIVITE to exclude
    output_folder_suffix = "" # Optional suffix for output folder name
) {

  # --- 1. Setup and Configuration ---
  message(paste0("--- Starting Travel Time Calculation to Commercial Points for Year: ", destination_year, " ---"))

  options(java.parameters = paste0('-Xmx', java_memory_gb, 'G'))
  gc() # R garbage collection
  if (requireNamespace("rJava", quietly = TRUE)) {
    rJava::J("java.lang.Runtime")$getRuntime()$gc() # Java garbage collection
  }

  # Define paths
  r5_dir = paste0(base_project_dir,"/R5/Street_networks/",as.character(destination_year))
  #r5_dir <- file.path(base_project_dir, "R5") # For r5r data (OSM, GTFS) and some inputs/outputs
  origins_csv_path <- file.path(base_project_dir, origins_input_filename)
  
  bdcom_dir <- file.path(data_base_dir, "BDCom")

  # Construct output directory name
  output_dir_name <- paste0(
    "results_buildings_to_com_", destination_year,
    "_", paste(r5r_travel_mode, collapse="-"),
    "_", r5r_walk_speed_kmh, "kmh",
    "_", r5r_max_trip_duration_min, "min",
    if (nzchar(output_folder_suffix)) paste0("_", output_folder_suffix) else ""
  )
  final_output_dir <- file.path(output_project_dir, output_dir_name)
  dir.create(final_output_dir, recursive = TRUE, showWarnings = FALSE)

  # --- 2. Load and Prepare Origins (Residential Buildings) ---
  message("Loading and preparing origin points (residential buildings)...")
  if (!file.exists(origins_csv_path)) {
    stop(paste("Origins CSV not found:", origins_csv_path, 
               "\nThis file should contain pre-processed building origins with 'id', 'lon', 'lat' columns (EPSG:4326)."))
  }
  # Using read_csv from readr, can be swapped with data.table::fread
  origins_df <- readr::read_csv(origins_csv_path, show_col_types = FALSE) %>%
    select(id, lon, lat) %>% # Ensure only necessary columns
    mutate(id = as.character(id)) # Ensure ID is character for r5r
  
  if (!all(c("id", "lon", "lat") %in% names(origins_df))) {
    stop("Origins file must contain 'id', 'lon', and 'lat' columns.")
  }
  message(paste("Loaded", nrow(origins_df), "origin points."))

  # --- 3. Load and Prepare Destinations (Commercial Points for specified year) ---
  message(paste("Loading and preparing destination points for year:", destination_year))
  
  dest_data_raw <- NULL
  dest_id_col <- "AUTO_ID" # Default ID column to be created
  
  if (destination_year == 2014) {
    dest_csv_path <- file.path(bdcom_dir, "commercesparis.csv")
    if (!file.exists(dest_csv_path)) stop(paste("2014 destinations CSV not found:", dest_csv_path))
    dest_data_raw <- readr::read_delim(dest_csv_path, delim = ";", show_col_types = FALSE) %>%# Handle potential weird X column name
      rename(CODE_ACTIVITE = any_of(c("CODE_ACTIVITE", "CODE.ACTIVITE", "Code activité (224 postes)", "CODE ACTIVITE"))) # Handle variable col name
  } else {
    # For 2017, 2020, 2023 (assuming 2023 follows this pattern)
    dest_csv_path <- file.path(bdcom_dir, paste0("BDCOM_", destination_year, ".csv"))
    if (!file.exists(dest_csv_path)) stop(paste(destination_year, "destinations CSV not found:", dest_csv_path))
    dest_data_raw <- readr::read_csv(dest_csv_path, show_col_types = FALSE)%>% # Handle potential weird X column name
      rename(CODE_ACTIVITE = any_of(c("CODE_ACTIVITE", "CODE.ACTIVITE", "codact")))
  }
  
  # Common processing for destinations
  destinations_sf <- dest_data_raw %>%
    filter(!is.na(X) & !is.na(Y)) %>% # Ensure coordinates are not NA
    filter(!toupper(CODE_ACTIVITE) %in% toupper(codes_to_exclude)) %>%
    mutate(!!dest_id_col := seq.int(nrow(.))) %>% # Create a unique ID for r5r
    st_as_sf(coords = c("X", "Y"), crs = 2154, remove = FALSE) # Initial CRS is Lambert 93

  # Check if any destinations remain after filtering
  if (nrow(destinations_sf) == 0) {
    stop(paste("No destination points remaining for year", destination_year, "after filtering. Check CSV and filters."))
  }
  
  # Prepare for r5r input format (transforms to 4326)
  destinations_r5 <- prepare_points_for_r5r(destinations_sf, dest_id_col)
  message(paste("Loaded and prepared", nrow(destinations_r5), "destination points for", destination_year, "."))

  # --- 4. Setup r5r Core ---
  message("Setting up r5r core...")
  # r5_dir should contain the osm.pbf and any GTFS.zip files
  if (!dir.exists(r5_dir) || length(list.files(r5_dir, pattern = "\\.pbf$|\\.zip$")) == 0) {
      stop(paste("r5r data directory '", r5_dir, "' does not exist or does not contain OSM/GTFS files.", sep=""))
  }
  r5_core <- setup_r5(
    data_path = r5_dir,
    verbose = FALSE,
    temp_dir = FALSE, # Use TRUE if memory is an issue
    overwrite = FALSE # Don't rebuild graph if one exists and data hasn't changed
  )

  # --- 5. Main Travel Time Calculation (Chunked) ---
  message(paste0("Starting travel time calculation in '", paste(r5r_travel_mode, collapse=", "), "' mode."))
  
  if (nrow(origins_df) < num_origin_chunks) num_origin_chunks <- max(1, nrow(origins_df))
  
  origin_groups <- cut(seq_len(nrow(origins_df)), breaks = num_origin_chunks, labels = FALSE)
  list_of_origin_chunks <- split(origins_df, origin_groups)
  
  actual_end_chunk <- min(end_chunk, length(list_of_origin_chunks))
  message(paste("Processing origin chunks from", start_chunk, "to", actual_end_chunk, "out of", length(list_of_origin_chunks), "total chunks."))

  # Prepare departure_datetime if it's a string (like in your original script)
  # Or ensure it's a POSIXct object if passed directly



  for (i in seq(start_chunk, actual_end_chunk)) {
    current_origins_chunk <- list_of_origin_chunks[[i]]
    message(paste0("Processing Chunk ", i, "/", length(list_of_origin_chunks), " (", nrow(current_origins_chunk), " origins)"))
    
    chunk_output_file <- file.path(final_output_dir, paste0("ttm_chunk_", i, ".csv"))

    if (file.exists(chunk_output_file)) {
        message(paste("Chunk", i, "output already exists. Skipping."))
        next
    }

    tt_chunk <- travel_time_matrix(
      r5r_core = r5_core,
      origins = current_origins_chunk,
      destinations = destinations_r5,
      mode = r5r_travel_mode,
      #mode_egress = if("TRANSIT" %in% r5r_travel_mode) "WALK" else NULL, # Egress mode for transit
      #departure_datetime = r5r_departure_datetime, # Required for TRANSIT, ignored for WALK alone
      #time_window = if("TRANSIT" %in% r5r_travel_mode) 10 else NULL, # Time window for transit
      max_walk_time = r5r_max_trip_duration_min, # Overall max walk for the trip if transit, or total trip time for walk only
      max_trip_duration = r5r_max_trip_duration_min,
      walk_speed = r5r_walk_speed_kmh,
      verbose = FALSE,
      progress = TRUE # Show progress for each chunk
    )
    
    if (!is.null(tt_chunk) && nrow(tt_chunk) > 0) {
      # travel_time_p50 is usually the median. If not present or NA, use travel_time.
      # For WALK mode, travel_time is the primary output.
      aggregated_tt_chunk <- tt_chunk %>%
        mutate(
          travel_time_selected = ifelse(
            "travel_time_p50" %in% names(.) & !is.na(travel_time_p50), 
            travel_time_p50, 
            travel_time
          )
        ) %>%
        group_by(from_id, to_id) %>%
        summarise(
          min_travel_time = min(travel_time_selected, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(min_travel_time = ifelse(is.infinite(min_travel_time), NA_integer_, as.integer(min_travel_time)))
      
      # Using write_csv from readr
      readr::write_csv(aggregated_tt_chunk, chunk_output_file)
      message(paste("Chunk", i, "results saved to:", chunk_output_file))
    } else {
      message(paste("Chunk", i, "produced no results or an error occurred."))
    }
    
    rm(tt_chunk, aggregated_tt_chunk)
    gc()
    if (requireNamespace("rJava", quietly = TRUE)) {
      rJava::J("java.lang.Runtime")$getRuntime()$gc() # More aggressive Java GC
    }
  }
  
  # --- 6. Cleanup and Finalization ---
  message("Stopping r5r core...")
  r5r::stop_r5(r5_core)

  message(paste0("--- Processing for destinations of year ", destination_year, " complete. ---"))
  
  gc()
  
  # Combine all chunked files
  all_chunk_files <- list.files(final_output_dir, pattern = "^ttm_chunk_.*\\.csv$", full.names = TRUE)
  # Check if we expect these files to be present based on start_chunk and actual_end_chunk
  expected_chunk_indices <- seq(start_chunk, actual_end_chunk)
  expected_chunk_files_count <- length(expected_chunk_indices)
  
  # Filter all_chunk_files to only include those that were supposed to be processed in this run
  processed_chunk_files <- all_chunk_files[
      as.integer(str_extract(basename(all_chunk_files), "(?<=^ttm_chunk_)[0-9]+(?=\\.csv$)")) %in% expected_chunk_indices
  ]

  if (length(processed_chunk_files) == expected_chunk_files_count && expected_chunk_files_count > 0) {
      message("Combining all processed chunk files...")
      # Using map_dfr from purrr (part of tidyverse)
      combined_results <- map_dfr(processed_chunk_files, readr::read_csv, show_col_types = FALSE)
      
      if (nrow(combined_results) > 0) {
        combined_file_path <- file.path(final_output_dir, paste0("ALL_ttm_", basename(final_output_dir), ".csv"))
        readr::write_csv(combined_results, combined_file_path)
        message(paste0("All results combined into: ", combined_file_path))
      } else {
        message("No data in combined results. Combined file not written.")
      }
  } else if (expected_chunk_files_count > 0) {
      message(paste("Not all expected chunks (", expected_chunk_files_count, ") seem to be processed or found (found ", length(processed_chunk_files), "). Combined file not created.", sep=""))
  } else {
      message("No chunks were processed in this run. Combined file not created.")
  }

  return(final_output_dir)
}

# --- Example Usage ---

# ==== FIRST TIME SETUP FOR ORIGINS (Residential Buildings) ====
# This is how you might create 'building_as_origins_reduced.csv' if it doesn't exist.
# This part is NOT in the main function, but shown for completeness.
# You would run this once.

# preprocess_building_origins <- function(
#     raw_origins_csv = "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/BDTopo_resid_points.csv",
#     output_csv = "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht/R5/building_as_origins_reduced.csv"
#   ) {
#   message("Preprocessing raw building origins...")
#   origins_raw <- readr::read_csv(raw_origins_csv, show_col_types = FALSE)
# 
#   # Parse WKT: "POINT (lon lat)"
#   # Ensure stringr is loaded for str_extract
#   origins_coords <- origins_raw %>%
#     mutate(
#       lon_str = stringr::str_extract(WKT, "(?<=\\POINT \\()[0-9\\.]+"),
#       lat_str = stringr::str_extract(WKT, "[0-9\\.]+(?=\\))")
#     ) %>%
#     mutate(
#       lon = as.numeric(lon_str),
#       lat = as.numeric(lat_str)
#     ) %>%
#     filter(!is.na(lon) & !is.na(lat))
# 
#   if (nrow(origins_coords) == 0) stop("No valid coordinates parsed from WKT.")
# 
#   origins_sf <- st_as_sf(origins_coords, coords = c("lon", "lat"), crs = 2154, remove = FALSE) %>%
#     st_transform(crs = 4326) # Transform to WGS84 for r5r
# 
#   coords_4326 <- st_coordinates(origins_sf)
# 
#   origins_for_r5r <- tibble(
#     id = seq.int(nrow(origins_sf)), # Simple sequential ID
#     lon = coords_4326[, "X"],
#     lat = coords_4326[, "Y"]
#   )
#   
#   readr::write_csv(origins_for_r5r, output_csv)
#   message(paste("Preprocessed origins saved to:", output_csv))
#   return(origins_for_r5r)
# }
# # Example: Run preprocessing if needed
# # building_origins_df <- preprocess_building_origins() 


# ==== RUNNING THE MAIN ANALYSIS FUNCTION ====

# Define base paths (adjust to your system)
my_project_dir <- "D:/Users/Calum/Documents/La_Rochelle/Second_paper/Fixe/Fixe/Utrecht"
my_data_dir    <- "D:/Users/Calum/Documents/La_Rochelle/DATA"

# --- Example for 2020 Destinations (WALK mode) ---

# results_path_2023_walk <- calculate_building_to_commerce_travel_times(
#    destination_year = 2023,
#    base_project_dir = my_project_dir,
#    data_base_dir = my_data_dir,
#    output_project_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris",
#    origins_input_filename = "R5/building_as_origins_reduced.csv", # Ensure this file exists
#    java_memory_gb = 8,
#    num_origin_chunks = 100, # Adjust as needed
#    r5r_travel_mode = "WALK",
#    r5r_walk_speed_kmh = 4.0,
#    r5r_max_trip_duration_min = 26,
#    output_folder_suffix = "custom_suffix" # Optional
#  )
# 
# message("2030 WALK results are in: ", results_path_2023_walk)
# 
# 
# .jinit() # Initialize Java
# print(J("java.lang.System")$getProperty("java.version"))
# print(J("java.lang.System")$getProperty("java.home"))
# 
# 
# results_path_2023_walk <- calculate_building_to_commerce_travel_times(
#   destination_year = 2020,
#   base_project_dir = my_project_dir,
#   data_base_dir = my_data_dir,
#   output_project_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris",
#   origins_input_filename = "R5/building_as_origins_reduced.csv", # Ensure this file exists
#   java_memory_gb = 8,
#   num_origin_chunks = 100, # Adjust as needed
#   r5r_travel_mode = "WALK",
#   r5r_walk_speed_kmh = 4.0,
#   r5r_max_trip_duration_min = 26,
#   output_folder_suffix = "custom_suffix" # Optional
# )
# 
# message("2030 WALK results are in: ", results_path_2023_walk)
# 
# results_path_2023_walk <- calculate_building_to_commerce_travel_times(
#   destination_year = 2017,
#   base_project_dir = my_project_dir,
#   data_base_dir = my_data_dir,
#   output_project_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris",
#   origins_input_filename = "R5/building_as_origins_reduced.csv", # Ensure this file exists
#   java_memory_gb = 8,
#   num_origin_chunks = 100, # Adjust as needed
#   r5r_travel_mode = "WALK",
#   r5r_walk_speed_kmh = 4.0,
#   r5r_max_trip_duration_min = 26,
#   output_folder_suffix = "custom_suffix" # Optional
# )
# 
# message("2030 WALK results are in: ", results_path_2023_walk)

results_path_2023_walk <- calculate_building_to_commerce_travel_times(
  destination_year = 2014,
  base_project_dir = my_project_dir,
  data_base_dir = my_data_dir,
  output_project_dir = "D:/Users/Calum/Documents/La_Rochelle/ACI_2025/Amenities-Paris",
  origins_input_filename = "R5/building_as_origins_reduced.csv", # Ensure this file exists
  java_memory_gb = 8,
  num_origin_chunks = 100, # Adjust as needed
  r5r_travel_mode = "WALK",
  r5r_walk_speed_kmh = 4.0,
  r5r_max_trip_duration_min = 26,
  output_folder_suffix = "custom_suffix" # Optional
)

message("2030 WALK results are in: ", results_path_2023_walk)

# --- Example for 2023 Destinations (WALK mode) ---
# Ensure "BDCOM_2023.csv" exists in "D:/Users/Calum/Documents/La_Rochelle/DATA/BDCom/"
# results_path_2023_walk <- calculate_building_to_commerce_travel_times(
#   destination_year = 2023,
#   base_project_dir = my_project_dir,
#   data_base_dir = my_data_dir,
#   origins_input_filename = "R5/building_as_origins_reduced.csv",
#   java_memory_gb = 8,
#   r5r_travel_mode = "WALK",
#   r5r_walk_speed_kmh = 4.0,
#   r5r_max_trip_duration_min = 26
# )
# message("2023 WALK results are in: ", results_path_2023_walk)

# --- Example for 2014 Destinations (WALK mode, special CSV) ---
# results_path_2014_walk <- calculate_building_to_commerce_travel_times(
#   destination_year = 2014,
#   base_project_dir = my_project_dir,
#   data_base_dir = my_data_dir,
#   origins_input_filename = "R5/building_as_origins_reduced.csv",
#   java_memory_gb = 8,
#   r5r_travel_mode = "WALK",
#   r5r_walk_speed_kmh = 4.0,
#   r5r_max_trip_duration_min = 26
# )
# message("2014 WALK results are in: ", results_path_2014_walk)


# --- Example for TRANSIT mode (e.g., 2020 destinations) ---
# transit_departure_time <- as.POSIXct("2023-10-26 08:30:00", tz = "Europe/Paris") # Example
# results_path_2020_transit <- calculate_building_to_commerce_travel_times(
#   destination_year = 2020,
#   base_project_dir = my_project_dir,
#   data_base_dir = my_data_dir,
#   origins_input_filename = "R5/building_as_origins_reduced.csv",
#   java_memory_gb = 8,
#   num_origin_chunks = 100,
#   r5r_travel_mode = c("WALK", "TRANSIT"), # For multimodal
#   r5r_walk_speed_kmh = 4.0,
#   r5r_max_trip_duration_min = 60, # Max total trip duration
#   r5r_departure_datetime = transit_departure_time # Crucial for transit
#   # Note: for transit, max_walk_time inside travel_time_matrix also considers this.
#   # The parameter max_walk_time in r5r::travel_time_matrix controls maximum access/egress/transfer walk.
#   # Here r5r_max_trip_duration_min is used for both max_walk_time and max_trip_duration,
#   # which is fine for WALK mode, but for TRANSIT you might want to distinguish these.
#   # The function could be extended to have a separate r5r_max_access_walk_min parameter.
# )
# message("2020 TRANSIT results are in: ", results_path_2020_transit)

# To resume a failed run (e.g., failed at chunk 50 of 100):
# results_path_resume <- calculate_building_to_commerce_travel_times(
#   destination_year = 2020,
#   base_project_dir = my_project_dir,
#   data_base_dir = my_data_dir,
#   origins_input_filename = "R5/building_as_origins_reduced.csv",
#   start_chunk = 50, # Start from chunk 50
#   end_chunk = 100,   # Go up to 100
#   r5r_travel_mode = "WALK",
#   r5r_walk_speed_kmh = 4.0,
#   r5r_max_trip_duration_min = 26
# )