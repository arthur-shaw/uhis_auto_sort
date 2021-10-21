# =============================================================================
# Set file paths
# =============================================================================

# data
data_dir <- paste0(proj_dir, "data/") # /data/
resource_dir <- paste0(data_dir, "00_resource/")  # /00_resource/
download_dir <- paste0(data_dir, "01_downloaded/")  # /01_downloaded/
combined_dir <- paste0(data_dir, "02_combined/")  # /02_combined/
derived_dir <- paste0(data_dir, "03_derived/")   # /03_derived/

# outputs
output_dir <- paste0(proj_dir, "output/")    # /output/


# =============================================================================
# Purge past data
# =============================================================================

# remove zip files


# remove unzipped folders and the data they contain

# =============================================================================
# Define function for checking that necessary objects exist
# =============================================================================

check_exists <- function(object_names) {
  names_sought <- object_names
  names_found <- purrr::map_lgl(
    .x = names_sought,
    .f = ~ exists(.x)
  )
  names_missing <- names_sought[!names_found]
  if (length(names_missing) > 0) {
    
    missing_list <- glue::glue_collapse(
      glue::glue("{glue::backtick(names_missing)}"), 
      sep = ", ", 
      last = ", and "
    )
    
    stop(glue::glue("The following objects are missing: {missing_list}"))
    
  }
}