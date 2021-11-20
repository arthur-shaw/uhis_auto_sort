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

# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "proj_dir"
)

check_exists(objects_needed)

# =============================================================================
# Folders for scripts
# =============================================================================

# scripts
script_dir <- paste0(proj_dir, "R/")

# =============================================================================
# Run scripts in required order
# =============================================================================

# prepare
# - set file paths
# - purge prior data
print("Setup")
source(paste0(script_dir, "00_setup.R"))

# get data
# - fetch
# print("Fetching data")
# source(paste0(script_dir, "01_get_data.R"))

# combine data
# - unpack data
# - combine and save unzipped Stata data
print("Unzipping and combining data files")
source(paste0(script_dir, "02_combine_data.R"))

# combine food consumption data
# - ingest all food files
# - harmonize variable names in files
# - extract, combine, and apply value labels
print("Combining food consumption roster data")
source(paste0(script_dir, "03_combine_food.R"))

# identify cases to review
# - create frame of all cases to review
# - create data sets for filtering
print("Identifying cases to review")
source(paste0(script_dir, "04_cases_to_review.R"))

if (nrow(cases_to_review) == 0) {
    warning("Currently no interviews to process that can be rejected")
} else {

# compile attributes
print("Compiling attributes")
source(paste0(script_dir, "05_compile_attributes.R"))

# compile issues
# - from attributes and microdata
# - from interview metadata
print("Compiling issues")
source(paste0(script_dir, "06_compile_issues.R"))

# make decisions
# - what to reject
# - what to review
print("Making decisions about what to reject and to review")
source(paste0(script_dir, "07_make_decisions.R"))

# execute decisions
# - post comments to individual questions
# - reject interviews
# if (should_reject == TRUE) {
# print("Executing decisions")
# source(paste0(script_dir, "08_execute_decisions.R"))
# }

# save decisions to disk
print("Saving decisions to disk")
source(paste0(script_dir, "09_save_results.R"))

}
