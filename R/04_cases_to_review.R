# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "combined_dir"
)

check_exists(objects_needed)

# =============================================================================
# Load all necessary data files
# =============================================================================

hholds  <- haven::read_dta(file = paste0(combined_dir, "UHIS_2021.dta"))
members <- haven::read_dta(file = paste0(combined_dir, "t0_hhroster.dta"))
assets  <- haven::read_dta(file = paste0(combined_dir, "HH_Assets.dta"))
parcels <- haven::read_dta(file = paste0(combined_dir, "HH_PARCELS.dta"))
plots   <- haven::read_dta(file = paste0(combined_dir, "AGRIC_PDN_PLOTS_unoma.dta"))

# =============================================================================
# Check that data files successfully loaded
# =============================================================================

objects_needed <- c(
    "hholds",
    "members",
    "assets",
    "parcels"
)

check_exists(objects_needed)

# =============================================================================
# Load necessary libraries
# =============================================================================

library(dplyr)

# =============================================================================
# Identify cases to review
# =============================================================================

# create cases to review overall
cases_to_review <- hholds %>%
    # complete based on SuSo status
    dplyr::filter(interview__status %in% statuses_to_reject) %>%
    # complete based on data
    dplyr::mutate(
        complete_aas = (
            (sample_type == 2) &    # AAS sample
            (s1bq06a_1 == 1)        # Completed, per the interviewer            
        ),
        complete_unps = (
            (sample_type == 1) &        # UNPS sample
            (s1bq06a %in% c(1, 2))      # Full interview and full for visit, respectively
        ),
        complete_unoma = (
            (sample_type == 3) &        # UNPS sample
            (s1bq06a %in% c(1, 2))      # Full interview and full for visit, respectively            
        ),
        complete_single_phase = (
            (s1aq14 == 4) &             # single phase visit type
            (parcel_posessn_check_s1 == 1)
        ),
        complete_full_interview = (
            (sample_type %in% c(1, 3)) &        # UNPS and UNOMA samples
            (s1bq06a %in% c(1, 2)) &    # Full interview and full for visit, respectively
            (s1aq13 == 1) &             # Full interview
            (s1aq14 %in% c(1, 4))       # first or single-phase visit            
        )
    ) %>%
    dplyr::filter(
        dplyr::if_any(
            .cols = c(
                complete_aas, complete_unps, 
                complete_single_phase, complete_full_interview
            ),
            .fns = ~ .x == 1
        )
    ) %>%
    dplyr::mutate(interview__complete = 1) %>%
    dplyr::select(
        interview__id, interview__key, interview__status,
        PHHID, XHHID, sample_type,
        complete_aas, complete_unps, complete_unoma, 
        complete_single_phase, complete_full_interview,
        interview__complete
    )

# create sets for filtering attributes
cases_aas <- dplyr::filter(cases_to_review, complete_aas == 1)
cases_unps <- dplyr::filter(cases_to_review, complete_unps == 1)
cases_unoma <- dplyr::filter(cases_to_review, complete_unoma == 1)
cases_single_phase <- dplyr::filter(cases_to_review, complete_single_phase == 1)
cases_full_interview <- dplyr::filter(cases_to_review, complete_full_interview == 1)

# =============================================================================
# Load data
# =============================================================================

# -----------------------------------------------------------------------------
# Define function
# -----------------------------------------------------------------------------

load_filtered <- function(
    dir,
    file,
    name = gsub(pattern = "\\.dta", replacement = "", x = file),
    filter_df
) {
    df <- haven::read_dta(file = paste0(dir, file))
    
    df_filtered <- df %>%
        dplyr::semi_join(filter_df, by = c("interview__id", "interview__key"))
    
    assign(
        x = name,
        value = df_filtered,
        envir = .GlobalEnv
    )
    
}

# -----------------------------------------------------------------------------
# Load filtered files
# -----------------------------------------------------------------------------

# TODO: add "interview__diagnostics.dta" once server upgraded
files <- c(
    "interview__errors.dta", "interview__comments.dta"
)
# TODO: add "suso_diagnostics" once server upgraded
file_names <- c(
    "suso_errors", "comments"
)

purrr::walk2(
    .x = files, 
    .y = file_names,
    .f = ~ load_filtered(
        dir = combined_dir,
        file = .x,
        name = .y,
        filter_df = cases_to_review
    )
)
