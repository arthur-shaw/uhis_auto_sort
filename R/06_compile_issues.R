# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "attribs",
    "food_df",
    "plots_w_indicators",
    "parcel_plots",
    "suso_errors"
)

check_exists(objects_needed)

# =============================================================================
# Load necessary libraries
# =============================================================================

library(susoreview)
library(haven)
library(purrr)
library(rlang)

# =============================================================================
# Flag errors
# =============================================================================

# -----------------------------------------------------------------------------
# Household heads
# -----------------------------------------------------------------------------

# no head
issue_no_head <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "num_heads",
    where = num_heads == 0,
    type = 1,
    desc = "No head",
    comment = paste0(
        "ERROR: No household member designated as head. ",
        "Please identify the member who is the household head."
    )
)

# more than 1 head
issue_too_many_heads <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "num_heads",
    where = num_heads > 1,
    type = 1,
    desc = "More than 1 head",
    comment = paste0(
        "ERROR: More than one person designated as head. ",
        "Please identify the member who is the household head."
    )
)

# -----------------------------------------------------------------------------
# Food consumption
# -----------------------------------------------------------------------------

# no food consumption inside household
issue_no_home_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "num_food_items",
    where = num_food_items == 0,
    type = 1,
    desc = "No home food consumption",
    comment = paste0(
        "ERROR: No food consumption reported. ",
        "The household did not consume any household at home. ",
        "This is highly unlikely. ",
        "Please confirm all the food consumption questions again."
    )
)

# no food consumed--neither inside nor outside the household
issue_no_food_cons <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("food_away_from_home", "num_food_items"),
    where = food_away_from_home == 0 & num_food_items == 0,
    type = 1,
    desc = "No food consumption",
    comment = paste0(
        "ERROR: No food consumption reported. ",
        "The household did not consume any food--neither inside nor outside the household. ",
        "This is not possible. Please ask the food consumption questions again."
    )
)

# -----------------------------------------------------------------------------
# Assets
# -----------------------------------------------------------------------------

issue_no_assets <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "num_assets",
    where = num_assets == 0,
    type = 1,
    desc = "No assets",
    comment = paste0(
        "ERROR: No assets reported. ",
        "No assets reported in section 14. ",
        "This is very unlikely. Please ask the household again."
    )
)

# -----------------------------------------------------------------------------
# Missing food consumption info
# -----------------------------------------------------------------------------

# error
issue_missing_food_info <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "food_item_missing_info",
    where = food_item_missing_info == 1,
    type = 1,
    desc = "Food item missing info",
    comment = paste0(
        "ERROR: Critical info missing for at least one food item. ",
        "Navigate to the food item for further information on what is missing: ",
        "unit, state, quantity, or some combination."
    )
)

# comment on concerned item
issue_where_missing_food_info <- susoreview::make_issue_in_roster(
    df = food_df,
    where = (
        haven::is_tagged_na(CEB05) |
        haven::is_tagged_na(CEB06) |
        haven::is_tagged_na(CEB07) |
        haven::is_tagged_na(CEB10) |
        haven::is_tagged_na(CEB12)
    ),
    roster_vars = "food__id",
    type = 2,
    desc = "Missing quantity, unit, or size",
    comment = "Missing quantity, unit, or size",
    issue_vars = "CEB0[567]|CEB1[02]"
)

# =============================================================================
# Flag critical inconsistencies
# =============================================================================

# -----------------------------------------------------------------------------
# Activities v. holdings
# -----------------------------------------------------------------------------

# member works in hhold ag, but no ag holdings reported
issue_work_ag_no_ag <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("work_on_farm", "num_ag_parcels", "raise_livestock"),
    where = work_on_farm == 1 & num_ag_parcels == 0 & raise_livestock == 0,
    type = 1,
    desc = "Work in hh ag, but no ag holdings",
    comment = paste0(
        "ERROR: Member works on household farm, but no land or livestock reported. ",
        "At least 1 member works on the housheold farm in section 8. ",
        "But no agricultural parcels reported among the household parcels, and ",
        "no livestock reported in the link with agriculture section. ",
        "This is impossible. Please confirm which information is right."
    )
)

# member works in enterprise, but no hhold enterprise reported
issue_work_biz_no_biz <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("work_in_hh_business", "num_businesses"),
    where = work_in_hh_business == 1 & num_businesses == 0,
    type = 1,
    desc = "Work in hh biz, but no hh biz",
    comment = paste0(
        "ERROR: Member works in household business, but no household business reported. ",
        "According to section 8, at least 1 member works in the household business. ",
        "But there is no enterprise reported in section 12. ",
        "This is impossible. Please confirm which information is right."
    )
)

# hhold engaged in agriculture, but owns no ag parcels
issue_work_ag_no_parcel <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("raise_crops", "num_ag_parcels"),
    where = raise_crops == 1 & num_ag_parcels == 0,
    type = 1,
    desc = "Raises crops, but no ag land",
    comment = paste0(
        "ERROR: Household raises crops, but no ag parcels owned. ",
        "In the link with the ag questionnaire, the household reports growing crops. ",
        "But there are no agricultural parcels reported in the household parcels. ",
        "Please check."
    )
)

# -----------------------------------------------------------------------------
# Plot issues
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# estimate plot area zero or missing
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_no_farmer_plot_area <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "farmer_plot_area_missing",
    where = farmer_plot_area_missing == 1,
    type = 1,
    desc = "Farmer-estimated area zero or missing",
    comment = paste0(
        "ERROR: Farmer-estimated plot area zero or missing. ",
        "Please record an appropriate answer or ask the owner."
    )
)

# comment on concerned plot(s)
issue_where_no_farmer_plot_area <- susoreview::make_issue_in_roster(
    df = plots_w_indicators, # NOTE: this comes from compile_attributes.R
    where = a3q06a %in% c(0, -999999999, haven::tagged_na("a")),
    roster_vars = c("HH_PARCELS__id", "AGRIC_PDN_PLOTS_unoma__id"),
    type = 2,
    desc = "Estimated plot area zero or missing",
    comment = "Estimated plot area zero or missing",
    issue_vars = "a3q06a"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# measured plot area zero or missing
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_no_gps_plot_area <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "gps_plot_area_missing",
    where = gps_plot_area_missing == 1,
    type = 1,
    desc = "GPS-measured area zero or missing",
    comment = paste0(
        "ERROR: GPS-measured plot area zero or missing. ",
        "Please record an appropriate answer or re-measure the area."
    )
)

# comment on concerned plot(s)
issue_where_no_gps_plot_area <- susoreview::make_issue_in_roster(
    df = plots_w_indicators,
    where = a3q06b %in% c(0, -999999999, haven::tagged_na("a")),
    roster_vars = c("HH_PARCELS__id", "AGRIC_PDN_PLOTS_unoma__id"),
    type = 2,
    desc = "GPS-measured plot area zero or missing",
    comment = "GPS-measured plot area zero or missing",
    issue_vars = "a3q06b"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# plot is cropped, but no crops reported
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_cropped_plot_no_crops <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "crop_use_plot_no_crop",
    where = crop_use_plot_no_crop == 1,
    type = 1,
    desc = "Cropped plot missing crops",
    comment = paste0(
        "ERROR: No crops reported on a plot used for growing crops. ",
        "Please confirm which information is correct: plot use or crop list."
    )
)

# comment on concerned plot(s)
issue_where_cropped_plot_no_crops <- susoreview::make_issue_in_roster(
    df = plots_w_indicators,
    where = (a3q05 == 1 | a3q05 == 2) & (has_crop == 0),
    roster_vars = c("HH_PARCELS__id", "AGRIC_PDN_PLOTS_unoma__id"),
    type = 2,
    desc = "Cropped plot missing crops",
    comment = "This plot is reported to be cropped, but has no crops listed.",
    issue_vars = "a3q05|a4q01"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# purestand plot, but has more than one crop planted
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_pure_stand_more_crops <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "purestand_plot_multiple_crops",
    where = purestand_plot_multiple_crops == 1,
    type = 1,
    desc = "Pure stand plot, multiple crops",
    comment = paste0(
        "ERROR: Plot reported as pure stand, but has multiple crops planted. ",
        "Please confirm which information is correct: crop stand or crop list."
    )
)

# comment on concerned plot(s)
issue_where_pure_stand_more_crops <- susoreview::make_issue_in_roster(
    df = plots_w_indicators,
    where = (a3q19 == 1) & (num_crops_on_plot > 1),
    roster_vars = c("HH_PARCELS__id", "AGRIC_PDN_PLOTS_unoma__id"),
    type = 2,
    desc = "Pure stand plot, multiple crops",
    comment = "This plot was reported as purestand, but has {number_crops_on_plot} crops",
    issue_vars = "a3q19|a4q01"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# mixed stand plot, but only one crop planted
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_mixed_stand_one_crop <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "mixstand_plot_multiple_crops",
    where = mixstand_plot_multiple_crops == 1,
    type = 1,
    desc = "Mixed stand plot, only one crop",
    comment = paste0(
        "ERROR: Plot reported as mixed stand, but has only one crop planted. ", 
        "Please confirm which information is correct: crop stand or crop list."
    )
)

# comment on concerned plot(s)
issue_where_mixed_stand_one_crop <- susoreview::make_issue_in_roster(
    df = plots_w_indicators,
    where = (a3q19 == 2) & (num_crops_on_plot == 1),
    roster_vars = c("HH_PARCELS__id", "AGRIC_PDN_PLOTS_unoma__id"),
    desc = "Mixed stand plot, only one crop",
    comment = "This plot was reported as being mixed stand, but only has 1 crop planted.",
    issue_vars = "a3q19|a4q01"
)


# -----------------------------------------------------------------------------
# Parcel use conflicts with activities
# -----------------------------------------------------------------------------

# land used for business, but no enterprise reported
issue_biz_land_no_biz <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("parcel_for_business", "run_business"),
    where = parcel_for_business == 1 & run_business == 0,
    type = 1,
    desc = "Business land, but no business",
    comment = paste0(
        "ERROR: Parcel reported being used for a business, but the household reports not running a business.",
        "Please confirm which information is correct: parcel use or enterprise."        
    )
)

# have ag parcel(s), but not engaged in ag according to filters
issue_ag_parcels_not_do_ag <- susoreview::create_issue(
    df_attribs = attribs, 
    vars = c("num_ag_parcels", "raise_crops","intend_raise_crops", "raise_livestock", "intend_raise_livestock"),
    where = (
        (num_ag_parcels > 0) &
		(raise_crops == 0) &
		(intend_raise_crops == 0) & 
		(raise_livestock == 0) & 
		(intend_raise_livestock == 0)
    ),
    type = 1,
    desc = "Have ag parcel(s), but not involved in ag",
    comment = paste0(
        "ERROR: Household owns at least 1 agricultural partcel, but is not engaged in any type of agriculture.",
        "Please check with the household which information is correct: parcel use or engagement in agriculture or livestock."
    )
)

# -----------------------------------------------------------------------------
# Asset ownership conflicts with parcel use
# -----------------------------------------------------------------------------

# own home, but no residential parcel reported
issue_own_home_no_home_parcel <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("owns_home", "parcel_for_home"),
    where = owns_home == 1 & parcel_for_home == 0,
    type = 1,
    desc = "Home owned, but no residential parcel reported",
    comment = paste0(
        "ERROR: Household owns their residence, but no residential parcel was reported.",
        "Please confirm which information is correct: home ownership or parcel listing."
    )
)

# owns non-ag land, but no non-ag land reported
issue_own_ag_land_but_no_ag <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("owns_non_ag_land", "parcel_for_non_ag"),
    where = owns_non_ag_land == 1 & parcel_for_non_ag == 0,
    type = 1,
    desc = "Non-ag land owned, but no non-ag land reported",
    comment = paste0(
        "ERROR: Household owns non-agricultural land, but no non-agricultural parcel reported.",
        "Please confirm which information is correct: non-ag land ownership or parcel listing."
    )
)

# -----------------------------------------------------------------------------
# Labor missing
# -----------------------------------------------------------------------------

# engaged in agriculture, but no labor inputs of any type reported
issue_ag_but_no_ag_labor <- susoreview::create_issue(
    df_attribs = attribs,
    vars = c("raise_crops", "any_non_hh_ag_labor", "any_hh_ag_labor"),
    where = raise_crops == 1 & any_non_hh_ag_labor == 0 & any_hh_ag_labor == 0,
    type = 1,
    desc = "Engaged in ag, but no labor inputs reported",
    comment = paste0(
        "ERROR: Household engaged in agricultural, but no labor inputs reported.",
        "Please ask the household again about their household and non-household labor inputs."
    )
)

# -----------------------------------------------------------------------------
# Parcel-plot inconsistencies
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# parcel cropped, but none of its plots is
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_parcel_cropped_plots_not <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "parcel_cropped_plots_not",
    where = parcel_cropped_plots_not == 1,
    type = 1,
    desc = "Parcel cropped, but none of its plots is",
    comment = paste0(
        "ERROR: At least 1 parcel is used for crops, but none of its plots is planted.",
        "Please check with the household which information is correct: parcel use or plot use."
    )
)

# comment for concerned parcels
issue_where_parcel_cropped_plots_not <- susoreview::make_issue_in_roster(
    df = parcel_plots,
    where = parcel_cropped_plots_not == 1,
    roster_vars = c("HH_PARCELS__id"),
    type = 2,
    desc = "Parcel cropped, but none of its plots is",
    comment = "This parcel is reported as being cropped, but none of its plots are.",
    issue_vars = "hp2q21_1"
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# parcel fallow, but none of its plots is
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# error
issue_parcel_fallow_plots_not <- susoreview::create_issue(
    df_attribs = attribs,
    vars = "parcel_fallow_plots_not",
    where = parcel_fallow_plots_not == 1,
    type = 1,
    desc = "Parcel fallow, but none of its plots is",
    comment = paste0(
        "ERROR: At least 1 parcel is fallow, but none of its plots is fallow.",
        "Please check with the household which information is correct: parcel use or plot use."
    )
)

# comment for concerned parcels
issue_where_parcel_fallow_plots_not <- susoreview::make_issue_in_roster(
    df = parcel_plots,
    where = parcel_fallow_plots_not == 1,
    roster_vars = c("HH_PARCELS__id"),
    type = 2,
    desc = "Parcel fallow, but none of its plots is",
    comment = "This parcel is reported as fallow, but none of its plots is.",
    issue_vars = "hp2q21_1"
)

# -----------------------------------------------------------------------------
# Woman's 24h consumption v. hhold's 7-day consumption
# -----------------------------------------------------------------------------

w_cons_24h <- c(
    "woman24h_grains",
    "woman24h_tubers",
    "woman24h_legumes",
    "woman24h_nuts",
    "woman24h_milk_prod",
    "woman24h_eggs",
    "woman24h_organ_meat",
    "woman24h_oth_meat",
    "woman24h_fish",
    "woman24h_leafy_veg",
    "woman24h_vit_a_veg",
    "woman24h_oth_veg",
    "woman24h_vit_a_fruit",
    "woman24h_oth_fruit",
    "woman24h_sweets",
    "woman24h_purch_sug_bev",
    "woman24h_fats",
    "woman24h_oils",
    "woman24h_salty_snacks",
    "woman24h_condiments"
)

w_cons_7d <- c(
    "w_conso7d_grains",
    "w_conso7d_tubers",
    "w_conso7d_legumes",
    "w_conso7d_nuts",
    "w_conso7d_milk_prod",
    "w_conso7d_egg",
    "w_conso7d_organ_meat",
    "w_conso7d_oth_meat",
    "w_conso7d_fish",
    "w_conso7d_leafy_veg",
    "w_conso7d_vit_a_veg",
    "w_conso7d_oth_veg",
    "w_conso7d_vit_a_fruit",
    "w_conso7d_oth_fruit",
    "w_conso7d_sweets",
    "w_conso7d_sugary_bev",
    "w_conso7d_fats",
    "w_conso7d_oils",
    "w_conso7d_savory_snack",
    "w_conso7d_condiment"
)

w_item_names <- c(
    "grains",
    "tubers",
    "legumes",
    "nuts",
    "milk products",
    "eggs",
    "organ meats",
    "other meats",
    "fish",
    "leafy green vegetables",
    "vitamin A-rich vegetables",
    "other vegetables",
    "vitamin A-rich fruit",
    "other fruits",
    "sweets",
    "sugary beverages",
    "fats",
    "oils",
    "savory snacks",
    "condiments"    
)


issue_w_24h_v_7d <- purrr::pmap_dfr(
    .l = list(w_cons_24h, w_cons_7d, w_item_names),
    .f = ~ susoreview::create_issue(
        df_attribs = attribs,
        vars = c(..1, ..2),
        where = !!rlang::parse_quo(
            glue::glue("{..1} == 1 & {..2} == 0"),
            rlang::global_env()
        ),
        type = 1,
        desc = glue::glue("Consumed {..3} in past 24 hours, but not 7 days"),
        comment = paste0(
            glue::glue("ERROR: a woman member of the household consumed {..3} "),
            "in the past 24 hours , but not in the past 7 days. ", 
            "Please confirm which information is correct: ", 
            "food consumption by the woman in the past 24 hours or ", 
            "by household in the past 7 days."
        )
    )
)

# -----------------------------------------------------------------------------
# Child's 24h consumption v. hhold's 7-day consumption
# -----------------------------------------------------------------------------

c_cons_24h <- c(
    "child24h_grains",
    "child24h_color_veg",
    "child24h_white_root",
    "child24h_leafy_veg",
    "child24h_oth_veg",
    "child24h_orange_fruit",
    "child24h_oth_fruit",
    "child24h_organ_meat",
    "child24h_process_meat",
    "child24h_oth_meat",
    "child24h_eggs",
    "child24h_fish",
    "child24h_legumes",
    "child24h_yoghurt",
    "child24h_cheese",
    "child24h_oils"
)

c_cons_7d <- c(
    "c_conso7d_cereal",
    "c_conso7d_vit_a_veg",
    "c_conso7d_tubers",
    "c_conso7d_leafy_veg",
    "c_conso7d_oth_veg",
    "c_conso7d_orange_fruit",
    "c_conso7d_oth_fruit",
    "c_conso7d_organ_meat",
    "c_conso7d_process_meat",
    "c_conso7d_oth_meat",
    "c_conso7d_eggs",
    "c_conso7d_fish",
    "c_conso7d_legumes",
    "c_conso7d_yoghurt",
    "c_conso7d_cheese",
    "c_conso7d_oils"
)

c_item_names <- c(
    "cereals",
    "colored vegetables",
    "white tubers",
    "leafy green vegetables",
    "other vegetables",
    "orange-flesh fruits",
    "other fruits",
    "organ meats",
    "processed meats",
    "other meats",
    "eggs",
    "fish",
    "legumes",
    "yoghurt",
    "cheese",
    "oils"
)


issue_c_24h_v_7d <- purrr::pmap_dfr(
    .l = list(c_cons_24h, c_cons_7d, c_item_names),
    .f = ~ susoreview::create_issue(
        df_attribs = attribs,
        vars = c(..1, ..2),
        where = !!rlang::parse_quo(
            glue::glue("{..1} == 1 & {..2} == 0"),
            rlang::global_env()
        ),
        type = 1,
        desc = glue::glue("Consumed {..3} in past 24 hours, but not 7 days"),
        comment = paste0(
            glue::glue("ERROR: a child member of the household consumed {..3} "),
            "in the past 24 hours , but not in the past 7 days. ", 
            "Please confirm which information is correct: ", 
            "food consumption by the children in the past 24 hours or ", 
            "by household in the past 7 days."
        )
    )
)

# =============================================================================
# Combine all issues
# =============================================================================

# combine all issues
issues <- dplyr::bind_rows(mget(ls(pattern = "^issue_")))

# remove intermediary objects to lighten load on memory
rm(list = ls(pattern = "^issue_"))

# =============================================================================
# Add issues from interview metadata
# =============================================================================

# -----------------------------------------------------------------------------
# ... if questions left unanswered
# -----------------------------------------------------------------------------

# get interview statistics
# creates data frame with interview stats
interviews <- cases_to_review$interview__id
interview_stats <- purrr::map_dfr(
        .x = interviews,
        .f = ~ susoapi::get_interview_stats(interview_id = .x)
    ) %>%
    dplyr::rename(interview__id = InterviewId, interview__key = InterviewKey)

# prepare number of legit missing file
# TODO: see if any legit unanswered
# num_legit_miss <- num_legit_miss %>%
#     rename(n_legit_miss = numLegitMiss) %>%
#     select(interview__id, interview__key, n_legit_miss)

# add error if interview completed, but questions left unanswered
# returns issues data supplemented with unanswered question issues
issues_plus_unanswered <- susoreview::add_issue_if_unanswered(
    df_cases_to_review = cases_to_review,
    df_interview_stats = interview_stats,
    df_issues = issues,
    n_unanswered_ok = 0
    # ,
    # df_legit_miss = num_legit_miss
)

# -----------------------------------------------------------------------------
# ... if any SuSo errors
# -----------------------------------------------------------------------------

# add issue if there are SuSo errors
issues_plus_miss_and_suso <- susoreview::add_issues_for_suso_errors(
    df_cases_to_review = cases_to_review,
    df_errors = suso_errors,
    issue_type = 3,
    df_issues = issues_plus_unanswered
)
