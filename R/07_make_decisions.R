# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "comments",
    "issues", # TODO: change to `issues_plus_miss_and_suso` once API user available
    "cases_to_review"
    # TODO: add back once API user available: , "interview_stats"
)

check_exists(objects_needed)

# =============================================================================
# Make decisions
# =============================================================================

# check for comments
# returns a data frame of cases that contain comments
interviews_with_comments <- susoreview::check_for_comments(
    df_comments = comments, 
    df_issues = issues, # TODO: change to `issues_plus_miss_and_suso` once API user available
    df_cases_to_review = cases_to_review
)

# decide what action to take 
# decisions <- susoreview::decide_action(
#     df_cases_to_review = cases_to_review,
#     df_issues = issues, # TODO: change to `issues_plus_miss_and_suso` once API user available
#     issue_types_to_reject = issues_to_reject,
#     df_has_comments = interviews_with_comments,
#     df_interview_stats = interview_stats
# )

    # has at least 1 major issue
    interviews_have_issues <- issues %>%
        dplyr::filter(.data$issue_type %in% issues_to_reject) %>%
        dplyr::distinct(.data$interview__id, .data$interview__key) %>%
        dplyr::left_join(cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key)

    # requires review
    interviews_need_review <- issues %>%
        dplyr::filter(.data$issue_type == 4) %>%
        dplyr::distinct(.data$interview__id, .data$interview__key) %>%
        dplyr::left_join(cases_to_review, by = c("interview__id", "interview__key"))	%>%
        dplyr::select(.data$interview__id, .data$interview__key)

    to_reject <- interviews_have_issues %>%
        # has at least 1 major issue, but no comments
        dplyr::anti_join(interviews_with_comments, by = c("interview__id", "interview__key")) %>%
        dplyr::inner_join(cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key, .data$interview__status)

    to_review <- interviews_have_issues %>%
        dplyr::select(.data$interview__id, .data$interview__key) %>%
        # has 1+ critical issue and commments on at least 1 critical issue
        dplyr::inner_join(interviews_with_comments, by = c("interview__id", "interview__key")) %>%
        # and/or has an issue that requires review
        dplyr::full_join(interviews_need_review, by = c("interview__id", "interview__key")) %>%
        # but is not on the rejection list
        dplyr::anti_join(to_reject, by = c("interview__id", "interview__key")) %>%
        dplyr::inner_join(cases_to_review, by = c("interview__id", "interview__key")) %>%
        dplyr::select(.data$interview__id, .data$interview__key, .data$interview__status)


# add rejection messages
# to_reject <- decisions[["to_reject"]]

to_reject <- susoreview::add_rejection_msgs(
    df_to_reject = to_reject,
    df_issues = issues # TODO: change to `issues_plus_miss_and_suso` once API user available
)

# flag persistent issues
revised_decisions <- susoreview::flag_persistent_issues(
    df_comments = comments,
    df_to_reject = to_reject
)

# =============================================================================
# Extract decisions into data representing them
# =============================================================================

# -----------------------------------------------------------------------------
# To reject
# -----------------------------------------------------------------------------

to_reject_ids <- revised_decisions[["to_reject"]] %>%
    dplyr::select(interview__id) %>%
    dplyr::left_join(cases_to_review, by = "interview__id") %>%
    dplyr::select(interview__id, interview__key, PHHID, XHHID)

to_reject_issues <- to_reject_ids %>%
    # issues_plus_miss_and_suso
    dplyr::left_join(issues, by = c("interview__id", "interview__key")) %>%
    dplyr::filter(issue_type %in% c(issues_to_reject, 2)) %>%
    dplyr::select(
        interview__id, interview__key, PHHID, XHHID,
        dplyr::starts_with("issue_")
    )

to_reject_api <- revised_decisions[["to_reject"]]

# -----------------------------------------------------------------------------
# To review
# -----------------------------------------------------------------------------

to_review_ids <- to_review %>% # decisions[["to_review"]]
    dplyr::select(interview__id) %>%
    dplyr::left_join(cases_to_review, by = "interview__id") %>%
    dplyr::select(interview__id, interview__key, PHHID, XHHID)

to_review_issues <- to_review_ids %>%
    dplyr::left_join(
        issues,
        # issues_plus_miss_and_suso, 
        by = c("interview__id", "interview__key")
    )

to_review_api <- susoreview::add_rejection_msgs(
    df_to_reject = to_review, # decisions[["to_review"]],
    df_issues = issues # issues_plus_miss_and_suso
)

# -----------------------------------------------------------------------------
# To follow up
# -----------------------------------------------------------------------------

to_follow_up_ids <- revised_decisions[["to_follow_up"]] %>%
    dplyr::left_join(cases_to_review, by = "interview__id") %>%
    dplyr::select(interview__id, interview__key, PHHID, XHHID)

to_follow_up_issues <- revised_decisions[["to_follow_up"]] %>%
    # issues_plus_miss_and_suso
    dplyr::left_join(issues, by = "interview__id") %>%
    dplyr::left_join(
        cases_to_review, 
        by = c("interview__id", "interview__key")
    ) %>%
    dplyr::select(
        interview__id, interview__key, interview__status,
        dplyr::starts_with("issue_")
    )

to_follow_up_api <- revised_decisions[["to_follow_up"]]
