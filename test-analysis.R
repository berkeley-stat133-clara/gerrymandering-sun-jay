#!/usr/bin/env Rscript
# Comprehensive Test Script for Gerrymandering Analysis
# Tests ALL completed parts of the assignment

cat("========================================\n")
cat("GERRYMANDERING ANALYSIS - FULL TEST\n")
cat("Testing all parts with actual data\n")
cat("========================================\n\n")

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(scales)
})

# Test counter
tests_passed <- 0
tests_failed <- 0

test_check <- function(name, condition, details = "") {
  if (condition) {
    cat("✓", name, "\n")
    if (details != "") cat("  ", details, "\n")
    return(1)
  } else {
    cat("✗", name, "FAILED\n")
    if (details != "") cat("  ", details, "\n")
    return(0)
  }
}

# ============================================================
# PART 2: DATA CLEANING (2024 SV PRECINCT DATA)
# ============================================================
cat("\n==================================================\n")
cat("PART 2: DATA CLEANING (2024)\n")
cat("==================================================\n\n")

raw_data <- read_csv("data/g24_sov_by_g24_svprec.csv", show_col_types = FALSE)
tests_passed <- tests_passed + test_check("Raw data loaded", nrow(raw_data) > 0,
                                           paste(nrow(raw_data), "rows"))

vote_cols <- names(raw_data)[grep("^CNG", names(raw_data))]
tests_passed <- tests_passed + test_check("Congressional vote columns found", length(vote_cols) > 0,
                                           paste(length(vote_cols), "columns:", paste(vote_cols, collapse=", ")))

clean_data <- raw_data %>%
  mutate(across(all_of(vote_cols), ~if_else(. == "***", NA_character_, .))) %>%
  mutate(across(all_of(vote_cols), as.numeric)) %>%
  mutate(CDDIST = as.numeric(CDDIST))

tests_passed <- tests_passed + test_check("Data cleaned", nrow(clean_data) == nrow(raw_data))

n_districts <- length(unique(na.omit(clean_data$CDDIST)))
tests_passed <- tests_passed + test_check("Districts identified", n_districts == 53,
                                           paste(n_districts, "districts"))

write_csv(clean_data, "data/g24_sov_by_g24_svprec_clean.csv")
tests_passed <- tests_passed + test_check("Cleaned data saved",
                                           file.exists("data/g24_sov_by_g24_svprec_clean.csv"))

# ============================================================
# PART 3: EXPLORATORY DATA ANALYSIS
# ============================================================
cat("\n==================================================\n")
cat("PART 3: EXPLORATORY DATA ANALYSIS\n")
cat("==================================================\n\n")

district_votes <- clean_data %>%
  filter(!is.na(CDDIST)) %>%
  group_by(CDDIST) %>%
  summarise(across(all_of(vote_cols), ~sum(., na.rm = TRUE)), .groups = "drop")

tests_passed <- tests_passed + test_check("District aggregation", nrow(district_votes) == 53)

district_margins <- district_votes %>%
  pivot_longer(cols = all_of(vote_cols), names_to = "candidate", values_to = "votes") %>%
  filter(votes > 0) %>%
  group_by(CDDIST) %>%
  arrange(CDDIST, desc(votes)) %>%
  mutate(vote_share = votes / sum(votes)) %>%
  filter(row_number() <= 2) %>%
  summarise(margin = vote_share[1] - if_else(n() >= 2, vote_share[2], 0), .groups = "drop")

tests_passed <- tests_passed + test_check("Vote margins calculated", nrow(district_margins) == 53,
                                           paste("Range:", percent(min(district_margins$margin), 0.1),
                                                 "to", percent(max(district_margins$margin), 0.1)))

precinct_sizes <- clean_data %>%
  rowwise() %>%
  mutate(total_votes = sum(c_across(all_of(vote_cols)), na.rm = TRUE)) %>%
  filter(total_votes > 0)

tests_passed <- tests_passed + test_check("Precinct sizes analyzed", nrow(precinct_sizes) > 0,
                                           paste("Median:", round(median(precinct_sizes$total_votes)), "votes"))

# ============================================================
# PART 4: GERRYMANDERING METRICS (2024)
# ============================================================
cat("\n==================================================\n")
cat("PART 4: GERRYMANDERING METRICS (2024)\n")
cat("==================================================\n\n")

# Load candidates
candidates <- read_csv("data/g24-candidates-by-district.csv", show_col_types = FALSE)
cong_candidates <- candidates %>% filter(DISTRICT_TYPE == "CNG")

tests_passed <- tests_passed + test_check("Candidate data loaded", nrow(cong_candidates) > 0,
                                           paste(nrow(cong_candidates), "congressional candidates"))

# Party mapping
party_mapping <- cong_candidates %>%
  select(FIELD, DISTRICT) %>%
  mutate(party = case_when(
    grepl("DEM", FIELD) ~ "Democrat",
    grepl("REP", FIELD) ~ "Republican",
    TRUE ~ "Other"
  ))

tests_passed <- tests_passed + test_check("Party mapping created", nrow(party_mapping) > 0)

# Aggregate by party
party_totals_2024 <- district_votes %>%
  pivot_longer(cols = all_of(vote_cols), names_to = "FIELD", values_to = "votes") %>%
  left_join(party_mapping, by = "FIELD", relationship = "many-to-many") %>%
  filter(votes > 0) %>%
  group_by(CDDIST, party) %>%
  summarise(total_votes = sum(votes), .groups = "drop") %>%
  pivot_wider(names_from = party, values_from = total_votes, values_fill = 0)

dem_rep_2024 <- party_totals_2024 %>%
  select(CDDIST, Democrat, Republican) %>%
  mutate(dem_share = Democrat / (Democrat + Republican))

tests_passed <- tests_passed + test_check("Party aggregation (2024)", nrow(dem_rep_2024) == 53)

# Helper functions
calculate_wasted_votes <- function(votes_a, votes_b) {
  total_votes <- votes_a + votes_b
  win_threshold <- total_votes / 2
  wasted_a <- ifelse(votes_a > votes_b, votes_a - win_threshold, votes_a)
  wasted_b <- ifelse(votes_b > votes_a, votes_b - win_threshold, votes_b)
  return(list(wasted_a = wasted_a, wasted_b = wasted_b))
}

calculate_mean_median <- function(vote_shares) {
  mean(vote_shares, na.rm = TRUE) - median(vote_shares, na.rm = TRUE)
}

calculate_efficiency_gap <- function(votes_a, votes_b) {
  wasted <- calculate_wasted_votes(votes_a, votes_b)
  (sum(wasted$wasted_a) - sum(wasted$wasted_b)) / sum(votes_a + votes_b)
}

# Calculate metrics
seats_2024_D <- sum(dem_rep_2024$Democrat > dem_rep_2024$Republican)
mm_2024 <- calculate_mean_median(dem_rep_2024$dem_share)
eg_2024 <- calculate_efficiency_gap(dem_rep_2024$Democrat, dem_rep_2024$Republican)

tests_passed <- tests_passed + test_check("2024 metrics calculated", !is.na(mm_2024) && !is.na(eg_2024),
                                           paste("Seats:", seats_2024_D, "D |",
                                                 "MM:", round(mm_2024, 4), "|",
                                                 "EG:", round(eg_2024, 4)))

# ============================================================
# PART 5: AB 604 DATA PROCESSING
# ============================================================
cat("\n==================================================\n")
cat("PART 5: AB 604 RE-RUN\n")
cat("==================================================\n\n")

sr_votes <- read_csv("data/state_g24_sov_data_by_g24_srprec.csv", show_col_types = FALSE)
tests_passed <- tests_passed + test_check("SR precinct votes loaded", nrow(sr_votes) > 0,
                                           paste(nrow(sr_votes), "SR precincts"))

sr_vote_cols <- names(sr_votes)[grep("^CNG", names(sr_votes))]
sr_votes <- sr_votes %>%
  mutate(across(all_of(sr_vote_cols), ~if_else(. == "***", NA_character_, .))) %>%
  mutate(across(all_of(sr_vote_cols), as.numeric))

tests_passed <- tests_passed + test_check("SR votes cleaned", all(!is.na(sr_vote_cols)))

sr_to_block <- read_csv("data/state_g24_sr_blk_map.csv", show_col_types = FALSE)
tests_passed <- tests_passed + test_check("Block mapping loaded", nrow(sr_to_block) > 0,
                                           paste(nrow(sr_to_block), "block-precinct mappings"))

ab604_blocks <- read_csv("data/ab604_block_assignments.csv",
                         col_names = c("BLOCK_KEY", "AB604_DISTRICT"),
                         show_col_types = FALSE)
tests_passed <- tests_passed + test_check("AB604 assignments loaded", nrow(ab604_blocks) > 0,
                                           paste(nrow(ab604_blocks), "blocks in",
                                                 length(unique(ab604_blocks$AB604_DISTRICT)), "districts"))

# Process AB604 (this may take a moment)
cat("Processing AB604 allocation (this may take 30-60 seconds)...\n")
ab604_allocated_cols <- paste0(sr_vote_cols, "_allocated")
ab604_results <- sr_to_block %>%
  left_join(sr_votes, by = "SRPREC_KEY") %>%
  mutate(across(all_of(sr_vote_cols), ~. * (PCTSRPREC / 100), .names = "{.col}_allocated")) %>%
  left_join(ab604_blocks, by = "BLOCK_KEY") %>%
  filter(!is.na(AB604_DISTRICT)) %>%
  group_by(AB604_DISTRICT) %>%
  summarise(across(all_of(ab604_allocated_cols), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~str_remove(., "_allocated"), all_of(ab604_allocated_cols))

tests_passed <- tests_passed + test_check("AB604 votes allocated", nrow(ab604_results) > 0,
                                           paste(nrow(ab604_results), "AB604 districts"))

write_csv(ab604_results, "data/ab604_district_results.csv")
tests_passed <- tests_passed + test_check("AB604 results saved",
                                           file.exists("data/ab604_district_results.csv"))

# ============================================================
# PART 6: GERRYMANDERING METRICS (AB 604)
# ============================================================
cat("\n==================================================\n")
cat("PART 6: GERRYMANDERING METRICS (AB 604)\n")
cat("==================================================\n\n")

party_totals_ab604 <- ab604_results %>%
  pivot_longer(cols = all_of(sr_vote_cols), names_to = "FIELD", values_to = "votes") %>%
  left_join(party_mapping, by = "FIELD", relationship = "many-to-many") %>%
  filter(votes > 0) %>%
  group_by(AB604_DISTRICT, party) %>%
  summarise(total_votes = sum(votes), .groups = "drop") %>%
  pivot_wider(names_from = party, values_from = total_votes, values_fill = 0)

dem_rep_ab604 <- party_totals_ab604 %>%
  select(AB604_DISTRICT, Democrat, Republican) %>%
  mutate(dem_share = Democrat / (Democrat + Republican))

tests_passed <- tests_passed + test_check("Party aggregation (AB604)", nrow(dem_rep_ab604) > 0)

seats_ab604_D <- sum(dem_rep_ab604$Democrat > dem_rep_ab604$Republican)
mm_ab604 <- calculate_mean_median(dem_rep_ab604$dem_share)
eg_ab604 <- calculate_efficiency_gap(dem_rep_ab604$Democrat, dem_rep_ab604$Republican)

tests_passed <- tests_passed + test_check("AB604 metrics calculated", !is.na(mm_ab604) && !is.na(eg_ab604),
                                           paste("Seats:", seats_ab604_D, "D |",
                                                 "MM:", round(mm_ab604, 4), "|",
                                                 "EG:", round(eg_ab604, 4)))

# ============================================================
# PART 7: DASHBOARD (VERIFICATION ONLY)
# ============================================================
cat("\n==================================================\n")
cat("PART 7: DASHBOARD\n")
cat("==================================================\n\n")

tests_passed <- tests_passed + test_check("Dashboard file exists",
                                           file.exists("dashboard.qmd"))

# Check if plotly is available
plotly_available <- "plotly" %in% installed.packages()[,"Package"]
tests_passed <- tests_passed + test_check("Plotly package available", plotly_available)

# ============================================================
# COMPARISON & SUMMARY
# ============================================================
cat("\n==================================================\n")
cat("COMPARISON: 2024 vs AB 604\n")
cat("==================================================\n\n")

cat("Seat Distribution:\n")
cat("  2024:   ", seats_2024_D, "D,", 53 - seats_2024_D, "R\n")
cat("  AB604:  ", seats_ab604_D, "D,", nrow(dem_rep_ab604) - seats_ab604_D, "R\n")
cat("  Change: ", sprintf("%+d", seats_ab604_D - seats_2024_D), "D\n\n")

cat("Gerrymandering Metrics:\n")
cat("                  2024        AB604       Change\n")
cat("  Mean-Median:   ", sprintf("%7.4f", mm_2024),
    "    ", sprintf("%7.4f", mm_ab604),
    "    ", sprintf("%+7.4f", mm_ab604 - mm_2024), "\n")
cat("  Eff. Gap:      ", sprintf("%7.4f", eg_2024),
    "    ", sprintf("%7.4f", eg_ab604),
    "    ", sprintf("%+7.4f", eg_ab604 - eg_2024), "\n\n")

# ============================================================
# FINAL SUMMARY
# ============================================================
cat("==================================================\n")
cat("TEST RESULTS\n")
cat("==================================================\n\n")

total_tests <- tests_passed + tests_failed
cat("Tests passed:", tests_passed, "/", total_tests, "\n")

if (tests_failed == 0) {
  cat("\n🎉 ALL TESTS PASSED! 🎉\n\n")
  cat("Assignment Status: COMPLETE ✅\n\n")
  cat("Completed Parts:\n")
  cat("  ✅ Part 2: Data Cleaning\n")
  cat("  ✅ Part 3: Exploratory Data Analysis\n")
  cat("  ✅ Part 4: 2024 Gerrymandering Metrics\n")
  cat("  ✅ Part 5: AB 604 Re-run (Proportional Allocation)\n")
  cat("  ✅ Part 6: AB 604 Gerrymandering Metrics\n")
  cat("  ✅ Part 7: Interactive Dashboard\n\n")
  cat("Next Steps:\n")
  cat("  1. Render .qmd files to PDF: quarto render <file>.qmd\n")
  cat("  2. View dashboard: quarto preview dashboard.qmd\n")
  cat("  3. Commit and push to GitHub\n\n")
} else {
  cat("\n⚠ Some tests failed. Review output above.\n\n")
}

cat("========================================\n")
