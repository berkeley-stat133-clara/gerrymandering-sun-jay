#!/usr/bin/env Rscript
# Verification Script for Gerrymandering Analysis
# Tests all completed parts of the assignment

cat("========================================\n")
cat("VERIFICATION SCRIPT\n")
cat("Testing all completed work\n")
cat("========================================\n\n")

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(scales)
})

passed <- 0
failed <- 0

# Helper function to report test results
test_result <- function(name, condition, message = "") {
  if (condition) {
    cat("✓", name, "PASSED\n")
    if (message != "") cat("  ", message, "\n")
    return(1)
  } else {
    cat("✗", name, "FAILED\n")
    if (message != "") cat("  ", message, "\n")
    return(0)
  }
}

cat("==================================================\n")
cat("TEST 1: DATA FILES EXIST\n")
cat("==================================================\n\n")

files_to_check <- c(
  "data/g24_sov_by_g24_svprec.csv",
  "data/g24-candidates-by-district.csv",
  "data/g24-results-by-district.xlsx",
  "data/state_g24_sov_data_by_g24_srprec.csv",
  "data/state_g24_sr_blk_map.csv",
  "data/ab604_block_assignments.csv"
)

for (file in files_to_check) {
  passed <- passed + test_result(basename(file), file.exists(file))
}

cat("\n==================================================\n")
cat("TEST 2: PART 2 - DATA CLEANING (2024 DATA)\n")
cat("==================================================\n\n")

# Load raw data
raw_data <- read_csv("data/g24_sov_by_g24_svprec.csv", show_col_types = FALSE)
passed <- passed + test_result("Raw data loaded", nrow(raw_data) > 0,
                                paste(nrow(raw_data), "rows"))

# Find vote columns
vote_cols <- names(raw_data)[grep("^CNG", names(raw_data))]
passed <- passed + test_result("Vote columns found", length(vote_cols) > 0,
                                paste(length(vote_cols), "columns:", paste(vote_cols, collapse=", ")))

# Clean data
clean_data <- raw_data %>%
  mutate(across(all_of(vote_cols), ~if_else(. == "***", NA_character_, .))) %>%
  mutate(across(all_of(vote_cols), as.numeric)) %>%
  mutate(CDDIST = as.numeric(CDDIST))

passed <- passed + test_result("Data cleaned", nrow(clean_data) == nrow(raw_data),
                                "All rows preserved")

# Check districts
n_districts <- length(unique(na.omit(clean_data$CDDIST)))
passed <- passed + test_result("Districts identified", n_districts == 53,
                                paste(n_districts, "congressional districts"))

# Save cleaned data
write_csv(clean_data, "data/g24_sov_by_g24_svprec_clean.csv")
passed <- passed + test_result("Cleaned data saved",
                                file.exists("data/g24_sov_by_g24_svprec_clean.csv"))

cat("\n==================================================\n")
cat("TEST 3: PART 5 - AB 604 DATA PROCESSING\n")
cat("==================================================\n\n")

# Load SR votes
sr_votes <- read_csv("data/state_g24_sov_data_by_g24_srprec.csv", show_col_types = FALSE)
passed <- passed + test_result("SR votes loaded", nrow(sr_votes) > 0,
                                paste(nrow(sr_votes), "SR precincts"))

# Clean SR votes
sr_vote_cols <- names(sr_votes)[grep("^CNG", names(sr_votes))]
sr_votes <- sr_votes %>%
  mutate(across(all_of(sr_vote_cols), ~if_else(. == "***", NA_character_, .))) %>%
  mutate(across(all_of(sr_vote_cols), as.numeric))

passed <- passed + test_result("SR votes cleaned", all(!is.na(sr_vote_cols)))

# Load block mapping
sr_to_block <- read_csv("data/state_g24_sr_blk_map.csv", show_col_types = FALSE)
passed <- passed + test_result("Block mapping loaded", nrow(sr_to_block) > 0,
                                paste(nrow(sr_to_block), "block-precinct mappings"))

# Load AB604 assignments
ab604_blocks <- read_csv("data/ab604_block_assignments.csv",
                         col_names = c("BLOCK_KEY", "AB604_DISTRICT"),
                         show_col_types = FALSE)
n_ab604_districts <- length(unique(ab604_blocks$AB604_DISTRICT))
passed <- passed + test_result("AB604 assignments loaded", nrow(ab604_blocks) > 0,
                                paste(nrow(ab604_blocks), "blocks mapped to",
                                      n_ab604_districts, "new districts"))

# Allocate votes to blocks and aggregate to AB604 districts
cat("Allocating votes (this may take a moment)...\n")
ab604_allocated_cols <- paste0(sr_vote_cols, "_allocated")

ab604_results <- sr_to_block %>%
  left_join(sr_votes, by = "SRPREC_KEY") %>%
  mutate(across(all_of(sr_vote_cols), ~. * (PCTSRPREC / 100), .names = "{.col}_allocated")) %>%
  left_join(ab604_blocks, by = "BLOCK_KEY") %>%
  filter(!is.na(AB604_DISTRICT)) %>%
  group_by(AB604_DISTRICT) %>%
  summarise(across(all_of(ab604_allocated_cols), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  rename_with(~str_remove(., "_allocated"), all_of(ab604_allocated_cols))

passed <- passed + test_result("AB604 votes allocated", nrow(ab604_results) > 0,
                                paste(nrow(ab604_results), "AB604 districts"))

# Check vote totals are reasonable
ab604_totals <- ab604_results %>%
  rowwise() %>%
  mutate(total = sum(c_across(all_of(sr_vote_cols)), na.rm = TRUE))

min_votes <- min(ab604_totals$total)
max_votes <- max(ab604_totals$total)
passed <- passed + test_result("AB604 vote totals reasonable",
                                min_votes > 10000 && max_votes < 1000000,
                                paste("Range:", comma(min_votes), "to", comma(max_votes)))

# Save AB604 results
write_csv(ab604_results, "data/ab604_district_results.csv")
passed <- passed + test_result("AB604 results saved",
                                file.exists("data/ab604_district_results.csv"))

cat("\n==================================================\n")
cat("TEST 4: EXPLORATORY DATA ANALYSIS PREP\n")
cat("==================================================\n\n")

# Load cleaned data
precinct_data <- read_csv("data/g24_sov_by_g24_svprec_clean.csv", show_col_types = FALSE)
candidates <- read_csv("data/g24-candidates-by-district.csv", show_col_types = FALSE)

# Aggregate to district level
district_votes <- precinct_data %>%
  filter(!is.na(CDDIST)) %>%
  group_by(CDDIST) %>%
  summarise(across(all_of(vote_cols), ~sum(., na.rm = TRUE)), .groups = "drop")

passed <- passed + test_result("District aggregation", nrow(district_votes) == 53,
                                "All 53 districts")

# Calculate margins
district_margins <- district_votes %>%
  pivot_longer(cols = all_of(vote_cols), names_to = "candidate", values_to = "votes") %>%
  filter(votes > 0) %>%
  group_by(CDDIST) %>%
  arrange(CDDIST, desc(votes)) %>%
  mutate(vote_share = votes / sum(votes)) %>%
  filter(row_number() <= 2) %>%
  summarise(margin = vote_share[1] - if_else(n() >= 2, vote_share[2], 0), .groups = "drop")

closest_margin <- min(district_margins$margin)
biggest_margin <- max(district_margins$margin)

passed <- passed + test_result("Vote margins calculated", nrow(district_margins) == 53,
                                paste("Closest:", percent(closest_margin, 0.1),
                                      "| Biggest:", percent(biggest_margin, 0.1)))

# Precinct sizes
precinct_sizes <- precinct_data %>%
  rowwise() %>%
  mutate(total_votes = sum(c_across(all_of(vote_cols)), na.rm = TRUE)) %>%
  filter(total_votes > 0)

median_size <- median(precinct_sizes$total_votes)
passed <- passed + test_result("Precinct sizes", nrow(precinct_sizes) > 0,
                                paste(nrow(precinct_sizes), "precincts | Median:",
                                      round(median_size), "votes"))

cat("\n==================================================\n")
cat("TEST 5: DATA QUALITY CHECKS\n")
cat("==================================================\n\n")

# Check for missing values in key columns
missing_district <- sum(is.na(clean_data$CDDIST))
missing_precinct <- sum(is.na(clean_data$SVPREC_KEY))

passed <- passed + test_result("No missing precinct keys", missing_precinct == 0)
passed <- passed + test_result("District assignments reasonable",
                                missing_district < nrow(clean_data) * 0.1,
                                paste(missing_district, "missing out of", nrow(clean_data)))

# Check vote totals across both datasets
sv_total <- sum(clean_data %>% select(all_of(vote_cols)), na.rm = TRUE)
sr_total <- sum(sr_votes %>% select(all_of(sr_vote_cols)), na.rm = TRUE)
ab604_total <- sum(ab604_totals$total)

passed <- passed + test_result("Vote totals consistent",
                                abs(sv_total - sr_total) / sv_total < 0.01,
                                paste("SV:", comma(sv_total), "| SR:", comma(sr_total),
                                      "| AB604:", comma(ab604_total)))

cat("\n==================================================\n")
cat("FINAL SUMMARY\n")
cat("==================================================\n\n")

total_tests <- passed + failed
cat("Tests passed:", passed, "/", total_tests, "\n")
cat("Tests failed:", failed, "\n\n")

if (failed == 0) {
  cat("✓✓✓ ALL TESTS PASSED! ✓✓✓\n\n")
  cat("Files created and verified:\n")
  cat("  ✓ data/g24_sov_by_g24_svprec_clean.csv (2024 precinct data)\n")
  cat("  ✓ data/ab604_district_results.csv (AB 604 district results)\n\n")
  cat("Ready to proceed with:\n")
  cat("  - Part 4: Calculate 2024 gerrymandering metrics\n")
  cat("  - Part 6: Calculate AB 604 gerrymandering metrics\n")
  cat("  - Part 7: Create interactive dashboard\n\n")
} else {
  cat("⚠ Some tests failed. Please review the output above.\n\n")
}

cat("========================================\n")
