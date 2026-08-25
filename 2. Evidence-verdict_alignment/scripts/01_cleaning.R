# 1. CLEANING
library(readxl)
library(dplyr)
library(janitor)

# Import
df <- read_excel("2. Evidence-verdict_alignment/data/veracity_exp2_raw.xlsx",
                 sheet = "completedataset") %>%
  clean_names()

# Keep only useful variables
df <- df %>%
  select(
    lg,
    claim,
    politifact_verdict,
    veracity_score,
    evidences_support_final_answers_yes_partially_no,
    relies_on_rag_yes_mixed_no_unclear,
    flags_uncertainty_in_claim_yes_no,
    flags_uncertainty_in_the_search_results_yes_no,
    english_french_score_consistency_yes_partially_no
  )
