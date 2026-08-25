# ============================================================
# EXPERIMENT 2 — DESCRIPTIVE RESULTS
# Evidence–verdict alignment
# ============================================================

library(dplyr)
library(tidyr)

# Run cleaning script
source(
  "2. Evidence-verdict_alignment/scripts/01_cleaning.R"
)

# ------------------------------------------------------------
# 1. Evidence supports final verdict
# ------------------------------------------------------------

evidence_summary <- df %>%
  count(
    lg,
    evidences_support_final_answers_yes_partially_no
  ) %>%
  group_by(lg) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

evidence_summary


# Overall distribution
evidence_overall <- df %>%
  count(
    evidences_support_final_answers_yes_partially_no
  ) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

evidence_overall


# ------------------------------------------------------------
# 2. Reliance on RAG
# ------------------------------------------------------------

rag_summary <- df %>%
  count(
    lg,
    relies_on_rag_yes_mixed_no_unclear
  ) %>%
  group_by(lg) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

rag_summary


rag_overall <- df %>%
  count(
    relies_on_rag_yes_mixed_no_unclear
  ) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

rag_overall


# ------------------------------------------------------------
# 3. Uncertainty in the claim
# ------------------------------------------------------------

claim_uncertainty <- df %>%
  count(
    lg,
    flags_uncertainty_in_claim_yes_no
  ) %>%
  group_by(lg) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

claim_uncertainty


# ------------------------------------------------------------
# 4. Uncertainty in retrieved/search evidence
# ------------------------------------------------------------

search_uncertainty <- df %>%
  count(
    lg,
    flags_uncertainty_in_the_search_results_yes_no
  ) %>%
  group_by(lg) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

search_uncertainty