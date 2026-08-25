#2. PIVOT RESULTS

pivot_evidence <- df %>%
  count(
    lg,
    evidences_support_final_answers_yes_partially_no
  ) %>%
  tidyr::pivot_wider(
    names_from = lg,
    values_from = n,
    values_fill = 0
  )

pivot_evidence

# Convert in percentages 
pivot_pct <- df %>%
  group_by(lg) %>%
  count(evidences_support_final_answers_yes_partially_no) %>%
  mutate(percent = round(100 * n / sum(n), 1))

pivot_pct

# Cross-tab for any variable
df %>%
  count(lg, relies_on_rag_yes_mixed_no_unclear) %>%
  tidyr::pivot_wider(
    names_from = lg,
    values_from = n,
    values_fill = 0
  )

df %>%
  count(lg, flags_uncertainty_in_claim_yes_no) %>%
  tidyr::pivot_wider(
    names_from = lg,
    values_from = n,
    values_fill = 0
  )

# MEAN

df %>%
  group_by(lg) %>%
  summarise(
    mean_score = mean(veracity_score, na.rm = TRUE),
    sd = sd(veracity_score, na.rm = TRUE),
    n = n()
  )
