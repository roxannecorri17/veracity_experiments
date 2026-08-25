# ============================================================
# Experiment 1.3 - Truth-aligned confidence analysis
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Import datasets
# ------------------------------------------------------------

dataset_false <- read_csv("1. Cross-language performance and retrieval quality/data/veracity_1.3 - false_cvs.csv")
dataset_true  <- read_csv("1. Cross-language performance and retrieval quality/data/veracity_1.3 - true_cvs.csv")


# ------------------------------------------------------------
# 2. Create truth-aligned confidence score
# ------------------------------------------------------------
# Scale:
# 100 = score is exactly at the correct endpoint
# 0   = score is exactly at the wrong endpoint
#
# TRUE claims: ideal = 100
# FALSE claims: ideal = 0

dataset_true <- dataset_true %>%
  mutate(
    truth_aligned_confidence = score
  )

dataset_false <- dataset_false %>%
  mutate(
    truth_aligned_confidence = 100 - score
  )


# ------------------------------------------------------------
# 3. Summary by dataset and language
# ------------------------------------------------------------

confidence_true <- dataset_true %>%
  group_by(LG) %>%
  summarise(
    n = sum(!is.na(truth_aligned_confidence)),
    mean_confidence = mean(truth_aligned_confidence, na.rm = TRUE),
    median_confidence = median(truth_aligned_confidence, na.rm = TRUE),
    sd_confidence = sd(truth_aligned_confidence, na.rm = TRUE)
  )

confidence_false <- dataset_false %>%
  group_by(LG) %>%
  summarise(
    n = sum(!is.na(truth_aligned_confidence)),
    mean_confidence = mean(truth_aligned_confidence, na.rm = TRUE),
    median_confidence = median(truth_aligned_confidence, na.rm = TRUE),
    sd_confidence = sd(truth_aligned_confidence, na.rm = TRUE)
  )

confidence_true
confidence_false


# ------------------------------------------------------------
# 4. Combine datasets for overall comparison
# ------------------------------------------------------------

confidence_all <- bind_rows(
  
  dataset_true %>%
    mutate(dataset = "True claims"),
  
  dataset_false %>%
    mutate(dataset = "False claims")
  
) %>%
  mutate(
    Language = recode(
      LG,
      "E" = "English",
      "F" = "French"
    )
  )


overall_confidence <- confidence_all %>%
  group_by(Language) %>%
  summarise(
    n = sum(!is.na(truth_aligned_confidence)),
    mean_confidence = mean(truth_aligned_confidence, na.rm = TRUE),
    median_confidence = median(truth_aligned_confidence, na.rm = TRUE),
    sd_confidence = sd(truth_aligned_confidence, na.rm = TRUE)
  )

overall_confidence


# ------------------------------------------------------------
# 5. Graph: mean confidence by dataset and language
# ------------------------------------------------------------

graph_confidence <- confidence_all %>%
  group_by(dataset, Language) %>%
  summarise(
    mean_confidence = mean(truth_aligned_confidence, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  ggplot(
    aes(
      x = dataset,
      y = mean_confidence,
      fill = Language
    )
  ) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = paste0(round(mean_confidence, 1), "%")
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4.5
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  
  labs(
    title = "Truth-Aligned Confidence by Language",
    subtitle = "Higher values indicate scores closer to the correct endpoint",
    x = NULL,
    y = "Mean truth-aligned confidence",
    fill = "Language"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11)
  )

graph_confidence


# ------------------------------------------------------------
# 6. Boxplot: distribution of confidence
# ------------------------------------------------------------

graph_confidence_distribution <- ggplot(
  confidence_all,
  aes(
    x = Language,
    y = truth_aligned_confidence,
    fill = Language
  )
) +
  
  geom_boxplot(alpha = 0.7) +
  
  facet_wrap(~ dataset) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  
  labs(
    title = "Distribution of Truth-Aligned Scores",
    x = NULL,
    y = "Truth-aligned confidence"
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

graph_confidence_distribution


# ------------------------------------------------------------
# 7. Create paired claim IDs
# ------------------------------------------------------------
# English and French claims appear in the same order.
# Giving each language its own sequence creates matching pairs.

dataset_true_paired <- dataset_true %>%
  group_by(LG) %>%
  mutate(pair_id = row_number()) %>%
  ungroup()

dataset_false_paired <- dataset_false %>%
  group_by(LG) %>%
  mutate(pair_id = row_number()) %>%
  ungroup()


# ------------------------------------------------------------
# 8. Compare English and French within each TRUE claim pair
# ------------------------------------------------------------

paired_true <- dataset_true_paired %>%
  select(
    pair_id,
    LG,
    truth_aligned_confidence
  ) %>%
  pivot_wider(
    names_from = LG,
    values_from = truth_aligned_confidence
  ) %>%
  mutate(
    difference_E_minus_F = E - F,
    
    best_language = case_when(
      E > F ~ "English",
      F > E ~ "French",
      E == F ~ "Same",
      TRUE ~ NA_character_
    )
  )

paired_true


# ------------------------------------------------------------
# 9. Compare English and French within each FALSE claim pair
# ------------------------------------------------------------

paired_false <- dataset_false_paired %>%
  select(
    pair_id,
    LG,
    truth_aligned_confidence
  ) %>%
  pivot_wider(
    names_from = LG,
    values_from = truth_aligned_confidence
  ) %>%
  mutate(
    difference_E_minus_F = E - F,
    
    best_language = case_when(
      E > F ~ "English",
      F > E ~ "French",
      E == F ~ "Same",
      TRUE ~ NA_character_
    )
  )

paired_false


# ------------------------------------------------------------
# 10. Count which language is closer to the truth
# ------------------------------------------------------------

paired_true %>%
  count(best_language)

paired_false %>%
  count(best_language)


# ------------------------------------------------------------
# 11. Paired statistical tests
# ------------------------------------------------------------
# Wilcoxon test is appropriate here because the scores are
# bounded and strongly clustered at values such as 0, 20, 80, 100.

wilcox.test(
  paired_true$E,
  paired_true$F,
  paired = TRUE,
  na.action = na.omit
)

wilcox.test(
  paired_false$E,
  paired_false$F,
  paired = TRUE,
  na.action = na.omit
)


# ------------------------------------------------------------
# 12. Save figures
# ------------------------------------------------------------

dir.create("figures", showWarnings = FALSE)

ggsave(
  "1. Cross-language performance and retrieval quality/figures/truth_aligned_confidence_png/truth_aligned_confidence.png",
  plot = graph_confidence,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "1. Cross-language performance and retrieval quality/figures/truth_aligned_confidence_png/truth_aligned_confidence_distribution.png",
  plot = graph_confidence_distribution,
  width = 8,
  height = 5,
  dpi = 300
)