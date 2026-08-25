# Cross-Language Consistency Analysis
# ============================================================

library(tidyverse)


# ------------------------------------------------------------
# 1. Import datasets
# ------------------------------------------------------------

dataset_false <- read_csv("1. Cross-language performance and retrieval quality/data/veracity_1.3 - false_cvs.csv")
dataset_true  <- read_csv("1. Cross-language performance and retrieval quality/data/veracity_1.3 - true_cvs.csv")


# ------------------------------------------------------------
# 2. Add ground truth and combine datasets
# ------------------------------------------------------------

dataset_false <- dataset_false %>%
  mutate(ground_truth = "False")

dataset_true <- dataset_true %>%
  mutate(ground_truth = "True")

all_data <- bind_rows(
  dataset_false,
  dataset_true
)


# ============================================================
# A. NUMBER OF SOURCES IN COMMON
# ============================================================


# ------------------------------------------------------------
# 3. Descriptive statistics by ground truth
# ------------------------------------------------------------

sources_summary <- all_data %>%
  group_by(ground_truth) %>%
  summarise(
    n = sum(!is.na(`number of sources in common`)),
    mean = mean(`number of sources in common`, na.rm = TRUE),
    median = median(`number of sources in common`, na.rm = TRUE),
    sd = sd(`number of sources in common`, na.rm = TRUE),
    min = min(`number of sources in common`, na.rm = TRUE),
    max = max(`number of sources in common`, na.rm = TRUE),
    .groups = "drop"
  )

sources_summary


# ------------------------------------------------------------
# 4. Overall descriptive statistics
# ------------------------------------------------------------

sources_overall <- all_data %>%
  summarise(
    n = sum(!is.na(`number of sources in common`)),
    mean = mean(`number of sources in common`, na.rm = TRUE),
    median = median(`number of sources in common`, na.rm = TRUE),
    sd = sd(`number of sources in common`, na.rm = TRUE),
    min = min(`number of sources in common`, na.rm = TRUE),
    max = max(`number of sources in common`, na.rm = TRUE)
  )

sources_overall


# ------------------------------------------------------------
# 5. Graph: distribution of sources in common
# ------------------------------------------------------------

graph_sources <- ggplot(
  all_data,
  aes(
    x = factor(`number of sources in common`),
    fill = ground_truth
  )
) +
  geom_bar(
    position = "dodge"
  ) +
  labs(
    title = "Number of Sources in Common Across Languages",
    subtitle = "English and French versions of equivalent claims",
    x = "Number of sources in common",
    y = "Number of claims",
    fill = "Ground truth"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  )

graph_sources


# ------------------------------------------------------------
# 6. Compare TRUE vs FALSE
# Wilcoxon rank-sum test
# ------------------------------------------------------------

wilcox_sources <- wilcox.test(
  `number of sources in common` ~ ground_truth,
  data = all_data,
  exact = FALSE
)

wilcox_sources



# ============================================================
# B. CROSS-LANGUAGE SCORE CONSISTENCY
# ============================================================


# ------------------------------------------------------------
# 7. Frequency and percentage by ground truth
# ------------------------------------------------------------

score_consistency_summary <- all_data %>%
  filter(
    !is.na(`cross-language score consistency`)
  ) %>%
  count(
    ground_truth,
    `cross-language score consistency`
  ) %>%
  group_by(ground_truth) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  ungroup()

score_consistency_summary


# ------------------------------------------------------------
# 8. Overall distribution
# ------------------------------------------------------------

score_consistency_overall <- all_data %>%
  filter(
    !is.na(`cross-language score consistency`)
  ) %>%
  count(
    `cross-language score consistency`
  ) %>%
  mutate(
    percent = n / sum(n) * 100
  )

score_consistency_overall


# ------------------------------------------------------------
# 9. Graph: score consistency
# ------------------------------------------------------------

graph_score_consistency <- ggplot(
  score_consistency_summary,
  aes(
    x = factor(`cross-language score consistency`),
    y = percent,
    fill = ground_truth
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(
      label = paste0(round(percent, 1), "%")
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Cross-Language Score Consistency",
    subtitle = "Distribution of consistency scores by ground truth",
    x = "Score consistency (0–2)",
    y = "Percentage of claims",
    fill = "Ground truth"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  )

graph_score_consistency


# ------------------------------------------------------------
# 10. TRUE vs FALSE: Chi-square test
# ------------------------------------------------------------

table_score <- table(
  all_data$ground_truth,
  all_data$`cross-language score consistency`
)

table_score

chisq_score <- chisq.test(table_score)

chisq_score


# If R warns that expected frequencies are too small,
# use Fisher's exact test instead:
fisher_score <- fisher.test(table_score)

fisher_score



# ============================================================
# C. CROSS-LANGUAGE RESPONSE CONSISTENCY
# ============================================================


# ------------------------------------------------------------
# 11. Frequency and percentage by ground truth
# ------------------------------------------------------------

response_consistency_summary <- all_data %>%
  filter(
    !is.na(`cross-language response consistency`)
  ) %>%
  count(
    ground_truth,
    `cross-language response consistency`
  ) %>%
  group_by(ground_truth) %>%
  mutate(
    percent = n / sum(n) * 100
  ) %>%
  ungroup()

response_consistency_summary


# ------------------------------------------------------------
# 12. Overall distribution
# ------------------------------------------------------------

response_consistency_overall <- all_data %>%
  filter(
    !is.na(`cross-language response consistency`)
  ) %>%
  count(
    `cross-language response consistency`
  ) %>%
  mutate(
    percent = n / sum(n) * 100
  )

response_consistency_overall


# ------------------------------------------------------------
# 13. Graph: response consistency
# ------------------------------------------------------------

graph_response_consistency <- ggplot(
  response_consistency_summary,
  aes(
    x = factor(`cross-language response consistency`),
    y = percent,
    fill = ground_truth
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(
      label = paste0(round(percent, 1), "%")
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Cross-Language Response Consistency",
    subtitle = "Distribution of consistency scores by ground truth",
    x = "Response consistency (0–2)",
    y = "Percentage of claims",
    fill = "Ground truth"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  )

graph_response_consistency


# ------------------------------------------------------------
# 14. TRUE vs FALSE: Chi-square test
# ------------------------------------------------------------

table_response <- table(
  all_data$ground_truth,
  all_data$`cross-language response consistency`
)

table_response

chisq_response <- chisq.test(table_response)

chisq_response


# If expected frequencies are too small:
fisher_response <- fisher.test(table_response)

fisher_response



# ============================================================
# D. DOES SOURCE OVERLAP RELATE TO CONSISTENCY?
# ============================================================


# ------------------------------------------------------------
# 15. Sources in common vs Score consistency
# Overall Spearman correlation
# ------------------------------------------------------------

cor_score_overall <- cor.test(
  all_data$`number of sources in common`,
  all_data$`cross-language score consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_score_overall


# ------------------------------------------------------------
# 16. Sources in common vs Response consistency
# Overall Spearman correlation
# ------------------------------------------------------------

cor_response_overall <- cor.test(
  all_data$`number of sources in common`,
  all_data$`cross-language response consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_response_overall



# ============================================================
# E. SPEARMAN CORRELATIONS SEPARATELY
# TRUE AND FALSE CLAIMS
# ============================================================


# ------------------------------------------------------------
# 17. TRUE claims:
# Sources in common vs Score consistency
# ------------------------------------------------------------

cor_true_score <- cor.test(
  dataset_true$`number of sources in common`,
  dataset_true$`cross-language score consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_true_score


# ------------------------------------------------------------
# 18. TRUE claims:
# Sources in common vs Response consistency
# ------------------------------------------------------------

cor_true_response <- cor.test(
  dataset_true$`number of sources in common`,
  dataset_true$`cross-language response consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_true_response


# ------------------------------------------------------------
# 19. FALSE claims:
# Sources in common vs Score consistency
# ------------------------------------------------------------

cor_false_score <- cor.test(
  dataset_false$`number of sources in common`,
  dataset_false$`cross-language score consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_false_score


# ------------------------------------------------------------
# 20. FALSE claims:
# Sources in common vs Response consistency
# ------------------------------------------------------------

cor_false_response <- cor.test(
  dataset_false$`number of sources in common`,
  dataset_false$`cross-language response consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_false_response



# ============================================================
# ASSOCIATION BETWEEN SCORE CONSISTENCY
# AND RESPONSE CONSISTENCY
# ============================================================

cor_score_response <- cor.test(
  all_data$`cross-language score consistency`,
  all_data$`cross-language response consistency`,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_score_response



# ============================================================
# G. SAVE FIGURES
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  "1. Cross-language performance and retrieval quality/figures/consistency_png/sources_in_common.png",
  plot = graph_sources,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "1. Cross-language performance and retrieval quality/figures/consistency_png/score_consistency.png",
  plot = graph_score_consistency,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "1. Cross-language performance and retrieval quality/figures/consistency_png/response_consistency.png",
  plot = graph_response_consistency,
  width = 8,
  height = 5,
  dpi = 300
)