# ============================================================
# Experiment 1.3
# Does retrieval quality affect score quality?
# ============================================================

library(tidyverse)
library(readxl)


# ------------------------------------------------------------
# 1. Import datasets
# ------------------------------------------------------------

dataset_false <- read_csv(
  "1. Cross-language performance and retrieval quality/data/veracity_1.3 - false_cvs.csv"
)

dataset_true <- read_csv(
  "1. Cross-language performance and retrieval quality/data/veracity_1.3 - true_cvs.csv"
)

false_sources <- read_excel(
  "1. Cross-language performance and retrieval quality/data/veracity_1.3.xlsx",
  sheet = "false_sources"
)

true_sources <- read_excel(
  "1. Cross-language performance and retrieval quality/data/veracity_1.3.xlsx",
  sheet = "true_sources"
)


# ------------------------------------------------------------
# 2. Create truth-aligned score
# ------------------------------------------------------------
# 100 = ideal
# 0 = maximally wrong

dataset_false <- dataset_false %>%
  mutate(
    ground_truth = "False",
    score_quality = 100 - score
  )

dataset_true <- dataset_true %>%
  mutate(
    ground_truth = "True",
    score_quality = score
  )


# ------------------------------------------------------------
# 3. Code source relevance as useful/not useful
# ------------------------------------------------------------
# Relevant = 1
# Partially relevant = 1
#
# Out of context = 0
# Outdated = 0
#
# Cannot determine is excluded.

false_sources <- false_sources %>%
  mutate(
    useful_source = case_when(
      `Source Relevance` == "Relevant" ~ 1,
      `Source Relevance` == "Partially relevant" ~ 1,
      `Source Relevance` == "Out of context" ~ 0,
      `Source Relevance` == "Outdated" ~ 0,
      `Source Relevance` == "Cannot determine" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )

true_sources <- true_sources %>%
  mutate(
    useful_source = case_when(
      `Source Relevance` == "Relevant" ~ 1,
      `Source Relevance` == "Partially relevant" ~ 1,
      `Source Relevance` == "Out of context" ~ 0,
      `Source Relevance` == "Outdated" ~ 0,
      `Source Relevance` == "Cannot determine" ~ NA_real_,
      TRUE ~ NA_real_
    )
  )


# ------------------------------------------------------------
# 4. Calculate retrieval quality for each response
# ------------------------------------------------------------
# ID + LG identifies each English/French response.
#
# retrieval_quality = proportion of sources that are
# Relevant or Partially relevant.

false_retrieval <- false_sources %>%
  group_by(ID, LG) %>%
  summarise(
    n_sources = sum(!is.na(useful_source)),
    n_useful = sum(useful_source == 1, na.rm = TRUE),
    retrieval_quality = mean(
      useful_source,
      na.rm = TRUE
    ) * 100,
    .groups = "drop"
  )


true_retrieval <- true_sources %>%
  group_by(ID, LG) %>%
  summarise(
    n_sources = sum(!is.na(useful_source)),
    n_useful = sum(useful_source == 1, na.rm = TRUE),
    retrieval_quality = mean(
      useful_source,
      na.rm = TRUE
    ) * 100,
    .groups = "drop"
  )


# ------------------------------------------------------------
# 5. Merge retrieval quality with Veracity scores
# ------------------------------------------------------------

false_analysis <- dataset_false %>%
  left_join(
    false_retrieval,
    by = c("ID", "LG")
  )

true_analysis <- dataset_true %>%
  left_join(
    true_retrieval,
    by = c("ID", "LG")
  )


all_analysis <- bind_rows(
  false_analysis,
  true_analysis
) %>%
  mutate(
    Language = recode(
      LG,
      "E" = "English",
      "F" = "French"
    )
  )


# ------------------------------------------------------------
# 6. Inspect descriptive statistics
# ------------------------------------------------------------

retrieval_summary <- all_analysis %>%
  group_by(
    ground_truth,
    Language
  ) %>%
  summarise(
    n = sum(
      !is.na(retrieval_quality) &
      !is.na(score_quality)
    ),
    
    mean_retrieval_quality =
      mean(
        retrieval_quality,
        na.rm = TRUE
      ),
    
    mean_score_quality =
      mean(
        score_quality,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

retrieval_summary


# ============================================================
# 7. MAIN TEST
# Retrieval quality vs score quality
# ============================================================

cor_overall <- cor.test(
  all_analysis$retrieval_quality,
  all_analysis$score_quality,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_overall


# ============================================================
# 8. TRUE CLAIMS
# ============================================================

cor_true <- cor.test(
  true_analysis$retrieval_quality,
  true_analysis$score_quality,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_true


# ============================================================
# 9. FALSE CLAIMS
# ============================================================

cor_false <- cor.test(
  false_analysis$retrieval_quality,
  false_analysis$score_quality,
  method = "spearman",
  exact = FALSE,
  use = "complete.obs"
)

cor_false


# ============================================================
# 10. ENGLISH ONLY
# ============================================================

cor_english <- all_analysis %>%
  filter(Language == "English") %>%
  with(
    cor.test(
      retrieval_quality,
      score_quality,
      method = "spearman",
      exact = FALSE,
      use = "complete.obs"
    )
  )

cor_english


# ============================================================
# 11. FRENCH ONLY
# ============================================================

cor_french <- all_analysis %>%
  filter(Language == "French") %>%
  with(
    cor.test(
      retrieval_quality,
      score_quality,
      method = "spearman",
      exact = FALSE,
      use = "complete.obs"
    )
  )

cor_french


# ============================================================
# 12. Graph
# ============================================================

graph_retrieval_score <- ggplot(
  all_analysis,
  aes(
    x = retrieval_quality,
    y = score_quality,
    color = Language
  )
) +
  geom_point(
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  facet_wrap(
    ~ ground_truth
  ) +
  labs(
    title = "Retrieval Quality and Veracity Score Quality",
    subtitle = "Higher values indicate better retrieval and scores closer to ground truth",
    x = "Useful sources (%)",
    y = "Truth-aligned score",
    color = "Language"
  ) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    legend.position = "top"
  )

graph_retrieval_score


# ------------------------------------------------------------
# 13. Save figure
# ------------------------------------------------------------

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  "1. Cross-language performance and retrieval quality/figures/retrieval_effect_png/retrieval_quality_vs_score.png",
  plot = graph_retrieval_score,
  width = 10,
  height = 6,
  dpi = 300
)