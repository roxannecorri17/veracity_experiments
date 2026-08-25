# ============================================================
# Experiment 3
# Robustness to sentence reformulation
# ============================================================

library(tidyverse)


# ============================================================
# 0. IMPORT AND CLEAN DATA
# ============================================================

df <- read_csv(
  "3. Wording_sensitivity/data/tidy_exp3.csv",
  skip = 2,
  col_names = c(
    "id",
    "variant",
    "lg",
    "ground_truth",
    "score",
    "sources_count",
    "justification_similarity",
    "score_similarity",
    "sources_similarity",
    "overall_consistency"
  )
)

df_clean <- df %>%
  filter(
    variant %in% c(
      "original",
      "passive",
      "synonyme",
      "negative"
    )
  ) %>%
  mutate(
    variant = factor(
      variant,
      levels = c(
        "original",
        "passive",
        "synonyme",
        "negative"
      )
    ),
    lg = factor(
      lg,
      levels = c("english", "french")
    )
  )


# ------------------------------------------------------------
# Long-format dataset for similarity measures
# ------------------------------------------------------------

df_long <- df_clean %>%
  filter(variant != "original") %>%
  pivot_longer(
    cols = c(
      justification_similarity,
      score_similarity,
      sources_similarity
    ),
    names_to = "metric",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "justification_similarity",
        "score_similarity",
        "sources_similarity"
      ),
      labels = c(
        "Justification",
        "Score",
        "Sources"
      )
    ),
    variant = droplevels(variant)
  )


# ------------------------------------------------------------
# Basic checks
# ------------------------------------------------------------

nrow(df_clean)
nrow(df_long)

table(df_clean$variant, useNA = "ifany")
table(df_clean$lg, useNA = "ifany")


# ============================================================
# 1. OVERALL SIMILARITY BY REFORMULATION TYPE
# ============================================================

similarity_summary <- df_long %>%
  group_by(variant, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

similarity_summary


# ------------------------------------------------------------
# Combined similarity score
# ------------------------------------------------------------


overall_similarity <- df_clean %>%
  filter(variant != "original") %>%
  filter(
    !is.na(justification_similarity),
    !is.na(score_similarity),
    !is.na(sources_similarity)
  ) %>%
  mutate(
    overall_similarity =
      justification_similarity +
      score_similarity +
      sources_similarity
  ) %>%
  group_by(variant) %>%
  summarise(
    mean_overall = mean(overall_similarity),
    n = n(),
    .groups = "drop"
  )

overall_similarity


# ============================================================
# 2. ENGLISH VS. FRENCH SIMILARITY
# ============================================================

language_similarity_summary <- df_long %>%
  group_by(variant, metric, lg) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

language_similarity_summary


# ------------------------------------------------------------
# Wilcoxon tests: English vs. French
# ------------------------------------------------------------

language_similarity_tests <- df_long %>%
  group_by(variant, metric) %>%
  summarise(
    p_value = wilcox.test(value ~ lg)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

language_similarity_tests


# ============================================================
# 3. FIGURE: SIMILARITY BY LANGUAGE
# ============================================================

plot_data_language <- df_long %>%
  group_by(variant, metric, lg) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )


# Significant differences observed for the negative variant

star_annotation <- data.frame(
  variant = factor(
    c("negative", "negative"),
    levels = c("passive", "synonyme", "negative")
  ),
  metric = factor(
    c("Score", "Sources"),
    levels = c("Justification", "Score", "Sources")
  ),
  y_pos = c(1.35, 1.65),
  label = c(
    "*** (p = 0.0007)",
    "* (p = 0.031)"
  )
)


similarity_language_plot <- ggplot(
  plot_data_language,
  aes(
    x = variant,
    y = mean_value,
    fill = lg
  )
) +
  geom_col(
    position = position_dodge(width = 0.9)
  ) +
  geom_text(
    aes(label = round(mean_value, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3
  ) +
  geom_text(
    data = star_annotation,
    aes(
      x = variant,
      y = y_pos,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold"
  ) +
  facet_wrap(~ metric) +
  scale_fill_manual(
    values = c(
      "english" = "#2a78d6",
      "french" = "#eb6834"
    ),
    labels = c(
      "english" = "English",
      "french" = "French"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, 0.5)
  ) +
  labs(
    x = "Sentence variant",
    y = "Average similarity score (0–2)",
    fill = "Language",
    title = "Similarity across sentence variants and languages"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    )
  )

similarity_language_plot


ggsave(
  "3. Wording_sensitivity/figures/similarity_by_language.png",
  plot = similarity_language_plot,
  width = 10,
  height = 5,
  dpi = 300
)


# ============================================================
# 4. COMPARISON ACROSS REFORMULATION TYPES
# ============================================================

# ------------------------------------------------------------
# Kruskal-Wallis tests
# ------------------------------------------------------------
# Tests whether similarity differs across passive, synonym,
# and negative reformulations for each metric.

kruskal_results <- df_long %>%
  group_by(metric) %>%
  summarise(
    p_value = kruskal.test(value ~ variant)$p.value,
    .groups = "drop"
  )

kruskal_results


# ------------------------------------------------------------
# Pairwise Wilcoxon tests
# ------------------------------------------------------------

pairwise_results <- df_long %>%
  group_by(metric) %>%
  group_modify(
    ~ {
      test <- pairwise.wilcox.test(
        x = .x$value,
        g = .x$variant,
        p.adjust.method = "holm"
      )

      as.data.frame(as.table(test$p.value)) %>%
        filter(!is.na(Freq)) %>%
        rename(
          variant_1 = Var1,
          variant_2 = Var2,
          p_value = Freq
        )
    }
  ) %>%
  ungroup() %>%
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    )
  )

pairwise_results


# ============================================================
# 5. FIGURE: MEAN SIMILARITY BY REFORMULATION TYPE
# ============================================================

plot_data_overall <- df_long %>%
  group_by(variant, metric) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    .groups = "drop"
  )


similarity_variant_plot <- ggplot(
  plot_data_overall,
  aes(
    x = variant,
    y = mean_value,
    fill = metric
  )
) +
  geom_col(
    position = position_dodge(width = 0.9)
  ) +
  geom_text(
    aes(label = round(mean_value, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 2),
    breaks = seq(0, 2, 0.5)
  ) +
  labs(
    x = "Sentence variant",
    y = "Average similarity score (0–2)",
    fill = "Metric",
    title = "Similarity by sentence variant"
  ) +
  theme_minimal()

similarity_variant_plot


ggsave(
  "3. Wording_sensitivity/figures/similarity_by_variant.png",
  plot = similarity_variant_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 6. OVERALL ROBUSTNESS SCORE
# ============================================================
# There are three reformulated versions:
# passive, synonym, and negative.
#
# Each reformulation has three similarity measures scored 0–2.
# Maximum total:
#
# 3 reformulations × 3 measures × 2 points = 18 points
#
# The following calculates one total score out of 18 for each
# claim and language.

totals_18 <- df_clean %>%
  filter(variant != "original") %>%
  filter(
    !is.na(justification_similarity),
    !is.na(score_similarity),
    !is.na(sources_similarity)
  ) %>%
  group_by(id, lg) %>%
  summarise(
    total_18 =
      sum(justification_similarity) +
      sum(score_similarity) +
      sum(sources_similarity),
    .groups = "drop"
  )


# Mean total robustness score out of 18

mean_total_18 <- mean(
  totals_18$total_18,
  na.rm = TRUE
)

mean_total_18


# Mean robustness score as a percentage

mean_total_percent <- mean_total_18 / 18 * 100

mean_total_percent