# ============================================================
# #object truth_aligned_confidence not found 

# Confidence comparison: first 50 rows vs. last 150 rows
# FALSE dataset
# ============================================================

dataset_false_split <- dataset_false %>%
  mutate(
    row_number_csv = row_number(),
    
    row_group = case_when(
      row_number_csv <= 50 ~ "Rows 1–50",
      row_number_csv > 50 ~ "Rows 51–200"
    ),
    
    Language = recode(
      LG,
      "E" = "English",
      "F" = "French"
    )
  )


# ------------------------------------------------------------
# 1. Summary statistics
# ------------------------------------------------------------

confidence_split <- dataset_false_split %>%
  group_by(row_group, Language) %>%
  summarise(
    n = sum(!is.na(truth_aligned_confidence)),
    mean_confidence = mean(
      truth_aligned_confidence,
      na.rm = TRUE
    ),
    median_confidence = median(
      truth_aligned_confidence,
      na.rm = TRUE
    ),
    sd_confidence = sd(
      truth_aligned_confidence,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

confidence_split


# ------------------------------------------------------------
# 2. Graph
# ------------------------------------------------------------

graph_confidence_split <- ggplot(
  confidence_split,
  aes(
    x = row_group,
    y = mean_confidence,
    fill = Language
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(round(mean_confidence, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10)
  ) +
  labs(
    title = "Truth-Aligned Confidence by Dataset Section",
    subtitle = "False claims: first 50 rows vs. remaining 150 rows",
    x = NULL,
    y = "Mean truth-aligned confidence",
    fill = "Language"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11)
  )

graph_confidence_split


# ------------------------------------------------------------
# 3. Save graph
# ------------------------------------------------------------

dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/local_test_png/confidence_false_first50_vs_last150.png",
  plot = graph_confidence_split,
  width = 8,
  height = 5,
  dpi = 300
)