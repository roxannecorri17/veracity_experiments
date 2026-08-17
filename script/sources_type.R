# ============================================================
# Experiment 1.3
# Source Type Distribution
# ============================================================

library(tidyverse)
library(readxl)


# ------------------------------------------------------------
# 1. Import recoded source sheets
# ------------------------------------------------------------

false_sources <- read_excel(
  "data/veracity_1.3.xlsx",
  sheet = "false_sources"
)

true_sources <- read_excel(
  "data/veracity_1.3.xlsx",
  sheet = "true_sources"
)


# ------------------------------------------------------------
# 2. FALSE SOURCES
# Calculate frequencies and percentages
# ------------------------------------------------------------

false_type_summary <- false_sources %>%
  filter(!is.na(`Source Type`)) %>%
  count(`Source Type`) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(
      round(percentage, 1),
      "%\n(n=", n, ")"
    )
  )

false_type_summary


# ------------------------------------------------------------
# 3. Pie chart: FALSE sources
# ------------------------------------------------------------

graph_source_type_false <- ggplot(
  false_type_summary,
  aes(
    x = "",
    y = n,
    fill = `Source Type`
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "Source Types — False Claims",
    fill = "Source type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    legend.position = "right"
  )

graph_source_type_false


# ------------------------------------------------------------
# 4. TRUE SOURCES
# Calculate frequencies and percentages
# ------------------------------------------------------------

true_type_summary <- true_sources %>%
  filter(!is.na(`Source Type`)) %>%
  count(`Source Type`) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(
      round(percentage, 1),
      "%\n(n=", n, ")"
    )
  )

true_type_summary


# ------------------------------------------------------------
# 5. Pie chart: TRUE sources
# ------------------------------------------------------------

graph_source_type_true <- ggplot(
  true_type_summary,
  aes(
    x = "",
    y = n,
    fill = `Source Type`
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "Source Types — True Claims",
    fill = "Source type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    legend.position = "right"
  )

graph_source_type_true


# ------------------------------------------------------------
# 6. OVERALL SOURCE TYPE DISTRIBUTION
# ------------------------------------------------------------

all_sources <- bind_rows(
  false_sources %>%
    mutate(ground_truth = "False"),
  
  true_sources %>%
    mutate(ground_truth = "True")
)


overall_type_summary <- all_sources %>%
  filter(!is.na(`Source Type`)) %>%
  count(`Source Type`) %>%
  mutate(
    percentage = n / sum(n) * 100,
    label = paste0(
      round(percentage, 1),
      "%\n(n=", n, ")"
    )
  )

overall_type_summary


# ------------------------------------------------------------
# 7. Pie chart: ALL sources
# ------------------------------------------------------------

graph_source_type_overall <- ggplot(
  overall_type_summary,
  aes(
    x = "",
    y = n,
    fill = `Source Type`
  )
) +
  geom_col(
    width = 1,
    color = "white"
  ) +
  coord_polar(
    theta = "y"
  ) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "Source Types — All Claims",
    fill = "Source type"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14,
      hjust = 0.5
    ),
    legend.position = "right"
  )

graph_source_type_overall


# ------------------------------------------------------------
# 8. Save figures
# ------------------------------------------------------------

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  "figures/sources_type_png/source_types_false.png",
  plot = graph_source_type_false,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/sources_type_png/source_types_true.png",
  plot = graph_source_type_true,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/sources_type_png/source_types_overall.png",
  plot = graph_source_type_overall,
  width = 9,
  height = 6,
  dpi = 300
)


# Source Type Distribution: English vs French


# ------------------------------------------------------------
# 2. Combine TRUE and FALSE sources
# ------------------------------------------------------------

all_sources <- bind_rows(
  false_sources %>%
    mutate(ground_truth = "False"),
  
  true_sources %>%
    mutate(ground_truth = "True")
)


# ------------------------------------------------------------
# 3. Create language labels
# ------------------------------------------------------------

all_sources <- all_sources %>%
  mutate(
    Language = recode(
      LG,
      "E" = "English",
      "F" = "French"
    )
  )


# ------------------------------------------------------------
# 4. Calculate source-type percentages by language
# ------------------------------------------------------------

language_type_summary <- all_sources %>%
  filter(
    !is.na(`Source Type`),
    !is.na(Language)
  ) %>%
  count(
    Language,
    `Source Type`
  ) %>%
  group_by(Language) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

language_type_summary


# ------------------------------------------------------------
# 5. English vs French comparison graph
# ------------------------------------------------------------

graph_source_type_language <- ggplot(
  language_type_summary,
  aes(
    x = `Source Type`,
    y = percentage,
    fill = Language
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_text(
    aes(
      label = paste0(round(percentage, 1), "%")
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Source Types by Language",
    subtitle = "English vs French retrieval",
    x = NULL,
    y = "Percentage of retrieved sources",
    fill = "Language"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "top"
  )

graph_source_type_language


# ------------------------------------------------------------
# 6. Save graph
# ------------------------------------------------------------

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  "figures/sources_type_png/source_types_english_vs_french.png",
  plot = graph_source_type_language,
  width = 11,
  height = 6,
  dpi = 300
)

