# ============================================================
# Experiment 1.3
# Source Relevance: English vs French
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
# 4. Calculate relevance percentages by language
# ------------------------------------------------------------

relevance_language_summary <- all_sources %>%
  filter(
    !is.na(`Source Relevance`),
    !is.na(Language)
  ) %>%
  count(
    Language,
    `Source Relevance`
  ) %>%
  group_by(Language) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  ungroup()

relevance_language_summary


# ------------------------------------------------------------
# 5. Set relevance category order
# ------------------------------------------------------------

relevance_language_summary <- relevance_language_summary %>%
  mutate(
    `Source Relevance` = factor(
      `Source Relevance`,
      levels = c(
        "Relevant",
        "Partially relevant",
        "Outdated",
        "Out of context",
        "Cannot determine"
      )
    )
  )


# ------------------------------------------------------------
# 6. English vs French comparison graph
# ------------------------------------------------------------

graph_relevance_language <- ggplot(
  relevance_language_summary,
  aes(
    x = `Source Relevance`,
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
      label = paste0(
        round(percentage, 1),
        "%"
      )
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Source Relevance by Language",
    subtitle = "English vs French retrieval",
    x = NULL,
    y = "Percentage of retrieved sources",
    fill = "Language"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(
      mult = c(0, 0.1)
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    ),
    legend.position = "top"
  )

graph_relevance_language


# ------------------------------------------------------------
# 7. Chi-square test
# Does relevance distribution differ by language?
# ------------------------------------------------------------

relevance_table <- table(
  all_sources$Language,
  all_sources$`Source Relevance`
)

relevance_table

chisq_relevance <- chisq.test(
  relevance_table
)

chisq_relevance


# ------------------------------------------------------------
# 8. Save graph
# ------------------------------------------------------------

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  "figures/relevance_png/source_relevance_english_vs_french.png",
  plot = graph_relevance_language,
  width = 10,
  height = 6,
  dpi = 300
)