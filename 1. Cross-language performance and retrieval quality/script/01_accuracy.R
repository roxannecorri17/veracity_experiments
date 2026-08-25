# Accuracy analysis : Compare Veracity scores with ground truth
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# 1. Import datasets
# ------------------------------------------------------------

dataset_false <- read_csv("1. Cross-language performance and retrieval quality/data/veracity_1.3 - false_cvs.csv")
dataset_true  <- read_csv("1. Cross-language performance and retrieval quality/data/veracity_1.3 - true_cvs.csv")


# ------------------------------------------------------------
# 2. Create accuracy variable
# ------------------------------------------------------------

# FALSE claims:
# Correct if Veracity score <= 50
dataset_false <- dataset_false %>%
  mutate(
    accuracy = case_when(
      is.na(score) ~ NA,
      score <= 50 ~ 1,
      TRUE ~ 0
    )
  )


# TRUE claims:
# Correct if Veracity score > 50
dataset_true <- dataset_true %>%
  mutate(
    accuracy = case_when(
      is.na(score) ~ NA,
      score > 50 ~ 1,
      TRUE ~ 0
    )
  )


# ------------------------------------------------------------
# 3. Accuracy summary by language
# ------------------------------------------------------------

accuracy_false <- dataset_false %>%
  group_by(LG) %>%
  summarise(
    total_claims = n(),
    scored_claims = sum(!is.na(score)),
    missing_scores = sum(is.na(score)),
    correct = sum(accuracy == 1, na.rm = TRUE),
    incorrect = sum(accuracy == 0, na.rm = TRUE),
    accuracy_rate = mean(accuracy, na.rm = TRUE) * 100
  )

accuracy_true <- dataset_true %>%
  group_by(LG) %>%
  summarise(
    total_claims = n(),
    scored_claims = sum(!is.na(score)),
    missing_scores = sum(is.na(score)),
    correct = sum(accuracy == 1, na.rm = TRUE),
    incorrect = sum(accuracy == 0, na.rm = TRUE),
    accuracy_rate = mean(accuracy, na.rm = TRUE) * 100
  )


# View results
accuracy_false
accuracy_true


# ------------------------------------------------------------
# 4. Give languages readable labels
# ------------------------------------------------------------

accuracy_false <- accuracy_false %>%
  mutate(
    Language = recode(
      LG,
      "E" = "English",
      "F" = "French"
    )
  )

accuracy_true <- accuracy_true %>%
  mutate(
    Language = recode(
      LG,
      "E" = "English",
      "F" = "French"
    )
  )


# ------------------------------------------------------------
# 5. Graph: FALSE dataset
# ------------------------------------------------------------

graph_false <- ggplot(
  accuracy_false,
  aes(x = Language, y = accuracy_rate, fill = Language)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(accuracy_rate, 1), "%")),
    vjust = -0.5,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Veracity Accuracy by Language — False Claims",
    x = NULL,
    y = "Accuracy"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11)
  )

graph_false


# ------------------------------------------------------------
# 6. Graph: TRUE dataset
# ------------------------------------------------------------

graph_true <- ggplot(
  accuracy_true,
  aes(x = Language, y = accuracy_rate, fill = Language)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = paste0(round(accuracy_rate, 1), "%")),
    vjust = -0.5,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Veracity Accuracy by Language — True Claims",
    x = NULL,
    y = "Accuracy"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 11)
  )

graph_true

# Create figures folder if it does not already exist
dir.create("figures", showWarnings = FALSE)

# Save figures
ggsave("1. Cross-language performance and retrieval quality/figures/accuracy_png/accuracy_false.png", plot = graph_false, width = 7, height = 5, dpi = 300)

ggsave("1. Cross-language performance and retrieval quality/figures/accuracy_png/accuracy_true.png", plot = graph_true, width = 7, height = 5, dpi = 300)
