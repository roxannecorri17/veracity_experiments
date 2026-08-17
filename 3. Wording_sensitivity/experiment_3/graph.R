# graph1 : score similary across languages
plot_data <- df_long %>%
  group_by(variant, metric, lg) %>%
  summarise(mean_value = mean(value), .groups = "drop") %>%
  mutate(metric = recode(metric,
                          "justification_similarity" = "Justification",
                          "score_similarity" = "Score",
                          "sources_similarity" = "Sources"))

# Annotation : une seule ligne, positionnée au-dessus de la paire "negative" du panneau "Score"
star_annotation <- data.frame(
  variant = c("negative", "negative"),
  metric = c("Score", "Sources"),
  y_pos = c(1.35, 1.65),
  label = c("*** (p = 0.0007)", "** (p = 0.031)")
)

plot_data %>%
  ggplot(aes(x = variant, y = mean_value, fill = lg)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_value, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  geom_text(data = star_annotation, aes(x = variant, y = y_pos, label = label),
            inherit.aes = FALSE, size = 3.8, fontface = "bold") +
  facet_wrap(~ metric) +
  scale_fill_manual(values = c("english" = "#2a78d6", "french" = "#eb6834")) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  labs(x = "Type de reformulation", y = "Score moyen (0-2)",
       fill = "Langue", title = "Cohérence par métrique, type de reformulation et langue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave("graph_3metriques_annote.png", width = 10, height = 5, dpi = 300)

# graph 2 : accuracy

polarity <- function(gt) {
  gt <- tolower(trimws(gt))
  true_ish  <- c("true","vrai","mostly-true","mostly true","barely-false","barely false")
  false_ish <- c("false","faux","mostly-false","mostly false","barely-true","barely true")
  half      <- c("half-true","half-false","half true","half false")
  case_when(
    gt %in% true_ish  ~ "true",
    gt %in% false_ish ~ "false",
    gt %in% half      ~ "half",
    TRUE ~ NA_character_
  )
}

df_acc %>%
  group_by(variant) %>%
  summarise(
    n_en = sum(lg == "english" & !is.na(accurate)),
    n_fr = sum(lg == "french" & !is.na(accurate)),
    acc_en = mean(accurate[lg == "english"], na.rm = TRUE) * 100,
    acc_fr = mean(accurate[lg == "french"], na.rm = TRUE) * 100,
    p_value = chisq.test(table(lg, accurate))$p.value,
    .groups = "drop"
  ) %>%
  mutate(signif = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", p_value < 0.1 ~ "*", TRUE ~ ""))
acc_plot_data <- df_acc %>%
  group_by(variant, lg) %>%
  summarise(accuracy = mean(accurate, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(variant = factor(variant, levels = c("original","passive","synonyme","negative")))

star_annotation_acc <- data.frame(
  variant = factor(c("original", "synonyme", "negative"), levels = c("original","passive","synonyme","negative")),
  y_pos = c(70, 65, 63),
  label = c("** (p = 0.047)", "* (p = 0.064)", "** (p = 0.044)")
)

acc_plot_data %>%
  ggplot(aes(x = variant, y = accuracy, fill = lg)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(round(accuracy), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  geom_text(data = star_annotation_acc, aes(x = variant, y = y_pos, label = label),
            inherit.aes = FALSE, size = 3.8, fontface = "bold") +
  scale_fill_manual(values = c("english" = "#2a78d6", "french" = "#eb6834")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = "Type de formulation", y = "Exactitude vs ground truth (%)",
       fill = "Langue", title = "Exactitude selon la formulation et la langue") +
  theme_minimal()

ggsave("graph_accuracy_annote.png", width = 7, height = 5, dpi = 300)

#graph 3 : mean per variable 

df_long %>%
  group_by(variant, metric) %>%
  summarise(mean_value = mean(value), .groups = "drop") %>%
  ggplot(aes(x = variant, y = mean_value, fill = metric)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_value, 2)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5)) +
  labs(x = "Type of sentence", y = "Average score (0-2)",
       fill = "Legend", title = "Coherence by sentence variant") +
  theme_minimal()
ggsave("breakdown.png", width = 7, height = 5, dpi = 300)

#moyenne

totals_18 <- df_clean %>%
  filter(variant != "original") %>%
  filter(!is.na(justification_similarity), !is.na(score_similarity), !is.na(sources_similarity)) %>%
  group_by(id, lg) %>%
  summarise(total_18 = sum(justification_similarity) + sum(score_similarity) + sum(sources_similarity), .groups = "drop")

mean(totals_18$total_18)

mean(totals_18$total_18) / 18 * 100