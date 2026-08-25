# ============================================================
# Accuracy analysis by language and sentence variant
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------
# 1. Function to convert ground-truth labels into polarity
# ------------------------------------------------------------

polarity <- function(gt) {
  
  gt <- tolower(trimws(as.character(gt)))
  
  true_ish <- c(
    "true",
    "vrai",
    "mostly-true",
    "mostly true",
    "barely-false",
    "barely false"
  )
  
  false_ish <- c(
    "false",
    "faux",
    "mostly-false",
    "mostly false",
    "barely-true",
    "barely true"
  )
  
  half <- c(
    "half-true",
    "half-false",
    "half true",
    "half false"
  )
  
  case_when(
    gt %in% true_ish  ~ "true",
    gt %in% false_ish ~ "false",
    gt %in% half      ~ "half",
    TRUE              ~ NA_character_
  )
}


# ------------------------------------------------------------
# 2. Create df_acc
# ------------------------------------------------------------

df_acc <- df_clean %>%
  mutate(
    
    # Standardize language names
    lg = tolower(trimws(as.character(lg))),
    
    # Standardize variant names
    variant = tolower(trimws(as.character(variant))),
    
    # Convert ground truth into true / false / half
    ground_truth_binary = polarity(ground_truth),
    
    # Convert Veracity score into binary prediction
    #
    # > 50 = SUPPORTS / TRUE
    # < 50 = REFUTES / FALSE
  
    prediction = case_when(
      score > 50 ~ "true",
      score <= 50 ~ "false",
      TRUE ~ NA_character_
    ),
    
    # Determine whether prediction is accurate
    accurate = case_when(
      
      # Exclude ambiguous half-true / half-false cases
      ground_truth_binary == "half" ~ NA,
      
      # Exclude missing values
      is.na(ground_truth_binary) ~ NA,
      is.na(prediction) ~ NA,
      
      # Correct classification
      prediction == ground_truth_binary ~ TRUE,
      
      # Incorrect classification
      TRUE ~ FALSE
    )
  )


# ------------------------------------------------------------
# 3. Check df_acc
# ------------------------------------------------------------

df_acc %>%
  select(
    id,
    lg,
    variant,
    ground_truth,
    score,
    ground_truth_binary,
    prediction,
    accurate
  ) %>%
  head(20)


# Check number of observations
table(df_acc$lg, useNA = "ifany")
table(df_acc$variant, useNA = "ifany")

# Check correct / incorrect classifications
table(df_acc$lg, df_acc$accurate, useNA = "ifany")

# Check by variant and language
table(
  df_acc$variant,
  df_acc$lg,
  df_acc$accurate,
  useNA = "ifany"
)


# ------------------------------------------------------------
# 4. Accuracy by variant and language
# ------------------------------------------------------------

accuracy_summary <- df_acc %>%
  group_by(variant, lg) %>%
  summarise(
    n = sum(!is.na(accurate)),
    correct = sum(accurate == TRUE, na.rm = TRUE),
    incorrect = sum(accurate == FALSE, na.rm = TRUE),
    accuracy = mean(accurate, na.rm = TRUE) * 100,
    .groups = "drop"
  )

accuracy_summary


# ------------------------------------------------------------
# 5. Compare English and French accuracy
# ------------------------------------------------------------

accuracy_tests <- df_acc %>%
  filter(!is.na(accurate)) %>%
  group_by(variant) %>%
  summarise(
    
    n_en = sum(lg == "english"),
    n_fr = sum(lg == "french"),
    
    acc_en = mean(
      accurate[lg == "english"],
      na.rm = TRUE
    ) * 100,
    
    acc_fr = mean(
      accurate[lg == "french"],
      na.rm = TRUE
    ) * 100,
    
    p_value = {
      
      tab <- table(lg, accurate)
      
      # Use Fisher's exact test when expected counts
      # are too small for chi-square
      if (
        nrow(tab) == 2 &&
        ncol(tab) == 2 &&
        any(chisq.test(tab)$expected < 5)
      ) {
        
        fisher.test(tab)$p.value
        
      } else if (
        nrow(tab) == 2 &&
        ncol(tab) == 2
      ) {
        
        chisq.test(tab, correct = FALSE)$p.value
        
      } else {
        
        NA_real_
      }
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    ),
    
    p_label = case_when(
      is.na(p_value) ~ "",
      p_value < 0.001 ~ "p < 0.001",
      TRUE ~ paste0("p = ", round(p_value, 3))
    )
  )

accuracy_tests


# ------------------------------------------------------------
# 6. Prepare graph data
# ------------------------------------------------------------

acc_plot_data <- df_acc %>%
  filter(!is.na(accurate)) %>%
  group_by(variant, lg) %>%
  summarise(
    accuracy = mean(accurate) * 100,
    .groups = "drop"
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
    )
  )


# ------------------------------------------------------------
# 7. Create automatic significance annotations
# ------------------------------------------------------------

star_annotation_acc <- accuracy_tests %>%
  filter(significance != "") %>%
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
    
    label = paste0(
      significance,
      " (",
      p_label,
      ")"
    ),
    
    # Put significance labels above the highest bar
    y_pos = pmax(acc_en, acc_fr) + 10
  )


star_annotation_acc <- star_annotation_acc %>%
  mutate(
    y_pos = pmin(y_pos, 97)
  )


# ------------------------------------------------------------
# 8. Accuracy graph
# ------------------------------------------------------------

accuracy_graph <- acc_plot_data %>%
  ggplot(
    aes(
      x = variant,
      y = accuracy,
      fill = lg
    )
  ) +
  
  geom_col(
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  
  geom_text(
    aes(
      label = paste0(
        round(accuracy, 1),
        "%"
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    size = 3.5
  ) +
  
  geom_text(
    data = star_annotation_acc,
    aes(
      x = variant,
      y = y_pos,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.8,
    fontface = "bold"
  ) +
  
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
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  
  labs(
    x = "Type of sentence",
    y = "Accuracy (%)",
    fill = "Language",
    title = "Accuracy by sentence variant and language"
  ) +
  
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(
      angle = 20,
      hjust = 1
    ),
    legend.position = "right"
  )

accuracy_graph


# ------------------------------------------------------------
# 9. Save graph
# ------------------------------------------------------------

ggsave(
  "3. Wording_sensitivity/figures/graph_accuracy_annote.png",
  plot = accuracy_graph,
  width = 7,
  height = 5,
  dpi = 300
)