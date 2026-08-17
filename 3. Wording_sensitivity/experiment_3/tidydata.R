library(tidyverse)
df <- read_csv("tidy_exp3.csv",
                skip = 2,
                col_names = c("id","variant","lg","ground_truth","score","sources_count",
                              "justification_similarity","score_similarity","sources_similarity","overall_consistency"))

df_clean <- df %>%
  filter(variant %in% c("original", "passive", "synonyme", "negative"))

df_long <- df_clean %>%
  pivot_longer(
    cols = c(justification_similarity, score_similarity, sources_similarity),
    names_to = "metric",
    values_to = "value",
    values_drop_na = TRUE
  )

nrow(df_clean)   # attendu : 800
nrow(df_long)    # attendu : 1800 (600 lignes non-original × 3 métriques, plus aucun NA)
df_long %>% filter(variant == "original")
