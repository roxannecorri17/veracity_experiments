library(tidyverse)
library(tidyverse)

# ============================================================
# 0. IMPORT ET NETTOYAGE
# ============================================================
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
  ) %>%
  mutate(variant = droplevels(as.factor(variant)))   # évite les niveaux fantômes (ex: "original") dans tous les tests qui suivent

nrow(df_clean)   # attendu : 800
nrow(df_long)    # attendu : ~1800 (600 lignes non-original x 3 métriques)


df_long <- df_clean %>%
  filter(variant != "original") %>%          # <- ajout : original n'a jamais de I/J/K légitime
  pivot_longer(
    cols = c(justification_similarity, score_similarity, sources_similarity),
    names_to = "metric",
    values_to = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(variant = droplevels(as.factor(variant)))

nrow(df_long)   # attendu : 1800 pile, si tout est propre maintenant

# ============================================================
# 1. COHÉRENCE GLOBALE (I=verdict, J=score, K=sources) par variante
# ============================================================
df_long %>%
  group_by(variant, metric) %>%
  summarise(mean_value = mean(value), n = n(), .groups = "drop") %>%
  arrange(variant, metric)

df_clean %>%
  filter(variant != "original") %>%
  filter(!is.na(justification_similarity), !is.na(score_similarity), !is.na(sources_similarity)) %>%
  mutate(overall = justification_similarity + score_similarity + sources_similarity) %>%
  group_by(variant) %>%
  summarise(mean_overall = mean(overall), n = n())

# ============================================================
# 2. ANGLAIS vs FRANÇAIS (par métrique, par variante)
# ============================================================
df_long %>%
  group_by(variant, metric, lg) %>%
  summarise(mean_value = mean(value), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = lg, values_from = c(mean_value, n))

df_long %>%
  group_by(variant, metric) %>%
  summarise(p_value = wilcox.test(value ~ lg)$p.value, .groups = "drop") %>%
  mutate(signif = case_when(p_value < 0.01 ~ "***", p_value < 0.05 ~ "**", TRUE ~ ""))

# ============================================================
# 3. ENTRE LES 3 TYPES DE REFORMULATION (Kruskal-Wallis + comparaisons deux à deux)
# ================================