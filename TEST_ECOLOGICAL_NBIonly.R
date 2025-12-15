# ============================
# Stranger / Kin ratio for NBI across the ecological gradient (0,1,2)
# ============================

# ---- Packages
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# ---- 1) Import de l'Excel (adapter le chemin si besoin)
df_raw <- read_excel("BATCH_1_queNBI.xlsx")

# ---- 2) Nettoyage des noms de colonnes (minuscules + underscores)
names(df_raw) <- names(df_raw) %>%
  str_trim() %>%
  str_replace_all("\\s+", "_") %>%
  str_replace_all("[^A-Za-z0-9_]", "") %>%
  tolower()

df <- df_raw

# ---- 3) Harmonisation des noms de colonnes
# On cherche des variantes possibles et on renomme vers:
#   neighbour  -> "Kin"/"Stranger"
#   ecological -> 0/1/2
#   nbi        -> valeur numérique
rename_if_exists <- function(dat, candidates, new_name) {
  hit <- intersect(candidates, names(dat))
  if (length(hit) > 0) dat <- dat %>% rename(!!new_name := !!sym(hit[1]))
  dat
}

df <- df %>%
  rename_if_exists(c("neighbour", "neighbor", "neigh", "neighb"), "neighbour") %>%
  rename_if_exists(c("ecological", "eco"), "ecological") %>%
  rename_if_exists(c("nbi_focal", "nbi"), "nbi")

# ---- 4) Mise en forme des variables
df <- df %>%
  mutate(
    neighbour = tolower(as.character(neighbour)),
    neighbour = case_when(
      neighbour %in% c("kin","k") ~ "Kin",
      neighbour %in% c("stranger","s") ~ "Stranger",
      TRUE ~ NA_character_
    ),
    ecological = suppressWarnings(as.numeric(ecological)),
    nbi = suppressWarnings(as.numeric(nbi))
  ) %>%
  filter(!is.na(neighbour), ecological %in% c(0, 1, 2))

# ---- 5) Fonction robuste de calcul du ratio S/K pour un groupe (ici NBI)
# Renvoie NA si l'un des groupes (Kin/Stranger) manque
compute_ratio <- function(data, value_col = "nbi") {
  stats <- data %>%
    group_by(neighbour) %>%
    summarise(
      mean = mean(.data[[value_col]], na.rm = TRUE),
      var  = var(.data[[value_col]],  na.rm = TRUE),
      n    = sum(!is.na(.data[[value_col]])),
      .groups = "drop"
    )
  
  if (!all(c("Kin","Stranger") %in% stats$neighbour)) {
    return(tibble(ratio = NA_real_, lwr = NA_real_, upr = NA_real_))
  }
  
  kin <- stats %>% filter(neighbour == "Kin")
  stranger <- stats %>% filter(neighbour == "Stranger")
  
  ratio <- stranger$mean / kin$mean
  
  # IC 95 % (approximation log-normale; suppose indépendance des groupes)
  se_log <- sqrt(
    (stranger$var / (stranger$n * (stranger$mean^2))) +
      (kin$var      / (kin$n      * (kin$mean^2)))
  )
  
  tibble(
    ratio = ratio,
    lwr = exp(log(ratio) - 1.96 * se_log),
    upr = exp(log(ratio) + 1.96 * se_log)
  )
}

# ---- 6) Calcul des ratios par niveau écologique
ratios_nbi <- df %>%
  group_by(ecological) %>%
  group_modify(~ compute_ratio(.x, "nbi")) %>%
  ungroup() %>%
  mutate(ecological = factor(ecological, levels = c(0, 1, 2)))

# (Optionnel) Aperçu du tableau résultat dans la console
print(ratios_nbi)

# ---- 7) Graphique (pas de ligne reliant les points)
p_nbi <- ggplot(ratios_nbi, aes(x = ecological, y = ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15) +
  labs(
    title = "Stranger / Kin ratios across the ecological gradient (NBI)",
    x = "Ecological level",
    y = "S/K ratio"
  ) +
  theme_minimal(base_size = 13)

print(p_nbi)

# ---- 8) Export optionnel
# write.csv(ratios_nbi, "SK_ratios_NBI_by_ecological.csv", row.names = FALSE)
# ggsave("SK_ratios_NBI_ecological.png", p_nbi, width = 6.5, height = 4, dpi = 300)
