# ============================
# Stranger / Kin ratios across the ecological gradient
# Traits: Length & Number of tillers
# ============================

# ---- Packages
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# ---- 1) Import de l'Excel
df_raw <- read_excel("BATCH_1_sansNBI.xlsx")

# ---- 2) Nettoyage des noms de colonnes
names(df_raw) <- names(df_raw) %>%
  str_trim() %>%
  str_replace_all("\\s+", "_") %>%
  str_replace_all("[^A-Za-z0-9_]", "") %>%
  tolower()

df <- df_raw

# ---- 3) Harmonisation des noms de colonnes
rename_if_exists <- function(dat, candidates, new_name) {
  hit <- intersect(candidates, names(dat))
  if (length(hit) > 0) {
    dat <- dat %>% rename(!!new_name := !!sym(hit[1]))
  }
  dat
}

df <- df %>%
  rename_if_exists(c("neighbour", "neighbor"), "neighbour") %>%
  rename_if_exists(c("ecological", "eco"), "ecological") %>%
  rename_if_exists(c("length_focal", "length"), "length") %>%
  rename_if_exists(c("tillers_focal", "tillers", "density_focal"), "tillers")

# ---- 4) Mise en forme des variables
df <- df %>%
  mutate(
    neighbour = tolower(neighbour),
    neighbour = case_when(
      neighbour == "kin" ~ "Kin",
      neighbour == "stranger" ~ "Stranger",
      TRUE ~ NA_character_
    ),
    ecological = as.numeric(ecological),
    length = as.numeric(length),
    tillers = as.numeric(tillers)
  ) %>%
  filter(!is.na(neighbour), ecological %in% c(0, 1, 2))

# ---- 5) Passage en format long (traits)
df_long <- df %>%
  select(ecological, neighbour, length, tillers) %>%
  pivot_longer(
    cols = c(length, tillers),
    names_to = "trait",
    values_to = "value"
  )

# ---- 6) Fonction de calcul du ratio Stranger / Kin
compute_ratio <- function(data) {
  stats <- data %>%
    group_by(neighbour) %>%
    summarise(
      mean = mean(value, na.rm = TRUE),
      var  = var(value, na.rm = TRUE),
      n    = sum(!is.na(value)),
      .groups = "drop"
    )
  
  kin <- stats %>% filter(neighbour == "Kin")
  stranger <- stats %>% filter(neighbour == "Stranger")
  
  ratio <- stranger$mean / kin$mean
  
  # Intervalle de confiance 95 % (approximation log-normale)
  se_log <- sqrt(
    (stranger$var / (stranger$n * stranger$mean^2)) +
      (kin$var / (kin$n * kin$mean^2))
  )
  
  tibble(
    ratio = ratio,
    lwr = exp(log(ratio) - 1.96 * se_log),
    upr = exp(log(ratio) + 1.96 * se_log)
  )
}

# ---- 7) Calcul des ratios par gradient écologique et par trait
ratios <- df_long %>%
  group_by(ecological, trait) %>%
  group_modify(~ compute_ratio(.x)) %>%
  ungroup()

# ---- 8) Mise en forme finale
ratios <- ratios %>%
  mutate(
    trait = dplyr::recode(
      trait,
      length = "Length",
      tillers = "Number of tillers"
    ),
    ecological = factor(ecological, levels = c(0, 1, 2))
  )

# ---- 9) Graphique (SANS ligne reliant les points)
p <- ggplot(ratios, aes(x = ecological, y = ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15) +
  facet_wrap(~ trait, scales = "free_y") +
  labs(
    title = "Stranger / Kin ratios across the ecological gradient",
    x = "Ecological level",
    y = "S/K ratio"
  ) +
  theme_minimal(base_size = 13)

print(p)

# ---- 10) Export optionnel
# ggsave("SK_ratios_ecological.png", p, width = 8, height = 4, dpi = 300)
