# ---- Packages ----
library(readxl)
library(janitor)
library(dplyr)
library(car)
library(ggplot2)
library(emmeans)

# ---- Import et nettoyage ----
df <- read_excel("Tableau_modifié_batch1_2.xlsx") |>
  clean_names() |>
  mutate(
    neighbour = factor(neighbour, levels = c("Kin","Stranger")),
    ecological = factor(ecological, levels = c(0,1,2))  # gradient écologique
  ) |>
  tidyr::drop_na(length_focal, tillers_focal)

#========================================================
# 1) MANOVA GLOBALE : effet de Kin vs Stranger
#========================================================

man_global <- manova(cbind(length_focal, tillers_focal) ~ neighbour, data = df)
summary(man_global, test = "Pillai")   # Pillai = le test le plus robuste

#========================================================
# 2) MANOVA AVEC MODE DE VIE (gradient écologique)
#    → teste si l'effet Kin/Stranger dépend du gradient
#========================================================

man_ecol <- manova(cbind(length_focal, tillers_focal) ~ neighbour * ecological, data = df)
summary(man_ecol, test = "Pillai")

# Effets séparés :
summary.aov(man_ecol)  # donne les effets sur chaque variable séparément

#========================================================
# 3) MANOVA PAR ESPÈCE (ou ici : par gradient écologique)
#    → boucle qui analyse chaque groupe séparément
#========================================================

split_manovas <- df |> group_by(ecological) |> group_split()

names(split_manovas) <- paste0("ECO_", levels(df$ecological))

results <- lapply(split_manovas, function(data_sub){
  manova(cbind(length_focal, tillers_focal) ~ neighbour, data = data_sub) |>
    summary(test = "Pillai")
})

results  # Affiche la MANOVA pour chaque mode de vie

#========================================================
# 4) Calcul de la "réponse au voisinage" = différence Kin-Stranger
#    → utile pour voir la corrélation avec le mode de vie
#========================================================

# Moyennes marginales estimées pour chaque espèce x voisinage
emm_growth <- emmeans(
  lm(cbind(length_focal, tillers_focal) ~ neighbour * ecological, data = df),
  ~ neighbour | ecological
)

emm_growth

# Calcul de la différence Kin - Stranger pour le gradient écologique
contrast_kin_str <- contrast(emm_growth, method = "revpairwise")  
contrast_kin_str  # donne la "réponse au voisinage" pour chaque mode de vie

#========================================================
# 5) Visualisation : réponse au voisinage en fonction du mode de vie
#========================================================

df_response <- as.data.frame(contrast_kin_str)

ggplot(df_response, aes(ecological, estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - SE, ymax = estimate + SE), width = 0.1) +
  labs(x = "Gradient écologique (mode de vie)",
       y = "Réponse Kin - Stranger (différence)",
       title = "Lien entre mode de vie et réponse à la parenté") +
  theme_minimal()
