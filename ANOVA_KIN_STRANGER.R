# ---- Packages ----
# install.packages(c("readxl","janitor","dplyr","car","effectsize","ggplot2"))
library(readxl)
library(janitor)
library(dplyr)
library(car)
library(effectsize)
library(ggplot2)

# ---- Import & préparation ----
df <- read_excel("Tableau_modifié_batch1_2.xlsx") |>
  clean_names()  # pot, id_focal, id_neighbour, neighbour, treatment, length_focal, tillers_focal, nbi, ecological

df2 <- df |>
  filter(neighbour %in% c("Kin","Stranger")) |>
  mutate(neighbour = factor(neighbour, levels = c("Kin","Stranger"))) |>
  tidyr::drop_na(length_focal, tillers_focal)

# =====================================================
# 1) ANOVA sur Length_Focal
# =====================================================
aov_len <- aov(length_focal ~ neighbour, data = df2)
summary(aov_len)                           # table ANOVA
eta_squared(aov_len, partial = TRUE)       # taille d'effet (η²p)

# Diagnostics
shapiro.test(residuals(aov_len))                          # normalité (p > .05 attendu)
leveneTest(length_focal ~ neighbour, data = df2)          # homogénéité (p > .05 attendu)

# Graphique
ggplot(df2, aes(neighbour, length_focal)) +
  geom_boxplot() +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(x = "Voisin (Kin vs Stranger)", y = "Length_Focal",
       title = "Longueur selon le voisin")

# (Option robuste si hypothèses violées : test de Wilcoxon)
wilcox.test(length_focal ~ neighbour, data = df2, exact = FALSE)

# =====================================================
# 2) ANOVA sur Tillers_Focal
# =====================================================
aov_til <- aov(tillers_focal ~ neighbour, data = df2)
summary(aov_til)
eta_squared(aov_til, partial = TRUE)

# Diagnostics
shapiro.test(residuals(aov_til))
leveneTest(tillers_focal ~ neighbour, data = df2)

# Graphique
ggplot(df2, aes(neighbour, tillers_focal)) +
  geom_boxplot() +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(x = "Voisin (Kin vs Stranger)", y = "Tillers_Focal",
       title = "Talles (tillers) selon le voisin")

# (Option robuste si hypothèses violées)
wilcox.test(tillers_focal ~ neighbour, data = df2, exact = FALSE)
