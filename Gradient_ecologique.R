library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(effectsize)
library(ggplot2)

# ---- 1) Import ----
df <- read_excel("Valeurs_BATCH1.xlsx")

# ---- 2) Préparation ----
df2 <- df %>%
  filter(Neighbour %in% c("Kin","Stranger")) %>%
  mutate(
    Neighbour  = factor(Neighbour, levels = c("Kin","Stranger")),
    Ecological = factor(Ecological, levels = c(0,1,2))   # force the 3 levels
  )

# Vérification des effectifs
cat("\n=== Effectifs par modalité Ecological ===\n")
print(table(df2$Ecological, useNA = "ifany"))

# Suppression uniquement des NA nécessaires pour chaque modèle
df_len <- df2 %>% drop_na(Length_Focal)
df_til <- df2 %>% drop_na(Tillers_Focal)
df_nbi <- df2 %>% drop_na(NBI)

# =============================
# ANOVA Length_Focal
# =============================
cat("\n=== ANOVA Length_Focal ~ Neighbour * Ecological ===\n")
aov_len <- aov(Length_Focal ~ Neighbour * Ecological, data = df_len)
print(summary(aov_len))
print(eta_squared(aov_len, partial = TRUE))

# =============================
# ANOVA Tillers_Focal
# =============================
cat("\n=== ANOVA Tillers_Focal ~ Neighbour * Ecological ===\n")
aov_til <- aov(Tillers_Focal ~ Neighbour * Ecological, data = df_til)
print(summary(aov_til))
print(eta_squared(aov_til, partial = TRUE))

# =============================
# ANOVA NBI
# =============================
cat("\n=== ANOVA NBI ~ Neighbour * Ecological ===\n")
aov_nbi <- aov(NBI ~ Neighbour * Ecological, data = df_nbi)
print(summary(aov_nbi))
print(eta_squared(aov_nbi, partial = TRUE))

library(dplyr)
library(tidyr)
library(ggplot2)

# df2 doit contenir: Ecological (0/1/2), Neighbour (Kin/Stranger), Length_Focal, Tillers_Focal, NBI
# Si besoin, crée-le ainsi :
# df2 <- df %>%
#   filter(Neighbour %in% c("Kin","Stranger")) %>%
#   mutate(
#     Neighbour  = factor(Neighbour, levels = c("Kin","Stranger")),
#     Ecological = factor(Ecological, levels = c(0,1,2))
#   )

# Long format + stats par groupe
long <- df2 %>%
  select(Ecological, Neighbour, Length_Focal, Tillers_Focal, NBI) %>%
  pivot_longer(c(Length_Focal, Tillers_Focal, NBI), names_to = "Trait", values_to = "Value")

sumdat <- long %>%
  group_by(Trait, Ecological, Neighbour) %>%
  summarise(
    n    = sum(!is.na(Value)),
    mean = mean(Value, na.rm = TRUE),
    se   = sd(Value, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  )

# Points sans lignes + barres d'erreur (± SE), facetté par trait
ggplot(sumdat, aes(x = Ecological, y = mean, color = Neighbour)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.8) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.15, position = position_dodge(width = 0.5)) +
  facet_wrap(~ Trait, scales = "free_y") +
  labs(title = "Means ± SE by Ecological level and Neighbour",
       x = "Ecological level (0–2)",
       y = "Mean (± SE)") +
  theme_minimal()

