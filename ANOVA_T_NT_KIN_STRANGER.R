# ---- Packages ----
# install.packages(c("readxl","janitor","dplyr","car","effectsize","ggplot2","emmeans"))
library(readxl)
library(janitor)
library(dplyr)
library(car)
library(effectsize)
library(ggplot2)
library(emmeans)

# ---- Import & préparation ----
df <- read_excel("Tableau_modifié_batch1_2.xlsx") |>
  clean_names()  # pot, id_focal, id_neighbour, neighbour, treatment, length_focal, tillers_focal, nbi, ecological

df2 <- df |>
  filter(neighbour %in% c("Kin","Stranger"),
         treatment %in% c("T","NT")) |>
  mutate(
    neighbour = factor(neighbour, levels = c("Kin","Stranger")),
    treatment = factor(treatment, levels = c("NT","T"))   # NT comme référence
  ) |>
  tidyr::drop_na(length_focal, tillers_focal)

# Pour un Type III correct si besoin (données déséquilibrées),
# vous pouvez activer les contrastes ci-dessous :
# options(contrasts = c("contr.sum","contr.poly"))

# =====================================================
# MODELE 2-FACTEURS : Length_Focal
# =====================================================
fit_len <- lm(length_focal ~ neighbour * treatment, data = df2)

# ANOVA Type II (robuste pour déséquilibre modéré)
Anova(fit_len, type = 2)

# Si vous souhaitez Type III :
# Anova(fit_len, type = 3)

# Taille d'effet (eta² partiel)
eta_squared(fit_len, partial = TRUE)

# Diagnostics
shapiro.test(residuals(fit_len))                                   # normalité
leveneTest(length_focal ~ neighbour * treatment, data = df2)        # homogénéité

# Interaction plot
ggplot(df2, aes(treatment, length_focal, color = neighbour, group = neighbour)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.15)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.15)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15,
               position = position_dodge(width = 0.15)) +
  labs(x = "Treatment (NT vs T)", y = "Length_Focal",
       title = "Length_Focal ~ Neighbour × Treatment")

# Post-hoc (simple effects) si interaction significative
emm_len <- emmeans(fit_len, ~ neighbour * treatment)
# Comparer Kin vs Stranger à NT et à T :
pairs(emmeans(fit_len, ~ neighbour | treatment), adjust = "tukey")
# Comparer T vs NT chez Kin et chez Stranger :
pairs(emmeans(fit_len, ~ treatment | neighbour), adjust = "tukey")

# =====================================================
# MODELE 2-FACTEURS : Tillers_Focal
# =====================================================
fit_til <- lm(tillers_focal ~ neighbour * treatment, data = df2)

Anova(fit_til, type = 2)
# Anova(fit_til, type = 3)  # option Type III

eta_squared(fit_til, partial = TRUE)

shapiro.test(residuals(fit_til))
leveneTest(tillers_focal ~ neighbour * treatment, data = df2)

ggplot(df2, aes(treatment, tillers_focal, color = neighbour, group = neighbour)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.15)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.15)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15,
               position = position_dodge(width = 0.15)) +
  labs(x = "Treatment (NT vs T)", y = "Tillers_Focal",
       title = "Tillers_Focal ~ Neighbour × Treatment")

emm_til <- emmeans(fit_til, ~ neighbour * treatment)
pairs(emmeans(fit_til, ~ neighbour | treatment), adjust = "tukey")
pairs(emmeans(fit_til, ~ treatment | neighbour), adjust = "tukey")

