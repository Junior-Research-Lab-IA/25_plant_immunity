# ============================
# ANOVA Kin vs Stranger (Length_Focal & Tillers_Focal)
# Fichier : "Valeurs BATCH 2.xlsx"
# ============================

# install.packages(c("readxl","dplyr","car","effectsize","ggplot2"))
library(readxl)
library(dplyr)
library(car)
library(effectsize)
library(ggplot2)

# ---- 1) Import ----
df <- read_excel("Valeurs BATCH 2.xlsx")

# ---- 2) Préparation ----
df2 <- df |>
  filter(Neighbour %in% c("Kin","Stranger")) |>
  mutate(Neighbour = factor(Neighbour, levels = c("Kin","Stranger"))) |>
  tidyr::drop_na(Length_Focal, Tillers_Focal)

# ============================
# 3) ANOVA : Length_Focal ~ Neighbour
# ============================
aov_len <- aov(Length_Focal ~ Neighbour, data = df2)

cat("\n=== ANOVA Length_Focal ~ Neighbour ===\n")
print(summary(aov_len))

cat("\nEffect size (partial eta^2) - Length_Focal:\n")
print(eta_squared(aov_len, partial = TRUE))

cat("\nNormality (Shapiro–Wilk) - Length_Focal:\n")
print(shapiro.test(residuals(aov_len)))

cat("\nHomogeneity (Levene) - Length_Focal:\n")
print(leveneTest(Length_Focal ~ Neighbour, data = df2))

# Boxplot Longueur (en anglais)
ggplot(df2, aes(Neighbour, Length_Focal)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray") +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(title = "Effect of Kin vs Stranger on Plant Length",
       x = "Neighbour Identity",
       y = "Length of Focal Plant") +
  theme_minimal()

# Test robuste si la normalité n'est pas respectée
cat("\nWilcoxon (robust) Length_Focal ~ Neighbour:\n")
print(wilcox.test(Length_Focal ~ Neighbour, data = df2, exact = FALSE))

# ============================
# 4) ANOVA : Tillers_Focal ~ Neighbour
# ============================
aov_til <- aov(Tillers_Focal ~ Neighbour, data = df2)

cat("\n=== ANOVA Tillers_Focal ~ Neighbour ===\n")
print(summary(aov_til))

cat("\nEffect size (partial eta^2) - Tillers_Focal:\n")
print(eta_squared(aov_til, partial = TRUE))

cat("\nNormality (Shapiro–Wilk) - Tillers_Focal:\n")
print(shapiro.test(residuals(aov_til)))

cat("\nHomogeneity (Levene) - Tillers_Focal:\n")
print(leveneTest(Tillers_Focal ~ Neighbour, data = df2))

# Boxplot Tilles (en anglais)
ggplot(df2, aes(Neighbour, Tillers_Focal)) +
  geom_boxplot(outlier.shape = NA, fill = "lightgray") +
  geom_jitter(width = 0.12, alpha = 0.6) +
  labs(title = "Effect of Kin vs Stranger on Number of Tillers",
       x = "Neighbour Identity",
       y = "Number of Tillers (Focal Plant)") +
  theme_minimal()

# Test robuste si la normalité n'est pas respectée
cat("\nWilcoxon (robust) Tillers_Focal ~ Neighbour:\n")
print(wilcox.test(Tillers_Focal ~ Neighbour, data = df2, exact = FALSE))
