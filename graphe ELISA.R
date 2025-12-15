############################
## Packages nécessaires
############################
library(ggplot2)
library(scales)

############################
## Données réelles
############################
df <- data.frame(
  PR1_concentration = c(2e-4, 2e-3, 4e-3),
  Absorbance = c(0.80703333, 0.41413333, 0.4271)
)

############################
## Régression linéaire
############################
fit <- lm(Absorbance ~ PR1_concentration, data = df)

# Coefficients
coef_fit <- coef(fit)
slope <- coef_fit[2]
intercept <- coef_fit[1]

# R²
r2 <- summary(fit)$r.squared

# Texte équation (format Excel)
eq_text <- paste0(
  "y = ",
  formatC(slope, digits = 3, format = "f"), "x + ",
  formatC(intercept, digits = 3, format = "f"),
  "\nR² = ",
  formatC(r2, digits = 4, format = "f")
)

############################
## Graphique
############################
ggplot(df, aes(x = PR1_concentration, y = Absorbance)) +
  
  # Points
  geom_point(color = "blue", size = 3) +
  
  # Droite de régression (pointillée)
  geom_smooth(
    method = "lm",
    se = FALSE,
    linetype = "dotted",
    color = "blue"
  ) +
  
  # Annotation équation + R²
  annotate(
    "text",
    x = max(df$PR1_concentration) * 0.55,
    y = max(df$Absorbance) * 0.95,
    label = eq_text,
    hjust = 0,
    size = 4
  ) +
  
  # Labels
  labs(
    x = "PR1 concentration (pmol/µl)",
    y = "Absorbance"
  ) +
  
  # Axe X en notation scientifique
  scale_x_continuous(
    labels = scientific_format(digits = 2),
    limits = c(0, 0.005)
  ) +
  
  # Style proche Excel
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
