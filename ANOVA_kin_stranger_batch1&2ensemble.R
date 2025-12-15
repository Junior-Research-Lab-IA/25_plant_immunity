# ============================
# ANOVA : effet de Kin vs Stranger + p-values & FC S/K annotés
# Fichier : "Tableau_batch1_2_NApourTB2.xlsx"
# ============================

library(readxl)
library(dplyr)
library(car)
library(effectsize)
library(ggplot2)

# ---- 1) Import ----
df <- read_excel("Tableau_batch1_2_NApourTB2.xlsx")

# ---- 2) Préparation ----
df2 <- df |>
  filter(Neighbour %in% c("Kin", "Stranger")) |>
  mutate(Neighbour = factor(Neighbour, levels = c("Kin","Stranger"))) |>
  tidyr::drop_na(Length_Focal, Tillers_Focal, NBI)

# Petite fonction pour formater proprement les p-values
p_fmt <- function(p) ifelse(p < 1e-4, formatC(p, format="e", digits=2),
                            formatC(p, format="f", digits=4))

# ---- 3) Fonction générique : stats + plot annoté (p-values + FC S/K) ----
plot_with_stats <- function(var, ylab, main_title) {
  d <- df2 |> tidyr::drop_na({{var}})
  y <- d[[var]]
  
  # ANOVA
  fml <- as.formula(paste(var, "~ Neighbour"))
  aov_m <- aov(fml, data = d)
  aov_p <- summary(aov_m)[[1]][["Pr(>F)"]][1]
  
  cat("\n=== ANOVA", var, "~ Neighbour ===\n"); print(summary(aov_m))
  cat("\nEffect size (partial eta^2):\n"); print(eta_squared(aov_m, partial = TRUE))
  cat("\nShapiro–Wilk:\n"); print(shapiro.test(residuals(aov_m)))
  cat("\nLevene:\n"); print(leveneTest(fml, data = d))
  
  # Wilcoxon (robuste)
  wlc <- wilcox.test(fml, data = d, exact = FALSE)
  wlc_p <- wlc$p.value
  
  # Fold-change Stranger/Kin sur le trait
  mK <- mean(d[d$Neighbour == "Kin", var, drop=TRUE], na.rm = TRUE)
  mS <- mean(d[d$Neighbour == "Stranger", var, drop=TRUE], na.rm = TRUE)
  fc <- mS / mK
  
  # Position annotation
  y_min <- min(y, na.rm = TRUE); y_max <- max(y, na.rm = TRUE)
  y_rng <- y_max - y_min
  y_ann <- y_max + 0.08*y_rng
  
  lab <- paste0(
    "ANOVA p = ", p_fmt(aov_p),
    "   |   Wilcoxon p = ", p_fmt(wlc_p),
    "   |   Fold-change S/K = ", sprintf("%.3f", fc)
  )
  
  g <- ggplot(d, aes(Neighbour, !!as.name(var))) +
    geom_boxplot(outlier.shape = NA, fill = "lightgray") +
    geom_jitter(width = 0.12, alpha = 0.6) +
    labs(title = main_title, x = "Neighbour identity", y = ylab) +
    theme_minimal(base_size = 13) +
    annotate("text", x = 1.5, y = y_ann, label = lab, size = 4) +
    expand_limits(y = y_ann)
  
  return(g)
}

# ============================
# 4) Graphiques annotés
# ============================

g_len <- plot_with_stats("Length_Focal",
                         ylab = "Length (cm)",
                         main_title = "Effect of Kin vs Stranger on Plant Length")
g_til <- plot_with_stats("Tillers_Focal",
                         ylab = "Number of tillers",
                         main_title = "Effect of Kin vs Stranger on Number of Tillers")
g_nbi <- plot_with_stats("NBI",
                         ylab = "NBI index",
                         main_title = "Effect of Kin vs Stranger on NBI")

print(g_len)
print(g_til)
print(g_nbi)
