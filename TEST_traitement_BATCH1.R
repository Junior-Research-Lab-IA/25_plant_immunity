library(readxl)
library(dplyr)
library(car)
library(effectsize)
library(ggplot2)

# ---- 1) Import ----
df <- read_excel("Valeurs_BATCH1.xlsx")

# ---- 2) Préparation ----
df2 <- df |>
  filter(Treatment %in% c("NT","T")) |>
  mutate(
    Treatment  = factor(Treatment, levels = c("NT","T")),
    Neighbour  = factor(Neighbour, levels = c("Kin","Stranger"))
  ) |>
  tidyr::drop_na(Length_Focal, Tillers_Focal, NBI)

# ---- fonction clean p-value ----
p_fmt <- function(p) ifelse(p < 1e-4, formatC(p, format="e", digits=2),
                            formatC(p, format="f", digits=4))

# ---- 3) Fonction générique avec FC = T / NT ----
plot_with_stats <- function(var, ylab = var, title_prefix = "Effect of Treatment on") {
  d <- df2 |> tidyr::drop_na({{var}})
  y  <- d[[var]]
  
  # ANOVA
  fml <- as.formula(paste(var, "~ Treatment"))
  aov_m <- aov(fml, data = d)
  aov_p <- summary(aov_m)[[1]][["Pr(>F)"]][1]
  
  # Wilcoxon (robuste)
  wlc <- wilcox.test(fml, data = d, exact = FALSE)
  wlc_p <- wlc$p.value
  
  # ---- fold-change T / NT ----
  mNT <- mean(df2[df2$Treatment == "NT", var, drop=TRUE], na.rm = TRUE)
  mT  <- mean(df2[df2$Treatment == "T",  var, drop=TRUE], na.rm = TRUE)
  fc  <- mT / mNT
  
  # position du texte
  y_min <- min(y, na.rm=TRUE); y_max <- max(y, na.rm=TRUE)
  y_rng <- y_max - y_min
  y_ann <- y_max + 0.08*y_rng
  
  lab <- paste0(
    "ANOVA p = ", p_fmt(aov_p),
    "   |   Wilcoxon p = ", p_fmt(wlc_p),
    "   |   Fold-change T/NT = ", sprintf("%.3f", fc)
  )
  
  # plot final
  g <- ggplot(d, aes(Treatment, !!as.name(var))) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.12, alpha = 0.6) +
    labs(
      title = paste(title_prefix, var),
      x = "Treatment (NT vs T)",
      y = ylab
    ) +
    theme_minimal(base_size = 13) +
    annotate("text", x = 1.5, y = y_ann, label = lab, size = 4) +
    expand_limits(y = y_ann)
  
  return(g)
}

# ============================
# 4) Graphes annotés
# ============================

g_len <- plot_with_stats("Length_Focal", ylab = "Length_Focal (cm)")
g_til <- plot_with_stats("Tillers_Focal", ylab = "Tillers_Focal")
g_nbi <- plot_with_stats("NBI", ylab = "NBI")

print(g_len)
print(g_til)
print(g_nbi)

# ============================
# 5) MANOVA
# ============================
cat("\n=== MANOVA (Length + Tillers + NBI) ~ Treatment ===\n")
man <- manova(cbind(Length_Focal, Tillers_Focal, NBI) ~ Treatment, data = df2)
print(summary(man, test = "Pillai"))
